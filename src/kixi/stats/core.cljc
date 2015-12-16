(ns kixi.stats.core
  (:require [kixi.stats.utils :refer [sq sqrt pow somef post-complete]])
  (:refer-clojure :exclude [count]))

(defn count
  "Calculates the count of inputs."
  ([] 0)
  ([n _] (inc n))
  ([n] n))

(defn mean
  "Calculates the arithmetic mean of numeric inputs."
  ([] [0 0])
  ([[s c] e]
   [(+ s e) (inc c)])
  ([[s c]]
   (when-not (zero? c)
     (/ s c))))

(defn variance
  "Estimates an unbiased variance of numeric inputs."
  ([] [0 0 0])
  ([[c m ss] e]
   (let [c' (inc c)
         m' (+ m (/ (- e m) c'))]
     [c' m' (+ ss (* (- e m') (- e m)))]))
  ([[c m ss]]
   (when-not (zero? c)
     (let [c' (dec c)]
       (if (pos? c')
         (/ ss c') 0)))))

(def pvariance
  "Calculates the population variance of numeric inputs."
  (completing variance (fn [[c _ ss]]
                         (when-not (zero? c)
                           (/ ss c)))))

(def standard-deviation
  "Estimates the sample standard deviation of numeric inputs."
  (post-complete variance (somef sqrt)))

(def pstandard-deviation
  "Calculates the population standard deviation of numeric inputs."
  (post-complete pvariance (somef sqrt)))

(defn skewness
  "Estimates the sample skewness of numeric inputs.
  See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance."
  ([] [0 0 0 0])
  ([[c m1 m2 m3] e]
   (let [c'  (inc c)
         d   (- e m1)
         dc  (/ d c')
         m1' (+ m1 dc)
         m2' (+ m2 (* (sq d) (/ c c')))
         m3' (+ m3
                (/ (* (pow d 3) (- c' 1) (- c' 2)) (sq c'))
                (* -3 m2 dc))]
     [c' m1' m2' m3']))
  ([[c _ m2 m3]]
   (let [d (* (pow m2 1.5) (- c 2))]
     (when-not (zero? d)
       (/ (* (sqrt (dec c)) m3 c) d)))))

(def pskewness
  "Calculates the population skewness of numeric inputs.
  See: http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis."
  (completing skewness
              (fn [[c _ m2 m3]]
                (let [d (pow m2 1.5)]
                  (when-not (zero? d)
                    (/ (* (sqrt c) m3) d))))))

(defn kurtosis
  "Estimates the sample kurtosis of numeric inputs.
  See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  and http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis."
  ([] [0 0 0 0 0])
  ([[c m1 m2 m3 m4] e]
   (let [c'  (inc c)
         d   (- e m1)
         dc  (/ d c')
         m1' (+ m1 dc)
         m2' (+ m2 (* (sq d) (/ c c')))
         m3' (+ m3
                (/ (* (pow d 3) (- c' 1) (- c' 2)) (sq c'))
                (* -3 m2 dc))
         m4' (+ m4
                (/ (* (pow d 4) (- c' 1) (+ (sq c') (* -3 c') 3))
                   (pow c' 3))
                (* 6 m2 (sq dc))
                (* -4 m3 dc))]
     [c' m1' m2' m3' m4']))
  ([[c _ m2 _ m4]]
   (when-not (or (zero? m2) (< c 4))
     (let [v (/ m2 (dec c))]
       (- (/ (* c (inc c) m4)
             (* (- c 1) (- c 2) (- c 3) (sq v)))
          (/ (* 3 (sq (dec c)))
             (* (- c 2) (- c 3))))))))

(defn covariance
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, estimates the unbiased covariance of those functions over
  inputs.
  
  Ignores any inputs where `(fx input)` or `(fy input)` are nil. If no
  inputs have both x and y, returns nil."
  [fx fy]
  (fn
    ([] [0 0 0 0])
    ([[c mx my ss :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ss (* (- x mx') (- y my)))]))))
    ([[c _ _ ss]]
     (when-not (zero? c)
       (/ ss c)))))

(defn correlation
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, estimates the unbiased linear correlation coefficient between fx and
  fy over inputs. Ignores any records where fx or fy are nil. If there are no
  records with values for fx and fy, the correlation is nil. See
  http://mathworld.wolfram.com/CorrelationCoefficient.html."
  [fx fy]
  (fn
    ([] [0 0 0 0 0 0])
    ([[c mx my ssx ssy ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ssx  (* (- x mx') (- x mx)))
            (+ ssy  (* (- y my') (- y my)))
            (+ ssxy (* (- x mx') (- y my)))]))))
    ([[c mx my ssx ssy ssxy]]
     (let [d (sqrt (* ssx ssy))]
       (when-not (zero? d)
         (/ ssxy d))))))

(defn simple-linear-regression
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, calculates a least squares linear model between fx and fy over inputs.
  Returns a vector containing the coefficients: offset and slope.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the linear relationship is nil. See
  https://en.wikipedia.org/wiki/Simple_linear_regression."
  [fx fy]
  (fn
    ([] [0 0 0 0 0])
    ([[c mx my ssx ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ssx  (* (- x mx') (- x mx)))
            (+ ssxy (* (- x mx') (- y my)))]))))
    ([[_ mx my ssx ssxy]]
     (when-not (zero? ssx)
       (let [b (/ ssxy ssx)]
         [(- my (* mx b)) b])))))
