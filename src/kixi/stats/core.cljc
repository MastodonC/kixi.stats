(ns kixi.stats.core
  (:require [kixi.stats.utils :refer [sq sqrt pow somef post-complete]]
            [redux.core :refer [fuse-matrix]])
  (:refer-clojure :exclude [count]))

(def count
  "Calculates the count of inputs."
  (fn
    ([] 0)
    ([n _] (inc n))
    ([n] n)))

(def mean
  "Calculates the arithmetic mean of numeric inputs."
  (fn
    ([] [0 0])
    ([[s c] e]
     [(+ s e) (inc c)])
    ([[s c]]
     (when-not (zero? c)
       (/ s c)))))

(def variance-s
  "Estimates an unbiased variance of numeric inputs."
  (fn
    ([] [0 0 0])
    ([[c m ss] e]
     (let [c' (inc c)
           m' (+ m (/ (- e m) c'))]
       [c' m' (+ ss (* (- e m') (- e m)))]))
    ([[c m ss]]
     (when-not (zero? c)
       (let [c' (dec c)]
         (if (pos? c')
           (/ ss c') 0))))))

(def variance
  "Alias for variance-s."
  variance-s)

(def variance-p
  "Calculates the population variance of numeric inputs."
  (completing variance-s (fn [[c _ ss]]
                           (when-not (zero? c)
                             (/ ss c)))))

(def standard-deviation-s
  "Estimates the sample standard deviation of numeric inputs."
  (post-complete variance-s (somef sqrt)))

(def standard-deviation
  "Alias for standard-deviation-s."
  standard-deviation-s)

(def standard-deviation-p
  "Calculates the population standard deviation of numeric inputs."
  (post-complete variance-p (somef sqrt)))

(def standard-error-s
  "Calculates the standard error of sample means."
  (completing standard-deviation-s
              (fn [[c _ ss]]
                (when-not (zero? c)
                  (let [c' (dec c)]
                    (if (pos? c')
                      (sqrt (/ ss c' c)) 0))))))

(def standard-error
  "Alias for standard-error-s"
  standard-error-s)

(def skewness-s
  "Estimates the sample skewness of numeric inputs.
  See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance."
  (fn
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
         (/ (* (sqrt (dec c)) m3 c) d))))))

(def skewness
  "Alias for skewness-s."
  skewness-s)

(def skewness-p
  "Calculates the population skewness of numeric inputs.
  See: http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis."
  (completing skewness-s
              (fn [[c _ m2 m3]]
                (let [d (pow m2 1.5)]
                  (when-not (zero? d)
                    (/ (* (sqrt c) m3) d))))))

(def kurtosis-s
  "Estimates the sample kurtosis of numeric inputs.
  See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
  and http://www.real-statistics.com/descriptive-statistics/symmetry-skewness-kurtosis."
  (fn
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
               (* (- c 2) (- c 3)))))))))

(def kurtosis
  "Alias for kurtosis-s."
  kurtosis-s)

(def kurtosis-p
  "Calculates the population kurtosis of numeric inputs.
  See http://www.macroption.com/kurtosis-formula/"
  (completing kurtosis-s (fn [[c _ m2 _ m4]]
                           (when-not (zero? m2)
                             (- (/ (* c m4)
                                   (sq m2)) 3)))))

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

(defn covariance-matrix
  "Given a map of key names to functions that extract values for those keys
  from an input, computes the covariance for each of the n^2 key pairs.
  For example:

      (covariance-matrix {:name-length #(.length (:name %))
                          :age         :age
                          :num-cats    (comp count :cats)})

  will, when reduced, return a map like:

      {[:name-length :age]      0.56
       [:name-length :num-cats] 0.95
       ...}"
  [kvs]
  (fuse-matrix covariance kvs))

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

(defn correlation-matrix
  "Given a map of key names to functions that extract values for those keys
  from an input, computes the correlation for each of the n^2 key pairs.
  For example:

      (correlation-matrix {:name-length #(.length (:name %))
                           :age         :age
                           :num-cats    (comp count :cats)})

  will, when reduced, return a map like:

      {[:name-length :age]      0.56
       [:name-length :num-cats] 0.95
       ...}"
  [kvs]
  (fuse-matrix correlation kvs))

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
