(ns kixi.stats.core
  (:require [kixi.stats.math :refer [sq sqrt pow root]]
            [redux.core :refer [fuse-matrix]])
  (:refer-clojure :exclude [count]))

(defn ^:no-doc somef
  [f]
  (fn [x & args]
    (when-not (nil? x)
      (apply f x args))))

(defn ^:no-doc post-complete
  [rf f]
  (completing rf #(f (rf %))))

(def count
  "Calculates the count of inputs."
  (fn
    ([] 0)
    ([n _] (inc n))
    ([n] n)))

(defn count-when
  "Calculates the count of inputs for which `pred` returns truthy."
  [pred]
  (fn
    ([] 0)
    ([n x]
     (if (pred x)
       (inc n)
       n))
    ([n] n)))

(def arithmetic-mean
  "Calculates the arithmetic mean of numeric inputs."
  (fn
    ([] [0.0 0.0])
    ([[^double s ^double c :as acc] e]
     (if (nil? e)
       acc
       (let [e (double e)]
         [(+ s e) (inc c)])))
    ([[s c]]
     (when-not (zero? c)
       (/ s c)))))

(def mean
  "Alias for arithmetic-mean."
  arithmetic-mean)

(def geometric-mean
  "Calculates the geometric mean of numeric inputs. Defined only for positive numbers."
  (fn
    ([] [1 0])
    ([[s c :as acc] e]
     (cond
       (nil? e) acc
       (neg? e) (reduced [nil 0])
       :else [(* s e) (inc c)]))
    ([[s c]]
     (when-not (zero? c)
       (if (zero? s)
         0.0 (root s c))))))

(def harmonic-mean
  "Calculates the harmonic mean of numeric inputs."
  (fn
    ([] [0 0])
    ([[s c :as acc] e]
     (cond
       (nil? e) acc
       (zero? e) (reduced [0 (inc c)])
       :else [(+ s (/ 1 e)) (inc c)]))
    ([[s c]]
     (when-not (zero? c)
       (if (zero? s)
         0.0 (/ c s))))))

(def variance-s
  "Estimates an unbiased variance of numeric inputs."
  (fn
    ([] [0.0 0.0 0.0])
    ([[^double c ^double m ^double ss :as acc] e]
     (if (nil? e)
       acc
       (let [e  (double e)
             c' (inc c)
             m' (+ m (/ (- e m) c'))]
         [c' m' (+ ss (* (- e m') (- e m)))])))
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
    ([] [0.0 0.0 0.0 0.0])
    ([[^double c ^double m1 ^double m2 ^double m3 :as acc] e]
     (if (nil? e)
       acc
       (let [e   (double e)
             c'  (inc c)
             d   (- e m1)
             dc  (/ d c')
             m1' (+ m1 dc)
             m2' (+ m2 (* (sq d) (/ c c')))
             m3' (+ m3
                    (/ (* (pow d 3) (- c' 1) (- c' 2)) (sq c'))
                    (* -3 m2 dc))]
         [c' m1' m2' m3'])))
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
    ([] [0.0 0.0 0.0 0.0 0.0])
    ([[^double c ^double m1 ^double m2 ^double m3 ^double m4 :as acc] e]
     (if (nil? e)
       acc
       (let [e   (double e)
             c'  (inc c)
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
         [c' m1' m2' m3' m4'])))
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

(defn covariance-s
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, estimates the unbiased covariance of those functions over
  inputs.

  Ignores any inputs where `(fx input)` or `(fy input)` are nil. If no
  inputs have both x and y, returns nil."
  [fx fy]
  (fn
    ([] [0.0 0.0 0.0 0.0])
    ([[^double c ^double mx my ss :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [x   (double x)
               y   (double y)
               c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ss (* (- x mx') (- y my)))]))))
    ([[c _ _ ss]]
     (when-not (zero? c)
       (let [c' (dec c)]
         (if (pos? c')
           (/ ss c') 0))))))

(def covariance
  "Alias for covariance-s"
  covariance-s)

(defn covariance-p
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, estimates the population covariance of those functions over
  inputs.

  Ignores any inputs where `(fx input)` or `(fy input)` are nil. If no
  inputs have both x and y, returns nil."
  [fx fy]
  (completing (covariance-s fx fy)
              (fn [[c _ _ ss]]
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
    ([] [0.0 0.0 0.0 0.0 0.0 0.0])
    ([[^double c ^double mx ^double my ^double ssx ^double ssy ^double ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [x   (double x)
               y   (double y)
               c'  (inc c)
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

(defn cramers-v
  "Cramer's Phi is the intercorrelation of two discrete variables and may be used with variables having two or more levels. It gives a value between 0 and +1 (inclusive).
  Given two functions: (fx input) and (fy input), each of which returns a the relevant discrete value."
  [fx fy]
  (fn
    ([] [{} {} {} 0])
    ([[f1 f2 f12 n] row]
     (let [k1 (fx row)
           k2 (fy row)
           k12 [k1 k2]
           increment-count (fn [m k] (update m k (fnil inc 0)))
           f1' (increment-count f1 k1)
           f2' (increment-count f2 k2)
           f12' (increment-count f12 k12)
           n' (inc n)]
       [f1' f2' f12' n']))
    ([[f1 f2 f12 n]]
     (let [r (clojure.core/count f1)
           r-tilde (when (> n 1) (- r (/ (sq (dec r)) (- n 1))))
           k (clojure.core/count f2)
           k-tilde (when (> n 1) (- k (/ (sq (dec k)) (- n 1))))
           chi-squared (reduce-kv (fn [acc k v]
                                    (let [n1 (get f1 (first k))
                                          n2 (get f2 (last k))
                                          n12 v]
                                      (+ acc (/ (sq (- n12 (/ (* n1 n2) n)))
                                                (/ (* n1 n2) n)))))
                                  0
                                  f12)]
       (when (and r-tilde k-tilde (> r-tilde 1) (> k-tilde 1))
         (sqrt (/ (/ chi-squared n) (min (- r-tilde 1) (- k-tilde 1)))))))))

(defn sum-squares
  [fx fy]
  (fn
    ([] [0.0 0.0 0.0 0.0 0.0 0.0])
    ([[^double c ^double mx ^double my ^double ssx ^double ssy ^double ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [x   (double x)
               y   (double y)
               c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ssx  (* (- x mx') (- x mx)))
            (+ ssy  (* (- y my') (- y my)))
            (+ ssxy (* (- x mx') (- y my)))]))))
    ([[c mx my ssx ssy ssxy]]
     {:n c
      :x-bar mx
      :y-bar my
      :ss-xy ssxy
      :ss-x  ssx
      :ss-y  ssy})))

(defn simple-linear-regression
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, calculates a least squares linear model between fx and fy over inputs.
  Returns a vector containing the coefficients: offset and slope.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the linear relationship is nil. See
  https://en.wikipedia.org/wiki/Simple_linear_regression."
  [fx fy]
  (post-complete (sum-squares fx fy)
                 (fn [{:keys [x-bar y-bar ss-x ss-xy]}]
                   (when-not (zero? ss-x)
                     (let [b (/ ss-xy ss-x)]
                       [(- y-bar (* x-bar b)) b])))))

(def standard-error-estimate
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  (let [f (fn [{:keys [n x-bar y-bar ss-x ss-y ss-xy]} x]
            (when (and (> n 2) (not (zero? ss-x)))
              (sqrt
               (* (/ 1 (- n 2))
                  (- ss-y (/ (sq ss-xy) ss-x))
                  (+ (/ 1 n) (/ (sq (- x x-bar)) ss-x))))))]
    (fn
      ([sum-squares x]
       (f sum-squares x))
      ([fx fy x]
       (post-complete (sum-squares fx fy) #(f % x))))))

(def standard-error-prediction
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  (let [f (fn [{:keys [n x-bar y-bar ss-x ss-y ss-xy]} x]
            (when (and (> n 2) (not (zero? ss-x)))
              (sqrt
               (* (/ 1 (- n 2))
                  (- ss-y (/ (sq ss-xy) ss-x))
                  (+ 1 (/ 1 n) (/ (sq (- x x-bar)) ss-x))))))]
    (fn
      ([sum-squares x]
       (f sum-squares x))
      ([fx fy x]
       (post-complete (sum-squares fx fy) #(f % x))))))
