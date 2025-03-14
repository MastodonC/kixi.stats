(ns kixi.stats.core
  (:refer-clojure :exclude [count infinite? min max])
  (:require [kixi.stats.digest :as digest]
            #?(:clj [kixi.stats.distribution :as d])
            [kixi.stats.estimate :as e]
            [kixi.stats.math :refer [sq sqrt pow root infinity negative-infinity infinite?]]
            [kixi.stats.protocols :as p]
            [kixi.stats.test :as t]
            [redux.core :refer [fuse-matrix]]))

(defn ^:no-doc somef
  [f & args]
  (fn [x]
    (when-not (nil? x)
      (apply f x args))))

(defn monoid
  "Add 0-arity returning `init` to `f`."
  [f init]
  (fn
    ([] init)
    ([acc] (f acc))
    ([acc x] (f acc x))))

(defn ^:no-doc post-complete
  [rf f]
  (completing rf #(f (rf %))))

#?(:clj
   (def histogram
     "Calculates a histogram of numeric inputs using the t-digest with default arguments."
     (digest/t-digest {:compression 100})))

#?(:clj
   (def median
     "Calculates the median of numeric inputs."
     (post-complete histogram d/median)))

#?(:clj
   (def iqr
     "Calculates the interquartile range of numeric inputs."
     (post-complete histogram d/iqr)))

#?(:clj
   (def summary
     "Calculates the five number summary of numeric inputs."
     (post-complete histogram d/summary)))

(defn cross-tabulate
  "Given a sequence of n functions, each of which returns a categorical value
  (e.g. keyword or string) of a factor, calculates an n-dimensional contingency table
  implementing PContingencyTable. This can be passed to kixi.stats.test/chi-squared-test
  to determine if the relationship between factors is significant.
  See also: kixi.stats.core/chi-squared-test"
  [& fxs]
  (let [f (apply juxt fxs)
        k (clojure.core/count fxs)
        inc (fnil inc 0)]
    (fn
      ([] (vector {} (vec (repeat k {})) 0))
      ([[cells margins n] x]
       [(update cells (f x) inc)
        (first (reduce (fn [[margins i] fx]
                         [(update-in margins [i (fx x)] inc) (inc i)])
                       [margins 0]
                       fxs))
        (inc n)])
      ([[cells margins n]]
       (reify p/PContingencyTable
         (cell [_ coordinates]
           (get cells coordinates 0))
         (grand-total [_] n)
         (margin-totals [_] margins)
         (size [_]
           (mapv clojure.core/count margins)))))))

(def count
  "Calculates the count of inputs."
  (fn
    ([] 0)
    ([n _] (inc n))
    ([n] n)))

(def arithmetic-mean
  "Calculates the arithmetic mean of numeric inputs."
  (fn
    ([] [0.0 0])
    ([[^double s ^long c :as acc] e]
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
    ([] [0 0.0 0.0])
    ([[^long c ^double m ^double ss :as acc] e]
     (if (nil? e)
       acc
       (let [e  (double e)
             c' (inc c)
             m' (+ m (/ (- e m) c'))]
         [c' m' (+ ss (* (- e m') (- e m)))])))
    ([[c _m ss]]
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
    ([[_c _mx _my ssx ssy ssxy]]
     (let [d (sqrt (* ssx ssy))]
       (when-not (zero? d)
         (/ ssxy d))))))

(defn r-squared
  "Given two functions: (fŷ input) and (fy input), returning
  the predicted and actual values of y respectively, estimates
  the coefficient of determination R^2.
  This is the fraction of variance in y explained by the model."
  [fy-hat fy]
  (fn
    ([] [0.0 0.0 0.0 0.0])
    ([[^double c ^double my ^double ssr ^double ssy :as acc] e]
     (let [y-hat (fy-hat e)
           y (fy e)]
       (if (or (nil? y-hat) (nil? y))
         acc
         (let [r   (double (- y y-hat)) ;; Residual
               y   (double y)
               c'  (inc c)
               my' (+ my (/ (- y my) c'))]
           [c' my'
            (+ ssr  (* r r))
            (+ ssy  (* (- y my') (- y my)))]))))
    ([[c _my ssr ssy]]
     (when-not (or (zero? c) (zero? ssy))
       (- 1 (/ ssr ssy))))))

(defn adjusted-r-squared
  "Given two functions: (fŷ input) and (fy input), returning
  the predicted and actual values of y respectively, and a constant k
  equal to the number of terms in the model, estimates the adjusted
  coefficient of determination R^2 using Wherry's Formula-1.
  This is the fraction of variance in y explained by the model,
  adjusted for the number of terms in the model.
  https://stats.stackexchange.com/questions/48703/what-is-the-adjusted-r-squared-formula-in-lm-in-r-and-how-should-it-be-interpret"
  [fy-hat fy k]
  (completing (r-squared fy-hat fy)
              (fn [[c _my ssr ssy]]
                (when (and (pos? ssy)
                           (pos? (- c k 1)))
                  (- 1 (/ (* (/ ssr ssy) (dec c))
                          (- c k 1)))))))

(defn mse
  "Given two functions: (fŷ input) and (fy input), returning
  the predicted and actual values of y respectively, calculates
  the mean squared error of the estimate."
  [fy-hat fy]
  (fn
    ([] [0.0 0.0])
    ([[^double c ^double mse :as acc] e]
     (let [y-hat (fy-hat e)
           y (fy e)]
       (if (or (nil? y-hat) (nil? y))
         acc
         (let [se (sq (- y y-hat))
               c' (inc c)]
           [c' (+ mse (/ (- se mse) c'))]))))
    ([[c mse]]
     (when (pos? c)
       mse))))

(defn rmse
  "Given two functions: (fŷ input) and (fy input), returning
  the predicted and actual values of y respectively, calculates
  the root mean squared error of the estimate."
  [fy-hat fy]
  (post-complete (mse fy-hat fy) (somef sqrt)))

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
         (sqrt (/ (/ chi-squared n) (clojure.core/min (- r-tilde 1) (- k-tilde 1)))))))))

(def sum-squares digest/sum-squares)

(defn simple-linear-regression
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, calculates a least squares linear model between fx and fy over inputs.
  Returns a reified kixi.stats.protocols/PParamaterised.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the linear relationship is nil. See
  https://en.wikipedia.org/wiki/Simple_linear_regression."
  [fx fy]
  (post-complete (sum-squares fx fy) e/simple-linear-regression))

(defn regression-standard-error
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Returns a reified kixi.stats.protocols/PDependent.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  ([fx fy]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (reify p/PDependent
                      (measure [_ x]
                        (e/regression-standard-error sum-squares x))))))
  ([fx fy x]
   (post-complete (sum-squares fx fy) #(e/regression-standard-error % x))))

(defn regression-confidence-interval
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Returns a reified kixi.stats.protocols/PDependent if alpha is supplied,
  or a reified kixi.stats.protocols/PDependentWithSignificance otherwise.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  ([fx fy]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (reify p/PDependentWithSignificance
                      (measure-with-significance [_ x alpha]
                        (e/regression-confidence-interval sum-squares x alpha))))))
  ([fx fy alpha]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (reify p/PDependent
                      (measure [_ x]
                        (e/regression-confidence-interval sum-squares x alpha))))))
  ([fx fy alpha x]
   (post-complete (sum-squares fx fy)
                  #(e/regression-confidence-interval % x alpha))))

(defn regression-prediction-standard-error
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Returns a reified kixi.stats.protocols/PDependent.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  ([fx fy]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (when sum-squares
                      (reify p/PDependent
                        (measure [_ x]
                          (e/regression-prediction-standard-error sum-squares x)))))))
  ([fx fy x]
   (post-complete (sum-squares fx fy)
                  #(e/regression-prediction-standard-error % x))))

(defn regression-prediction-confidence-interval
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, and an x value, calculates the standard error of the least
  squares linear model of fx and fy over inputs.
  Returns a reified kixi.stats.protocols/PDependent if alpha is supplied,
  or a reified kixi.stats.protocols/PDependentWithSignificance otherwise.
  Ignores any records with fx or fy are nil. If there are no records with
  values for fx and fy, the standard error of the estimate is nil."
  ([fx fy]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (reify p/PDependentWithSignificance
                      (measure-with-significance [_ x alpha]
                        (e/regression-prediction-interval sum-squares x alpha))))))
  ([fx fy alpha]
   (post-complete (sum-squares fx fy)
                  (fn [sum-squares]
                    (reify p/PDependent
                      (measure [_ x]
                        (e/regression-prediction-interval sum-squares x alpha))))))
  ([fx fy alpha x]
   (post-complete (sum-squares fx fy)
                  #(e/regression-prediction-interval % x alpha))))

(defn chi-squared-test
  "Given a sequence of functions, each of which returns the categorical value
  (e.g. keyword or string) of a factor, performs a X^2 test of independence."
  [& fxs]
  (post-complete (apply cross-tabulate fxs) t/chi-squared-test))

(defn simple-t-test
  "Performs a simple t test against a specified population mean
  and standard deviation. The standard deviation is optional,
  if not provided, a 'plug-in' test using the sample's sd
  will be performed instead.
  mu: the population mean
  sd: (optional) the population standard deviation"
  [{:keys [mu sd]}]
  (if sd
    (completing mean
                (fn [[s c]]
                  (when-not (zero? c)
                    (t/simple-t-test {:mu mu :sd sd}
                                     {:mean (/ s c) :n c}))))
    (completing variance
                (fn [[c m ss]]
                  (when-not (zero? c)
                    (let [c' (dec c)
                          var (if (pos? c') (/ ss c') 0)]
                      (t/simple-t-test {:mu mu :sd (sqrt var)}
                                       {:mean m :n c})))))))

(defn t-test
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, performs the t test of mean significance of those functions over
  inputs.
  Ignores only inputs where both `(fx input)` and `(fy input)` are nil."
  [fx fy]
  (fn
    ([] [0.0 0.0 0.0 0.0 0.0 0.0])
    ([[^double cx ^double cy ^double mx ^double my ^double ssx ^double ssy :as acc] e]
     (let [x (some-> (fx e) double)
           y (some-> (fy e) double)]
       (if (and (nil? x) (nil? y))
         acc
         (let [cx' (cond-> cx x inc)
               cy' (cond-> cy y inc)
               mx' (cond-> mx x (+ (/ (- x mx) cx')))
               my' (cond-> my y (+ (/ (- y my) cy')))
               ssx' (cond-> ssx x (+ (* (- x mx') (- x mx))))
               ssy' (cond-> ssy y (+ (* (- y my') (- y my))))]
           [cx' cy' mx' my' ssx' ssy']))))
    ([[cx cy mx my ssx ssy]]
     (let [cx' (dec cx) cy' (dec cy)]
       (when (and (pos? cx') (pos? cy'))
         (t/t-test {:mean mx :sd (sqrt (/ ssx cx')) :n cx}
                   {:mean my :sd (sqrt (/ ssy cy')) :n cy}))))))

(defn simple-z-test
  "Performs a simple z test against a specified population mean
  and standard deviation. The standard deviation is optional,
  if not provided, a 'plug-in' test using the sample's sd
  will be performed instead.
  mu: the population mean
  sd: (optional) the population standard deviation"
  [{:keys [mu sd]}]
  (if sd
    (completing mean
                (fn [[s c]]
                  (when-not (zero? c)
                    (t/simple-z-test {:mu mu :sd sd}
                                     {:mean (/ s c) :n c}))))
    (completing variance
                (fn [[c m ss]]
                  (when-not (zero? c)
                    (let [c' (dec c)
                          var (if (pos? c') (/ ss c') 0)]
                      (t/simple-z-test {:mu mu :sd (sqrt var)}
                                       {:mean m :n c})))))))

(defn z-test
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, performs the z-test of mean significance of those functions over
  inputs.

  Ignores only inputs where both `(fx input)` and `(fy input)` are nil."
  [fx fy]
  (fn
    ([] [0.0 0.0 0.0 0.0 0.0 0.0])
    ([[^double cx ^double cy ^double mx ^double my ^double ssx ^double ssy :as acc] e]
     (let [x (some-> (fx e) double)
           y (some-> (fy e) double)]
       (if (and (nil? x) (nil? y))
         acc
         (let [cx' (cond-> cx x inc)
               cy' (cond-> cy y inc)
               mx' (cond-> mx x (+ (/ (- x mx) cx')))
               my' (cond-> my y (+ (/ (- y my) cy')))
               ssx' (cond-> ssx x (+ (* (- x mx') (- x mx))))
               ssy' (cond-> ssy y (+ (* (- y my') (- y my))))]
           [cx' cy' mx' my' ssx' ssy']))))
    ([[cx cy mx my ssx ssy]]
     (let [cx' (dec cx) cy' (dec cy)]
       (when (and (pos? cx') (pos? cy'))
         (t/z-test {:mean mx :sd (sqrt (/ ssx cx')) :n cx}
                   {:mean my :sd (sqrt (/ ssy cy')) :n cy}))))))

(defn proportion
  "Calculate the proportion of inputs for which `pred` returns true."
  [pred]
  (let [arrv (volatile! (long-array 2))]
    (fn
      ([] nil)
      ([_ e]
       (let [^longs arr @arrv]
         (when (pred e)
           (aset arr 0 (inc (aget arr 0))))
         (aset arr 1 (inc (aget arr 1)))
         (vreset! arrv arr)))
      ([_]
       (let [^longs arr @arrv
             n (aget arr 0)
             d (aget arr 1)]
         (when (pos? d)
           (double (/ n d))))))))

(def share
  "Alias for proportion"
  proportion)

(def min
  "Like clojure.core/min, but transducer and nil-friendly."
  (fn
    ([] infinity)
    ([acc]
     (when-not (infinite? acc)
       acc))
    ([^double acc e]
     (if (nil? e)
       acc
       (let [e (double e)]
         (clojure.core/min acc e))))))

(def max
  "Like clojure.core/max, but transducer and nil-friendly."
  (fn
    ([] negative-infinity)
    ([acc]
     (when-not (infinite? acc)
       acc))
    ([^double acc e]
     (if (nil? e)
       acc
       (let [e (double e)]
         (clojure.core/max acc e))))))
