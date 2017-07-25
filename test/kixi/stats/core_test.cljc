(ns kixi.stats.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [kixi.stats.core :as kixi]
            [kixi.stats.math :refer [abs sq pow sqrt root equal]]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :refer [for-all]]
                 [clojure.test :refer [is deftest]]])))

(def test-opts {:num-tests 100
                :par       4})

(defn infinite? [x]
  (and (float? x) (.isInfinite x)))

(defn seq= [f]
  (fn [x y]
    (cond
      (and (sequential? x) (sequential? y))
      (every? true? (map f x y))
      (or (sequential? x) (sequential? y))
      false
      :else (f x y))))

(defn map= [f]
  (fn [x y]
    (cond
      (and (map? x) (map? y))
      (->> (merge-with f x y)
           (vals)
           (every? identity))
      (or (map? x) (map? y))
      false
      :else (f x y))))

(defn some= [f]
  (fn [x y]
    (if (and (nil? x) (nil? y))
      true
      (f x y))))

(defn inf= [f]
  (fn [x y]
    (if (and (infinite? x) (infinite? y))
      true
      (f x y))))

(defn approx= [e]
  (fn [x y]
    (let [e (if (or (zero? x) (zero? y))
              e (* (abs (min x y)) e))]
      (equal x y e))))

(def =ish (-> (approx= 1e-12) inf= some= map= seq=))

(defn arithmetic-mean'
  [coll]
  (let [c (count coll)]
    (when (pos? c)
      (/ (reduce + coll) c))))

(def mean' arithmetic-mean')

(defn geometric-mean'
  [coll]
  (let [c (count coll)]
    (when (and (pos? c) (every? (complement neg?) coll))
      (root (reduce * coll) c))))

(defn harmonic-mean'
  [coll]
  (let [reciprocal #(/ 1 %)
        c (count coll)
        s (if (some zero? coll)
            0.0
            (reduce + (map reciprocal coll)))]
    (when-not (zero? c)
      (if (zero? s)
        0.0 (/ c s)))))

(defn variance'
  [coll]
  (let [c (count coll)]
    (when (pos? c)
      (let [c' (dec c)]
        (if (pos? c')
          (/ (->> coll
                  (map #(sq (- % (mean' coll))))
                  (reduce +))
             c')
          0)))))

(defn pvariance'
  [coll]
  (let [c (count coll)]
    (when (pos? c)
      (/ (->> coll
              (map #(sq (- % (mean' coll))))
              (reduce +))
         (count coll)))))

(defn standard-error'
  [coll]
  (let [c (count coll)]
    (when (pos? c)
      (let [c' (dec c)]
        (if (pos? c')
          (sqrt (/ (->> coll
                        (map #(sq (- % (mean' coll))))
                        (reduce +))
                   c' c))
          0)))))

(defn covariance'
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (if (empty? coll')
      nil
      (let [mean-x (mean' (map fx coll'))
            mean-y (mean' (map fy coll'))
            c (count coll')]
        (let [c' (dec c)]
          (if (pos? c')
            (/ (reduce + (map #(* (- (fx %) mean-x)
                                  (- (fy %) mean-y))
                              coll'))
               c')
            0))))))

(defn pcovariance'
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (if (empty? coll')
      nil
      (let [mean-x (mean' (map fx coll'))
            mean-y (mean' (map fy coll'))
            c (count coll')]
        (when-not (zero? c)
          (/ (reduce + (map #(* (- (fx %) mean-x)
                                (- (fy %) mean-y))
                            coll'))
             c))))))

(defn covariance-matrix'
  [coll]
  {[:x :y] (covariance' :x :y coll)
   [:x :z] (covariance' :x :z coll)
   [:y :z] (covariance' :y :z coll)
   [:y :x] (covariance' :y :x coll)
   [:z :x] (covariance' :z :x coll)
   [:z :y] (covariance' :z :y coll)})

(defn correlation'
  [fx fy coll]
  "http://mathworld.wolfram.com/CorrelationCoefficient.html"
  (let [coll' (filter fx (filter fy coll))]
    (when-not (empty? coll')
      (let [xs (map fx coll')
            ys (map fy coll')
            mx (mean' xs)
            my (mean' ys)
            mxs (map #(- % mx) xs)
            mys (map #(- % my) ys)]
        (let [d (sqrt (* (reduce + (map * mxs mxs))
                         (reduce + (map * mys mys))))]
          (when-not (zero? d)
            (/ (reduce + (map * mxs mys)) d)))))))

(defn cramers-v'
  [fx fy coll]
  (let [count-values (fn [m]
                       (into {} (for [[k v] m] [k (count v)]) ))
        x-counts (count-values (group-by fx coll))
        y-counts (count-values (group-by fy coll))
        xy-counts (count-values (group-by (juxt fx fy) coll))
        n (clojure.core/count coll)
        chi-squared  (reduce-kv (fn [acc k v]
                                  (let [n1 (get x-counts (first k))
                                        n2 (get y-counts (last k))
                                        n12 v]
                                    (+ acc (/ (sq (- n12 (/ (* n1 n2) n)))
                                              (/ (* n1 n2) n)))))
                                0
                                xy-counts)
        r (clojure.core/count x-counts)
        k (clojure.core/count y-counts)
        r-tilde (when (> r 1) (- r (/ (sq (dec r)) (- n 1))))
        k-tilde (when (> k 1) (- k (/ (sq (dec k)) (- n 1))))]
    (when (and r-tilde k-tilde (> r-tilde 1) (> k-tilde 1))
      (sqrt (/ (/ chi-squared n) (min (- r-tilde 1) (- k-tilde 1)))))))

(defn correlation-matrix'
  [coll]
  {[:x :y] (correlation' :x :y coll)
   [:x :z] (correlation' :x :z coll)
   [:y :z] (correlation' :y :z coll)
   [:y :x] (correlation' :y :x coll)
   [:z :x] (correlation' :z :x coll)
   [:z :y] (correlation' :z :y coll)})

(defn simple-linear-regression'
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (when-not (empty? coll')
      (let [xs (map fx coll')
            ys (map fy coll')
            mx (mean' xs)
            my (mean' ys)
            vx (variance' xs)
            vxy (covariance' fx fy coll')]
        (when-not (zero? vx)
          (let [slope (/ vxy vx)]
            [(- my (* mx slope)) slope]))))))

(defn standard-error-estimate'
  [fx fy x coll]
  (let [coll' (filter fx (filter fy coll))]
    (when-not (empty? coll')
      (let [c (count coll')
            xs (map fx coll')
            ys (map fy coll')
            mx (mean' xs)
            my (mean' ys)
            ssx (reduce + (map #(sq (- % mx)) xs))
            ssy (reduce + (map #(sq (- % my)) ys))
            ssxy (reduce + (map #(* (- %1 mx) (- %2 my)) xs ys))]
        (when (and (> c 2) (not (zero? ssx)))
          (let [steyx (sqrt (* (/ 1 (- c 2)) (- ssy (/ (sq ssxy) ssx))))]
            (* steyx (sqrt (+ (/ 1 c) (/ (sq (- x mx)) ssx))))))))))

(defn standard-error-prediction'
  [fx fy x coll]
  (let [coll' (filter fx (filter fy coll))]
    (when-not (empty? coll')
      (let [c (count coll')
            xs (map fx coll')
            ys (map fy coll')
            mx (mean' xs)
            my (mean' ys)
            ssx (reduce + (map #(sq (- % mx)) xs))
            ssy (reduce + (map #(sq (- % my)) ys))
            ssxy (reduce + (map #(* (- %1 mx) (- %2 my)) xs ys))]
        (when (and (> c 2) (not (zero? ssx)))
          (let [steyx (sqrt (* (/ 1 (- c 2)) (- ssy (/ (sq ssxy) ssx))))]
            (* steyx (sqrt (+ 1 (/ 1 c) (/ (sq (- x mx)) ssx))))))))))

(defn skewness'
  [coll]
  (let [m (mean' coll)
        n (count coll)
        d (* (pow (reduce + (map #(pow (- % m) 2) coll)) 1.5)
             (- n 2))]
    (when-not (zero? d)
      (/ (* (reduce + (map #(pow (- % m) 3) coll))
            (* n (sqrt (dec n))))
         d))))

(defn pskewness'
  [coll]
  (let [m (mean' coll)
        v (pvariance' coll)]
    (when-not (or (nil? v) (zero? v))
      (let [s (sqrt v)]
        (mean' (map #(pow (/ (- % m) s) 3) coll))))))

(defn kurtosis'
  [coll]
  (let [m (mean' coll)
        n (count coll)
        v (variance' coll)]
    (when-not (or (nil? v) (zero? v) (< n 4))
      (- (/ (* n (inc n) (reduce + (map #(pow (- % m) 4) coll)))
            (* (- n 1) (- n 2) (- n 3) (pow v 2)))
         (/ (* 3 (sq (dec n)))
            (* (- n 2) (- n 3)))))))

(defn pkurtosis'
  [coll]
  (let [m (mean' coll)
        n (count coll)
        d (reduce + (map #(sq (- % m)) coll))]
    (when-not (zero? d)
      (- (/ (* n (reduce + (map #(pow (- % m) 4) coll)))
            (sq d))
         3))))

(defn finite?
  [x]
  #?(:clj  (or (nil? x) (Double/isFinite x))
     :cljs (or (nil? x) (js/isFinite x))))

(def numeric
  (gen/such-that finite? (gen/one-of [gen/int gen/double (gen/return nil)])))

(defspec count-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (= (transduce identity kixi/count xs)
                  (count xs)))))

(deftest count-test
  (is (zero? (transduce identity kixi/count []))))

(defspec arithmetic-mean-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/arithmetic-mean xs)
              (arithmetic-mean' (remove nil? xs))))))

(deftest arithmetic-mean-test
  (is (nil? (transduce identity kixi/arithmetic-mean []))))

(defspec geometric-mean-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/geometric-mean xs)
              (geometric-mean' (remove nil? xs))))))

(deftest geometric-mean-test
  (is (nil? (transduce identity kixi/geometric-mean [])))
  (is (zero? (transduce identity kixi/geometric-mean [0])))
  (is (nil? (transduce identity kixi/geometric-mean [-1]))))

(defspec harmonic-mean-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/harmonic-mean xs)
              (harmonic-mean' (remove nil? xs))))))

(deftest harmonic-mean-test
  (is (nil? (transduce identity kixi/harmonic-mean []))))

(defspec variance-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/variance-s xs)
              (variance' (remove nil? xs))))))

(deftest variance-test
  (is (nil?  (transduce identity kixi/variance-s [])))
  (is (zero? (transduce identity kixi/variance-s [1])))
  (is (= 2   (transduce identity kixi/variance-s [1 3])))
  (is (= 4   (transduce identity kixi/variance-s [1 3 5]))))

(defspec pvariance-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/variance-p xs)
              (pvariance' (remove nil? xs))))))

(deftest pvariance-test
  (is (nil?  (transduce identity kixi/variance-p [])))
  (is (zero? (transduce identity kixi/variance-p [1])))
  (is (= 4   (transduce identity kixi/variance-p [1 5]))))

(defspec standard-deviation-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/standard-deviation-s xs)
              (some-> (variance' (remove nil? xs)) sqrt)))))

(deftest standard-deviation-test
  (is (nil?  (transduce identity kixi/standard-deviation-s [])))
  (is (zero? (transduce identity kixi/standard-deviation-s [1])))
  (is (== 2  (transduce identity kixi/standard-deviation-s [1 3 5]))))

(defspec pstandard-deviation-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/standard-deviation-p xs)
              (some-> (pvariance' (remove nil? xs)) sqrt)))))

(deftest pstandard-deviation-test
  (is (nil?  (transduce identity kixi/standard-deviation-p [])))
  (is (zero? (transduce identity kixi/standard-deviation-p [1])))
  (is (== 2  (transduce identity kixi/standard-deviation-p [1 5]))))

(defspec standard-error-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/standard-error xs)
              (standard-error' (remove nil? xs))))))

(deftest standard-error-test
  (is (nil?   (transduce identity kixi/standard-error [])))
  (is (zero?  (transduce identity kixi/standard-error [1])))
  (is (== 1.0 (transduce identity kixi/standard-error [1 3]))))

(defspec skewness-spec
  test-opts
  (for-all [xs (gen/vector gen/int)]
    (is (=ish (transduce identity kixi/skewness-s xs)
              (skewness' xs)))))

(deftest skewness-test
  (is (nil? (transduce identity kixi/skewness-s [])))
  (is (nil? (transduce identity kixi/skewness-s [1])))
  (is (nil? (transduce identity kixi/skewness-s [1 2]))))

(defspec pskewness-spec
  test-opts
  (for-all [xs (gen/vector gen/int)]
    (is (=ish (transduce identity kixi/skewness-p xs)
              (pskewness' xs)))))

(deftest pskewness-test
  (is (nil?  (transduce identity kixi/skewness-p [])))
  (is (nil?  (transduce identity kixi/skewness-p [1])))
  (is (zero? (transduce identity kixi/skewness-p [1 2]))))

(defspec kurtosis-spec
  test-opts
  (for-all [xs (gen/vector gen/int)]
    (is (=ish (transduce identity kixi/kurtosis-s xs)
              (kurtosis' xs)))))

(deftest kurtosis-test
  (is (nil? (transduce identity kixi/kurtosis-s [])))
  (is (nil? (transduce identity kixi/kurtosis-s [1])))
  (is (nil? (transduce identity kixi/kurtosis-s [1 2])))
  (is (nil? (transduce identity kixi/kurtosis-s [1 2 3])))
  (is (not (nil? (transduce identity kixi/kurtosis-s [1 2 3 4])))))

(defspec pkurtosis-spec
  test-opts
  (for-all [xs (gen/vector gen/int)]
    (is (=ish (transduce identity kixi/kurtosis-p xs)
              (pkurtosis' xs)))))

(deftest pkurtosis-test
  (is (nil? (transduce identity kixi/kurtosis-p [])))
  (is (nil? (transduce identity kixi/kurtosis-p [1])))
  (is (not (nil? (transduce identity kixi/kurtosis-p [1 2])))))

(defspec covariance-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute covariance
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
    (is (=ish (transduce identity (kixi/covariance :x :y) coll)
              (covariance' :x :y coll)))))

(deftest covariance-test
  (is (nil?  (transduce identity (kixi/covariance :x :y) [])))
  (is (zero? (transduce identity (kixi/covariance :x :y) [{:x 1 :y 2}]))))

(defspec pcovariance-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute covariance
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
    (is (=ish (transduce identity (kixi/covariance-p :x :y) coll)
              (pcovariance' :x :y coll)))))

(deftest pcovariance-test
  (is (nil?  (transduce identity (kixi/covariance-p :x :y) [])))
  (is (zero? (transduce identity (kixi/covariance-p :x :y) [{:x 1 :y 2}]))))

(defspec covariance-matrix-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation matrix
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y :z]) gen/int))]
           (is (=ish (transduce identity (kixi/covariance-matrix {:x :x :y :y :z :z}) coll)
                     (covariance-matrix' coll)))))

(defspec correlation-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
           (is (=ish (transduce identity (kixi/correlation :x :y) coll)
                     (correlation' :x :y coll)))))

(deftest correlation-test
  (is (nil? (transduce identity (kixi/correlation :x :y) [])))
  (is (nil? (transduce identity (kixi/correlation :x :y) [{:x 1 :y 2}]))))

(defspec cramers-v-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
           (is (=ish (transduce identity (kixi/cramers-v :x :y) coll)
                     (cramers-v' :x :y coll)))))

(defspec correlation-matrix-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation matrix
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y :z]) gen/int))]
           (is (=ish (transduce identity (kixi/correlation-matrix {:x :x :y :y :z :z}) coll)
                     (correlation-matrix' coll)))))

(defspec simple-linear-regression-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
    (is (=ish (transduce identity (kixi/simple-linear-regression :x :y) coll)
              (simple-linear-regression' :x :y coll)))))

(deftest simple-linear-regression-test
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [])))
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [{:x 1 :y 2}]))))

(defspec sum-squares-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (= (kixi/standard-error-estimate
            (transduce identity (kixi/sum-squares :x :y) coll) x)
           (transduce identity (kixi/standard-error-estimate :x :y x) coll)))
    (is (= (kixi/standard-error-prediction
            (transduce identity (kixi/sum-squares :x :y) coll) x)
           (transduce identity (kixi/standard-error-prediction :x :y x) coll)))))

(defspec standard-error-estimate-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (=ish (transduce identity (kixi/standard-error-estimate :x :y x) coll)
              (standard-error-estimate' :x :y x coll)))))

(defspec standard-error-prediction-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (=ish (transduce identity (kixi/standard-error-prediction :x :y x) coll)
              (standard-error-prediction' :x :y x coll)))))
