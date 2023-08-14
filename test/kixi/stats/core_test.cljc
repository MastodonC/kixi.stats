(ns kixi.stats.core-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [kixi.stats.core :as kixi]
            [kixi.stats.estimate :as estimate]
            [kixi.stats.test :refer [simple-z-test z-test p-value]]
            [kixi.stats.test-helpers :as t :refer [=ish approx= numeric]]
            [kixi.stats.math :refer [sq pow sqrt root]]
            [kixi.stats.protocols :as p]))

(def test-opts {:num-tests 100
                :par       4})

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
            c (count coll')
            c' (dec c)]
        (if (pos? c')
          (/ (reduce + (map #(* (- (fx %) mean-x)
                                (- (fy %) mean-y))
                            coll'))
             c')
          0)))))

(defn pcovariance'
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (if (empty? coll')
      nil
      (let [mean-x (mean' (map fx coll'))
            mean-y (mean' (map fy coll'))
            c (double (count coll'))]
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
  "http://mathworld.wolfram.com/CorrelationCoefficient.html"
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (when-not (empty? coll')
      (let [xs (map fx coll')
            ys (map fy coll')
            mx (mean' xs)
            my (mean' ys)
            mxs (map #(- % mx) xs)
            mys (map #(- % my) ys)
            d (sqrt (* (reduce + (map * mxs mxs))
                       (reduce + (map * mys mys))))]
        (when-not (zero? d)
          (/ (reduce + (map * mxs mys)) d))))))

(defn r-squared'
  [fy-hat fy coll]
  (let [coll' (filter fy-hat (filter fy coll))
        m (mean' (map fy coll'))
        fe (fn [x] (- (fy x) (fy-hat x)))
        vare (apply + (map (comp sq fe) coll'))
        vary (apply + (map (comp sq #(- (fy %) m)) coll'))]
    (when (and vare vary (pos? vary))
      (double (- 1 (/ vare vary))))))

(defn mse'
  [fy-hat fy coll]
  (let [coll' (filter fy-hat (filter fy coll))
        n (count coll')
        fe (fn [x] (- (fy x) (fy-hat x)))]
    (when (pos? n)
      (/ (apply + (map (comp sq fe) coll')) n))))

(defn cramers-v'
  [fx fy coll]
  (let [[x-counts y-counts xy-counts] (reduce
                                       (fn [[x-acc y-acc xy-acc] element]
                                         (let [value-x (fx element)
                                               value-y (fy element)
                                               increment-count (fn [m k] (update m k (fnil inc 0)))
                                               x-acc' (increment-count x-acc value-x)
                                               y-acc' (increment-count y-acc value-y)
                                               xy-acc' (increment-count xy-acc [value-x value-y])]
                                           [x-acc' y-acc' xy-acc']))
                                       [{} {} {}]
                                       coll)
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

(defn cross-tabulate'
  [f1 f2 coll]
  (->> coll
       (map #(vector (f1 %) (f2 %)))
       (frequencies)))

(defspec count-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (= (transduce identity kixi/count xs)
           (count xs)))))

(deftest count-test
  (is (zero? (transduce identity kixi/count []))))

#?(:clj
   (defspec median-spec
     test-opts
     (for-all [xs (gen/vector numeric)]
       (is (=ish (transduce identity kixi/median xs)
                 (t/quantile' 0.5 xs))))))

#?(:clj
   (deftest median-test
     (is (nil? (transduce identity kixi/median [])))
     (is (zero? (transduce identity kixi/median [0])))
     (is (= 42.0 (transduce identity kixi/median [42])))
     (is (= 4.5 (transduce identity kixi/median (range 10))))))

#?(:clj
   (deftest iqr-test
     (is (= 5.0 (transduce identity kixi/iqr (range 10))))))

#?(:clj
   (deftest histogram-test
     (let [histogram (transduce identity kixi/histogram (range 10))]
       (is (satisfies? kixi.stats.protocols/PBounded histogram))
       (is (satisfies? kixi.stats.protocols/PQuantile histogram)))))

#?(:clj
   (deftest summary-test
     (is (= (transduce identity kixi/summary (range 10))
            {:min 0.0, :q1 2.0, :median 4.5, :q3 7.0, :max 9.0, :iqr 5.0}))))

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
  (is (= 2.0 (transduce identity kixi/variance-s [1 3])))
  (is (= 4.0 (transduce identity kixi/variance-s [1 3 5]))))

(defspec pvariance-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (is (=ish (transduce identity kixi/variance-p xs)
              (pvariance' (remove nil? xs))))))

(deftest pvariance-test
  (is (nil?  (transduce identity kixi/variance-p [])))
  (is (zero? (transduce identity kixi/variance-p [1])))
  (is (= 4.0 (transduce identity kixi/variance-p [1 5]))))

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

(defspec r-squared-spec
  test-opts
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation
    (is (=ish (transduce identity (kixi/r-squared :x :y) coll)
              (r-squared' :x :y coll)))))

(deftest r-squared-test
  (is (nil? (transduce identity (kixi/r-squared :x :y) [])))
  (is (nil? (transduce identity (kixi/r-squared :x :y) [{:x 1 :y 2}])))
  (is (=ish 0.625 (transduce identity (kixi/r-squared :x :y) [{:x 1 :y 1} {:x 2 :y 3} {:x 3 :y 3}])))
  (is (=ish -0.2 (transduce identity (kixi/r-squared :x :y) [{:x 1 :y 1} {:x 3 :y 2} {:x 5 :y 3} {:x 5 :y 4}]))))

(deftest adjusted-r-squared-test
  (is (nil? (transduce identity (kixi/adjusted-r-squared :x :y 1) [])))
  (is (nil? (transduce identity (kixi/adjusted-r-squared :x :y 1) [{:x 1 :y 2}])))
  (is (nil? (transduce identity (kixi/adjusted-r-squared :x :y 2) [{:x 1 :y 1} {:x 2 :y 3} {:x 3 :y 3}])))
  (is (=ish -0.8 (transduce identity (kixi/adjusted-r-squared :x :y 1) [{:x 1 :y 1} {:x 3 :y 2} {:x 5 :y 3} {:x 5 :y 4}])))
  (is (=ish -2.6 (transduce identity (kixi/adjusted-r-squared :x :y 2) [{:x 1 :y 1} {:x 3 :y 2} {:x 5 :y 3} {:x 5 :y 4}]))))

(defspec mse-spec
  test-opts
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
    (is (=ish (transduce identity (kixi/mse :x :y) coll)
              (mse' :x :y coll)))))

(defspec rmse-spec
  test-opts
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
    (is (=ish (transduce identity (kixi/rmse :x :y) coll)
              (some-> (mse' :x :y coll) sqrt)))))

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
    (is (=ish (some-> (transduce identity (kixi/simple-linear-regression :x :y) coll) p/parameters)
              (simple-linear-regression' :x :y coll)))))

(deftest simple-linear-regression-test
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [])))
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [{:x 1 :y 2}]))))

(defspec sum-squares-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (= (estimate/regression-standard-error
            (transduce identity (kixi/sum-squares :x :y) coll) x)
           (transduce identity (kixi/regression-standard-error :x :y x) coll)))
    (is (= (estimate/regression-prediction-standard-error
            (transduce identity (kixi/sum-squares :x :y) coll) x)
           (transduce identity (kixi/regression-prediction-standard-error :x :y x) coll)))))

(defspec standard-error-estimate-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (=ish (transduce identity (kixi/regression-standard-error :x :y x) coll)
              (standard-error-estimate' :x :y x coll)))))

(defspec standard-error-prediction-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            x gen/int]
    (is (=ish (transduce identity (kixi/regression-prediction-standard-error :x :y x) coll)
              (standard-error-prediction' :x :y x coll)))))

(deftest cross-tabulate-test
  (let [xtab (transduce identity (kixi/cross-tabulate :v1 :v2) (concat (repeat 3 {:v1 :a :v2 :x})
                                                                       (repeat 2 {:v1 :b :v2 :y})
                                                                       (repeat 1 {:v1 :b :v2 :z})))]
    (is (= 6 (p/grand-total xtab)))
    (is (= [2 3] (p/size xtab)))
    (is (= [{:a 3, :b 3} {:x 3, :y 2, :z 1}] (p/margin-totals xtab)))))

(deftest chi-squared-test-test
  (let [xs (concat (repeat 2 {:v1 :a :v2 :x})
                   (repeat 4 {:v1 :a :v2 :y})
                   (repeat 6 {:v1 :b :v2 :x})
                   (repeat 8 {:v1 :b :v2 :y}))]
    (is (=ish (p-value (transduce identity (kixi/chi-squared-test :v1 :v2) xs))
              0.6903283294641935))))

(defspec simple-z-test-plugin-spec
  test-opts
  (for-all [coll (gen/vector gen/int 2 100)
            alternate (gen/elements [:<> :> :<])
            x gen/int]
    (is (=ish (some-> (transduce identity (kixi/simple-z-test {:mu x}) coll)
                      (p-value alternate))
              (some-> (simple-z-test {:mu x :sd (sqrt (variance' coll))}
                                     {:mean (mean' coll) :n (count coll)})
                      (p-value alternate))))))

(defspec simple-z-test-spec
  test-opts
  (for-all [coll (gen/vector gen/int 2 100)
            alternate (gen/elements [:<> :> :<])
            sd gen/pos-int
            x gen/int]
    (is (=ish (some-> (transduce identity (kixi/simple-z-test {:mu x :sd sd}) coll)
                      (p-value alternate))
              (some-> (simple-z-test {:mu x :sd sd}
                                     {:mean (mean' coll) :n (count coll)})
                      (p-value alternate))))))

(defspec z-test-test
  test-opts
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))
            alternate (gen/elements [:<> :> :<])]
    (is (=ish (some-> (transduce identity (kixi/z-test :x :y) coll)
                      (p-value alternate))
              (let [xs (keep identity (map :x coll))
                    x-var (variance' xs)
                    ys (keep identity (map :y coll))
                    y-var (variance' ys)]
                (when (and (> (count xs) 1)
                           (> (count ys) 1))
                  (some-> (z-test {:mean (mean' xs)
                                   :sd (if x-var (sqrt x-var) 0)
                                   :n (count xs)}
                                  {:mean (mean' ys)
                                   :sd (if y-var (sqrt y-var) 0)
                                   :n (count ys)})
                          (p-value alternate))))))))

(deftest t-test-test
  ;; Example 2 from https://en.wikipedia.org/wiki/Welch%27s_t-test
  (let [samples (concat (map (partial hash-map :a) [17.2 20.9 22.6 18.1 21.7 21.4 23.5 24.2 14.7 21.8])
                        (map (partial hash-map :b) [21.5 22.8 21.0 23.0 21.6 23.6 22.5 20.7 23.4 21.8
                                                    20.7 21.7 21.5 22.5 23.6 21.5 22.5 23.5 21.5 21.8]))
        result (transduce identity (kixi/t-test :a :b) samples)
        =ish (approx= 1e-6)]
    (is (=ish (p-value result) 0.1488416))))

(deftest min-test
  (is (= 1.0 (transduce identity kixi/min [2 1 nil 5 3 ])))
  (is (nil? (transduce identity kixi/min []))))

(deftest max-test
  (is (= 5.0 (transduce identity kixi/max [2 1 nil 5 3])))
  (is (nil? (transduce identity kixi/max []))))

(deftest proportion-test
  (is (=ish (transduce identity (kixi/proportion neg?) [1 -1 3 5]) 0.25)))

(deftest monoid-test
  (is (= :init ((kixi/monoid identity :init)))))
