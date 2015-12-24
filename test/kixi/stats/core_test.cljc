(ns kixi.stats.core-test
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [kixi.stats.core :as kixi]
            [kixi.stats.utils :refer [sq pow sqrt]]
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

(defn approx=
  "Equal to within err fraction, or if one is zero, to within err absolute."
  ([err x y]
   (or (= x y)
       (== x y)
       (if (or (zero? x) (zero? y))
         (< (- err) (- x y) err)
         (< (- 1 err) (/ x y) (+ 1 err)))))
  ([err x y & more]
   (->> more
        (cons y)
        (every? (partial approx= err x)))))

(def =ish
  "Almost equal"
  (partial approx= 0.0000001))

(defn each? [pred & colls]
  (not-any? false? (apply map pred colls)))

(defn mean'
  [coll]
  (let [c (count coll)]
    (when (pos? c)
      (/ (reduce + coll) c))))

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

(defn covariance'
  [fx fy coll]
  (let [coll' (filter fx (filter fy coll))]
    (if (empty? coll')
      nil
      (let [mean-x (mean' (map fx coll'))
            mean-y (mean' (map fy coll'))]
        (/ (reduce + (map #(* (- (fx %) mean-x)
                              (- (fy %) mean-y))
                          coll'))
           (count coll'))))))

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
            vx (pvariance' xs)
            vxy (covariance' fx fy coll')]
        (when-not (zero? vx)
          (let [slope (/ vxy vx)]
            [(- my (* mx slope)) slope]))))))

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
  #?(:clj  (Double/isFinite x)
     :cljs (js/isFinite x)))

(def numeric
  (gen/such-that finite? (gen/one-of [gen/int gen/double])))

(defspec count-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (= (transduce identity kixi/count xs)
                  (count xs)))))

(deftest count-test
  (is (zero? (transduce identity kixi/count []))))

(defspec mean-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (=ish (transduce identity kixi/mean xs)
                     (mean' xs)))))

(deftest mean-test
  (is (nil? (transduce identity kixi/mean []))))

(defspec variance-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (=ish (transduce identity kixi/variance-s xs)
                     (variance' xs)))))

(deftest variance-test
  (is (nil?  (transduce identity kixi/variance-s [])))
  (is (zero? (transduce identity kixi/variance-s [1])))
  (is (= 2   (transduce identity kixi/variance-s [1 3])))
  (is (= 4   (transduce identity kixi/variance-s [1 3 5]))))

(defspec pvariance-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (=ish (transduce identity kixi/variance-p xs)
                     (pvariance' xs)))))

(deftest pvariance-test
  (is (nil?  (transduce identity kixi/variance-p [])))
  (is (zero? (transduce identity kixi/variance-p [1])))
  (is (= 4   (transduce identity kixi/variance-p [1 5]))))

(defspec standard-deviation-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (=ish (transduce identity kixi/standard-deviation-s xs)
                     (some-> (variance' xs) sqrt)))))

(deftest standard-deviation-test
  (is (nil?  (transduce identity kixi/standard-deviation-s [])))
  (is (zero? (transduce identity kixi/standard-deviation-s [1])))
  (is (== 2  (transduce identity kixi/standard-deviation-s [1 3 5]))))

(defspec pstandard-deviation-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
           (is (=ish (transduce identity kixi/standard-deviation-p xs)
                     (some-> (pvariance' xs) sqrt)))))

(deftest pstandard-deviation-test
  (is (nil?  (transduce identity kixi/standard-deviation-p [])))
  (is (zero? (transduce identity kixi/standard-deviation-p [1])))
  (is (== 2  (transduce identity kixi/standard-deviation-p [1 5]))))

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

(defspec covariance-matrix-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation matrix
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y :z]) gen/int))]
           (is (= (transduce identity (kixi/covariance-matrix {:x :x :y :y :z :z}) coll)
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

(defspec correlation-matrix-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation matrix
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y :z]) gen/int))]
           (is (= (transduce identity (kixi/correlation-matrix {:x :x :y :y :z :z}) coll)
                  (correlation-matrix' coll)))))

(defspec simple-linear-regression-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute linear least-squares
  (for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
           (is (each? =ish
                      (transduce identity (kixi/simple-linear-regression :x :y) coll)
                      (simple-linear-regression' :x :y coll)))))

(deftest simple-linear-regression-test
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [])))
  (is (nil? (transduce identity (kixi/simple-linear-regression :x :y) [{:x 1 :y 2}]))))
