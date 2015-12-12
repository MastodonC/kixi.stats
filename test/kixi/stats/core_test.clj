(ns kixi.stats.core-test
  (:refer-clojure :rename {count count'})
  (:require [kixi.stats.core :refer :all]
            [kixi.stats.utils :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check [clojure-test :refer :all]
                                [generators :as gen]
             [properties :as prop]]))

(def test-opts {:num-tests 100
                :par       4})

(defn mean'
  [coll]
  (let [c (count' coll)]
    (when (pos? c)
      (/ (reduce + coll) c))))

(defn variance'
  [coll]
  (let [c (count' coll)]
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
  (let [c (count' coll)]
    (when (pos? c)
      (/ (->> coll
              (map #(sq (- % (mean' coll))))
              (reduce +))
         (count' coll)))))

(defn covariance'
  [fx fy coll]
  (let [coll (filter fx (filter fy coll))]
    (if (empty? coll)
      nil
      (let [mean-x (mean' (map fx coll))
            mean-y (mean' (map fy coll))]
        (/ (reduce + (map #(* (- (fx %) mean-x)
                              (- (fy %) mean-y))
                          coll))
           (count' coll))))))

(defn correlation'
  [fx fy coll]
  "http://mathworld.wolfram.com/CorrelationCoefficient.html"
  (let [coll (filter fx (filter fy coll))]
    (when-not (empty? coll)
      (let [xs (map fx coll)
            ys (map fy coll)
            mx (mean' (map fx coll))
            my (mean' (map fy coll))
            mxs (map #(- % mx) xs)
            mys (map #(- % my) ys)]
        (try
          (/ (reduce + (map * mxs mys))
             (sqrt (* (reduce + (map * mxs mxs))
                      (reduce + (map * mys mys)))))
          (catch ArithmeticException e
            nil))))))

(defn finite?
  [x]
  (Double/isFinite x))

(def numerics
  (gen/such-that finite? (gen/one-of [gen/int gen/double])))

(defspec count-spec
  test-opts
  (prop/for-all [coll (gen/vector numerics)]
                (is (= (transduce identity count coll)
                       (count' coll)))))

(deftest count-spec
  (is (zero? (transduce identity count []))))

(defspec mean-spec
  test-opts
  (prop/for-all [ints (gen/vector numerics)]
                (is (= (transduce identity mean ints)
                       (mean' ints)))))

(deftest mean-test
  (is (nil? (transduce identity mean []))))

(defspec variance-spec
  test-opts
  (prop/for-all [ints (gen/vector gen/int)]
                (is (= (transduce identity variance ints)
                       (variance' ints)))))

(deftest variance-test
  (is (nil?  (transduce identity variance [])))
  (is (zero? (transduce identity variance [1])))
  (is (= 2   (transduce identity variance [1 3])))
  (is (= 4   (transduce identity variance [1 3 5]))))

(defspec pvariance-spec
  test-opts
  (prop/for-all [ints (gen/vector gen/int)]
                (is (= (transduce identity pvariance ints)
                       (pvariance' ints)))))

(deftest pvariance-test
  (is (nil?  (transduce identity pvariance [])))
  (is (zero? (transduce identity pvariance [1])))
  (is (= 4   (transduce identity pvariance [1 5]))))

(defspec standard-deviation-spec
  test-opts
  (prop/for-all [ints (gen/vector gen/int)]
                (is (= (transduce identity standard-deviation ints)
                       (some-> (variance' ints) sqrt)))))

(deftest standard-deviation-test
  (is (nil?  (transduce identity standard-deviation [])))
  (is (zero? (transduce identity standard-deviation [1])))
  (is (== 2  (transduce identity standard-deviation [1 3 5]))))

(defspec pstandard-deviation-spec
  test-opts
  (prop/for-all [ints (gen/vector gen/int)]
                (is (= (transduce identity pstandard-deviation ints)
                       (some-> (pvariance' ints) sqrt)))))

(deftest pstandard-deviation-test
  (is (nil?  (transduce identity pstandard-deviation [])))
  (is (zero? (transduce identity pstandard-deviation [1])))
  (is (== 2  (transduce identity pstandard-deviation [1 5]))))

(defspec covariance-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute covariance
  (prop/for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
                (is (= (transduce identity (covariance :x :y) coll)
                       (covariance' :x :y coll)))))

(deftest covariance-test
  (is (nil?  (transduce identity (covariance :x :y) [])))
  (is (zero? (transduce identity (covariance :x :y) [{:x 1 :y 2}]))))

(defspec correlation-spec
  test-opts
  ;; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation
  (prop/for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
                (is (= (transduce identity (correlation :x :y) coll)
                       (correlation' :x :y coll)))))

(deftest correlation-test
  (is (nil? (transduce identity (correlation :x :y) [])))
  (is (nil? (transduce identity (correlation :x :y) [{:x 1 :y 2}]))))
