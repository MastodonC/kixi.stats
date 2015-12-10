(ns kixi.stats.core-test
  (:require [kixi.stats.core :refer :all]
            [kixi.stats.utils :refer :all]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check [clojure-test :refer :all]
                                [generators :as gen]
             [properties :as prop]]))

(def test-opts {:num-tests 100
                :par       4})

(defn approx=
  "Equal to within err fraction, or if one is zero, to within err absolute."
  ([err x y]
   (or (= x y)
       (if (or (zero? x) (zero? y))
         (< (- err) (- x y) err)
         (< (- 1 err) (/ x y) (+ 1 err)))))
  ([err x y & more]
   (->> more
        (cons y)
        (every? (partial approx= err x)))))

(def =ish
  "Almost equal"
  (partial approx= 1/1000))

(defn mean'
  [coll]
  (assert (not (empty? coll)))
  (/ (reduce + coll) (count coll)))

(defn variance'
  [coll]
  (/ (->> coll
          (map #(expt (- % (mean' coll)) 2))
          (reduce +))
     (max (dec (count coll)) 1)))

(defn pvariance'
  [coll]
  (/ (->> coll
          (map #(expt (- % (mean' coll)) 2))
          (reduce +))
     (max (count coll))))

(defn covariance'
  [fx fy coll]
  (let [coll (filter fx (filter fy coll))]
    (if (empty? coll)
      nil
      (let [mean-x (mean' (map fx coll))
            mean-y (mean' (map fy coll))]
        (double (/ (reduce + (map #(* (- (fx %) mean-x)
                                      (- (fy %) mean-y))
                                  coll))
                   (count coll)))))))

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

(defspec mean-spec
  test-opts
  (prop/for-all [ints (gen/such-that not-empty (gen/vector gen/int))]
                (is (== (transduce identity mean ints)
                        (mean' ints)))))

(defspec variance-spec
  test-opts
  (prop/for-all [ints (gen/such-that not-empty (gen/vector gen/int))]
                (=ish (transduce identity variance ints)
                      (variance' ints))))

(defspec pvariance-spec
  test-opts
  (prop/for-all [ints (gen/such-that not-empty (gen/vector gen/int))]
                (=ish (transduce identity pvariance ints)
                      (pvariance' ints))))

(defspec standard-deviation-spec
  test-opts
  (prop/for-all [ints (gen/such-that not-empty (gen/vector gen/int))]
                (=ish (transduce identity standard-deviation ints)
                      (sqrt (variance' ints)))))

(defspec pstandard-deviation-spec
  test-opts
  (prop/for-all [ints (gen/such-that not-empty (gen/vector gen/int))]
                (=ish (transduce identity pstandard-deviation ints)
                      (sqrt (pvariance' ints)))))

(defspec covariance-spec
  test-opts
  ; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute covariance
  (prop/for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
                (is (= (transduce identity (covariance :x :y) coll)
                       (covariance' :x :y coll)))))

(defspec correlation-spec
  test-opts
  ; Take maps like {}, {:x 1}, {:x 2 :y 3} and compute correlation
  (prop/for-all [coll (gen/vector (gen/map (gen/elements [:x :y]) gen/int))]
                (is (= (transduce identity (correlation :x :y) coll)
                       (correlation' :x :y coll)))))
