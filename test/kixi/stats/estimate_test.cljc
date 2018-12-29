(ns kixi.stats.estimate-test
  (:require [kixi.stats.estimate :as sut]
            [kixi.stats.digest :as digest]
            [kixi.stats.protocols :as p]
            [clojure.test.check.generators :as gen]
            [clojure.test.check]
            [kixi.stats.test-helpers :refer [approx= seq= numeric]]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :as prop :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :as prop :refer [for-all]]
                 [clojure.test :refer [is deftest]]])))

(def test-opts
  {:num-tests 100})

(def gen-x
  (gen/fmap
   #(double (/ % 1e4))
   (gen/choose -1e8 1e8)))

(def varied-xs
  (gen/such-that #(pos? (- (apply max %) (apply min %)))
                 (gen/vector-distinct gen-x {:min-elements 2 :max-elements 100})))

(defspec simple-linear-regression-spec
  test-opts
  (for-all [xs varied-xs
            offset gen-x
            slope gen-x]
    (let [ys (map #(+ (* % slope) offset) xs)
          sum-squares (transduce identity (digest/sum-squares first last) (map vector xs ys))
          model (sut/simple-linear-regression sum-squares)]
      (when-not (zero? (:ss-x sum-squares 0.0))
        (let [[offset' slope'] (p/parameters model)
              =ish (seq= (approx= 1e-10))]
          (is (=ish offset offset'))
          (is (=ish slope slope'))
          (is (=ish (map (partial p/measure model) xs) ys)))))))

(deftest linear-regression-is-nil-without-x-variance
  (let [sum-squares (transduce identity (digest/sum-squares first last) [[0 0] [0 1]])]
    (is (nil? (sut/simple-linear-regression sum-squares)))))
