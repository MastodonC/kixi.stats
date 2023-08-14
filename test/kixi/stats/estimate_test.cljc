(ns kixi.stats.estimate-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [kixi.stats.estimate :as sut]
            [kixi.stats.digest :as digest]
            [kixi.stats.protocols :as p]
            [kixi.stats.test-helpers :refer [=ish approx= seq=]]))

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

(deftest linear-regression-ci-test
  ;; Example from http://www.real-statistics.com/regression/confidence-and-prediction-intervals/
  (let [xs [5 23 25 48 17 8 4 26 11 19 14 35 29 4 23]
        ys [80 78 60 53 85 84 73 79 81 75 68 72 58 92 65]
        alpha 0.05
        sum-squares (transduce identity (digest/sum-squares first last) (map vector xs ys))
        model (sut/simple-linear-regression sum-squares)
        ci (sut/regression-confidence-interval sum-squares 20 alpha)
        ci-intercept (sut/regression-confidence-interval sum-squares 0 alpha)]
    (is (=ish (:n sum-squares) 15.0))
    (is (=ish (:x-bar sum-squares) 19.4))
    (is (=ish (:y-bar sum-squares) 73.5333333333333))
    (is (=ish (p/measure model 20)
              73.15641309019463))
    (is (=ish (p/lower ci) 68.70256961645774))
    (is (=ish (p/upper ci) 77.61025656393153))
    (is (=ish (p/lower ci-intercept) 77.28074486049653))
    (is (=ish (p/upper ci-intercept) 94.16009752913939))))

(deftest linear-regression-prediction-ci-test
  ;; Example from https://newonlinecourses.science.psu.edu/stat462/node/128/
  (let [xs [7.39 7.58 7.94 8.02 8.03 8.19 8.28 8.37 8.45 8.54 8.63 8.82 8.84 8.88 8.9 8.92 9.06 9.23 9.31 9.36 9.42 9.66
            9.74 9.76 9.77 9.78 9.84 9.89 9.89 9.97 10.02 10.05 10.23 10.24 10.3 10.33 10.39 10.42 10.47 10.73 10.79
            10.9 11.03 11.07 11.15 11.18 11.18 11.2 11.48 11.62 11.65 11.77 11.8 12.01 12.07 12.78 13.59 13.95]
        ys [4.2 3.7 3.5 2.1 3.5 3.2 3.9 5.5 3.4 2.5 3.1 1.6 6.3 4.4 2.9 1.3 4.2 4.3 4.5 4.8
            4.3 4.4 6.3 5.1 5.3 5 5.2 4.9 4.3 2.8 4.4 4.5 4.9 4.8 5.1 5 4.3 3.4 4.1 3.9 2.9
            5.5 5 4.9 3.9 5.4 5.7 5.7 5.6 6.4 4.4 5.3 5.7 4.8 7.8 7.7 6.1 6.6]
        alpha 0.05
        sum-squares (transduce identity (digest/sum-squares first last) (map vector xs ys))
        model (sut/simple-linear-regression sum-squares)
        ci (sut/regression-confidence-interval sum-squares 10 alpha)
        ci-pred (sut/regression-prediction-interval sum-squares 10 alpha)]
    (is (=ish (p/lower ci) 4.259205192881628))
    (is (=ish (p/upper ci) 4.798485908111156))
    (is (=ish (p/lower ci-pred) 2.4589104495031533))
    (is (=ish (p/upper ci-pred) 6.598780651489631))
    (is (=ish (p/measure model 10) 4.528845550496392))))
