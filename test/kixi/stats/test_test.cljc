(ns kixi.stats.test-test
  (:require [clojure.test :refer [is deftest]]
            [kixi.stats.test :as sut]
            [kixi.stats.core :refer [cross-tabulate]]
            [kixi.stats.distribution :as dist]
            [kixi.stats.test-helpers :refer [=ish]]))

(deftest chi-squared-test-test
  (let [xtab (transduce identity (cross-tabulate :x :y)
                        (concat (repeat 2 {:x :a :y :x})
                                (repeat 4 {:x :a :y :y})
                                (repeat 6 {:x :b :y :x})
                                (repeat 8 {:x :b :y :y})))]
    (is (=ish (sut/p-value (sut/chi-squared-test xtab))
              0.6903283294641935))
    (is (=ish (:statistic (sut/chi-squared-test xtab))
              0.1587301587301587))))

(deftest simple-t-test-test
  (let [p 0.00830793906096361]
    (is (=ish (sut/p-value (sut/simple-t-test {:mu 100 :sd 12} {:mean 96 :n 55}) :<>)
              (* 2 p)))
    (is (=ish (sut/p-value (sut/simple-t-test {:mu 100 :sd 12} {:mean 96 :n 55}) :<)
              p))
    (is (=ish (sut/p-value (sut/simple-t-test {:mu 100 :sd 12} {:mean 96 :n 55}) :>)
              (- 1 p)))
    (is (=ish (sut/p-value (sut/simple-t-test {:mu 96 :sd 12} {:mean 100 :n 55}) :>)
              p))))

(deftest t-test-test
  (is (=ish (sut/p-value (sut/t-test {:mean 28 :sd 14.1 :n 75} {:mean 33 :sd 9.5 :n 50}) :<>)
            0.019409259933322962)))

(deftest simple-z-test-test
  (let [p 0.0067167326028858]
    (is (=ish (sut/p-value (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55}) :<>)
              (* 2 p)))
    (is (=ish (sut/p-value (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55}) :<)
              p))
    (is (=ish (sut/p-value (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55}) :>)
              (- 1 p)))
    (is (=ish (sut/p-value (sut/simple-z-test {:mu 96 :sd 12} {:mean 100 :n 55}) :>)
              p))))

(deftest z-test-test
  (is (=ish (sut/p-value (sut/z-test {:mean 28 :sd 14.1 :n 75} {:mean 33 :sd 9.5 :n 50}) :<>)
            0.01785148959436078)))

(deftest test-result-significance
  (let [dist (dist/t {:v 10})
        t-crit 2.0
        alpha 0.05]
    (is (not (-> (sut/test-result t-crit dist)
                 (sut/significant? alpha))))
    (is (not (-> (sut/test-result t-crit dist :<>)
                 (sut/significant? alpha))))
    (is (not (-> (sut/test-result t-crit dist)
                 (sut/significant? alpha :<>))))
    (is (-> (sut/test-result t-crit dist :>)
            (sut/significant? alpha)))
    (is (-> (sut/test-result t-crit dist)
            (sut/significant? alpha :>)))
    (is (not (-> (sut/test-result t-crit dist)
                 (sut/significant? alpha :<))))
    (is (not (-> (sut/test-result t-crit dist :<)
                 (sut/significant? alpha))))
    (is (-> (sut/test-result (- t-crit) dist :<)
            (sut/significant? alpha)))
    (is (-> (sut/test-result (- t-crit) dist)
            (sut/significant? alpha :<)))))
