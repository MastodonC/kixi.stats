(ns kixi.stats.test-test
  (:require [kixi.stats.test :as sut]
            [kixi.stats.core :refer [cross-tabulate]]
            [kixi.stats.test-helpers :refer [=ish]]
            #?@(:cljs
                [[cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test :refer [is deftest]]])))

(deftest chi-squared-test-test
  (let [xtab (transduce identity (cross-tabulate :x :y)
                        (concat (repeat 2 {:x :a :y :x})
                                (repeat 4 {:x :a :y :y})
                                (repeat 6 {:x :b :y :x})
                                (repeat 8 {:x :b :y :y})))]
    (is (=ish (sut/chi-squared-test xtab)
              {:p-value 0.6903283294641935, :X-sq 0.1587301587301587, :dof 1}))))

(deftest simple-z-test-test
  (let [p 0.0067167326028858]
    (is (=ish (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55} {:tails :both})
              {:p-value (* 2 p)}))
    (is (=ish (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55} {:tails :lower})
              {:p-value p}))
    (is (=ish (sut/simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55} {:tails :upper})
              {:p-value (- 1 p)}))
    (is (=ish (sut/simple-z-test {:mu 96 :sd 12} {:mean 100 :n 55} {:tails :upper})
              {:p-value p}))))
