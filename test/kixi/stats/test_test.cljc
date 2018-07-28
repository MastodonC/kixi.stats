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
  (is (=ish (sut/simple-z-test 100 12 96 55)
            {:p-value 0.0134334652057716})))
