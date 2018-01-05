(ns kixi.stats.test-test
  (:require [kixi.stats.test :as sut]
            [kixi.stats.data :as d]
            [kixi.stats.test-helpers :refer [=ish]]
            #?@(:cljs
                [[cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test :refer [is deftest]]])))

(deftest chisq-test-test
  (is (=ish (sut/chisq-test (d/map->ITable {[:a :x] 2 [:a :y] 4 [:b :x] 6 [:b :y] 8}))
            {:p-value 0.6903283294641935, :X-sq 0.1587301587301587, :dof 1})))
