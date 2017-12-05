(ns kixi.stats.digest-test
  (:require [kixi.stats.digest :as sut]
            [kixi.stats.test-helpers :refer [numeric]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test :refer [is]]))

(def test-opts {:num-tests 100
                :par       4})

(defspec t-digest-spec
  test-opts
  (for-all [xs (gen/vector numeric)]
    (let [digest (transduce identity (sut/t-digest {:compression 100}) xs)]
      (is (satisfies? kixi.stats.distribution/IBounded digest))
      (is (satisfies? kixi.stats.distribution/IQuantile digest)))))
