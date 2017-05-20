(ns kixi.stats.random-test
  (:require [kixi.stats.random :as sut]
            [clojure.test.check.generators :as gen]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :refer [for-all]]
                 [clojure.test :refer [is deftest]]])))

(def test-opts
  {:num-tests 100
   :par 4})

(def gen-probability
  "Returns a double between 0 and 1 inclusive"
  (gen/fmap #(* % 0.001) (gen/such-that #(<= % 1000) gen/int)))

(def gen-probabilities
  "Returns a vector of probabilities which sum to 1.0"
  (->> (gen/not-empty (gen/vector gen/s-pos-int))
       (gen/fmap (fn [vector]
                   (let [sum (apply + vector)]
                     (->> (concat vector [0 0])
                          (mapv #(double (/ % sum)))))))))

(def gen-categories
  "Returns [[categories] [probabilities]]. Probabilities sum to 1.0"
  (gen/fmap #(vector (range (count %)) %) gen-probabilities))

(defspec seeded-draws-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/draw (sut/uniform a b) {:seed seed})
           (sut/draw (sut/uniform a b) {:seed seed})))
    (is (= (sut/draw (sut/bernoulli p) {:seed seed})
           (sut/draw (sut/bernoulli p) {:seed seed})))
    (is (= (sut/draw (sut/binomial {:n n :p p}) {:seed seed})
           (sut/draw (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/draw (sut/categorical ks ps) {:seed seed})
           (sut/draw (sut/categorical ks ps) {:seed seed})))))

(defspec seeded-samples-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/sample n (sut/uniform a b) {:seed seed})
           (sut/sample n (sut/uniform a b) {:seed seed})))
    (is (= (sut/sample n (sut/bernoulli p) {:seed seed})
           (sut/sample n (sut/bernoulli p) {:seed seed})))
    (is (= (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})
           (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/sample n (sut/categorical ks ps) {:seed seed})
           (sut/sample n (sut/categorical ks ps) {:seed seed})))))

(defspec sample-summary-returns-categorical-sample-frequencies
  test-opts
  (for-all [seed gen/int
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (let [empty-bernoulli-counts {true 0 false 0}
          empty-category-counts (zipmap ks (repeat 0))]
      (is (= (sut/sample-summary n (sut/bernoulli p) {:seed seed})
             (->> (sut/sample n (sut/bernoulli p) {:seed seed})
                  (frequencies)
                  (merge empty-bernoulli-counts))))
      (is (= (sut/sample-summary n (sut/categorical ks ps) {:seed seed})
             (->> (sut/sample n (sut/categorical ks ps) {:seed seed})
                  (frequencies)
                  (merge empty-category-counts)))))))

(defspec uniform-does-not-exceed-bounds
  test-opts
  (for-all [[a b] (->> (gen/tuple gen/int gen/int)
                       (gen/such-that (fn [[a b]] (not= a b)))
                       (gen/fmap sort))]
    (let [draw (sut/draw (sut/uniform a b))]
      (is (and (<= a draw) (< draw b))))))

(defspec bournoulli-returns-boolean
  test-opts
  (for-all [p gen-probability]
           (is (contains? #{true false} (sut/draw (sut/bernoulli p))))))

(defspec binomial-returns-integers
  test-opts
  (for-all [n gen/nat
            p gen-probability]
    (is (integer? (sut/draw (sut/binomial {:n n :p p}))))))

(defspec normal-returns-floats
  test-opts
  (for-all [mu (gen/double* {:infinite? false :NaN? false})
            sd (gen/double* {:infinite? false :NaN? false :min 0})]
    (is (float? (sut/draw (sut/normal {:mu mu :sd sd}))))))

(defspec categorical-returns-supplied-categories
  test-opts
  (for-all [[ks ps] gen-categories]
    (is (contains? (set ks) (sut/draw (sut/categorical ks ps))))))

(defspec bernoulli-probabilities-are-well-behaved
  test-opts
  (for-all [seed gen/int]
    (is (false? (sut/draw (sut/bernoulli 0.0) {:seed seed})))
    (is (true? (sut/draw (sut/bernoulli 1.0) {:seed seed})))))
