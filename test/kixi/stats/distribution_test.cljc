(ns kixi.stats.distribution-test
  (:require [kixi.stats.distribution :as sut]
            [kixi.stats.core :as kixi]
            [kixi.stats.math :refer [gamma log equal]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check]
            [kixi.stats.test-helpers :refer [=ish numeric cdf' quantile']]
            #?@(:cljs
                [[clojure.test.check.clojure-test :refer-macros [defspec]]
                 [clojure.test.check.properties :as prop :refer-macros [for-all]]
                 [cljs.test :refer-macros [is deftest]]]
                :clj
                [[clojure.test.check.clojure-test :refer [defspec]]
                 [clojure.test.check.properties :as prop :refer [for-all]]
                 [clojure.test :refer [is deftest]]])))

(def test-opts
  {:num-tests 100
   :par 4})

(def gen-probability
  "Returns a double between 0 and 1 inclusive"
  (gen/fmap #(* % 0.001) (gen/such-that #(<= % 1000) gen/nat)))

(def gen-small-n
  (gen/choose 1 10))

(def gen-rate
  (gen/such-that pos? gen-probability))

(def gen-alphas
  "Returns a vector of alphas"
  (gen/vector gen/s-pos-int 1 100))

(def gen-probabilities
  "Returns a vector of probabilities which sum to 1.0"
  (->> gen-alphas
       (gen/fmap (fn [vector]
                   (let [sum (apply + vector)]
                     (->> (concat vector [0 0])
                          (mapv #(double (/ % sum)))))))))

(def gen-shape
  "Returns a double > 0 and <= 10"
  (gen/fmap #(* % 0.001) (gen/choose 1 10000)))

(def gen-pos-real
  (gen/double* {:infinite? false :NaN? false :min 0.1 :max 100}))

(def gen-dof
  (gen/choose 3 1000))

(def gen-categories
  "Returns [[categories] [probabilities]]. Probabilities sum to 1.0"
  (gen/fmap #(vector (range (count %)) %) gen-probabilities))

#?(:clj
   (defspec histogram-spec
     test-opts
     (for-all [xs (gen/vector numeric)]
       (is (= (count (transduce identity kixi/histogram xs))
              (->> xs (remove nil?) count)))
       (is (= (sut/minimum (transduce identity kixi/histogram xs))
              (some->> xs (remove nil?) seq (apply min) double)))
       (is (= (sut/maximum (transduce identity kixi/histogram xs))
              (some->> xs (remove nil?) seq (apply max) double))))))

#?(:clj
   (defspec quantile-spec
     test-opts
     (for-all [xs (gen/vector numeric)
               n (gen/choose 1 100)]
       (let [q (double (/ 1 n))]
         (is (=ish (sut/quantile (transduce identity kixi/histogram xs) q)
                   (quantile' q xs)))))))

#?(:clj
   (defspec cdf-spec
     test-opts
     (for-all [xs (gen/vector (gen/choose 1 100) 3 100)
               x (gen/choose 1 100)]
       (is (=ish (sut/cdf (transduce identity kixi/histogram xs) x)
                 (cdf' x xs))))))

#?(:clj
   (defspec bounded-spec
     test-opts
     (for-all [xs (gen/vector numeric)]
       (is (= (some->> (remove nil? xs) seq (apply min) double)
              (sut/minimum (transduce identity kixi/histogram xs))))
       (is (= (some->> (remove nil? xs) seq (apply max) double)
              (sut/maximum (transduce identity kixi/histogram xs)))))))

#?(:clj
   (defspec summary-spec
     test-opts
     (for-all [xs (gen/vector numeric)]
       (let [summary (sut/summary (transduce identity kixi/histogram xs))]
         (is (= #{:min :q1 :median :q3 :max :iqr}
                (-> summary keys set)))
         (is (if (empty? (remove nil? xs))
               (every? nil? (vals summary))
               (every? number? (vals summary))))))))

(defspec seeded-draws-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            alpha gen-pos-real
            beta gen-pos-real
            k gen/s-pos-int
            d gen/s-pos-int
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/draw (sut/uniform a b) {:seed seed})
           (sut/draw (sut/uniform a b) {:seed seed})))
    (is (= (sut/draw (sut/exponential r) {:seed seed})
           (sut/draw (sut/exponential r) {:seed seed})))
    (is (= (sut/draw (sut/bernoulli p) {:seed seed})
           (sut/draw (sut/bernoulli p) {:seed seed})))
    (is (= (sut/draw (sut/binomial {:n n :p p}) {:seed seed})
           (sut/draw (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/draw (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/draw (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})
           (sut/draw (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})))
    (is (= (sut/draw (sut/beta {:alpha alpha :beta beta}) {:seed seed})
           (sut/draw (sut/beta {:alpha alpha :beta beta}) {:seed seed})))
    (is (= (sut/draw (sut/weibull {:shape alpha :scale beta}) {:seed seed})
           (sut/draw (sut/weibull {:shape alpha :scale beta}) {:seed seed})))
    (is (= (sut/draw (sut/chi-squared k) {:seed seed})
           (sut/draw (sut/chi-squared k) {:seed seed})))
    (is (= (sut/draw (sut/f k d) {:seed seed})
           (sut/draw (sut/f k d) {:seed seed})))
    (is (= (sut/draw (sut/poisson alpha) {:seed seed})
           (sut/draw (sut/poisson alpha) {:seed seed})))
    (is (= (sut/draw (sut/categorical ks ps) {:seed seed})
           (sut/draw (sut/categorical ks ps) {:seed seed})))))

(defspec seeded-samples-are-deterministic
  test-opts
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            alpha gen-pos-real
            beta gen-pos-real
            k gen/s-pos-int
            d gen/s-pos-int
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/sample n (sut/uniform a b) {:seed seed})
           (sut/sample n (sut/uniform a b) {:seed seed})))
    (is (= (sut/sample n (sut/exponential r) {:seed seed})
           (sut/sample n (sut/exponential r) {:seed seed})))
    (is (= (sut/sample n (sut/bernoulli p) {:seed seed})
           (sut/sample n (sut/bernoulli p) {:seed seed})))
    (is (= (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})
           (sut/sample n (sut/binomial {:n n :p p}) {:seed seed})))
    (is (= (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})
           (sut/sample n (sut/normal {:mu a :sd b}) {:seed seed})))
    (is (= (sut/sample n (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})
           (sut/sample n (sut/gamma {:shape s :scale (/ 0.5 r)}) {:seed seed})))
    (is (= (sut/sample n (sut/beta {:alpha alpha :beta beta}) {:seed seed})
           (sut/sample n (sut/beta {:alpha alpha :beta beta}) {:seed seed})))
    (is (= (sut/sample n (sut/weibull {:shape alpha :scale beta}) {:seed seed})
           (sut/sample n (sut/weibull {:shape alpha :scale beta}) {:seed seed})))
    (is (= (sut/sample n (sut/chi-squared k) {:seed seed})
           (sut/sample n (sut/chi-squared k) {:seed seed})))
    (is (= (sut/sample n (sut/f k d) {:seed seed})
           (sut/sample n (sut/f k d) {:seed seed})))
    (is (= (sut/sample n (sut/poisson alpha) {:seed seed})
           (sut/sample n (sut/poisson alpha) {:seed seed})))
    (is (= (sut/sample n (sut/categorical ks ps) {:seed seed})
           (sut/sample n (sut/categorical ks ps) {:seed seed})))))

(defn mean-convergence-reducer
  [mean]
  (fn
    ([] [0 0 0])
    ([[n m ss :as acc] e]
     (let [n' (inc n)
           m' (+ m (/ (- e m) n'))
           ss (+ ss (* (- e m') (- e m)))
           ci (* (/ ss n') 0.1)]
       (cond
         (> n' 10000) (reduced false)
         (and (> n'  100)
              (<= (- mean ci) m' (+ mean ci)))
         (reduced true)
         :else [n' m' ss])))
    ([acc] acc)))

(defn multivariate-reducer
  [rfs]
  (fn
    ([] (mapv #(%) rfs))
    ([acc e]
     (let [acc (mapv #(if (reduced? %2)
                        %2
                        (%1 %2 %3)) rfs acc e)
           [done run res] (reduce (fn [[done run res] x]
                                    (if (reduced? x)
                                      [(inc done) run (conj res (unreduced x))]
                                      [done (inc run) res]))
                                  [0 0 []] acc)]
       (if (or (zero? run)
               (and (= run 1) (every? true? res)))
         (reduced (mapv #(if (reduced? %1)
                           (unreduced %1)
                           false) acc))
         acc)))
    ([acc]
     (let [results (->> (mapv #(%1 %2) rfs acc)
                        (group-by identity))]
       (<= (count (get results false)) 1)))))

(defn converges-to-mean? [mean distribution]
  (if (sequential? mean)
    (let [rfs (-> (map mean-convergence-reducer mean)
                  (multivariate-reducer))]
      (transduce identity rfs distribution))
    (transduce identity (mean-convergence-reducer mean) distribution)))

(defspec sample-means-converge-to-parameter
  {:num-tests 1 :par 4}
  (for-all [seed gen/int
            a gen/int
            b gen/int
            r gen-rate
            s gen-shape
            p gen-probability
            ps gen-probabilities
            alpha gen-pos-real
            beta gen-pos-real
            n gen/nat
            k gen/s-pos-int
            d gen-dof
            small-n gen-small-n]
    (is (converges-to-mean? (+ a (/ (- b a) 2))
                            (sut/uniform a b)))
    (is (converges-to-mean? (/ 1 r)
                            (sut/exponential r)))
    (is (converges-to-mean? (* n p)
                            (sut/binomial {:n n :p p})))
    (is (converges-to-mean? a
                            (sut/normal {:mu a :sd r})))
    (is (converges-to-mean? (/ s r)
                            (sut/gamma {:shape s :scale (/ 1 r)})))
    (is (converges-to-mean? (/ alpha (+ alpha beta))
                            (sut/beta {:alpha alpha :beta beta})))
    (is (converges-to-mean? (* beta (gamma (inc (/ 1 alpha))))
                            (sut/weibull {:shape alpha :scale beta})))
    (is (converges-to-mean? (/ s r)
                            (sut/gamma {:shape s :scale (/ 1 r)})))
    (is (converges-to-mean? k
                            (sut/chi-squared k)))
    (is (converges-to-mean? (/ d (- d 2))
                            (sut/f k d)))
    (is (converges-to-mean? alpha
                            (sut/poisson alpha)))
    (is (converges-to-mean? (mapv #(* small-n %) ps)
                            (sut/multinomial small-n ps)))))

(defspec sample-summary-returns-categorical-sample-frequencies
  test-opts
  (for-all [seed gen/int
            p gen-probability
            n gen/nat
            [ks ps] gen-categories]
    (is (= (sut/sample-summary n (sut/bernoulli p) {:seed seed})
           (->> (sut/sample n (sut/bernoulli p) {:seed seed})
                (frequencies)
                (merge {true 0 false 0}))))
    (is (= (->> (sut/sample-summary n (sut/categorical ks ps) {:seed seed})
                (remove (fn [[k v]] (zero? v)))
                (into {}))
           (->> (sut/sample n (sut/categorical ks ps) {:seed seed})
                (frequencies))))))

(defspec uniform-does-not-exceed-bounds
  test-opts
  (for-all [seed gen/int
            [a b] (->> (gen/tuple gen/int gen/int)
                       (gen/such-that (fn [[a b]] (not= a b)))
                       (gen/fmap sort))]
    (let [draw (sut/draw (sut/uniform a b) {:seed seed})]
      (is (and (<= a draw) (< draw b))))))

(defspec exponential-returns-positive-floats
  test-opts
  (for-all [seed gen/int
            r gen-rate]
    (is (float? (sut/draw (sut/exponential r) {:seed seed})))
    (is (pos? (sut/draw (sut/exponential r) {:seed seed})))))

(defspec bournoulli-returns-boolean
  test-opts
  (for-all [seed gen/int
            p gen-probability]
    (is (contains? #{true false} (sut/draw (sut/bernoulli p) {:seed seed})))))

(defspec binomial-returns-integers
  test-opts
  (for-all [seed gen/int
            n gen/nat
            p gen-probability]
    (is (integer? (sut/draw (sut/binomial {:n n :p p}) {:seed seed})))))

(defspec categorical-returns-supplied-categories
  test-opts
  (for-all [seed gen/int
            [ks ps] gen-categories]
    (is (contains? (set ks) (sut/draw (sut/categorical ks ps) {:seed seed})))))

(defspec bernoulli-probabilities-are-well-behaved
  test-opts
  (for-all [seed gen/int]
    (is (false? (sut/draw (sut/bernoulli 0.0) {:seed seed})))
    (is (true? (sut/draw (sut/bernoulli 1.0) {:seed seed})))))

(defspec multinomial-sample-sums-to-n
  test-opts
  (for-all [seed gen/int
            n gen-small-n
            probs gen-probabilities]
    (is (= n (apply + (sut/draw (sut/multinomial n probs) {:seed seed}))))))

(defspec dirichlet-sample-sums-to-1
  test-opts
  (for-all [seed gen/int
            as gen-alphas]
    (is (equal 1.0 (apply + (sut/draw (sut/dirichlet as) {:seed seed})) 1e-15))))
