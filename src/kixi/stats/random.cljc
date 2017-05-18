(ns kixi.stats.random
  (:refer-clojure :exclude [shuffle rand-int])
  (:require [kixi.stats.utils :refer [log sqrt cos PI]]
            [clojure.test.check.random :refer [make-random rand-double rand-long split split-n]]))

;;;; Randomness helpers

(defn ^:no-doc swap
  [coll [i1 i2]]
  (assoc coll i2 (coll i1) i1 (coll i2)))

(defn ^:no-doc rand-int
  [a b rng]
  (let [r (* (rand-double rng) (- b a))]
    (int (+ a r))))

(defn ^:no-doc rand-int-tuple
  [a b rng]
  (let [[r1 r2] (split rng)]
    [(rand-int a b r1) (rand-int a b r2)]))

(def ^:no-doc next-rng
  (comp first split))

(defn ^:no-doc shuffle
  [coll rng]
  (let [coll (if (vector? coll) coll (vec coll))
        n (count coll)]
    (->> (split-n rng (rand-int 0 (* 2 n) rng))
         (map #(rand-int-tuple 0 n %))
         (reduce swap coll))))


;;;; Protocols and protocol helpers

(defprotocol ^:no-doc ISampleable
  (sample-1 [this rng])
  (sample-n [this n rng]))

(defprotocol ^:no-doc IDiscrete
  (sample-frequencies [this n rng]))

(defn ^:no-doc sampleable->seq
  ([^kixi.stats.random.ISampleable distribution]
   (sampleable->seq distribution (make-random)))
  ([^kixi.stats.random.ISampleable distribution rng]
   (lazy-seq
    (let [[r1 r2] (split rng)]
      (cons (sample-1 distribution r1)
            (sampleable->seq distribution r2))))))

(defn ^:no-doc default-sample-n
  [^kixi.stats.random.ISampleable distribution n rng]
  (take n (sampleable->seq distribution rng)))

(declare ->Binomial)

(defn ^:no-doc categorical-sample
  [category-probs n rng]
  (loop [coll '() n n
         rem 1 rng rng
         categories (sequence category-probs)]
    (if (seq categories)
      (let [[c p] (first categories)
            x (sample-1 (->Binomial n (/ p rem)) rng)]
        (recur (concat coll (repeat x c)) (- n x)
               (- rem p) (next-rng rng)
               (rest categories)))
      coll)))


;;;; Protocol implementations

(deftype ^:no-doc Uniform
    [a b]
    ISampleable
    (sample-1 [this rng]
      (+ (* (rand-double rng) (- b a)) a))
    (sample-n [this n rng]
      (map #(sample-1 this %) (split-n rng n)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Binomial
    [n p]
    ISampleable
    (sample-1 [this rng]
      (loop [i 0 rng rng result 0]
        (if (< i n)
          (recur (inc i) (next-rng rng)
                 (if (< (rand-double rng) p)
                   (inc result)
                   result))
          result)))
    (sample-n [this n rng]
      (lazy-seq
       (if (pos? n)
         (let [[r1 r2] (split rng)]
           (cons (sample-1 this r1)
                 (sample-n this (dec n) r2)))
         nil)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Bernoulli
    [p]
    ISampleable
    (sample-1 [this rng]
      (<= (rand-double rng) p))
    (sample-n [this n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        (-> (concat (repeat v true)
                    (repeat (- n v) false))
            (shuffle rng))))
    IDiscrete
    (sample-frequencies [this n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        {true v false (- n v)}))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Normal
    [mu sd]
    ISampleable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)]
        (+ (* (sqrt (* -2 (log (rand-double r1))))
              (cos (* 2 PI (rand-double r2)))
              sd)
           mu)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Categorical
    [category-probs]
    ISampleable
    (sample-1 [this rng]
      (first (categorical-sample category-probs 1 rng)))
    (sample-n [this n rng]
      (shuffle (categorical-sample category-probs n rng) rng))
    IDiscrete
    (sample-frequencies [this n rng]
      (loop [coll (transient {}) n n
             rem 1 rng rng
             categories (sequence category-probs)]
        (if (seq categories)
          (let [[c p] (first categories)
                x (sample-1 (->Binomial n (/ p rem)) rng)]
            (recur (assoc! coll c x) (- n x)
                   (- rem p) (next-rng rng)
                   (rest categories)))
          (persistent! coll))))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))


;;;; Public API

(defn uniform
  "Returns a uniform distribution.
  Params: a ∈ R, b ∈ R"
  [a b]
  (->Uniform a b))

(defn bernoulli
  "Returns a Bernoulli distribution.
  Params: p ∈ [0 1]"
  [p]
  (->Bernoulli p))

(defn binomial
  "Return a binomial distribution.
  Params: {:n ∈ N_0, :p ∈ [0 1]}"
  [{:keys [n p]}]
  (->Binomial n p))

(defn normal
  "Returns a normal distribution.
  Params: {:mu ∈ R, :sd ∈ R}"
  [{:keys [mu sd]}]
  (->Normal mu sd))

(defn categorical
  "Returns a categorical distribution.
  Params: {k1 ∈ [0 1], ..., kn ∈ [0 1]}
  where keys k1...n are the categories
  and the vals are in the range 0..1.
  Vals should sum to 1.0"
  [category-probs]
  (->Categorical category-probs))

(defn draw
  "Returns a single sample from the distribution"
  ([^kixi.stats.random.ISampleable distribution]
   (draw distribution {}))
  ([^kixi.stats.random.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-1 distribution rng))))

(defn sample
  "Returns n samples from the distribution"
  ([n ^kixi.stats.random.ISampleable distribution]
   (sample n distribution {}))
  ([n ^kixi.stats.random.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-n distribution n rng))))

(defn sample-summary
  "Returns a summary count of each class
  for a sample of a given length from a discrete distribution
  such as the Bernoulli or categorical"
  ([n ^kixi.stats.random.IDiscrete distribution]
   (sample-summary n distribution {}))
  ([n ^kixi.stats.random.IDiscrete distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-frequencies distribution n rng))))
