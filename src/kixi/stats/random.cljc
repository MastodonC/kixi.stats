(ns kixi.stats.random
  (:refer-clojure :exclude [shuffle rand-int])
  (:require [kixi.stats.math :refer [pow log sqrt exp cos sin PI]]
            [clojure.test.check.random :refer [make-random rand-double rand-long split split-n]]))

;;;; Randomness helpers

(def ^:no-doc next-rng
  (comp first split))

(defn ^:no-doc swap
  [coll [i1 i2]]
  (assoc coll i2 (coll i1) i1 (coll i2)))

(defn ^:no-doc rand-int
  [a b rng]
  (let [r (* (rand-double rng) (- b a))]
    (int (+ a r))))

(defn ^:no-doc rand-normal
  [rng]
  (let [[r1 r2] (split rng)]
    (* (sqrt (* -2 (log (rand-double r1))))
       (cos (* 2 PI (rand-double r2))))))

(defn ^:no-doc rand-gamma
  [k rng]
  (let [k' (cond-> k (< 1) inc)
        a1 (- k' (/ 1 3))
        a2 (/ 1 (sqrt (* 9 a1)))
        [r1 r2] (split rng)
        [v u] (loop [rng r1]
                (let [[r1 r2] (split rng)
                      [x v] (loop [rng r2]
                              (let [x (rand-normal rng)
                                    v (+ 1 (* a2 x))]
                                (if (<= v 0)
                                  (recur (next-rng rng))
                                  [x v])))
                      v (* v v v)
                      u (rand-double r1)]
                  (if (and (> u (- 1 (* 0.331 (pow x 4))))
                           (> (log u) (+ (* 0.5 x x)
                                         (* a1 (+ 1 (- v) (log v))))))
                    (recur (next-rng r1))
                    [v u])))]
    (if (= k k')
      (* a1 v)
      (* (pow (loop [rng r2]
                (let [r (rand-double rng)]
                  (if (> r 0) r
                      (recur (next-rng rng)))))
              (/ 1 k))
         a1 v))))

(defn ^:no-doc rand-int-tuple
  [a b rng]
  (let [[r1 r2] (split rng)]
    [(rand-int a b r1) (rand-int a b r2)]))

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
  [ks ps n rng]
  (loop [coll '() n n
         rem 1 rng rng
         ks ks ps ps]
    (if (and (seq ks) (> rem 0))
      (let [k (first ks)
            p (first ps)
            x (sample-1 (->Binomial n (/ p rem)) rng)]
        (recur (concat coll (repeat x k)) (- n x)
               (- rem p) (next-rng rng)
               (rest ks) (rest ps)))
      coll)))


;;;; Protocol implementations

(deftype ^:no-doc Uniform
    [a b]
    ISampleable
    (sample-1 [this rng]
      (+ (* (rand-double rng) (- b a)) a))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Exponential
    [rate]
    ISampleable
    (sample-1 [this rng]
      (/ (- (log (rand-double rng))) rate))
    (sample-n [this n rng]
      (default-sample-n this n rng))
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
      (default-sample-n this n rng))
    IDiscrete
    (sample-frequencies [this n' rng]
      (-> (sample-n this n' rng)
          (frequencies)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Bernoulli
    [p]
    ISampleable
    (sample-1 [this rng]
      (< (rand-double rng) p))
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
      (+ (* (rand-normal rng) sd) mu))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Gamma
    [shape scale]
    ISampleable
    (sample-1 [this rng]
      (* (rand-gamma shape rng) scale))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Beta
    [alpha beta]
    ISampleable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)
            u (rand-gamma alpha r1)]
        (/ u (+ u (rand-gamma beta r2)))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc ChiSquared
    [k]
    ISampleable
    (sample-1 [this rng]
      (* (rand-gamma (/ k 2) rng) 2))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc F
    [d1 d2]
    ISampleable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)
            x1 (* (rand-gamma (/ d1 2) r1) 2)
            x2 (* (rand-gamma (/ d2 2) r2) 2)]
        (/ (/ x1 d1) (/ x2 d2))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Poisson
    [lambda]
    ISampleable
    (sample-1 [this rng]
      (let [l (exp (- lambda))]
        (loop [p 1 k 0 rng rng]
          (let [p (* p (rand-double rng))]
            (if (> p l)
              (recur p (inc k) (next-rng rng))
              k)))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Weibull
    [shape scale]
    ISampleable
    (sample-1 [this rng]
      (* (pow (- (log (rand-double rng)))
              (/ 1 shape))
         scale))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Categorical
    [ks ps]
    ISampleable
    (sample-1 [this rng]
      (first (categorical-sample ks ps 1 rng)))
    (sample-n [this n rng]
      (shuffle (categorical-sample ks ps n rng) rng))
    IDiscrete
    (sample-frequencies [this n rng]
      (loop [coll (transient {}) n n
             rem 1 rng rng
             ks ks ps ps]
        (if (and (seq ks) (pos? rem))
          (let [k (first ks)
                p (first ps)
                x (sample-1 (->Binomial n (/ p rem)) rng)]
            (recur (assoc! coll k x) (- n x)
                   (- rem p) (next-rng rng)
                   (rest ks) (rest ps)))
          (if (seq ks)
            (-> (reduce #(assoc! %1 %2 0) coll ks)
                (persistent!))
            (persistent! coll)))))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))


;;;; Public API

(defn uniform
  "Returns a uniform distribution.
  Params: a ∈ ℝ, b ∈ ℝ"
  [a b]
  (->Uniform a b))

(defn exponential
  "Returns an exponential distribution.
  Params: rate ∈ ℝ > 0"
  [rate]
  (->Exponential rate))

(defn bernoulli
  "Returns a Bernoulli distribution.
  Params: p ∈ [0 1]"
  [p]
  (->Bernoulli p))

(defn binomial
  "Return a binomial distribution.
  Params: {:n ∈ ℕ, :p ∈ [0 1]}"
  [{:keys [n p]}]
  (->Binomial n p))

(defn normal
  "Returns a normal distribution.
  Params: {:mu ∈ ℝ, :sd ∈ ℝ}"
  [{:keys [mu sd]}]
  (->Normal mu sd))

(defn gamma
  "Returns a gamma distribution.
  Params: {:shape ∈ ℝ, :scale ∈ ℝ}"
  [{:keys [shape scale] :or {shape 1.0 scale 1.0}}]
  (->Gamma shape scale))

(defn beta
  "Returns a beta distribution.
  Params: {:alpha ∈ ℝ, :beta ∈ ℝ}"
  [{:keys [alpha beta] :or {alpha 1.0 beta 1.0}}]
  (->Beta alpha beta))

(defn weibull
  "Returns a weibull distribution.
  Params: {:shape ∈ ℝ >= 0, :scale ∈ ℝ >= 0}"
  [{:keys [shape scale] :or {shape 1.0 scale 1.0}}]
  (->Weibull shape scale))

(defn chi-squared
  "Returns a chi-squared distribution.
  Params: k ∈ ℕ > 0"
  [k]
  (->ChiSquared k))

(defn f
  "Returns an F distribution.
  Params: d1 ∈ ℕ > 0, d2 ∈ ℕ > 0"
  [d1 d2]
  (->F d1 d2))

(defn poisson
  "Returns a Poisson distribution.
  Params: lambda ∈ ℝ > 0"
  [lambda]
  (->Poisson lambda))

(defn categorical
  "Returns a categorical distribution.
  Params: [k1, ..., kn], [p1, ..., pn]
  where k1...kn are the categories
  and p1...pn are probabilities.
  Probabilities should be >= 0 and sum to 1"
  [ks ps]
  (->Categorical ks ps))

(defn draw
  "Returns a single variate from the distribution.
  An optional seed long will ensure deterministic results"
  ([^kixi.stats.random.ISampleable distribution]
   (draw distribution {}))
  ([^kixi.stats.random.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-1 distribution rng))))

(defn sample
  "Returns n variates from the distribution.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.random.ISampleable distribution]
   (sample n distribution {}))
  ([n ^kixi.stats.random.ISampleable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-n distribution n rng))))

(defn sample-summary
  "Returns a summary count of each variate for a sample
  of a given length from a discrete distribution
  such as the Bernoulli, binomial or categorical.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.random.IDiscrete distribution]
   (sample-summary n distribution {}))
  ([n ^kixi.stats.random.IDiscrete distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-frequencies distribution n rng))))
