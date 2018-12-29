(ns kixi.stats.distribution
  (:refer-clojure :exclude [shuffle rand-int])
  (:require [kixi.stats.math :refer [abs pow log sqrt exp cos sin PI log-gamma sq floor erf erfcinv] :as m]
            [kixi.stats.protocols :as p :refer [sample-1 sample-n sample-frequencies]]
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

(defn ^:no-doc btrd-f
  [k]
  (case k
    0 0.08106146679532726
    1 0.04134069595540929
    2 0.02767792568499834
    3 0.02079067210376509
    4 0.01664469118982119
    5 0.01387612882307075
    6 0.01189670994589177
    7 0.01041126526197209
    8 0.009255462182712733
    9 0.008330563433362871
    (let [k' (inc k) k2' (sq k')]
      (double (/ (- 0.08333333333333333
                    (/ (- 0.002777777777777778
                          (/ 7.936507936507937E-4 k2')) k2')) k')))))

(defn ^:no-doc rand-binomial-btrd
  "Algorithm BTRD from \"The Generation of Binomial Random Variates\", Wolfgang Hormann, p6"
  [n p rng]
  (if (> p 0.5)
    (- n (rand-binomial-btrd n (- 1 p) rng))
    (let [m (int (floor (* (inc n) p)))
          q (- 1 p)
          r (/ p q)
          nr (* (inc n) r)
          npq (* n p q)
          rnpq (sqrt npq)
          b (+ 1.15 (* 2.53 rnpq))
          a (+ -0.0873 (* 0.0248 b) (* 0.01 p))
          c (+ (* n p) 0.5)
          alpha (* (+ 2.83 (/ 5.1 b)) rnpq)
          vr (- 0.92 (/ 4.2 b))
          urvr (* 0.86 vr)]
      ;; 1
      (loop [rng rng]
        (let [v (rand-double rng)]
          (if (<= v urvr)
            (let [u (- (/ v vr) 0.43)]
              (int (floor (+ (* (+ (/ (* 2 a) (- 0.5 (abs u))) b) u) c))))
            (let [[r1 r2] (split rng)
                  ;; 2
                  [u v] (if (>= v vr)
                          [(- (rand-double r1) 0.5) v]
                          (let [u (- (/ v vr) 0.93)]
                            [(- (* 0.5 (if (pos? u) 1 -1)) u) (* (rand-double r1) vr)]))
                  ;; 3
                  us (- 0.5 (abs u))
                  k (int (floor (+ (* (+ (* 2 (/ a us)) b) u) c)))]
              (if (<= 0 k n)
                (let [v (* v (/ alpha (+ (/ a (sq us)) b)))
                      km (abs (- k m))]
                  (if (<= km 15)
                    ;; 3.1
                    (let [f 1.0
                          fx (fn [x i] (* x (- (/ nr (inc i)) r)))
                          [f v] (if (< m k)
                                  [(reduce fx f (range m k)) v]
                                  [f (reduce fx v (range k m))])]
                      (if (<= v f) k (recur r2)))
                    ;; 3.2
                    (let [v (log v)
                          p (* (/ km npq) (+ (/ (+ (* (+ (/ km 3) 0.625) km) 0.1666666666666667) npq) 0.5))
                          t (/ (* (- km) km) (* 2 npq))]
                      (cond
                        (< v (- t p)) k
                        (> v (+ t p)) (recur r2)
                        :else
                        ;; 3.3
                        (let [nm (inc (- n m))
                              h (+ (* (+ m 0.5) (log (/ (inc m) (* r nm)))) (btrd-f m) (btrd-f (- n m)))
                              ;; 3.4
                              nk (inc (- n k))]
                          (if (<= v (+ h
                                       (* (inc n) (log (/ nm nk)))
                                       (* (+ k 0.5) (log (/ (* nk r) (inc k))))
                                       (- (btrd-f k))
                                       (- (btrd-f (- n k)))))
                            k
                            (recur r2)))))))
                (recur r2)))))))))

(defn ^:no-doc rand-binomial-binv
  [n p rng]
  (if (> p 0.5)
    (- n (rand-binomial-binv n (- 1 p) rng))
    (let [cutoff 110
          q (- 1 p)
          s (/ p q)]
      (loop [ix 0 f (pow q n) u (rand-double rng)]
        (cond
          (< u f) ix
          (>= ix cutoff) (rand-binomial-binv n p (next-rng rng))
          :else (recur (inc ix) (* f s (/ (- n ix) (inc ix))) (- u f)))))))

(defn ^:no-doc rand-binomial
  [n p rng]
  (let [p (max 0.0 (min p 1.0))]
    (cond
      (= p 0.0) 0
      (= p 1.0) n
      (< (* n p) 14) (rand-binomial-binv n p rng)
      :else (rand-binomial-btrd n p rng))))

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

(defn ^:no-doc rand-beta
  [alpha beta rng]
  (let [[r1 r2] (split rng)
        u (rand-gamma alpha r1)]
    (/ u (+ u (rand-gamma beta r2)))))

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


;;;; Protocol helpers

(defn ^:no-doc sampleable->seq
  ([^kixi.stats.protocols.PRandomVariable distribution]
   (sampleable->seq distribution (make-random)))
  ([^kixi.stats.protocols.PRandomVariable distribution rng]
   (lazy-seq
    (let [[r1 r2] (split rng)]
      (cons (sample-1 distribution r1)
            (sampleable->seq distribution r2))))))

(defn ^:no-doc default-sample-n
  [^kixi.stats.protocols.PRandomVariable distribution n rng]
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

(defn ^:no-doc quantile-t
  [dof p]
  (let [x (m/ibetainv (* 2 (min p (- 1 p)))
                      (* 0.5 dof)
                      0.5)
        x (sqrt (* dof (/ (- 1 x) x)))]
    (if (> p 0.5) x (- x))))


;;;; Protocol implementations

(deftype ^:no-doc Uniform
    [a b]
    p/PRandomVariable
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
    p/PRandomVariable
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
    p/PRandomVariable
    (sample-1 [this rng]
      (rand-binomial n p rng))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n' rng]
      (-> (sample-n this n' rng)
          (frequencies)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Bernoulli
    [p]
    p/PRandomVariable
    (sample-1 [this rng]
      (< (rand-double rng) p))
    (sample-n [this n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        (-> (concat (repeat v true)
                    (repeat (- n v) false))
            (shuffle rng))))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        {true v false (- n v)}))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Normal
    [mu sd]
    p/PRandomVariable
    (sample-1 [this rng]
      (+ (* (rand-normal rng) sd) mu))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [this x]
      (* 0.5 (+ 1 (erf (/ (- x mu)
                          (sqrt (* 2 sd sd)))))))
    (quantile [this p]
      (+ (* -1.41421356237309505 sd (erfcinv (* 2 p))) mu))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc T
    [dof]
    p/PRandomVariable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)]
        (* (rand-normal r1)
           (sqrt (/ dof (* 2 (rand-gamma (* 0.5 dof) r2)))))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [this x]
      (let [dof2 (* dof 0.5)]
        (m/ibeta (/ (+ x (sqrt (+ (sq x) dof)))
                    (* 2 (sqrt (+ (sq x) dof))))
                 dof2 dof2)))
    (quantile [this p]
      (quantile-t dof p))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Gamma
    [shape scale]
    p/PRandomVariable
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
    p/PRandomVariable
    (sample-1 [this rng]
      (rand-beta alpha beta rng))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc BetaBinomial
    [n alpha beta]
    p/PRandomVariable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)
            p (rand-beta alpha beta r1)]
        (rand-binomial n p r2)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc ChiSquared
    [k]
    p/PRandomVariable
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
    p/PRandomVariable
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
    p/PRandomVariable
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
    p/PRandomVariable
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
    p/PRandomVariable
    (sample-1 [this rng]
      (first (categorical-sample ks ps 1 rng)))
    (sample-n [this n rng]
      (shuffle (categorical-sample ks ps n rng) rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n rng]
      (loop [coll (transient {}) n n
             rem 1 rng rng
             ks ks ps ps]
        (if (and (seq ks) (pos? rem))
          (let [k (first ks)
                p (first ps)
                x (rand-binomial n (/ p rem) rng)]
            (recur (assoc! coll k x) (- n x)
                   (- rem p) (next-rng rng)
                   (rest ks) (rest ps)))
          (persistent! coll))))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Multinomial
    [n ps]
    p/PRandomVariable
    (sample-1 [this rng]
      (loop [coll (transient []) n n
             rem 1 rng rng
             ps ps]
        (if (and (seq ps) (pos? rem))
          (let [p (first ps)
                x (rand-binomial n (/ p rem) rng)]
            (recur (conj! coll x) (- n x)
                   (- rem p) (next-rng rng)
                   (rest ps)))
          (persistent! coll))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n rng]
      (frequencies (sample-n this n rng)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Dirichlet
    [as]
    p/PRandomVariable
    (sample-1 [this rng]
      (let [rs (split-n rng (count as))
            xs (map #(rand-gamma %1 %2) as rs)
            s (apply + xs)]
        (mapv #(/ % s) xs)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc DirichletMultinomial
    [n as]
    p/PRandomVariable
    (sample-1 [this rng]
      (let [[r1 r2] (split rng)
            ps (sample-1 (->Dirichlet as) r1)]
        (sample-1 (->Multinomial n ps) r2)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n rng]
      (frequencies (sample-n this n rng)))
    #?@(:clj (clojure.lang.ISeq
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))


;;;; Public API

(def minimum p/minimum)

(def maximum p/maximum)

(def quantile p/quantile)

(def cdf p/cdf)

(defn iqr
  "Returns the interquartile range"
  [^kixi.stats.protocols.PQuantile distribution]
  (- (quantile distribution 0.75)
     (quantile distribution 0.25)))

(defn median
  "Returns the median"
  [^kixi.stats.protocols.PQuantile distribution]
  (quantile distribution 0.5))

(defn summary
  "Returns the 5-number distribution summary
  and the interquartile range."
  [^kixi.stats.protocols.PQuantile distribution]
  (let [q1 (quantile distribution 0.25)
        q3 (quantile distribution 0.75)]
    {:min (minimum distribution)
     :q1 q1
     :median (quantile distribution 0.5)
     :q3 q3
     :max (maximum distribution)
     :iqr (when (and q1 q3) (- q3 q1))}))

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

(defn t
  "Returns a t distribution.
  Params: dof ∈ ℕ > 0"
  [dof]
  (->T dof))

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

(defn beta-binomial
  "Returns a beta distribution.
  Params: n ∈ ℕ, {:alpha ∈ ℝ, :beta ∈ ℝ}"
  [n {:keys [alpha beta] :or {alpha 1.0 beta 1.0}}]
  (->BetaBinomial n alpha beta))

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

(defn multinomial
  "Returns a multinomial distribution.
  Params: n ∈ ℕ > 0, [p1, ..., pn]
  where p1...pn are probabilities.
  Probabilities should be >= 0 and sum to 1"
  [n ps]
  (->Multinomial n ps))

(defn dirichlet
  "Returns a Dirichlet distribution.
  Params: [a1...an] ∈ ℝ >= 0"
  [as]
  (->Dirichlet as))

(defn dirichlet-multinomial
  "Returns a Dirichlet-multinomial distribution.
  Params: n ∈ ℕ, [a1...an] ∈ ℝ >= 0"
  [n as]
  (->DirichletMultinomial n as))

(defn draw
  "Returns a single variate from the distribution.
  An optional seed long will ensure deterministic results"
  ([^kixi.stats.protocols.PRandomVariable distribution]
   (draw distribution {}))
  ([^kixi.stats.protocols.PRandomVariable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-1 distribution rng))))

(defn sample
  "Returns n variates from the distribution.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.protocols.PRandomVariable distribution]
   (sample n distribution {}))
  ([n ^kixi.stats.protocols.PRandomVariable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-n distribution n rng))))

(defn sample-summary
  "Returns a summary count of each variate for a sample
  of a given length from a discrete distribution
  such as the Bernoulli, binomial or categorical.
  An optional seed long will ensure deterministic results"
  ([n ^kixi.stats.protocols.PDiscreteRandomVariable distribution]
   (sample-summary n distribution {}))
  ([n ^kixi.stats.protocols.PDiscreteRandomVariable distribution {:keys [seed]}]
   (let [rng (if seed (make-random seed) (make-random))]
     (sample-frequencies distribution n rng))))
