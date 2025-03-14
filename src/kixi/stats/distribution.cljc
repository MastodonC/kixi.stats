(ns kixi.stats.distribution
  (:refer-clojure :exclude [shuffle rand-int abs])
  (:require [kixi.stats.math :refer [abs pow log sqrt exp cos tan atan PI sq floor erf erfcinv] :as m]
            [kixi.stats.protocols :as p :refer [sample-1 sample-n sample-frequencies]]
            [clojure.test.check.random :refer [make-random rand-double split split-n]]))

;;;; Assert helpers

(def ^:no-doc non-neg?
  (complement neg?))

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
  "Returns a random variate generated from a Gamma distribution with shape
  parameter `alpha`, internally using `rng` to generate random normal and
  uniform variates.

  The variate is generated using Marsaglia's transformation-rejection method
  described in [\"A simple method for generating Gamma
  variables\"](https://dl.acm.org/doi/10.1145/358407.358414), page 369.

  ### References

  - [Wikipedia section on random variate generation](https://en.wikipedia.org/wiki/Gamma_distribution#Random_variate_generation)"
  [alpha rng]
  (let [;; First part of the correction for $alpha < 1$, described on p371 of
        ;; the paper.
        alpha' (if (< alpha 1) (inc alpha) alpha)
        d (- alpha' (/ 1.0 3.0))
        c (/ 1.0 (sqrt (* 9.0 d)))
        [r1 r2] (split rng)
        v (loop [rng r1]
            (let [[r1 r2] (split rng)
                  [x v]   (loop [rng r2]
                            (let [x (rand-normal rng)
                                  v (inc (* c x))]
                              (if (pos? v)
                                [x v]
                                (recur (next-rng rng)))))
                  v    (* v (* v v))
                  u    (rand-double r1)
                  x**2 (* x x)]
              (if (or (< u (- 1.0 (* 0.331 (* x**2 x**2))))
                      (< (log u) (+ (* 0.5 x**2)
                                    (* d (+ (- 1.0 v) (log v))))))
                v
                (recur (next-rng r1)))))]
    (if (= alpha alpha')
      (* d v)
      ;; Correction for $alpha < 1$, described on p371 of the paper.
      (* (pow (loop [rng r2]
                (let [r (rand-double rng)]
                  (if (pos? r)
                    r
                    (recur (next-rng rng)))))
              (/ 1.0 alpha))
         d v))))

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
  (cond
    (<= p 0.0) m/negative-infinity
    (>= p 1.0) m/infinity
    :else
    (let [x (m/ibetainv (* 2 (min p (- 1 p)))
                        (* 0.5 dof)
                        0.5)
          x (sqrt (* dof (/ (- 1 x) x)))]
      (if (> p 0.5) x (- x)))))

(defn ^:no-doc cdf-t
  [dof x]
  (cond
    (= x m/negative-infinity) 0.0
    (= x m/infinity) 1.0
    :else
    (let [dof2 (* dof 0.5)]
      (m/ibeta (/ (+ x (sqrt (+ (sq x) dof)))
                  (* 2 (sqrt (+ (sq x) dof))))
               dof2 dof2))))


;;;; Protocol implementations

(deftype ^:no-doc Uniform
    [a b]
    p/PRandomVariable
    (sample-1 [_ rng]
      (+ (* (rand-double rng) (- b a)) a))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [_ x]
      (cond
        (<= x a) 0.0
        (>= x b) 1.0
        :else
        (/ (- x a) (- b a))))
    (quantile [_ p]
      (cond
        (zero? p) a
        (= p 1.0) b
        :else
        (+ a (* p (- b a)))))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Exponential
    [rate]
    p/PRandomVariable
    (sample-1 [_ rng]
      (/ (- (log (rand-double rng))) rate))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [_ x]
      (- 1.0 (exp (- (* rate x)))))
    (quantile [_ p]
      (/ (- (log (- 1.0 p))) rate))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Binomial
    [n p]
    p/PRandomVariable
    (sample-1 [_ rng]
      (rand-binomial n p rng))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n' rng]
      (-> (sample-n this n' rng)
          (frequencies)))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Bernoulli
    [p]
    p/PRandomVariable
    (sample-1 [_ rng]
      (< (rand-double rng) p))
    (sample-n [_ n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        (-> (concat (repeat v true)
                    (repeat (- n v) false))
            (shuffle rng))))
    p/PDiscreteRandomVariable
    (sample-frequencies [_ n rng]
      (let [v (sample-1 (->Binomial n p) rng)]
        {true v false (- n v)}))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Normal
    [mu sd]
    p/PRandomVariable
    (sample-1 [_ rng]
      (+ (* (rand-normal rng) sd) mu))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [_ x]
      (* 0.5 (+ 1 (erf (/ (- x mu)
                          (sqrt (* 2 sd sd)))))))
    (quantile [_ p]
      (+ (* -1.41421356237309505 sd (erfcinv (* 2 p))) mu))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc T
    [dof]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [[r1 r2] (split rng)]
        (* (rand-normal r1)
           (sqrt (/ dof (* 2 (rand-gamma (* 0.5 dof) r2)))))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [_ x]
      (cdf-t dof x))
    (quantile [_ p]
      (quantile-t dof p))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Gamma
    [shape scale]
    p/PRandomVariable
    (sample-1 [_ rng]
      (* (rand-gamma shape rng) scale))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Beta
    [alpha beta]
    p/PRandomVariable
    (sample-1 [_ rng]
      (rand-beta alpha beta rng))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc BetaBinomial
    [n alpha beta]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [[r1 r2] (split rng)
            p (rand-beta alpha beta r1)]
        (rand-binomial n p r2)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc ChiSquared
    [k]
    p/PRandomVariable
    (sample-1 [_ rng]
      (* (rand-gamma (/ k 2) rng) 2))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PQuantile
    (cdf [_ x]
      (m/lower-regularized-gamma (* 0.5 k) (* 0.5 x)))
    (quantile [_ p]
      (* 2.0 (m/gamma-pinv p (* 0.5 k))))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc F
    [d1 d2]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [[r1 r2] (split rng)
            x1 (* (rand-gamma (/ d1 2) r1) 2)
            x2 (* (rand-gamma (/ d2 2) r2) 2)]
        (/ (/ x1 d1) (/ x2 d2))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Poisson
    [lambda]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [l (exp (- lambda))]
        (loop [p 1 k 0 rng rng]
          (let [p (* p (rand-double rng))]
            (if (> p l)
              (recur p (inc k) (next-rng rng))
              k)))))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Weibull
    [shape scale]
    p/PRandomVariable
    (sample-1 [_ rng]
      (* (pow (- (log (rand-double rng)))
              (/ 1 shape))
         scale))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Categorical
    [ks ps]
    p/PRandomVariable
    (sample-1 [_ rng]
      (first (categorical-sample ks ps 1 rng)))
    (sample-n [_ n rng]
      (shuffle (categorical-sample ks ps n rng) rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [_ n rng]
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
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Multinomial
    [n ps]
    p/PRandomVariable
    (sample-1 [_ rng]
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
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Dirichlet
    [as]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [rs (split-n rng (count as))
            xs (map #(rand-gamma %1 %2) as rs)
            s (apply + xs)]
        (mapv #(/ % s) xs)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc DirichletMultinomial
    [n as]
    p/PRandomVariable
    (sample-1 [_ rng]
      (let [[r1 r2] (split rng)
            ps (sample-1 (->Dirichlet as) r1)]
        (sample-1 (->Multinomial n ps) r2)))
    (sample-n [this n rng]
      (default-sample-n this n rng))
    p/PDiscreteRandomVariable
    (sample-frequencies [this n rng]
      (frequencies (sample-n this n rng)))
    #?@(:clj (clojure.lang.Seqable
              (seq [this] (sampleable->seq this)))
        :cljs (ISeqable
               (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Cauchy
  [location scale]
  p/PRandomVariable
  (sample-1 [_ rng]
    (+ location (* scale (tan (* PI (- (rand-double rng) 0.5))))))
  (sample-n [this n rng]
    (default-sample-n this n rng))
  p/PQuantile
  (cdf [_ x]
    (+ 0.5 (/ (atan (/ (- x location) scale)) PI)))
  (quantile [_ p]
    (+ location (* scale (tan (* PI (- p 0.5))))))
  #?@(:clj (clojure.lang.Seqable
            (seq [this] (sampleable->seq this)))
      :cljs (ISeqable
             (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc LogNormal
  [mu sd]
  p/PRandomVariable
  (sample-1 [_ rng]
    (exp (+ (* (rand-normal rng) sd) mu)))
  (sample-n [this n rng]
    (default-sample-n this n rng))
  p/PQuantile
  (cdf [_ x]
    (* 0.5 (+ 1 (erf (/ (- (log x) mu)
                           (sqrt (* 2 sd sd)))))))
  (quantile [_ p]
    (exp (+ (* -1.41421356237309505 sd (erfcinv (* 2 p))) mu)))
  #?@(:clj (clojure.lang.Seqable
            (seq [this] (sampleable->seq this)))
      :cljs (ISeqable
             (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Pareto
  [scale shape]
  p/PRandomVariable
  (sample-1 [_ rng]
    (/ scale (pow (rand-double rng) (/ 1 shape))))
  (sample-n [this n rng]
    (default-sample-n this n rng))
  p/PQuantile
  (cdf [_ x]
    (if (< scale x)
      (- 1 (pow (/ scale x) shape))
      0.0))
  (quantile [_ p]
    (/ scale (pow (- 1 p) (/ 1 shape))))
  #?@(:clj (clojure.lang.Seqable
            (seq [this] (sampleable->seq this)))
      :cljs (ISeqable
             (-seq [this] (sampleable->seq this)))))

(deftype ^:no-doc Truncated
  [distribution lower upper lower-cdf upper-cdf]
  p/PRandomVariable
  (sample-1 [this rng]
    (p/quantile distribution
              (+ (* (rand-double rng) (- upper-cdf lower-cdf)) lower-cdf)))
  (sample-n [this n rng]
    (default-sample-n this n rng))
  p/PBounded
  (minimum [this] lower)
  (maximum [this] upper)
  p/PQuantile
  (cdf [this x]
    (cond (>= x upper) 1.0
          (< x lower) 0.0
          :else (/ (- (p/cdf distribution x) lower-cdf)
                   (- upper-cdf lower-cdf))))
  (quantile [this p]
    (p/quantile distribution (+ (* p (- upper-cdf lower-cdf)) lower-cdf)))
  #?@(:clj (clojure.lang.Seqable
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
  Params: {:a ∈ ℝ, :b ∈ ℝ, :a < :b}"
  [{:keys [a b]}]
  (assert (< a b) (str "a (" a ") must be less than b (" b ")."))
  (->Uniform a b))

(defn exponential
  "Returns an exponential distribution.
  Params: {:rate ∈ ℝ > 0}"
  [{:keys [rate]}]
  (assert (pos? rate) (str "rate (" rate ") must be positive."))
  (->Exponential rate))

(defn bernoulli
  "Returns a Bernoulli distribution.
  Params: {:p ∈ [0 1]}"
  [{:keys [p]}]
  (assert (<= 0.0 p 1.0) (str "p (" p ") must be between 0.0 and 1.0."))
  (->Bernoulli p))

(defn binomial
  "Return a binomial distribution.
  Params: {:n ∈ ℕ, :p ∈ [0 1]}"
  [{:keys [n p]}]
  (assert (nat-int? n) (str "n (" n ") must be a natural number."))
  (assert (<= 0.0 p 1.0) (str "p (" p ") must be between 0.0 and 1.0."))
  (->Binomial n p))

(defn normal
  "Returns a normal distribution.
  Params: {:location ∈ ℝ, :scale ∈ ℝ > 0}"
  [{:keys [location scale mu sd]}]
  (assert (pos? (or scale sd)) (str "scale/sd (" (or scale sd) ") must be positive."))
  (->Normal (or location mu) (or scale sd)))

(defn t
  "Returns a t distribution.
  Params: {:v ∈ ℝ > 0}"
  [{:keys [v]}]
  (assert (pos? v) (str "v (" v ") must be positive."))
  (->T v))

(defn gamma
  "Returns a gamma distribution.
  Params: {:shape ∈ ℝ > 0, :scale ∈ ℝ > 0} or {:shape ∈ ℝ > 0, :rate ∈ ℝ > 0}"
  [{:keys [shape scale rate] :or {shape 1.0}}]
  (assert (and (pos? shape) (pos? (or scale rate)))
          (str "shape (" shape ") and scale/rate (" (or scale rate) ") must be positive."))
  (->Gamma shape (or scale (/ 1.0 rate))))

(defn beta
  "Returns a beta distribution.
  Params: {:alpha ∈ ℝ > 0, :beta ∈ ℝ > 0}"
  [{:keys [alpha beta] :or {alpha 1.0 beta 1.0}}]
  (assert (and (pos? alpha) (pos? beta)) (str "alpha (" alpha ") and beta (" beta ") must be positive."))
  (->Beta alpha beta))

(defn beta-binomial
  "Returns a beta distribution.
  Params: {:n ∈ ℕ > 0, :alpha ∈ ℝ > 0, :beta ∈ ℝ > 0}"
  [{:keys [n alpha beta] :or {alpha 1.0 beta 1.0}}]
  (assert (pos-int? n) (str "n (" n ") must be a positive integer."))
  (assert (and (pos? alpha) (pos? beta)) (str "alpha (" alpha ") and beta (" beta ") must be positive."))
  (->BetaBinomial n alpha beta))

(defn weibull
  "Returns a weibull distribution.
  Params: {:shape ∈ ℝ >= 0, :scale ∈ ℝ >= 0}"
  [{:keys [shape scale] :or {shape 1.0 scale 1.0}}]
  (assert (and (non-neg? shape) (non-neg? scale))
          (str "shape (" shape ") and scale (" scale ") must not be negative."))
  (->Weibull shape scale))

(defn chi-squared
  "Returns a chi-squared distribution.
  Params: {:k ∈ ℕ > 0}"
  [{:keys [k]}]
  (assert (pos-int? k) (str "k (" k ") must be a positive integer."))
  (->ChiSquared k))

(defn f
  "Returns an F distribution.
  Params: {:d1 ∈ ℝ > 0, :d2 ∈ ℝ > 0}"
  [{:keys [d1 d2]}]
  (assert (and (pos? d1) (pos? d2))
          (str "d1 (" d1 ") and d2 (" d2 ") must be positive."))
  (->F d1 d2))

(defn poisson
  "Returns a Poisson distribution.
  Params: {:lambda ∈ ℝ > 0}"
  [{:keys [lambda]}]
  (assert (pos? lambda) (str "lambda (" lambda ") must be positive."))
  (->Poisson lambda))

(defn categorical
  "Returns a categorical distribution.
  Params: {[category] [probability], ...}
  Probabilities should be >= 0 and sum to 1"
  [category-probs]
  (let [[ks ps] (apply map vector category-probs)]
    (assert (every? #(<= 0.0 % 1.0) ps) "All the probabilities must be between 0.0 and 1.0.")
    (->Categorical ks ps)))

(defn multinomial
  "Returns a multinomial distribution.
  Params: {:n ∈ ℕ > 0, :probs [ℝ >= 0, ...]}
  Probabilities should be >= 0 and sum to 1"
  [{:keys [n probs]}]
  (assert (pos-int? n) (str "n (" n ") must be a positive integer."))
  (assert (every? #(<= 0.0 % 1.0) probs)
          "All the probabilities must be between 0.0 and 1.0.")
  (->Multinomial n probs))

(defn dirichlet
  "Returns a Dirichlet distribution.
  Params: {:alphas [ℝ >= 0, ...]}"
  [{:keys [alphas]}]
  (assert (every? non-neg? alphas) "All the alphas must be non-negative.")
  (->Dirichlet alphas))

(defn dirichlet-multinomial
  "Returns a Dirichlet-multinomial distribution.
  Params: {:n ∈ ℕ, :alphas [ℝ >= 0, ...]}"
  [{:keys [n alphas]}]
  (assert (pos-int? n) (str "n (" n ") must be a positive integer."))
  (assert (every? non-neg? alphas) "All the alphas must be non-negative.")
  (->DirichletMultinomial n alphas))

(defn cauchy
  "Returns a Cauchy distribution.
  Params: {:location ∈ ℝ, :scale ∈ ℝ > 0}"
  [{:keys [location scale]}]
  (assert (pos? scale) (str "scale (" scale ") must be positive."))
  (->Cauchy location scale))

(defn log-normal
  "Returns a Log-normal distribution.
  The parameters are the log of the
  mean and sd of this distribution.
  Params: {:location ∈ ℝ, :scale ∈ ℝ > 0}"
  [{:keys [location scale mu sd]}]
  (assert (pos? (or scale sd)) (str "scale/sd (" (or scale sd) ") must be positive."))
  (->LogNormal (or location mu) (or scale sd)))

(defn pareto
  "Returns a Pareto distribution.
  Params: {:scale ∈ ℝ > 0, :shape ∈ ℝ > 0}"
  [{:keys [scale shape]}]
  (assert (and (pos? scale) (pos? shape))
          (str "Scale (" scale ") and shape (" shape ") must be positive."))
  (->Pareto scale shape))

(defn truncated
  "Returns a distribution that is a truncated version
  of the supplied `distribution` between the lower bound `lower`
  and the upper bound `upper`.
  Params: {:distribution, :lower ∈ ℝ, :upper ∈ ℝ, :lower < :upper}"
  [{:keys [distribution lower upper]}]
  (assert (and (satisfies? p/PRandomVariable distribution)
               (satisfies? p/PQuantile distribution))
          "distribution must satisfy PRandomVaraible and PQuantile.")
  (assert (and (number? lower) (number? upper) (< lower upper))
          (str "lower (" lower ") must be less than upper (" upper ")."))
  (let [cdf-lower (cdf distribution lower)
        cdf-upper (cdf distribution upper)]
    (assert (< cdf-lower cdf-upper)
            (str "lower (" lower ") and upper (" upper ") are beyond the extremes of the distribution."))
    (->Truncated distribution lower upper cdf-lower cdf-upper)))

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

(defn critical-value
  ([^kixi.stats.protocols.PQuantile distribution]
   (critical-value distribution 0.05))
  ([^kixi.stats.protocols.PQuantile distribution alpha]
   (critical-value distribution alpha :<>))
  ([^kixi.stats.protocols.PQuantile distribution alpha tails]
   (case tails
     :<> (quantile distribution (- 1 (* 0.5 alpha)))
     :<  (quantile distribution alpha)
     :>  (quantile distribution (- 1 alpha)))))
