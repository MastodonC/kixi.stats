(ns kixi.stats.math
  (:refer-clojure :exclude [infinite? abs]))

(def PI
  #?(:clj Math/PI
     :cljs js/Math.PI))
(defn abs [x]
  (cond-> x
    (< x 0) -))

(defn sqrt [x]
  #?(:clj  (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn sq [x]
  (* x x))

(defn pow [x n]
  #?(:clj  (Math/pow x n)
     :cljs (js/Math.pow x n)))

(defn root [x n]
  (pow x (/ 1 n)))

(defn log [x]
  #?(:clj  (Math/log x)
     :cljs (js/Math.log x)))

(defn log1p [x]
  #?(:clj  (Math/log1p x)
     :cljs (js/Math.log (inc x))))

(defn exp [x]
  #?(:clj  (Math/exp x)
     :cljs (js/Math.exp x)))

(defn cos [x]
  #?(:clj  (Math/cos x)
     :cljs (js/Math.cos x)))

(defn clamp [x lower upper]
  (min (max x lower) upper))

(defn sin [x]
  #?(:clj  (Math/sin x)
     :cljs (js/Math.sin x)))

(defn tan [x]
  #?(:clj  (Math/tan x)
     :cljs (js/Math.tan x)))

(defn atan [x]
  #?(:clj  (Math/atan x)
     :cljs (js/Math.atan x)))

(defn ceil [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn floor [x]
  #?(:clj  (Math/floor x)
     :cljs (js/Math.floor x)))

(defn round [x]
  #?(:clj  (Math/round x)
     :cljs (js/Math.round x)))

(defn equal [x y e]
  (<= (abs (- y x)) e))

(def infinity
  #?(:clj Double/POSITIVE_INFINITY
     :cljs js/Infinity))

(def negative-infinity
  #?(:clj Double/NEGATIVE_INFINITY
     :cljs js/-Infinity))

(defn infinite? [x]
  #?(:clj (Double/isInfinite x)
     :cljs (not (js/isFinite x))))

;;;; Gamma

(def ^:no-doc SQRT_2_PI 2.506628274631000502)

(def ^:no-doc HALF_LOG_2_PI (* 0.5 (log (* 2.0 PI))))

(def ^:no-doc LANCZOS
  [[14 3.6899182659531625E-6] [13 -2.6190838401581408E-5]
   [12 8.441822398385275E-5] [11 -1.643181065367639E-4]
   [10 2.1743961811521265E-4] [9 -2.1026444172410488E-4]
   [8 1.580887032249125E-4] [7 -9.837447530487956E-5]
   [6 4.652362892704858E-5] [5 3.399464998481189E-5]
   [4 -0.4919138160976202] [3 14.136097974741746]
   [2 -59.59796035547549] [1 57.15623566586292]])

(def ^:no-doc A
  [0.611609510448141581788E-08 0.624730830116465516210E-08])

(def ^:no-doc B
  [0.195755836614639731882E-09 -0.607761895722825260739E-07
   0.992641840672773722196E-06 -0.643045481779353022248E-05
   -0.851419432440314906588E-05 0.493944979382446875238E-03
   0.266205348428949217746E-01 0.203610414066806987300E+00])

(def ^:no-doc P
  [4.343529937408594E-15 -1.2494415722763663E-13 1.5728330277104463E-12
   4.686843322948848E-11 6.820161668496171E-10 6.8716741130671986E-9
   6.116095104481416E-9])

(def ^:no-doc Q
  [2.6923694661863613E-4 0.004956830093825887 0.054642130860422966
   0.3056961078365221])

(def ^:no-doc C
  [-0.205633841697760710345015413002057E-06 0.113302723198169588237412962033074E-05
   -0.125049348214267065734535947383309E-05 -0.201348547807882386556893914210218E-04
   0.128050282388116186153198626328164E-03 -0.215241674114950972815729963053648E-03
   -0.116516759185906511211397108401839E-02 0.721894324666309954239501034044657E-02
   -0.962197152787697356211492167234820E-02 -0.421977345555443367482083012891874E-01
   0.166538611382291489501700795102105E+00 -0.420026350340952355290039348754298E-01
   -0.655878071520253881077019515145390E+00])

(def ^:no-doc CA
  -0.422784335098467139393487909917598E+00)

(def ^:no-doc CB
  0.577215664901532860606512090082402E+00)

(def LANCZOS_G
  "The Lanczos constant"
  (/ 607 128))

(defn lanczos-approximation
  "Computes the Lanczos approximation to the Gamma function"
  [x]
  (+ (reduce (fn [sum [i l]] (+ sum (/ l (+ x i)))) 0.0 LANCZOS)
     0.9999999999999971))

(defn inv-gamma-1pm1
  "Computes the function `(dec (/ 1 (gamma (inc x))))`
  for -0.5 <= x <= 0.5"
  [x]
  (let [t (if (<= x 0.5) x (- (- x 0.5) 0.5))]
    (if (< t 0)
      (let [[a0 a1] A
            b (inc (* t (reduce (fn [b b'] (+ (* t b) b')) B)))
            c (+ CA (* t (reduce (fn [c c'] (+ (* t c) c')) (/ (+ a0 (* a1 t)) b) C)))]
        (if (> x 0.5)
          (* t (/ c x))
          (* x (inc c))))
      (let [p (reduce (fn [p p'] (+ (* t p) p')) P)
            q (inc (* t (reduce (fn [q q'] (+ (* t q) q')) Q)))
            c (+ CB (* t (reduce (fn [c c'] (+ (* t c) c')) (/ p q) C)))]
        (if (> x 0.5)
          (* (/ t x) (dec c))
          (* x c))))))

(defn log-gamma-1p
  "Computes the function `(ln (gamma (inc x)))`
  for -0.5 <= x <= 0.5"
  [x]
  (- (log1p (inv-gamma-1pm1 x))))

(defn log-gamma
  "Computes the value of ln(Γx)"
  [x]
  (cond
    (< x 0.5) (- (log-gamma-1p x) (log x))
    (<= x 2.5) (log-gamma-1p (dec x))
    (<= x 8.0) (let [n (int (floor (- x 1.5)))]
                 (+ (log-gamma-1p (- x (inc n)))
                    (loop [i 1
                           p 1.0]
                      (if (<= i n)
                        (recur (inc i) (* p (- x i)))
                        (log p)))))
    :else (let [t (+ x LANCZOS_G 0.5)]
            (+ (- (* (+ x 0.5) (log t)) t)
               HALF_LOG_2_PI
               (log (/ (lanczos-approximation x) x))))))

(defn gamma
  "Computes the value of Γx"
  [x]
  (let [abs-x (abs x)]
    (if (<= abs-x 20)
      (if (>= x 1)
        (loop [t (dec x) p 1]
          (if (> t 1.5)
            (recur (dec t) (* p t))
            (/ p (inc (inv-gamma-1pm1 t)))))
        (loop [t (inc x) p x]
          (if (< t 0.5)
            (recur (inc t) (* p t))
            (/ 1 (* p (inc (inv-gamma-1pm1 (dec t))))))))
      (let [y (+ abs-x LANCZOS_G 0.5)
            abs-g (* (/ SQRT_2_PI abs-x)
                     (pow y (+ abs-x 0.5))
                     (exp (- y))
                     (lanczos-approximation abs-x))]
        (if (pos? x)
          abs-g
          (/ (- PI)
             (* x abs-g (sin (* PI x)))))))))

(defn lower-regularized-gamma
  "Computes the lower regularized incomplete gamma function P(a,x)"
  [a x]
  (when (and (>= x 0) (> a 0))
    (let [max-iter (-> (+ (* (log (if (>= a 1) a (/ 1 a))) 8.5)
                          (* 0.4 a)
                          17)
                       inc
                       floor
                       int)]
      (if (< x (inc a))
        (loop [i 1
               ap a
               del (double (/ 1 ap))
               sum (double (/ 1 ap))]
          (if (< i max-iter)
            (let [ap (inc ap)
                  del (* del (/ x ap))]
              (recur (inc i) ap del (+ sum del)))
            (* sum (exp (- (* a (log x)) x (log-gamma a))))))
        (loop [i 1
               b (double (- (inc x) a))
               c (double (/ 1 1e-30))
               d (double (/ 1 b))
               h (double (/ 1 b))]
          (let [an (* (- i) (- i a))
                b (+ b 2)
                d (+ (* an d) b)
                c (+ b (/ an c))
                d (/ 1 d)
                h (* h d c)]
            (if (< i max-iter)
              (recur (inc i) b c d h)
              (- 1 (* h (exp (- (* a (log x)) x (log-gamma a))))))))))))

(defn gamma-pinv
  "Returns the inverse of the lower regularized incomplete gamma function"
  [p a]
  (cond
    (>= p 1.0) (max 100.0 (+ a (* 100.0 (sqrt a))))
    (<= p 0.0) 0.0
    :else
    (let [gln (log-gamma a)
          a1 (dec a)
          lna1 (log a1)
          afac (exp (- (* a1 (dec lna1)) gln))
          EPS 1e-8
          x (if (> a 1)
              (let [pp (if (< p 0.5) p (- 1 p))
                    t (sqrt (* -2 (log pp)))
                    x (- (/ (+ 2.30753 (* 0.27061 t)) (+ 1 (* t (+ 0.99229 (* 0.04481 t))))) t)
                    x (if (< p 0.5) (- x) x)]
                (max 1e-3 (* a (pow (- 1 (/ 1 (* 9 a)) (/ x (* 3 (sqrt a)))) 3))))
              (let [t (- 1 (* a (+ 0.253 (* 0.12 a))))]
                (if (< p t)
                  (pow (/ p t) (/ 1 a))
                  (- 1 (log (- 1 (/ (- p t) (- 1 t))))))))]
      (loop [j 0 x x]
        (if (<= x 0.0)
          0.0
          (let [err (- (lower-regularized-gamma a x) p)
                t (if (> a 1)
                    (* afac (exp (- (* a1 (- (log x) lna1)) (- x a1))))
                    (exp (- (* a1 (log x)) gln x)))
                u (/ err t)
                t (/ u (- 1 (* 0.5 (min 1 (* u (dec (/ (dec a) x)))))))
                x (- x t)
                x (if (<= x 0) (* 0.5 (+ x t)) x)]
            (if (or (< (abs t) (* EPS x)) (= j 11))
              x
              (recur (inc j) x))))))))

(defn log-beta
  "Computes the log of the beta function"
  [a b]
  (- (+ (log-gamma a) (log-gamma b)) (log-gamma (+ a b))))

(defn beta
  "Computes the beta function"
  [a b]
  (when (and (pos? a) (pos? b))
    (if (> (+ a b) 170)
      (exp (log-beta a b))
      (/ (* (gamma a) (gamma b))
         (gamma (+ a b))))))

(defn betacf
  "Evaluates the continued fraction for the incomplete beta function.
  Modified Lentz's method"
  [x a b]
  (let [fpmin 1e-30
        check (fn [x] (if (< (abs x) fpmin) fpmin x))
        qab (+ a b)
        qap (inc a)
        qam (dec a)
        d (/ 1 (check (- 1 (/ (* x qab) qap))))]
    (loop [m 1
           h d
           c 1
           d d]
      (let [m2 (* 2 m)
            aa (* m (- b m) (/ x (* (+ qam m2) (+ a m2))))
            d (/ 1 (check (+ 1 (* aa d))))
            c (check (+ 1 (/ aa c)))
            h (* h d c)
            aa (* (- (+ a m)) (+ qab m) (/ x (* (+ a m2) (+ qap m2))))
            d (/ 1 (check (+ 1 (* aa d))))
            c (check (+ 1 (/ aa c)))
            del (* d c)
            h (* h del)]
        (if (or (< (abs del) 3e-7)
                (>= m 100))
          h
          (recur (inc m) h c d))))))

(defn ibeta
  "Returns the incomplete beta function I_x(a,b)"
  [x a b]
  (when (<= 0 x 1)
    (let [bt (if (or (== x 0)
                     (== x 1))
               0
               (exp (+ (- (log-gamma (+ a b))
                          (log-gamma a)
                          (log-gamma b))
                       (* a (log x))
                       (* b (log (- 1 x))))))]
      (if (< x (/ (inc a) (+ a b 2)))
        (* bt (/ (betacf x a b) a))
        (- 1 (* bt (/ (betacf (- 1 x) b a) b)))))))

(defn ibetainv
  "Returns the inverse of the incomplete beta function"
  [p a b]
  (cond
    (<= p 0) 0.0
    (>= p 1) 1.0
    :else
    (let [eps 1e-8
          a1 (dec a)
          b1 (dec b)
          x (if (and (>= a 1)
                     (>= b 1))
              (let [pp (if (< p 0.5) p (- 1 p))
                    t (sqrt (* -2 (log pp)))
                    x (- (/ (+ 2.30753 (* t 0.27061))
                            (inc (* t (+ 0.99229
                                         (* t 0.04481))))) t)
                    x (if (< p 0.5) (- x) x)
                    al (/ (- (sq x) 3) 6)
                    h (/ 2 (+ (/ 1 (dec (* 2 a)))
                              (/ 1 (dec (* 2 b)))))
                    w (- (/ (* x (sqrt (+ al h))) h)
                         (* (- (/ 1 (dec (* 2 b)))
                               (/ 1 (dec (* 2 a))))
                            (+ al (/ 5 6) (/ -2 (* 3 h)))))]
                (/ a (+ a (* b (exp (* 2 w))))))
              (let [lna (log (/ a (+ a b)))
                    lnb (log (/ b (+ a b)))
                    t (/ (exp (* a lna)) a)
                    u (/ (exp (* b lnb)) b)
                    w (+ t u)]
                (if (< p (/ t w))
                  (pow (* a w p) (/ 1 a))
                  (- 1 (pow (* b w (- 1 p)) (/ 1 b))))))
          afac (- (log-gamma (+ a b))
                  (log-gamma a)
                  (log-gamma b))]
      (loop [j 0
             x x]
        (if (or (== x 0)
                (== x 1)
                (>= j 10))
          x
          (let [err (- (ibeta x a b) p)
                t (exp (+ (* a1 (log x))
                          (* b1 (log (- 1 x)))
                          afac))
                u (/ err t)
                t (/ u (- 1 (* 0.5 (min 1 (* u (- (/ a1 x)
                                                  (/ b1 (- 1 x))))))))
                x (- x t)
                x (cond
                    (<= x 0)
                    (* 0.5 (+ x t))
                    (>= x 1)
                    (* 0.5 (+ x t 1))
                    :else x)]
            (if (and (> j 0)
                     (< (abs t) (* eps x)))
              x
              (recur (inc j) x))))))))

(defn erf
  "Computes the error function"
  [x]
  (let [x' (abs x)
        t (/ 2 (+ x' 2))
        ty (- (* 4 t) 2)
        [d dd] (reduce (fn [[d dd] cof]
                         (vector (+ (- (* ty d) dd) cof) d))
                       [0 0]
                       [-2.8E-17 1.21E-16 -9.4E-17 -1.523E-15 7.106E-15 3.81E-16
                        -1.12708E-13 3.13092E-13 8.94487E-13 -6.886027E-12 2.394038E-12
                        9.6467911E-11 -2.27365122E-10 -9.91364156E-10 5.059343495E-9
                        6.529054439E-9 -8.5238095915E-8 1.5626441722E-8 1.30365583558E-6
                        -1.624290004647E-6 -2.0278578112534E-5 4.2523324806907E-5
                        3.66839497852761E-4 -9.46595344482036E-4 -0.00956151478680863
                        0.019476473204185836 0.6419697923564902])
        cof -1.3026537197817094
        res (* t (exp (+ (* (- x') x') (* 0.5 (+ (* ty d) cof)) (- dd))))]
    (if (neg? x)
      (- res 1)
      (- 1 res))))

(defn erfc
  "Computes the complementary error function"
  [x]
  (- 1 (erf x)))

(defn erfcinv
  "Computes the inverse of the complementary error function"
  [p]
  (cond (>= p 2) -100
        (<= p 0) 100
        :else (let [pp (if (< p 1) p (- 2 p))
                    t (sqrt (* -2 (log (* pp 0.5))))
                    x (* -0.70711
                         (- (/ (+ (* 0.27061 t) 2.30753)
                               (+ 1 (* t (+ (* 0.04481 t) 0.99229))))
                            t))
                    x (loop [j 0 x x]
                        (if (< j 2)
                          (let [err (- (erfc x) pp)]
                            (recur (inc j)
                                   (+ x (/ err (- (* 1.12837916709551257 (exp (* (- x) x)))
                                                  (* x err))))))
                          x))]
                (if (< p 1)
                  x (- x)))))
