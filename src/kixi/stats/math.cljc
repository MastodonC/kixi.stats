(ns kixi.stats.math)

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

(defn sin [x]
  #?(:clj  (Math/sin x)
     :cljs (js/Math.sin x)))

(defn ceil [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn floor [x]
  #?(:clj  (Math/floor x)
     :cljs (js/Math.floor x)))

(defn equal [x y e]
  (<= (abs (- y x)) e))


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
