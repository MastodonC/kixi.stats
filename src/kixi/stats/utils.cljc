(ns kixi.stats.utils)

(def PI
  #?(:clj Math/PI
     :cljs js/Math.PI))

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

(defn cos [x]
  #?(:clj  (Math/cos x)
     :cljs (js/Math.cos x)))

(defn somef [f]
  (fn [x & args]
    (when-not (nil? x)
      (apply f x args))))

(defn post-complete [rf f]
  (completing rf #(f (rf %))))
