(ns kixi.stats.utils)

(defn sqrt [x]
  (Math/sqrt x))

(defn expt [x p]
  (Math/pow x p))

(defn somef [f]
  (fn [x & args]
    (when-not (nil? x)
      (apply f x args))))

(defn post-complete [rf f]
  (completing rf #(f (rf %))))
