(ns kixi.stats.digest
  (:require [kixi.stats.distribution])
  #?(:clj (:import [com.tdunning.math.stats TDigest])))

#?(:clj
   (defn ^:no-doc nan->nil [n]
     (when-not (or (.isNaN n) (.isInfinite n)) n)))

#?(:clj
   (defn t-digest
     "Return a reducing function which uses the t-digest to summarize a seq"
     [{:keys [compression] :or {compression 100}}]
     (fn
       ([] (TDigest/createAvlTreeDigest compression))
       ([^TDigest digest x]
        (when (number? x) (.add digest x 1))
        digest)
       ([^TDigest digest]
        (reify
          clojure.lang.Counted
          (count [_]
            (.size digest))
          kixi.stats.protocols.PBounded
          (minimum [_]
            (nan->nil (.getMin digest)))
          (maximum [_]
            (nan->nil (.getMax digest)))
          kixi.stats.protocols.PQuantile
          (cdf [_ x]
            (nan->nil (.cdf digest x)))
          (quantile [_ q]
            (nan->nil (.quantile digest q))))))))

(defn sum-squares
  [fx fy]
  (fn
    ([] [0.0 0.0 0.0 0.0 0.0 0.0])
    ([[^double c ^double mx ^double my ^double ssx ^double ssy ^double ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [x   (double x)
               y   (double y)
               c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ssx  (* (- x mx') (- x mx)))
            (+ ssy  (* (- y my') (- y my)))
            (+ ssxy (* (- x mx') (- y my)))]))))
    ([[c mx my ssx ssy ssxy]]
     {:n c
      :x-bar mx
      :y-bar my
      :ss-xy ssxy
      :ss-x  ssx
      :ss-y  ssy})))
