(ns kixi.stats.digest
  (:require [kixi.stats.distribution])
  (:import [com.tdunning.math.stats TDigest]))

(defn ^:no-doc nan->nil [n]
  (when-not (or (.isNaN n) (.isInfinite n)) n))

(defn t-digest
  "Return a reducing function which uses the t-digest to summarize a seq"
  [{:keys [compression] :or {compression 100} :as opts}]
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
       kixi.stats.protocols.IBounded
       (minimum [_]
         (nan->nil (.getMin digest)))
       (maximum [_]
         (nan->nil (.getMax digest)))
       kixi.stats.protocols.IQuantile
       (cdf [_ x]
         (nan->nil (.cdf digest x)))
       (quantile [_ q]
         (nan->nil (.quantile digest q)))))))
