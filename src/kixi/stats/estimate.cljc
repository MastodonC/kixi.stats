(ns kixi.stats.estimate
  (:require [kixi.stats.distribution :as d]
            [kixi.stats.math :refer [sq sqrt]]
            [kixi.stats.protocols :as p]))

(defn simple-linear-regression
  [{:keys [x-bar y-bar ss-x ss-xy] :as sum-squares}]
  (when-not (zero? ss-x)
    (let [slope (/ ss-xy ss-x)
          offset (- y-bar (* x-bar slope))]
      (reify
        p/PDependent
        (measure [_ x]
          (+ offset (* slope x)))
        p/PParameterised
        (parameters [_]
          [offset slope])))))

(defn regression-standard-error
  [{:keys [n x-bar y-bar ss-x ss-y ss-xy] :as sum-squares} x]
  (when (and (> n 2) (not (zero? ss-x)))
    (sqrt
     (* (/ 1 (- n 2))
        (- ss-y (/ (sq ss-xy) ss-x))
        (+ (/ 1 n) (/ (sq (- x x-bar)) ss-x))))))

(defn regression-confidence-interval
  [{:keys [n] :as sum-squares} x alpha]
  (let [regression (simple-linear-regression sum-squares)
        y-hat (p/measure regression x)
        se (regression-standard-error sum-squares x)
        df (dec n)
        t-crit (d/critical-value (d/t df) alpha)
        err (* t-crit se)]
    (reify p/PInterval
      (lower [_] (- y-hat err))
      (upper [_] (+ y-hat err)))))

(defn regression-prediction-standard-error
  [{:keys [n x-bar y-bar ss-x ss-y ss-xy] :as sum-squares} x]
  (when (and (> n 2) (not (zero? ss-x)))
    (sqrt
     (* (/ 1 (- n 2))
        (- ss-y (/ (sq ss-xy) ss-x))
        (+ 1 (/ 1 n) (/ (sq (- x x-bar)) ss-x))))))

(defn regression-prediction-interval
  [{:keys [n] :as sum-squares} x alpha]
  (let [regression (simple-linear-regression sum-squares)
        y-hat (p/measure regression x)
        se (regression-prediction-standard-error sum-squares x)
        df (dec n)
        t-crit (d/critical-value (d/t df) alpha)
        err (* t-crit se)]
    (reify p/PInterval
      (lower [_] (- y-hat err))
      (upper [_] (+ y-hat err)))))
