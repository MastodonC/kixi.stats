(ns kixi.stats.core
  (:require [kixi.stats.utils :refer [sqrt somef post-complete]])
  (:refer-clojure :exclude [count]))

(defn count
  "Calculates the count of inputs."
  ([] 0)
  ([n _] (inc n))
  ([n] n))

(defn mean
  "Calculates the arithmetic mean of numeric inputs."
  ([] [0 0])
  ([[s c] e]
   [(+ s e) (inc c)])
  ([[s c]]
   (when-not (zero? c)
     (/ s c))))

(defn variance
  "Estimates an unbiased variance of numeric inputs."
  ([] [0 0 0])
  ([[c m ss] e]
   (let [c' (inc c)
         m' (+ m (/ (- e m) c'))]
     [c' m' (+ ss (* (- e m') (- e m)))]))
  ([[c m ss]]
   (when-not (zero? c)
     (let [c' (dec c)]
       (if (pos? c')
         (/ ss c') 0)))))

(def pvariance
  "Calculates the population variance of numeric inputs."
  (completing variance (fn [[c _ ss]]
                         (when-not (zero? c)
                           (/ ss c)))))

(def standard-deviation
  "Estimates the sample standard deviation of numeric inputs."
  (post-complete variance (somef sqrt)))

(def pstandard-deviation
  "Calculates the population standard deviation of numeric inputs."
  (post-complete pvariance (somef sqrt)))

(defn covariance
  "Given two functions of an input `(fx input)` and `(fy input)`, each of which
  returns a number, estimates the unbiased covariance of those functions over
  inputs.
  
  Ignores any inputs where `(fx input)` or `(fy input)` are nil. If no
  inputs have both x and y, returns nil."
  [fx fy]
  (fn
    ([] [0 0 0 0])
    ([[c mx my ss :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ss (* (- x mx') (- y my)))]))))
    ([[c _ _ ss]]
     (when-not (zero? c)
       (/ ss c)))))

(defn correlation
  "Given two functions: (fx input) and (fy input), each of which returns a
  number, estimates the unbiased linear correlation coefficient between fx and
  fy over inputs. Ignores any records where fx or fy are nil. If there are no
  records with values for fx and fy, the correlation is nil. See
  http://mathworld.wolfram.com/CorrelationCoefficient.html."
  [fx fy]
  (fn
    ([] [0 0 0 0 0 0])
    ([[c mx my ssx ssy ssxy :as acc] e]
     (let [x (fx e)
           y (fy e)]
       (if (or (nil? x) (nil? y))
         acc
         (let [c'  (inc c)
               mx' (+ mx (/ (- x mx) c'))
               my' (+ my (/ (- y my) c'))]
           [c' mx' my'
            (+ ssx  (* (- x mx') (- x mx)))
            (+ ssy  (* (- y my') (- y my)))
            (+ ssxy (* (- x mx') (- y my)))]))))
    ([[c mx my ssx ssy ssxy]]
     (let [d (sqrt (* ssx ssy))]
       (when-not (zero? d)
         (/ ssxy d))))))
