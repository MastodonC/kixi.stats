(ns kixi.stats.test-helpers
  (:require [clojure.test.check.generators :as gen]
            [kixi.stats.math :refer [abs equal floor ceil]]))


#?(:clj
   (defn infinite? [x]
     (and (float? x) (.isInfinite x))))

(defn finite?
  [x]
  #?(:clj  (or (nil? x) (Double/isFinite x))
     :cljs (or (nil? x) (js/isFinite x))))

(def numeric
  (gen/such-that finite? (gen/one-of [gen/int gen/double (gen/return nil)])))

(defn seq= [f]
  (fn [x y]
    (cond
      (and (sequential? x) (sequential? y))
      (every? true? (map f x y))
      (or (sequential? x) (sequential? y))
      false
      :else (f x y))))

(defn map= [f]
  (fn [x y]
    (cond
      (and (map? x) (map? y))
      (->> (merge-with f x y)
           (vals)
           (every? identity))
      (or (map? x) (map? y))
      false
      :else (f x y))))

(defn some= [f]
  (fn [x y]
    (cond
      (and (nil? x) (nil? y))
      true
      (or (nil? x) (nil? y))
      false
      :else (f x y))))

(defn inf= [f]
  (fn [x y]
    (if (and (infinite? x) (infinite? y))
      true
      (f x y))))

(defn approx= [e]
  (fn [x y]
    (let [e (if (or (zero? x) (zero? y))
              e (* (abs (min x y)) e))]
      (equal x y e))))

(def =ish (-> (approx= 1e-11) inf= some= map= seq=))

(defn quantile'
  [p coll]
  (cond
    (<= p 0.0) 0.0
    :else
    (let [coll (->> coll
                    (remove nil?)
                    sort
                    vec)
          n (count coll)
          np (min (max 0 (- (* n p) 0.5)) (dec n))
          i1 (floor np)
          i2 (ceil np)]
      (if (<= n 1)
        (some-> (first coll) double)
        (+ (nth coll i1)
           (* (- (nth coll i2) (nth coll i1))
              (- np (floor np))))))))

(defn interpolate
  [x a b]
  (let [d (- b a)]
    (if (zero? d)
      0.0
      (/ (- x a) d))))

(defn cdf'
  [x xs]
  (let [xs (->> (remove nil? xs)
                (sort))
        n (count xs)]
    (cond
      (zero? n) nil
      (= n 1) (if (< x (first xs)) 0.0 1.0)
      :else
      (loop [r 0
             a (first xs)
             b (second xs)
             left (/ (- b a) 2.0)
             right left
             xs (drop 2 xs)]
        (if (seq xs)
          (if (< x (+ a right))
            (max 0.0 (/ (+ r (interpolate x (- a left) (+ a right))) n))
            (recur (inc r)
                   b
                   (first xs)
                   right
                   (/ (- (first xs) b) 2.0)
                   (rest xs)))
          (if (< x (+ a right))
            (/ (+ r (interpolate x (- a left) (+ a right))) n)
            1.0))))))

(defn categories
  [n]
  (map #(->> % inc (str "category-") keyword) (range n)))

(defn gen-category
  [n]
  (->> (categories n)
       (gen/elements)))
