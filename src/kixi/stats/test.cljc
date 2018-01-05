(ns kixi.stats.test
  (:require [kixi.stats.math :refer [sq lower-regularized-gamma]]))

(defn chisq-test
  [^kixi.stats.data.ITable table]
  (let [+' (fnil + 0)
        [xs ys total] (reduce (fn [[xs ys total] [[x y] n]]
                                [(update xs x +' n)
                                 (update ys y +' n)
                                 (+ total n)])
                              [{} {} 0]
                              table)
        dof (* (dec (count xs)) (dec (count ys)))
        stat (->> (for [x (keys xs) y (keys ys)] (vector x y))
                  (reduce (fn [acc [x y]]
                            (let [e (/ (* (get xs x) (get ys y)) total)]
                              (+ acc (/ (sq (- e (get table [x y]))) e))))
                          0))]
    {:p-value (- 1 (lower-regularized-gamma (/ dof 2.0) (/ stat 2.0)))
     :X-sq (double stat)
     :dof dof}))
