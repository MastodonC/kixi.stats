(ns kixi.stats.test
  (:require [kixi.stats.math :refer [abs erf pow sq sqrt lower-regularized-gamma]]
            [kixi.stats.protocols :as p]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(defn chi-squared-test
  "Calculates the X^2 test of independence for a given contingency table.
  See kixi.stats.core/cross-tabulate"
  [^kixi.stats.protocols.IContingencyTable table]
  (let [margins (p/margin-totals table)
        size (p/size table)
        factors (count size)
        total (pow (p/grand-total table) (dec factors))
        dof (apply - (apply * size) 1 (map dec size))
        stat (->> (apply cartesian-product margins)
                  (map (partial apply map vector))
                  (reduce (fn [acc [levels counts]]
                            (let [cell (p/cell table levels)
                                  e (/ (apply * counts) total)]
                              (+ acc (/ (sq (- e cell)) e))))
                          0))]
    {:p-value (- 1 (lower-regularized-gamma (/ dof 2.0) (/ stat 2.0)))
     :X-sq (double stat)
     :dof dof}))

(defn simple-z-test
  "Calculates the z-test of statistical significance"
  [mu sd x-bar n & [{:keys [one-tail?] :or {one-tail? false}}]]
  (let [z (double (/ (- (abs (- x-bar mu)))
                     (/ sd (sqrt n))))]
    {:p-value (* (* 0.5 (+ 1 (erf (/ z (sqrt 2)))))
                 (if one-tail? 1 2))}))
