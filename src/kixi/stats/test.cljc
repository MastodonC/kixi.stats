(ns kixi.stats.test
  (:require [kixi.stats.math :refer [pow sq lower-regularized-gamma]]
            [kixi.stats.protocols :as p]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(defn chisq-test
  "Calculates the X^2 test of independence for a given contingency table.
  See kixi.stats.core/cross-tabulate"
  [^kixi.stats.protocols.IContingencyTable table]
  (let [margins (p/margin-totals table)
        size (p/size table)
        factors (count size)
        total (pow (p/grand-total table) (dec factors))
        dof (apply - (apply * size) 1 (map dec size))
        stat (reduce (fn [acc level-counts]
                       (let [cell (p/cell table (mapv first level-counts))
                             e (/ (apply * (map second level-counts)) total)]
                         (+ acc (/ (sq (- e cell)) e))))
                     0
                     (apply cartesian-product margins))]
    {:p-value (- 1 (lower-regularized-gamma (/ dof 2.0) (/ stat 2.0)))
     :X-sq (double stat)
     :dof dof}))
