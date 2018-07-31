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

(defn z-table
  [z & [{:keys [tails] :or {tails :both}}]]
  (case tails
    :lower (* 0.5 (+ 1 (erf (/ z (sqrt 2)))))
    :upper (- 1 (* 0.5 (+ 1 (erf (/ z (sqrt 2))))))
    :both (+ 1 (erf (/ z (sqrt 2))))))

(defn simple-z-test
  "Calculates the z-test of statistical significance for a sample mean.
  mu: the population mean
  sd: the population standard deviation
  mean: the sample mean
  n: the sample size
  opts: tails: (optional) must be one of :lower, :upper, or :both (default).
  See also: kixi.stats.core/simple-z-test"
  [{:keys [mu sd]} {:keys [mean n]} & [opts]]
  (when-let [z (and (pos? sd)
                    (double (/ (- mean mu)
                               (/ sd (sqrt n)))))]
    {:p-value (z-table z opts)}))

(defn z-test
  "Calculates the z-test of statistical significance between two sample means.
  Requires the mean, sd and sample size (n) of both samples.
  opts: tails: (optional) must be one of :lower, :upper, or :both (default).
  See also: kixi.stats.core/z-test"
  [{mean-x :mean sd-x :sd n-x :n}
   {mean-y :mean sd-y :sd n-y :n}
   & [opts]]
  (when-let [sd-xy (and (pos? n-x) (pos? n-y)
                        (sqrt (+ (/ (sq sd-x) n-x)
                                 (/ (sq sd-y) n-y))))]
    (when-let [z (and (pos? sd-xy)
                      (double (/ (- mean-x mean-y) sd-xy)))]
      {:p-value (z-table z opts)})))
