(ns kixi.stats.test
  (:require [kixi.stats.distribution :as d]
            [kixi.stats.math :refer [abs clamp pow sq sqrt]]
            [kixi.stats.protocols :as p]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(defrecord TestResult [statistic distribution alternate])

(defn significant?
  ([^TestResult {:keys [statistic distribution alternate] :as test} alpha]
   (when (and statistic distribution alternate alpha)
     (let [critical (d/critical-value distribution alpha alternate)]
       (case alternate
         :<> (> (abs statistic) critical)
         :<  (< statistic critical)
         :>  (> statistic critical)))))
  ([^TestResult test alpha alternate]
   (significant? (assoc test :alternate alternate) alpha)))

(defn p-value
  ([^TestResult {:keys [statistic distribution alternate]}]
   (when (and statistic distribution alternate)
     (case alternate
       :<> (clamp (* 2 (d/cdf distribution (- (abs statistic)))) 0.0 1.0)
       :<  (d/cdf distribution statistic)
       :>  (- 1 (d/cdf distribution statistic)))))
  ([^TestResult test alternate]
   (p-value (assoc test :alternate alternate))))

(defn test-result
  ([statistic distribution]
   (test-result statistic distribution :<>))
  ([statistic distribution alternate]
   (->TestResult statistic distribution alternate)))

(defn chi-squared-test
  "Calculates the X^2 test of independence for a given contingency table.
  See kixi.stats.core/cross-tabulate"
  [^kixi.stats.protocols.PContingencyTable table]
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
    (test-result stat (d/chi-squared dof) :>)))

(defn simple-z-test
  "Calculates the z-test of statistical significance for a sample mean.
  mu: the population mean
  sd: the population standard deviation
  mean: the sample mean
  n: the sample size
  See also: kixi.stats.core/simple-z-test"
  [{:keys [mu sd]} {:keys [mean n]}]
  (when (and (pos? n) (pos? sd))
    (let [z (double (/ (- mean mu) (/ sd (sqrt n))))]
      (test-result z (d/normal {:mu 0.0 :sd 1.0})))))

(defn z-test
  "Calculates the z-test of statistical significance between two sample means.
  Requires the mean, sd and sample size (n) of both samples.
  See also: kixi.stats.core/z-test"
  [{mean-x :mean sd-x :sd n-x :n}
   {mean-y :mean sd-y :sd n-y :n}]
  (when-let [sd-xy (and (pos? n-x) (pos? n-y)
                        (sqrt (+ (/ (sq sd-x) n-x)
                                 (/ (sq sd-y) n-y))))]
    (when-let [z (and (pos? sd-xy)
                      (double (/ (- mean-x mean-y) sd-xy)))]
      (test-result z (d/normal {:mu 0.0 :sd 1.0})))))

(defn t-test
  "Calculates Welch's unequal variances t-test of statistical significance.
  Requires the mean, sd and sample size (n) of both samples.
  See also: kixi.stats.core/t-test"
  [{mean-a :mean sd-a :sd n-a :n}
   {mean-b :mean sd-b :sd n-b :n}]
  (let [sd-ab (+ (/ (sq sd-a) n-a)
                 (/ (sq sd-b) n-b))
        t (/ (- mean-a mean-b)
             (sqrt sd-ab))
        dof (/ (sq sd-ab)
               (+ (/ (pow sd-a 4) (* n-a n-a (dec n-a)))
                  (/ (pow sd-b 4) (* n-b n-b (dec n-b)))))]
    (test-result t (d/t dof))))
 
(defn simple-t-test
  "Calculates the t-test of statistical significance for a sample mean.
  mu: the population mean
  sd: the population standard deviation
  mean: the sample mean
  n: the sample size
  See also: kixi.stats.core/simple-t-test"
  [{:keys [mu sd]} {:keys [mean n]}]
  (let [dof (dec n)
        t (and (pos? sd)
               (double (/ (- mean mu)
                          (/ sd (sqrt n)))))]
    (when (and t (pos? dof))
      (test-result t (d/t dof)))))
