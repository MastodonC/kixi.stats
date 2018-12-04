(ns kixi.stats.protocols)

(defprotocol PBounded
  (minimum [this] "Returns the minimum x")
  (maximum [this] "Returns the maximum x"))

(defprotocol PContingencyTable
  (cell [this coordinates] "Returns the cell identified by `coordinates`, which must contain a label for each of the table's dimensions.")
  (grand-total [this] "Returns the grand total")
  (margin-totals [this] "Returns the totals for all levels of all factors")
  (size [this] "Returns the table extent in each dimension"))

(defprotocol PDiscreteRandomVariable
  (sample-frequencies [this n rng]))

(defprotocol IPredictiveModel
  (predict [this x] "Returns the predicted value of y given x"))

(defprotocol PQuantile
  (cdf [this x] "Returns the cumulative probability for a given x")
  (quantile [this p] "Returns the x for a given cumulative probability"))

(defprotocol IRandomVariable
  (sample-1 [this rng])
  (sample-n [this n rng]))
