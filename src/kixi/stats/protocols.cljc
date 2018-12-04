(ns kixi.stats.protocols)

(defprotocol PBounded
  (minimum [this] "Returns the minimum x")
  (maximum [this] "Returns the maximum x"))

(defprotocol PInterval
  (lower [this] "Returns the lower bound")
  (upper [this] "Returns the upper bound"))

(defprotocol PContingencyTable
  (cell [this coordinates] "Returns the cell identified by `coordinates`, which must contain a label for each of the table's dimensions.")
  (grand-total [this] "Returns the grand total")
  (margin-totals [this] "Returns the totals for all levels of all factors")
  (size [this] "Returns the table extent in each dimension"))

(defprotocol PDiscreteRandomVariable
  (sample-frequencies [this n rng]))

(defprotocol PDependent
  (measure [this x] "Returns the value of a dependent variable at a given x"))

(defprotocol PDependentWithSignificance
  (measure-with-significance [this x alpha]
    "Returns the value of a dependent variable at a given x and significance level"))

(defprotocol PParameterised
  (parameters [this] "Returns the learned parameters"))

(defprotocol PQuantile
  (cdf [this x] "Returns the cumulative probability for a given x")
  (quantile [this p] "Returns the x for a given cumulative probability"))

(defprotocol PRandomVariable
  (sample-1 [this rng])
  (sample-n [this n rng]))
