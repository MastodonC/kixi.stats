(ns kixi.stats.protocols)

(defprotocol IContingencyTable
  (cell [this coordinates] "Returns the cell identified by `coordinates`, which must contain a label for each of the table's dimensions.")
  (grand-total [this] "Returns the grand total")
  (margin-totals [this] "Returns the totals for all levels of all factors")
  (size [this] "Returns the table extent in each dimension"))
