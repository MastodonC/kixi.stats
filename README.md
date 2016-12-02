# kixi.stats

A Clojure/ClojureScript library of statistical transducing functions. Currently implemented:

* Count
* Mean
* Variance
* Standard deviation
* Standard error
* Skewness
* Kurtosis
* Covariance
* Covariance matrix
* Correlation
* Correlation matrix
* Simple linear regression
* Standard error of the mean
* Standard error of the estimate
* Standard error of the prediction

Variance, standard deviation, skewness and kurtosis each have sample and population variants.

## Documentation

View the [documentation here](http://mastodonc.github.io/kixi.stats/).

Examples of `kixi.stats` usage can be seen between 10:20-16:00 of this video on [Clojure for Machine Learning](https://skillsmatter.com/skillscasts/9050-clojure-for-machine-learning).

## Installation

Add the following dependency:

```clojure
[kixi/stats "0.2.5"]
```

## Usage

[kixi.stats.core](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/core.cljc) contains statistical reducing functions that can be used with `transduce`:

```clojure
(require '[kixi.stats.core :refer [standard-deviation correlation])

(->> [{:x 2} {:x 4} {:x 4} {:x 4} {:x 5} {:x 5} {:x 5} {:x 7} {:x 9}]
     (transduce (map :x) standard-deviation))

;; => 2.0

(->>  [{:x 1 :y 3} {:x 2 :y 2} {:x 3 :y 1}]
      (transduce identity (correlation :x :y)))

;; => -1.0

(->> [{:x 1 :y 3 :z 2} {:x 2 :y 2 :z 4} {:x 3 :y 1 :z 6}]
     (transduce identity (correlation-matrix {:x :x :y :y :z :z})))

;; => {[:x :y] -1.0, [:x :z] 1.0, [:y :z] -1.0,
;;     [:y :x] -1.0, [:z :x] 1.0, [:z :y] -1.0}
```

If you have multiple statistics to calculate over the same collection, take a look at the reducing function combinators available in [redux](https://github.com/henrygarner/redux). Redux' `fuse` will return a higher-order reducing function that can be used to execute an arbitrary number of reducing functions simultaneously.

## References

Strongly influenced by [Tesser](https://github.com/aphyr/tesser).

## Contributors

  * [Henry Garner](https://github.com/henrygarner)

## License

Copyright © 2016 Mastodon C Ltd

Distributed under the Eclipse Public License version 1.0.
