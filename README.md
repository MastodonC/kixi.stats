# kixi.stats

A Clojure/ClojureScript library of statistical transducing functions. Currently implemented:

* Count
* Mean
* Variance
* Standard deviation
* Skewness
* Kurtosis
* Covariance
* Correlation
* Simple linear regression

Variance, standard deviation, skewness and kurtosis each have sample and population variants.

## Documentation

View the [documentation here](http://mastodonc.github.io/kixi.stats/).

## Installation

Add the following dependency:

```clojure
[kixi/stats "0.1.2"]
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
```

## References

Strongly influenced by [Tesser](https://github.com/aphyr/tesser).

## Contributors

  * [Henry Garner](https://github.com/henrygarner)

## License

Copyright © 2015 Mastodon C Ltd

Distributed under the Eclipse Public License version 1.0.
