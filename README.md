# kixi.stats

A Clojure/ClojureScript library of statistical sampling and transducing functions.

**Available distributions:**

* Uniform
* Bernoulli
* Binomial
* Normal
* Categorical

**Transducing functions:**

* Count
* Arithmetic mean
* Geometric mean
* Harmonic mean
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

Variance, covariance, standard deviation, skewness and kurtosis each have sample and population variants.

## Documentation

View the [documentation here](http://mastodonc.github.io/kixi.stats/).

Examples of `kixi.stats` usage can be seen between 10:20-16:00 of this video on [Clojure for Machine Learning](https://skillsmatter.com/skillscasts/9050-clojure-for-machine-learning).

## Installation

Add the following dependency:

```clojure
[kixi/stats "0.3.2"]
```

## Usage

**Transducing functions**

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

**Distribution sampling**

[kixi.stats.random](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/random.cljc) contains functions for specifying and sampling from statistical distributions.

```clojure
(require '[kixi.stats.random :refer [draw sample binomial]])

(draw (binomial {:n 100 :p 0.5}))

;;=> 54


(sample 10 (binomial {:n 100 :p 0.5}))

;;=> (49 53 53 44 55 47 45 51 49 51)
```

`draw` and `sample` are the primary methods for extracting random variables from a distribution. `draw` returns a single value whereas `sample` returns _n_ values.

Each distribution implements the `clojure.lang.ISeq` interface, so an infinite lazy sequence can ge generated with `(seq (binomial {:n 100 :p 0.5)))`. However, where possible, `sample` uses optimisations to return exactly _n_ values, and should be preferred.

**Discrete summarisation**

The Bernoulli and categorical distributions are discrete distributions, so samples can be summarised by counting the number of elements of each sampled class. Discrete distributions can be sampled in this way with `sample-summary`:

```clojure
(require '[kixi.stats.random :refer [sample-summary bernoulli]])

(sample-summary 1000 (bernoulli 0.3))

;;=> {true 296, false 704}
```

This is the equivalent of `(frequencies (sample 1000 (bernoulli 0.3)))`, but as with `sample`, `sample-summary` uses internal optimisations to avoid realising and aggregating large number of samples, and should be preferred.

**Deterministic sampling**

The sampling functions `draw`, `sample` and `sample-summary` are all designed to perform deterministically when provided with a seed value. If repeatable samples are desired, pass `{:seed SEED_LONG}` as the final argument:

```clojure
(require '[kixi.stats.random :refer [uniform]])

(draw (uniform 0 1) {:seed 42})

;;=> 0.7415648787718233

(draw (uniform 0 1) {:seed 42})

;;=> 0.7415648787718233
```

## References

Statistical reducing functions strong influenced by [Tesser](https://github.com/aphyr/tesser).
Pseudorandom number generation is provided by [test.check](https://github.com/clojure/test.check/).

## Contributors

  * [Henry Garner](https://github.com/henrygarner)
  * [Simon Belak](https://github.com/sbelak)

## License

Copyright © 2016 Mastodon C Ltd

Distributed under the Eclipse Public License version 1.0.
