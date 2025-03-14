# kixi.stats

A Clojure/ClojureScript library of statistical sampling and transducing functions.

<div align="center">

[![Build Status][build-status]][build-status-url]
[![License][license]][license-url]
[![cljdoc badge][cljdoc]][cljdoc-url]
[![Clojars Project][clojars]][clojars-url]

</div>

**Available distributions:**

* Bernoulli
* Beta
* Beta-binomial
* Binomial
* Categorical
* Cauchy
* Chi-Squared
* Dirichlet
* Dirichlet-multinomial
* Exponential
* F
* Gamma
* Log-normal
* Multinomial
* Normal
* Pareto
* Poisson
* Student's t
* Uniform
* Weibull

**Statistical tests:**

* Simple Z-test (one-sample location test)
* Two-sample Z-test
* Welch's unequal variances t-test
* Simple t-test
* Chi-squared test of categorical independence

**Available transducing functions:**

* Count
* Min
* Max
* Proportion
* (Arithmetic) mean
* Geometric mean
* Harmonic mean
* Median
* Variance
* Interquartile range
* Standard deviation
* Standard error
* Skewness
* Kurtosis
* Covariance
* Covariance matrix
* Correlation
* R-squared coefficient of determination
* Adjusted R-squared
* MSE / RMSE
* Correlation matrix
* Simple linear regression
* Standard error of the mean
* Standard error of the estimate
* Standard error of the prediction
* Simple Z-test & two-sample Z-test
* Simple t-test and two-sample t-test
* Chi-squared test

Variance, covariance, standard deviation, skewness and kurtosis each have sample and population variants.

## Documentation

View the [documentation here](https://cljdoc.org/d/kixi/stats).

## Installation

Install `kixi.stats` into your Clojure project using the appropriate form at its
Clojars page:

[![Clojars Project][clojars]][clojars-url]

Or grab the most recent code using a Git dependency:

```clj
replace $GIT_SHA with the sha you'd like to target!

{kixi/stats
  {:git/url "https://github.com/MastodonC/kixi.stats.git"
  :git/sha "$GIT_SHA"}}
```

## Usage

**Transducing functions**

[kixi.stats.core](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/core.cljc) contains statistical reducing functions that can be used with `transduce`:

```clojure
(require '[kixi.stats.core :refer [standard-deviation correlation]])

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

One advantage of using `transduce` for statistics calculation is that multiple statistics can be calculated simultaneously by composing together reducing functions. The generic combinators available in [redux](https://github.com/henrygarner/redux) or [xforms](https://github.com/cgrand/xforms) can be used together with the reducing functions in `kixi.stats`. For example, redux' `fuse` will return a higher-order reducing function that can be used to execute an arbitrary number of reducing functions simultaneously:

```clojure
(require '[kixi.stats.core :refer [mean standard-deviation]]
         '[redux.core :refer [fuse]])

;; Calculate mean and standard deviation at the same time:

(->> [2 4 4 4 5 5 5 7 9]
     (transduce identity (fuse {:mean mean :sd standard-deviation})))

;; => {:mean 5.0, :sd 2.0}
```

Integration with transducers means that the wealth of core Clojure support can be applied to working with statistics. For example, `filter` can be used to constrain the elements over which statistics are calculated:

```clojure
(require '[kixi.stats.core :refer [median]])

(def gt5? (filter #(> % 5)))

;; Calculate the median only of numbers greater than 5:

(transduce gt5? median (range 10))

;; => 7.5
```

So long as `xform` is a stateless transducer, we can use it to create a new reducing function locally which doesn't affect other reducing functions also being composed:

```clojure
(require '[kixi.stats.core :refer [count]]
         '[redux.core :refer [fuse]])

(def gt5? (filter #(> % 5)))

;; Count both all numbers and those greater than 5:

(transduce identity (fuse {:n count :gt5 (gt5? count)}) (range 10))

;; => {:n 10, :gt5 4}
```

The `kixi.stats` API is focused primarily on statistical functions and doesn't need to be littered with exhaustive `count-when`-style specialisms. Combinators from libraries such as [redux](https://github.com/henrygarner/redux) and Clojure itself can be used to combine those functions in sophisticated ways.

**Empirical distribution histograms**

The Clojure version of `kixi.stats.core` contains reducing functions for calculating the median, interquartile range and 5-number summary using the [t-digest](https://github.com/tdunning/t-digest). They can be used like this:

```clojure
(require '[kixi.stats.core :refer [median iqr summary]]
         '[redux.core :refer [fuse]])

;; Calculate the median, iqr and 5-number summary:

(->> (range 100)
     (transduce identity (fuse {:median median
                                :iqr iqr
                                :summary summary})))

;; => {:median 49.5, :iqr 50.0, :summary {:min 0.0, :q1 24.5, :median 49.5, :q3 74.5, :max 99.0, :iqr 50.0}}
```

Although this works fine, it should be noted that each function maintains its own digest. In cases where multiple quantiles must be calculated it's more efficient to calculate a single digest with the `histogram` function and subsequently query it with the equivalent functions from the `kixi.stats.distribution` namespace.

```clojure
(require '[kixi.stats.core :refer [histogram]]
         '[kixi.stats.distribution :refer [quantile]])

;; Calculate the 2.5 and 97.5 quantile from an empirical distribution

(def distribution
  (->> (range 100)
       (transduce identity histogram)))

{:lower (quantile distribution 0.025)
 :upper (quantile distribution 0.975)}

;; => {:lower 2.0, :upper 97.0}
```

The `post-complete` function defined in the `kixi.stats.core` allows us to chain the histogram and quantile steps like so:

```clojure
(require '[kixi.stats.core :refer [histogram post-complete]]
         '[kixi.stats.distribution :refer [quantile]])

;; Calculate the 2.5 and 97.5 quantile from an empirical disribution

(->> (range 100)
     (transduce identity (post-complete histogram
                           (fn [hist]
                             {:lower (quantile hist 0.025)
                              :upper (quantile hist 0.975)}))

;; => {:lower 2.0, :upper 97.0}
```

The `kixi.stats.distribution` namespace contains many functions for operating on histograms which mirror the names from `kixi.stats.core`: `cdf`, `iqr`, `minimum`, `maximum`, `quantile` and `summary`. In each case, the `kixi.stats.core` function will return a reducing function for use with `transduce` whereas the `kixi.stats.distribution` function will accept a calculated digest and return a value directly.

**Distribution sampling**

[kixi.stats.distribution](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/distribution.cljc) contains functions for specifying and sampling from statistical distributions.

```clojure
(require '[kixi.stats.distribution :refer [draw sample binomial]])

(draw (binomial {:n 100 :p 0.5}))

;;=> 54


(sample 10 (binomial {:n 100 :p 0.5}))

;;=> (49 53 53 44 55 47 45 51 49 51)
```

`draw` and `sample` are the primary means of extracting variates from a distribution. `draw` returns a single variate whereas `sample` returns _n_ variates.

Each distribution implements the `clojure.lang.ISeq` / `ISeqable`  interface, so _n_ variates can be sampled with `(take n (binomial {:n 100 :p 0.5}))`. However, where possible `sample` uses optimisations to return exactly _n_ variates, and should be preferred.

**Discrete summarisation**

The Bernoulli, binomial and categorical distributions are discrete, so samples can be summarised by counting the number of times each variate appears. Discrete distributions can be directly sampled in this way with `sample-summary`:

```clojure
(require '[kixi.stats.distribution :refer [sample-summary bernoulli]])

(sample-summary 1000 (bernoulli {:p 0.3}))

;;=> {true 296, false 704}
```

This is equivalent to `(frequencies (sample 1000 (bernoulli {:p 0.3})))`, but where possible `sample-summary` uses optimisations to avoid reifying and aggregating a large intermediate sample, and should be preferred. When `sample-summary` doesn't return a value for a particular variate, that value should be assumed zero.

**Deterministic sampling**

The sampling functions `draw`, `sample` and `sample-summary` are all designed to perform deterministically when provided with a seed value. If repeatable samples are desired, pass `{:seed SEED_LONG}` as the final argument:

```clojure
(require '[kixi.stats.distribution :refer [uniform]])

(draw (uniform {:a 0 :b 1}) {:seed 42})

;;=> 0.7415648787718233

(draw (uniform {:a 0 :b 1}) {:seed 42})

;;=> 0.7415648787718233
```

**Statistical tests**

The [kixi.stats.test](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/test.cljc) namespace contains functions for performing statistical tests.

For example, we can perform a z-test between a known population mean & standard deviation and a sampled mean with a given sample size in the following way:

```clojure
(require '[kixi.stats.test :refer [simple-z-test]])

(simple-z-test {:mu 100 :sd 12} {:mean 96 :n 55} {:tails :lower})

;;=> {:p-value 0.0067167326028858}
```

As with the `kixi.stats.distribution` namespace - which contains many functions which mirror `kixi.stats.core` - `simple-z-test` is also available in `kixi.stats.core`. The latter function returns a reducing function for use with `transduce`.

```clojure
(require '[kixi.stats.core :refer [simple-z-test]])

;; If the standard deviation is not provided, the sample standard deviation will be used instead (a 'plug-in test')
(transduce identity (simple-z-test {:mu 100}) (range 200))

;;=> {:p-value 0.9027648250246222}
```

## References

Statistical reducing functions strong influenced by [Tesser](https://github.com/aphyr/tesser).
Pseudorandom number generation is provided by [test.check](https://github.com/clojure/test.check/).

## Contributors

  * [Henry Garner](https://github.com/henrygarner)
  * [Simon Belak](https://github.com/sbelak)
  * [Elise Huard](https://github.com/elisehuard)
  * [Dorab Patel](https://github.com/bombaywalla)
  * [Sam Ritchie](https://github.com/sritchie)
  * [Oleksandr Yakushev](https://github.com/alexander-yakushev)

## License

Copyright © 2025 Mastodon C Ltd

Distributed under the Eclipse Public License version 1.0.

[build-status-url]: https://github.com/MastodonC/kixi.stats/actions?query=workflow%3A%22Clojure+CI%22
[build-status]: https://github.com/MastodonC/kixi.stats/workflows/Clojure%20CI/badge.svg?branch=master
[license-url]: LICENSE
[license]: https://img.shields.io/badge/license-EPLv1-brightgreen.svg
[cljdoc-url]: https://cljdoc.org/d/kixi/stats/CURRENT
[cljdoc]: https://cljdoc.org/badge/kixi/stats
[clojars-url]: https://clojars.org/kixi/stats
[clojars]: https://img.shields.io/clojars/v/kixi/stats.svg
