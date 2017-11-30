# kixi.stats

A Clojure/ClojureScript library of statistical sampling and transducing functions.

**Available distributions:**

* Bernoulli
* Beta
* Beta-binomial
* Binomial
* Categorical
* Chi-Squared
* Dirichlet
* Dirichlet-multinomial
* Exponential
* F
* Gamma
* Multinomial
* Normal
* Poisson
* Uniform
* Weibull

**Available transducing functions:**

* Count
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
[kixi/stats "0.3.10"]
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

(def g5t (filter #(> % 5)))

;; Calculate the median only of numbers greater than 5:

(transduce gt5 median (range 10))

;; => 7.5
```

So long as `xform` is a stateless transducer, we can use it to create a new reducing function locally which doesn't affect other reducing functions also being composed:

```clojure
(require '[kixi.stats.core :refer [count]])

(def gt5 (filter #(> % 5)))

;; Count both all numbers and those greater than 5:

(transduce identity (fuse {:n count :gt5 (gt5 count)}) (range 10))

;; => {:n 10, :gt5 4}
```

The `kixi.stats` API is focused primarily on statistical functions and doesn't need to be littered with exhaustive `count-when`-style specialisms. Combiantors from libraries such as [redux](https://github.com/henrygarner/redux) and Clojure itself can be used to combine those functions in sophisticated ways.

**Distribution sampling**

[kixi.stats.random](https://github.com/MastodonC/kixi.stats/blob/master/src/kixi/stats/random.cljc) contains functions for specifying and sampling from statistical distributions.

```clojure
(require '[kixi.stats.random :refer [draw sample binomial]])

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
(require '[kixi.stats.random :refer [sample-summary bernoulli]])

(sample-summary 1000 (bernoulli 0.3))

;;=> {true 296, false 704}
```

This is equivalent to `(frequencies (sample 1000 (bernoulli 0.3)))`, but where possible `sample-summary` uses optimisations to avoid reifying and aggregating a large intermediate sample, and should be preferred. When `sample-summary` doesn't return a value for a particular variate, that value should be assumed zero.

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

Copyright © 2017 Mastodon C Ltd

Distributed under the Eclipse Public License version 1.0.
