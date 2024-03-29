## 0.5.5 (2022-04-22)

* Add quantile and cdf support for the uniform and exponential distributions
* Assert `kixi.stats.distribution` parameters are within the valid range
* Update docstrings in `kixi.stats.core` to describe when a `kixi.stats.protocol` type is returned
* Fix bug in tests whereby positive and negative infinity would test equal
* Fix bug in `significant?` which returned an error when the number of tails was unspecified
* Exclude `abs` and `infinite?` to prevent compilation warnings

## 0.5.4 (2020-07-12)

* Test that quantile and cdf functions are inverses
* Fix gamma-pinv issue causing inaccurate chi-square quantiles to sometimes be returned
* Fix Student's t exception at extreme quantiles

## 0.5.3 (2020-07-05)

* Add Cauchy, Log-normal and Pareto distributions

## 0.5.2 (2019-07-31)

* Fix compilation error in `kixi.stats.distribution`

## 0.5.1 (2019-07-30)

* **BREAKING CHANGE** All distributions now expect named rather than positional args
* The normal distribution can now be paramaterised with `:location` & `:scale` to align more closely with other distributions
* The categorical distribution now expects a map of {category-name => probability}, rather than separate category and probability vectors

## 0.5.0 (2019-01-05)

* **BREAKING CHANGE** Protocol I prefix is replaced with P
* **BREAKING CHANGE** `kixi.stats.core/standard-error-estimate` renamed to `regression-standard-error`
* **BREAKING CHANGE** `kixi.stats.core/standard-error-prediction` renamed to `regression-prediction-standard-error`
* **BREAKING CHANGE** Tests in the `kixi.stats.test` namespace, e.g. `chi-squared-test`, `z-test` & `simple-t-test`, as well as those which mirror them in the `kixi.stats.core` namespace, now return a `TestResult`. This can be passed to helper functions `kixi.stats.test/p-value` and `kixi.stats.test/significant?`
* **BREAKING CHANGE** Tests in the `kixi.stats.test` namespace, e.g. `chi-squared-test`, `z-test` & `simple-t-test`, as well as those which mirror them in the `kixi.stats.core` namespace, no longer accept an optional `:tails` argument. Instead an equivalent argument is now provided to the helper functions in `kixi.stats.test`
* New protocols: `PInterval` for bounded intervals, `PDepdendent` & `PDependentWithSignificance` for returning functions of x,`PParameterised` for returning learned parameters, and `PTestResult` for returning a hypothesis test result
* Namespace `kixi.stats.distribution` now contains Student's t distribution
* New namespace `kixi.stats.estimate` contains functions which accept `sum-squares` as an argument and return a learned regression model, the standard error and intervals for the estimate and prediction
* Namespace `kixi.stats.digest` now also contains a `sum-squares` reducing function
* Distributions now implement clojure.lang.Seqble instead of clojure.lang.ISeq. This means that printing a distribution no longer causes an infinite sequence of draws to be printed

## 0.4.4 (2018-12-06)

* Baseline release
