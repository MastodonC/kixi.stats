(ns kixi.stats.test-runner
  #?(:cljs (:require [doo.runner :refer-macros [doo-tests]]
                     [kixi.stats.core-test]
                     [kixi.stats.estimate-test]
                     [kixi.stats.distribution-test]
                     [kixi.stats.math-test]
                     [kixi.stats.test-test])))

#?(:cljs (doo-tests 'kixi.stats.core-test
                    'kixi.stats.estimate-test
                    'kixi.stats.distribution-test
                    'kixi.stats.math-test
                    'kixi.stats.test-test))
