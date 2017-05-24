(ns kixi.stats.test-runner
  #?(:cljs (:require [doo.runner :refer-macros [doo-tests]]
                     [kixi.stats.core-test]
                     [kixi.stats.math-test]
                     [kixi.stats.random-test])))

#?(:cljs (doo-tests 'kixi.stats.core-test
                    'kixi.stats.math-test
                    'kixi.stats.random-test))
