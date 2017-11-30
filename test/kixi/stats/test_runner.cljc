(ns kixi.stats.test-runner
  #?(:cljs (:require [doo.runner :refer-macros [doo-tests]]
                     [kixi.stats.core-test]
                     [kixi.stats.math-test]
                     [kixi.stats.distribution-test])))

#?(:cljs (doo-tests 'kixi.stats.core-test
                    'kixi.stats.math-test
                    'kixi.stats.distribution-test))
