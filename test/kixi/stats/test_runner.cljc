(ns kixi.stats.test-runner
  #?(:cljs (:require [doo.runner :refer-macros [doo-tests]]
                     [kixi.stats.core-test])))

#?(:cljs (doo-tests 'kixi.stats.core-test))
