(ns arachne.http-test
  (:require [clojure.test :refer :all]
            [arachne.core]))

(deftest smoketest
  (is (= 2 (+ 1 1)) "Math still works. Phew."))
