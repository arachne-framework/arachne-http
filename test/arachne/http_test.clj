(ns arachne.http-test
  (:require [clojure.test :refer :all]
            [arachne.http.config :as http-cfg]
            [arachne.core.config.validation :as v]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.config.script :as init]

            [arachne.core.dsl :as a]
            [arachne.http :as http]
            [arachne.http.dsl :as h]
            [arachne.http.dsl.test :as dsltest])
  (:import (arachne ArachneException)))

(defn url-gen-cfg []

  (a/runtime :test/rt [:test/server])

  (a/component :test/handler 'clojure.core/hash-map)

  (dsltest/dummy-server :test/server 8080

    (h/endpoint :get "/" :test/handler :name :a)
    (h/endpoint :get "a/b/c" :test/handler :name :b)
    (h/endpoint :get "a/:foo/:bar" :test/handler :name :c)
    (h/endpoint :get "a/*wild" :test/handler :name :d)))

(deftest url-gen
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] `(url-gen-cfg))
        url-for (http/url-generator cfg :test/server)]

    (is (= "/" (url-for :a)))
    (is (= "/a/b/c" (url-for :b)))
    (is (= "/a/fred/bob" (url-for :c {:foo "fred" :bar "bob"})))
    (is (= "/a/some/nested/url" (url-for :d {:wild "some/nested/url"})))

    (is (thrown-with-msg? ArachneException #"Could not find endpoint"
          (url-for :no-such-route)))

    (is (thrown-with-msg? ArachneException #"Missing required URL parameter"
          (url-for :c {:foo "fred"})))

    (is (= "/a/fr+ed/bob" (url-for :c {:foo "fr ed" :bar "bob"})))
    (is (= "/a/fred/bob?baz=joe" (url-for :c {:foo "fred" :bar "bob" :baz "joe"})))

    (let [url (url-for :b {:a 1 :b 2 :c "x+y"})]
      (is (re-find #"a=1" url))
      (is (re-find #"b=2" url))
      (is (re-find #"c=x%2By" url)))))
