(ns arachne.http.dsl-test
  (:require [clojure.test :refer :all]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.dsl :as a]
            [arachne.http.dsl :as h]
            [arachne.core.config.script :as script :refer [defdsl]]
            [arachne.error :as e]
            [clojure.spec :as s]
            [arachne.http.dsl.test :refer [dummy-server]]
            [arachne.core.runtime :as rt])
  (:import (arachne ArachneException)))

(defn plain-server-cfg
  []
  (a/runtime :test/rt [:test/server])
  (dummy-server :test/server 8080))

(deftest plain-server
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/plain-server-cfg))]
    (is (= {:arachne/id :test/server
            :arachne.http.server/port 8080}
          (cfg/pull cfg [:arachne.http.server/port :arachne/id]
            [:arachne/id :test/server])))))

(defn basic-endpoints-cfg
  []

  (a/runtime :test/rt [:test/server])

  (a/component :test/handler-1 'clojure.core/hash-map)
  (a/component :test/handler-3 'clojure.core/hash-map)

  (dummy-server :test/server 8080

    (h/endpoint :get "/" :test/handler-1 :name :the-root)

    (h/context "/a/b/"

      (h/endpoint #{:get :head} "/c/*w" (a/component :test/handler-2 'clojure.core/hash-map) :name :handler-2-name)

      (h/endpoint :get "/:d(/[0-9]+/)/e" :test/handler-3))))

(deftest basic-endpoints
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/basic-endpoints-cfg))]

    (def the-cfg cfg)

    (is (= 1 (cfg/q cfg '[:find (count ?r) .
                          :where [?r :arachne.http.route-segment/pattern "a"]])))
    (is (= 1 (cfg/q cfg '[:find (count ?r) .
                          :where [?r :arachne.http.route-segment/pattern "b"]])))
    (is (= 1 (cfg/q cfg '[:find (count ?r) .
                          :where [?r :arachne.http.route-segment/pattern "c"]])))
    (is (= 1 (cfg/q cfg '[:find (count ?r) .
                          :where [?r :arachne.http.route-segment/param :d]])))
    (is (= 1 (cfg/q cfg '[:find (count ?r) .
                          :where
                          [?r :arachne.http.route-segment/param :d]
                          [?r :arachne.http.route-segment/constraint "[0-9]+"]])))
    (is (cfg/q cfg '[:find ?end1 .
                     :where

                     [?end1 :arachne.http.endpoint/name :the-root]
                     [?end1 :arachne.http.endpoint/methods :get]
                     [?end1 :arachne.http.endpoint/route ?server]
                     [?end1 :arachne.http.endpoint/handler ?h1]
                     [?h1 :arachne/id :test/handler-1]

                     [?server :arachne.http.server/port 8080]

                     [?end2 :arachne.http.endpoint/name :handler-2-name]
                     [?end2 :arachne.http.endpoint/methods :get]
                     [?end2 :arachne.http.endpoint/methods :head]
                     [?end2 :arachne.http.endpoint/route ?w]
                     [?end2 :arachne.http.endpoint/handler ?h2]
                     [?h2 :arachne/id :test/handler-2]
                     [?w :arachne.http.route-segment/wildcard :w]
                     [?w :arachne.http.route-segment/parent ?c]
                     [?c :arachne.http.route-segment/pattern "c"]
                     [?c :arachne.http.route-segment/parent ?b]
                     [?b :arachne.http.route-segment/pattern "b"]
                     [?b :arachne.http.route-segment/parent ?a]
                     [?a :arachne.http.route-segment/pattern "a"]
                     [?a :arachne.http.route-segment/parent ?server]

                     [?end3 :arachne.http.endpoint/name :test/handler-3]
                     [?end3 :arachne.http.endpoint/methods :get]
                     [?end3 :arachne.http.endpoint/route ?e]
                     [?end3 :arachne.http.endpoint/handler ?h3]
                     [?h3 :arachne/id :test/handler-3]
                     [?e :arachne.http.route-segment/pattern "e"]
                     [?e :arachne.http.route-segment/parent ?d]
                     [?d :arachne.http.route-segment/param :d]
                     [?d :arachne.http.route-segment/constraint "[0-9]+"]
                     [?d :arachne.http.route-segment/parent ?b]]))))

(defn handler-endpoint-cfg
  []
  (a/runtime :test/rt [:test/server])

  (dummy-server :test/server 8080

    (a/component :test/dep 'clojure.core/hash-map)

    (h/handler :test/handler2 'test/handler-fn-2 {:dep :test/dep})

    (h/endpoint :get "/foo" (h/handler 'test/handler-fn-1))
    (h/endpoint :get "/bar" :test/handler2)
    (h/endpoint :get "/baz" (h/handler 'test/handler-fn-1) :name :override-name)))

(deftest handler-endpoint
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/handler-endpoint-cfg))]

    (is (cfg/q cfg '[:find ?handler .
                     :where
                     [?handler :arachne.http.endpoint/name :test/handler-fn-1]
                     [?handler :arachne.http.endpoint/route ?r]
                     [?r :arachne.http.route-segment/pattern "foo"]]))

    (is (cfg/q cfg '[:find ?endpoint .
                     :where
                     [?endpoint :arachne.http.endpoint/name :test/handler2]
                     [?endpoint :arachne.http.endpoint/route ?r]
                     [?r :arachne.http.route-segment/pattern "bar"]
                     [?endpoint :arachne.http.endpoint/handler ?handler]
                     [?handler :arachne.component/dependencies ?d]
                     [?d :arachne.component.dependency/key :dep]
                     [?d :arachne.component.dependency/entity ?dep]
                     [?dep :arachne/id :test/dep]]))

    (is (cfg/q cfg '[:find ?handler .
                     :where
                     [?handler :arachne.http.endpoint/name :override-name]
                     [?handler :arachne.http.endpoint/route ?r]
                     [?r :arachne.http.route-segment/pattern "baz"]]))))

(defn duplicate-name-validation-cfg []

  (a/runtime :test/rt [:test/server])

  (dummy-server :test/server 8080

    (h/endpoint :get "/foo" (h/handler 'test/handler))
    (h/endpoint :get "/bar" (h/handler 'test/handler))))

(deftest duplicate-name-validation
  (is (thrown-with-msg? ArachneException #"1 errors while validating"
        (core/build-config [:org.arachne-framework/arachne-http]
          '(arachne.http.dsl-test/duplicate-name-validation-cfg)))))

(defn any-method-cfg []

  (a/runtime :test/rt [:test/server])

  (dummy-server :test/server 8080

    (h/endpoint :any "/foo" (h/handler 'test/handler))

    ))

(deftest any-method
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/any-method-cfg))]

    (is (cfg/q cfg '[:find ?e .
                     :where
                     [?e :arachne.http.endpoint/name :test/handler]
                     [?e :arachne.http.endpoint/methods :any]]))))