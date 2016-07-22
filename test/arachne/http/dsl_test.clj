(ns arachne.http.dsl-test
  (:require [clojure.test :refer :all]
            [arachne.core :as core]
            [arachne.core.config :as cfg]))

(deftest plain-server
  (let [cfg (core/build-config "test" '[:org.arachne-framework/arachne-http]
              '(do
                 (require '[arachne.core.dsl :as core])
                 (require '[arachne.http.dsl :as http])

                 (core/runtime :test/rt [:test/server])
                 (http/server :test/server 8080)))]
    (is (= {:arachne/id :test/server
            :arachne.http.server/port 8080}
          (cfg/pull cfg [:arachne.http.server/port :arachne/id]
            [:arachne/id :test/server])))))


(deftest basic-endpoints
  (let [cfg (core/build-config "test" '[:org.arachne-framework/arachne-http]
              '(do

                 (require '[arachne.core.dsl :as core])
                 (require '[arachne.http.dsl :as http])

                 (core/runtime :test/rt [:test/server])

                 (core/component :test/handler-1 {} 'test/ctor)
                 (core/component :test/handler-2 {} 'test/ctor)
                 (core/component :test/handler-3 {} 'test/ctor)

                 (http/server :test/server 8080

                   (http/endpoint :get "/" :test/handler-1 :the-root)

                   (http/context "/a/b/"

                     (http/endpoint #{:get :head} "/c/*" :test/handler-2)
                     (http/endpoint :get "/:d(/[0-9]+/)/e" :test/handler-3)))))]

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
    (is (cfg/q cfg '[:find [?end1]
                     :where

                     [?end1 :arachne.http.endpoint/name :the-root]
                     [?end1 :arachne.http.endpoint/methods :get]
                     [?end1 :arachne.http.endpoint/implementation ?i1]
                     [?i1 :arachne/id :test/handler-1]
                     [?end1 :arachne.http.endpoint/route ?server]
                     [?server :arachne.http.server/port 8080]

                     [?end2 :arachne.http.endpoint/name :test/handler-2]
                     [?end2 :arachne.http.endpoint/methods :get]
                     [?end2 :arachne.http.endpoint/methods :head]
                     [?end2 :arachne.http.endpoint/implementation ?i2]
                     [?i2 :arachne/id :test/handler-2]
                     [?end2 :arachne.http.endpoint/route ?w]
                     [?w :arachne.http.route-segment/wildcard true]
                     [?w :arachne.http.route-segment/parent ?c]
                     [?c :arachne.http.route-segment/pattern "c"]
                     [?c :arachne.http.route-segment/parent ?b]
                     [?b :arachne.http.route-segment/pattern "b"]
                     [?b :arachne.http.route-segment/parent ?a]
                     [?a :arachne.http.route-segment/pattern "a"]
                     [?a :arachne.http.route-segment/parent ?server]

                     [?end3 :arachne.http.endpoint/name :test/handler-3]
                     [?end3 :arachne.http.endpoint/methods :get]
                     [?end3 :arachne.http.endpoint/implementation ?i3]
                     [?i3 :arachne/id :test/handler-3]
                     [?end3 :arachne.http.endpoint/route ?e]
                     [?e :arachne.http.route-segment/pattern "e"]
                     [?e :arachne.http.route-segment/parent ?d]
                     [?d :arachne.http.route-segment/param :d]
                     [?d :arachne.http.route-segment/constraint "[0-9]+"]
                     [?d :arachne.http.route-segment/parent ?b]]))
    )
  )