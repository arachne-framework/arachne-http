(ns arachne.http.dsl-test
  (:require [clojure.test :refer :all]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.dsl :as a]
            [arachne.http.dsl :as h]
            [arachne.core.config.init :as script :refer [defdsl]]
            [arachne.core.dsl.specs :as cspec]
            [arachne.error :as e]
            [clojure.spec :as s]))

(s/fdef create-dummy-server
  :args (s/cat :arachne-id ::cspec/id
          :port integer?))

(s/fdef dummy-server
  :args (s/cat :arachne-id ::cspec/id
          :port integer?
          :body (s/* any?)))

(defdsl create-dummy-server
  "Define a dummy Arachne server entity with the given Arachne ID and port. Return
  the tempid of the new server."
  [arachne-id port]
  (let [server-tid (cfg/tempid)
        new-cfg (script/transact [{:db/id server-tid
                                   :arachne/id arachne-id
                                   :arachne.component/constructor :clojure.core/hash-map
                                   :arachne.http.server/port port}])]
    (cfg/resolve-tempid new-cfg server-tid)))

(defmacro dummy-server
  "Define a dummy HTTP server in the current configuration. Evaluates the body with
  the server bound as the context server. Returns the eid of the Server
  component.

  The dummy server looks correct from the perspective of the configuration, and is useful for
  testing, but does nothing when started."
  [arachne-id port & body]
  `(let [server-eid# (create-dummy-server ~arachne-id ~port)]
     (binding [h/*context-server* server-eid#]
       ~@body)
     server-eid#))

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

  (a/component :test/handler-1 {} 'test/ctor)
  (a/component :test/handler-3 {} 'test/ctor)

  (dummy-server :test/server 8080

    (h/endpoint :get "/" :test/handler-1 :the-root)

    (h/context "/a/b/"

      (h/endpoint :get :head "/c/*"
        (a/component :test/handler-2 {} 'test/ctor)
        :handler-2-name)

      (h/endpoint :get "/:d(/[0-9]+/)/e" :test/handler-3))))

(deftest basic-endpoints
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/basic-endpoints-cfg))]

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
                     [?end1 :arachne/id :test/handler-1]
                     [?end1 :arachne.http.endpoint/route ?server]
                     [?server :arachne.http.server/port 8080]

                     [?end2 :arachne.http.endpoint/name :handler-2-name]
                     [?end2 :arachne.http.endpoint/methods :get]
                     [?end2 :arachne.http.endpoint/methods :head]
                     [?end2 :arachne/id :test/handler-2]
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
                     [?end3 :arachne/id :test/handler-3]
                     [?end3 :arachne.http.endpoint/route ?e]
                     [?e :arachne.http.route-segment/pattern "e"]
                     [?e :arachne.http.route-segment/parent ?d]
                     [?d :arachne.http.route-segment/param :d]
                     [?d :arachne.http.route-segment/constraint "[0-9]+"]
                     [?d :arachne.http.route-segment/parent ?b]]))))

(defn handler-endpoint-cfg
  []

  (a/runtime :test/rt [:test/server])

  (h/handler :test/handler {} 'test/handler)

  (dummy-server :test/server 8080
    (h/endpoint :get "/foo" :test/handler :the-handler))

  )

(deftest handler-endpoint
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http]
              '(arachne.http.dsl-test/handler-endpoint-cfg))]

    (is (cfg/q cfg '[:find ?handler .
                     :where

                     [?handler :arachne.http.endpoint/name :the-handler]
                     [?handler :arachne.http.endpoint/methods :get]
                     [?handler :arachne.http.handler/fn :test/handler]
                     [?handler :arachne.http.endpoint/route ?r]
                     [?r :arachne.http.route-segment/pattern "foo"]
                     [?r :arachne.http.route-segment/parent ?s]
                     [?s :arachne.http.server/port 8080]]))))