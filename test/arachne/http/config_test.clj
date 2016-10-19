(ns arachne.http.config-test
  (:require [clojure.test :refer :all]
            [arachne.http.config :as http-cfg]
            [arachne.core.config.validation :as v]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.config.init :as init]))

(def cfg-init '(do
                 (require '[arachne.core.dsl :as core])
                 (require '[arachne.http.dsl :as http])

                 (core/runtime :test/rt [:test/server])

                 (core/component :test/handler-1 {} 'test/ctor)
                 (core/component :test/handler-2 {} 'test/ctor)
                 (core/component :test/handler-3 {} 'test/ctor)

                 (http/server :test/server 8080

                   (http/endpoint :get "/" :test/handler-1)
                   (http/endpoint :get "a/b/c/d" :test/handler-2)
                   (http/endpoint :get "a/b/x/y" :test/handler-3))

                 (core/transact
                   [{:arachne/id :test/server
                     :arachne.component/constructor :no.such/constructor}])))

(deftest find-endpoints
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] cfg-init)
        servers (@#'http-cfg/servers cfg)
        deps (@#'http-cfg/find-endpoints cfg (first servers))]
    (is (= 1 (count servers)))
    (is (= 3 (count deps)))))

(deftest method-validation
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] cfg-init)
        cfg' (cfg/with-provenance :test `method-validation
               (init/apply-initializer cfg
                 (let [server (cfg/tempid)]
                   [{:db/id server
                     :arachne/id :test/server}
                    {:arachne.http.endpoint/methods :frobnicate
                     :arachne.http.endpoint/name :test/frob-handler
                     :arachne.component/constructor :no.such/constructor
                     :arachne.http.endpoint/route server}])))]
        (is (thrown-with-msg? arachne.ArachneException #"1 errors while validating"
              (v/validate cfg')))))

(deftest route-segment-validation
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] cfg-init)]
    (let [cfg' (cfg/with-provenance :test `route-segment-validation
                 (init/apply-initializer cfg
                   (let [server (cfg/tempid)]
                     [{:db/id server
                       :arachne/id :test/server}
                      {:arachne.http.route-segment/parent server}])))]
      (is (thrown-with-msg? arachne.ArachneException #"1 errors while validating"
            (v/validate cfg'))))
    (let [cfg' (cfg/with-provenance :test `route-segment-validation
                 (init/apply-initializer cfg
                   [{:arachne.http.route-segment/pattern "foo"}]))]
      (is (thrown-with-msg? arachne.ArachneException #"1 errors while validating"
            (v/validate cfg'))))))
