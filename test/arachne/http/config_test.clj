(ns arachne.http.config-test
  (:require [clojure.test :refer :all]
            [arachne.http.config :as http-cfg]
            [arachne.core.config.validation :as v]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.config.script :as init]

            [arachne.core.dsl :as a]
            [arachne.http.dsl :as h]
            [arachne.http.dsl.test :as dsltest]

            [arachne.core.runtime :as rt]
            [com.stuartsierra.component :as c]))

(defn test-cfg []

  (a/runtime :test/rt [:test/server])

  (a/component :test/handler-1 'clojure.core/hash-map)
  (a/component :test/handler-2 'clojure.core/hash-map)
  (a/component :test/handler-3 'clojure.core/hash-map)

  (dsltest/dummy-server :test/server 8080

    (h/endpoint :get "/" :test/handler-1)
    (h/endpoint :get "a/b/c/d" :test/handler-2)
    (h/endpoint :get "a/b/x/y" :test/handler-3)))

(deftest find-endpoints
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] `(test-cfg))
        servers (@#'http-cfg/servers cfg)
        deps (@#'http-cfg/find-endpoints cfg (first servers))]
    (is (= 1 (count servers)))
    (is (= 3 (count deps)))))

(deftest method-validation
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] `(test-cfg))
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
              (v/validate cfg' true)))))

(deftest route-segment-validation
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] `(test-cfg))]
    (let [cfg' (cfg/with-provenance :test `route-segment-validation
                 (init/apply-initializer cfg
                   (let [server (cfg/tempid)]
                     [{:db/id server
                       :arachne/id :test/server}
                      {:arachne.http.route-segment/parent server}])))]
      (is (thrown-with-msg? arachne.ArachneException #"1 errors while validating"
            (v/validate cfg' true))))
    (let [cfg' (cfg/with-provenance :test `route-segment-validation
                 (init/apply-initializer cfg
                   [{:arachne.http.route-segment/pattern "foo"}]))]
      (is (thrown-with-msg? arachne.ArachneException #"1 errors while validating"
            (v/validate cfg' true))))))

(deftest route-path-test
  (let [cfg (core/build-config [:org.arachne-framework/arachne-http] `(test-cfg))
        rt (rt/init cfg [:arachne/id :test/rt])
        rt (c/start rt)]
    (is (= "/a/b/x/y"
          (http-cfg/route-path cfg [:arachne/id :test/handler-3])))))
