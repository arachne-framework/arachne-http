(ns arachne.http.dsl.test
  "Contains DSL forms for defining a HTTP server in the configuration, with a runtime
   implementation that does exactly nothing. Useful for testing and not much else."
  (:require [clojure.test :refer :all]
            [arachne.core :as core]
            [arachne.core.config :as cfg]
            [arachne.core.dsl :as a]
            [arachne.http.dsl :as h]
            [arachne.core.config.script :as script :refer [defdsl]]
            [arachne.error :as e]
            [clojure.spec :as s]))

(defdsl create-dummy-server
  "Define a dummy Arachne server entity with the given Arachne ID and port. Return
  the tempid of the new server."
  (s/cat :port integer?)
  [port]
  (let [server-tid (cfg/tempid)]
    (script/transact [{:db/id server-tid
                       :arachne.component/constructor :clojure.core/hash-map
                       :arachne.http.server/port port}]
      server-tid)))

(s/fdef dummy-server
  :args (s/cat
          :port integer?
          :body (s/* any?)))

(defmacro dummy-server
  "Define a dummy HTTP server in the current configuration. Evaluates the body with
  the server bound as the context server. Returns the eid of the Server
  component.

  The dummy server looks correct from the perspective of the configuration, and is useful for
  testing, but does nothing when started."
  [port & body]
  `(let [server-eid# (create-dummy-server ~port)]
     (binding [h/*context-server* server-eid#]
       ~@body)
     server-eid#))