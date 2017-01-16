(ns arachne.http
  (:require [arachne.core.util :as util]
            [arachne.core.config :as cfg]
            [arachne.http.config :as http-cfg]
            [arachne.http.schema :as schema]
            [arachne.http.validators :as v]))

(defprotocol Handler
  "A logical Ring-style request handler which is also an Arachne component that
  can have dependencies, etc."
  (handle [this request] "Given a Ring-style request map, return a Ring-style
  response"))

(defrecord HandlerComponent [handler dep-keys]
  Handler
  (handle [this request]
    (handler (merge request (select-keys this dep-keys)))))

(defn handler-component
  "Constructor for a handler component"
  [cfg eid]
  (let [handler-kw (cfg/attr cfg eid :arachne.http.handler/fn)
        dep-keys (cfg/q cfg '[:find [?key ...]
                              :in $ ?component
                              :where
                              [?component :arachne.component/dependencies ?dep]
                              [?dep :arachne.component.dependency/key ?key]] eid)
        handler (util/require-and-resolve handler-kw)]
    (->HandlerComponent handler dep-keys)))

(defn schema
  "Return the schema for the arachne.http module"
  []
  schema/schema)

(defn configure
  "Configure the arachne.http module"
  [cfg]
  (-> cfg
    (v/add-validators)
    (http-cfg/infer-endpoint-names)))
