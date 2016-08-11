(ns arachne.http
  (:require [arachne.core.util :as util]
            [arachne.http.config :as http-cfg]
            [arachne.http.schema :as schema]))

(defprotocol Handler
  "A logical Ring-style request handler which is also an Arachne component that
  can have dependencies, etc."
  (handle [this request] "Given a Ring-style request map, return a Ring-style
  response"))

(defn schema
  "Return the schema for the arachne.http module"
  []
  schema/schema)

(defn configure
  "Configure the arachne.http module"
  [cfg]
  cfg)


