(ns arachne.http
  (:require [arachne.core.util :as util]
            [arachne.http.config :as http-cfg]
            [arachne.http.schema :as schema]))

(defprotocol Handler
  (handler [this] "Return a Ring-style request handler function"))

(defn schema
  "Return the schema for the core module"
  []
  schema/schema)

(defn configure
  "Configure the core module"
  [cfg]
  cfg)
