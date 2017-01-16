(ns arachne.http.validators
  (:require [arachne.core.config :as cfg]
            [arachne.core.config.validation :as v]
            [arachne.core.config.model :as ont]
            [arachne.error :as e :refer [error deferror]]
            [arachne.http.config :as http-cfg]
            [arachne.core.util :as util]
            [clojure.string :as str]))

(def legal-methods #{:options :get :head :post :put :delete :trace :connect})

(deferror ::unknown-http-method
  :message "Unknown HTTP method `:method`"
  :explanation "The configuration specified that an endpoint with name `:endpoint-name` and entity ID `:endpoint` should support the HTTP method `:method`, but that is not a valid HTTP method type.

  Supported values are `#{:options :get :head :post :put :delete :trace :connect}`"
  :suggestions ["Use a valid HTTP method"]
  :ex-data-docs {:method "The invalid method"
                 :endpoint "The endpoint with the invalid method"
                 :endpoint-name "The endpoint's name"})

(defn method-types
  "Ensure that HTTP method types are in the allowed set"
  [cfg]
  (let [methods (cfg/q cfg '[:find ?endpoint ?name ?method
                             :in $
                             :where
                             [?endpoint :arachne.http.endpoint/methods ?method]
                             [?endpoint :arachne.http.endpoint/name ?name]])]
    (for [[endpoint name method] methods]
      (when-not (legal-methods method)
        (e/arachne-ex ::unknown-http-method {:method method
                                             :endpoint endpoint
                                             :endpoint-name name})))))

(deferror ::missing-parent
  :message "Route segment missing parent"
  :explanation "Every route segment (except for servers, which are technically route segments) must have a parent route segment, but the following segment did not:

      `:segment`"
  :suggestions ["Make sure the route segment has a parent route segment, as defined in the configuration."]
  :ex-data-docs {:segment "The invalid route segment."})

(deferror ::missing-specification
  :message "Route segment must have pattern, paramter or wildcard"
  :explanation "Every route segment (except for servers, which are technically route segments) must have either a pattern, a paramter or be marked as a wildcard. However, the following segment did not:

      `:segment`"
  :suggestions ["Make sure the segment is defined correclty in the configuration."]
  :ex-data-docs {:segment "The invalid route segment."})

(defn valid-route-segments
  "Ensure that route segments have at least one of their required attributes"
  [cfg]
  (let [segments (cfg/q cfg '[:find [?rs ...]
                              :in $ %
                              :where
                              [?rs-cls :db/ident :arachne.http/RouteSegment]
                              (type ?rs-cls ?rs)]
                   ont/rules)]
    (for [segment segments]
      (let [s (cfg/pull cfg '[*] segment)]
        (cond
          (and (not (:arachne.http.route-segment/parent s))
               (not (:arachne.http.server/port s)))
          (e/arachne-ex ::missing-parent {:segment s})
          (and (not (:arachne.http.server/port s))
               (not (:arachne.http.route-segment/param s))
               (not (:arachne.http.route-segment/pattern s))
               (not (:arachne.http.route-segment/wildcard s)))
          (e/arachne-ex ::missing-specification {:segment s}))))))

(deferror ::duplicate-name
  :message "Duplicate endpoint name `:name` on server `:server-eid` (Arachne ID: `:server-aid`)"
  :explanation "Endpoint names must be unique in the context of a server. This is important for things like route generation, so that an endpoint can be uniquely identified.

  In this case, the name `:name` was used by all the following endpoints:

  :formatted-endpoints

  This can happen because, by default, the endpoint name is derived from either the `:arachne/id` of the endpoint, or the `:arachne.http.handler/fn` of a handler component.

  If the same component or handler function is used for more than one endpoint, the name may be inadvertently duplicated."
  :suggestions ["Explicitly define a unique name for the endpoint in your configuration script."]
  :ex-data-docs {:server-eid "Entity ID of the server"
                 :server-aid "Aachne ID of the server"
                 :name "The name that was duplicated"
                 :endpoints "Eids of the duplicated endpoints"
                 :formatted-endpoints "Formatted string of the duplicated endpoints"})

(defn unique-endpoint-names
  "Ensure that every endpoint name is used only once, in the context of the same server"
  [cfg]
  (let [names (cfg/q cfg '[:find ?server ?name ?endpoint
                           :in $ %
                           :where
                           [?server :arachne.http.server/port _]
                           (endpoints ?server ?endpoint)
                           [?endpoint :arachne.http.endpoint/name ?name]]
                http-cfg/route-rules)
        duplicates (->> names
                     (group-by #(take 2 %))
                     (vals)
                     (filter #(< 1 (count %))))]
    (for [[dup & _ :as dups] duplicates]
      (let [server (first dup)
            name (second dup)
            endpoints (map #(nth % 2) dups)
            formatted-endpoints (str/join "\n" (map (fn [endpoint]
                                                      (str " - " endpoint
                                                        " (Arachne ID: "
                                                        (or (cfg/attr cfg endpoint :arachne/id) "<none>")
                                                        ")"))
                                                 endpoints))]
        (e/arachne-ex ::duplicate-name {:server-eid server
                                        :server-aid (cfg/attr cfg server :arachne/id)
                                        :name name
                                        :endpoints endpoints
                                        :formatted-endpoints formatted-endpoints})))))

;; TODO: write module configurer to add names based on :arachne/id or :arachne.http.handler/fn

(def validators [:arachne.http.validators/method-types
                 :arachne.http.validators/valid-route-segments
                 :arachne.http.validators/unique-endpoint-names])

(defn add-validators
  "Add HTTP validator functions to the config"
  [cfg]
  (v/add-validators cfg validators))

