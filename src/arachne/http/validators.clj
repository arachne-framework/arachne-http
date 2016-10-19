(ns arachne.http.validators
  (:require [arachne.core.config :as cfg]
            [arachne.core.config.validation :as v]
            [arachne.core.config.ontology :as ont]
            [arachne.error :as e :refer [error deferror]]
            [arachne.core.util :as util]))

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
                              (class ?rs-cls ?rs)]
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

(def validators [:arachne.http.validators/method-types
                 :arachne.http.validators/valid-route-segments])

(defn add-validators
  "Add HTTP validator functions to the config"
  [cfg]
  (let [cfg-eids (cfg/q cfg '[:find [?cfg ...]
                              :where [?cfg :arachne.configuration/roots _]])]
    (cfg/with-provenance :module `add-validators
      (cfg/update cfg (for [c cfg-eids v validators]
                        [:db/add c :arachne.configuration/validators v])))))

