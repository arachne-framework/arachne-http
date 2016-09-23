(ns arachne.http.validators
  (:require [arachne.core.config :as cfg]
            [arachne.core.config.validation :as v]
            [arachne.core.config.ontology :as ont]
            [arachne.core.util :as util]))

(def legal-methods #{:options :get :head :post :put :delete :trace :connect})

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
        {::v/message "Unknown HTTP method"
         :method method
         :endpoint endpoint
         :endpoint-name name}))))

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
          {::v/message "All route segments (except for servers) must have a parent route segment."
           :segment s}

          (and (not (:arachne.http.server/port s))
               (not (:arachne.http.route-segment/param s))
               (not (:arachne.http.route-segment/pattern s))
               (not (:arachne.http.route-segment/wildcard s)))
          {::v/message "All route segments (except for servers) must have either a pattern, paramter or wildcard specified."
           :segment s}
          )))))


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

