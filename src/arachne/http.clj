(ns arachne.http
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.set :as set]
            [arachne.core.util :as util]
            [arachne.core.config :as cfg]
            [arachne.core.config.specs :as core-specs]
            [arachne.error :refer [error deferror]]
            [arachne.http.config :as http-cfg]
            [arachne.http.schema :as schema]
            [arachne.http.validators :as v])
  (:import [java.net URLEncoder]))

(defprotocol Handler
  "A Component representing a Ring-style request handler in which the response map is a function of the request map.

   Dependencies of a Handler component will be injected into the request map, using their dependency key."
  (handle [this request] "Given a Ring-style request map, return a Ring-style response map"))

(defrecord HandlerComponent [handler dep-keys]
  Handler
  (handle [this request]
    (handler (merge request (select-keys this dep-keys)))))

(defn handler-component
  "Constructor for a `Handler` component. The component must have a `:arachne.http.handler/fn`
   attribute indicating the handler function to which the request will be delegated."
  [cfg eid]
  (let [handler-kw (cfg/attr cfg eid :arachne.http.handler/fn)
        dep-keys (cfg/q cfg '[:find [?key ...]
                              :in $ ?component
                              :where
                              [?component :arachne.component/dependencies ?dep]
                              [?dep :arachne.component.dependency/key ?key]] eid)
        handler (util/require-and-resolve handler-kw)]
    (->HandlerComponent handler dep-keys)))

(defn ^:no-doc schema
  "Return the schema for the arachne.http module"
  []
  schema/schema)

(defn ^:no-doc configure
  "Configure the arachne.http module"
  [cfg]
  (-> cfg
    (v/add-validators)
    (http-cfg/infer-endpoint-names)
    (http-cfg/add-endpoint-dependencies)))

(deferror ::urlgen-missing
  :message  "Could not find endpoint named `:endpoint`"
  :explanation "Attempted to generate a URL from the named endpoint `:endpoint`, but could not because no endpoint with that name could be found in the specified server `server`."
  :suggestions ["Make sure the endpoint name is correct with no typos."]
  :ex-data-docs {:endpoint "name of the endpoint"
                 :server "the specified server in which to look"
                 :cfg "the Arachne configuration"})

(deferror ::urlgen-missing-param
  :message  "Missing required URL parameter `:param` to generate url for endpoint named `:endpoint`."
  :explanation "Attempted to generate a URL from the named endpoint `:endpoint`. However, the specified endpoint has a URL parameter or wildcard segment named `:param`, which was not specified in the given parameter map. Therefore, there is not enough data to generate a URL."
  :suggestions ["Provide a value to use for `:param` in your call to `url-for`"]
  :ex-data-docs {:endpoint "name of the endpoint"
                 :param "the missing URL parameter or wildcard"
                 :cfg "the Arachne configuration"})

(defn- segment-string
  "Return a function that, given a segment entity ID, returns an appropriate segment string"
  [cfg params endpoint-name server]
  (fn [{:keys [:arachne.http.route-segment/pattern
               :arachne.http.route-segment/param
               :arachne.http.route-segment/wildcard]}]
    (cond
      pattern pattern
      wildcard (or (get params wildcard)
                 (error ::urlgen-missing-param {:endpoint endpoint-name
                                                :param wildcard
                                                :cfg cfg}))
      param (URLEncoder/encode
              (or (get params param)
                (error ::urlgen-missing-param {:endpoint endpoint-name
                                               :param param
                                               :cfg cfg}))))))
(defn query-string
  "Create a URL query string from the parameters in the provided map. Values will be URL-encoded."
  [params]
  (let [s (str/join "&" (map (fn [[k v]]
                               (str (name k) "=" (URLEncoder/encode (str v) "UTF8")))
                            params))]
    (when-not (str/blank? s)
      (str "?" s))))

(defn- url-for-fn
  "Return a URL-generating function"
  [cfg server endpoint-map]
  (fn url-for
    ([endpoint-name] (url-for endpoint-name {}))
    ([endpoint-name params]
     (let [segments (get endpoint-map endpoint-name)
           url-params (->> segments
                        (keep #(or (:arachne.http.route-segment/param %)
                                   (:arachne.http.route-segment/wildcard %))))
           query-params (set/difference (set (keys params)) (set url-params))
           query-param-map (select-keys params query-params)]
       (when-not segments
         (error ::urlgen-missing {:endpoint endpoint-name
                                  :server server
                                  :cfg cfg}))
       (str (->> segments
              (map (segment-string cfg params endpoint-name server))
              (str/join "/")
              (str "/"))
            (query-string query-param-map))))))

(defn url-generator
  "Given a configuration and a server (Arachne ID or entity ID), return a function that will
   construct a query string given an endpoint name and an (optional) parameter map.

   Usage example:

       (def url-for (url-generator cfg :my/server))

       (url-for :my-route {:n 42})
       ;;=> \"/my/route/42\"

   If additional parameters are passed to the url generating function that are not url parameters,
   they will be added as query parameters.

   All URL and query parameters are URL encoded.

   Note: returns a higher order function for performance reasons. Calling `url-generator` performs an
   expensive Datalog query of the configuration, but calls to the returned function are fast.
   Therefore you should typically call `url-generator` once, and re-use the result."
  [cfg server]
  (let [server-ref (if (keyword? server) [:arachne/id server] server)
        endpoints (cfg/q cfg '[:find ?name ?segment
                               :in $ % ?server
                               :where
                               (endpoints ?server ?endpoint)
                               [?endpoint :arachne.http.endpoint/name ?name]
                               [?endpoint :arachne.http.endpoint/route ?segment]]
                    http-cfg/route-rules server-ref)
        endpoint-map (into {}
                       (map (fn [[name segment]]
                              (let [segment-eids (drop 1 (http-cfg/route-segments cfg segment))
                                    segments (map #(cfg/pull cfg '[:arachne.http.route-segment/pattern
                                                                   :arachne.http.route-segment/param
                                                                   :arachne.http.route-segment/wildcard] %)
                                               segment-eids)]
                                [name segments]))
                         endpoints))]
    (url-for-fn cfg server endpoint-map)))


