(ns arachne.http.config
  "Utilities for working with HTTP entities in a configuration"
  (:require [arachne.core.config :as cfg]
            [clojure.set :as set]
            [clojure.string :as str]))

(def route-rules
  "Datalog rule to recursively associate parent and child route segments in a routing tree.

  Provided rules are:

  - (routes ?ancestor ?descendant) - Link a route with all its parent, ancestor and child routes.
  - (endpoints ?route ?endpoint) - Link a route with all endpoints that are transitively or directly attached to it.

  "
  '[[(routes ?parent ?child)
     [?child :arachne.http.route-segment/parent ?parent]]
    [(routes ?ancestor ?descendant)
     [?descendant :arachne.http.route-segment/parent ?parent]
     (routes ?ancestor ?parent)]

    [(endpoints ?route ?endpoint)
     [?endpoint :arachne.http.endpoint/route ?route]]

    [(endpoints ?ancestor ?endpoint)
     (routes ?ancestor ?route)
     [?endpoint :arachne.http.endpoint/route ?route]]])

(defn servers
  "Find the eids of all the servers in the given configuration"
  [cfg]
  (cfg/q cfg '[:find [?server ...]
               :where
               [?server :arachne.http.server/port _]]))

(defn find-endpoint-handlers
  "Given a server eid, find the eids of all endpoint handlers in that server"
  [cfg server-eid]
  (cfg/q cfg '[:find [?handler ...]
               :in $ ?server %
               :where
               (endpoints ?server ?endpoint)
               [?endpoint :arachne.http.endpoint/handler ?handler]]
    server-eid route-rules))

(defn- infer-name
  "Infer a name for the given endpoint eid"
  [cfg endpoint-eid]
  (let [entity (cfg/pull cfg '[{:arachne.http.endpoint/handler [:db/id :arachne/id :arachne.http.handler/fn]}]
                 endpoint-eid)]
    (or (-> entity :arachne.http.endpoint/handler :arachne/id)
        (-> entity :arachne.http.endpoint/handler :arachne.http.handler/fn)
        (keyword (str (gensym))))))

(defn ^:no-doc infer-endpoint-names
  "Find all the endpoints without names, and try to infer names for them based on Arachne ID or handler function.

  Used internally during the module configure phase."
  [cfg]
  (let [endpoints (cfg/q cfg '[:find [?endpoint ...]
                               :in $
                               :where
                               [?endpoint :arachne.http.endpoint/route _]
                               [(missing? $ ?endpoint :arachne.http.endpoint/name)]])
        txdata (map (fn [endpoint]
                      {:db/id endpoint
                       :arachne.http.endpoint/name (infer-name cfg endpoint)}) endpoints)]
    (if (seq txdata)
      (cfg/with-provenance :module `infer-endpoint-names
        (cfg/update cfg txdata))
      cfg)))

(defn ^:no-doc add-endpoint-dependencies
  "Ensure that the Server entity has a dependency on all Endpoints under it (if a transitive
   dependency doesn't already exist)

   Used internally during the module configure phase."
  [cfg]
  (let [txdata (mapcat (fn [server]
                         (let [existing-deps (set (cfg/dependencies cfg server))
                               handlers (set (find-endpoint-handlers cfg server))
                               deps (set/difference handlers existing-deps)]
                           (map (fn [dep]
                                  {:db/id server
                                   :arachne.component/dependencies
                                   [{:arachne.component.dependency/entity dep}]})
                             deps)))
                 (servers cfg))]
    (if (seq txdata)
      (cfg/with-provenance :module `add-endpoint-dependencies
        (cfg/update cfg txdata))
      cfg)))

(defn endpoints
  "Return the EIDs of all endpoints that are children of the given route segment."
  [cfg segment-eid]
  (cfg/q cfg '[:find [?e ...]
               :in $ % ?root
               :where
               (endpoints ?root ?e)]
   route-rules segment-eid))


(defn route-segments
  "Return an ordered list of segment eids between the root server and the given route segment"
  [cfg segment-eid]
  (reverse
    (take-while identity
      (iterate (fn [eid]
                 (cfg/attr cfg eid :arachne.http.route-segment/parent :db/id))
        segment-eid))))

(defn route-path
  "Build a route path string for the given route segment eid."
  [cfg segment-eid]
  (let [segments (route-segments cfg segment-eid)
        path (str/join "/"
               (for [seg segments]
                 (let [s (cfg/pull cfg '[*] seg)
                       pattern (:arachne.http.route-segment/pattern s)
                       param (:arachne.http.route-segment/param s)
                       wild (:arachne.http.route-segment/wildcard s)]
                   (cond
                     pattern pattern
                     param param
                     wild (str "*" (name wild))))))]
    (if (str/blank? path)
      "/"
      path)))
