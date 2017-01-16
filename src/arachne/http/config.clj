(ns arachne.http.config
  "Utilities for working with HTTP entities in a configuration"
  (:require [arachne.core.config :as cfg]
            [clojure.set :as set]
            [clojure.string :as str]))

(def route-rules
  "Datalog rule to recursively associate parent and child route segments in a
  routing tree."
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

(defn find-endpoints
  "Given a server eid, find the eids of all endpoints in that server"
  [cfg server-eid]
  (cfg/q cfg '[:find [?endpoint ...]
               :in $ ?server %
               :where
               (endpoints ?server ?endpoint)]
    server-eid route-rules))

(defn- infer-name
  "Infer a name for the given endpoint eid"
  [cfg endpoint-eid]
  (let [entity (cfg/pull cfg '[:arachne/id :arachne.http.handler/fn] endpoint-eid)]
    (or (:arachne.http.handler/fn entity)
        (:arachne/id entity))))

(defn infer-endpoint-names
  "Find all the endpoints without names, and try to infer names for them based on Arachne ID or handler function"
  [cfg]
  (let [endpoints (cfg/q cfg '[:find [?endpoint ...]
                               :in $
                               :where
                               [?endpoint :arachne.http.endpoint/route _]
                               [(missing? $ ?endpoint :arachne.http.endpoint/name)]])
        names (map #(infer-name cfg %) endpoints)
        txdata (filter identity
                 (map (fn [endpoint name]
                         (when name
                           {:db/id endpoint
                            :arachne.http.endpoint/name name})) endpoints names))]
    (if (seq txdata)
      (cfg/with-provenance :module `infer-endpoint-names
        (cfg/update cfg txdata))
      cfg)))

(defn add-endpoint-dependencies
  "Ensure that the Server entity has a dependency on all Endpoints under it (if a transitive
   dependency doesn't already exist)"
  [cfg]
  (let [txdata (mapcat (fn [server]
                         (let [existing-deps (set (cfg/dependencies cfg server))
                               endpoints (set (find-endpoints cfg server))
                               endpoints (set/difference endpoints existing-deps)]
                           (map (fn [endpoint]
                                  {:db/id server
                                   :arachne.component/dependencies
                                   [{:arachne.component.dependency/entity endpoint}]})
                             endpoints)))
                 (servers cfg))]
    (if (seq txdata)
      (cfg/with-provenance :module `add-endpoint-dependencies
        (cfg/update cfg txdata))
      cfg)))

(defn endpoints
  "Return the EIDs of all endpoints that are children of a root route segment"
  [cfg root-eid]
  (cfg/q cfg '[:find [?e ...]
               :in $ % ?root
               :where
               (endpoints ?root ?e)]
   route-rules root-eid))

(defn route-segments
  "Return an ordered list of segments between the root server and a given endpoint"
  [cfg eid]
  (reverse
    (take-while identity
      (iterate (fn [eid]
                 (cfg/attr cfg eid :arachne.http.route-segment/parent :db/id))
        (cfg/attr cfg eid :arachne.http.endpoint/route :db/id)))))

(defn route-path
  "Build a route path , given an endpoint eid."
  [cfg endpoint]
  (let [segments (route-segments cfg endpoint)
        path (str/join "/"
               (for [seg segments]
                 (let [s (cfg/pull cfg '[*] seg)]
                   (or (:arachne.http.route-segment/pattern s)
                       (:arachne.http.route-segment/param s)
                       (when (:arachne.http.route-segment/wildcard s) "*")))))]
    (if (str/blank? path)
      "/"
      path)))