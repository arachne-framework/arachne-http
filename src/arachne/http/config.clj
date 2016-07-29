(ns arachne.http.config
  "Utilities for working with HTTP entities in a configuration"
  (:require [arachne.core.config :as cfg]))

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