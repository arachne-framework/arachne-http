(ns arachne.http.config
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

(defn- servers
  [cfg]
  (cfg/q cfg '[:find [?server ...]
               :where
               [?server :arachne.http.server/port _]]))

(defn- find-server-dependencies
  [cfg server-eid]
  (cfg/q cfg '[:find [?impl ...]
               :in $ ?server %
               :where
               (endpoints ?server ?endpoint)
               [?endpoint :arachne.http.endpoint/implementation ?impl]]
    server-eid route-rules))

(defn- add-server-dependencies
  [cfg server-eid]
  (let [dep-eids (find-server-dependencies cfg server-eid)
        deps (map (fn [dep-eid]
                    {:arachne.component.dependency/entity dep-eid
                     :arachne.component.dependency/key
                     (keyword (str "endpoint-" dep-eid))})
               dep-eids)]
    (if (empty? deps)
      cfg
      (cfg/update cfg
        [{:db/id server-eid
          :arachne.component/dependencies deps}]))))

(defn add-endpoint-dependencies
  "Add every endpoint implementation as a direct dependency of its associated
  server"
  [cfg]
  (reduce add-server-dependencies cfg (servers cfg)))
