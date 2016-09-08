(ns arachne.http.dsl
  (:require [arachne.core.config :as cfg]
            [arachne.core.util :as util]
            [arachne.core.config.init :as script :refer [defdsl]]
            [arachne.http.dsl.specs]
            [clojure.spec :as s]
            [clojure.string :as str]))

(def ^:dynamic *context-server*
  "The eid of the HTTP server currently in context"
  nil)

(def ^:dynamic *context-path*
  "The path bound in the current context."
  nil)

(defdsl create-server
  "Define an Arachne server entity with the given Arachne ID and port. Return
  the tempid of the new server."
  [arachne-id port]
  (let [server-tid (cfg/tempid)
        new-cfg (script/transact [{:db/id server-tid
                                   :arachne/id arachne-id
                                   :arachne.http.server/port port}])]
    (cfg/resolve-tempid new-cfg server-tid)))

(defmacro server
  "Define a HTTP server in the current configuration. Evaluates the body with
  the server bound as the context server. Returns the eid of the Server
  component."
  [arachne-id port & body]
  (apply util/validate-args `server arachne-id port body)
  `(let [server-eid# (create-server ~arachne-id ~port)]
     (binding [*context-server* server-eid#]
       ~@body)
     server-eid#))

(defn with-context
  "Given a path, return a path including any existing context path"
  [path]
  (if *context-path*
    (str *context-path* "/" path)
    path))

(defmacro context
  "Creates a context which scopes endpoint definitions (and possibly other types
  of definitions) to the specified path. For example,

    (context \"foo/bar\"
      (endpoint :get \"baz\" :some/handler))

  is the same as:

    (endpoint :get \"foo/bar/baz\" :some/handler).

  Concretely, this macro evaluates the body with *context-path* bound to the
  given path."
  [path & body]
  `(binding [*context-path* (with-context ~path)]
     ~@body))

;; Note: to use forward slashes in a wildcard constraint, you can always use the
;; unicode foward slash...

(def ^:private segment-re
  "Regex to parse a path segment. Yields the following capturing groups:

  1. The wildcard charcter (if present)
  2. The param name (if present)
  3. The literal segment (if present)
  4. The regex constraint (if present)"
  #"(?:(\*)?|(?::([\w\-]+)?)|([\w\-]+))(?:\(\/(.+)\/\))?")

(defn- parse-segment
  "Given a single path segment, return a partial entity map"
  [path segment]
  (let [[match wild param literal constraint]
        (re-matches segment-re segment)]
    (when-not match
      (throw
        (ex-info (format "Path segment \"%s\" was not valid, in path \"%s\""
                   segment path)
          {:segment segment, :path path})))
    (cond-> {}
      wild (assoc :arachne.http.route-segment/wildcard true)
      param (assoc :arachne.http.route-segment/param (keyword param))
      literal (assoc :arachne.http.route-segment/pattern literal)
      constraint (assoc :arachne.http.route-segment/constraint constraint))))

(def ^:private delimiter-re
  "Uses negative lookaround to match only forward slashes that are not next to
  an open/close paren"
  #"(?<!\()/(?!\))")

(defn- path-segments
  "Given a path as a string, return a seq of path segment entity map fragments"
  [path]
  (let [segments (str/split path delimiter-re)]
    (filter identity
      (for [segment segments]
        (when-not (str/blank? segment)
          (parse-segment path segment))))))

(defn- find-segment
  "Given a config, a parent eid and a partial entity map, return true a matching
  segment already exists"
  [cfg parent-eid segment-map]
  (let [children (cfg/q cfg '[:find [?child ...]
                              :in $ ?parent
                              :where
                              [?child :arachne.http.route-segment/parent ?parent]]
                   parent-eid)
        children (map #(cfg/pull cfg '[*] %) children)]
    (first (filter #(= segment-map (select-keys %
                                     [:arachne.http.route-segment/wildcard
                                      :arachne.http.route-segment/param
                                      :arachne.http.route-segment/pattern
                                      :arachne.http.route-segment/constraint]))
             children))))

(defn- ensure-segment
  "Given a parent entity ID and a partial entity map, ensure that the segment
  exists in the context configuration, returing its concrete entity ID."
  [parent-eid segment-map]
  (if-let [existing (find-segment @script/*config* parent-eid segment-map)]
    (:db/id existing)
    (let [tid (cfg/tempid)]
      (cfg/resolve-tempid
        (script/transact [(assoc segment-map
                            :db/id tid
                            :arachne.http.route-segment/parent parent-eid)])
        tid))))

(util/deferror ::not-in-server-context
  "Could not :action outside a server context. Make sure you are evaluating this inside a `server` macro (or with *context-server* otherwise bound).")

(defn ensure-path
  "Given a path (as a string), ensure that all of the corresponding routing
  segments exist in the context configuration, creating them if they are not
  already present. Returns the entity ID of the final segment in the given
  path."
  [path]
  (reduce ensure-segment
    (if *context-server*
      *context-server*
      (util/error ::not-in-server-context
        {:action "build a routing tree", :path path}))
    (path-segments path)))

(defdsl endpoint
  "Define a HTTP endpoint with the given method(s), path and either an Arachne
  ID or the entity ID of a component that implements the endpoint. Returns the
  resolved EID of the given component."
  [& args]
  (let [conformed (s/conform (:args (s/get-spec `endpoint)) args)
        methods (set (:methods conformed))
        arachne-id (-> conformed :identity val :arachne-id)
        name (or (-> conformed :identity val :name)
               arachne-id)
        path (with-context (:path conformed))
        segment (ensure-path path)
        eid (-> conformed :identity val :eid)
        tid (cfg/tempid)
        new-cfg (script/transact
                  [(util/mkeep {:db/id (or eid tid)
                                :arachne/id arachne-id
                                :arachne.http.endpoint/route segment
                                :arachne.http.endpoint/name name
                                :arachne.http.endpoint/methods methods})])]
    (or eid (cfg/resolve-tempid new-cfg tid))))
