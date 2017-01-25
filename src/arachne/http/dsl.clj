(ns arachne.http.dsl
  (:require [arachne.core.config :as cfg]
            [arachne.core.util :as util]
            [arachne.core.config.script :as script :refer [defdsl]]
            [arachne.error :as e :refer [error deferror]]
            [clojure.spec :as s]
            [arachne.core.dsl :as core]
            [clojure.string :as str]))

(s/def ::http-method #{:options :get :head :post :put :delete :trace :connect})

(def ^:dynamic *context-server*
  "The eid of the HTTP server currently in context"
  nil)

(def ^:dynamic *context-path*
  "The path bound in the current context."
  nil)

(defn with-context
  "Given a path, return a path including any existing context path"
  [path]
  (if *context-path*
    (str *context-path* "/" path)
    path))

(s/fdef context
  :args (s/cat :path string?
               :body (s/* any?)))

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

  1. The wildcard name (if present)
  2. The param name (if present)
  3. The literal segment (if present)
  4. The regex constraint (if present)"
  #"(?:(\*[\w\-]+)?|(?::([\w\-]+)?)|([\w\-]+))(?:\(\/(.+)\/\))?")

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
      wild (assoc :arachne.http.route-segment/wildcard (keyword (subs wild 1)))
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
  (if-let [existing (find-segment (script/context-config) parent-eid segment-map)]
    (:db/id existing)
    (let [tid (cfg/tempid)]
      (script/transact [(assoc segment-map
                          :db/id tid
                          :arachne.http.route-segment/parent parent-eid)]
        tid))))

(deferror ::not-in-server-context
  :message "Cannot build routes outside a server context."
  :explanation "You used a DSL form that attempted to define HTTP routes for `:path`. However, this is only applicable inside of a `arachne.http.dsl/server` macro."
  :suggestions ["Check the stacktrace to see what DSL function you were evaluating when this error was thrown"
                "Make sure you only invoke route-building DSL functions from inside a `arachne.http.dsl/server` form."
                "If you're building your own DSL extensions, make sure that `arachne.http.dsl/*context-server*` is bound"])

(defn ensure-path
  "Given a path (as a string), ensure that all of the corresponding routing
  segments exist in the context configuration, creating them if they are not
  already present. Returns the entity ID of the final segment in the given
  path."
  [path]
  (reduce ensure-segment
    (if *context-server*
      *context-server*
      (error ::not-in-server-context {:path path}))
    (path-segments path)))

(s/def ::name keyword?)

(defdsl endpoint
  "Define the given component to be a HTTP endpoint and attaches it to the routing tree. The
   component instance should be of a type that can be used as an endpoint with whatever server
   you're using (such as a Pedestal interceptor, an Arachne Handler component or a Ring handler
   function).

   Arguments are:

   - Method(s) (mandatory): either a keyword or set of keywords indicating the HTTP methods
     that this endpoint supports.
   - Path (mandatory): The URL path to which the endpoint is attached.
   - Component (mandatory): A reference to the actual endpoint component. A component reference can
     be an Arachne ID of a component declared elsewhere in the config, or an eid (such as is
     returned by DSL forms, allowing endpoints to be defined inline.)
   - Options (optional): A map (or kwargs) of additional options.

  Supported options are:

   - :name - the name of the endpoint. If none is provided, one will later be inferred from the
     Arachne ID or handler function of the component.

   Returns the entity ID of the endpoint component"
  (s/cat :methods (s/or :one ::http-method
                        :many (s/coll-of ::http-method :min-count 1))
         :path string?
         :component ::core/ref
         :opts (util/keys** :opt-un [::name]))
  [methods path component & opts]
  (let [methods (let [[type methods] (:methods &args)]
                  (case type
                    :one #{methods}
                    :many (set methods)))
        path (with-context (:path &args))
        segment (ensure-path path)
        eid (core/resolved-ref (:component &args))
        name (-> &args :opts second :name)
        entity (util/mkeep {:db/id eid
                            :arachne.http.endpoint/route segment
                            :arachne.http.endpoint/methods methods
                            :arachne.http.endpoint/name name})]
    (script/transact [entity])
    eid))

(s/fdef arachne.http.dsl/handler
  :args (s/cat
          :arachne-id ::core/id
          :dependencies ::core/dependency-map
          :handler (s/and symbol? namespace)))

(defdsl handler
  "Defines a HTTP request handler component that uses a simple Ring-style
  request handler function.

  Arguments are:

  - Arachne ID (optional): An Arachne ID for the handler component
  - Handler function (mandatory): A symbol naming the handler function
  - Dependency map (optional): A component dependency map of {<key> <component-reference>}.
    A component reference may be an Arachne ID or entity ID.

  Dependencies will be assoc'd with the specified key to the Ring request map before it is
  passed to the supplied handler function."
  (s/cat :arachne-id (s/? ::core/arachne-id)
         :handler (s/and symbol? namespace)
         :dependencies (s/? ::core/dependency-map))
  [<arachne-id> handler <dependencies>]
  (let [tid (cfg/tempid)
        entity (util/mkeep
                 {:db/id tid
                  :arachne/id (:arachne-id &args)
                  :arachne.http.handler/fn (keyword (:handler &args))
                  :arachne.component/constructor :arachne.http/handler-component})
        txdata (map (fn [[k v]]
                      {:db/id tid
                       :arachne.component/dependencies
                       [{:arachne.component.dependency/key k
                         :arachne.component.dependency/entity (core/ref v)}]})
                 (:dependencies &args))
        txdata (conj txdata entity)]
    (script/transact txdata tid)))