(ns arachne.http.schema
  (:require [arachne.core.config :refer [tempid]]
            [arachne.core.config.ontology :as o]))

(def schema
  (concat

    (o/class :arachne.http/Server [:arachne/Component
                                   :arachne.http/RouteSegment]
      "An abstract HTTP server"
      (o/attr :arachne.http.server/port :one :long
        "The network port upon which to run a HTTP server"))

    (o/class :arachne.http/RouteSegment []
      "A route segment; a node in a routing tree structure"
      (o/attr :arachne.http.route-segment/parent
        :one :arachne.http/RouteSegment
        "The parent route segment")
      (o/attr :arachne.http.route-segment/pattern
        :one-or-none :string
        "A literal path segment string pattern")
      (o/attr :arachne.http.route-segment/param
        :one-or-none :keyword
        "A URL path parameter")
      (o/attr :arachne.http.route-segment/wildcard
        :one-or-none :boolean
        "True if this is a wildcard path segment.")
      (o/attr :arachne.http.route-segment/constraint
        :one-or-none :string
        "A regular expression constraining the value of a param or wildcard segment."))

    (o/class :arachne.http/Endpoint [:arachne/Component]
      "A resolveable HTTP endpoint. The concrete type that this entails at runtime is implementation-specific, although all implementations should support instances of arachne.http.Handler as a least common denominator."
      (o/attr :arachne.http.endpoint/route
        :one :arachne.http/RouteSegment
        "Route at which this endpoint can serve")
      (o/attr :arachne.http.endpoint/name
        :one :keyword
        "Unique ID of an endpoint (used for URL generation)")
      (o/attr :arachne.http.endpoint/methods
        :one-or-more :keyword
        "One or more HTTP methods that this endpoint will respond to. Values should be in #{:options :get :head :post :put :delete :trace :connect}"))))