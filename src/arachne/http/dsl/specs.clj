(ns arachne.http.dsl.specs
  (:require [clojure.spec :as s]
            [arachne.core.dsl.specs :as cspec]))

(s/fdef arachne.http.dsl/create-server
  :args (s/cat :arachne-id ::cspec/id
               :port integer?))

(s/fdef arachne.http.dsl/server
  :args (s/cat :arachne-id ::cspec/id
               :port integer?
               :body (s/* any?)))

(s/fdef arachne.http.dsl/context
  :args (s/cat :path string?
               :body (s/* any?)))

(s/def ::http-method #{:options :get :head :post :put :delete :trace :connect})

(s/fdef arachne.http.dsl/endpoint
  :args (s/cat
          :methods (s/+ ::http-method)
          :path string?
          :identity (s/alt
                      :by-eid (s/cat :eid pos-int? :name keyword?)
                      :by-arachne-id (s/cat :arachne-id ::cspec/id
                                       :name (s/? keyword?)))))