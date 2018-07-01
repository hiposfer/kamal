(ns hiposfer.kamal.specs.resources
  (:require [clojure.spec.alpha :as s]
            [hiposfer.kamal.services.routing.core :as routing]))

;; according to json api both type and id MUST be strings
(s/def ::id string?)
(s/def ::type (set (for [[k v] routing/schema
                         :when (contains? v :db.unique)]
                     (namespace k))))
(s/def ::attributes (s/map-of keyword? any?))
(s/def ::relationships (s/keys :req-un [::data])) ;; data or links or meta

(s/def ::resource (s/keys :req-un [::type ::id]
                          :opt-un [::attributes ::relationships]))

(s/def ::data (s/nilable ::resource))

;;;; REQUEST

(s/def ::area ::id)
(s/def ::params (s/keys :req-un [::area ::type ::id]))

;;;; RESPONSE

(s/def ::response   (s/keys :req-un [::data]))

