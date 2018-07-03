(ns hiposfer.kamal.specs.resources
  (:require [clojure.spec.alpha :as s]
            [hiposfer.kamal.services.routing.core :as routing]))

;; according to json api both type and id MUST be strings
(s/def ::id (s/or :text (s/and string? not-empty)
                  :number number?))

(s/def ::type (set (for [[k v] routing/schema
                         :when (contains? v :db.unique)]
                     (namespace k))))

(s/def ::resource (s/keys :req-un [::id]))

;;;; REQUEST

(s/def ::area ::id)
(s/def ::params (s/keys :req-un [::area ::type ::id]))

;;;; RESPONSE

(s/def ::data (s/merge ::resource (s/map-of simple-keyword? any?)))
(s/def ::response (s/map-of simple-keyword? ::data :count 1))
