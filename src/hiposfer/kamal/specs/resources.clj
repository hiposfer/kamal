(ns hiposfer.kamal.specs.resources
  (:require [clojure.spec.alpha :as s]
            [hiposfer.kamal.services.routing.core :as routing]))

;; according to json api both type and id MUST be strings
(s/def ::id (s/or :text (s/and string? not-empty)
                  :number number?))

(s/def ::name (set (for [[k v] routing/schema
                         :when (contains? v :db.unique)]
                     (namespace k))))

(s/def ::resource (s/keys :req-un [::id]))

;;;; REQUEST

(s/def ::area ::id)
(s/def ::params (s/keys :req-un [::area ::name ::id]))

;;;; RESPONSE

(s/def ::type string?)
(s/def ::value string?)
(s/def ::json-type (s/keys :req-un [::type ::value]))
(s/def ::value (s/or :text string? :number number? :type ::json-type
                     :array (s/coll-of ::value)))
(s/def ::data (s/merge ::resource (s/map-of simple-keyword? ::value)))
(s/def ::response (s/map-of simple-keyword? ::data :count 1))
