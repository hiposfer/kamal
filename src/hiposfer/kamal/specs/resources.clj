(ns hiposfer.kamal.specs.resources
  (:require [clojure.spec.alpha :as s]
            [hiposfer.kamal.services.routing.core :as routing]))

;; according to json api both type and id MUST be strings
(s/def ::id (s/or :text (s/and string? not-empty)
                  :number number?))

(s/def ::name (set (for [[k v] routing/schema
                         :when (contains? v :db.unique)]
                     (namespace k))))

(s/def ::resource (s/map-of keyword? ::id :count 1)) ;; only the ref itself, no extra info

;;;; REQUEST

(s/def ::area ::id)
(s/def ::params (s/keys :req-un [::area ::name ::id]))
