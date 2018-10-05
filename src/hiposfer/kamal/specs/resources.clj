(ns hiposfer.kamal.specs.resources
  (:require [clojure.spec.alpha :as s]
            [hiposfer.kamal.services.routing.core :as routing]
            [hiposfer.kamal.io.gtfs :as gtfs]))

(defn- no-pull-inside?
  [query]
  (let [ss (set (filter symbol? (tree-seq coll? seq query)))]
    (not (contains? ss 'pull))))

;; ......................................................

;; according to json api both type and id MUST be strings
(s/def ::id (s/or :text (s/and string? not-empty)
                  :number number?))

(s/def ::name (set (for [[k v] routing/schema
                         :when (contains? v :db.unique)]
                     (namespace k))))

(s/def ::reference (s/map-of keyword? ::id :count 1)) ;; only the ref itself, no extra info

;;;; REQUEST

(s/def ::area ::id)
(s/def ::params (s/keys :req-un [::area ::name ::id]))

(s/def ::q (s/and coll? not-empty no-pull-inside?))

(s/def ::args (s/and coll? not-empty))
(s/def ::query (s/keys :req-un [::area ::q]
                       :opt-un [::args]))

(s/def ::gtfs-entry (s/map-of gtfs/attributes some?))
(s/def ::transaction (s/coll-of ::gtfs-entry :kind vector?))
