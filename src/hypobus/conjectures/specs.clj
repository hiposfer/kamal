(ns hypobus.conjectures.specs
  (:require [clojure.spec :as s]
            [hypobus.conjectures.core]))

(s/def ::dist  (s/and number? (comp not neg?)))
(s/def ::index (s/and integer? (comp not neg?)))

(s/def ::lat (s/and number? #(<= -90 % 90)))
(s/def ::lon (s/and number? #(<= -180 % 180)))
(s/def ::weight (s/and number? #(< 0 % 10)))
(s/def ::distrust (s/and number? #(and (< 0 % 1))))

;;(s/def ::geo-point  (s/keys :req-un [::lat ::lon]))
(s/def ::hypo-point (s/keys :req-un [::lat ::lon ::weight ::distrust]))

;;(s/def ::geo-curve  (s/coll-of ::geo-point  :kind sequential? :min-count 2))
(s/def ::hypo-curve (s/coll-of ::hypo-point
                               :kind sequential?
                               :min-count 2
                               :distinct true))

(s/def ::hypothesis (s/coll-of ::hypo-curve
                               :kind sequential?
                               :distinct true))
;; TODO: ::couple doesnt take into account the order of the indexes
(s/def ::couple (s/coll-of (s/coll-of ::index :kind vector? :count 2)))
;; TODO: only one of the indexes should be nil not both of them
; (s/def ::full-coupling (s/coll-of (s/coll-of (s/nilable ::index)
;                                              :kind sequential?
;                                              :count 2)))

(s/fdef hypobus.conjectures.core/hypothize
  :args (s/cat :hypos (s/nilable ::hypothesis)
               :trace ::hypo-curve)
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.core/conjectures
  :args (s/alt :traces (s/nilable ::hypothesis)
               :trace-sets (s/cat :trace-1 ::hypothesis
                                  :trace-2 ::hypothesis))
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.core/recombine
  :args (s/cat :hypos (s/nilable ::hypothesis))
  :ret (s/nilable ::hypothesis))

; (s/fdef hypobus.conjectures.route/full-coupling
;   :args (s/cat :P ::hypo-curve
;                :Q ::hypo-curve
;                :coupling ::couple)
;   :ret ::couple)

; (s/fdef hypobus.conjectures.route/fuse
;   :args (s/cat :P ::hypo-curve :Q ::hypo-curve :coupling ::couple)
;   :ret ::hypo-curve)

(s/fdef hypobus.basics.geometry/haversine
  :args (s/cat :lon-1 ::lon :lat-1 ::lat
               :lon-2 ::lon :lat-2 ::lat)
  :ret ::dist)
