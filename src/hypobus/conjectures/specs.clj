(ns hypobus.conjectures.specs
  (:require [clojure.spec :as s]
            [frechet-dist.protocols :as frepos]))

(defn arc-length
  [curve]
  (reduce + (subvec (mapv frepos/distance curve (rest curve))
                    0 (dec (count curve)))))

(s/def ::dist  (s/and number? (comp not neg?)))
(s/def ::index (s/and integer? (comp not neg?)))

(s/def ::lat (s/and number? #(<= -90 % 90)))
(s/def ::lon (s/and number? #(<= -180 % 180)))
(s/def ::rad-lat (s/and number? #(<= (Math/toRadians -90) % (Math/toRadians 90))))
(s/def ::rad-lon (s/and number? #(<= (Math/toRadians -180) % (Math/toRadians 180))))
(s/def ::weight (s/and number? #(< 0 % 10)))
(s/def ::distrust (s/and number? #(and (< 0 % 1))))

(s/def ::geo-point  (s/keys :req-un [::lat ::lon]))
(s/def ::hypo-point (s/keys :req-un [::lat ::lon ::weight ::distrust]))

(s/def ::geo-curve  (s/coll-of ::geo-point  :kind sequential? :min-count 2))
(s/def ::hypo-curve (s/and (s/coll-of ::hypo-point
                                      :kind sequential?
                                      :min-count 2
                                      :distinct true)
                           #(> (arc-length %) 0)))

(s/def ::hypothesis (s/coll-of ::hypo-curve
                               :kind sequential?
                               :distinct true))
(s/def ::couple (s/coll-of (s/coll-of ::index :kind vector? :count 2)))

(s/fdef hypobus.conjectures.core/hypothize
  :args (s/cat :hypos (s/nilable ::hypothesis)
               :trace ::hypo-curve)
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.specs/arc-length
    :args ::hypo-curve
    :ret ::dist)


(s/fdef hypobus.conjectures.core/conjectures
  :args (s/cat :tr1 ::hypothesis :tr2 ::hypothesis)
  :ret ::hypothesis)

(s/fdef hypobus.conjectures.core/recombine
  :args (s/cat :hypos (s/nilable ::hypothesis))
  :ret (s/nilable ::hypothesis))

(s/fdef hypobus.basics.geometry/haversine
  :args (s/cat :lon-1 ::rad-lon :lat-1 ::rad-lat
               :lon-2 ::rad-lon :lat-2 ::rad-lat)
  :ret ::dist)
