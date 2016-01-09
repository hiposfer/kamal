(ns sagacious-woof.broker)

; TODO: compute a real weight function
(defn compute-weight
  "using the trust of each point of curves A and B and taking into account
  the coupling sequence, computes the weight of each point in the coupling"
  [tA tB coupling]
  (for [[i j] coupling]
    (cond
     (nil? i) [0 (tB j)]
     (nil? j) [(tA i) 0]
     :else [(tA i) (tB j)])))

; TODO: compute a real trust
(defn find-trust
  "get the trust of a curve in their points"
  [points]
  (map #(vector 1 %) points))
