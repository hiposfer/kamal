(ns service.routing.spec
  (:require [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [compojure.api.sweet :refer [context GET resource]]
            [ring.util.http-response :refer [ok]]
            [spec-tools.spec :as spec]
            [service.routing.graph.generators :as g]
            [service.routing.directions :as dir]
            ;[expound.alpha :as expound]
            [clojure.string :as str]
            [clojure.edn :as edn]))
            ;[clojure.spec.test.alpha :as stest]))

(s/def ::lon (s/and spec/number? #(<= -180 % 180)))
(s/def ::lat (s/and spec/number? #(<= -90 % 90)))
(s/def ::code string?)
(s/def ::name string?)
(s/def ::coordinate-object (s/tuple ::lon ::lat))
(s/def ::location ::coordinate-object)
(s/def ::type string?)
(s/def ::coordinates (s/coll-of ::coordinate-object :kind sequential?))
(s/def ::duration (s/and spec/number? pos?))
(s/def ::distance (s/and spec/number? pos?))
(s/def ::weight spec/number?)
(s/def ::weight_name string?)
(s/def ::legs vector?)
(s/def ::waypoint-object (s/keys :req-un [::name ::location]))
(s/def ::waypoints (s/coll-of ::waypoint-object :kind sequential? :min-count 2))
(s/def ::geometry (s/keys :req-un [::type ::coordinates]))
(s/def ::route-object (s/keys :req-un [::geometry ::duration ::distance ::weight ::weight_name ::legs]))
(s/def ::routes (s/coll-of ::route-object :kind sequential?))
(s/def ::direction (s/keys :req-un [::code] :opt-un [::waypoints ::routes]))

(def coordinate-regex #"(-?\d+(\.\d+)?),(-?\d+(\.\d+)?)(;(-?\d+(\.\d+)?),(-?\d+(\.\d+)?))+")
(s/def ::coordinate-regex (s/and string? #(re-matches coordinate-regex %)))

(defn ->coordinates
  [text]
  (let [pairs (str/split text #";")]
    (for [coords pairs]
      (map edn/read-string (str/split coords #",")))))

;(->coordinates "-122.42,37.78;-77.03,38.91")

(def routes
  (context "/spec" []
    :tags ["spec"]
    :coercion :spec

    (GET "/direction/:coordinates" []
      :summary "direction with clojure.spec"
      :path-params [coordinates :- ::coordinate-regex]
      :query-params [{steps :- boolean? false}]
      :return ::direction
      (ok (let [coords (map zipmap (repeat [:lon :lat])
                                   (->coordinates coordinates))]
            (dir/direction (gen/generate (g/graph 1000))
              :coordinates coords
              :steps steps))))))

    ; (context "/data-plus" []
    ;   (resource
    ;     {:post
    ;      {:summary "data-driven plus with clojure.spec"
    ;       :parameters {:body-params (s/keys :req-un [::x ::y])}
    ;       :responses {200 {:schema ::total-map}}
    ;       :handler (fn [{{:keys [x y]} :body-params}]
    ;                  (ok {:total (+ x y)}))}}))


;;;; TEST @mehdi
;(expound/expound-str ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
;(s/explain ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
