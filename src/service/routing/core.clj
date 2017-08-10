(ns service.routing.core
  (:require [clojure.spec.gen.alpha :as gen]
            [ring.util.http-response :refer [ok]]
            [compojure.api.sweet :refer [context GET api]]
            [service.routing.spec :as s]
            [service.routing.directions :as dir]
            [service.routing.graph.generators :as g]))
            ;[expound.alpha :as expound]

;(s/valid? (s/and ::radiuses-regex #(= (count coordinates) (count %)))))
;(s/and ::s/radiuses-regex #(= (count coordinates) (count %))))

(def routes
  (context "/spec" []
    :tags ["spec"]
    :coercion :spec

    (GET "/direction/:coordinates" []
      :summary "direction with clojure.spec"
      :path-params [coordinates :- :s/coordinate-regex]
      :query-params [{steps :- boolean? false}
                     {radiuses :- :s/radiuses-regex nil}
                     {alternatives :- boolean? false}
                     {language :- string? "en"}]
      :return ::s/direction
      (ok (let [coords (map zipmap (repeat [:lon :lat])
                                   (s/parse-coordinates coordinates))
                rads (when-not (nil? radiuses) (s/parse-radiuses radiuses))]
            (dir/direction (gen/generate (g/graph 1000))
              :coordinates coords
              :steps steps
              :radiuses rads
              :alternatives alternatives
              :language language))))))

;;;; TEST @mehdi
;(expound/expound-str ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
;(s/explain ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))

(def app
  (api
    {:swagger
     {:ui "/"
      :spec "/swagger.json"
      :data {:info {:title "Routing API"
                    :description "Routing for hippos"}
             :tags [{:name "direction", :description "direction similar to mabbox"}]}}}

    routes))
