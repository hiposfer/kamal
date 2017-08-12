(ns service.routing.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [ring.util.http-response :refer [ok]]
            [compojure.api.sweet :refer [context GET defapi]]
            [service.routing.spec :as spec]
            [service.routing.directions :as dir]
            [service.routing.graph.generators :as g]))
            ;[expound.alpha :as expound]

;(s/valid? (s/and ::radiuses-regex #(= (count coordinates) (count %)))))
;(s/and ::s/radiuses-regex #(= (count coordinates) (count %))))

;;;; TEST @mehdi
;(expound/expound-str ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))
;(s/explain ::direction (dir/direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))

(defapi app
  {:swagger {:ui "/"
             :spec "/swagger.json"
             :data {:info {:title "Routing API"
                           :description "Routing for hippos"}
                    :tags [{:name "direction", :description "direction similar to mabbox"}]}}}
  (GET "spec/direction/:coordinates" []
    :coercion :spec
    :summary "direction with clojure.spec"
    :path-params [coordinates :- ::spec/coordinate-raw]
    :query-params [{steps :- boolean? false}
                   {radiuses :- ::spec/radiuses-raw nil}
                   {alternatives :- boolean? false}
                   {language :- string? "en"}]
    :return ::spec/direction
    (ok (if-not (s/valid? (s/and #(= (count coordinates) (count %))) radiuses)
          {:message "The same amount of radiouses and coordinates must be provided"
           :code "InvalidInput"}
          (let [coords (map zipmap (repeat [:lon :lat]) coordinates)]
            (dir/direction (gen/generate (g/graph 1000))
              :coordinates coords
              :steps steps
              :radiuses radiuses
              :alternatives alternatives
              :language language))))))

