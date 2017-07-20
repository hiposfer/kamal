(ns service.routing.core
  (:require [compojure.api.sweet :refer :all]
            [service.routing.spec]))

(def app
  (api
    {:swagger
     {:ui "/"
      :spec "/swagger.json"
      :data {:info {:title "Routing API"
                    :description "Routing for hippos"}
             :tags [{:name "direction", :description "direction similar to mabbox"}]}}}

    service.routing.spec/routes))

;;(println (direction (gen/generate (g/graph 1000)) :coordinates [{:lon 1 :lat 2} {:lon 3 :lat 4}]))