(ns hiposfer.kamal.gtfs
  "parse the Markdown GTFS spec definition and returns it as Clojure data structures.

  Uses several heuristics to guess how to interpret the data.

  Useful to avoid monkey patching"
  (:require [clojure.string :as str]
            [markdown2clj.core :as md]
            [clojure.pprint :as pprint]))

(def url "https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md")

;; NOTE: we purposedly discard the edge case of several nested headers as
;; it makes this function much more simpler
(defn sections
  [md]
  (let [prev (volatile! (first (:document md)))]
    (partition-by (fn [v] (if (:heading v)
                            (vreset! prev v)
                            (deref prev)))
                  (:document md))))
;; (sections content)

(defn zipify
  [section]
  (let [table   (some :table-block section)
        head    (some :table-head table)
        body    (some :table-body table)
        headers (->> (tree-seq coll? seq head)
                     (filter string?)
                     (map str/lower-case)
                     (map #(str/replace % " " "-")) ;; otherwise not valid literal keyword
                     (map keyword))
        rows    (for [row (map :table-row body)]
                  (for [cell row]
                    (->> (tree-seq coll? seq cell)
                         (filter map?)
                         (filter :text)
                         (map :text)
                         (str/join ""))))]
    (map #(zipmap %1 %2) (repeat headers) rows)))
;; example
;; (feed-files content)


(defn- header [section] (-> section first :heading second :text))

(def enum-edge-cases #{"wheelchair_boarding" "direction_id"
                       "wheelchair_accessible" "timepoint"
                       "payment_method" "transfers"
                       "exact_times" "bikes_allowed"})

(defn- enum?
  [parent field]
  (and (empty? (:field-name field))
       (empty? (:required field))
       (not (empty? (:details field)))
       (or (str/ends-with? (:field-name parent) "_type")
           (contains? enum-edge-cases
                      (:field-name parent)))))

(defn- enum-value
  [text]
  (if-let [[_ value description] (re-matches #"\* (\d) - (.*)" text)]
    {:description description
     :value value}
    text))

(defn- parse-enums
  ([fields]
   (parse-enums (rest fields) [] (first fields)))
  ([fields result parent]
   (cond
     (enum? parent (first fields))
     (recur (rest fields)
            result
            (update parent :values conj (enum-value (:details (first fields)))))

     (some? (:values parent))
     (recur (rest fields)
            (conj result (update parent :values reverse))
            (first fields))

     (and (empty? fields) (nil? parent)) result

     (empty? fields) (conj result parent)

     :else
     (recur (rest fields) (conj result parent) (first fields)))))

(defn- parse
  [raw]
  (let [content    (md/parse raw)
        parts      (sections content)
        feed-files (some #(when (= "Feed Files" (header %)) %) parts)
        feed-data  (zipify feed-files)
        files      (filter #(when (str/ends-with? (header %) ".txt") %)
                           (sections content))
        files-data (for [file files]
                     {:filename (header file)
                      :fields   (->> (zipify file)
                                     (parse-enums)
                                     (remove #(and (empty? (:field-name %))
                                                   (empty? (:required %)))))})]
    {:feed-files feed-data
     :field-definitions files-data}))

(defn -main
  [f out]
  (spit out (with-out-str (pprint/pprint (parse (slurp f))))))

;(-main url)
