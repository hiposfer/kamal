(ns hiposfer.kamal.components.server.edn
  "Ring middleware for parsing EDN requests and generating EDN responses."
  (:require [clojure.edn :as edn])
  (:import (java.io InputStreamReader PushbackReader)))

(defn- edn-request? [request]
  (when-let [type (get-in request [:headers "content-type"])]
    (some? (re-find #"^application/(.+\+)?edn" type))))

(defn- read-edn
  [opts request]
  (when (edn-request? request)
    (when-let [body (:body request)]
      (with-open [stream (PushbackReader. (InputStreamReader. body))]
        (edn/read opts stream)))))

(defn wrap-body
  "Middleware that parses the body of EDN request maps, and replaces the :body
  key with the parsed data structure. Requests without a EDN content type are
  unaffected.

  Accepts the same options as clojure.edn/read"
  ([handler]
   (wrap-body handler {}))
  ([handler opts]
   (fn [request]
     (if-let [data (read-edn opts request)]
       (handler (assoc request :body data))
       (handler request)))))
