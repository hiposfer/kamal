(ns hypobus.visuals.plotter)
;;   (:import  [org.jfree.chart ChartUtilities JFreeChart])
;;   (:require [clojure.data.codec.base64 :as b64]
;;             [incanter.charts :as chart]))

;; (defn- plot
;;   [^JFreeChart chart & options]
;;   (let [opts      (when options (apply assoc {} options))
;;         width     (or (:width opts) 400)
;;         height    (or (:height opts) 300)
;;         ; byte array with binary PNG data
;;         image-buf (ChartUtilities/encodeAsPNG (.createBufferedImage chart width height))]
;;      (clojure.string/join (map char (b64/encode image-buf)))))

;; (defn show-polyline
;;   ([coll]
;;   (let [image (chart/scatter-plot (map :lon coll) (map :lat coll))]
;;     (plot image)))
;;   ([coll groups]
;;   (let [image (chart/scatter-plot (map :lon coll) (map :lat coll)
;;                                   :group-by groups)]
;;     (plot image))))
