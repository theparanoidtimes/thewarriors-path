(ns thewarriors-path.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]

            ol.View
            ol.Map
            ol.Feature
            ol.Overlay
            ol.geom.Point
            ol.geom.LineString
            ol.source.OSM
            ol.source.Vector
            ol.layer.Tile
            ol.layer.Vector
            ol.style.Icon
            ol.style.Style
            ol.style.Fill
            ol.style.Stroke))

(enable-console-print!)

(defonce state (atom {:path-file-location "./edn/path.edn"
                      :map-anchor "map"
                      :view-center #js [-73.988113 40.736332]
                      :view-extent #js [-74.211960 40.565981
                                        -73.749847 40.930634]
                      :popup-elements {:e (.getElementById js/document "popup")
                                       :content (.getElementById js/document "popup-content")
                                       :closer (.getElementById js/document "popup-closer")}}))

(defn point-style
  []
  (ol.style.Style. #js {:image (ol.style.Icon. #js {:src "./img/point.png"
                                                    :size #js [30 30]})}))

(defn line-style
  [category]
  (let [color (case category
                :all "#B23C4E"
                :riverside "#03E0A0"
                :swan "#AE81FF"
                :lizzies "#778B99")]
    (ol.style.Style. #js {:stroke (ol.style.Stroke. #js {:color color
                                                         :width 4})})))

(def text-template
 "<div>
   <span style=\"font-style: italic;
                 font-weight: bold;
                 font-size: 18px;\">%t</span>
   <p style=\"\">%b</p>
  </div>")

(defn point-text
 [title body]
  (-> text-template
   (clojure.string/replace "%t" title)
   (clojure.string/replace "%b" body)))

(defn point-feature
  [point]
  (let [f (ol.Feature. #js {:name (:name point)
                            :desc (:description point)
                            :type :point})
        g (ol.geom.Point. (clj->js (:coordinates point)))
        s (point-style)]
    (doto f
      (.setGeometry g)
      (.setStyle s))))

(defn path-points
  [path]
  (->> (:points path)
      vals
      flatten
      (filter #(= :story (:type %)))
      (map point-feature)))

(defn line-feature
  [k]
  (fn [points]
    (let [sp (clj->js (-> points first))
          ep (clj->js (-> points last))
          line (ol.geom.LineString. #js [sp ep])
          s (line-style k)]
      (doto (ol.Feature.)
        (.setGeometry line)
        (.setStyle s)))))

(defn linefy
  [points]
  (remove nil?
          (let [c (count points)]
            (map-indexed (fn [i p] ; TODO - maybe reduce?
                           (let [ni (inc i)]
                             (when (< ni c)
                               [p (nth points ni)])))
                         points))))

(defn collect-coordinates
  [points]
  (map :coordinates points))

(defn lines-per-segment
  [path]
  (let [p (:points path)
        seg (keys p)]
    (map (fn [k]
           (let [points (get p k)
                 coords (collect-coordinates points)]
             (map (line-feature k) (linefy coords))))
         seg)))

(defn point-feature?
  [feature]
  (= :point (.get feature "type")))

(defn display-popup
  [p-elements overlay]
  (fn [feature]
    (when (and feature (point-feature? feature))
      (let [coord (-> feature .getGeometry .getCoordinates)
            text-title (.get feature "name")
            text-body (.get feature "desc")
            content (:content p-elements)]
        (set! (.-innerHTML content) (point-text text-title text-body))
        (.setPosition overlay coord)))))

(defn draw-popup-handler
  [map p-elements overlay]
  (doto map
    (.on "click" (fn [e]
                   (.forEachFeatureAtPixel map (.-pixel e) (display-popup p-elements overlay))))))

(defn popup-overlay-close-handler
  [closer-element overlay]
  (set! (.-onclick closer-element)
        (fn []
          (.setPosition overlay nil)
          (.blur closer-element)
          false)))

(defn popup-overlay
  [popup]
  (ol.Overlay. #js {:element popup
                    :autoPan true
                    :autoPanAnimation #js {:duration 250}
                    :position #js [0 0]}))

(defn init-map
  [map-anchor view-center view-extent path p-elements]
  (let [source (ol.source.OSM. #js {:layer "sat"})
        layer (ol.layer.Tile. #js {:source source})
        story-points (path-points path)
        story-lines (flatten (lines-per-segment path))
        vsource (ol.source.Vector. #js {:features (clj->js (into [] (concat story-points story-lines)))})
        vlayer (ol.layer.Vector. #js {:source vsource})
        view (ol.View. #js {:center view-center
                            :zoom 11
                            :extent view-extent
                            :minZoom 11
                            :maxZoom 17
                            :projection "EPSG:4326"})
        overlay (popup-overlay (:e p-elements))]
    (popup-overlay-close-handler (:closer p-elements) overlay)
    (doto (ol.Map. #js {:layers #js [layer vlayer]
                   :target map-anchor
                   :view view
                   :controls #js []})
      (draw-popup-handler p-elements overlay)
      (.addOverlay overlay))))

(set! (.-onload js/window)
      (fn []
        (go (let [r (<! (http/get (:path-file-location @state)))]
              (swap! state assoc :path (:body r))
              (init-map (:map-anchor @state)
                  (:view-center @state)
                  (:view-extent @state)
                  (:path @state)
                  (:popup-elements @state))))))
