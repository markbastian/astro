(ns astro.quil
  (:require
    [quil.core :as q #?@(:cljs [:include-macros true])]
    [quil.middleware :as m]
    #?(:clj [clojure.pprint :refer [pprint]]
       :cljs [cljs.pprint :refer [pprint]])
    [astro.kepler :as k]
    [astro.universal-variable :as uv]))

(defn setup []
  {:world {:minx -2 :maxx 2 :miny -2 :maxy 2 }
   :r [0.177378 -0.357838 1.046140],
   :v [-0.713825 0.544356 0.307233],
   :mu 1.0
   :time (.getTime #?(:clj (java.util.Date.) :cljs (js/Date.)))})

(defn sim[state]
  (let [t (.getTime #?(:clj (java.util.Date.) :cljs (js/Date.)))
        dt (* (- t (state :time t)) 1E-3)]
    (into state { :state (uv/kepler state dt )})))

(defn draw [{:keys [world state]}]
  (let [{ :keys [minx maxx miny maxy] } world
        dx (- maxx minx) dy (- maxy miny)
        max-world-dim (max dx dy)
        w (q/width) h (q/height)
        min-screen-dim (min w h)]
    (do
      (q/background 0 0 0)
      (q/translate (* 0.5 w) (* 0.5 h))
      (q/scale 1 -1)
      (q/scale (/ min-screen-dim max-world-dim))
      (q/stroke-weight (/ max-world-dim min-screen-dim 0.5))
      (when state (q/with-translation
        [(get-in state [:r 0]) (get-in state [:r 1])]
        (q/stroke 255 0 0)
        (q/ellipse 0 0 (* 2 0.01) (* 2 0.01)))))))

(defn launch-sketch [{:keys[width height host]}]
  (q/sketch
    :title "Flocking Behaviors"
    ;#?@(:cljs [:host host])
    :setup setup
    :draw draw
    :update sim
    :middleware [m/fun-mode]
    :size [width height]))

#?(:clj (launch-sketch { :width 600 :height 600 }))