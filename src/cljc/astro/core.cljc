(ns astro.core
  (:require [numerics.polynomials :as p]
            [kepler.time :refer [jd->jc1900]]
            [clojure.pprint :as pp]))

;http://www.braeunig.us/space/plntpos.htm
(def keps { :eccentricity 0
           :semimajor-axis 0
           :inclination 3
           :right-ascension-of-the-ascending-node 3
           :argument-of-periapsis 3
           :mean-anomaly 3 })

(def planetary-ephemerides
  {:Earth { :L [99.69668 36000.76892 0.0003025]
           :a [1.0000002]
           :e [0.01675104 -0.0000418 -0.000000126]
           :i [0]
           :w [0]
           :W [0] }
   :Mars { :L [293.737334 19141.69551 0.0003107]
          :a [1.5236883]
          :e [0.09331290	0.000092064	-0.000000077]
          :i [1.850333	-0.0006750	0.0000126]
          :w [285.431761	1.0697667	0.0001313	0.00000414]
          :W [48.786442	0.7709917	-0.0000014	-0.00000533] }})

(def jd 2442980.0)

(defn close? [[a b]] (<= (Math/abs (- a b)) 1E-10))
(defn E [M e E] (+ M (* e (Math/sin E))))

(defn solve-E [M e]
  (let [M-rads (Math/toRadians M) step (partial E M-rads e)]
    (loop [E0 M-rads E1 (step E0)]
      (if (close? [E0 E1]) E1 (recur E1 (step E1))))))

(defn compute [jd ephemeris]
  (let [T (jd->jc1900 jd)
        x (into {} (for [[k v] ephemeris] [k (p/poly-eval T (rseq v))]))
        y (reduce #(update %1 %2 mod 360) x [:L :w :W])
        p (mod (reduce + ((juxt :w :W) y)) 360)
        M (mod (- (:L y) p) 360)]
    (into y { :p p :M M })))

(def m (compute jd (:Mars planetary-ephemerides)))

(pp/pprint m)

(def e (compute jd (:Earth planetary-ephemerides)))

(pp/pprint e)
