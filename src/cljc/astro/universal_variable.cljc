(ns astro.universal-variable
  (:require [vecmath.vec :refer [dot mag cross add scale]]
            [clojure.pprint :refer [pprint]]))

(defn c2c3 [psi]
  (cond
    (> psi 1E-6) (let [rt (Math/sqrt psi)]
                   {:c2 (/ (- 1.0 (Math/cos rt)) psi)
                    :c3 (/ (- rt (Math/sin rt)) (Math/sqrt (* psi psi psi)))})
    (< psi -1E-6) (let [npsi (* -1 psi)
                        rt (Math/sqrt npsi)]
                    {:c2 (/ (- 1.0 (Math/cosh rt)) psi)
                     :c3 (/ (- (Math/sinh rt) rt) (Math/sqrt (* npsi npsi npsi)))})
    :default {:c2 0.5 :c3 (/ 1.0 6.0)}))

(defn X0 [alpha r0 v0 dt mu]
  (cond
    (> alpha 1E-6)
    (* (Math/sqrt mu) dt alpha)
    (< alpha -1E-6)
    (let [h (cross r0 v0)
          p (/ (dot h h) mu)
          s (* 0.5 (Math/atan (/ 1.0 (* 3.0 dt (Math/sqrt (/ mu (* p p p)))))))
          w (Math/atan (Math/cbrt (Math/tan s)))]
      (/ (* 2.0 (Math/sqrt p)) (Math/tan (* 2.0 w))))
    :default
    (let [a (/ 1.0 alpha) sign (Math/signum ^double dt)]
      (* sign
         (Math/sqrt (- a))
         (Math/log (/ (* -2.0 mu alpha dt)
                      (+ (dot r0 v0)
                         (* sign
                            (Math/sqrt (* -1.0 mu a))
                            (- 1.0 (* (mag r0) alpha))))))))))

(defn kepler [{:keys [r v mu]} dt]
  (let [vv (dot v v)
        rv (dot r v)
        rtmu (Math/sqrt mu)
        rmag (mag r)
        alpha (- (/ 2.0 rmag) (/ vv mu))
        X (loop [X (X0 alpha r v dt mu)]
            (let [psi (* X X alpha)
                  {:keys [c2 c3]} (c2c3 psi)
                  rng (+ (* X X c2) (* (/ rv rtmu) X (- 1.0 (* psi c3))) (* rmag (- 1.0 (* psi c2))))
                  Xn (+ X (/ (- (* rtmu dt) (* X X X c3) (/ (* rv X X c2) rtmu) (* rmag X (- 1.0 (* psi c3)))) rng))]
              (if (< (Math/abs (- Xn X)) 1E-6) Xn (recur Xn))))
        psi (* X X alpha)
        {:keys [c2 c3]} (c2c3 psi)
        rng (+ (* X X c2) (* (/ rv rtmu) X (- 1.0 (* psi c3))) (* rmag (- 1.0 (* psi c2))))
        f (- 1.0 (/ (* X X c2) rmag))
        g (- dt (/ (* X X X c3) rtmu))
        g-dot (- 1.0 (/ (* X X c2) rng))
        f-dot (/ (* rtmu X (dec (* psi c3))) (* rng rmag))]
    {:r (add (scale r f) (scale v g))
     :v (add (scale r f-dot) (scale v g-dot))  }))

(pprint (kepler { :r [0.177378 -0.357838 1.046140]
                 :v  [-0.713825 0.544356 0.307233]
                 :mu 1.0}
                2.974674))