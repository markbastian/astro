(ns astro.universal-variable
  (:require [vecmath.vec :refer [dot mag cross]]))

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

(defn kepler [r0 v0 dt mu]
  (let [vv (dot v0 v0)
        r (mag r0)
        zeta (- (* vv 0.5) (/ mu r0))
        alpha (- (/ vv mu) (/ 2.0 r))
        X0 (cond
             (< 1E-6 alpha) (* (Math/sqrt mu) dt alpha)
             (< alpha -1E-6) (let [h (cross r0 v0)
                                   p (/ (dot h h) mu)]
                               0)
             :default 3)]
    (loop [psi (* zeta zeta alpha)]
      (if true
        4
        (recur [])))))