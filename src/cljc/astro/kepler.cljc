(ns astro.kepler
  (:require [vecmath.vec :refer [dot cross scale mag sub normalize x-axis y-axis z-axis]]
    [vecmath.quat :as qt]
            #?(:clj [clojure.pprint :refer [pprint]] :cljs [cljs.pprint :refer [pprint]])))

(defn Estep [M e E]
  (let [num  (+ M (* -1.0 E) (* e (Math/sin E)))
        den (- 1.0 (* e (Math/cos E)))]
    (+ E (/ num den))))

(defn Hstep [M e H]
  (let [num  (+ M H (* -1.0 e (Math/sinh H)))
        den (- (* e (Math/cosh H)) 1.0)]
    (+ H (/ num den))))

(defn guess-anomaly [^double M e]
  (if (< e 1.6)
    (if (or (< (* -1.0 Math/PI) M 0.0) (> M Math/PI))
      (- M e)
      (+ M e))
    (if (and (< e 3.6) (> (Math/abs M) Math/PI))
      (- M (* (Math/signum M) e))
      (/ M (- e 1.0)))))

(defmulti solve-kep #(let [e (:e %)] (cond (< e 1) :E (> e 1.0) :H :default :B)))

(defmethod solve-kep :E [{:keys[M e]}]
  {:E (some (fn [[E0 E1]] (when (< (Math/abs (- E0 E1)) 1E-14) E1))
            (partition 2 1 (iterate (partial Estep M e) (guess-anomaly M e))))})

(defmethod solve-kep :H [{:keys[M e]}]
  {:H (some (fn [[H0 H1]] (when (< (Math/abs (- H0 H1)) 1E-14) H1))
            (partition 2 1 (iterate (partial Hstep M e) (guess-anomaly M e))))})

(defmethod solve-kep :B [{:keys[mu dt p]}]
  (let [np (* 2.0 (Math/sqrt (/ mu (* p p p))))
        w (Math/atan (Math/cbrt (Math/tan (* 0.5 (Math/atan (/ 1.0 (* 1.5 np dt)))))))]
    {:B (/ 2.0 (Math/tan (* 2.0 w)))}))

(defn solve-kepE [M e]
  {:E (some (fn [[E0 E1]] (when (< (Math/abs (- E0 E1)) 1E-14) E1))
            (partition 2 1 (iterate (partial Estep M e) (guess-anomaly M e))))})

(defn solve-kepH [M e]
  {:H (some (fn [[H0 H1]] (when (< (Math/abs (- H0 H1)) 1E-14) H1))
            (partition 2 1 (iterate (partial Hstep M e) (guess-anomaly M e))))})

(defn solve-kepB [mu dt p]
  (let [np (* 2.0 (Math/sqrt (/ mu (* p p p))))
        w (Math/atan (Math/cbrt (Math/tan (* 0.5 (Math/atan (/ 1.0 (* 1.5 np dt)))))))]
    {:B (/ 2.0 (Math/tan (* 2.0 w)))}))

(defn atanh [x] (* 0.5 (Math/log (/ (+ 1.0 x) (- 1.0 x)))))

(defn nu->anomaly [e nu]
  (cond
    (< e 1.0)
    { :E (Math/atan2 (* (Math/sin nu) (Math/sqrt (- 1.0 (* e e)))) (+ e (Math/cos nu))) }
    (> e 1.0)
    { :H (atanh (/ (* (Math/sin nu) (Math/sqrt (- (* e e) 1.0))) (+ e (Math/cos nu)))) }
    :default
    { :P (Math/atan (* 0.5 nu))}))

(defn anomaly->nu [e anomaly]
  (cond
    (< e 1.0)
    (let [E (:E anomaly)]
      (Math/atan2 (* (Math/sin E) (Math/sqrt (- 1.0 (* e e)))) (- (Math/cos E) e)) )
    (> e 1.0)
    (let [H (:H anomaly)]
      (Math/atan2 (* -1.0 (Math/sinh H) (Math/sqrt (- (* e e) 1.0))) (- (Math/cosh H) e)))
    :default
    (let [{:keys [P p r]} anomaly]
      (Math/atan2 (* p P) (- p r)))))

(defn n [{:keys [mu a]}] (Math/sqrt (/ mu a a a)))

(defn quadrant-check [c t] (if (neg? c) (- (* 2.0 Math/PI) t) t))

(defn rv->coe [{:keys[r v mu]}]
  (let [vv (dot v v)
        rv (dot r v)
        rmag (mag r)
        mur (/ mu rmag)
        e (sub (scale r (/ (- vv mur) mu)) (scale v (/ rv mu)))
        h (cross r v)
        n (cross z-axis h)
        emag (mag e) hmag (mag h) nmag (mag n)
        [a p] (if (zero? emag)
                [(/ (* hmag hmag) mu) Double/POSITIVE_INFINITY]
                (let [a (/ mu -2.0 (- (* vv 0.5) mur))]
                  [a (* a (- 1.0 (* emag emag)))]))
        i (Math/acos (/ (dot h z-axis) hmag))
        OMEGA (quadrant-check (dot n y-axis) (Math/acos (/ (dot n x-axis) nmag)))
        omega (quadrant-check (dot e z-axis) (Math/acos (/ (dot n e) (* nmag emag))))
        nu (quadrant-check rv (Math/acos (/ (dot e r) (* emag rmag))))
        omega_true (quadrant-check (dot e y-axis) (Math/acos (/ (dot e x-axis) emag)))
        u (quadrant-check (dot r z-axis) (Math/acos (/ (dot n r) (* nmag rmag))))
        lambda_true (quadrant-check (dot r y-axis) (Math/acos (/ (dot r x-axis) rmag)))]
    (into { :p p :a a :e emag :i i :mu mu }
          (cond
            (and (zero? i) (zero? emag))
            {:OMEGA 0.0 :omega 0.0 :nu lambda_true :lambda_true lambda_true }
            (zero? emag) { :OMEGA OMEGA :omega 0.0 :nu u :u u }
            (zero? i) { :OMEGA 0.0 :omega omega_true :nu nu }
            :default { :OMEGA OMEGA :omega omega :nu nu }))))

(pprint (rv->coe {:r [1.023 1.076 1.011] :v [0.62 0.7 -0.25] :mu 1.0 }))

(defn coe->rv [{:keys [p e i OMEGA omega nu mu]}]
  (let [c (Math/cos nu)
        s (Math/sin nu)
        den (inc (* e c))
        r-pqw [(/ (* p c) den) (/ (* p s) den) 0]
        mp (Math/sqrt (/ mu p))
        v-pqw [(* -1.0 mp s) (* mp (+ e c)) 0]
        r (qt/mul (qt/rotz OMEGA) (qt/rotx i) (qt/rotz omega))]
    {:r (qt/xform r r-pqw) :v (qt/xform r v-pqw) :mu mu}))

(defn coe [rv dt]
  (let [{:keys [e nu mu p] :as keps} (rv->coe rv)
        anomaly (if (zero? e)
                  {:E (or (:lambda_true keps) (:u keps))}
                  (nu->anomaly e nu))
        fraz (solve-kep
               (into keps
                     (cond
                       (< e 1.0) {:M (let [E0 (:E anomaly)]
                                       (+ (- E0 (* e (Math/sin E0))) (* (n keps) dt))) }
                       (> e 1.0) {:H (let [H0 (:H anomaly)]
                                       (+ (- (* e (Math/sinh H0)) H0) (* (n keps) dt))) }
                       :default { :dt dt })))
        new-anomaly (cond
            (< e 1.0) (let [E0 (:E anomaly)]
                        (solve-kepE (+ (- E0 (* e (Math/sin E0))) (* (n keps) dt)) e))
            (> e 1.0) (let [H0 (:H anomaly)]
                        (solve-kepH (+ (- (* e (Math/sinh H0)) H0) (* (n keps) dt)) e))
            :default (solve-kepB mu dt p))
        nu (if (zero? e)
             { :u (:E new-anomaly) :lambda_true (:E new-anomaly)}
             {:nu (anomaly->nu e fraz)})]
    (coe->rv (into keps nu))))

(pprint (coe {:r [1.0230836995735855 1.0757893262467102 1.0111288755563401],
      :v [0.6201204177935656 0.699922029799119 -0.24992014137296167],
      :mu 1.0} 10.0))

(pprint (coe->rv {:p 1.73527 :e 0.83285 :i (Math/toRadians 87.87)
                  :OMEGA (Math/toRadians 227.89) :omega (Math/toRadians 53.38)
                  :nu (Math/toRadians 92.335) :mu 1.0 }))
