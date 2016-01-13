(ns astro.time)

(defn calendar [{:keys [year month day] :or {year 0 month 0 day 0}}]
  (cond
    (> year 1582) :gregorian
    (< year 1582) :julian
    (> month 10) :gregorian
    (< month 10) :julian
    (>= day 15) :gregorian
    :default :julian))

(defn cal->jd [{:keys [year month day] :or {year 0 month 0 day 0} :as date}]
  (let [[Y M] (if (> month 2) [year month] [(dec year) (+ month 12)])
        B (if (= :gregorian (calendar date))
            (let [A (int (/ Y 100))]
              (- (int (/ A 4)) A -2))
            0)]
    (+ (int (* 365.25 (+ Y 4716)))
       (int (* 30.6001 (inc M)))
       day B -1524.5)))

(defn jd->jc [jd epoch](/ (- jd epoch) 36525))

(defn jd->jc1900 [jd] (jd->jc jd 2415020.0))
(defn jd->jc2000 [jd] (jd->jc jd 2451545.0))

(defn gmst [jd]
  (let [c [67310.54841 (+ (* 876600 60 60) 8640184.812866) 0.093104 6.2E-6]]
    (/ (mod (reduce #(+ (* %1 jd) %2) (rseq c)) 86400) 240.0)))

(-> {:year 1992 :month 8 :day (+ 20.5 (/ 14 24 60.0))}
    cal->jd
    jd->jc2000
    gmst
    prn)