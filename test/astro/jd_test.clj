(ns astro.jd-test
  (:require [clojure.test :refer :all]
            [astro.time :refer [cal->jd]]))

(deftest
  example-7.a
  (testing "Meeus Example 7.a"
    (is (= (cal->jd {:year 1957 :month 10 :day 4.81}) 2436116.31))))

(deftest
  example-7.b
  (testing "Meeus Example 7.b"
    (is (= (cal->jd {:year 333 :month 1 :day 27.5}) 1842713.0))))

(deftest
  example-1
  (testing "1.5 Jan 2000"
    (is (= (cal->jd {:year 2000 :month 1 :day 1.5}) 2451545.0))))

(deftest
  example-2
  (testing "27 Jan 1987"
    (is (= (cal->jd {:year 1987 :month 1 :day 27}) 2446822.5))))

(deftest
  example-3
  (testing "19.5 Jun 1987"
    (is (= (cal->jd {:year 1987 :month 6 :day 19.5}) 2446966.0))))

(deftest
  example-4
  (testing "27 Jan 1988"
    (is (= (cal->jd {:year 1988 :month 1 :day 27}) 2447187.5))))

(deftest
  example-5
  (testing "19.5 Jun 1988"
    (is (= (cal->jd {:year 1988 :month 6 :day 19.5}) 2447332.0))))

(deftest
  example-6
  (testing "1 Jan 1900"
    (is (= (cal->jd {:year 1900 :month 1 :day 1}) 2415020.5))))

(deftest
  example-7
  (testing "1 Jan 1600"
    (is (= (cal->jd {:year 1600 :month 1 :day 1}) 2305447.5))))

(deftest
  example-8
  (testing "31 Dec 1600"
    (is (= (cal->jd {:year 1600 :month 12 :day 31}) 2305812.5))))

(deftest
  example-9
  (testing "10.3 Apr 837"
    (is (= (cal->jd {:year 837 :month 4 :day 10.3}) 2026871.8))))

(deftest
  example-10
  (testing "12.5 Jul -1000"
    (is (= (cal->jd {:year -1000 :month 7 :day 12.5}) 1356001.0))))

(deftest
  example-11
  (testing "29 Feb -1000"
    (is (= (cal->jd {:year -1000 :month 2 :day 29}) 1355866.5))))

(deftest
  example-11
  (testing "17.9 Aug -1001"
    (is (= (cal->jd {:year -1001 :month 8 :day 17.9}) 1355671.4))))

(deftest
  example-13
  (testing "1.5 Jan -4712"
    (is (= (cal->jd {:year -4712 :month 1 :day 1.5}) 0.0))))