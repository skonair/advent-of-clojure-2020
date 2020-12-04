(ns aoc-2020.day04
  (:require [clojure.string :as str]))

(defn- valid-number? [value min max]
  (and
    (not (nil? value))
    (<= min (Integer. value) max)))

(defn- valid-pattern? [value pattern]
  (and
    (not (nil? value))
    (not (nil? (re-find pattern value)))))

(defn- valid-height? [value]
  (cond
    (nil? value) false
    (str/ends-with? value "cm") (<= 150 (Integer. (re-find #"\d*" value)) 193)
    (str/ends-with? value "in") (<= 59 (Integer. (re-find #"\d*" value)) 76)))

(defn- valid-passport2? [passport]
  (and
    (valid-number? (passport "byr") 1920 2002)
    (valid-number? (passport "iyr") 2010 2020)
    (valid-number? (passport "eyr") 2020 2030)
    (valid-height? (passport "hgt"))
    (valid-pattern? (passport "hcl") #"#[0-9a-f]{6}")
    (valid-pattern? (passport "ecl") #"(amb|blu|brn|gry|grn|hzl|oth)")
    (valid-pattern? (passport "pid") #"^[0-9]{9}$")))

(defn- valid-passport? [passport]
  (= 7 (count (select-keys passport ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]))))

(defn- extract-passport [lines]
  (loop [[l & ls] lines
         passports []
         current-passport {}]
    (if (nil? l) 
      passports
      (let [[np ncp] (if (str/blank? l) 
                       [(conj passports current-passport) {}]
                       [passports (into current-passport (apply hash-map (str/split l #"[: ]")))])]
        (recur ls np ncp)))))

(defn part1 [lines]
    (count (filter valid-passport? (extract-passport lines))))

(defn part2 [lines]
    (count (filter valid-passport2? (extract-passport lines))))

(def input (slurp "resources/day04_input.txt"))
(def lines (conj (str/split-lines input) ""))

(println "Day 04 - 1: " (part1 input)) ; 213
(println "Day 04 - 2: " (part2 input)) ; 147
