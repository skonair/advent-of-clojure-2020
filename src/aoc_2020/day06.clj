(ns aoc-2020.day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- count-distinct [groups]
  (-> groups
    str/join
    distinct
    count))

(defn- count-intersection [groups]
  (->> groups
    (map #(into #{} %))
    (apply set/intersection)
    count))

(defn- sum-by [groups f]
  (apply + (map f groups)))

(defn part1 [groups]
  (sum-by groups count-distinct))

(defn part2 [groups]
  (sum-by groups count-intersection))

(def lines (str/split-lines (slurp "resources/day06_input.txt")))
(def groups (filter #(not (= '("") %)) (partition-by str/blank? lines)))

(println "Day 06 - 1: " (part1 input)) ; 6625
(println "Day 06 - 2: " (part2 input)) ; 3360
