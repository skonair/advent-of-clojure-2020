(ns aoc-2020.day19
  (:require [clojure.string :as str]))

; your code here
(defn- parse-line [line]
  (let [[n & rs] (str/split (str/replace line #"\"" "") #"[:|]")
        rules (map #(str/split (str/trim (str/join %)) #"\s") rs) ]
    {(Integer. n) (map #(map (fn [a] (Integer. a)) %) rules)}))

(defn- a-rule? [rn] (= rn 106))
(defn- b-rule? [rn] (= rn 65))

(defn- strip-rule [[s & rs :as st] rules rn]
  (if (nil? s)
    []
    (cond
      (and (a-rule? rn) (= s \a)) [rs]
      (and (b-rule? rn) (= s \b)) [rs]
      :otherwise (apply concat (for [rule (rules rn)]
                                 (strip-rule-seq st rules rule))))))

(defn- strip-rule-seq [s m [i & is]]
  (if (nil? i)
    [s]
    (apply concat
    (for [stripped (strip-rule s m i)]
      (strip-rule-seq stripped m is)))))

(defn- matches-rule [m i s]
  (let [tails (strip-rule s m i)]
    [(> (count (filter nil? tails)) 0) tails]))

(defn part1 [rs ws]
  (count 
    (filter 
      #(true? %) 
      (map 
        first 
        (map 
          #(matches-rule rs 0 %) 
          ws)))))

(def lines (str/split-lines (slurp "resources/day19_input.txt")))
(def rules1 (into {} (map parse-line (filter #(nil? (str/index-of % \")) lines))))
(def words (str/split-lines (slurp "resources/day19_words.txt")))

(def rules12 (assoc rules1 8 [[42] [42 8]] 11 [[42 31] [42 11 31]]))

(println "Day 19 - 1: " (part1 rules1 words)) ; 205
(println "Day 19 - 2: " (part1 rules12 words)) ; 329
