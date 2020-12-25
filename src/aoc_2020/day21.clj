(ns aoc-2020.day21
  (:require [clojure.string :as str]))

; your code here

(defn- ingredients-with-allergen [lines allergen]
  (apply 
    set/intersection 
    (map 
      first 
      (filter 
        #(contains? (second %) allergen) 
        lines))))

(defn- all-allergens [lines]
  (apply set/union (map second lines)))

(defn- all-ingredients [lines]
  (apply set/union (map first lines)))

(defn- one? [coll]
  (= 1 (count coll)))

(defn- remove-from-lines [lines ing allergen]
  (map 
    #(list 
       (into #{} (remove #{ing} (first %)))
       (into #{} (remove #{allergen} (second %))))
    lines))

(defn- run-round [lines unmapped-allergens]
  (loop [[a & as] unmapped-allergens
         unmapped #{}
         mapped {}
         newlist lines]
    (if (nil? a)
      [mapped unmapped newlist]
      (let [ing (ingredients-with-allergen newlist a)]
        (if (one? ing) 
          (recur as unmapped (assoc mapped (first ing) a) (remove-from-lines newlist (first ing) a))
          (recur as (conj unmapped a) mapped newlist))))))

(defn- run [lines]
  (loop [as (all-allergens lines)
         mapped {}
         nl lines]
    (if (empty? as)
      mapped
      (let [[nm nu nnl] (run-round nl as)]
        (recur nu (into mapped nm) nnl)))))

(defn part1 [lines]
  (let [mapped (run lines)
        ingredients (set/difference (all-ingredients lines) (into #{} (keys mapped)))]
  (apply 
    +
    (for [ing ingredients]
      (count (filter #(contains? (first %) ing) lines))))))

(defn part2 [lines]
  (str/join "," (map first (sort-by second (run lines)))))

(defn- parse-line [line]
  (map 
    #(into #{} %)
    (map 
      #(str/split (str/trim (str/replace % #"," "")) #"[ ]") 
      (str/split line #"(\(contains)|(\))"))))

(def lines (map parse-line (str/split-lines (slurp "resources/day21_input.txt"))))

(println "Day 21 - 1: " (part1 lines)) ; 2627
(println "Day 21 - 2: " (part2 lines)) ; "hn,dgsdtj,kpksf,sjcvsr,bstzgn,kmmqmv,vkdxfj,bsfqgb"
