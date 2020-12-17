(ns aoc-2020.day16
  (:require [clojure.string :as str]))

; your code here

(defn- number-in-field? [[s1 e1 s2 e2] number]
  (or 
    (<= s1 number e1)
    (<= s2 number e2)))

(defn- zero-or-number [fields number]
  (if (some #(number-in-field? % number) (vals fields))
    0
    number))

(defn- possible-fields [fields number]
  (into #{} (map first (filter #(number-in-field? (val %) number) fields))))

(defn part1 [input]
  (apply 
    +
    (flatten
      (for [nearby-ticket (nth input 2)]
        (map #(zero-or-number (first input) %) nearby-ticket)))))

(defn part22 [input]
  (let [fields (first input)
        nearby-tickets (nth input 2)]
    (apply merge-with set/intersection
    (filter 
      #(not (some empty? (vals %)))
      (map 
        #(into 
          {} 
          (map-indexed 
            (fn [idx item] {idx (possible-fields fields item)}) 
            %)) 
        nearby-tickets)))))

(defn- part2 [input]
  (loop [[i & is] (sort-by #(count (val %)) (part22 input))
         rem #{}
         akk []]
    (if (nil? i)
      akk
      (recur is (second i) (conj akk [(first i) (first (set/difference (second i) rem))])))))

(defn part2 [input n]
      (map 
        #(possible-fields (first input) %) 
        (map #(nth % n) (nth input 2))
       ))

(defn- parse-fields [line]
  (let [m (re-matcher #"(?<field>[a-zA-Z ]+): (?<s1>\d+)-(?<e1>\d+) or (?<s2>\d+)-(?<e2>\d+)" line)]
    (if (.matches m)
      {(.group m "field") [(Integer. (.group m "s1")) (Integer. (.group m "e1")) (Integer. (.group m "s2")) (Integer. (.group m "e2"))]}
      nil)))

(defn- parse-ticket [line]
  (map #(Integer. %) (str/split line #",")))

(defn- parse-line [line section fields myticket nearby-tickets]
  (cond 
    (empty? line) [(inc section) fields myticket nearby-tickets]
    (str/starts-with? line "your ticket:") [section fields myticket nearby-tickets]
    (str/starts-with? line "nearby tickets:") [section fields myticket nearby-tickets]
    (= 0 section) [section (into fields (parse-fields line)) myticket nearby-tickets]
    (= 1 section) [section fields (parse-ticket line) nearby-tickets]
    (= 2 section) [section fields myticket (conj nearby-tickets (parse-ticket line))]))

(defn- parse-lines [lines]
  (loop [[l & ls] lines
          section 0
          fields {}
          myticket nil
          nearby-tickets []]
    (if (nil? l)
      [fields myticket nearby-tickets]
      (let [[s f m n] (parse-line l section fields myticket nearby-tickets)]
        (recur ls s f m n)))))

(def input (parse-lines (str/split-lines (slurp "resources/day16_input.txt"))))

(println "Day 16 - 1: " (part1 input)) ; 25916

(apply * (map #(nth (list 179,101,223,107,127,211,191,61,199,193,181,131,89,109,197,59,227,53,103,97) %) (map first (filter #(str/starts-with? (second %) "departure") (part2 input)))))

(println "Day 16 - 2: " (part2 input)) ; 2564529489989
