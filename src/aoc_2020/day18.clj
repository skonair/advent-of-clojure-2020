(ns aoc-2020.day18
  (:require [clojure.string :as str]))

(defn- prepare-line [line]
  (-> line 
    (str/replace #" " "")))

(defn- get-braced-expression [text]
  (loop [[c & cs] text
         expression [] 
         depth 0]
    (if (and (zero? depth) (= c \)))
      [(str/join expression) cs]
      (let [newdepth (cond (= c \() (inc depth) (= c \)) (dec depth) :otherwise depth)]
        (recur cs (conj expression c) newdepth)))))

(defn- get-number [[c & cs] part1?]
  (cond 
    (= c \() (let [[braced rs] (get-braced-expression cs)] [(parse-next braced nil part1?) rs])
    :otherwise [(- (int c) 48) cs]))

(defn- parse-next [[c & cs :as line] num part1?]
  (if (nil? c)
    num 
    (cond
      (= c \+) (let [[n rs] (get-number cs part1?)] (parse-next rs (+ num n) part1?))
      (and part1? (= c \*)) (let [[n rs] (get-number cs part1?)] (parse-next rs (* num n) part1?)) ; for part1
      (= c \*) (* num (parse-next cs nil part1?)) ; for part2
      :otherwise (let [[n rs] (get-number line part1?)] (parse-next rs n part1?)))))

(defn- parse [lines part1?]
  (apply 
    +
    (map 
      #(parse-next (prepare-line %) 0 part1?)
      lines)))
 
(defn part1 [lines]
  (parse lines true))

(defn part2 [lines]
  (parse lines false))

(def lines (str/split-lines (slurp "resources/day18_input.txt")))

(println "Day 18 - 1: " (part1 input)) ; 510009915468
(println "Day 18 - 2: " (part2 input)) ; 321176691637769
