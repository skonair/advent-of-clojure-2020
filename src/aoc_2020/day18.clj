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

(defn- get-number [[c & cs]]
  (cond 
    (= c \() (let [[braced rs] (get-braced-expression cs)] [(parse-next braced nil) rs])
    :otherwise [(- (int c) 48) cs]))

(defn- parse-next [[c & cs :as line] num]
  (if (nil? c)
    num 
    (cond
      (= c \() (let [[braced rs] (get-braced-expression cs)] (parse-next rs (parse-next braced nil)))
      (= c \+) (let [[n rs] (get-number cs)] (parse-next rs (+ num n)))
      (= c \*) (let [[n rs] (get-number cs)] (parse-next rs (* num n))) ; for part1
   ;   (= c \*) (* num (parse-next cs nil)) ; for part2
      :otherwise (parse-next cs (- (int c) 48)))))
 
(defn part1 [lines]
  (apply 
    +
    (map 
      #(parse-next (prepare-line %) 0)
      lines)))

(def lines (str/split-lines (slurp "resources/day18_input.txt")))

(println "Day 18 - 1: " (part1 input)) ; 510009915468
(println "Day 18 - 2: " (part2 input)) ; 321176691637769
