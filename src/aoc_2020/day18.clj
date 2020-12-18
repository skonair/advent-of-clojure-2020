(ns aoc-2020.day18
  (:require [clojure.string :as str]))

; your code here

(defn- prepare-line [line]
  (-> line 
    (str/replace #" " "")
    (str/replace #"[(]" "#")
    (str/replace #"[)]" "(")
    (str/replace #"[#]" ")")
    reverse))

;
(defn- get-braced-expression [text]
  (loop [[c & cs] text
         expression [] 
         depth 0]
    (if (and (zero? depth) (= c \)))
      [(str/join expression) cs]
      (let [newdepth (cond (= c \() (inc depth) (= c \)) (dec depth) :otherwise depth)]
        (recur cs (conj expression c) newdepth)))))



(defn- parse-next [[c & cs :as line] num]
  (if (nil? c)
    num 
    (cond
      (= c \() (let [[braced rs] (get-braced-expression cs)] (parse-next rs (parse-next braced nil)))
      (= c \)) num
      (= c \+) (+ num (parse-next cs nil))
      (= c \*) (* num (parse-next cs nil))
      :otherwise (parse-next cs (- (int c) 48)))))
 
(defn part1 [lines]
  (apply 
    +
    (map 
      #(parse-next (prepare-line %) 0)
      lines)))

(defn part2 []
  (apply 
    +
    (map 
      #(parse-next (prepare-line (str/replace % #"(\d \+ \d)" "($0)")) 0)
      lines)))


(def lines (str/split-lines (slurp "resources/day18_input.txt")))

(println "Day 18 - 1: " (part1 input))
(println "Day 18 - 2: " (part2 input))
