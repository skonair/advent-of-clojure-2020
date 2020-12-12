(ns aoc-2020.day12
  (:require [clojure.string :as str]))

(def move-vector {\N [0 1] \S [0 -1] \E [1 0] \W [-1 0]})
(def rotate-matrix {\L [[0 1] [-1 0]] \R [[0 -1] [1 0]]}) 

(defn- mmul [[[m00 m01] [m10 m11]] [v1 v2]]
  [(+ (* m00 v1) (* m10 v2)) (+ (* m01 v1) (* m11 v2))])

(defn- move [[x y] [dx dy] steps]
  (let [nx (+ x (* dx steps))
        ny (+ y (* dy steps))]
    [nx ny]))

(defn- rotate [startpos matrix degree]
  (loop [pos startpos
         cnt (/ degree 90)]
    (if (zero? cnt)
      pos
      (recur (mmul matrix pos) (dec cnt)))))

(defn- move-part2 [shippos waypos shipdirection [action steps]]
  (cond
    (= action \F) [(move shippos waypos steps) waypos shipdirection]
    (some #{action} [\R \L]) [shippos (rotate waypos (rotate-matrix action) steps) shipdirection]
      :otherwise [shippos (move waypos (move-vector action) steps) shipdirection]))
  
(defn- move-part1 shippos waypos shipdirection [action steps]]
  (cond
    (= action \F) [(move shippos shipdirection steps) waypos shipdirection]
    (some #{action} [\R \L]) [shippos waypos (rotate shipdirection (rotate-matrix action) steps)]
    :otherwise [(move shippos (move-vector action) steps) waypos shipdirection]))

(defn run [lines move-function]
  (loop [shippos [0 0]
         waypos [10 1]
         shipdirection [1 0]
         [a  & as] lines]
    (if (nil? a)
      (apply + (map #(Math/abs %) shippos))
      (let [[action steps] a
            [newpos newwp newdirection] (move-function shippos waypos shipdirection a)]
        (recur newpos newwp newdirection as)))))

(defn part1 [lines]
  (run lines move-part1))

(defn part2 [lines]
  (run lines move-part2))

(def input 
  (map 
    (fn [[d & mv]] [d (Integer. (str/join mv))]) 
    (str/split-lines (slurp "resources/day12_input.txt"))))

(println "Day 12 - 1: " (part1 input)) ; 1645
(println "Day 12 - 2: " (part2 input)) ; 35292
