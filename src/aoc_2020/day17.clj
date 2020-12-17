(ns aoc-2020.day17
  (:require [clojure.string :as str]))

(defn- neighbours [grid [x y z w]]
  (apply 
    +
    (for [nw (range (dec w) (+ w 2))
          nz (range (dec z) (+ z 2))
          ny (range (dec y) (+ y 2))
          nx (range (dec x) (+ x 2))
          :when (not (and (= nx x) (= ny y) (= nz z) (= nw w)))]
      (if (contains? grid [nx ny nz nw]) 
        1
        0))))

(defn- new-state [grid pos]
  (let [n (neighbours grid pos)
        active? (contains? grid pos)]
    (if active?
      (<= 2 n 3)
      (= 3 n))))

(defn- step [grid fourth-dim?]
  (let [minx (apply min (map #(nth % 0) grid)) 
        maxx (apply max (map #(nth % 0) grid))
        miny (apply min (map #(nth % 1) grid))
        maxy (apply max (map #(nth % 1) grid))
        minz (apply min (map #(nth % 2) grid))
        maxz (apply max (map #(nth % 2) grid))
        minw (apply min (map #(nth % 3) grid))
        maxw (apply max (map #(nth % 3) grid))]
    (into 
      #{}
      (filter
        some?
        (for [x (range (dec minx) (+ 2 maxx))
              y (range (dec miny) (+ 2 maxy))
              z (range (dec minz) (+ 2 maxz))
              w (if fourth-dim? (range (dec minw) (+ 2 maxw)) [0])
              :let [pos [x y z w]]]
          (if (new-state grid pos)
            pos))))))

(defn- myloop [grid fourth-dim?]
  (count
    (loop [n 6
           g grid]
      (if (zero? n)
        g
        (recur (dec n) (step g fourth-dim?))))))

(defn part1 [grid]
  (myloop grid false))

(defn part2 [grid]
  (myloop grid true))

(def 
  grid
  (into
    #{}
    (apply
      concat
      (map-indexed 
        (fn [y line] 
          (filter
            some?
            (map-indexed 
              (fn [x c] (if (= \# c) [(Integer. x) (Integer. y) 0 0])) 
              line)))
      (str/split-lines 
        (slurp "resources/day17_input.txt"))))))

(println "Day 17 - 1: " (part1 grid)) ; 313
(println "Day 17 - 2: " (part2 input)) ; 2640
