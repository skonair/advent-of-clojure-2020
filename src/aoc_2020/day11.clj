(ns aoc-2020.day11
  (:require [clojure.string :as str]))

(defn occupied? [seats [sx sy] [dx dy] breakout? [mx my]]
  (loop [x sx
         y sy]
    (let [nx (+ x dx)
          ny (+ y dy)]
      (if (or (< nx 0) (> nx mx) (< ny 0) (> ny my)) 
        false ; no seat in the direction
        (let [seat (seats [nx ny])]
          (cond
            (true? seat) false
            (false? seat) true
            :otherwise (if breakout?
              false
              (recur nx ny))))))))

(defn- occupied-neighbour-count [seats [x y] breakout? maxpos]
  (apply
    +
    (for [dy (range -1 2)
          dx (range -1 2)
          :when (not (and (zero? dx) (zero? dy))) ]
      (if (true? (occupied? seats [x y] [dx dy] breakout? maxpos))
        1
        0))))

(defn- step [seats neighbours breakout? maxpos]
  (into 
    {}
    (for [[pos free?] seats]
      (let [occupied-neighbours (occupied-neighbour-count seats pos breakout? maxpos)
            new-seat-status (cond
                              (and free? (zero? occupied-neighbours)) false
                              (and (not free?) (>= occupied-neighbours neighbours)) true
                              :otherwise free?)]
        {pos new-seat-status}))))

(defn- loop-steps [seats neighbours breakout?]
  (let [maxpos [(apply max (map (fn [[[x _] _]] x) seats)) (apply max (map (fn [[[_ y] _]] y) seats))]]
    (loop [s seats]
      (let [new-seats (step s neighbours breakout? maxpos)]
        (if (= new-seats s)
          (count (filter false? (vals s)))
          (recur new-seats))))))

(defn part1 [seats]
  (loop-steps seats 4 true))

(defn part2 [seats]
  (loop-steps seats 5 false))

(def 
  seats 
  (into
    {}
    (filter #(not (nil? %))
      (flatten
        (map-indexed 
          (fn [x line] 
            (map-indexed 
              (fn [y w] 
                (cond 
                  (= w \L) {[x y] true}
                  (= w \#) {[x y] false}
                  :otherwise nil))
              line))
          (str/split-lines (slurp "resources/day11_input.txt")))))))

(def max-pos
   [(apply max (map (fn [[[x _] _]] x) seats))
    (apply max (map (fn [[[_ y] _]] y) seats))])


(println "Day 11 - 1: " (part1 input)) ; 2277
(println "Day 11 - 2: " (part2 input)) ; 2066
