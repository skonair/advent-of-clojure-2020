(ns aoc-2020.day20
  (:require [clojure.string :as str]))

; your code here

(defn- rev [list]
  (map #(- 9 %) list)) 

(defn- borders [tile]
  (into 
    #{}
    (map
      sort
      (let [left (map second (filter (fn [[x _]] (= x 0)) tile))
            right (map second (filter (fn [[x _]] (= x 9)) tile)) 
            top (map first (filter (fn [[_ y]] (= y 0)) tile))
            bottom (map first (filter (fn [[_ y]] (= y 9)) tile))]
     [left top right bottom (rev left) (rev top) (rev right) (rev bottom)]))))

(defn- common-borders [tiles tile1 tile2]
   (set/intersection (borders (tiles tile1)) (borders (tiles tile2))))

(defn- border-idx [tiles tile border]
  (let [v (into [] (borders (tiles tile)))]
    (.indexOf v border)))

(defn- relative-border [tiles tile border idx]
  (let [v (into [] (borders (tiles tile)))
        i  (.indexOf v border)
        d (mod (+ i idx) 4)]
    (if (< i 4)
      (nth v d)
      (nth v (+ 4 d)))))


(defn- opposite-border [tiles tile1 tile2]
  (let [cb (common-borders tiles tile1 tile2)]
    (if (empty? cb)
      nil
      (let [v (into [] (borders (tiles tile2)))]
        (nth v (mod (+ 2 (.indexOf v (first cb))) 4))))))

(defn- tile1-bottom-border [tiles tile1 tile2]
  (let [cb (common-borders tiles tile1 tile2)]
    (if (empty? cb)
      nil
      (let [v (into [] (borders (tiles tile1)))]
        (nth v (mod (+ 3 (.indexOf v (first cb))) 4))))))
      
(defn- tiles-with-border [tiles border]
  (filter #(contains? (borders (val %)) border) tiles))

(defn- count-common-borders [tiles tile1 tile2]
   (/ (count (common-borders tiles tile1 tile2)) 2))

(defn- neighbours [tiles tile]
  (for [t (keys tiles)
        :let [c (count-common-borders tiles t tile)]
        :when (and (not= tile t) (> c 0))]
    t))

(defn- count-all [tiles]
  (let [ks (keys tiles)]
    (apply
      concat
      (for [t1 ks 
            t2 ks 
            :let [c (count-common-borders tiles t1 t2)] 
            :when (and (< t1 t2) (> c 0))] 
        [t1 t2]))))

(defn- corner-tiles [tiles]
  (map 
    first 
    (filter 
      #(= 2 (val %)) 
      (frequencies 
        (count-all tiles)))))

(defn- build-first-row [tiles corner-tile]
  (let [tile2 (first (neighbours tiles corner-tile))]
    (loop [row [corner-tile tile2]]
      (let [previous-tile (last (drop-last row))
            current-tile (last row)
            ob (opposite-border tiles previous-tile current-tile)
            twb (tiles-with-border tiles ob)
            next-tile (first (filter #(not= % current-tile) (keys twb)))]
        (println "next=tile is " next-tile)
        (if (nil? next-tile)
          row
          (recur (conj row next-tile)))))))

(defn- build-next-row [tiles [t00 t10 & _]]
  (let [t0r (first (filter #(not= % t10) (neighbours tiles t00)))
        t00-left-border (opposite-border tiles t10 too)
        t00-bottom-border (


(defn part1 [tiles]
  (apply 
    * 
    (corner-tiles tiles)))

(defn- rot-right [tile]
  (map (fn [[x y]] [y (- 9 x)]) tile))

(defn- flip-x [tile]
  (map (fn [[x y]] [(- 9 x) y]) tile))

(defn- match-tile 

(defn part1 []
)

(defn part2 []
)

(defn- parse-header [line]
  (Integer. (last (str/split line #"[: ]"))))

(defn- parse-line [line y]
  (filter 
    not-empty 
    (map-indexed 
      (fn [x item] (if (= item \#) [x y])) 
      line)))

(defn- parse-lines [lines]
  (loop [[line & ls] (conj lines "")
         tile-number nil
         y 0
         tile #{}
         tiles {}]
    (if (nil? line)
      tiles
      (cond
        (str/starts-with? line "Tile") (recur ls (parse-header line) y tile tiles)
        (empty? line) (recur ls nil 0 #{} (assoc tiles tile-number tile))
        :otherwise (recur ls tile-number (inc y) (into tile (parse-line line y)) tiles)))))


(def lines (str/split-lines (slurp "resources/day20_input.txt")))

(println "Day 20 - 1: " (part1 input))
(println "Day 20 - 2: " (part2 input))
