(ns aoc-2020.day23
  (:require [clojure.string :as str]))

(defn- not-in? [coll e]
  (nil? (some #{e} coll)))

(defn- pos-of [coll e]
  (first 
    (map 
      first 
      (filter 
        #(= (second %) e) 
        (map-indexed (fn [idx item] [idx item]) coll)))))

(defn- dest [pickup n max]
  (loop [d (dec n)]
    (let [newd (if (zero? d) max d)]
      (if (not-in? pickup newd)
        newd
      (recur (dec newd))))))

(defn- round [cups e maxn]
  (let [[pe ne] (cups e)
        [_ nn1] (cups ne)
        [_ nn2] (cups nn1)
        [_ nn3] (cups nn2)
        [_ nn4] (cups nn3)
        c1 (assoc cups e [pe nn3] nn3 [e nn4])
        d (dest [ne nn1 nn2] e maxn)
        [pn nn] (c1 d)
        [_ nn5] (c1 nn)
        c2 (assoc c1 d [pn ne] ne [d nn1] nn2 [nn1 nn] nn [nn2 nn5])] 
    (assoc c1 d [pn ne] ne [d nn1] nn2 [nn1 nn] nn [nn2 nn5])))
    
(defn- run [cups start steps]
  (let [maxn (apply max (keys cups))]
    (loop [cs cups
           e start
           n 0]
      (if (= n steps)
        cs
        (let [ncs (round cs e maxn)
              [_ ne] (ncs e)]
          (recur ncs ne (inc n)))))))

(defn- parse-input [line]
  (into 
    {} 
    (map 
      (fn [[p e n]] {e [p n]}) 
      (take 
        (count line) 
        (partition 3 1 (cycle (map #(- (int %) 48) line)))))))

(defn- print-cups [cups e]
  (loop [[_ n] (cups e)
         akk [e]]
    (if (= n e)
      akk
      (recur (cups n) (conj akk n)))))

(defn part1 [line]
  (str/join 
    (rest 
      (print-cups 
        (run 
          (parse-input line) 
          (- (int (first line)) 48) 
          100) 
        1))))

(defn- millionize [line n]
  (let [p1 (map #(- (int %) 48) line)
        c (count line)]
    (into 
      {} 
      (map 
        (fn [[p e n]] {e [p n]}) 
        (take 
          n 
          (partition 
            3 
            1 
            (cycle 
              (concat p1 (range (inc c) (inc n))))))))))

(defn part2 [line]
  (let [res (run (millionize line 1000000) (- (int (first line)) 48) 10000000)
        [_ n1] (res 1)
        [_ n2] (res n1)]
    (* n1 n2)))

(println "Day 23 - 1: " (part1 "284573961") ; 26354798
(println "Day 23 - 2: " (part2 "284573961")) ; 166298218695

