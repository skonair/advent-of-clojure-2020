(ns aoc-2020.day14
  (:require [clojure.string :as str]))

(def bit-value 
  (memoize 
    (fn[decimal pos]
      (if (zero? (bit-and decimal (long (Math/pow 2 pos))))
        0
        1))))

(defn- trinary [mask decimal]
  (BigInteger.
    (str/join
        (map-indexed 
          (fn [n bit] 
            (cond
              (= bit \0) \0
              (= bit \1) \1
              (= bit \X) (char (+ 48 (bit-value decimal (- 35 n))))))
          mask))
    2))

(defn trinary-part2 [mask decimal]
  (map-indexed 
    (fn [n bit] 
      (cond
        (= bit \0) [(bit-value decimal (- 35 n))]
        (= bit \1) [\1]
        (= bit \X) [\0 \1]))
  mask))

(defn- all-tris [mask decimal]
  (map 
    #(BigInteger. % 2) 
    (map 
      str/join 
      (map 
        flatten 
          (reduce 
            (fn [a b] (for [ai a bi b] (vector ai bi))) 
            (trinary-part2 mask decimal))))))

(defn- akk-fkt1 [akk mask ptr value]
  (assoc akk ptr (trinary mask (Integer. value))))

(defn- akk-fkt2 [akk mask ptr value]
  (into akk (map #(hash-map % (Integer. value)) (all-tris mask ptr))))

(defn- parse-line [line mask akk akk-fkt]
  (let [[res value] (str/split line #"( = )")]
    (if (= res "mask")
      [value akk]
      (let [ptr (Integer. (str/replace res #"\D" ""))
            newakk (akk-fkt akk mask ptr value)]
        [mask newakk]))))

(defn- run [lines akk-fkt]
  (loop [[line & ls] lines
         mask ""
         mem {}]
    (if (nil? line)
      (apply + (vals mem))
      (let [[newmask newmem] (parse-line line mask mem akk-fkt)]
        (recur ls newmask newmem)))))

(defn part1 [lines]
  (run lines akk-fkt1))

(defn part2 [lines]
  (run lines akk-fkt2))

(def lines (str/split-lines (slurp "resources/day14_input.txt")))

(println "Day 14 - 1: " (part1 input)) ; 10035335144067
(println "Day 14 - 2: " (part2 input)) ; 3817372618036
