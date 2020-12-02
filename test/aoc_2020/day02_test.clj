(ns aoc-2020.day02-test
  (:require [clojure.test :refer :all]
            [aoc-2020.day02 :refer :all]))

(deftest aoc2020-day02
  (testing "correct-pwd? works correct"
    (is (correct-pwd? 1 3 \a "abcde"))
    (is (not (correct-pwd? 1 3 \b "cdefg")))
    (is (correct-pwd? 2 9 \c "ccccccccc")))
  (testing "match-pwd-policy? works correct"
    (is (match-pwd-policy? 1 3 \a "abcde"))
    (is (not (match-pwd-policy? 1 3 \b "cdefg")))
    (is (not (match-pwd-policy? 2 9 \c "ccccccccc")))))

