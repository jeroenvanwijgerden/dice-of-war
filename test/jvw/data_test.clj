(ns jvw.data-test
  "In the A+D state space, there can be multiple paths from a state S to some
   other state S'. A path is a sequence of transitions, where each transition
   is denoted as [dice losses]."
  (:require [clojure.test :refer [deftest testing is]]
            [jvw.data :as sut]))
 
(def non-capital-start->end->p (sut/state->end-state->p 5 5))
(def     capital-start->end->p (sut/state->end-state->p 5 5 (sut/->state->dice 3)))

(def dice->losses->p
  {[1 1] {[0 1] 15/36      [1 0] 21/36}
   [1 2] {[0 1] 55/216     [1 0] 161/216}
   [1 3] {[0 1] 225/1296   [1 0] 1071/1296}
   [2 1] {[0 1] 125/216    [1 0] 91/216}
   [2 2] {[0 2] 295/1296   [1 1] 420/1296    [2 0] 581/1296}
   [2 3] {[0 2] 979/7776   [1 1] 1981/7776   [2 0] 4816/7776}
   [3 1] {[0 1] 855/1296   [1 0] 441/1296}
   [3 2] {[0 2] 2890/7776  [1 1] 2611/7776   [2 0] 2275/7776}
   [3 3] {[0 3] 6420/46656 [1 2] 10017/46656 [2 1] 12348/46656 [3 0] 17871/46656}})

(defn path->p
  [path]
  (reduce * (map (partial get-in dice->losses->p) path)))

(defn paths->p
  [paths]
  (reduce + (map path->p paths)))

(deftest probabilities-test
  (doseq [[data-label data state->end-state->paths]
          [["non-capital"
            non-capital-start->end->p
            {[1 1] {[1 0] [[[[1 1] [0 1]]]]}
             [2 1] {[0 1] [[[[2 1] [1 0]] [[1 1] [1 0]]]]
                    [2 0] [[[[2 1] [0 1]]]]}
             [3 2] {[1 0] [[[[3 2] [1 1]] [[2 1] [1 0]] [[1 1] [0 1]]]
                           [[[3 2] [2 0]] [[1 2] [0 1]] [[1 1] [0 1]]]]
                    [3 0] [[[[3 2] [0 2]]]]}
             [4 3] {[2 0] [[[[3 2] [0 2]] [[3 1] [1 0]] [[3 1] [1 0]] [[2 1] [0 1]]]
                           [[[3 2] [1 1]] [[3 2] [1 1]] [[2 1] [0 1]]]
                           [[[3 2] [2 0]] [[2 2] [0 2]] [[2 1] [0 1]]]]}}]
           ["capital"
            capital-start->end->p
            {[1 3] {[1 0] [[[[1 3] [0 1]] [[1 2] [0 1]] [[1 1] [0 1]]]]}
             [4 4] {[0 2] [[[[3 3] [1 2]] [[3 2] [2 0]] [[1 2] [1 0]]]
                           [[[3 3] [2 1]] [[2 3] [1 1]] [[1 2] [1 0]]]
                           [[[3 3] [3 0]] [[1 3] [0 1]] [[1 3] [0 1]] [[1 2] [1 0]]]]}
             [5 5] {[5 0] [[[[3 3] [0 3]] [[3 2] [0 2]]]]}}]]]
    (testing data-label
      (doseq [[start-state end-state->paths] state->end-state->paths]
        (testing (str "start state " start-state)
          (doseq [[end-state paths] end-state->paths]
            (testing (str "end state " end-state)
              (is (= (get-in data [start-state end-state])
                     (paths->p paths))))))))))

(deftest probabilities-sum-to-1-test
  (doseq [[label data] [["non-capital"
                         non-capital-start->end->p]
                        ["capital"
                         capital-start->end->p]]]
    (testing label
      (doseq [[start-state end-state->p] data]
        (testing (str "start state " start-state)
          (is (= 1 (reduce + (vals end-state->p)))))))))
