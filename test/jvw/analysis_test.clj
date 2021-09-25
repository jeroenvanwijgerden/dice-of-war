(ns jvw.analysis-test
  (:require [clojure.test :refer [deftest testing is]]
            [jvw.data :as data]
            [jvw.util :as util]
            [jvw.analysis :as sut]))

(def non-capital-start->end->p (jvw.data/state->end-state->p 10 10))

(deftest p-win-test
  (let [Osborne-2003-table-3
        [[0.417 0.106 0.027 0.007 0.002 0.0   0.0   0.0   0.0   0.0]
         [0.754 0.363 0.206 0.091 0.049 0.021 0.011 0.005 0.003 0.001]
         [0.916 0.656 0.47  0.315 0.206 0.134 0.084 0.054 0.033 0.021]
         [0.972 0.785 0.642 0.477 0.359 0.253 0.181 0.123 0.086 0.057]
         [0.99  0.89  0.769 0.638 0.506 0.397 0.297 0.224 0.162 0.118]
         [0.997 0.934 0.857 0.745 0.638 0.521 0.423 0.329 0.258 0.193]
         [0.999 0.967 0.91  0.834 0.736 0.64  0.536 0.446 0.357 0.287]
         [1.0   0.98  0.947 0.888 0.818 0.73  0.643 0.547 0.464 0.38]
         [1.0   0.99  0.967 0.93  0.873 0.808 0.726 0.646 0.558 0.48]
         [1.0   0.994 0.981 0.954 0.916 0.861 0.8   0.724 0.65  0.568]]
        A+D+p (->> Osborne-2003-table-3
                   (map-indexed (fn [row-idx row]
                                  (map-indexed (fn [col-idx p]
                                                 [(inc row-idx)
                                                  (inc col-idx)
                                                  p])
                                               row)))
                   (apply concat)
                   set)]
    
    (testing "A+D+p is constructed correctly"
      (is (contains? A+D+p [ 5 3 0.769]))
      (is (contains? A+D+p [10 6 0.861])))
    
    (testing "p-win against Osborne's table"
      (doseq [[A D p] A+D+p]
        (testing (str "A = " A ", D = " D)
          (is (= p (util/round (sut/p-win (non-capital-start->end->p [A D])
                                          :attacker)
                               3))))))))

