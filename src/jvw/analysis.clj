(ns jvw.analysis
  (:require [jvw.data :as data]))


(defn- wins [end-state->p side]
  (keep (fn [[state p]]
          (when (pos? (data/troops state side))
            [(data/troops state side) p]))
        end-state->p))


(defn p-win
  ([end-state->p side] (p-win end-state->p side 1))
  ([end-state->p side n]
   (->> (wins end-state->p side)
        (filter (fn [[k _]]
                  (<= n k)))
        (map second)
        (reduce +))))


(defn required-troops
  "Returns the minimal a for which the probability of the
   attacking side winning against `d` troops is at least `p`.
   As an optimization, `starting-a` indicates from which a to start checking."
  [start->end->p p d starting-a]
   (loop [a starting-a]
     (when-let [end->p (start->end->p [a d])]
       (let [p-win (->> end->p
                        (keep (fn [[[a _d] p]]
                                (when (pos? a) p)))
                        (reduce +))]
         (if (<= p p-win)
           a
           (recur (inc a)))))))


(comment
  ;; Calculates data found in csv files.
  ;; Note that for csv files max-d was 200.
  ;; !!! high max-d may take hours to compute !!!
  ;; For large max-d, manually tweak the argument
  ;; that is currently (* 5 max-d) to save much time
  ;; computing state->end-state->p.
  (let [max-d 5
        state->end-state->p (data/state->end-state->p (* 5 max-d) max-d)]
    (->> (for [p (->> [0.99 0.95 0.75 0.5 0.25 0.05 0.01])]
           (reduce (fn [p+d+a d]
                     (conj p+d+a [p d (required-troops state->end-state->p
                                                       p
                                                       d
                                                       (if-let [[_p _d a] (peek p+d+a)]
                                                         a
                                                         1))]))
                   []
                   (range 1 (inc max-d))))
         (apply concat)
         ;; add 1 to a to go from a in battle to a on territory
         (map (fn [[p d a]] [p d (inc a)]))))

  
  ;; comparison of Hendel et al. 2014, Table 1 to my results.
  (def Hendel-et-al-2014-table-1-p->d->a
    {0.2 {2  3,  3  3,  4  4,  5  4,  6  5,  7  5,  8  6,  9  7, 10  7, 11  8, 12  9, 13  9, 14 10, 15 11}
     0.3 {2  3,  3  3,  4  4,  5  5,  6  6,  7  6,  8  7,  9  8, 10  8, 11  9, 12 10, 13 11, 14 11, 15 12}
     0.4 {2  4,  3  4,  4  5,  5  5,  6  6,  7  7,  8  8,  9  9, 10 10, 11 10, 12 11, 13 12, 14 13, 15 13}
     0.5 {2  4,  3  5,  4  6,  5  6,  6  7,  7  8,  8  9,  9 10, 10 10, 11 11, 12 12, 13 13, 14 14, 15 15}
     0.6 {2  4,  3  5,  4  6,  5  7,  6  8,  7  9,  8 10,  9 11, 10 12, 11 13, 12 14, 13 15, 14 15, 15 16}
     0.7 {2  5,  3  6,  4  7,  5  8,  6  9,  7 10,  8 11,  9 12, 10 13, 11 14, 12 15, 13 16, 14 17, 15 18}
     0.8 {2  6,  3  7,  4  8,  5  9,  6 10,  7 11,  8 12,  9 13, 10 15, 11 16, 12 17, 13 18, 14 19, 15 20}})

  (let [state->end-state->p (data/state->end-state->p 20 15)]
    (->> Hendel-et-al-2014-table-1-p->d->a
         (map (fn [[p d->a]]
                [p (->> (keep (fn [[d a]]
                                (let [my-a (inc (required-troops
                                                 state->end-state->p p d 1))]
                                  (when (not= my-a a)
                                    {d {:theirs a
                                        :mine my-a}})))
                              d->a)
                        (into (sorted-map)))]))
         (into (sorted-map)))))