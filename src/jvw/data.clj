(ns jvw.data
  (:require [jvw.util :as util]
            [clojure.math.combinatorics :as combo]))


(defn troops [[a d] side]
  (case side :attacker a :defender d))


(defn loss
  [a-roll d-roll]
  (if (<= a-roll d-roll)
    [1 0]
    [0 1]))


(defn losses
  [a-rolls d-rolls]
  (->> [a-rolls d-rolls]
       (map (comp reverse sort))
       (apply map loss)
       (apply map +)))


(defn rolls
  [dice]
  (let [pips (range 1 (inc 6))]
    (->> dice
         (map #(apply combo/cartesian-product (repeat % pips)))
         (apply combo/cartesian-product))))


(def losses->p
  (memoize
   (fn [dice]
     (let [rolls (rolls dice)]
       (-> (map (partial apply losses) rolls)
           frequencies
           (util/update-vals / (count rolls)))))))


(defn end-state? [[a d]] (or (zero? a) (zero? d)))


(defn parent->child
  [state losses]
  (mapv - state losses))


(defn ->state->dice
  [max-d]
  (fn [[a d]]
    [(min 3 a) (min max-d d)]))


(defn- ensure-contains-state
  "Ensures `state->end-state->p` contains an entry for `state`.
   Adds entries to `state->end-state->p`, depth-first.
   It could be that `state->end-state->p` already contains an entry for a state,
   including `state`."
  [state->end-state->p state state->dice]
  (let [child+p (memoize (fn [parent]
                           (for [[losses p] (losses->p (state->dice parent))]
                             [(parent->child parent losses) p])))]
    (loop [state->end-state->p state->end-state->p
           jobs                [[:init state]]]
      (if-let [[stage state] (peek jobs)]
        (cond
          (contains?  state->end-state->p state) (recur state->end-state->p
                                                        (pop jobs))
          (end-state? state) (recur (assoc state->end-state->p
                                           state
                                           {state 1})
                                    (pop jobs))
          (= :init   stage)  (recur state->end-state->p
                                    (apply conj
                                           (pop jobs)
                                           [:finish state]
                                           (for [[child] (child+p state)]
                                             [:init child])))
          (= :finish stage)  (recur (assoc state->end-state->p
                                           state
                                           (reduce (partial merge-with +)
                                                   (for [[child p] (child+p state)]
                                                     (util/update-vals (state->end-state->p child)
                                                                       *
                                                                       p))))
                                    (pop jobs)))
        state->end-state->p))))


(defn state->end-state->p
  ([a d] (state->end-state->p a d (->state->dice 2)))
  ([a d state->dice]
   (let [states (for [a' (range (inc a))
                      d' (range (inc d))
                      :when (not= a' d' 0)]
                  [a' d'])]
     (reduce #(ensure-contains-state
               %1 %2
               state->dice)
             {}
             states))))