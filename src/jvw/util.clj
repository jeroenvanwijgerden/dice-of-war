(ns jvw.util)

(defn update-vals
  "Like update but for all keys in m."
  [m f & args]
  (reduce-kv (fn [m k _v] (apply update m k f args))
             m
             m))

(defn round
  "Round number n to d decimals"
  [n d]
  (-> n
      (* (Math/pow 10 d))
      Math/round
      (/ (Math/pow 10 d))))