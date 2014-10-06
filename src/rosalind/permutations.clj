(ns rosalind.permutations)

(defn String->Digits  [s]
  (->>
    (seq s)
    (map (fn [x] (Integer/valueOf (Character/toString x))))
    (vec)))

(defn swapDigits [v pos1 pos2]  (assoc v pos2 (v pos1) pos1 (v pos2)))

(defn Digits->String [v]
  (reduce #(str %1 %2) v))

(defn findFirstLessThan [v pos]
  (let [cur pos]
    (loop [cur cur]
      (if (< cur 0) nil (if (< (v cur) (v pos)) cur (recur (dec cur)))))))

(def not-nil? (complement nil?))

; Where should we start "promoting" from?
; And who is the one we shold be replacing with?
(defn findStartingPos [v]
  (let [cur (dec (count v))]
    (loop [cur cur
           maxPos -1
           acc []]
      (if (or (< cur maxPos) (< cur 0))
        (if (= maxPos -1 ) nil acc)
        (if-let [pos (findFirstLessThan v cur)]
          (let [newMaxPos (max pos maxPos)
                 newAcc (if (not= maxPos newMaxPos) [cur pos] acc)]
            (recur (dec cur) newMaxPos (if (not= maxPos newMaxPos) [cur pos] acc)))
            (recur (dec cur) maxPos acc))))))

(defn sort-remainder [v pos1]
  (if (= (dec (count v)) pos1) v (into (subvec v 0 pos1) (sort (subvec v pos1)))))

(defn permute [v]
  (when-let [[pos2 pos1] (findStartingPos v)]
    (let [nxt (sort-remainder (swapDigits v pos2 pos1) (inc pos1))]
      (cons nxt (lazy-seq (permute nxt))))))
