(ns rosalind.core
  (:gen-class)
  (:require [rosalind.permutations :as perm])
  )

(defn -main
  [name input n]
  (println (map perm/Digits->String (take (Integer. n) (perm/permute (perm/String->Digits input)))))
   )
