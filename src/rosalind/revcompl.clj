(ns rosalind.revcompl
  (:require [clojure.string :as s]))

(def compliments (hash-map "A" "T" "T" "A" "G" "C" "C" "G"))

(defn compl [s]
  (compliments s))

;;Find reverse complement of a DNA string
(defn reverseComplement [s]
  (->>
    s
    .toUpperCase
    s/reverse
    seq
    (map str)
    (map compl)
    s/join)
  )
