(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (last-element (rest a-seq))
    ))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
    :else false
    ))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq1 a-seq]
    (cond
      (empty? seq1) nil
      (pred (first seq1)) acc
      :else (recur (inc acc) (rest seq1)) 
      )))

(defn avg [a-seq]
  (loop [acc 0
         cnt 1
         seq1 a-seq]
    (if (empty? seq1)
      (/ acc (dec cnt))
      (recur (+ acc (first seq1)) (inc cnt) (rest seq1))
    )))

(defn parity [a-seq]
  (loop [acc #{}
         seq1 a-seq]
    (cond
      (empty? seq1) acc
      (contains? acc (first seq1)) (recur (disj acc (first seq1)) (rest seq1))
      :else (recur (conj acc (first seq1)) (rest seq1))
      )))
  
(defn fast-fibo [n]
  (loop [fibn1 1
         fibn 1
         ind 3]
    (cond
      (= n 0) 0
      (< n 3) 1
      (= ind n) (+ fibn1 fibn)
      :else (recur (+ fibn1 fibn) fibn1 (inc ind))
      )))
      

(defn cut-at-repetition [a-seq]
   (reverse (loop [priors #{}
         seq1 a-seq
         acc nil]
              (cond
                (empty? seq1) acc
                (contains? priors (first seq1)) acc
                :else (recur (conj priors (first seq1)) (rest seq1) (cons (first seq1) acc))
      ))))

