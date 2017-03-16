(ns looping-is-recursion)

(defn power [base exp]
 (let [helper (fn [acc n]
               (if (zero? n)
                acc
                (recur (* acc base) (dec n))))]
  (helper 1 exp)))

(defn last-element [a-seq]
 (cond
  (empty? a-seq) nil
  (empty? (rest a-seq)) (first a-seq)
  :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
 (cond
  (and (empty? seq1) (empty? seq2)) true
  (not= (count seq1) (count seq2)) false
  (not= (first seq1) (first seq2)) false
  :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
 (loop [index-count 0
        sequence a-seq]
  (cond
   (empty? sequence) nil
   (pred (first sequence)) index-count
   :else (recur (inc index-count) (rest sequence)))))

(defn avg [a-seq]
 (loop [total 0
        n-of-elements 0
        sequence a-seq]
  (if (empty? sequence)
   (/ total n-of-elements)
   (recur (+ total (first sequence)) (inc n-of-elements) (rest sequence)))))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)))

(defn parity [a-seq]
 (loop [set-of-odds #{}
        sequence a-seq]
  (if (empty? sequence)
   set-of-odds
   (recur (toggle set-of-odds (first sequence)) (rest sequence)))))

(defn fast-fibo [n]
 (loop [counter 0
        previous 0
        current 1]
  (if (= counter n)
   previous
   (recur (inc counter) current (+ previous current)))))

(defn cut-at-repetition [a-seq]
 (loop [cut-vector []
        current-seq a-seq]
  (cond
   (empty? current-seq) cut-vector
   (= (first current-seq) (first cut-vector)) cut-vector
   :else (recur (conj cut-vector (first current-seq)) (rest current-seq)))))
