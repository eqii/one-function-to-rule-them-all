(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    []
    (rest (reduce (fn [z y] (conj (conj z x) y)) [] a-seq))))

(defn my-count [a-seq]
  (if (empty? a-seq)
    0
    (reduce (fn [acc _] (inc acc)) 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [head (first a-seq)
        get-min-max (fn [min-max x]
                      (let [[c-min c-max] min-max
                            new-min (if (< x c-min) x c-min)
                            new-max (if (> x c-max) x c-max)]
                        [new-min new-max]))]
    (reduce get-min-max [head head] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [before (take-while (fn [x] (< x n)) sorted-seq)
        after (drop-while (fn [x] (< x n)) sorted-seq)]
    (concat before (cons n after))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce 
    (fn [a-set x] 
      (if (contains? a-set x)
        (disj a-set x)
        (conj a-set x))) 
    #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (reduce (fn [x _] (inc x)) 0 more))

(defn my-*
  ([] 1)
  ([x & more]
   (reduce (fn [acc y] (* acc y)) x more)))

(defn pred-and
  ([] (fn [_] true))
  ([x y] (fn [a] (and (x a) (y a))))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn- accumulate [f seqs]
  (reduce (fn [acc s] (conj acc (f s))) [] seqs))

(defn- my-map2 [acc f seqs]
  (if (some empty? seqs)
    acc
    (let [head (accumulate first seqs)
          others  (accumulate rest seqs)]
      (recur (conj acc (apply f head)) f others))))

(defn my-map [f & seqs]
  (my-map2 [] f seqs))