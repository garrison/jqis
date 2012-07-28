#!/usr/bin/env clojure

(defstruct complex-number-struct :re :im)

(def c0 (struct complex-number-struct 0.0 0.0))
(def c1 (struct complex-number-struct 1.0 0.0))
(def c-1 (struct complex-number-struct -1.0 0.0))
(def cs2 (struct complex-number-struct (Math/sqrt 0.5) 0.0))
(def c-s2 (struct complex-number-struct (- (Math/sqrt 0.5)) 0.0))

(defn c+ [x y] (struct complex-number-struct (+ (get x :re) (get y :re)) (+ (get x :im) (get y :im))))
(defn c* [x y] (struct complex-number-struct (- (* (get x :re) (get y :re)) (* (get x :im) (get y :im))) (+ (* (get x :re) (get y :im)) (* (get x :im) (get y :re)))))

(def initial-q (hash-map 0 c1))

(defn ^{:op :primitive} X [r]
  (fn [b] (hash-map (bit-flip b r) c1)))

(defn ^{:op :primitive} Z [r]
  (fn [b] (hash-map b (if (bit-test b r) c-1 c1))))

(defn ^{:op :primitive} H [r]
  (fn [b] (hash-map (bit-flip b r) cs2
                    b (if (bit-test b r) c-s2 cs2))))

(defn ^{:op :primitive} CNOT [r1 r2]
  (assert (not= r1 r2))
  (fn [b] (hash-map (if (bit-test b r1) (bit-flip b r2) b) c1)))

(defn execute-qprogram [program]
  ;; do-eval takes an instruction and a state and returns a new state by
  ;; performing the instruction on each basis state and combining like
  ;; terms
  (defn do-eval [step state] (loop [in-state (seq state) out-state {}]
                               (if (nil? in-state) out-state
                                   (recur (next in-state)
                                          (loop [ii (seq (apply step [(ffirst in-state)])) oo out-state]
                                            (if (nil? ii) oo
                                                (recur (next ii) (assoc oo (ffirst ii) (c+ (c* (second (first ii)) (second (first in-state))) (get oo (ffirst ii) c0))))))))))
  (loop [remaining-program program state initial-q]
    (if (nil? remaining-program) state
        (recur (next remaining-program) (do-eval (first remaining-program) state)))))

(def short-qprogram
  ;; creates a Bell state
  (list (H 0) (CNOT 0 1)))

(println (execute-qprogram short-qprogram))
