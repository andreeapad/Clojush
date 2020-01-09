(ns clojush.problems.test.initial
  (:require [clojure.string :as str])
  (:use [clojush.ns]
        [clojure.math.numeric-tower])
  )
(use-clojush)

(defn return-result-program
  [best population generation error-function report-simplifications]
  ; generation is just an index
  ; best is an individual
  population
  )

(def argmap
  {:error-function                 (fn [individual]
                                     (assoc individual
                                       :errors
                                       (doall
                                         (for [input (range 10)]
                                           (let [state (->> (make-push-state)
                                                            (push-item input :integer)
                                                            (push-item input :input)
                                                            (run-push (:program individual)))
                                                 top-int (top-item :integer state)]
                                             (if (number? top-int)
                                               (abs (- top-int
                                                       (- (* input input input)
                                                          (* 2 input input)
                                                          input)))
                                               1000))))))
   :atom-generators                (list (fn [] (lrand-int 10))
                                         'in1
                                         'integer_div
                                         'integer_mult
                                         'integer_add
                                         'integer_sub)
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :visualize                      true
   :problem-specific-report        return-result-program
   })

(defn -main []
  ;(def result (pushgp argmap))
  )

(def transitions (to-array-2d [[0 0.5 0 0.5]
                               [0 0 0.25 0.75]
                               [0.33 0 0.33 0.33]
                               [0 0 0.25 0.75]]))

(def transition-intervals (to-array-2d [[0 0.5 0.5 1]
                                        [0 0 0.25 1]
                                        [0.33 0.33 0.66 1]
                                        [0 0 0.25 1]]))

(def outputs (to-array-2d [[0 0.5 0 0.5]
                           [0 0 0.25 0.75]
                           [0.33 0 0.33 0.33]
                           [0 0 0.25 0.75]]))

(def output-intervals (to-array-2d [[0 0.5 0.5 1]
                                    [0 0 0.25 1]
                                    [0.33 0 0.66 1]
                                    [0 0 0.25 1]]))

(defn get-transition
  [start]
  (let [n (alength (aget transitions 0))
        r (rand)]
    (loop [end 0]
      (if (< r (aget transition-intervals start end))
        end
        (recur (inc end))
        )
      )
    )
  )

(defn get-output
  [start]
  (let [n (alength (aget outputs 0))
        r (rand)]
    (loop [end 0]
      (if (< r (aget output-intervals start end))
        (inc end)
        (recur (inc end))
        )
      )
    )
  )

(defn generate-seq
  [n start]
  (loop [i 0
         state start
         output-sequence []]
    (if (< i n)
      (let [output (get-output state)     ;get output for current state
            next (get-transition state)]       ;go to the next state
        (recur (inc i) next (conj output-sequence output)))
      output-sequence
      )
    )
  )
