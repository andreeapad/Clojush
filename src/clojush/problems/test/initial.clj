(ns clojush.problems.test.initial
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math])
  (:use [clojush.ns])
  )

(use-clojush)

(def ^:const sequence-length 6)
(def ^:const start-state 0)

(defn sigmoid
  [x]
  (float (/ 1 (+ 1 (Math/exp (- x)))))
  )

(defn pushgp-result
  [best population generation error-function report-simplifications]
  [best (mapv #(agent % :error-handler agent-error-handler)
    population)]
  )

(defn generator-simple-pattern-error
  [individual]
  (assoc individual
    :errors
    (doall
      (for [length [6]]
        (let [output [1 3 1 3 1 3]
              state (->> (make-push-state)
                         (push-item length :integer)
                         (push-item length :input)
                         (run-push (:program individual)))
              result-sequence-vector (top-item :vector_integer state)]
          (if (vector? result-sequence-vector)
            (+ (* 100 (math/abs (- (count result-sequence-vector) length)))
              (reduce + (map (fn [x y] (* (- x y) (- x y))) result-sequence-vector output))
              )
            10000)))))
  )

(defn generator-error
  "Penalizes anything that doesn't produce a vector of n numbers.
  The error is the abs value of the difference between n and the size of the result"
  [individual]
  (assoc individual
    :errors
    (doall
      (for [input (range 1 20)]
        (let [state (->> (make-push-state)
                         (push-item input :integer)
                         (push-item input :input)
                         (run-push (:program individual)))
              result-sequence-vector (top-item :vector_integer state)]
          (if (vector? result-sequence-vector)
            (math/abs (- (count result-sequence-vector) input))
            1000)))))
  )

;---------------------------------
;([] exec_dup_times (exec_stackdepth vector_integer_conj))
;Example of how to run a push program, give a state, and print the result state
;(let [state (->> (make-push-state)
;                 (push-item 10 :integer)
;                 (push-item 10 :input)
;                 (run-push '([] exec_dup_times (exec_stackdepth vector_integer_conj)))
;                 )]
;  (state-pretty-print state))

(def generator-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (- (lrand-int 201) 100)) ;Integer ERC [-1000,1000]
            ;;; end ERCs
            (tag-instruction-erc [:integer :vector_integer :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :vector_integer :exec])))

(def generator-argmap
  {:error-function                 generator-simple-pattern-error
   :atom-generators                generator-atom-generators
   :error-threshold                0
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :return-simplified-on-failure   true
   :max-points                     100
   :max-generations                500
   :visualize                      false
   :print-csv-logs                 true
   :csv-log-filename              "generator_log1.csv"
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn generator []
  (def pop-agents (pushgp generator-argmap))

  ;testing generator result
  (def result (nth pop-agents 0))
  (def program (:program result))
  (let [input 6
        state (->> (make-push-state)
                   (push-item input :integer)
                   (push-item input :input)
                   (run-push program)
                   )
        ]
    (state-pretty-print state)
    (def state state))
  )


(def sample-argmap
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
                                               (math/abs (- top-int
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
   :visualize                      false
   :problem-specific-report        pushgp-result
   })

;(defn test-give-population-as-input []
;  (def pop-agents (pushgp sample-argmap))
;  (def new-pop (pushgp-custom sample-argmap pop-agents))
;  )
;
;
;(defn test-create-population []
;   (load-push-argmap sample-argmap)
;   (def pop-agents (make-pop-agents @push-argmap))
;   (def child-agents (make-child-agents @push-argmap))
;   (nth child-agents 0)
;  )

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
  "Return the next state after randomly choosing a transition from 'start' state."
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
  "Returns the output corresponding to 'start' state. Outcomes take values starting from 1."
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
  "Generates an output sequence of length n starting from 'start' state >= 0. States are indexed from 0."
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

(defn generate-random-seq
  "Generates a random sequence of length n"
  [n]
  (loop [i 0
         output-sequence []]
    (if (< i n)
      (let [ r (inc (rand-int 9))]
        (recur (inc i) (conj output-sequence r)))
      output-sequence
      )
    )
)

; have n real examples and n fake ones
;     ---- the fake are random digits from the output possibilites?
;      then it will be output from the best generator in the population
;
;     ---- the real will be generated using my code
;real (repeatedly 20 #(generate-seq sequence-length 0))
;fake (repeatedly 20 #(generate-random-seq sequence-length)

(defn discriminator-result
  [input program]
  (let [state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push program))
        result-float (top-item :float state)]
    (if (number? result-float)
      ;(if (and (pos? result-float) (< result-float 1)) ;should return a value between 0 and 1
        ;result-float
        (sigmoid result-float)
         ;999)
      1000))
  )

(defn discriminator-error
  [individual]
  (assoc individual
    :errors
    (doall
      (for [i (range 20)
            :let [real (generate-seq sequence-length 0)
                  fake (generate-random-seq sequence-length)
                  dx (discriminator-result real (:program individual))
                  dgz (discriminator-result fake (:program individual))]]

        ; minimax log
        ; (- (+ (Math/log dx) (Math/log (- 1 dgz))))

        ; minimise mean squares loss
        (+ (Math/pow (- dx 1) 2) (Math/pow dgz 2))
        )
      )
    )
  )

(def discriminator-atom-generators
  (concat (list
            []
            ;;; end constants
            (fn [] (rand)) ;
            ;;; end ERCs
            (tag-instruction-erc [:integer :float :vector_integer :exec] 100)
            (tagged-instruction-erc 100)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :float :vector_integer :exec])))

(def discriminator-argmap
  {:error-function                 discriminator-error
   :atom-generators                discriminator-atom-generators
   :error-threshold                0
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :return-simplified-on-failure   true
   :max-points                     60
   :max-generations                300
   :visualize                      false
   :print-csv-logs                 true
   :csv-log-filename              (str (.format (java.text.SimpleDateFormat. "dd-MM-yyyy HH-mm") (new java.util.Date))
                                   " discr_sigmoid_300gen.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn discr []
  (def pop-agents (pushgp discriminator-argmap))

  ;testing discriminator result
  (def individual (nth pop-agents 0))
  (let [input [4 3 8 9 1]
        state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push (:program individual))
                   )
        ]
    (state-pretty-print state))

  )

(def best-simple-program (vector_integer_pushall
               -0.01734426750584627
               exec_stackdepth
               float_cos
               exec_dup_items
               float_tan
               integer_max
               float_cos
               integer_max
               exec_do*count
               (exec_do*count
                 vector_integer_set
                 exec_yankdup
                 integer_dup_times
                 exec_dup_times
                 ())))

(defn discriminator-actual-result
  [input program]
  (let [state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push program))
        result-float (top-item :float state)]
    result-float)
  )

(defn evaluate-discr
  [individual]
    (for [i (range 20)
          :let [real (generate-seq sequence-length 0)
                fake (generate-random-seq sequence-length)
                dx (discriminator-actual-result real (:program individual))
                dgz (discriminator-actual-result fake (:program individual))]]
       (println "Real: " real dx (> dx 0.5) " Fake: " fake dgz (< dgz 0.5))
      )
  )

;(defn gan-gp-run []
;  (for [i (range 10)]
;    ()
;    )
;  )
