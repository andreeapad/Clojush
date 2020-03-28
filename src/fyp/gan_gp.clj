(ns fyp.gan-gp
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use [clojush.ns])
  )

(load "sequence_generation")
(load "initial")

(def best-discr1
  '(in1
     vector_integer_pushall
     in1
     integer_gte
     vector_integer_contains
     exec_eq
     exec_dup_items
     0.5780758433312891
     integer_gt
     -6.097087002955089
     exec_do*vector_integer
     (integer_max integer_max integer_dup boolean_stackdepth integer_lte exec_if float_inc ()))
  )

(def best-discr2
  '(vector_integer_pushall
     float_div
     integer_max
     integer_max
     integer_max
     float_swap
     vector_integer_swap
     exec_while
     integer_sub
     boolean_or
     integer_max
     float_frominteger
     float_dup
     float_dup
     integer_max
     float_swap
     boolean_rot
     exec_do*range
     float_dup
     float_tan
     exec_dup_items
     float_mult
     exec_dup_items))

(def best-discr3
  '(vector_integer_yank boolean_fromfloat boolean_fromfloat vector_integer_yankdup float_swap exec_dup_items integer_sub vector_integer_conj vector_integer_eq integer_div float_lte integer_sub float_frominteger vector_integer_pushall exec_do*count (float_dec boolean_rot integer_mod exec_do*range (vector_integer_take) integer_min exec_noop exec_k (integer_gte integer_dup_items vector_integer_rot float_add boolean_invert_second_then_and integer_gte integer_fromboolean integer_mod exec_do*range (boolean_xor) boolean_and vector_integer_rot exec_k (integer_gte integer_fromboolean integer_div exec_do*range (float_lt)) (vector_integer_shove) boolean_empty integer_yank float_frominteger vector_integer_eq float_dup_times float_tan float_mult exec_if () ()) ())))

(def best-discr)
(def best-generator)
(def discr-pop)
(def gen-pop)
(def d-child-agents)
(def g-child-agents)
(def g-rand-gens)
(def d-rand-gens)

(defn restrict-g-output
  [output]
  (vec (map (fn [x] (mod x 5)) output))
  )

(defn generator-gan-error
  [individual]
  (assoc individual
    :errors
    (doall
      (for [i (range 20)
            :let [gen-result (generator-result sequence-length (:program individual))]
            ]
        (if (vector? gen-result)
          (let [gen-result (restrict-g-output gen-result)]
            (+ (* 100 (math/abs (- (count gen-result) sequence-length)))
               (let [discr-result (discriminator-sigmoid-result gen-result (:program best-discr))]
                 (math/abs (- 1 discr-result))))
            )
          10000
          )
        )
      )
    )
  )


(def generator-gan-argmap
  {:error-function                 generator-gan-error
   :atom-generators                generator-atom-generators
   :error-threshold                0
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :return-simplified-on-failure   true
   :max-points                     500
   :max-generations                1
   :visualize                      false
   :final-report-simplifications   250
   :print-csv-logs                 false
   :csv-log-filename               (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
                                        (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                        " generator_gan_seq12_100_d1.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn discr-gan-error
  [individual]
  (assoc individual
    :errors
    (let [sorted-g (sort-by :errors gen-pop)]
      (doall
        (for [i (range 20)
              :let [real (generate-seq sequence-length 0)
                    ; Takes first 20 best generator results
                    gen-result (generator-result sequence-length (:program (nth sorted-g i)))
                    fake (restrict-g-output gen-result)
                    dx (discriminator-sigmoid-result real (:program individual))
                    dgz (discriminator-sigmoid-result fake (:program individual))]]
          ; minimise mean squares loss
          (+ (Math/pow (- dx 1) 2) (Math/pow dgz 2))
          )
        )
      )
    )
  )

(def discr-gan-argmap
  {:error-function                 discr-gan-error
   :atom-generators                discriminator-atom-generators
   :error-threshold                0
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :return-simplified-on-failure   true
   :max-points                     500
   :max-generations                1
   :visualize                      false
   :final-report-simplifications   250
   :print-csv-logs                 false
   :csv-log-filename               (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
                                        (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                        " discr_gan_seq12_100_d1.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn train-step [i]
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println "Starting train step " i)
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println "Training Discriminator...")
  ; Train Discriminator giving previous saved population as input
  (def results (pushgp-custom discr-gan-argmap (pop-agents discr-pop) d-child-agents d-rand-gens))
  (def best-discr (nth results 0))
  (def discr-pop (nth results 1))
  (def d-child-agents  (nth results 2))
  (def d-rand-gens  (nth results 3))

  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println "")
  (println "Training Generator...")
  ; Train Generator giving previous saved population as input
  (def results (pushgp-custom generator-gan-argmap (pop-agents gen-pop) g-child-agents g-rand-gens))
  (def best-generator (nth results 0))
  (def gen-pop (nth results 1))
  (def g-child-agents  (nth results 2))
  (def g-rand-gens  (nth results 3))

  ; Print results to file
  (csv-print discr-pop (+ i 1) discr-gan-argmap)
  (csv-print gen-pop (+ i 1) generator-gan-argmap)
  )

(defn gan-gp-run
  ;; with initial discr training
  [iterations]
  ; Train discr, save best discr, save discr population
  (def results (pushgp discriminator-argmap))
  (def best-discr (nth results 0))
  (def discr-pop (nth results 1))
  (def d-child-agents  (nth results 2))
  (def d-rand-gens  (nth results 3))
  (println "Initialized first discriminator population.")

  ; Do the same for generator
  (def results (pushgp generator-gan-argmap))
  (def best-generator (nth results 0))
  (def gen-pop (nth results 1))
  (def g-child-agents  (nth results 2))
  (def g-rand-gens  (nth results 3))
  (println "Initialized first generator population.")

  ; Print results to file
  (csv-print discr-pop 0 discr-gan-argmap)
  (csv-print gen-pop 0 generator-gan-argmap)

  ; Training
  (dotimes [i iterations]
    (train-step i))

  ; Sort populations by error
  (def sorted-gen-pop (sort-by :errors gen-pop))
  (def sorted-discr-pop (sort-by :errors discr-pop))

  ; Print final population to file
  (spit (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
             (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
             " discr_gan_seq12_100_d1_population.edn") (with-out-str (pr sorted-discr-pop)))
  (spit (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
             (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
             " generator_gan_seq12_100_d1_population.edn") (with-out-str (pr sorted-gen-pop)))

  ; Reading pop from file
  ;(def gen-pop (read-string (slurp "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\13-03-2020 generator_gan_seq12_50x1_population.edn")))

  ; Results from a part of the pop
  (for [i (range 20)]
    (generator-result sequence-length (:program (nth sorted-gen-pop i)))
    )

  ;; Load previous pop and best
  ;(def best-discr (nth discr-pop 0))
  ;(def best-generator (nth gen-pop 0))
  ;; Population is saved sorted, so need to shuffle at the start
  ;(def discr-pop (shuffle discr-pop))
  ;(def gen-pop (shuffle gen-pop))
  )


(defn test-training-pushgp-custom [iterations]
  (def results (pushgp generator-argmap))
  (def best-generator (nth results 0))
  (def gen-pop (nth results 1))
  (def child-agents  (nth results 2))
  (def rand-gens  (nth results 3))
  (csv-print gen-pop 0 generator-argmap)

  (dotimes [i iterations]
      (println "")
      (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (println "Starting train step " i)

      (def results (pushgp-custom generator-argmap (pop-agents gen-pop) child-agents rand-gens))
      (def best-generator (nth results 0))
      (def gen-pop (nth results 1))
      (def child-agents  (nth results 2))
      (def rand-gens  (nth results 3))
      (csv-print gen-pop (+ i 1) generator-argmap)
      )
  )
