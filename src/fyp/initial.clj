(in-ns 'fyp.gan-gp)

(use-clojush)

(defn sigmoid
  [x]
  (float (/ 1 (+ 1 (Math/exp (- x)))))
  )

(defn pop-agents
  [population]
  (mapv #(agent % :error-handler agent-error-handler)
        population)
  )

(defn pushgp-result
  [best population child-agents rand-gens generation error-function report-simplifications]
  [best population child-agents rand-gens]
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

(def generator-atom-generators
  (concat (list
            []  ;; constants
            (fn [] (- (lrand-int 201) 100)) ;; erc
            'in1  ;; input instructions
            )
          (registered-for-stacks [:boolean :integer :float :vector_integer :exec ])))

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
   ;:final-report-simplifications   250
   :print-csv-logs                 false
   :csv-log-filename              (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
                                       (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                       " generator_simple_pattern_500_3.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn generator-result
  [input program]
  (let [state (->> (make-push-state)
                   (push-item input :integer)
                   (push-item input :input)
                   ; Add random noise as input
                   (push-item (rand) :float)
                   (run-push program))
        result-sequence-vector (top-item :vector_integer state)]
    result-sequence-vector
    )
  )

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

(defn discriminator-sigmoid-result
  [input program]
  (let [state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push program))
        result-float (top-item :float state)]
    (if (number? result-float)
      (sigmoid result-float)
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
                  dx (discriminator-sigmoid-result real (:program individual))
                  dgz (discriminator-sigmoid-result fake (:program individual))]]
        ; minimise L2 loss
        (+ (Math/pow (- dx 1) 2) (Math/pow dgz 2))
        )
      )
    )
  )

(def discriminator-atom-generators
  (concat (list
            []  ;; constants
            (fn [] (rand))   ;; erc
            'in1  ;; input instructions
            )
          (registered-for-stacks [:boolean :integer :float :vector_integer :exec])))

(def discriminator-argmap
  {:error-function                 discriminator-error
   :atom-generators                discriminator-atom-generators
   :error-threshold                0
   :parent-selection               :tournament
   :genetic-operator-probabilities {:alternation      0.5
                                    :uniform-mutation 0.5}
   :return-simplified-on-failure   true
   :max-points                     500
   :max-generations                1
   :visualize                      false
   ;:final-report-simplifications   250
   :print-csv-logs                 false
   :csv-log-filename               (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
                                        (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                        " discr_sigmoid_seq12_300gen_2.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(def best-discr-program-single-training
  '(exec_stackdepth
     exec_do*vector_integer
     (exec_dup_items
       float_max
       float_mult
       -51.19677241067835
       integer_dup
       integer_dec
       float_dec
       float_dec
       float_dec
       float_dec
       float_dec
       float_yankdup)))

(defn discr []
  (def pop-agents (pushgp discriminator-argmap))

  ;testing discriminator result
  (def individual (nth pop-agents 0))
  (let [input [4 3 8 9 1 1 1 2 1 1 1 1]
        state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push (:program individual))
                   )
        ]
    (state-pretty-print state))

  ;Print final population to file
  (spit (str "C:\\Users\\Andreea\\OneDrive\\University\\third year\\final year project\\experiments\\"
             (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
             " discr_sigmoid_seq12_300gen_population.edn") (with-out-str (pr pop-agents)))

  ;(def read-pop (read-string (slurp "population.edn")))
  )

; if using sigmoid, then use the other result function
(defn discriminator-simple-result
  [input program]
  (let [state (->> (make-push-state)
                   (push-item input :vector_integer)
                   (push-item input :input)
                   (run-push program))
        result-float (top-item :float state)]
    result-float)
  )

(defn evaluate-discr-simple
  [program]
  (loop [i 0
         error-fake 0
         error-real 0]
    (if (= i 5000)
      (println (/ error-fake 5000.) (/ error-real 5000.))
      (let [real (generate-seq sequence-length 0)
            fake (generate-random-seq sequence-length)
            dx (discriminator-simple-result real program)
            dgz (discriminator-simple-result fake program)]
        (recur (inc i) (if (< dx 0.5)
                         (inc error-real)
                         error-real)
               (if (> dgz 0.5)
                 (inc error-fake)
                 error-fake)))
      )
    ;(println "Real: " real dx (> dx 0.5) " Fake: " fake dgz (< dgz 0.5))
    )
  )

(defn evaluate-discr-sigmoid
  [program]
  (loop [i 0
         error-fake 0
         error-real 0]
    (if (= i 5000)
      (println (/ error-fake 5000.) (/ error-real 5000.))
      (let [real (generate-seq sequence-length 0)
            fake (generate-random-seq sequence-length)
            dx (discriminator-sigmoid-result real program)
            dgz (discriminator-sigmoid-result fake program)]
        (recur (inc i) (if (< dx 0.5)
                         (inc error-real)
                         error-real)
               (if (> dgz 0.5)
                 (inc error-fake)
                 error-fake)))
      )
    ;(println "Real: " real dx (> dx 0.5) " Fake: " fake dgz (< dgz 0.5))
    )
  )
