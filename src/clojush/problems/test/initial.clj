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
  {:error-function (fn [individual]
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
   :atom-generators (list (fn [] (lrand-int 10))
                          'in1
                          'integer_div
                          'integer_mult
                          'integer_add
                          'integer_sub)
   :parent-selection :tournament
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   :visualize true
   :problem-specific-report return-result-program
   })

(defn -main []
  (def result (pushgp argmap))
  )
