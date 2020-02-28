(ns fyp.gan-gp
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use [clojush.ns])
  )

(load "sequence_generation")
(load "initial")

(def best-discr)
(def best-generator)
(def discr-pop)
(def gen-pop)

(defn generator-gan-error
  [individual]
  (assoc individual
    :errors
    (doall
      (for [i (range 20)
            :let [gen-result (generator-result sequence-length (:program individual))]
            ]
        (if (vector? gen-result)
          (+ (* 100 (math/abs (- (count gen-result) sequence-length)))
             (let [discr-result (discriminator-sigmoid-result gen-result (:program best-discr))]
               (Math/pow (- 1 discr-result) 2)))
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
   :max-points                     100
   :max-generations                1
   :visualize                      false
   :print-csv-logs                 false
   :csv-log-filename               (str (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                        " generator_gan_test.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn discr-gan-error
  [individual]
  (assoc individual
    :errors
    (doall
      (for [i (range 20)
            :let [real (generate-seq sequence-length 0)
                  fake (generator-result sequence-length (:program best-generator))
                  dx (discriminator-sigmoid-result real (:program individual))
                  dgz (discriminator-sigmoid-result fake (:program individual))]]
        ; minimise mean squares loss
        (+ (Math/pow (- dx 1) 2) (Math/pow dgz 2))
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
   :max-points                     60
   :max-generations                1
   :visualize                      false
   :print-csv-logs                 false
   :csv-log-filename               (str (.format (java.text.SimpleDateFormat. "dd-MM-yyyy") (new java.util.Date))
                                        " discr_gan_test.csv")
   :csv-columns                    [:generation :total-error]
   :problem-specific-report        pushgp-result
   })

(defn train-step [i]
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println "Starting train step " i)
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")

  (println "Training Discriminator...")
  ; Train Discriminator giving previous saved population as input
  (def results (pushgp-custom discr-gan-argmap (pop-agents discr-pop)))
  (def best-discr (nth results 0))
  (def discr-pop (nth results 1))

  (println "Training Generator...")
  ; Train Generator giving previous saved population as input
  (def results (pushgp-custom generator-gan-argmap (pop-agents gen-pop)))
  (def best-generator (nth results 0))
  (def gen-pop (nth results 1))

  ; Print results to file
  (csv-print discr-pop (+ i 1) discr-gan-argmap)
  (csv-print gen-pop (+ i 1) generator-gan-argmap)

  )


(defn gan-gp-run
  [iterations]
  ; Train discr, save best discr, save discr population
  (def results (pushgp discriminator-argmap))
  (def best-discr (nth results 0))
  (def discr-pop (nth results 1))
  (println "Initialized first discriminator population.")

  ; Do the same for generator
  (def results (pushgp generator-gan-argmap))
  (def best-generator (nth results 0))
  (def gen-pop (nth results 1))
  (println "Initialized first generator population.")

  ; Print results to file
  (csv-print discr-pop 0 discr-gan-argmap)
  (csv-print gen-pop 0 generator-gan-argmap)

  (dotimes [i iterations]
    (train-step i))

  )
