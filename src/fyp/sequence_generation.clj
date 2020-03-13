(in-ns 'fyp.gan-gp)

(def ^:const sequence-length 12)
(def ^:const start-state 0)


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

(defn test-no-of-possible-sequences []
  (count (set (for [i (range 120000)
                    :let [seq (generate-seq sequence-length 0)]]
                seq
                ))))

(defn lazy-contains? [coll key]
  (boolean (some #(= % key) coll)))

(defn test-freq-random-in-real-samples [n]
  (def real-samples (for [i (range 10000)
                          :let [seq (generate-seq sequence-length 0)]]
                      seq
                      )
    )
  (loop [i 0
         occurences 0
         rand-seq (generate-random-seq sequence-length)]
    (if (= i n)
      (println (/ occurences n))
      (let [result (if (true? (lazy-contains? real-samples rand-seq)) 1 0)]
        (recur (inc i) (+ occurences result) (generate-random-seq sequence-length))
        )
      )
    )
  )