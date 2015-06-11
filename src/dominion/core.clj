(ns dominion.core)

(defn draw [state remaining]
 (if (zero? remaining)
    state
    (if (empty? (:deck state))
      (draw
        {:deck    (shuffle (:discard state))
         :hard    (:hand state)
         :discard []}
        remaining)
      (draw {:deck    (drop 1 (:deck state))
             :hand    (conj (:hand state) (first (:deck state)))
             :discard (:discard state)}
            (dec remaining)))))

(defn deal [state]
  (draw state 5))

(def state
  {:deck    []
   :discard (concat (repeat 3 :estate) (repeat 7 :copper))
   :hand    []})

(println (deal state))