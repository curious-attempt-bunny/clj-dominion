(ns dominion.core)

(defrecord State [supply deck discard hard])

(defrecord Card [name cost types victory-value money-value])

(defn draw [state remaining]
  (cond
    (zero? remaining) state
    (empty? (:deck state))
      (draw
        (State/create
          {:deck    (:discard state)
           :hand    (:hand state)
           :discard []
           :supply  (:supply state)})
        remaining)
    :otherwise
        (let [deck (shuffle (:deck state))]
          (draw (State/create
                  {:deck    (drop 1 deck)
                   :hand    (conj (:hand state) (first deck))
                   :discard (:discard state)
                   :supply  (:supply state)})
                (dec remaining))) ))

(defn deal [state]
  (draw state 5))

(def estate (Card/create {
                :name          :estate
                :types         [:victory]
                :cost          2
                :victory-value 1
                :money-value   0}))

(def dutchy (Card/create {
                :name          :dutchy
                :types         [:victory]
                :cost          5
                :victory-value 3
                :money-value   0}))

(def province (Card/create {
                :name          :province
                :types         [:victory]
                :cost          8
                :victory-value 6
                :money-value   0}))

(def copper (Card/create {
                :name          :copper
                :types         [:money]
                :cost          0
                :victory-value 0
                :money-value   1}))

(def silver (Card/create {
                :name          :silver
                :types         [:money]
                :cost          3
                :victory-value 0
                :money-value   2}))

(def gold (Card/create {
                :name          :gold
                :types         [:money]
                :cost          6
                :victory-value 0
                :money-value   3}))

(def state (State/create {
                :hand    []
                :deck    []
                :discard (concat (repeat 3 estate) (repeat 7 copper))
                ;:discard (concat (repeat 6 copper))
                :supply  {
                          copper   50
                          estate   8
                          silver   30
                          dutchy   8
                          gold     20
                          province 8}}))

(defn round [state]
  ;(println "before deal" (map :name (:hand state)) "/" (map :name (:deck state)) "/" (map :name (:discard state)))
  (let [state    (deal state)
        money       (apply + (map :money-value (:hand state)))
        preferences #{:province :gold :silver}
        options     (filter (fn [card]
                              (and (<= (:cost card) money)
                                   (> (get (:supply state) card) 0)
                                   (contains? preferences (:name card)))) (keys (:supply state)))
        purchase    (last options)]
    ;(println "before purchase" (map :name (:hand state)) "/" (map :name (:deck state)) "/" (map :name (:discard state)))
    (println (map :name (:hand state)) "\t-->" money (if (nil? purchase) nil (:name purchase)))
    (State/create {
       :hand    []
       :deck    (:deck state)
       :discard (concat (:discard state) (:hand state) (if (nil? purchase) [] [purchase]))
                        :supply (:supply state)})))

(loop [turn 1
       state state]
  (let [state     (round state)
        provinces (count (filter (fn [card] (= (:name card) :province)) (concat (:discard state) (:deck state))))]
    (println turn provinces)
    (if (< provinces 6)
      (recur (inc turn) state))))
