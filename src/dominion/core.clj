(ns dominion.core)

(defn draw [state remaining]
 (if (zero? remaining)
    state
    (if (empty? (:deck state))
      (draw
        (State/create
          {:deck    (shuffle (:discard state))
           :hard    (:hand state)
           :discard []
           :supply  (:supply state)})
        remaining)
      (draw (State/create
              {:deck    (drop 1 (:deck state))
               :hand    (conj (:hand state) (first (:deck state)))
               :discard (:discard state)
               :supply  (:supply state)})
            (dec remaining)))))

(defn deal [state]
  (draw state 5))

(defrecord Card [name cost types victory-value money-value])

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

(defrecord State [supply deck discard hard])

(def state (State/create {
                :hand    []
                :deck    []
                :discard (concat (repeat 3 estate) (repeat 7 copper))
                :supply  {
                          copper   50
                          estate   8
                          silver   30
                          dutchy   8
                          gold     20
                          province 8}}))

(println (deal state))