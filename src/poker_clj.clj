(ns poker-clj)

;; deck
;; 13 numbered cards
;; x
;; 4 suits

;; [{:suit :hearts :number :ace}
;;  {:suit :clubs :number :ace}
;;  {:suit :clubs :number :ten}]

(defn prn-card [{:keys [suit number]}]
  (str (name number) " of " (name suit)))

(defn prn-cards [cards]
  (map prn-card cards))

;; shuffle deck

(defn peek [deck num]
  (->> deck
       shuffle
       (take num)
       (map prn-card)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-deck [suits numbers]
  (for [suit   suits
        number numbers]
    {:suit suit :number number}))

(def suits #{:hearts :diamonds :clubs :spades})

(def numbers #{:ace :two :three :four :five :six :seven
               :eight :nine :ten :jack :queen :king})

;; draw a hand of 5 cards

(defn shuffle-deck [world]
  (update world :deck shuffle))

(defn remove-cards [deck amount]
  (vec (drop amount deck)))

(defn draw-hand [world hand-size]
  (let [deck (:deck world)
        hand (vec (take hand-size deck))]
    ;; world is the same here
    (prn :-----------------------)
    (prn :world world)
    (prn :hand hand)
    (prn)
    (-> world
        (update :deck  remove-cards hand-size)
        (update :hands conj hand))))

(def world {:deck  (make-deck (take 2 suits) (take 3 numbers))
            :hands []})

;;;;;;

(def game (atom world)) @game
(def history (atom [@game])) @history

(defn update-game! []
  (let [new-world (swap! game #(draw-hand % 2))]
    (swap! history conj new-world)))

(reset! game (nth @history 2))

(update-game!)

(-> world
    ;; shuffle-deck
    (draw-hand 2)
    (draw-hand 2)
    (draw-hand 2))

(defn combo-for-hand [hand]
  :pair
  :two-pair
  :triple
  :full-house
  :flush
  :royal-flush)
