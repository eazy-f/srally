(ns srally.main
  (:require
   [goog.dom :as gdom]
   [goog.fx :as gfx]
   ; why the following cannot be imported along with goog.fx?
   [goog.fx.DragListDirection]
   [goog.fx.DragListGroup]
   [goog.fx.DragListGroup.EventType]))

(defn column-id
  [name]
  (str "column-" name))

(defn set-ids
  [prefix objects]
  (-> (reduce (fn [[n acc] object]
                [(inc n)
                 (->> (assoc object :id (str prefix "-" n))
                      (conj acc))]) [0 (empty objects)] objects)
      second))

(defn get-card-deps
  []
  [#{"card-1" "card-3"}])

(defn get-cards
  []
  (let [cards [{:name "Lemon nation!"
                :state "In progress"
                :description "Kill all humans"}
               {:name "Wake up"
                :state "Defined"
                :description "Terminate sleep"}
               {:name "Kick a baby"
                :state "Accepted"
                :description "subj"}
               {:name "Culture training"
                :state "In progress"
                :description "Listen hammer smashed face"}]]
    (set-ids "card" cards)))

(defn get-board
  []
  (gdom/getElement "board"))

(defn get-cards-layout
  []
  (let [board (get-board)
        columns (gdom/getElementsByClass "Column" board)]
    (areduce columns i ret (array-map)
      (let [column (aget columns i)
            card-list (gdom/getElementByClass "CardList" column)
            cards (gdom/getElementsByClass "Card" card-list)
            column-id (aget column "id")]
        (assoc ret
          column-id
          (areduce cards j card-vector (vector)
                   (let [card (aget cards j)]
                     (->> (aget card "id")
                          (conj card-vector)))))))))

(defn add-card
  [{id :id
    name :name
    state :state
    description :description}]
  (let [column (gdom/getElement (column-id state))
        list (gdom/getElementByClass "CardList" column)]
    (gdom/appendChild
     list
     (gdom/createDom "div" #js{:id id :class "Card"}
      (gdom/createDom "div" #js{:class "CardContent"}
       (gdom/createDom "h3" nil name)
       (gdom/createDom "p" nil description))))))

(defn get-column-list
  []
  [{:name "Defined"}
   {:name "In progress"}
   {:name "Review"}
   {:name "Accepted"}
   {:name "Released"}])

(defn create-groups
  [columns]
  (let [board (get-board)]
    (map #(let [{name :name} %
                list (gdom/createDom "div" #js{:class "CardList"})
                column (gdom/createDom
                      "div" #js{:id (column-id name)
                                :class "Column"}
                      (gdom/createDom "h2" nil name)
                      list)]
            (gdom/appendChild board column)
            list)
         columns)))

(defn show-layout
  []
  (doseq [[column-id cards] (get-cards-layout)]
        (.log js/console column-id (count cards))))

(defn apply-layout
  [layout]
  (doseq [[column-id card-ids] layout]
    (let [column (gdom/getElement column-id)
          card-list (gdom/getElementByClass "CardList" column)
          cards (doall (map gdom/getElement card-ids))]
      (gdom/removeChildren card-list)
      (doseq [card cards] (gdom/appendChild card-list card)))))

(defn get-layout-score
  [layout]
  0)

(defn better-score
  [left right]
  "returns true if left score is better than right"
  (< left right))

(defn cartesian-product
  [cols]
  (if-not (< (count cols) 2)
    (for [head (first cols)
          tail (cartesian-product (next cols))]
      (conj tail head))
    (map list (first cols)))) ; TODO do something with this

(defn get-group-cards
  [group cards leftovers]
  (reduce
   (fn [[grouped leftovers] card]
     (if (get group card)
       [(conj (if grouped grouped []) card) (disj leftovers card)]
       [grouped leftovers]))
   [nil leftovers]
   cards))

(defn group-cards
  [groups0 cards]
  (let [[groupped ungroupped-set]
        (reduce
         (fn [[groups leftovers] group]
           (let [[grouped new-leftovers] (get-group-cards group cards leftovers)]
             [(if grouped
                (conj groups grouped)
                groups)
              new-leftovers]))
         [[] (into #{} cards)]
         groups0)]
    [groupped (filter (partial get ungroupped-set) cards)]))

(defn permutations
  [items0]
  (if (< (count items0) 2)
    [items0]
    (loop [items  items0
           rest   []
           result []]
      (if-not (empty? items)
        (let [head (first items)
              tail (next items)
              others (into rest tail)
              perms (map #(conj % head) (permutations others))]
          (recur tail (conj rest head) (into result perms)))
        result))))

(defn slices-splits
  [slices-num items-num]
  (if (= 0 slices-num)
    [[items-num]]
    (for [i (range 0 (inc items-num))
          rest (slices-splits (dec slices-num) (- items-num i))]
         (conj rest i))))

(defn slices
  [slices-num items]
  (let [splits (slices-splits slices-num (count items))]
    (map
     #(->> %
           (reduce
            (fn [[to-split splitted] n]
              (let [[left right] (split-at n to-split)]
                [right (conj splitted left)]))
            [items []])
           second)
     splits)))

(defn column-permutations
  [groups column]
  (let [[column-id cards] column
        [grouped-cards ungrouped] (group-cards groups cards)
        groups-num (count grouped-cards)
        grouped-permutations (permutations grouped-cards)
        ungrouped-slices (slices groups-num ungrouped)]
    (map
     (fn [[ungrouped grouped]]
       [column-id
        (reduce into []
                (into
                 (interleave ungrouped grouped)
                 (drop (count grouped) ungrouped)))])
     (cartesian-product [ungrouped-slices grouped-permutations]))))

(defn layout-permuations
  [layout]
  (let [groups (get-card-deps)
        columns-permutations (map (partial column-permutations groups) layout)]
    (map
     (partial into {})
     (cartesian-product columns-permutations))))

(defn optimize-layout
  [layout]
  (let [permutations (layout-permuations layout)]
    (transduce
     (map (fn [layout] [(get-layout-score layout) layout]))
     (fn
       ([final]
          (second final))
       ([best current]
          (if best
            (let [[best-score _] best
                  [current-score _] current]
              (if (better-score current-score best-score)
                current
                best))
            current)))
     nil
     permutations)))

(defn handle-layout-change
  [layout]
  (-> (optimize-layout layout)
      apply-layout))

(defn listen-drag
  [drag-group cards-layout]
  (let [layout-box #js{:layout cards-layout}]
    (.listen drag-group
             goog.fx.DragListGroup.EventType/DRAGEND
             (fn [_]
               (let [new-cards-layout (get-cards-layout)
                     old-layout (aget layout-box "layout")]
                 (aset layout-box "layout" new-cards-layout)
                 (if-not (= new-cards-layout old-layout)
                   (handle-layout-change new-cards-layout)))))))

(defn load
  []
  ;(.log js/console (.getOwnPropertyDescriptor js/Object js/document "location"))
  (let [groups (create-groups (get-column-list))
        drag-group (new gfx/DragListGroup)]
    (.setDraggerElClass drag-group "Card")
    (.setCurrDragItemClass drag-group "MovedCard")
    (.setIsCurrDragItemAlwaysDisplayed drag-group)
    (doseq [list groups]
           (.addDragList drag-group list goog.fx.DragListDirection/DOWN))
    (doseq [card (get-cards)] (add-card card))
    (.init drag-group)
    (.log js/console (print-str (cartesian-product [[:a :b] [1 2]])))
    (.log js/console "done")
    (listen-drag drag-group (get-cards-layout))))
