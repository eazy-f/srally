(ns srally.main
  (:require
   [goog.dom :as gdom]
   [goog.fx :as gfx]
   ; why the following cannot be imported along with goog.fx?
   [goog.fx.DragListDirection]
   [goog.fx.DragListGroup]))

(defn column-id
  [name]
  (str "column-" name))

(defn get-cards
  []
  [{:name "Lemon nation!"
    :state "In progress"
    :description "Kill all humans"}
   {:name "Wake up"
    :state "Defined"
    :description "Terminate sleep"}])

(defn add-card
  [{name :name
    state :state 
    description :description}]
  (let [column (gdom/getElement (column-id state))
        list (gdom/getElementByClass "CardList" column)]
    (gdom/appendChild
     list
     (gdom/createDom "div" #js{:class "Card"}
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
  (let [board (gdom/getElement "board")]
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
    (.log js/console "done")))
