(ns ai-playground.client.core
  (:require [pinot.html :as html]
            [pinot.dom :as dom]
            [pinot.draw.visualization :as vis])
  (:require-macros [pinot.macros :as pm]))

(def x (html/html [:p [:em "hey"]]))
(dom/css x {:color :blue})
(dom/attr x {:class "para"})
;(dom/val (dom/query "input"))


(pm/defpartial canvas []
  [:svg:svg {:width 800 :height 400}])

(pm/defpartial item [[x y]]
  [:svg:circle {:r (+ 4 (rand 4)) :cx x :cy y}])

(dom/append (dom/query "body") (canvas))

(def lines (for [x (range 0 780 20)
                 y (range 0 380 20)
                 d [0 20]]
             [(+ x 10) (+ y 10) (+ x 10 d) (+ y 10 (- 20 d))]))

(pm/defpartial line [[x1 y1 x2 y2]]
  [:svg:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2}])

(-> (vis/visual lines)
    (vis/elem line)
    (vis/attr :stroke "#333")
    (vis/enter (partial dom/append (dom/query "svg"))))

(def items (for [x (range 0 800 20)
                 y (range 0 400 20)]
             [(+ x 10) (+ y 10)]))

(def colors ["#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900"])

(-> (vis/visual items)
  (vis/elem item)
  (vis/attr :stroke "#333")
  (vis/attr :fill #(rand-nth colors))
;  (vis/attr :cx first)
;  (vis/attr :cy second)
  (vis/enter (partial dom/append (dom/query "svg"))))

