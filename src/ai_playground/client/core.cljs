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

(pm/defpartial item [x]
  [:svg:circle {:r (* 2 x)}])

(dom/append (dom/query "body") (canvas))

(def items (range 10))

(-> (vis/visual items)
  (vis/elem item)
  (vis/attr :stroke "#333")
  (vis/attr :fill "#777")
  (vis/attr :cx #(+ 20 (rand-int 800)))
  (vis/attr :cy #(+ 80 (* 10 (mod % 4))))
  (vis/enter (partial dom/append (dom/query "svg"))))