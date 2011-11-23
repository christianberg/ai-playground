(ns ai-playground.client.core
  (:require [ai-playground.client.algorithms.clustering :as clustering]
            [pinot.html :as html]
            [pinot.dom :as dom]
            [pinot.draw.visualization :as vis])
  (:require-macros [pinot.macros :as pm]))

(def x (html/html [:p [:em "hey"]]))
(dom/css x {:color :blue})
(dom/attr x {:class "para"})
;(dom/val (dom/query "input"))


(pm/defpartial canvas []
  [:svg:svg {:width 800 :height 400}])

(dom/append (dom/query "body") (canvas))

(def colors ["#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900"])

(pm/defpartial data-point [[x y]]
  [:svg:circle {:r 4 :cx x :cy y :stroke "#333" :fill "#fff"}])

(pm/defpartial mean [[x y]]
  [:svg:circle {:r 6 :cx x :cy y :stroke "#333" :fill (colors 1)}])

(defn random-cluster [n]
  (let [cx (+ 50 (rand 700))
        cy (+ 50 (rand 300))]
    (for [_ (range n)]
      [(+ cx -40 (rand 80)) (+ cy -40 (rand 80))])))

(defn k-means-vis []
  (let [k 5
        N 8
        data (mapcat random-cluster (repeat k N))
        means (clustering/k-means k data)]
    (-> (vis/visual data)
        (vis/elem data-point)
        (vis/enter (partial dom/append (dom/query "svg"))))
    (-> (vis/visual means)
        (vis/elem mean)
        (vis/enter (partial dom/append (dom/query "svg"))))))

(k-means-vis)