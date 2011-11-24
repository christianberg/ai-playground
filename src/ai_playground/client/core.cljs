(ns ai-playground.client.core
  (:require [ai-playground.client.algorithms.clustering :as clustering]
            [pinot.html :as html]
            [pinot.dom :as dom]
            [pinot.events :as events]
            [pinot.draw.visualization :as vis])
  (:require-macros [pinot.macros :as pm]))

(def x (html/html [:p [:em "hey"]]))
(dom/css x {:color :blue})
(dom/attr x {:class "para"})
;(dom/val (dom/query "input"))


(pm/defpartial canvas []
  [:svg:svg {:width 800 :height 400}])

(pm/defpartial legend []
  [:div#legend [:h2 "Legend"]])

(pm/defpartial button [text]
  [:input {:type "submit" :value text}])

(defn append-button [parent text fn]
  (let [b (button text)]
    (events/on b :click fn)
    (dom/append parent b)))

(dom/append (dom/query "body") (canvas))
(dom/append (dom/query "body") (legend))

(def colors ["#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900"])

(pm/defpartial data-point [[x y]]
  [:svg:circle {:r 4 :cx x :cy y :stroke "#333" :fill "#fff"}])

(pm/defpartial mean [[x y]]
  [:svg:circle {:r 6 :cx x :cy y :stroke "#333" :fill (colors 1)}])

(def points (atom []))

(def initial-state {:next-step :init
                    :k 3
                    :points []})
(def state (atom initial-state))

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
    (-> (vis/visual @points)
        (vis/elem data-point)
        (vis/enter (partial dom/append (dom/query "svg"))))
    (-> (vis/visual means)
        (vis/elem mean)
        (vis/enter (partial dom/append (dom/query "svg"))))))

(defn clear-svg []
  (dom/empty (dom/query "svg")))

(defn visualize []
  (clear-svg)
  (-> (vis/visual (@state :points))
      (vis/elem data-point)
      (vis/enter (partial dom/append (dom/query "svg")))))

(defn add-random-cluster []
  (swap! state (fn [{points :points :as state}]
                 (assoc state :points (concat points (random-cluster 8)))))
  (visualize))

(defn step []
  (js/alert (str (@state :next-step)))
  (swap! state clustering/k-means-step)

  (visualize))

(append-button (dom/query "#legend") "Clear" #(do (swap! state (constantly initial-state)) (visualize)))
(append-button (dom/query "#legend") "Add Random Cluster" add-random-cluster)
(append-button (dom/query "#legend") "Step" step)
(append-button (dom/query "#legend") "Run" k-means-vis)
