(ns ai-playground.client.core
  (:require [ai-playground.client.algorithms.clustering :as clustering]
            [pinot.html :as html]
            [pinot.dom :as dom]
            [pinot.events :as events]
            [pinot.draw.visualization :as vis]
            [pinot.util.clj :as pclj])
  (:require-macros [pinot.macros :as pm]))

(defn remove-attribute [elem k]
  (doseq [el (pclj/->coll elem)]
    (. el (removeAttribute (name k)))))

(pm/defpartial page []
  [:div.topbar
   [:div.topbar-inner [:div.container-fluid [:a.brand "AI Playground"]]]]
  [:div.container-fluid
   [:div.sidebar]
   [:div.content
    [:div.row
     [:div.span16 [:h2 "K-Means"]]]
    [:div.row
     [:div.span16
      [:svg:svg {:width 800 :height 400}]]]
    [:div.row
     [:div#legend.span16]]]])

(dom/append (dom/query "body") (page))

(pm/defpartial button [text id]
  (let [attr (if id {:id id} {})]
    [:button.btn attr text]))

(defn append-button
  ([parent text fn] (append-button parent text fn nil))
  ([parent text fn id]
     (let [b (if id (button text id) (button text))]
       (events/on b :click fn)
       (dom/append parent b)
       b)))

(def colors ["#b58900" "#cb4b16" "#dc322f" "#d33682" "#6c71c4" "#268bd2" "#2aa198" "#859900"])

(defn visualize-group [data function]
  (-> (vis/visual data)
      (vis/elem function)
      (vis/enter (partial dom/append (dom/query "svg")))))

(pm/defpartial data-point [color [x y]]
  [:svg:circle {:r 4 :cx x :cy y :stroke "#333" :fill color}])

(pm/defpartial line [[x1 y1] [x2 y2]]
  [:svg:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke "#ccc"}])

(pm/defpartial mean [[[x y] {:keys [index points]}]]
  (let [color (nth (cycle colors) index)
        r (Math/sqrt (+ 15 (* 4 (count points))))]
    (visualize-group points (partial line [x y]))
    (visualize-group points (partial data-point color))
    [:svg:circle {:r r :cx x :cy y :stroke "#333" :fill color}]))

(def initial-state {:next-step :init
                    :k 10
                    :points []})
(def state (atom initial-state))

(defn random-cluster [n]
  (let [cx (+ 50 (rand 700))
        cy (+ 50 (rand 300))]
    (for [_ (range n)]
      [(+ cx -40 (rand 80)) (+ cy -40 (rand 80))])))

(defn clear-svg []
  (dom/empty (dom/query "svg")))

(defn visualize []
  (clear-svg)
  (visualize-group (@state :points) (partial data-point "#fff"))
  (visualize-group (@state :means) mean))

(defn enable-buttons [enabled]
  (if enabled
    (remove-attribute (dom/query "#step-button") :disabled)
    (dom/attr (dom/query "#step-button") :disabled "disabled")))

(defn add-random-cluster []
  (swap! state (fn [{:keys [points next-step] :as state}]
                 (-> state
                     (assoc :points (concat points (random-cluster 8)))
                     (assoc :next-step (if (= next-step :init) :init :group)))))
  (enable-buttons true)
  (visualize))

(defn add-grid []
  (swap! state (fn [state]
                 (assoc state :points (for [x (range 10 800 20) y (range 10 400 20)] [x y]))))
  (enable-buttons true)
  (visualize))

(defn step []
  (swap! state clustering/k-means-step)
  (when (= (@state :next-step) :done)
    (enable-buttons false))
  (visualize))

(defn run []
  (step)
  (when (not= (@state :next-step) :done) (recur)))

(append-button (dom/query "#legend") "Clear" #(do (swap! state (constantly initial-state)) (visualize)))
(append-button (dom/query "#legend") "Add Random Cluster" add-random-cluster)
(append-button (dom/query "#legend") "Add Grid" add-grid)
(append-button (dom/query "#legend") "Step" step "step-button")
(append-button (dom/query "#legend") "Run" run "run-button")
