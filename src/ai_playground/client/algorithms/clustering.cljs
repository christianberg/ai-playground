(ns ai-playground.client.algorithms.clustering)

(defn distance-squared [v1 v2]
  (reduce + (map #(* % %) (map - v1 v2))))

(defn append-to-closest [means-map point]
  (loop [means (keys means-map)
         closest-mean nil
         shortest-distance nil]
    (if (empty? means)
      (merge-with conj means-map {closest-mean point})
      (let [m (first means)
            d (distance-squared m point)]
        (if (or (nil? closest-mean) (< d shortest-distance))
          (recur (rest means) m d)
          (recur (rest means) closest-mean shortest-distance))))))

(defn average [data]
  (let [N (count data)]
    (map #(/ % N) (reduce (partial map +) data))))

(defn k-means [k data]
  (let [dimension (count (first data))]
    (loop [initial-means  (take k (repeatedly #(map (partial + (rand)) (rand-nth data))))]
      (let [new-means (map (fn [[k v]] (if (seq v) (average v) k))
                           (reduce append-to-closest (zipmap initial-means (repeat [])) data))]
        (if (= new-means initial-means)
          new-means
          (recur new-means))))))

(defn k-means-init [{:keys [k points] :as state}]
  (let [initial-means (take k (repeatedly #(map (partial + (rand)) (rand-nth points))))
        means-map (zipmap initial-means (map #({:index % :points []}) (range)))]
    (merge state
           {:next-step :group
            :means means-map
            :old-means {}})))

(defn k-means-group [state]
  (merge state
         {:next-step :move}))

(defn k-means-move [state]
  (merge state
         {:next-step :group}))

(defn k-means-step [state]
  (condp = (state :next-step)
    :init (k-means-init state)
    :group (k-means-group state)
    :move (k-means-move state)
    (js/alert (str "Something's wrong! Next step is: " (state :next-step)))))