(ns shifting-sands.subs
  (:require
   [re-frame.core :as re-frame]
   [shifting-sands.db :as db]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::current-floor
 (fn [db]
   (::db/current-floor db)))

(re-frame/reg-sub
 ::notes
 (fn [{floor ::db/current-floor :as db} [_ coord]]
   (get-in db [::db/floors floor ::db/map coord ::db/notes] "")))

(re-frame/reg-sub
 ::exit-index
 (fn [db]
   (get-in db [::db/floors (::db/current-floor db) ::db/exit-index])))

(defn room->adjacent-unexplored
  "Returns a map of coord --> unexplored room for coords adjacent to the
   given room"
  [coord room]
  (->> (db/adjacent-coords coord room)
       (map #(hash-map (second %)
                       (assoc (::db/unexplored db/universal-rooms)
                              ::db/hallways [(db/opposite-dir (first %))])))
       (apply merge)))

(defn merge-unexplored-rooms
  [r1 r2]
  (merge-with (fn [v1 v2] (if (coll? v1) (into [] (concat v1 v2)) v1)) r1 r2))

(defn add-unexplored-rooms
  [floor-map]
  (merge (->> (map (partial apply room->adjacent-unexplored) floor-map)
              (apply merge-with merge-unexplored-rooms))
         floor-map))

(defn add-empty-rooms
  [floor-map]
  (let [{:keys [max-x min-x max-y min-y]} (db/get-map-bounds floor-map)
        ;; If I need to even out columns again in the future:
        ;; x-range (max (db/abs max-x) (db/abs min-x))
        ]
    (merge
     (apply merge (for [x (range min-x (inc max-x))
                        y (range min-y (inc max-y))]
                    {[x y] (::db/empty db/universal-rooms)}))
     floor-map)))

(re-frame/reg-sub
 ::floor-map
 (fn [{floor ::db/current-floor :as db}]
   (let [floor-map (get-in db [::db/floors floor ::db/map])]
     (-> (add-unexplored-rooms floor-map)
         add-empty-rooms))))

(re-frame/reg-sub
 ::room-list
 (fn [{floor ::db/current-floor :as db}]
   (let [floor-map (get-in db [::db/floors floor ::db/map])]
     (->> (vals floor-map)
          (sort-by ::db/room-index)))))

(re-frame/reg-sub
 ::modal-result
 (fn [db] (get db ::db/modal-result)))

(re-frame/reg-sub
 ::slug-map
 (fn [db] (::db/slugs db)))

(re-frame/reg-sub
 ::show-slugs
 (fn [db] (::db/show-slugs db)))

(re-frame/reg-sub
 ::show-generate-dialog?
 (fn [db] (::db/show-generate-dialog db)))

(re-frame/reg-sub
 ::re-pressed-example
 (fn [db _]
   (:re-pressed-example db)))

(re-frame/reg-sub
 ::generate-result
 (fn [db] (get db ::db/generate-result)))

(re-frame/reg-sub
 ::show-reset-dialog?
 (fn [db] (get db ::db/show-reset-dialog?)))

(re-frame/reg-sub
 ::history
 (fn [db] (reverse (get db ::db/history))))

(re-frame/reg-sub
 ::show-history?
 (fn [db] (get db ::db/show-history?)))

(re-frame/reg-sub
 ::room-adv
 (fn [db] (get db ::db/room-adv)))

(re-frame/reg-sub
 ::showing-coord
 (fn [db _] (get db ::db/showing-coord)))

(re-frame/reg-sub
 ::expanded-notes?
 (fn [db _] (get db ::db/expanded-notes?)))

(re-frame/reg-sub
 ::current-room
 (fn [db _] (get db ::db/current-room)))

(re-frame/reg-sub
 ::active-page
 (fn [db _] (get db ::db/active-page)))

(re-frame/reg-sub
 ::new-character
 (fn [db _] (get db ::db/new-character)))
