(ns shifting-sands.events
  (:require
   [re-frame.core :as re-frame]
   [shifting-sands.db :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [cljs-time.core :as time]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]))

(defn check-and-throw
  "Throws an exception if `db` doesn't match the Spec `a-spec`."
  [a-spec db]
  (when-not (s/valid? a-spec db)
    (throw (ex-info (str "spec check failed: " (s/explain-str a-spec db)) {}))))

(def check-spec-interceptor (re-frame/after (partial check-and-throw ::db/db)))

(def ->local-store (re-frame/after db/state->local-store))

(def interceptors [->local-store])

(defn get-room-index [db coord]
  (let [floor (::db/current-floor db)]
    (get-in db [::db/floors floor ::db/map coord ::db/room-index])))

(re-frame/reg-event-fx
 ::set-active-page
 (fn-traced
  [{:keys [db]} [_ {:keys [page]}]]
  (let [set-page (assoc db ::db/active-page page)]
    (case page
      :home {:db set-page}
      :new-character {:db set-page
                      :dispatch [::generate-new-character]}
      :not-found {:db set-page}))))

(re-frame/reg-event-fx
 ::initialize-db
 [(re-frame/inject-cofx :local-store-state)]
 (fn-traced
  [{:keys [db local-store-state]} _]
  {:db (merge db/default-db local-store-state)}))

(re-frame/reg-event-db
 ::init-floor
 interceptors
 (fn-traced
  [db [_ floor]]
  (assoc-in db [::db/floors floor] (db/init-floor floor))))

(re-frame/reg-event-db
 ::change-floor
 interceptors
 (fn-traced
  [db [_ floor]]
  (assoc db ::db/current-floor floor)))

(re-frame/reg-event-db
 ::generate-loot
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} _]
  (let [loot (db/generate-loot (get-in db [::db/floors floor]))]
    (-> (assoc db ::db/modal-result (::db/description loot))
        (update
         ::db/history
         #(conj % {::db/description (::db/description loot)
                   ::db/floor floor
                   ::db/time (time/now)}))))))

(defn item->str [enc]
  (let [name (::db/name enc)
        desc (::db/description enc)]
    (str (when name name)
         (when (and name desc) " - ")
         (when desc desc))))

(re-frame/reg-event-db
 ::generate-encounter
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord]]
  (let [adv (get-in db [::db/floors floor ::db/map coord ::db/danger])
        enc (db/generate (get-in db [::db/floors floor]) [::db/encounter floor]
                         adv)]
    (-> (assoc db ::db/modal-result (::db/description enc))
        (update
         ::db/history
         #(conj % {::db/description (str "Generated encounter: "
                                         (item->str enc))
                   ::db/room-index (get-room-index db coord)
                   ::db/floor floor
                   ::db/time (time/now)}))))))

(re-frame/reg-event-db
 ::force-shop
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord]]
  (let [shop (db/generate-shop (get-in db [::db/floors floor]))]
    (-> (assoc-in db [::db/floors floor ::db/map coord ::db/situation] shop)
        (update
         ::db/history
         #(conj % {::db/description (str "Forced Shop:\n"
                                         (::db/description shop))
                   ::db/room-index (get-room-index db coord)
                   ::db/floor floor
                   ::db/time (time/now)}))))))

(re-frame/reg-event-db
 ::force-shrine
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord]]
  (let [shrine (db/generate-shrine (get-in db [::db/floors floor]))]
    (-> (assoc-in db [::db/floors floor ::db/map coord ::db/situation] shrine)
        (update
         ::db/history
         #(conj % {::db/description (str "Forced Shrine: " (::db/name shrine))
                   ::db/room-index (get-room-index db coord)
                   ::db/floor floor
                   ::db/time (time/now)}))))))

(re-frame/reg-event-db
 ::clear-modal-result
 (fn-traced
  [db _]
  (dissoc db ::db/modal-result)))

(re-frame/reg-event-db
 ::show-slugs
 (fn-traced
  [db _]
  (assoc db ::db/show-slugs true)))

(re-frame/reg-event-db
 ::hide-slugs
 (fn-traced
  [db _]
  (dissoc db ::db/show-slugs)))

(re-frame/reg-event-db
 ::show-reset-dialog
 (fn-traced
  [db _]
  (assoc db ::db/show-reset-dialog? true)))

(re-frame/reg-event-db
 ::hide-reset-dialog
 (fn-traced
  [db _]
  (dissoc db ::db/show-reset-dialog?)))

(re-frame/reg-event-db
 ::reset-all
 interceptors
 (fn-traced
  [db [_ reset-slugs?]]
  (-> (db/init-db db reset-slugs?)
      (dissoc ::db/show-reset-dialog?))))

(re-frame/reg-event-db
 ::rotate-room
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord dir]]
  (-> (update-in db [::db/floors floor ::db/map coord] #(db/rotate-room % dir))
      (update
       ::db/history
       #(conj % {::db/description (str "Rotate "
                                       (string/upper-case (name dir)))
                 ::db/room-index (get-room-index db coord)
                 ::db/floor floor
                 ::db/time (time/now)})))))

(defn generate-history-log [desc ridx floor]
  {::db/description desc
   ::db/room-index ridx
   ::db/floor floor
   ::db/time (time/now)})

(def adv->str (partial db/adv->str ""))

(re-frame/reg-event-db
 ::generate-room
 interceptors
 (fn-traced
  [{floor ::db/current-floor adv ::db/room-adv :as db} [_ coord from-dir]]
  (let [floor-state (db/generate-room (get-in db [::db/floors floor])
                                      coord from-dir adv)
        room (get-in floor-state [::db/map coord])
        situation (get room ::db/situation)]
    (-> (assoc-in db [::db/floors floor] floor-state)
        (assoc ::db/room-adv 0)
        (update
         ::db/history
         #(conj % (generate-history-log
                   (str "Generated room" (adv->str adv) ": "  (::db/name room))
                   (::db/room-index room) floor)))
        (update
         ::db/history
         #(conj % (generate-history-log
                   (str "Generated situation" (adv->str adv)": "
                        (::db/name situation))
                   (::db/room-index room) floor)))))))

(re-frame/reg-event-db
 ::regenerate-room
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord from-dir]]
  (let [prev (get-in db [::db/floors floor ::db/map coord])
        room-index (get prev ::db/room-index)
        adv (get prev ::db/adv)
        floor-state (-> (db/generate-room (get-in db [::db/floors floor])
                                          coord from-dir adv)
                        (assoc-in [::db/map coord ::db/room-index] room-index))
        room (-> (get-in floor-state [::db/map coord])
                 (assoc ::db/room-index room-index))
        situation (get room ::db/situation)]
    (-> 
     (assoc-in db [::db/floors floor] floor-state)
     (update
      ::db/history
      #(conj % (generate-history-log
                (str "Regenerated room " room-index (adv->str adv)": "
                     (::db/name room))
                (::db/room-index room) floor)))
     (update
      ::db/history
      #(conj % (generate-history-log
                (str "Regenerated situation" (adv->str adv) ": "
                     (::db/name situation))
                (::db/room-index room) floor)))))))

(re-frame/reg-event-db
 ::show-generate-dialog
 (fn-traced
  [db _]
  (assoc db ::db/show-generate-dialog true)))

(re-frame/reg-event-db
 ::hide-generate-dialog
 (fn-traced
  [db _]
  (dissoc db ::db/show-generate-dialog)))

(defn stringify-desc [{desc ::db/description :as item}]
  (merge
   item
   (when (::db/defense desc) {::db/description (db/armor->str item)})
   (when (::db/damage desc) {::db/description (db/weapon->str item)})))

(re-frame/reg-event-db
 ::generate-generic
 interceptors
 (fn-traced
  [db [_ path adv]]
  (let [floor (if (db/floors (last path)) (last path) (::db/current-floor db))
        item (-> (db/generate (get-in db [::db/floors floor]) path adv)
                 stringify-desc)]
    (-> db
        (assoc ::db/generate-result item)
        (update
         ::db/history
         #(conj % {::db/description (str  "Generate" (adv->str adv) ": "
                                          (item->str item))
                   ::db/floor floor
                   ::db/time (time/now)}))))))

(re-frame/reg-event-db
 ::clear-generate-result
 (fn-traced
  [db _]
  (dissoc db ::db/generate-result)))

(re-frame/reg-event-db
 ::show-history
 (fn-traced
  [db _]
  (assoc db ::db/show-history? true)))

(re-frame/reg-event-db
 ::hide-history
 (fn-traced
  [db _]
  (dissoc db ::db/show-history?)))

(re-frame/reg-event-db
 ::update-room-adv
 (fn-traced
  [db [_ update-fn]]
  (update db ::db/room-adv (comp (partial max -3) (partial min 3) update-fn))))

(re-frame/reg-event-db
 ::update-notes
 interceptors
 (fn-traced
  [{floor ::db/current-floor :as db} [_ coord notes]]
  (assoc-in db [::db/floors floor ::db/map coord ::db/notes] notes)))

(re-frame/reg-event-db
 ::show-room-menu
 (fn-traced
  [db [_ coord]]
  (assoc db ::db/showing-coord coord)))

(re-frame/reg-event-db
 ::hide-room-menu
 (fn-traced
  [db _]
  (-> (dissoc db ::db/showing-coord)
      (dissoc db ::db/expanded-notes?))))

(re-frame/reg-event-db
 ::toggle-expanded-notes
 (fn-traced
  [db _]
  (update db ::db/expanded-notes? not)))

(re-frame/reg-event-db
 ::hide-expanded-notes
 (fn-traced
  [db _]
  (dissoc db ::db/expanded-notes?)))

(re-frame/reg-event-db
 ::current-room
 interceptors
 (fn-traced
  [db [_ floor coord]]
  (assoc db ::db/current-room {::db/floor floor ::db/coord coord})))

(re-frame/reg-event-db
 ::generate-new-character
 (fn-traced
  [db _]
  (assoc db ::db/new-character (db/generate-new-character))))
