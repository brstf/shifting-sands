(ns shifting-sands.views
  (:require
   [clojure.string :as string]
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [re-com.core :as re-com]
   [shifting-sands.events :as events]
   [shifting-sands.router :as router]
   [shifting-sands.subs :as subs]
   [shifting-sands.db :as db]
   [cljs-time.core :as time]
   [cljs-time.format :as time-format]
   [goog.string :as gstring]
   [goog.string.format])
  (:require-macros [reagent.ratom :refer [reaction]]))

(def aqua-green "#84c7a8")

(defn text->hiccup
  "Convert newlines to [:br]'s."
  [text]
  (->> (string/split text "\n")
       (interpose [:br])
       (map #(if (string? %)
               %
               (with-meta % {:key (gensym "br-")})))))

(defn capitalize-first [s]
  (->> ((juxt (comp string/upper-case first)
              (comp string/join rest)) s)
       (apply str)))

(defn keyword->display-str [k]
  (->> (string/split (name k) #"-")
       (map capitalize-first)
       (string/join " ")))

(defn title []
  (let [name (re-frame/subscribe [::subs/name])]
    [re-com/title
     :label (str "Hello from " @name)
     :level :level1]))

(def floors
  (->> (sort second db/floor->depth)
       (map first)
       (map (fn [f] {:id f :label (->> (string/split (name f) #"-")
                                       (map capitalize-first)
                                       (string/join " "))}))
       (into [])))

(defn floor-header []
  (let [current-floor (re-frame/subscribe [::subs/current-floor])]
    [re-com/single-dropdown
     :choices floors
     :model current-floor
     :width "100%"
     :on-change #(re-frame/dispatch [::events/change-floor %])
     :class "mono floor-header"]))

(defn exit-index-label []
  (let [exit-index (re-frame/subscribe [::subs/exit-index])]
    [:p {:class "exit-index"} [:b (str "Exit Index: " @exit-index)]]))

(defn room->hiccup
  [{ri ::db/room-index n ::db/name d ::db/description
    s ::db/situation r ::db/roll}]
  [:div
   [:p {:style {:display "inline"}} [:b (str ri ". ")]
    (str (if (nil? s) "" (str "(" r "/" (::db/roll s) ") ")) n)]
   (when d [re-com/info-button
            :info [:span (text->hiccup d)]
            :style {:display "inline"}
            :position :right-below
            :width "250px"])     
   [:p {:style {:display "inline"}}
    (if (nil? s) "" (str " + " (::db/name s)))]
   (when (::db/description s)
     [re-com/info-button
      :info [:span (text->hiccup (::db/description s))]
      :style {:display "inline"}
      :position :right-below
      :width "250px"])])

(defn room-list []
  (let [room-list (re-frame/subscribe [::subs/room-list])]
    [re-com/scroller
     :style {:flex "1"}
     :child [:ul
             (for [room @room-list]
               ^{:key (::db/room-index room)}
               [:li (room->hiccup room)])]]))

(def adv->str (partial db/adv->str "None"))

(defn room-adv []
  (let [room-adv (re-frame/subscribe [::subs/room-adv])]
    [re-com/h-box
     :class "room-adv-overlay"
     :children
     [[:p {:style {:flex "1" :align-self "center"}
           :class "mono"}
       "Generate room" [:br] "advantage:"]
      [re-com/v-box
       :style {:align-self "center"}
       :children [[re-com/md-icon-button
                   :md-icon-name "zmdi-triangle-up"
                   :size :smaller
                   :on-click #(re-frame/dispatch
                               [::events/update-room-adv inc])]
                  [re-com/md-icon-button
                   :md-icon-name "zmdi-triangle-down"
                   :size :smaller
                   :on-click #(re-frame/dispatch
                               [::events/update-room-adv dec])]]]
      [:p
       {:class "mono"
        :style {:text-align "center"
                :align-self "center"
                :width "60px"}}
       (adv->str @room-adv)]]]))

(defn sidebar []
  [re-com/v-box
   :children [[floor-header]
              [exit-index-label]
              [re-com/line :color aqua-green]
              [room-list]
              [re-com/line :color aqua-green]
              [room-adv]]
   :class "sidebar"])

(defn explore-button
  [coord dir]
  [:div [:p {:class (str "mono unselectable explore-qm explore-" (name dir))}
         "?"]
   [:div {:class (str "explore-button explore-button-" (name dir))
          :on-click #(re-frame/dispatch
                      [::events/generate-room coord dir])}]])

(defn unexplored-room
  [coord {h ::db/hallways}]
  [:div {:class "tile"}
   [:div {:class "explore-container"}
    (for [dir h]
      ^{:key (str coord dir)}
      [explore-button coord dir])]])

(defn regenerate-button [coord from-dir]
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/regenerate-room coord from-dir])}
   "Regenerate"])

(defn encounter-button [coord room-map]
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/generate-encounter coord])}
   "Encounter"])

(defn rotate-button [coord dir]
  [:div
   {:class "menu-button"
    :style {:display "flex"
            :align-items "center"
            :justify-content "center"
            :flex-direction "row"}
    :on-click #(re-frame/dispatch [::events/rotate-room coord dir])}
   [:p {:style {:align-self "center"
                :margin "0"}}  "Rotate"]
   [re-com/gap :size "1"]
   [re-com/md-icon-button
    :style {:align-self "center"}
    :disabled? true
    :md-icon-name (case dir ::db/cw "zmdi-rotate-cw"
                        ::db/ccw "zmdi-rotate-ccw")]])

(defn force-shop-button [coord]
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/force-shop coord])}
   "Force Shop"])

(defn force-shrine-button [coord]
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/force-shrine coord])}
   "Force Shrine"])

(defn expandable [label component]
  (let [expanded? (re-frame/subscribe [::subs/expanded-notes?])]
    [re-com/v-box
     :width "100%"
     :children
     [[:div {:class "menu-button"
             :on-click #(re-frame/dispatch [::events/toggle-expanded-notes])
             :style {:display "flex"
                     :align-items "center"
                     :justify-content "center"
                     :flex-direction "row"}}
       [:p {:style {:align-self "center" :margin "0"}} label]
       [re-com/md-icon-button
        :style {:align-self "center"}
        :size :larger
        :md-icon-name (if @expanded?
                        "zmdi-chevron-down"
                        "zmdi-chevron-right")
        :disabled? true]]
      [re-com/box
       :class "collapsible"
       :width "100%"
       :style {:display (if @expanded? "block" "none")}
       :child component]]]))

(defn room-notes [coord]
  (let [notes (re-frame/subscribe [::subs/notes coord])]
    [expandable "Room Notes"
     [re-com/input-textarea
      :class "montserrat"
      :style {:resize "none"
              :width "100%"
              :height "100px"
              :font-weight "bold"}
      :model notes
      :on-change #(re-frame/dispatch [::events/update-notes coord %])
      :change-on-blur? true]]))

(defn room-menu [coord room-map]
  (let [showing-coord (re-frame/subscribe [::subs/showing-coord])
        showing? (reaction (= coord @showing-coord))]
    [re-com/popover-anchor-wrapper
     :showing? showing?
     :class "button-container unselectable"
     :style {:pointer-events "auto"}
     :position :left-below
     :anchor [re-com/md-icon-button
              :md-icon-name "zmdi-more-vert"
              :on-click #(if @showing?
                           (re-frame/dispatch [::events/hide-room-menu])
                           (re-frame/dispatch [::events/show-room-menu coord]))
              :class "room-menu"
              :size :smaller]
     :popover [re-com/popover-content-wrapper
               :on-cancel #(re-frame/dispatch [::events/hide-room-menu])
               :no-clip? true
               :body [re-com/v-box
                      :children [(when (::db/from-dir room-map)
                                   [regenerate-button
                                    coord (::db/from-dir room-map)])
                                 [encounter-button coord room-map]
                                 [force-shop-button coord]
                                 [force-shrine-button coord]
                                 [room-notes coord]
                                 [rotate-button coord ::db/cw]
                                 [rotate-button coord ::db/ccw]]]]]))

(defn room-cell
  [coord room-map]
  (let [current-floor (re-frame/subscribe [::subs/current-floor])
        current-room (re-frame/subscribe [::subs/current-room])]
    [re-com/v-box
     :class "room cell"
     :children [[:p {:class "mono room-text unselectable"
                     :style {:align-self "center"
                             :pointer-events "auto"
                             :font-size
                             (if (> 10 (::db/room-index room-map))
                               "30px" "25px")}
                     :on-click #(re-frame/dispatch
                                 [::events/current-room @current-floor coord])}
                 (gstring/format
                  (if (and (= coord (::db/coord @current-room))
                           (= @current-floor (::db/floor @current-room)))
                    "[%d]" "%d")
                  (::db/room-index room-map))]
                [room-menu coord room-map]]]))

(defn hallway
  [dir secret?]
  [:svg {:class dir}
   [:line
    (merge (case dir
             :north {:x1 0 :y1 0 :x2 0 :y2 "3em"}
             :south {:x1 0 :y1 0 :x2 0 :y2 "3em"}
             :east {:x1 0 :y1 0 :x2 "3em" :y2 0}
             :west {:x1 0 :y1 0 :x2 "3em" :y2 0})
           {:style
            (merge
             (if secret? {:stroke-dasharray "5,5"} {})
             {:stroke "rgb(36,36,36)"
              :stroke-width "2"})})]])

(defn explored-room
  [coord room-map]
  [:div {:class "tile"}
   (concat [^{:key (str coord)} [room-cell coord room-map]]
           (for [h (::db/hallways room-map)]
             ^{:key (str coord h)} [hallway h false])
           (for [sh (::db/secret-hallways room-map)]
             ^{:key (str coord sh)} [hallway sh true]))])

(defn room-panel
  [coord floor-map]
  (let [room-map (get floor-map coord)]
    (case (::db/id room-map)
      ::db/empty [:div {:class "tile empty"}]
      ::db/unexplored [unexplored-room coord room-map]
      [explored-room coord room-map])))

(defn map-column
  [column-index floor-map]
  (let [{:keys [max-y]} (db/get-map-bounds floor-map)]
    [re-com/v-box
     :children
     (for [y (range 0 (inc max-y))]
       ^{:key y}
       [room-panel [column-index y] floor-map])]))

(defn map-panel []
  (let [floor-map (re-frame/subscribe [::subs/floor-map])
        {:keys [max-x min-x]} (db/get-map-bounds @floor-map)]
    [re-com/h-box
     :children [(for [x (range min-x (inc max-x))]
                  ^{:key x}
                  [map-column x @floor-map])]]))

(defn loot-button []
  [:div
   {:class "menu-button"
    :on-click #(re-frame/dispatch [::events/generate-loot])}
   "Loot!"])

(defn slugs-button []
  [:div
   {:class "menu-button"
    :on-click #(re-frame/dispatch [::events/show-slugs])}
   "Slugs"])

(defn slugs-header []
  [re-com/h-box
   :class "slug-row slug-header"
   :children [[:span {:class "slug-left"} [:b "Color"]]
              [re-com/line :size "2px" :color aqua-green]
              [:span {:class "slug-right"} [:b "Effect"]]]])

(defn slug-row [idx [color effect]]
  [re-com/h-box
   :class (str "slug-row "
               (if (even? idx) "slug-row-even" "slug-row-odd"))
   :children [[:span {:class "slug-left"} color]
              [re-com/line :size "2px" :color aqua-green]
              [:span {:class "slug-right"} effect]]])

(defn slug-table []
  (let [show-slugs (re-frame/subscribe [::subs/show-slugs])
        slug-map (re-frame/subscribe [::subs/slug-map])]
    (when @show-slugs
      [re-com/modal-panel
       :class "slug-panel"
       :backdrop-on-click #(re-frame/dispatch [::events/hide-slugs])
       :child [re-com/v-box
               :children [[slugs-header]
                          (for [[idx slug-mapping]
                                (zipmap (range) @slug-map)]
                            ^{:key idx} [slug-row idx slug-mapping])]]])))

(defn generate-button []
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/show-generate-dialog])}
   "Generate..."])

(defn history-button []
  [:div
   {:class "menu-button"
    :style {:display "flex"
            :align-items "center"
            :justify-content "center"
            :flex-direction "row"}
    :on-click #(re-frame/dispatch [::events/show-history])}
   [:p {:style {:margin "0"}} "History"]
   [re-com/md-icon-button
    :md-icon-name "zmdi-time-restore"
    :disabled? true]])

(defn reset-button []
  [:div {:class "menu-button"
         :on-click #(re-frame/dispatch [::events/show-reset-dialog])}
   "Reset All"])

(defn new-character-button []
  [:a {:href (router/url-for :new-character)
       :style {:color "inherit"
               :text-decoration "none"}
       :class "menu-button"} "New Character"])

(defn source-link []
  [:a {:href "https://github.com/brstf/shifting-sands"
       :style {:color "inherit"
               :text-decoration "none"}
       :class "menu-button"} "View Source"
   [re-com/md-icon-button
    :md-icon-name "zmdi-github"
    :disabled? true]])

(defn button-overlay []
  (let [showing? (reagent/atom false)]
    [re-com/popover-anchor-wrapper
     :showing? showing?
     :class "button-container"
     :position :left-below
     :anchor [re-com/md-icon-button
              :style {:padding "10px"}
              :md-icon-name "zmdi-menu"
              :on-click #(swap! showing? not)
              :size :regular]
     :popover [re-com/popover-content-wrapper
               :on-cancel #(swap! showing? not)
               :body [re-com/v-box
                      :class "menu"
                      :children [[loot-button]
                                 [slugs-button]
                                 [generate-button]
                                 [history-button]
                                 [new-character-button]
                                 [reset-button]
                                 [source-link]]]]]))

(defn right-panel []
  [re-com/box
   :size "auto"
   :style {:height "auto"
           :width "100%"
           :overflow "auto"}
   :child [re-com/h-box
           :justify :center
           :class "main-panel"
           :children [[map-panel]
                      [button-overlay]]]])

(def generate-options
  (->> (db/get-table-names)
       (map-indexed
        (fn [idx p]
          {:id idx
           :path p
           :label (string/join " - " (map keyword->display-str p))}))))

(def adv-map
  [{:id 0 :value -3 :label "[---]"}
   {:id 1 :value -2 :label "[--]"}
   {:id 2 :value -1 :label "[-]"}
   {:id 3 :value 0 :label "None"}
   {:id 4 :value 1 :label "[+]"}
   {:id 5 :value 2 :label "[++]"}
   {:id 6 :value 3 :label "[+++]"}])

(defn generate->hiccup
  [{name ::db/name roll ::db/roll description ::db/description}]
  [re-com/v-box
   :children [(when name [re-com/title :level :level2 :label name])
              (when description [:p {:style {:class "montserrat"}}
                                 [:b (text->hiccup description)]])
              (when roll [:p {:style {:class "montserrat"}}
                          [:b (str "Roll: " roll)]])]])

(defn generate-dialog [generate-choice advantage-choice]
  (let [disable-button? (reaction (nil? @generate-choice))
        generate-result (re-frame/subscribe [::subs/generate-result])]
    [re-com/v-box
     :width "600px"
     :children
     [[re-com/single-dropdown
       :choices generate-options
       :model generate-choice
       :width "100%"
       :on-change #(reset! generate-choice %)
       :placeholder "Select Object To Generate..."
       :style {:padding-bottom "10px"}
       :class "montserrat generate-dropdown"]
      [re-com/h-box
       :style {:align-content "center"
               :align-items "center"
               :padding-bottom "10px"}
       :children [[re-com/label
                   :label [:b "Advantage/Disadvantage:"]
                   :style {:text-align "center"
                           :align-self "center"
                           :font-size "15px"
                           :padding-right "10px"}
                   :class "montserrat"]
                  [re-com/single-dropdown
                   :choices adv-map
                   :model advantage-choice
                   :class "montserrat"
                   :on-change #(reset! advantage-choice %)]]]
      [re-com/h-box
       :width "100%"
       :justify :end
       :children [[re-com/button
                   :label [:b "Generate"]
                   :style {:width "120px"}
                   :disabled? disable-button?
                   :on-click #(re-frame/dispatch
                               [::events/generate-generic
                                (:path (nth generate-options @generate-choice))
                                (:value (nth adv-map @advantage-choice))])
                   :class "montserrat button"]]]
      (when @generate-result
        [re-com/line :color aqua-green
         :style {:margin-top "10px"
                 :margin-bottom "10px"}])
      (when @generate-result (generate->hiccup @generate-result))]]))

(defn reset-dialog []
  (let [show-reset-dialog? (re-frame/subscribe [::subs/show-reset-dialog?])
        reset-slugs? (reagent/atom true)]
    (when @show-reset-dialog?
      [re-com/modal-panel
       :backdrop-on-click #(re-frame/dispatch [::events/hide-reset-dialog])
       :child
       [re-com/v-box
        :children
        [[:p {:class "modal-result"}
          [:b "Reset all floors and start a new generation?"]]
         [re-com/h-box
          :children [[:p [:b "Reset slugs?"]]
                     [re-com/gap :size "10px"]
                     [re-com/checkbox
                      :model reset-slugs?
                      :on-change #(reset! reset-slugs? %)]]]
         [re-com/h-box
          :justify :end
          :children
          [[re-com/button
            :label "Reset"
            :on-click #(re-frame/dispatch [::events/reset-all @reset-slugs?])
            :class "button"]]]]]])))

(defn history-dialog []
  (let [history (re-frame/subscribe [::subs/history])
        show-history? (re-frame/subscribe [::subs/show-history?])
        formatter (time-format/formatter "yyyyMMdd hh:mm")]
    (when @show-history?
      [re-com/modal-panel
       :class "history-panel"
       :backdrop-on-click #(re-frame/dispatch [::events/hide-history])
       :child
       [:table {:id "history"}
        [:thead
         [:tr
          [:th [:p {:class "header"} [:b "Timestamp"]]]
          [:th [:p {:class "header"} [:b "Floor"]]]
          [:th [:p {:class "header"} [:b "Room" [:br] "Index"]]]
          [:th [:p {:class "header"} [:b "Description"]]]]]
        [:tbody
         (for [[idx {description ::db/description
                     room-index ::db/room-index
                     floor ::db/floor time ::db/time}]
               (map-indexed (comp vec list) @history)]
           ^{:key idx} [:tr
                        [:td (time-format/unparse-local-date
                              formatter
                              (time/to-default-time-zone time))]
                        [:td (keyword->display-str floor)]
                        [:td {:style {:text-align "center"}}
                         (str room-index)]
                        [:td {:class "description"}
                         (text->hiccup description)]])]]])))

(defn shifting-sands-panel []
  (let [modal-result (re-frame/subscribe [::subs/modal-result])
        show-generate? (re-frame/subscribe [::subs/show-generate-dialog?])
        generate-choice (reagent/atom nil)
        advantage-choice (reagent/atom 3)]
    [re-com/v-box
     :height "100%"
     :style {:overflow "hidden"}
     :children
     [[re-com/h-box
       :height "100%"
       :style {:flex-direction "row-reverse"}
       :children
       [[sidebar]
        [re-com/line :color aqua-green]
        [right-panel]
        [slug-table]
        (when @show-generate?
          [re-com/modal-panel
           :backdrop-on-click
           #(do (reset! generate-choice 0)
                (reset! advantage-choice 3)
                (re-frame/dispatch [::events/clear-generate-result])
                (re-frame/dispatch [::events/hide-generate-dialog]))
           :child [generate-dialog generate-choice advantage-choice]])
        (when @modal-result
          [re-com/modal-panel
           :backdrop-on-click #(re-frame/dispatch [::events/clear-modal-result])
           :child [:p {:class "modal-result"}
                   (text->hiccup @modal-result)]])
        [reset-dialog]
        [history-dialog]]]]]))

(def starting-character-keys (concat db/character-traits
                                     db/starting-equipment))

(defn bonus->ability-str [b]
  (str (+ 10 b) "/+" b))

(defn stat-row
  [stat [s1 s2 s3]]
  [re-com/h-box
   :children
   [[:span {:class "stat-left mono"} (name stat)]
    [:span {:class "stat-right montserrat"} (str s1)]
    [:span {:class "stat-right montserrat"} (str s2)]
    [:span {:class "stat-right montserrat"} (str s3)]]])

(defn stat-block
  [stats]
  [:div {:class "character-right"
         :style {:display "block"}}
   [re-com/v-box
    :width "auto"
    :children
    [[re-com/h-box
      :style {:align-contents "center"}
      :children
      [[:span {:class "stat-left stats-header"} "Prev ad:"]
       [:span {:class "stat-right stats-header"} "11-14"]
       [:span {:class "stat-right stats-header"} "15-18"]
       [:span {:class "stat-right stats-header"} "19-20"]]]
     (for [stat db/stats]
       ^{:key (str stat)} [stat-row stat (get stats stat)])]]])

(defn stats-row
  [stats]
  [re-com/h-box
   :class "character-odd"
   :children [[:span {:class "mono character-left"} "Ability Bonuses"]
              [re-com/line :size "2px" :color aqua-green]
              [stat-block stats]]])

(defn character-row
  [even? character k]
  [re-com/h-box
   :class (if even? "character-even" "character-odd")
   :children [[:span {:class "mono character-left"} (keyword->display-str k)]
              [re-com/line :size "2px" :color aqua-green]
              [:span {:class "montserrat character-right"}
               (text->hiccup (get character k))]]])

(defn new-character-panel []
  (let [character @(re-frame/subscribe [::subs/new-character])]
    [re-com/v-box
     :children
     [[re-com/h-box
       :class "character-header"
       :width "100%"
       :style {:align-contents "center"}
       :children
       [[re-com/title
         :class "character-header"
         :level :level2
         :label "New Character"]
        [re-com/gap :size "1"]
        [re-com/md-icon-button
         :class "button"
         :size :larger
         :style {:align-self "center"}
         :md-icon-name "zmdi-refresh"
         :on-click #(re-frame/dispatch [::events/generate-new-character])]
        [re-com/gap :size "20px"]]]
      [re-com/gap :size "5px"]
      [stats-row (::db/stats character)]
      (for [[idx trait] (map-indexed vector starting-character-keys)]
        ^{:key idx} [character-row (even? idx) character trait])]]))

(defn not-found-panel []
  [re-com/v-box
   :children [[re-com/title
               :class "mono"
               :style {:padding-left "20px"}
               :level :level1
               :label "Not Found"]]])

(defn pages [page-name]
  (case page-name
    :home [shifting-sands-panel]
    :new-character [new-character-panel]
    :not-found [not-found-panel]))

(defn main-panel []
  (let [active-page @(re-frame/subscribe [::subs/active-page])]
    (pages active-page)))
