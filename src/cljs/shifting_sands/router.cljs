(ns shifting-sands.router
  (:require [bidi.bidi :as bidi]
            [pushy.core :as pushy]
            [re-frame.core :as re-frame]
            [shifting-sands.events :as events]))

(def routes
  ["/" {"shifting-sands" :home
        "shifting-sands/" {"" :home
                           "new-character" :new-character
                           true :not-found}
        "shifting-sands-new-character" :new-character
        true :not-found}])

(def history
  (let [dispatch #(re-frame/dispatch [::events/set-active-page {:page (:handler %)}])
        match #(do (print %) (print (bidi/match-route routes %)) (bidi/match-route routes %))]
    (pushy/pushy dispatch match)))

(defn start! []
  (pushy/start! history))

(def url-for (partial bidi/path-for routes))

(defn set-token!
  [token]
  (pushy/set-token! history token))
