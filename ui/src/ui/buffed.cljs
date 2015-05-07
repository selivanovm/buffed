(ns ui.buffed
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-tools.dom :as d :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [om-bootstrap.nav :as n]
            [om-bootstrap.button :as b]
            [om-bootstrap.panel :as p]
            [om-bootstrap.pagination :as pg]
            [ajax.core :refer [GET POST]]))

(defn main []
  (let [cmd-chan (chan)]

    (enable-console-print!)
    ;;(.log js/console (str response))

    (defonce application-state (atom {:view-mode :initial
                                      :feed
                                      {:data []
                                       :paginator
                                       {:currentPage 0
                                        :pagesLimit 10
                                        :recordLimit 10
                                        :pagesNumber nil}}
                                      :publicList
                                      {:data []}}))

    ;; Views
    (defmulti main-view (fn [app-state owner] (:view-mode app-state)))

    (defmethod main-view :feed [app-state owner] (feed-view app-state owner))
    (defmethod main-view :public-list [app-state owner] (public-list-view app-state owner))
    (defmethod main-view :initial [app-state owner]
      (om/update! app-state [:view-mode] :feed)
      (show-feed (get-in @app-state [:feed :paginator :currentPage]) (get-in @app-state [:feed :paginator :recordLimit]) (:cmd-chan (om/get-shared owner)))
      (feed-view app-state owner))

    (defmethod main-view :default [app-state owner]
      (throw (IllegalArgumentException.
              (str "No render method found for view mode " (get-in @app-state [:view-mode])))))

    (defn paginator [app-state cmd-chan]
      (def currentPage (get-in @app-state [:feed :paginator :currentPage]))
      (def pagesNumber (get-in @app-state [:feed :paginator :pagesNumber]))
      (def pagesLimit (get-in @app-state [:feed :paginator :pagesLimit]))
      (def currentOffset (* pagesLimit (quot currentPage pagesLimit)))
      (def pagesToShow (min pagesLimit (- pagesNumber currentOffset)))
      (defn updatePaginator [pageNumber] (put! cmd-chan {:cmd-type :ct-paginator-updated :cmd-params pageNumber}))
      (defn btnClassName [pageNumber] (if (= pageNumber currentPage) "active" ""))
      (apply pg/pagination {} (concat
                               [(pg/page {:onClick (fn [_] (updatePaginator 0))} ".")]
                               [(pg/previous {:onClick (fn [_] (updatePaginator (max 0 (- currentOffset pagesLimit))))})]
                               (map (fn [pageNumber]
                                      (pg/page {:className (btnClassName pageNumber) :onClick (fn [_] (updatePaginator pageNumber))} (inc pageNumber)))
                                    (range currentOffset (+ currentOffset pagesToShow)))
                               [(pg/next {:onClick (fn [_] (updatePaginator (min pagesNumber (+ pagesLimit currentOffset))))})]
                               [(pg/page {:onClick (fn [_] (updatePaginator (* pagesLimit (quot pagesNumber pagesLimit))))} ".")])))

    (defn feed-view [app-state owner]
      (p/panel {}
               (paginator app-state (:cmd-chan (om/get-shared owner)))
               (apply dom/ul #js {:className "list-group"}
                      (map stripeHtml (get-in @app-state [:feed :data]) (cycle ["#ff0" "#fff"])))))

    (defn public-list-view [app-state owner]
      (p/panel {}
               (apply dom/ul #js {:className "list-group"}
                      (map stripe
                           (map publicListItem (get-in @app-state [:publicList :data]))
                           (cycle ["#ff0" "#fdf"])))))

    ;; UI functions
    (defn publicListItem [publicItem]
      (def publicId (:publicId publicItem))
      (def publicName (:publicName publicItem))
      (dom/div nil
               (.log js/console publicId)
               (dom/span nil publicName)
               (b/button {:bs-size "xsmall" :className "pull-right" :onClick #(fetch-feed (:publicId publicItem))} "Обновить")))

    (defn stripe [text bgc]
      (let [st #js {:backgroundColor bgc}]
        (dom/li #js {:className "list-group-item" :style st} text)))

    (defn stripeHtml [text bgc]
      (let [st #js {:backgroundColor bgc}]
        (dom/li #js {:className "list-group-item" :style st} nil
                (b/button-group {} (b/button {:bs-size "xsmall" :onClick ()} "Загрузить"))
                (dom/div #js {:dangerouslySetInnerHTML #js {:__html text}} nil))))

    ;; API CALLS
    (defn show-feed [currentPage recordLimit cmd-chan]
      (GET (str "/api/feed?currentPage=" currentPage "&limit=" recordLimit) {:handler (fn [r] (put! cmd-chan {:cmd-type :ct-updated-feed :cmd-params r}))}))

    (defn show-public-list [cmd-chan]
      (GET "/api/publicList" {:response-format :json
                              :keywords? true
                              :handler (fn [r]
                                         (.log js/console (str r))
                                         (put! cmd-chan {:cmd-type :ct-show-public-list :cmd-params r}))}))

    (defn fetch-feed [publicId]
      (GET (str "/api/fetch?publicId=" publicId)))

    ;; RENDER FN

    (defmulti process-command (fn [cmd app-state cmd-chan] (:cmd-type cmd)))
    (defmethod process-command :ct-updated-feed [cmd app-state cmd-chan]
      (om/update! app-state [:feed :data] (map #(% "postText") (get-in cmd [:cmd-params "posts"])))
      (om/update! app-state [:feed :paginator :pagesNumber] (get-in cmd  [:cmd-params "pagesNumber"]))
      (om/update! app-state [:view-mode] :feed))

    (defmethod process-command :ct-paginator-updated [cmd app-state cmd-chan]
      (om/update! app-state [:feed :paginator :currentPage] (:cmd-params cmd))
      (show-feed (:cmd-params cmd) (get-in @app-state [:feed :paginator :recordLimit]) cmd-chan))

    (defmethod process-command :ct-show-public-list [cmd app-state cmd-chan]
      (om/update! app-state [:publicList :data] (:cmd-params cmd))
      (om/update! app-state [:view-mode] :public-list))

    (om/root
     (fn [app-state owner]
       (reify
         om/IInitState
         (init-state [_] {})
         om/IDidMount
         (did-mount [_]
           (let [cmd-chan (:cmd-chan (om/get-shared owner))]
             (go
               (loop []
                 (let [cmd (<! cmd-chan)]
                   (process-command cmd app-state cmd-chan))
                 (recur)))))
         om/IRender
         (render [_]
           (n/navbar
            {:brand (d/a {:href "#"} "Буфет")}
            (n/nav
             {:collapsible? true}
             (n/nav-item {:id "btn-feed" :href "#"
                          :className (if (= (get-in @app-state [:view-mode]) :feed) "active" "inactive")
                          :onClick #(show-feed
                                     (get-in @app-state [:feed :paginator :currentPage])
                                     (get-in @app-state [:feed :paginator :recordLimit])
                                     (:cmd-chan (om/get-shared owner)))} "Лента")
             (n/nav-item {:id "btn-public" :href "#"
                          :className (if (= (get-in @app-state [:view-mode]) :public-list) "active" "inactive")
                          :onClick #(show-public-list (:cmd-chan (om/get-shared owner)))}
                         "Паблики"))
            (main-view app-state owner)))))

     application-state

     {:shared {:cmd-chan cmd-chan}
      :target (. js/document (getElementById "app"))})))

(main)
