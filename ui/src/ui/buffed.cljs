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
            [om-bootstrap.input :as i]
            [om-bootstrap.grid :as g]
            [ajax.core :refer [GET POST]]
            [clojure.string :as str]))

(defn main []
  (let [cmd-chan (chan)
        default-record-limit 10
        default-pages-limit 10
        default-page 0]

    (enable-console-print!)
    ;;(.log js/console (str response))

    (defonce application-state (atom {:view-mode :initial
                                      :feed
                                      {:data []
                                       :paginator
                                       {:currentPage 0
                                        :pagesLimit 10
                                        :recordLimit 10
                                        :pagesNumber nil
                                        :filter-value ""}}
                                      :newPublicName ""
                                      :publicList
                                      {:data []}}))

    ;; Views
    (defmulti main-view (fn [app-state owner] (:view-mode app-state)))

    (defmethod main-view :feed [app-state owner] (feed-view app-state owner))
    (defmethod main-view :public-list [app-state owner] (public-list-view app-state owner))
    (defmethod main-view :initial [app-state owner]
      (om/update! app-state [:view-mode] :feed)
      (show-feed (get-in @app-state [:feed :paginator :currentPage])
                 (get-in @app-state [:feed :paginator :recordLimit])
                 (get-in @app-state [:feed :paginator :filter-value])
                 (:cmd-chan (om/get-shared owner)))
      (feed-view app-state owner))

    (defmethod main-view :default [app-state owner]
      (throw (IllegalArgumentException.
              (str "No render method found for view mode " (get-in @app-state [:view-mode])))))


    (defn text-filter [app-state owner]
      (defn apply-text-filter []
        (let [text-filter-value (.-value (om/get-node owner "text-filter-value"))]
          (when text-filter-value
            (put! (:cmd-chan (om/get-shared owner)) {:cmd-type :ct-apply-filter :cmd-params text-filter-value}))))
      (g/grid {}
       (g/row {}
        (g/col {:md 8} (i/input {:type "text" :addon-before "Фильтр" :ref "text-filter-value"}))
        (g/col {:md 4} (b/button {:bs-size "medium" :on-click #(apply-text-filter)} "Применить")))))


    (defn paginator [app-state cmd-chan]
      (let [currentPage (get-in @app-state [:feed :paginator :currentPage])
            pagesNumber (get-in @app-state [:feed :paginator :pagesNumber])
            pagesLimit (get-in @app-state [:feed :paginator :pagesLimit])
            currentOffset (* pagesLimit (quot currentPage pagesLimit))
            pagesToShow (min pagesLimit (- pagesNumber currentOffset))]

        (defn updatePaginator [pageNumber] (put! cmd-chan {:cmd-type :ct-paginator-updated :cmd-params pageNumber}))
        (defn btnClassName [pageNumber] (if (= pageNumber currentPage) "active" ""))
        (apply pg/pagination {} (concat
                                 [(pg/page {:onClick (fn [_] (updatePaginator 0))} ".")]
                                 [(pg/previous {:onClick (fn [_] (updatePaginator (max 0 (- currentOffset pagesLimit))))})]
                                 (map (fn [pageNumber]
                                        (pg/page {:className (btnClassName pageNumber) :onClick (fn [_] (updatePaginator pageNumber))} (inc pageNumber)))
                                      (range currentOffset (+ currentOffset pagesToShow)))
                                 [(pg/next {:onClick (fn [_] (updatePaginator (min pagesNumber (+ pagesLimit currentOffset))))})]
                                 [(pg/page {:onClick (fn [_] (updatePaginator (* pagesLimit (quot pagesNumber pagesLimit))))} ".")]))))

    (defn feed-view [app-state owner]
      (let [cmd-chan (:cmd-chan (om/get-shared owner))]
        (p/panel {}
                 (text-filter app-state owner)
                 (paginator app-state (:cmd-chan (om/get-shared owner)))
                 (apply dom/ul #js {:className "list-group"}
                        (map render-feed-item (get-in @app-state [:feed :data]) (cycle ["#ff0" "#fff"]) (cycle [cmd-chan]))))))

    (defn public-list-view [app-state owner]
      (defn createNewPublic []
        (let [new-public-name (.-value (om/get-node owner "new-public-name"))]
          (when new-public-name
            (put! (:cmd-chan (om/get-shared owner)) {:cmd-type :ct-create-new-public :cmd-params new-public-name}))))

      (p/panel {}
               (p/panel {:header "Добавить паблик"}
                        (i/input {:type "text" :addon-before "http://vk.com/" :ref "new-public-name"})
                        (b/button {:bs-size "xsmall" :onClick #(createNewPublic)} "Ok"))

               (p/panel {:header "Список пабликов"}
                        (apply dom/ul #js {:className "list-group"}
                               (map stripe
                                    (map publicListItem (get-in @app-state [:publicList :data]))
                                    (cycle ["#ff0" "#fdf"]))))))

    ;; UI functions
    (defn publicListItem [publicItem]
      (let [publicId (:publicId publicItem)
            publicName (:publicName publicItem)]
        (dom/div nil
                 (dom/span nil publicName)
                 (b/button {:bs-size "xsmall" :className "pull-right" :onClick #(fetch-feed (:publicId publicItem))} "Обновить"))))

    (defn stripe [text bgc]
      (let [st #js {:backgroundColor bgc}]
        (dom/li #js {:className "list-group-item" :style st} text)))

    (defn render-feed-item [data bgc cmd-chan]
      (let [st #js {:backgroundColor bgc}
            postId (get data "postId")
            postText (get data "postText")
            publicId (get data "publicId")
            postImages (str/split (get data "postImages") "<|>")]

        (defn hide-post [publicId postId]
          (put! cmd-chan {:cmd-type :ct-hide-post :cmd-params {:publicId publicId :postId postId} }))

        (.log js/console (str postImages))

        (dom/li #js {:className "list-group-item" :style st} nil
                (b/button-group {}
                                (b/button {:bs-style "link" :bs-size "xsmall"
                                           :href (str "http://vk.com/wall" publicId "_" postId)
                                           :target "_blank"} "Link")
                                (b/button {:bs-size "xsmall" :on-click #(download-post publicId postId)} "Загрузить")
                                (b/button {:bs-size "xsmall" :on-click #(hide-post publicId postId)} "Скрыть"))
                (dom/div #js {:dangerouslySetInnerHTML #js {:__html postText}} nil)
                (dom/div nil
                 (map (fn [image] (dom/img #js {:src image :width 100})) postImages)) )))

    ;; API CALLS
    (defn show-feed [currentPage recordLimit filterValue cmd-chan]
      (GET (str "/api/feed?currentPage=" currentPage "&limit=" recordLimit "&filterValue=" filterValue)
           {:handler (fn [r] (put! cmd-chan {:cmd-type :ct-updated-feed :cmd-params r}))}))

    (defn show-public-list [cmd-chan]
      (GET "/api/publicList" {:response-format :json
                              :keywords? true
                              :handler (fn [r] (.log js/console (str r))  (put! cmd-chan {:cmd-type :ct-show-public-list :cmd-params r}))}))

    (defn fetch-feed [publicId]
      (GET (str "/api/fetch?publicId=" publicId)))

    (defn download-post [publicId postId]
      (GET (str "/api/download-post?publicId=" publicId "&postId=" postId)))

    (defn req-hide-post [publicId postId]
      (GET (str "/api/hide-post?publicId=" publicId "&postId=" postId)))

    (defn create-new-public [newPublicName cmd-chan]
      (GET (str "/api/create-new-public?name=" newPublicName) {:handler (fn [r] (show-public-list cmd-chan))}))

    ;; UI COMMAND HANDLERS
    (defmulti process-command (fn [cmd app-state cmd-chan] (:cmd-type cmd)))
    (defmethod process-command :ct-updated-feed [cmd app-state cmd-chan]
      (om/update! app-state [:feed :data] (get-in cmd [:cmd-params "posts"]))
      (om/update! app-state [:feed :paginator :pagesNumber] (get-in cmd  [:cmd-params "pagesNumber"]))
      (om/update! app-state [:view-mode] :feed))

    (defmethod process-command :ct-paginator-updated [cmd app-state cmd-chan]
      (om/update! app-state [:feed :paginator :currentPage] (:cmd-params cmd))
      (show-feed (:cmd-params cmd)
                 (get-in @app-state [:feed :paginator :recordLimit])
                 (get-in @app-state [:feed :paginator :filter-value])
                 cmd-chan))

    (defmethod process-command :ct-show-public-list [cmd app-state cmd-chan]
      (om/update! app-state [:publicList :data] (:cmd-params cmd))
      (om/update! app-state [:view-mode] :public-list))

    (defmethod process-command :ct-create-new-public [cmd app-state cmd-chan]
      (create-new-public (:cmd-params cmd) cmd-chan))

    (defmethod process-command :ct-apply-filter [cmd app-state cmd-chan]
      (om/update! app-state [:feed :paginator :filter-value] (:cmd-params cmd))
      (show-feed (get-in @app-state [:feed :paginator :currentPage])
                 (get-in @app-state [:feed :paginator :recordLimit])
                 (:cmd-params cmd)
                 cmd-chan))

    (defmethod process-command :ct-hide-post [cmd app-state cmd-chan]
      (let [publicId (get-in cmd [:cmd-params :publicId])
            postId (get-in cmd [:cmd-params :postId])
            data (get-in @app-state [:feed :data])
            new-data (filter (fn [post] (or (not= (get post "postId") postId) (not= (get post "publicId") publicId))) data)]
        (om/update! app-state [:feed :data] new-data)
        (req-hide-post publicId postId)))

    ;; RENDER FN
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
                                     (get-in @app-state [:feed :paginator :filter-value])
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
