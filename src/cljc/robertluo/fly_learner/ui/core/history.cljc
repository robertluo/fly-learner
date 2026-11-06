(ns robertluo.fly-learner.ui.core.history
  "URL <-> state (pushState). Routes:
    /            home (sources)
    /n/<uuid>    a node (?page=N: its source on page N, not the node's own)
    /k/<keyword> nodes tagged with a keyword
    /a/<uuid>    an answer from the knowledge base
    /c           every concept
    /c/<uuid>    a concept
    /q           study: what you know, quizzes
  The server serves index.html for these paths (robertluo.fly-learner.server.system)."
  (:require [clojure.string :as str]))

(defn- url-encode [s]
  ;; URLEncoder writes spaces as "+"; encodeURIComponent never emits "+"
  (str/replace #?(:clj  (java.net.URLEncoder/encode ^String s "UTF-8")
                  :cljs (js/encodeURIComponent s))
               "+" "%20"))

(defn- url-decode [s]
  #?(:clj  (java.net.URLDecoder/decode ^String s "UTF-8")
     :cljs (js/decodeURIComponent s)))

(defn state->path
  "URL for db; nil while a question waits for its answer (no URL yet)."
  [{:keys [view node-id keyword ask-id concept-id pdf]}]
  (case view
    :node     (str "/n/" node-id (when-let [p (:page pdf)] (str "?page=" p)))
    :concept  (str "/c/" concept-id)
    :concepts "/c"
    :study    "/q"
    :keyword (str "/k/" (url-encode keyword))
    :ask     (when ask-id (str "/a/" ask-id))
    "/"))

(defn path->state [path]
  (or (when-let [[_ id page] (re-matches #"/n/([0-9a-f-]{36})/?(?:\?page=(\d+))?" path)]
        (cond-> {:view :node :id (parse-uuid id)}
          page (assoc :page (parse-long page))))
      (when-let [[_ id] (re-matches #"/a/([0-9a-f-]{36})/?" path)]
        {:view :ask :id (parse-uuid id)})
      (when-let [[_ id] (re-matches #"/c/([0-9a-f-]{36})/?" path)]
        {:view :concept :id (parse-uuid id)})
      (when (re-matches #"/c/?" path) {:view :concepts})
      (when (re-matches #"/q/?" path) {:view :study})
      (when-let [[_ kw] (re-matches #"/k/(.+?)/?" path)]
        {:view :keyword :keyword (url-decode kw)})
      {:view :home}))

#?(:cljs
   (defn current-path [] (str (.-pathname js/location) (.-search js/location))))

#?(:cljs
   (defn push-state!
     "pushState the URL for db, unless the browser is already there."
     [db]
     (let [path (state->path db)]
       (when (and path (not= path (current-path)))
         (.pushState js/history nil "" path)))))

#?(:cljs
   (defn replace-state! [db]
     (when-let [path (state->path db)]
       (.replaceState js/history nil "" path))))

#?(:cljs
   (defn on-popstate!
     "popstate listener: calls (f parsed-route)."
     [f _event]
     (f (path->state (current-path)))))

^:rct/test
(comment
  (state->path {:view :home}) ;=> "/"
  (state->path {:view :node :node-id #uuid "00000000-0000-0000-0000-000000000001"})
  ;=> "/n/00000000-0000-0000-0000-000000000001"
  (state->path {:view :keyword :keyword "system fω"}) ;=> "/k/system%20f%CF%89"

  (path->state "/n/00000000-0000-0000-0000-000000000001")
  ;=> {:view :node :id #uuid "00000000-0000-0000-0000-000000000001"}
  (path->state "/k/system%20f%CF%89") ;=> {:view :keyword :keyword "system fω"}
  ;; the source on another page than the node's
  (state->path {:view :node :node-id #uuid "00000000-0000-0000-0000-000000000001" :pdf {:page 88}})
  ;=> "/n/00000000-0000-0000-0000-000000000001?page=88"
  (path->state "/n/00000000-0000-0000-0000-000000000001?page=88")
  ;=> {:view :node :id #uuid "00000000-0000-0000-0000-000000000001" :page 88}
  (path->state "/a/00000000-0000-0000-0000-000000000001")
  ;=> {:view :ask :id #uuid "00000000-0000-0000-0000-000000000001"}
  (state->path {:view :ask :ask-id nil}) ;=> nil
  (state->path {:view :concepts}) ;=> "/c"
  (path->state "/c/") ;=> {:view :concepts}
  (path->state "/c/00000000-0000-0000-0000-000000000001")
  ;=> {:view :concept :id #uuid "00000000-0000-0000-0000-000000000001"}
  (path->state "/whatever")) ;=> {:view :home}
