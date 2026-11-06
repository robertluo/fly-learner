(ns learner.ui.core.history
  "URL <-> state (pushState). Routes:
    /            home (sources)
    /n/<uuid>    a node
    /k/<keyword> nodes tagged with a keyword
  The server serves index.html for these paths (learner.server.system)."
  (:require [clojure.string :as str]))

(defn- url-encode [s]
  ;; URLEncoder writes spaces as "+"; encodeURIComponent never emits "+"
  (str/replace #?(:clj  (java.net.URLEncoder/encode ^String s "UTF-8")
                  :cljs (js/encodeURIComponent s))
               "+" "%20"))

(defn- url-decode [s]
  #?(:clj  (java.net.URLDecoder/decode ^String s "UTF-8")
     :cljs (js/decodeURIComponent s)))

(defn state->path [{:keys [view node-id keyword]}]
  (case view
    :node    (str "/n/" node-id)
    :keyword (str "/k/" (url-encode keyword))
    "/"))

(defn path->state [path]
  (or (when-let [[_ id] (re-matches #"/n/([0-9a-f-]{36})/?" path)]
        {:view :node :id (parse-uuid id)})
      (when-let [[_ kw] (re-matches #"/k/(.+?)/?" path)]
        {:view :keyword :keyword (url-decode kw)})
      {:view :home}))

#?(:cljs
   (defn current-path [] (.-pathname js/location)))

#?(:cljs
   (defn push-state!
     "pushState the URL for db, unless the browser is already there."
     [db]
     (let [path (state->path db)]
       (when (not= path (current-path))
         (.pushState js/history nil "" path)))))

#?(:cljs
   (defn replace-state! [db]
     (.replaceState js/history nil "" (state->path db))))

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
  (path->state "/whatever")) ;=> {:view :home}
