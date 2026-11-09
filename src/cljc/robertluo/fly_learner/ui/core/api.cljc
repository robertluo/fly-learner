(ns robertluo.fly-learner.ui.core.api
  "The backend as the UI sees it: the one namespace that knows how requests
  are spelled on the wire. Everything else speaks in requests, data naming
  what is wanted:

    (fetch :sources)                  a whole collection, or a value (:learning)
    (fetch :nodes {:node/id id})      one member, or a lookup's answer
                                      ({:search {:q ..}})
    (create :asks {:ask/question q})  a new member; the answer is what was made
    (delete :ideas {:idea/id id})     the answer is nil
    (batch {:sources (fetch :sources) ..})
                                      fetches at once; the answer is a map
                                      by the same names

  Today's format: lasagna-remote pull patterns (robertluo.fly-learner.server.api)
  posted as transit+json to `endpoint`. `pattern` spells a request, `answer`
  reads the server's reply back, `failure` its errors; `send!` (browser)
  does the round trip. Moving to another API format changes this namespace
  only. Files are served apart: `source-url`, `source-pages-url`."
  #?(:cljs (:require [cognitect.transit :as t])))

;;=============================================================================
;; Requests
;;=============================================================================

(defn fetch
  "Read collection (or value) `coll`; with `k`, the member or answer at `k`."
  ([coll] {:op :fetch :coll coll})
  ([coll k] {:op :fetch :coll coll :key k}))

(defn create [coll value] {:op :create :coll coll :value value})

(defn delete [coll k] {:op :delete :coll coll :key k})

(defn change "Rewrite member `k` of `coll` as `value`." [coll k value] {:op :update :coll coll :key k :value value})

(defn batch
  "Several fetches in one round trip, by name: {name request}."
  [named]
  {:op :batch :requests named})

(defn source-url
  "Where the original document of source `sid` is served."
  [sid]
  (str "/files/sources/" sid))

(defn source-pages-url
  "Where the pages of source `sid` are served as HTML (an EPUB's; a PDF's
  are in its document)."
  [sid]
  (str (source-url sid) "/pages"))

;;=============================================================================
;; Wire format: lasagna-remote pull patterns
;;=============================================================================

(def endpoint "/api")

(defn- var-of [nm] (symbol (str "?" (name nm))))
(defn- binding-of [nm] (symbol (name nm)))

(defn- fetch-pattern [{:keys [coll] :as req} nm]
  (if (contains? req :key)
    {coll {(:key req) (var-of nm)}}
    {coll (var-of nm)}))

(defn pattern
  "Request `req` as a pull pattern."
  [{:keys [op coll value requests] :as req}]
  (case op
    :fetch  (fetch-pattern req :value)
    :create {coll {nil value}}
    :delete {coll {(:key req) nil}}
    :update {coll {(:key req) value}}
    :batch  (do (assert (apply distinct? (map :coll (vals requests))) "one fetch per collection")
                (into {} (map (fn [[nm r]] (fetch-pattern r nm))) requests))))

(defn answer
  "What request `req` asked for, from the server's reply `bindings`."
  [{:keys [op coll requests]} bindings]
  (case op
    :fetch  (get bindings (binding-of :value))
    :create (get bindings (binding-of coll))
    :delete nil
    :update (get bindings (binding-of coll))
    :batch  (into {} (map (fn [[nm _]] [nm (get bindings (binding-of nm))])) requests)))

(defn failure
  "{:code :reason} of a failed reply: its first error, else the HTTP status."
  [body status]
  (or (first (:errors body)) {:code :unknown :reason (str "HTTP " status)}))

#?(:cljs
   (do
     (def ^:private writer (t/writer :json))
     (def ^:private reader (t/reader :json {:handlers {"u" (fn [s] (uuid s))}}))

     (defn send!
       "Send request `req`: (on-answer answer) when it succeeds, else
       (on-error {:code :reason})."
       [req on-answer on-error]
       (-> (js/fetch endpoint #js {:method  "POST"
                                   :headers #js {"Content-Type" "application/transit+json"
                                                 "Accept"       "application/transit+json"}
                                   :body    (t/write writer {:pattern (pattern req)})})
           (.then (fn [^js resp]
                    (-> (.text resp)
                        (.then (fn [text]
                                 (let [body (when (seq text) (t/read reader text))]
                                   (if (.-ok resp)
                                     (on-answer (answer req body))
                                     (on-error (failure body (.-status resp))))))))))
           (.catch (fn [e] (on-error {:code :network :reason (str e)})))))))

^:rct/test
(comment
  (def id #uuid "00000000-0000-0000-0000-000000000001")

  (= '{:sources ?value} (pattern (fetch :sources))) ;=> true
  (= {:nodes {{:node/id id} '?value}} (pattern (fetch :nodes {:node/id id}))) ;=> true
  (answer (fetch :nodes {:node/id id}) {'value {:node/id id}}) ;=> {:node/id #uuid "00000000-0000-0000-0000-000000000001"}

  (pattern (create :asks {:ask/question "Why?"})) ;=> {:asks {nil {:ask/question "Why?"}}}
  (answer (create :asks {:ask/question "Why?"}) {'asks {:ask/id id}}) ;=> {:ask/id #uuid "00000000-0000-0000-0000-000000000001"}

  (pattern (delete :ideas {:idea/id id})) ;=> {:ideas {{:idea/id #uuid "00000000-0000-0000-0000-000000000001"} nil}}
  (answer (delete :ideas {:idea/id id}) {'ideas true}) ;=> nil
  (pattern (change :ideas {:idea/id id} {:idea/text "x"}))
  ;=> {:ideas {{:idea/id #uuid "00000000-0000-0000-0000-000000000001"} {:idea/text "x"}}}
  (answer (change :ideas {:idea/id id} {}) {'ideas {:idea/id id}}) ;=> {:idea/id #uuid "00000000-0000-0000-0000-000000000001"}

  (def init (batch {:sources (fetch :sources) :universe (fetch :universe)}))
  (= '{:sources ?sources :universe ?universe} (pattern init)) ;=> true
  (answer init {'sources [] 'universe {:clusters []}}) ;=> {:sources [] :universe {:clusters []}}

  (failure {:errors [{:code :forbidden}]} 403) ;=> {:code :forbidden}
  (failure nil 502) ;=> {:code :unknown :reason "HTTP 502"}
  )
