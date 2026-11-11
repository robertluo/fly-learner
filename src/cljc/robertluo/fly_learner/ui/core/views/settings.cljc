(ns robertluo.fly-learner.ui.core.views.settings
  "Settings (/s). Adding sources: files dropped (or chosen) are sent and
  read, each with what it would make and a rough cost; what is checked is
  ingested in the background, its progress shown until done. The library.
  The models: each tier's provider and model, the providers (presets for
  local servers, keys, what each serves), tried with a tiny request, in
  force once saved. Advanced: the prompts given to the models, each
  editable and back to its default in one click."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.template :as t]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.nav :as nav]))

(def ^:private accepted ".pdf,.epub,.md,.markdown,.zip")

(defn- upload-effects
  "Effects sending `files` (js Files) to be read."
  [files]
  (let [pairs (mapv (fn [f] [(random-uuid) f]) files)]
    (when (seq pairs)
      {:db     (fn [d] (reduce (fn [d [id f]] (db/upload-started d id (.-name f))) d pairs))
       :upload (mapv (fn [[id f]] {:id id :file f}) pairs)})))

(defn- files-of
  "The files of a drop or a file input's change."
  #?(:cljs [^js e] :clj [_e])
  #?(:cljs (array-seq (or (some-> e .-dataTransfer .-files) (.. e -target -files)))
     :clj  []))

(defn- dropzone [{:keys [drag?]} dispatch!]
  [:label.dropzone
   {:class (when drag? "over")
    :on    {:dragover  (fn [e] (.preventDefault e) (when-not drag? (dispatch! {:db #(db/drag-over % true)})))
            :dragleave (fn [_] (dispatch! {:db #(db/drag-over % false)}))
            :drop      (fn [e] (.preventDefault e)
                         (dispatch! (or (upload-effects (files-of e)) {:db #(db/drag-over % false)})))}}
   [:input {:type "file" :multiple true :accept accepted
            :on {:change (fn [e] (some-> (upload-effects (files-of e)) dispatch!)
                           #?(:cljs (set! (.. e -target -value) "")))}}]
   [:strong "Drop files here, or click to choose"]
   [:span.muted "PDF, EPUB, Markdown, or a zip of them. The Markdown in a zip is read as one book, in its folder's order."]])

(defn- money [usd]
  (let [x (or usd 0)]
    (if (< x 0.01) "under a cent" (str "about $" #?(:cljs (.toFixed x 2) :clj (format "%.2f" (double x)))))))

(defn- candidate [dispatch! upload-id {:keys [path type title pages chapters usd parts known error include? skip]}]
  [:li.candidate {:replicant/key path :class (when-not include? "off")}
   [:label.candidate-head
    [:input {:type "checkbox" :checked (boolean include?) :disabled (some? error)
             :on {:change #(dispatch! {:db (fn [d] (db/candidate-toggled d upload-id path))})}}]
    [:span
     [:strong (or title (last (str/split path #"/")))]
     [:span.muted " · " (or type "?")
      (when pages (str " · " pages (if (= 1 pages) " page" " pages")))
      (when chapters (str " · " chapters " chapters"))
      (when usd (str " · folding " (money usd)))]]]
   [:div.candidate-path.muted path]
   (when error [:p.error-text "Cannot read it: " error])
   (when known [:p.note "Already in your library as “" known "”: ingesting it again replaces it, your notes kept."])
   (when (and include? (seq parts))
     [:div.parts
      [:span.muted "Parts:"]
      (for [{p :title k :skip} parts]
        [:label.part {:replicant/key k :class (when (contains? skip k) "off")}
         [:input {:type "checkbox" :checked (not (contains? skip k))
                  :on {:change #(dispatch! {:db (fn [d] (db/part-toggled d upload-id path k))})}}]
         " " p])
      [:span.muted.hint-inline "Uncheck a part to leave it out, a reference you would look up rather than learn."]])])

(defn- upload-card [db dispatch! {:keys [id file status error candidates]}]
  (let [request (db/upload-request db id)]
    [:div.upload {:replicant/key (str id)}
     [:div.upload-head
      [:strong file]
      (case status
        :uploading [:span.muted "Reading…"]
        :failed    [:span.error-text error]
        :ready     (when (empty? candidates) [:span.muted "Nothing in it to read."])
        nil)
      [:button.btn.quiet {:title "Dismiss" :on {:click #(dispatch! {:db (fn [d] (db/upload-dismissed d id))})}} "×"]]
     (when (seq candidates)
       [:ul.candidates (for [c candidates] (candidate dispatch! id c))])
     (when (= :ready status)
       [:div.actions
        [:button.btn.primary {:disabled (nil? request)
                              :on {:click #(dispatch! {:pull (pull/ingest-create id request)})}}
         "Ingest"]
        [:span.muted "The cost is a rough estimate of folding; the steps after it (concepts, prerequisites, key sentences) add to it."]])]))

(def ^:private status-label {:queued "Waiting" :running "Ingesting…" :done "Done" :failed "Failed"})

(defn- job-card [db dispatch! {:ingest/keys [id paths status log cost sources error]}]
  (let [by-id (into {} (map (juxt :source/id identity)) (:sources db))]
    [:div.job {:replicant/key (str id) :class (name status)}
     [:div.job-head
      [:strong (str/join ", " (map #(last (str/split % #"/")) paths))]
      [:span.status (status-label status)]
      (when cost [:span.muted (money cost) " spent"])
      (when (#{:done :failed} status)
        [:button.btn.quiet {:title "Take it off the list" :on {:click #(dispatch! {:pull (pull/ingest-forget id)})}} "×"])]
     (when (= :done status)
       [:div.job-sources
        (for [sid sources :let [s (by-id sid)] :when s]
          [:span {:replicant/key (str sid)}
           (if-let [root (get-in s [:source/root :node/id])]
             (nav/node-link dispatch! {:node/id root} {} (:source/title s))
             (:source/title s))])])
     (when error [:p.error-text error])
     (when (seq log)
       [:pre.log (str/join "\n" (take-last (if (= :running status) 12 6) log))])]))

(defn- library [{:keys [sources]} dispatch!]
  [:section.library
   [:h2 "Library"]
   (if (empty? sources)
     [:p.muted "Nothing yet: drop a book above."]
     [:ul.plain
      (for [{:source/keys [id title type root]} (sort-by :source/title sources)]
        [:li {:replicant/key (str id)}
         (if-let [nid (:node/id root)] (nav/node-link dispatch! {:node/id nid} {} title) title)
         [:span.muted " · " (some-> type name)]])])])

(def ^:private type-labels {:anthropic "Anthropic" :openai "OpenAI-compatible" :ollama "Ollama"})

(defn- field
  "A labelled input of the draft's provider `i`, field `k`."
  [dispatch! i k label value & [{:keys [type placeholder hint]}]]
  [:label.field
   [:span.field-label label]
   [:input {:type (or type "text") :value (str (or value "")) :placeholder placeholder :spellcheck false
            :autocomplete "off"
            :on {:input #(dispatch! {:db (fn [d] (db/provider-set d i k (.. % -target -value)))})}}]
   (when hint [:span.field-hint.muted hint])])

(defn- key-status [{:keys [from hint name]}]
  (case from
    :saved    (str "Saved in the app (" hint ")")
    :env      (str "From " name " (" hint ")")
    :keychain (str "From the keychain item " name " (" hint ")")
    "None: a server on this machine may need none"))

(defn- provider-card [db dispatch! i {:keys [name type base-url api-key forget-key? concurrency num-ctx timeout-s new?] :as p}]
  (let [{:keys [status models problem]} (get-in db [:settings :listed name])
        used? (db/provider-used? db name)]
    [:div.provider {:replicant/key (str i)}
     [:div.provider-head
      (if new?
        [:input.provider-name {:value name :spellcheck false :title "Its name: models are named \"name/model\""
                               :on {:input #(dispatch! {:db (fn [d] (db/provider-set d i :name (.. % -target -value)))})}}]
        [:strong.provider-name name])
      [:select {:on {:change #(dispatch! {:db (fn [d] (db/provider-set d i :type (keyword (.. % -target -value))))})}}
       (for [[t l] type-labels] [:option {:replicant/key t :value (clojure.core/name t) :selected (= t type)} l])]
      [:button.btn.quiet {:disabled used? :title (if used? "The default or a tier uses it" "Remove this provider")
                          :on {:click #(dispatch! {:db (fn [d] (db/provider-removed d i))})}} "Remove"]]
     (when-not (= :anthropic type)
       (field dispatch! i :base-url "Server address" base-url
              {:placeholder (if (= :ollama type) "http://localhost:11434" "http://localhost:8000/v1")}))
     [:label.field
      [:span.field-label "API key"]
      [:input {:type "password" :value api-key :autocomplete "off"
               :placeholder (if (:key p) "Type a new one to replace it" "Paste it here, if it needs one")
               :on {:input #(dispatch! {:db (fn [d] (db/provider-set d i :api-key (.. % -target -value)))})}}]
      [:span.field-hint.muted (if forget-key? "Forgotten when saved" (key-status (:key p)))
       (when (and (= :saved (get-in p [:key :from])) (not forget-key?))
         [:button.btn.quiet.inline {:type "button"
                                    :on {:click #(dispatch! {:db (fn [d] (db/provider-set d i :forget-key? true))})}}
          "Forget it"])]]
     [:details.more
      [:summary.muted "More"]
      (field dispatch! i :concurrency "Requests at a time" concurrency {:type "number" :placeholder "no limit"})
      (when (= :ollama type)
        (field dispatch! i :num-ctx "Context length" num-ctx {:type "number" :placeholder "32768"}))
      (field dispatch! i :timeout-s "Timeout (seconds)" timeout-s {:type "number" :placeholder "600"})]
     [:div.provider-models
      [:button.btn {:type "button" :disabled (= :listing status)
                    :on {:click #(dispatch! {:db (fn [d] (db/listing d name)) :pull (pull/models-list p)})}}
       (if (= :listing status) "Asking…" "List its models")]
      (case status
        :ok     [:span.muted (count models) (if (= 1 (count models)) " model" " models") ": pick one for a tier below"]
        :failed [:span.error-text problem]
        nil)]
     ;; the models it serves, suggested as a tier's model is typed
     [:datalist {:id (str "models-" name)} (for [m models] [:option {:replicant/key m :value m}])]]))

(def ^:private tier-uses
  {:strong "sections, chapters, quizzes, grading, explorations"
   :common "runs of paragraphs, key sentences, answers, prerequisites"})

(defn- tier-row [db dispatch! tier {:keys [provider model] :as t} names]
  (let [{:keys [status ms reply problem]} (get-in db [:settings :tried tier])]
    [:div.tier {:replicant/key (clojure.core/name tier)}
     [:div.tier-name [:strong (str/capitalize (clojure.core/name tier))] [:span.muted (tier-uses tier)]]
     [:div.tier-pick
      [:select {:on {:change #(dispatch! {:db (fn [d] (db/tier-set d tier :provider (.. % -target -value)))})}}
       (for [n names] [:option {:replicant/key n :value n :selected (= n provider)} n])]
      [:input {:value model :list (str "models-" provider) :spellcheck false :placeholder "model"
               :on {:input #(dispatch! {:db (fn [d] (db/tier-set d tier :model (.. % -target -value)))})}}]
      [:button.btn {:type "button" :disabled (or (= :trying status) (db/models-dirty? db))
                    :title (if (db/models-dirty? db) "Save first" "Send it a tiny request")
                    :on {:click #(dispatch! {:db (fn [d] (db/trying d tier)) :pull (pull/model-try tier (db/tier-ref t))})}}
       (if (= :trying status) "Trying…" "Try")]]
     (case status
       :ok     [:p.tier-result.ok "Answered “" reply "” in " #?(:cljs (.toFixed (/ ms 1000) 1) :clj (/ ms 1000.0)) " s"]
       :failed [:p.tier-result.error-text problem]
       nil)]))

(defn- models [db dispatch!]
  (let [{:keys [providers default tiers] :as draft} (get-in db [:settings :draft])
        names (mapv :name providers)
        dirty? (db/models-dirty? db)]
    [:section.models
     [:h2 "Models"]
     (if-not draft
       (t/skeleton 3)
       [:div
        [:p.muted "Two tiers do the work. Each can be a model of any provider: Anthropic's, or one on this machine "
         "through oMLX, Ollama, LM Studio or any OpenAI-compatible server."]
        (for [tier [:strong :common]] (tier-row db dispatch! tier (get tiers tier) names))
        [:h3 "Providers"]
        (map-indexed (fn [i p] (provider-card db dispatch! i p)) providers)
        [:div.add-provider
         [:span.muted "Add:"]
         (for [[label p] db/provider-presets]
           [:button.btn.quiet {:replicant/key label :type "button"
                               :on {:click #(dispatch! {:db (fn [d] (db/provider-added d p))})}}
            "+ " label])]
        [:label.field.default
         [:span.field-label "Default provider"]
         [:select {:on {:change #(dispatch! {:db (fn [d] (db/default-set d (.. % -target -value)))})}}
          (for [n names] [:option {:replicant/key n :value n :selected (= n default)} n])]
         [:span.field-hint.muted "Serves a model named without a provider."]]
        [:div.save-bar {:class (when dirty? "dirty")}
         [:button.btn.primary {:disabled (not dirty?)
                               :on {:click #(dispatch! {:pull (pull/models-save (db/models-request db))})}}
          "Save"]
         [:button.btn {:disabled (not dirty?) :on {:click #(dispatch! {:db db/models-discarded})}} "Discard changes"]
         [:span.muted (if dirty?
                        "Not saved yet."
                        "In force now. Keys typed here are kept in data/secrets.edn, readable by you only.")]]])]))

(defn- prompt-row [db dispatch! {:prompt/keys [id title about edited? default] :as p}]
  (let [open? (= id (get-in db [:settings :open]))
        draft (db/prompt-draft db p)]
    [:li.prompt {:replicant/key (str id) :class (when open? "open")}
     [:button.prompt-head {:type "button" :on {:click #(dispatch! {:db (fn [d] (db/prompt-opened d id))})}}
      [:strong title]
      (when edited? [:span.tag.authored "edited"])
      (when (and (not open?) (db/prompt-changed? db p)) [:span.tag "draft"])
      [:span.muted.prompt-id (str id)]]
     [:p.muted.about about]
     (when open?
       [:div.prompt-editor
        [:textarea {:rows 18 :spellcheck false :value draft
                    :on {:input #(dispatch! {:db (fn [d] (db/prompt-typed d id (.. % -target -value)))})}}]
        [:div.actions
         [:button.btn.primary {:disabled (or (not (db/prompt-changed? db p)) (str/blank? draft))
                               :on {:click #(dispatch! {:pull (pull/prompt-save id draft)})}}
          "Save"]
         [:button.btn {:disabled (not (db/prompt-changed? db p))
                       :on {:click #(dispatch! {:db (fn [d] (db/prompt-dropped d id))})}}
          "Discard changes"]
         (when edited?
           [:button.btn.quiet {:on {:click #(when #?(:cljs (js/confirm "Go back to the default text?") :clj true)
                                              (dispatch! {:pull (pull/prompt-reset id)}))}}
            "Back to the default"])]
        (when edited?
          [:details.default [:summary.muted "The default"] [:pre default]])])]))

(defn- prompts [db dispatch!]
  [:details.advanced
   [:summary [:h2 "Advanced: the prompts"]]
   [:p.muted "What the models are told. An edit applies from the next call and is kept in "
    [:code "data/prompts.edn"] ". Each says what it should reply: keep that part, or the step using it fails."]
   (if-let [groups (seq (db/prompt-groups db))]
     (for [[g ps] groups]
       [:section.prompt-group {:replicant/key g}
        [:h3 g]
        [:ul.prompts (for [p ps] (prompt-row db dispatch! p))]])
     (t/skeleton 3))])

(defn settings-view [db dispatch!]
  (let [{:keys [uploads ingests] :as s} (:settings db)]
    [:main.single.settings
     [:span.meta "Settings"]
     [:h1 "Settings"]
     [:section.add
      [:h2 "Add sources"]
      (dropzone s dispatch!)
      (for [u uploads] (upload-card db dispatch! u))]
     (when (seq ingests)
       [:section.jobs
        [:h2 "Ingesting"]
        (for [j ingests] (job-card db dispatch! j))])
     (library db dispatch!)
     (models db dispatch!)
     (prompts db dispatch!)]))
