(ns robertluo.fly-learner.ui.core.views.selection
  "The popover on selected text: highlight it, react to it, write a note on
  it, or ask about it."
  (:require [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.pull :as pull]
            [robertluo.fly-learner.ui.core.views.ask :as ask-ui]
            [robertluo.fly-learner.ui.core.views.ideas :as ideas-ui]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.template :as t]
            #?(:cljs [clojure.string :as str])))

(defn- submit-selection-ask
  "Form submit: ask the form's `q` input about the selected text."
  #?(:cljs [dispatch! e] :clj [_dispatch! _e])
  #?(:cljs (do (.preventDefault e)
               (let [q (str/trim (.. e -target -elements -q -value))]
                 (when-not (str/blank? q)
                   (dispatch! {:db #(db/selection-ask % q) :pull :selection-ask}))))
     :clj  nil))

(defn- closing
  "`dispatch!` that also closes the selection popover (leaving it for a page)."
  [dispatch!]
  (fn [effects]
    (dispatch! (update effects :db #(if % (comp % db/draft-cancel) db/draft-cancel)))))

(defn selection-popover
  "Selected text, offered a reaction, a note or a question to the assistant."
  [db dispatch!]
  (when-let [{:keys [anchor ask] :as sel} (get-in db [:ideas :selection])]
    (let [draft (get-in db [:ideas :draft])
          {:keys [question status answer]} ask
          close [:button.popover-close {:title "Close" :on {:click #(dispatch! {:db db/draft-cancel})}} "×"]]
      [:div.idea-popover {:class (when (and ask (not (:from-selection? draft))) "wide")
                          :style (t/popover-style sel (and ask (not (:from-selection? draft))))}
       close
       [:p.quote-preview "“" (t/snippet (:quote anchor) 120) "”"]
       (cond
         (:from-selection? draft)
         (ideas-ui/composer db dispatch!)

         (= :asking status)
         [:div.asked
          [:p.question question]
          [:p.muted.thinking "Thinking about it"]]

         (= :answered status)
         [:div.asked
          [:p.question question]
          (ask-ui/answer-view answer (closing dispatch!))
          [:div.actions
           [:button.btn.primary {:title "Keep this answer as your note on the passage"
                                 :on {:click #(dispatch! {:db (fn [d] (db/draft-start d [anchor] true
                                                                                      (db/answer-as-note question (:ask/answer answer))))})}}
            "✎ Keep as note"]
           (nav/ask-link (closing dispatch!) answer {:class "btn" :title "The answer with its sources"} "Open")]]

         :else
         [:div
          [:div.actions
           (for [stance [:highlight :agree :disagree]]
             [:button.btn {:replicant/key (name stance) :class (name stance)
                           :on {:click #(dispatch! {:pull (pull/idea-create {:idea/stance stance :idea/anchors [anchor]})})}}
              (ideas-ui/stance-mark stance) " " (ideas-ui/stance-label stance)])
           [:button.btn {:on {:click #(dispatch! {:db (fn [d] (db/draft-start d [anchor] true))})}} "✎ Note"]]
          [:form.ask-selection {:on {:submit (partial submit-selection-ask dispatch!)}}
           [:input {:name "q" :type "text" :autocomplete "off" :placeholder "Ask about this… e.g. explain it"}]
           [:button.btn {:type "submit"} "Ask"]]])])))
