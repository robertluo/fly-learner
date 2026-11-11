(ns robertluo.fly-learner.ui.core.views.quiz
  "Quizzes (a question, the learner's answer, its grading) docked or on the
  study page, and what to study next."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.ui.core.db :as db]
            [robertluo.fly-learner.ui.core.views.markdown :as md]
            [robertluo.fly-learner.ui.core.views.nav :as nav]
            [robertluo.fly-learner.ui.core.views.template :as t]))

(defn- submit-answer [dispatch!]
  (dispatch! {:db db/quiz-submitted :pull :answer-create}))

(defn- question-meta
  "What the question is on; a book exercise says so, counts more and links
  to where it is in the book (its tables read best there)."
  [dispatch! {:keys [title] :question/keys [by anchor weight] :as question}]
  (if (= :author by)
    [:span.meta {:replicant/key "m"}
     [:span.tag.authored {:title "Written by the book's author, graded against their solution"} "from the book"]
     " " (:question/title question) " · on " title
     (when (and weight (< 1 weight)) (str " · counts " (if (== 2 weight) "double" (str weight "×"))))
     (when-let [n (:node/id anchor)]
       (list " · " (nav/node-link dispatch! {:node/id n} {:title "Its tables and formulas read best in the book"
                                                          :class "nowrap"}
                                  (str "read it on p. " (:page anchor)))))]
    [:span.meta {:replicant/key "m"} "Question · " title]))

(defn- text
  "Text of the quiz: Markdown with formulas, or (`verbatim?`, a book's text
  as its source gave it) as it is."
  [verbatim? s]
  (if verbatim? [:p.source-text s] (md/rich s)))

(defn- quiz-panel [{{:keys [status question response result]} :quiz} dispatch!]
  (let [{:question/keys [prompt by verbatim?]} question
        authored? (= :author by)
        prompt-el [:div.prompt {:replicant/key "p" :class (when verbatim? "book")}
                   (if verbatim? prompt (md/rich prompt))]]
    [:section.quiz {:class (when authored? "authored")}
     (case status
       :asking
       [:p.muted.thinking "Writing a question"]

       (:answering :grading)
       (list
        (question-meta dispatch! question)
        prompt-el
        (when-let [ctx (:question/context question)]
          [:details.builds-on {:replicant/key "b"}
           [:summary "The exercise it builds on"]
           (text verbatim? ctx)])
        [:textarea {:replicant/key "t"
                    :value response :rows 6 :placeholder "Answer in your own words… (formulas in TeX: $x^2$)"
                    :disabled (= :grading status)
                    :on {:input   #(dispatch! {:db (fn [d] (db/quiz-typed d (.. % -target -value)))})
                         :keydown #(when (and (= "Enter" (.-key %)) (or (.-metaKey %) (.-ctrlKey %))
                                              (not (str/blank? response)))
                                     (submit-answer dispatch!))}}]
        (if (= :grading status)
          [:p.muted.thinking {:replicant/key "g"} "Grading"]
          [:div.actions {:replicant/key "a"}
           [:button.btn.primary {:disabled (str/blank? response) :on {:click #(submit-answer dispatch!)}
                                 :title "⌘/Ctrl + Enter"}
            "Submit"]
           [:button.btn {:on {:click #(dispatch! {:db (fn [d] (-> d (db/quiz-typed "") db/quiz-submitted))
                                                  :pull :answer-create})}
                         :title "Counts as a wrong answer: honest scores pick better questions"}
            "I don't know"]
           [:button.btn.quiet {:on {:click #(dispatch! {:db db/quiz-close})}} "Skip"]]))

       :graded
       (let [{:keys [grade feedback before after] :question/keys [answer points]} result
             title (:title question)]
         (list
          (question-meta dispatch! question)
          prompt-el
          [:div.graded {:replicant/key "g"}
           [:span.grade (t/percent grade)] (t/score-bar grade)]
          [:div.feedback {:replicant/key "f"} (md/rich feedback)]
          (when-not (str/blank? response)
            [:details {:replicant/key "y"} [:summary "Your answer"] (md/rich response)])
          [:details {:replicant/key "x" :open (< grade 0.8)}
           [:summary (if authored? "The author's solution" "Model answer")]
           (text verbatim? answer)
           [:ul.points (for [[i pt] (map-indexed vector points)] [:li {:replicant/key i} (md/inline pt)])]]
          [:p.change {:replicant/key "c"}
           title " " (t/percent (:item before)) " → " [:strong (t/percent (:item after))]
           [:span.muted " · whole graph "] (t/percent (:overall before)) " → " [:strong (t/percent (:overall after))]]
          [:div.actions {:replicant/key "a"}
           [:button.btn.primary {:on {:click #(dispatch! (nav/quiz-effects nil))}} "Next question"]
           [:button.btn.quiet {:on {:click #(dispatch! {:db db/quiz-close})}} "Done"]]))

       nil)]))

(defn quiz-dock
  "The quiz docked at the top of the left pane: it stays there while the
  learner reads on (the source beside it) to answer it; folds to one line."
  [{{:keys [status question folded?]} :quiz :as db} dispatch!]
  (when status
    [:aside.quiz-dock {:class (when folded? "folded")}
     [:div.dock-head
      [:button.dock-title {:title (if folded? "Open the quiz" "Fold the quiz away")
                           :on {:click #(dispatch! {:db (fn [d] (db/quiz-fold d (not folded?)))})}}
       [:span.chev (if folded? "▸" "▾")]
       [:span "Quiz"]
       [:span.muted (case status
                      :asking  " · writing a question…"
                      :grading " · grading…"
                      :graded  (str " · " (or (:question/title question) (:title question)) ": graded")
                      (some->> (or (:question/title question) (:title question)) (str " · ")))]]
      [:button.btn.quiet {:title "Close the quiz" :on {:click #(dispatch! {:db db/quiz-close})}} "×"]]
     (when-not folded? (quiz-panel db dispatch!))]))

(defn next-cards
  "What to study next: [{:item :title :about :score :prerequisite? :concepts} ..].
  Its title links to what it is about: a concept's page (the passages
  teaching it), or each of the two concepts of a relation; Read opens the
  first."
  [dispatch! next]
  [:ul.cards.compact.next
   (for [{:keys [item title about score prerequisite? concepts]} next]
     [:li.card {:replicant/key (str item)}
      [:div.next-head
       [:span.card-title
        (if (seq concepts)
          (interpose " — " (for [c concepts]
                             (nav/concept-link dispatch! c {:replicant/key (str (:concept/id c)) :class "read-link"})))
          title)
        (when prerequisite?
          [:span.tag.prereq {:title "Not in this topic: concepts in it build on this one"} "prerequisite"])]
       (t/score-bar score)
       (when-let [c (first concepts)]
         (nav/concept-link dispatch! c {:class "btn read" :title (str "Read about " (:concept/label c) ": the passages teaching it")}
                           "Read"))
       [:button.btn.primary {:on {:click #(dispatch! (nav/quiz-effects item))}} "Quiz me"]]
      [:p.preview about]])])

(defn study-view [{:keys [learning quiz loading?] :as db} dispatch!]
  (let [{:keys [overall answers next]} learning]
    [:main.single.study
     [:span.meta "Study"]
     [:h1 "What you know"]
     (if learning
       [:div.overall-score
        [:span.big (t/percent overall)] (t/score-bar overall)
        [:p.muted "of the whole knowledge graph: every concept and every relation between two, "
         "weighted by how much the sources discuss it. Scores fade as you forget; a review brings them back. "
         (when (pos? answers) (str answers (if (= 1 answers) " answer" " answers") " so far."))]]
       (when loading? [:p.muted "Loading…"]))
     (when (:status quiz) (quiz-panel db dispatch!))
     (when (and (seq next) (not (#{:asking :answering :grading} (:status quiz))))
       [:section
        [:h2 "Study next"]
        [:p.hint.muted "What raises your score most: important, not yet known, and built on what you already know."]
        (next-cards dispatch! next)])]))
