(ns robertluo.fly-learner.architecture-test
  "The layers know each other as little as possible, so each can be
  reimplemented freely. Checked from the ns forms of every source file.

    knowledge  depends on nothing above it
    learning   the core depends on nothing; it states what it needs as
               protocols (Curriculum, Model)
    decay      an addition to learning (a Model wrapping a Model); learning
               knows nothing of it
    adapter    learning.concepts: the one namespace where learning meets
               knowledge
    features   quiz and the like: learning only, never knowledge (nor the
               evidence store: the caller records what they return)
    ideas      the learner's own voice; the core depends on nothing,
               ideas.knowledge is the one namespace where it meets knowledge
    explore    interactive views as data; the core depends on nothing,
               explore.knowledge is the one namespace where it meets knowledge
    app        server and UI compose everything"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private app-prefix "robertluo.fly-learner.")

(defn- layer [ns-name]
  (let [n (str ns-name)]
    (cond
      (= n "robertluo.fly-learner.learning")          :learning
      (= n "robertluo.fly-learner.learning.decay")    :decay
      (= n "robertluo.fly-learner.learning.store")    :learning-store
      (= n "robertluo.fly-learner.learning.concepts") :adapter
      (= n "robertluo.fly-learner.quiz")              :features
      (= n "robertluo.fly-learner.ideas")             :ideas
      (= n "robertluo.fly-learner.ideas.store")       :ideas-store
      (= n "robertluo.fly-learner.ideas.knowledge")   :ideas-adapter
      (= n "robertluo.fly-learner.llm")               :infra
      (or (= n "robertluo.fly-learner.explore")
          (= n "robertluo.fly-learner.explore.examples")) :explore
      (= n "robertluo.fly-learner.explore.store")     :explore-store
      (= n "robertluo.fly-learner.explore.knowledge") :explore-adapter
      (or (= n "robertluo.fly-learner.cli")
          (str/starts-with? n "robertluo.fly-learner.server.")
          (str/starts-with? n "robertluo.fly-learner.ui"))      :app
      :else                                                     :knowledge)))

(def allowed
  "layer -> layers it may require (besides itself)."
  {:knowledge      #{:infra}
   :learning       #{}
   :decay          #{:learning}
   :learning-store #{:learning}
   :adapter        #{:learning :knowledge}
   :features       #{:learning :infra}
   :infra          #{}
   :ideas          #{}
   :ideas-store    #{:ideas}
   :ideas-adapter  #{:ideas :knowledge}
   :explore        #{}
   :explore-store  #{:explore}
   :explore-adapter #{:explore :explore-store :knowledge :infra}
   :app            #{:knowledge :learning :decay :learning-store :adapter :features :infra
                     :ideas :ideas-store :ideas-adapter :explore :explore-store :explore-adapter}})

(defn- ns-form
  "The ns form of `file`, reader conditionals of both platforms kept."
  [file]
  (for [platform [:clj :cljs]]
    (with-open [r (java.io.PushbackReader. (io/reader file))]
      (binding [*read-eval* false]
        (read {:read-cond :allow :features #{platform}} r)))))

(defn- requires [[_ns-sym _ & clauses]]
  (->> clauses
       (filter #(and (seq? %) (= :require (first %))))
       (mapcat rest)
       (map #(if (sequential? %) (first %) %))
       (filter symbol?)))

(defn app-deps
  "{ns #{required app ns}} for every source file."
  []
  (->> ["src/clj" "src/cljc" "src/cljs"]
       (mapcat #(file-seq (io/file %)))
       (filter #(re-find #"\.clj[cs]?$" (.getName ^java.io.File %)))
       (reduce (fn [acc f]
                 (let [forms (ns-form f)]
                   (update acc (second (first forms)) (fnil into #{})
                           (->> forms (mapcat requires)
                                (filter #(str/starts-with? (str %) app-prefix))))))
               {})))

(deftest layers-depend-only-downwards
  (let [deps (app-deps)]
    (is (< 20 (count deps)) "every source file was read")
    (doseq [[from tos] deps
            to tos
            :let [lf (layer from), lt (layer to)]
            :when (not= lf lt)]
      (testing (str from " -> " to)
        (is (contains? (allowed lf) lt)
            (str (name lf) " may not depend on " (name lt)))))))

(def display-only
  "Namespaces shaping data for the API or the page: only the app requires them."
  '#{robertluo.fly-learner.concept.view})

(deftest views-are-for-the-app
  (is (= #{} (set (for [[from tos] (app-deps)
                        to (filter display-only tos)
                        :when (not= :app (layer from))]
                    [from to])))
      "shaped for display: compute from what it is built on instead"))

(def standalone
  "Namespaces that require nothing of the app: generic templates, and the
  one place that knows the backend's wire format."
  '#{robertluo.fly-learner.ui.core.views.template robertluo.fly-learner.ui.core.api})

(deftest standalone-parts-stand-alone
  (let [deps (app-deps)]
    (doseq [n standalone]
      (is (contains? deps n) (str n " exists"))
      (is (empty? (deps n)) (str n " requires nothing of the app")))))

(deftest the-wire-format-is-in-one-place
  (doseq [f (->> ["src/cljc/robertluo/fly_learner/ui" "src/cljs"]
                 (mapcat #(file-seq (io/file %)))
                 (filter #(re-find #"\.clj[cs]$" (.getName ^java.io.File %)))
                 (remove #(= "api.cljc" (.getName ^java.io.File %))))
          :let [text (slurp f)]]
    (testing (str f)
      (is (not (re-find #"cognitect\.transit|\"/api\"|\"/files/|:pattern" text))
          "requests go through robertluo.fly-learner.ui.core.api"))))

(deftest the-rule-is-checked
  (testing "a feature reaching into knowledge would be caught"
    (is (not (contains? (allowed (layer 'robertluo.fly-learner.quiz))
                        (layer 'robertluo.fly-learner.concept.graph)))))
  (testing "the explore core would be caught reaching into knowledge"
    (is (not (contains? (allowed (layer 'robertluo.fly-learner.explore))
                        (layer 'robertluo.fly-learner.db)))))
  (testing "ideas and learning know nothing of each other"
    (is (not (contains? (allowed :ideas) :learning)))
    (is (not (contains? (allowed :learning) :ideas))))
  (testing "learning would be caught knowing about decay"
    (is (not (contains? (allowed (layer 'robertluo.fly-learner.learning))
                        (layer 'robertluo.fly-learner.learning.decay))))))
