(ns robertluo.fly-learner.fold-test
  (:require [clojure.test :refer [deftest is testing]]
            [robertluo.fly-learner.fold :as fold]
            [robertluo.fly-learner.ingest.outline :as outline]))

(defn- para [i] {:kind :text :text (str "paragraph " i) :page 1})

(def blocks
  (concat [{:kind :text :text "cover page" :page 1}
           {:kind :heading :depth 1 :title "1 Intro" :page 1}]
          (map para (range 3))
          [{:kind :heading :depth 2 :title "1.1 Long" :page 2}]
          (map para (range 14))
          [{:kind :heading :depth 1 :title "Bibliography" :page 3}
           (para 99)]))

(def calls (atom []))

(defn- fake-summarize [{:keys [path items] :as req}]
  (swap! calls conj req)
  {:title (str "T" (count items)) :summary "s" :keywords ["Shared" (str "k" (count path))]})

(defn- studied
  "The tree of `blocks` without the parts a learner left out."
  []
  (outline/without (outline/tree "doc" blocks) ["front matter" "Bibliography"]))

(deftest outline-tree
  (let [t (outline/tree "doc" blocks)]
    (testing "nothing is dropped: text before the first heading is the front matter"
      (is (= ["Front matter" "1 Intro" "Bibliography"] (map :title (:children t))))
      (is (= ["cover page"] (map :text (:children (first (:children t)))))))
    (testing "parts left out by title"
      (is (= ["1 Intro"] (map :title (:children (studied))))))
    (testing "subsection nests under its chapter after the chapter's paragraphs"
      (is (= "1.1 Long" (:title (last (:children (first (:children (studied)))))))))))

(deftest regroup-long-runs
  (let [t  (fold/regroup (studied) {:max-children 10 :group-size 6})
        ss (-> t :children first :children last)]
    (is (= [6 6 2] (map (comp count :children) (:children ss))))
    (is (every? :group (:children ss)))))

(deftest fold-pipeline
  (reset! calls [])
  (let [tree  (fold/regroup (studied))
        nodes (-> (fold/summarize-tree fake-summarize tree) fold/flatten-tree fold/link-by-keywords)
        by-id (into {} (map (juxt :node/id identity)) nodes)
        root  (first (filter #(zero? (:node/level %)) nodes))]
    (testing "one call per section/group"
      (is (= 6 (count @calls))))
    (testing "headings are kept as titles and passed as context"
      (is (= #{"1 Intro" "1.1 Long"} (set (keep :heading @calls))))
      (is (some #(= ["doc" "1 Intro" "1.1 Long"] (:path %)) @calls)))
    (testing "single root of kind :source; leaves keep text"
      (is (= :source (:node/kind root)))
      (is (= 17 (count (filter #(= :paragraph (:node/kind %)) nodes))))
      (is (every? :node/text (filter #(= :paragraph (:node/kind %)) nodes))))
    (testing "child ids resolve"
      (is (every? by-id (mapcat :node/children nodes))))
    (testing "common keyword (used by > 6 nodes) does not link; children not linked to parent"
      (is (every? (fn [n] (not-any? (set (:node/children n)) (:node/links n))) nodes)))))
