(ns robertluo.fly-learner.ingest.outline
  "Build a section tree from flat extracted blocks.

    {:title t :depth d :page p :children [section-or-paragraph ...]}
    (:tags [..] too when its heading has keywords: a Markdown chapter's tags)
    paragraph = {:text .. :page ..}

  Nothing is dropped here: text before the first heading is a part of its
  own, `front-matter` (a title page, a preface without a heading, or the
  whole book when its headings came late); what the learner leaves out is
  taken out by title (`without`)."
  (:require [clojure.string :as str]))

(def front-matter
  "The title of the part holding the text before a document's first heading."
  "Front matter")

(defn- close-top
  "Pop the innermost open section and attach it to its parent."
  [stack]
  (let [top   (peek stack)
        stack (pop stack)]
    (update-in stack [(dec (count stack)) :children] conj top)))

(defn- section? [x] (contains? x :children))

(defn tree
  "Nest `blocks` under their headings. When the document has headings, the
  text before the first one is the part `front-matter`."
  [title blocks]
  (let [stack (reduce
               (fn [stack {:keys [kind depth] :as b}]
                 (case kind
                   :heading (let [stack (loop [s stack]
                                          (if (>= (:depth (peek s)) depth) (recur (close-top s)) s))]
                              (conj stack (cond-> {:title (:title b) :depth depth :page (:page b) :children []}
                                            (:keywords b) (assoc :tags (:keywords b)))))
                   :text    (update-in stack [(dec (count stack)) :children]
                                       conj (select-keys b [:text :page]))))
               [{:title title :depth 0 :children []}]
               blocks)
        root  (loop [s stack] (if (> (count s) 1) (recur (close-top s)) (peek s)))
        [before after] (split-with (complement section?) (:children root))]
    (if (and (seq before) (seq after))
      (assoc root :children (into [{:title front-matter :depth 1 :page (:page (first before)) :children (vec before)}]
                                  after))
      root)))

(defn- title-key [t] (str/lower-case (str/trim (str t))))

(defn without
  "Section tree `tree` without its sections titled one of `titles` (any
  case), at any depth, with what they hold."
  [tree titles]
  (let [out (set (map title-key titles))]
    (if (empty? out)
      tree
      (letfn [(walk [s]
                (assoc s :children (into [] (comp (remove #(and (section? %) (out (title-key (:title %)))))
                                                  (map #(if (section? %) (walk %) %)))
                                         (:children s))))]
        (walk tree)))))

(defn parts
  "The parts of `tree` a learner chooses among: its sections, or those of
  its one section when it has one alone (a book under a title heading)."
  [tree]
  (let [ss (filterv section? (:children tree))]
    (if (and (= 1 (count ss)) (= 1 (count (:children tree))) (some section? (:children (first ss))))
      (recur (first ss))
      ss)))

^:rct/test
(comment
  (def t (tree "Book" [{:kind :text :text "Cover" :page 1}
                       {:kind :heading :depth 1 :title "One" :page 2}
                       {:kind :text :text "a" :page 2}
                       {:kind :heading :depth 1 :title "License" :page 9}
                       {:kind :text :text "b" :page 9}]))
  (mapv :title (:children t)) ;=> ["Front matter" "One" "License"]
  (mapv :title (:children (without t ["license" "front matter"]))) ;=> ["One"]
  (mapv :title (parts {:title "B" :children [{:title "B" :children [{:title "1" :children []} {:title "2" :children []}]}]}))
  ;=> ["1" "2"]
  (mapv :text (:children (tree "Plain" [{:kind :text :text "only text" :page 1}]))) ;=> ["only text"]
  )
