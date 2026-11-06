(ns robertluo.fly-learner.ingest.outline
  "Build a section tree from flat extracted blocks.

    {:title t :depth d :page p :children [section-or-paragraph ...]}
    paragraph = {:text .. :page ..}")

(def default-skip
  "Section titles dropped together with their contents."
  #"(?i)contents|table of contents|list of (figures|tables)|bibliography|references|index|acknowledge?ments|dedication")

(defn- close-top
  "Pop the innermost open section and attach it to its parent."
  [stack]
  (let [top   (peek stack)
        stack (pop stack)]
    (update-in stack [(dec (count stack)) :children] conj top)))

(defn- section? [x] (contains? x :children))

(defn- prune [skip section]
  (let [children (->> (:children section)
                      (remove #(and (section? %) (re-matches skip (:title %))))
                      (mapv #(if (section? %) (prune skip %) %)))]
    (assoc section :children children)))

(defn tree
  "Nest `blocks` under their headings. Text before the first heading (cover
  pages etc.) is dropped when the document has headings."
  ([title blocks] (tree title blocks {}))
  ([title blocks {:keys [skip] :or {skip default-skip}}]
   (let [stack (reduce
                (fn [stack {:keys [kind depth] :as b}]
                  (case kind
                    :heading (let [stack (loop [s stack]
                                           (if (>= (:depth (peek s)) depth) (recur (close-top s)) s))]
                               (conj stack {:title (:title b) :depth depth :page (:page b) :children []}))
                    :text    (update-in stack [(dec (count stack)) :children]
                                        conj (select-keys b [:text :page]))))
                [{:title title :depth 0 :children []}]
                blocks)
         root  (loop [s stack] (if (> (count s) 1) (recur (close-top s)) (peek s)))
         root  (cond-> root
                 (some section? (:children root)) (update :children #(filterv section? %)))]
     (prune skip root))))
