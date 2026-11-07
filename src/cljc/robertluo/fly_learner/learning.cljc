(ns robertluo.fly-learner.learning
  "Learning: how much the learner knows of a body of knowledge, as a score
  in [0, 1] per item, rising with evidence. The learner's goal is to raise
  the weighted score of every item, `overall`.

  This namespace depends on nothing. It states what it needs as protocols:

    Curriculum  what is there to learn: items, their weights, what comes
                before what, and the material that teaches or tests an item.
                Implemented over a body of knowledge elsewhere (e.g.
                robertluo.fly-learner.learning.concepts); item ids are
                opaque here, only compared.
    Model       how evidence turns into a score. `mastery` is the basic one;
                others wrap it (e.g. robertluo.fly-learner.learning.decay adds
                forgetting) without this namespace knowing.

  Evidence is the only input, a log of plain maps:
    {:item id :kind kw :at epoch-ms :grade 0..1 (for graded kinds) :source any}
  Kinds are open: a model scores the kinds it knows and ignores the rest.
    :answer  a graded answer about the item (e.g. a quiz)
    :seen    the learner met the item (read it, asked about it); exposure,
             not recall
  States are derived by replaying evidence, so a new model or a corrected
  curriculum needs no migration.")

;;=============================================================================
;; Contracts
;;=============================================================================

(defprotocol Curriculum
  (items [c] "{item weight}: everything to learn; weight > 0 is how much knowing it counts.")
  (prerequisites [c item] "Items best known before `item` (may be empty).")
  (material [c item]
    "What teaching or testing `item` draws on:
     {:title str :about str (what knowing it means) :sources [{:title str :text str} ..]}"))

(defprotocol Model
  (init [m] "State of an item without evidence.")
  (observe [m state evidence] "State after `evidence` about the item.")
  (score [m state at] "Score in [0, 1] at time `at` (epoch ms)."))

;;=============================================================================
;; Evidence -> scores
;;=============================================================================

(defn states
  "{item state} after replaying `evidence` (any order) through `model`."
  [model evidence]
  (reduce (fn [acc e] (update acc (:item e) #(observe model (or % (init model)) e)))
          {}
          (sort-by :at evidence)))

(defn scores
  "{item score} at `at` for every item of `curriculum`."
  [model curriculum states at]
  (into {} (map (fn [item] [item (score model (get states item (init model)) at)]))
        (keys (items curriculum))))

(defn overall
  "Weighted mean score of the whole curriculum: the learner's target."
  [curriculum scores]
  (let [ws (items curriculum)
        total (reduce + (vals ws))]
    (if (pos? total)
      (/ (reduce + (map (fn [[item w]] (* w (get scores item 0))) ws)) total)
      0)))

(defn restricted
  "`curriculum` limited to the items in `keep?` (a set or predicate): what a
  learner studies when they focus on part of it. Prerequisites outside still
  count for readiness."
  [curriculum keep?]
  (let [ws (into {} (filter (comp keep? key)) (items curriculum))]
    (reify Curriculum
      (items [_] ws)
      (prerequisites [_ item] (prerequisites curriculum item))
      (material [_ item] (material curriculum item)))))

(defn readiness
  "How well `item`'s prerequisites are known: their mean score, 1 if none."
  [curriculum scores item]
  (let [ps (prerequisites curriculum item)]
    (if (seq ps) (/ (reduce + (map #(get scores % 0) ps)) (count ps)) 1)))

(def unlock-share
  "Of the value of an item not known yet, the share that goes to its
  prerequisites (split among them): studying a prerequisite is worth part
  of what it opens."
  0.5)

(defn- with-prerequisites
  "`items` and every prerequisite they rest on, directly or not."
  [curriculum items]
  (loop [seen (set items) todo (vec items)]
    (if-let [i (peek todo)]
      (let [new (remove seen (prerequisites curriculum i))]
        (recur (into seen new) (into (pop todo) new)))
      seen)))

(defn- values
  "{item value} over `cands`: an item's weight (0 outside the curriculum's
  items) plus `unlock-share` of the value of each item needing it that is
  not known yet, split among that item's prerequisites. Each level gives
  back at most half of what it has, so the total stays within twice the
  weights. A cycle (not expected) is cut where it is met."
  [curriculum scores cands]
  (let [ws      (items curriculum)
        needers (reduce (fn [m i] (reduce #(update %1 %2 (fnil conj #{}) i) m (prerequisites curriculum i)))
                        {} cands)
        memo    (volatile! {})]
    (letfn [(value [i path]
              (or (@memo i)
                  (let [v (+ (get ws i 0)
                             (* unlock-share
                                (reduce + (for [d (needers i) :when (not (path d))]
                                            (/ (* (- 1 (get scores d 0)) (value d (conj path i)))
                                               (count (prerequisites curriculum d)))))))]
                    (vswap! memo assoc i v)
                    v)))]
      (into {} (map (fn [i] [i (value i #{i})])) cands))))

(defn next-items
  "The `n` items whose study raises `overall` most, best first:
  value x room to grow x readiness. Value is the item's weight plus part of
  what it unlocks (see `values`), so a foundation many items rest on comes
  early, and a prerequisite outside the curriculum's items (one restricted
  to a part, say) is offered too when items rest on it. Readiness never
  drops below a floor, so a beginner (nothing known) starts from items
  without prerequisites rather than from nothing. [[item priority] ..]"
  [curriculum scores n]
  (let [cands (with-prerequisites curriculum (keys (items curriculum)))
        vs    (values curriculum scores cands)]
    (->> cands
         (map (fn [item]
                [item (* (vs item) (- 1 (get scores item 0))
                         (+ 0.2 (* 0.8 (readiness curriculum scores item))))]))
         (filter (comp pos? second))
         (sort (fn [[i p] [j q]]                     ; ties in a stable order
                 (let [o (compare q p)] (if (zero? o) (compare (str i) (str j)) o))))
         (take n)
         vec)))

;;=============================================================================
;; The basic model: mastery, no notion of time
;;=============================================================================

(defrecord Mastery [rate seen-cap]
  Model
  (init [_] {:mastery 0.0 :answers 0 :seen? false})
  (observe [_ state {:keys [kind grade]}]
    (case kind
      ;; each answer moves mastery part of the way to its grade
      :answer (-> state
                  (update :mastery #(+ % (* rate (- grade %))))
                  (update :answers inc))
      ;; exposure counts a little, never more than seen-cap
      :seen   (-> state (update :mastery max seen-cap) (assoc :seen? true))
      state))
  (score [_ state _at] (:mastery state)))

(defn mastery
  "Score = running mastery: each graded answer moves it `rate` of the way to
  its grade; being seen lifts it to `seen-cap` at most."
  ([] (mastery {}))
  ([{:keys [rate seen-cap] :or {rate 0.5 seen-cap 0.1}}]
   (->Mastery rate seen-cap)))

^:rct/test
(comment
  (def c (reify Curriculum
           (items [_] {:a 3 :b 1 :ab 2})
           (prerequisites [_ item] (case item :ab [:a :b] []))
           (material [_ item] {:title (name item)})))
  (def m (mastery))

  ;; nothing known: score 0 everywhere; start with the heaviest item without prerequisites
  (overall c (scores m c {} 0)) ;=> 0.0
  (ffirst (next-items c (scores m c {} 0) 3)) ;=> :a

  (def log [{:item :a :kind :answer :grade 1.0 :at 2}
            {:item :a :kind :answer :grade 1.0 :at 1}
            {:item :b :kind :seen :at 3}
            {:item :b :kind :something-new :at 4}])
  (def s (scores m c (states m log) 5))
  s ;=> {:a 0.75 :b 0.1 :ab 0.0}
  (overall c s) ;=> 0.39166666666666666 ; (3 x 0.75 + 1 x 0.1) / 6

  ;; :a is half-way known, :b barely: what to study next moves on, :b first as
  ;; :ab rests on it too
  (mapv first (next-items c s 3)) ;=> [:b :ab :a]

  ;; focusing on part of it: its own overall, next items only from there
  (def ab (restricted c #{:b :ab}))
  (overall ab s) ;=> 0.03333333333333333 ; (1 x 0.1 + 2 x 0) / 3
  (readiness ab s :ab) ;=> 0.425 ; :a counts though it is outside
  ;; and is offered (after the rest: it is 3/4 known and only opens :ab here)
  (mapv first (next-items ab s 3)) ;=> [:b :ab :a]

  ;; a foundation outside the part learned: :x and :y rest on :h, which counts
  ;; for nothing itself here; knowing nothing, :h comes first
  (def h (restricted (reify Curriculum
                       (items [_] {:x 2 :y 2 :h 1})
                       (prerequisites [_ item] (if (= :h item) [] [:h]))
                       (material [_ _] nil))
                     #{:x :y}))
  (next-items h {} 3) ;=> [[:h 2.0] [:x 0.4] [:y 0.4]]

  ;; a wrong answer pulls mastery down
  (:mastery (observe m {:mastery 0.8 :answers 1} {:kind :answer :grade 0.0})) ;=> 0.4
  )
