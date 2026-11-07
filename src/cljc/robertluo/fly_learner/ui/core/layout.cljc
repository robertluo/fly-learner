(ns robertluo.fly-learner.ui.core.layout
  "Incremental force-directed layout (Fruchterman-Reingold) for the concept
  graph, plus the view box and tweening used to draw it.

  Incremental: concepts already on screen start where they are and move
  less, a new concept starts beside the concept that revealed it, and the
  result is shifted so that, on average, old concepts stay put. A broader
  concept is pushed above its narrower ones, a prerequisite above the
  concepts needing it (:requires runs from the one needing to the one
  needed, as :broader runs narrower -> broader).

  Parts with no edge between them are laid out each on its own and packed in
  rows: repulsion alone would push them ever further apart (only the weak
  gravity holds them), leaving the picture mostly gap.

  Deterministic (no randomness), so it is tested on the JVM.")

(def defaults
  {:length     110   ; ideal edge length
   :iterations 240
   :old-mobility 0.3 ; how freely concepts already placed move (new ones: 1)
   :aspect     2.0   ; labels are wide: repulsion counts horizontal distance as this much shorter
   :widths     {}    ; id -> label width; labels' boxes are kept apart
   :height     36    ; height of a concept's box (dot and label)
   :gravity    0.03
   :pack-aspect 1.5}) ; width / height the separate parts are packed into

(def ^:private golden-angle 2.399963229728653)

(defn- beside
  "A spot near `[x y]`, spread by `i` so siblings do not start on one point."
  [[x y] i]
  (let [a (* i golden-angle), r (+ 24 (* 4 (mod i 5)))]
    [(+ x (* r (Math/cos a))) (+ y (* r (Math/sin a)))]))

(defn components
  "The connected parts of `ids` under `edges`, largest first (then by their
  first id as a string), each a vector of ids."
  [ids edges]
  (let [in  (set ids)
        adj (reduce (fn [m {:keys [from to]}]
                      (if (and (in from) (in to))
                        (-> m (update from (fnil conj []) to) (update to (fnil conj []) from))
                        m))
                    {} edges)
        reach (fn [id]
                (loop [stack [id], seen #{id}]
                  (if-let [x (peek stack)]
                    (let [nbrs (remove seen (adj x))]
                      (recur (into (pop stack) nbrs) (into seen nbrs)))
                    seen)))]
    (loop [[id & more :as todo] (sort-by str ids), seen #{}, out []]
      (cond
        (empty? todo) (vec (sort-by (juxt (comp - count) (comp str first)) out))
        (seen id)     (recur more seen out)
        :else         (let [part (reach id)]
                        (recur more (into seen part) (conj out (vec (sort-by str part)))))))))

(declare settle)

(defn- extent
  "[x0 y0 x1 y1] around the boxes of `pos`."
  [pos widths height]
  (let [hw #(/ (max 24 (get widths % 0)) 2)]
    [(apply min (for [[id [x _]] pos] (- x (hw id))))
     (apply min (for [[_ [_ y]] pos] (- y (/ height 2))))
     (apply max (for [[id [x _]] pos] (+ x (hw id))))
     (apply max (for [[_ [_ y]] pos] (+ y (/ height 2))))]))

(defn- pack
  "Parts (each {id [x y]}, largest first) put together without gaps: those
  holding concepts already shown (else the largest) stay where they are;
  the others go in rows below them, the rows about as wide as `pack-aspect`
  makes the whole."
  [parts prev {:keys [length widths height pack-aspect]}]
  (let [gap      (* 0.5 length)
        box      #(extent % widths height)
        size     (fn [[x0 y0 x1 y1]] [(- x1 x0) (- y1 y0)])
        shown    (filter #(some prev (keys %)) parts)
        anchored (if (seq shown) shown (take 1 parts))
        loose    (remove (set anchored) parts)
        area     (reduce + (for [p parts :let [[w h] (size (box p))]] (* (+ w gap) (+ h gap))))
        row-w    (max (apply max (map (comp first size box) parts))
                      (Math/sqrt (* area pack-aspect)))
        [x-start _ _ ay1] (box (apply merge anchored))]
    (loop [[p & more] loose, x x-start, y (+ ay1 gap), row-h 0
           out (apply merge {} anchored)]
      (if-not p
        out
        (let [[x0 y0 :as b] (box p)
              [w h]  (size b)
              wrap?  (and (> x x-start) (> (+ (- x x-start) w) row-w))
              y      (if wrap? (+ y row-h gap) y)
              x      (if wrap? x-start x)
              row-h  (if wrap? 0 row-h)
              dx (- x x0), dy (- y y0)]
          (recur more (+ x w gap) y (max row-h h)
                 (into out (for [[id [px py]] p] [id [(+ px dx) (+ py dy)]]))))))))

(defn layout
  "Positions {id [x y]} for `ids` and `edges` ({:from :to :kind}; :broader
  runs narrower -> broader). `prev` holds positions of concepts already
  shown; `seeds` maps a new concept to the concept beside which it starts.
  Unconnected parts are laid out apart, then packed (see `pack`)."
  ([ids edges prev seeds] (layout ids edges prev seeds {}))
  ([ids edges prev seeds opts]
   (let [parts (components ids edges)]
     (if (<= (count parts) 1)
       (settle ids edges prev seeds opts)
       (pack (for [part parts :let [in (set part)]]
               (settle part (filter #(and (in (:from %)) (in (:to %))) edges) prev seeds opts))
             prev
             (merge defaults opts))))))

(defn- reaches?
  "Whether `to` is reachable from `from` in `up` {i #{j ..}}."
  [up from to]
  (loop [seen #{} todo [from]]
    (if-let [i (peek todo)]
      (cond (= i to)  true
            (seen i)  (recur seen (pop todo))
            :else     (recur (conj seen i) (into (pop todo) (up i))))
      false)))

(defn above-edges
  "[[i j above?] ..] for edges [i j kind]: above? when j is to be drawn
  above i, a broader concept or a prerequisite, unless that closes a loop
  with the ones taken before (broader ones first): constraints in a loop
  would push each other down without end. Such an edge is drawn, just
  not as a constraint."
  [edges]
  (first (reduce (fn [[out up] [i j kind]]
                   (if (and (#{:broader :requires} kind) (not (reaches? up j i)))
                     [(conj out [i j true]) (update up i (fnil conj #{}) j)]
                     [(conj out [i j false]) up]))
                 [[] {}]
                 (sort-by (fn [[_ _ kind]] (case kind :broader 0 :requires 1 2)) edges))))

^:rct/test
(comment
  ;; 1 is a kind of 0, and 0 would need 1 first: the broader link wins
  (above-edges [[0 1 :requires] [1 0 :broader] [2 0 :related]])
  ;=> [[1 0 true] [0 1 false] [2 0 false]]
  )

(defn- settle
  "The force layout of one connected part (see `layout`)."
  [ids edges prev seeds opts]
  (let [{:keys [length iterations old-mobility gravity aspect widths height]} (merge defaults opts)
        ids  (vec (sort-by str ids))
        n    (count ids)
        idx  (zipmap ids (range))
        xs   (double-array n), ys (double-array n), mob (double-array n)
        ws   (double-array (map #(double (get widths % 0)) ids))
        dxs  (double-array n), dys (double-array n)
        start (reduce (fn [placed [i id]]
                        (assoc placed id (or (prev id)
                                             (some-> (seeds id) placed (beside i))
                                             (beside [0 0] i))))
                      {}
                      ;; seeds before the concepts they seed
                      (sort-by (fn [[_ id]] (if (prev id) 0 (if (prev (seeds id)) 1 2)))
                               (map-indexed vector ids)))
        es   (above-edges (keep (fn [{:keys [from to kind]}]
                                  (when-let [i (idx from)]
                                    (when-let [j (idx to)] [i j kind])))
                                edges))
        l2   (* length length)]
    (dotimes [i n]
      (let [[x y] (start (ids i))]
        (aset xs i (double x)) (aset ys i (double y))
        (aset mob i (if (prev (ids i)) (double old-mobility) 1.0))))
    (dotimes [it iterations]
      (let [temp (* length (- 0.5 (* 0.49 (/ it iterations))))]
        (dotimes [i n] (aset dxs i 0.0) (aset dys i 0.0))
        ;; every pair repels, side by side more than one above the other,
        ;; so labels do not overlap and siblings stagger
        (dotimes [i n]
          (loop [j (inc i)]
            (when (< j n)
              (let [dx (- (aget xs i) (aget xs j)), dy (- (aget ys i) (aget ys j))
                    ex (/ dx aspect)
                    d  (max 1.0 (Math/sqrt (+ (* ex ex) (* dy dy))))
                    f  (/ l2 d d)]
                (aset dxs i (+ (aget dxs i) (* dx f))) (aset dys i (+ (aget dys i) (* dy f)))
                (aset dxs j (- (aget dxs j) (* dx f))) (aset dys j (- (aget dys j) (* dy f))))
              (recur (inc j)))))
        ;; edges attract
        (doseq [[i j above?] es]
          (let [dx (- (aget xs i) (aget xs j)), dy (- (aget ys i) (aget ys j))
                d  (max 1.0 (Math/sqrt (+ (* dx dx) (* dy dy))))
                f  (* (if above? 1.0 0.7) (/ d length))]
            (aset dxs i (- (aget dxs i) (* dx f))) (aset dys i (- (aget dys i) (* dy f)))
            (aset dxs j (+ (aget dxs j) (* dx f))) (aset dys j (+ (aget dys j) (* dy f)))))
        ;; a little gravity keeps the picture together
        (dotimes [i n]
          (aset dxs i (- (aget dxs i) (* gravity (aget xs i))))
          (aset dys i (- (aget dys i) (* gravity (aget ys i))))
          (let [dx (aget dxs i), dy (aget dys i)
                d  (Math/sqrt (+ (* dx dx) (* dy dy)))]
            (when (pos? d)
              (let [step (* (min d temp) (aget mob i))]
                (aset xs i (+ (aget xs i) (* step (/ dx d))))
                (aset ys i (+ (aget ys i) (* step (/ dy d))))))))
        ;; a constraint, not a force (a hub's many children would outvote
        ;; it): a broader concept is at least `gap` above each narrower one;
        ;; whoever is freer to move does more of the moving
        (doseq [[i j above?] es :when above?]
          (let [over (- (aget ys j) (- (aget ys i) (* 0.6 length)))]
            (when (pos? over)
              (let [mi (aget mob i), mj (aget mob j), total (+ mi mj)]
                (aset ys i (+ (aget ys i) (* over (/ mi total))))
                (aset ys j (- (aget ys j) (* over (/ mj total))))))))))
    ;; label boxes that still overlap: the lower concept moves down (never
    ;; up, so broader concepts stay above), its narrower ones follow
    (loop [pass 0]
      (let [moved (volatile! false)]
        (dotimes [i n]
          (loop [j (inc i)]
            (when (< j n)
              (let [dy (- (aget ys j) (aget ys i))]
                (when (and (< (abs (- (aget xs i) (aget xs j))) (/ (+ (aget ws i) (aget ws j)) 2))
                           (< (abs dy) height))
                  (let [low (if (or (pos? dy) (and (zero? dy) (> j i))) j i)]
                    (aset ys low (+ (aget ys low) (- height (abs dy)) 0.5))
                    (vreset! moved true))))
              (recur (inc j)))))
        (doseq [[i j above?] es :when above?]
          (let [over (- (aget ys j) (- (aget ys i) (* 0.6 length)))]
            (when (pos? over)
              (aset ys i (+ (aget ys i) over))
              (vreset! moved true))))
        (when (and @moved (< pass 60)) (recur (inc pass)))))
    ;; old concepts stay put on average
    (let [old    (filter #(prev (ids %)) (range n))
          [sx sy] (if (seq old)
                    (let [k (count old)]
                      [(/ (reduce + (map #(- (first (prev (ids %))) (aget xs %)) old)) k)
                       (/ (reduce + (map #(- (second (prev (ids %))) (aget ys %)) old)) k)])
                    [0 0])]
      (into {} (map (fn [i] [(ids i) [(+ sx (aget xs i)) (+ sy (aget ys i))]])) (range n)))))

(defn view-box
  "[x y w h] around `pos` with `pad` around it, at least `min-size` wide and high."
  [pos {:keys [pad min-size] :or {pad 70 min-size 360}}]
  (if (empty? pos)
    [(- (/ min-size 2)) (- (/ min-size 2)) min-size min-size]
    (let [xs (map first (vals pos)), ys (map second (vals pos))
          x0 (- (apply min xs) pad), x1 (+ (apply max xs) pad)
          y0 (- (apply min ys) pad), y1 (+ (apply max ys) pad)
          w  (max min-size (- x1 x0)), h (max min-size (- y1 y0))]
      [(- (/ (+ x0 x1) 2) (/ w 2)) (- (/ (+ y0 y1) 2) (/ h 2)) w h])))

(defn ease [t] (let [u (- 1 t)] (- 1 (* u u u)))) ; cubic out

(defn tween
  "Positions part-way (`t` in [0, 1]) from `from` to `to`; concepts without a
  `from` position appear at their target."
  [from to t]
  (if (>= t 1)
    to
    (let [e (ease t)]
      (into {} (map (fn [[id [x y]]]
                      (let [[fx fy] (get from id [x y])]
                        [id [(+ fx (* e (- x fx))) (+ fy (* e (- y fy)))]])))
            to))))

^:rct/test
(comment
  (def edges [{:from :t :to :p :kind :broader}
              {:from :a :to :t :kind :related}
              {:from :b :to :t :kind :related}])
  (def pos (layout [:p :t :a :b] edges {} {}))

  ;; finite (NaN fails every comparison)
  (every? (fn [[x y]] (and (< (abs x) 1e4) (< (abs y) 1e4))) (vals pos)) ;=> true
  ;; the broader concept is above (smaller y) by at least 0.6 x length
  (<= (* 0.6 110) (- (second (pos :t)) (second (pos :p)))) ;=> true
  ;; also for a hub: every child below its parent
  (def star (layout (cons :hub (range 12)) (for [i (range 12)] {:from i :to :hub :kind :broader}) {} {}))
  (every? #(< (second (star :hub)) (second (star %))) (range 12)) ;=> true

  ;; with label widths, no two label boxes overlap, and the hub stays on top
  (def wide (layout (cons :hub (range 12)) (for [i (range 12)] {:from i :to :hub :kind :broader}) {} {}
                    {:widths (zipmap (cons :hub (range 12)) (repeat 120))}))
  (every? (fn [[a b]] (let [[xa ya] (wide a) [xb yb] (wide b)]
                        (or (>= (abs (- xa xb)) 120) (>= (abs (- ya yb)) 36))))
          (for [a (keys wide) b (keys wide) :when (neg? (compare (str a) (str b)))] [a b]))
  ;=> true
  (every? #(< (second (wide :hub)) (second (wide %))) (range 12)) ;=> true
  ;; deterministic
  (= pos (layout [:p :t :a :b] edges {} {})) ;=> true

  ;; parts with no edge between them stay close: a star and three loners
  (components [:x :hub 1 2 :y] [{:from 1 :to :hub} {:from 2 :to :hub}]) ;=> [[1 2 :hub] [:x] [:y]]
  (def apart (layout (concat [:hub :x :y :z] (range 12)) (for [i (range 12)] {:from i :to :hub :kind :broader}) {} {}
                     {:widths (zipmap (concat [:hub :x :y :z] (range 12)) (repeat 120))}))
  (let [xs (map first (vals apart)) ys (map second (vals apart))]
    (< (max (- (apply max xs) (apply min xs)) (- (apply max ys) (apply min ys))) 1200)) ;=> true
  (every? (fn [[a b]] (let [[xa ya] (apart a) [xb yb] (apart b)]
                        (or (>= (abs (- xa xb)) 120) (>= (abs (- ya yb)) 36))))
          (for [a (keys apart) b (keys apart) :when (neg? (compare (str a) (str b)))] [a b]))
  ;=> true
  ;; a part already shown stays about where it was when a loner joins
  (def apart2 (layout (concat [:hub :x :y :z :w] (range 12)) (for [i (range 12)] {:from i :to :hub :kind :broader})
                      apart {} {:widths (zipmap (concat [:hub :x :y :z :w] (range 12)) (repeat 120))}))
  ;; (its concepts settle a little; it is not moved as a whole to make room)
  (def centre (fn [pos ks] (let [ps (map pos ks)] [(/ (reduce + (map first ps)) (count ps)) (/ (reduce + (map second ps)) (count ps))])))
  (let [[x1 y1] (centre apart (cons :hub (range 12))) [x2 y2] (centre apart2 (cons :hub (range 12)))]
    (< (Math/hypot (- x1 x2) (- y1 y2)) 1)) ;=> true

  ;; adding :c next to :a: old concepts move well under an edge's length,
  ;; and :c ends about an edge's length from :a
  (def dist (fn [[x1 y1] [x2 y2]] (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))))
  (def edge-len (dist (pos :t) (pos :a)))
  (def pos2 (layout [:p :t :a :b :c] (conj edges {:from :c :to :a :kind :related}) pos {:c :a}))
  (< (apply max (map #(dist (pos %) (pos2 %)) [:p :t :a :b])) (* 0.4 edge-len)) ;=> true
  (< (dist (pos2 :c) (pos2 :a)) (* 1.5 edge-len)) ;=> true

  (view-box {} {}) ;=> [-180 -180 360 360]
  (view-box {:a [0 0] :b [1000 0]} {:pad 0 :min-size 100}) ;=> [0 -50 1000 100]

  (tween {:a [0 0]} {:a [10 0] :b [5 5]} 1) ;=> {:a [10 0] :b [5 5]}
  (tween {:a [0 0]} {:a [10 0] :b [5 5]} 0.0) ;=> {:a [0.0 0.0] :b [5.0 5.0]}
  )
