(ns robertluo.fly-learner.ui.core.layout
  "Incremental force-directed layout (Fruchterman-Reingold) for the concept
  graph, plus the view box and tweening used to draw it.

  Incremental: concepts already on screen start where they are and move
  less, a new concept starts beside the concept that revealed it, and the
  result is shifted so that, on average, old concepts stay put. A broader
  concept is pushed above its narrower ones.

  Deterministic (no randomness), so it is tested on the JVM.")

(def defaults
  {:length     110   ; ideal edge length
   :iterations 240
   :old-mobility 0.3 ; how freely concepts already placed move (new ones: 1)
   :aspect     2.0   ; labels are wide: repulsion counts horizontal distance as this much shorter
   :widths     {}    ; id -> label width; labels' boxes are kept apart
   :height     36    ; height of a concept's box (dot and label)
   :gravity    0.03})

(def ^:private golden-angle 2.399963229728653)

(defn- beside
  "A spot near `[x y]`, spread by `i` so siblings do not start on one point."
  [[x y] i]
  (let [a (* i golden-angle), r (+ 24 (* 4 (mod i 5)))]
    [(+ x (* r (Math/cos a))) (+ y (* r (Math/sin a)))]))

(defn layout
  "Positions {id [x y]} for `ids` and `edges` ({:from :to :kind}; :broader
  runs narrower -> broader). `prev` holds positions of concepts already
  shown; `seeds` maps a new concept to the concept beside which it starts."
  ([ids edges prev seeds] (layout ids edges prev seeds {}))
  ([ids edges prev seeds opts]
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
         es   (vec (keep (fn [{:keys [from to kind]}]
                           (when-let [i (idx from)]
                             (when-let [j (idx to)] [i j (= :broader kind)])))
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
         (doseq [[i j broader?] es]
           (let [dx (- (aget xs i) (aget xs j)), dy (- (aget ys i) (aget ys j))
                 d  (max 1.0 (Math/sqrt (+ (* dx dx) (* dy dy))))
                 f  (* (if broader? 1.0 0.7) (/ d length))]
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
         (doseq [[i j broader?] es :when broader?]
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
         (doseq [[i j broader?] es :when broader?]
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
       (into {} (map (fn [i] [(ids i) [(+ sx (aget xs i)) (+ sy (aget ys i))]])) (range n))))))

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
