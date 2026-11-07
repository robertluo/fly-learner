(ns robertluo.fly-learner.ui.core.explore
  "Drawing an exploration: the elements robertluo.fly-learner.explore/view
  resolves, as Replicant hiccup. `send!` takes an action
  (robertluo.fly-learner.explore/act). Pure, so it renders on the JVM too;
  diagrams are laid out here (`diagram-layout`), plots scaled (`ticks`)."
  (:require [clojure.string :as str]
            [robertluo.fly-learner.explore :as explore]))

;;=============================================================================
;; Diagram layout: layered, children under their parents
;;=============================================================================

(def ^:private char-w 7.2)
(def ^:private line-h 16)

(defn- box-size [label]
  (let [lines (str/split-lines (or label ""))]
    [(+ 20 (* char-w (apply max 1 (map count lines)))) (+ 12 (* line-h (max 1 (count lines))))]))

(defn- ranks
  "id -> rank: the longest path to it from a node with no incoming edge
  (edges back to an ancestor, as in a cycle, are not followed)."
  [ids edges]
  (let [succ (reduce (fn [m {:keys [from to]}] (update m from (fnil conj []) to)) {} edges)
        has-in (set (map :to edges))
        roots (or (seq (remove has-in ids)) (take 1 ids))]
    (loop [rank (zipmap roots (repeat 0)), frontier (vec roots), n 0]
      (if (or (empty? frontier) (> n (count ids)))
        (merge (zipmap ids (repeat 0)) rank)
        (let [step (for [f frontier, t (succ f) :when (< (get rank t -1) (inc (rank f)))] [t (inc (rank f))])]
          (recur (into rank step) (vec (distinct (map first step))) (inc n)))))))

(def ^:private gap-across 24)
(def ^:private gap-along 56)

(defn diagram-layout
  "Positions for a diagram's nodes ([{:id :label}]) joined by edges
  ([{:from :to}]): {:boxes {id {:x :y :w :h}} (centres) :width :height}.
  Ranks run down (or right): each node at the mean place of its parents,
  then pushed apart so none overlap."
  [nodes edges direction]
  (let [ids    (mapv :id nodes)
        size   (into {} (map (fn [{:keys [id label]}] [id (box-size label)])) nodes)
        down?  (not= :right direction)
        across (fn [id] (let [[w h] (size id)] (if down? w h)))
        along  (fn [id] (let [[w h] (size id)] (if down? h w)))
        rank   (ranks ids edges)
        order  (into {} (map-indexed (fn [i id] [id i])) ids)
        parents (reduce (fn [m {:keys [from to]}] (update m to (fnil conj []) from)) {} edges)
        layers (->> ids (group-by rank) (sort-by key) (map val))
        place  (reduce
                (fn [pos layer]
                  (let [want   (fn [id] (let [ps (filter pos (parents id))]
                                          (if (seq ps) (/ (reduce + (map pos ps)) (count ps)) 0)))
                        layer  (sort-by (juxt want order) layer)
                        ;; left to right, each at its wish or just right of the one before
                        packed (reduce (fn [acc id]
                                         (let [lo (if-let [prev (peek acc)]
                                                    (+ (second prev) (/ (across (first prev)) 2) gap-across (/ (across id) 2))
                                                    ##-Inf)]
                                           (conj acc [id (max lo (want id))])))
                                       [] layer)
                        shift  (- (/ (reduce + (map want layer)) (count layer))
                                  (/ (reduce + (map second packed)) (count packed)))]
                    (into pos (map (fn [[id x]] [id (+ x shift)])) packed)))
                {} layers)
        depth  (reduce (fn [m layer]
                         (let [start (if (empty? m) 0
                                         (+ (apply max (map (fn [[id d]] (+ d (/ (along id) 2))) m)) gap-along))
                               thick (apply max (map along layer))]
                           (into m (map (fn [id] [id (+ start (/ thick 2))])) layer)))
                       {} layers)
        lo     (apply min 0 (map #(- (place %) (/ (across %) 2)) ids))
        boxes  (into {} (map (fn [id]
                               (let [[w h] (size id) a (- (place id) lo) d (depth id)]
                                 [id (if down? {:x a :y d :w w :h h} {:x d :y a :w w :h h})])))
                     ids)
        extent (fn [k s] (apply max 0 (map (fn [b] (+ (k b) (/ (s b) 2))) (vals boxes))))]
    {:boxes boxes :width (extent :x :w) :height (extent :y :h)}))

^:rct/test
(comment
  (def l (diagram-layout [{:id "E" :label "Entrant"} {:id "out" :label "Out"} {:id "I" :label "Inc"}
                          {:id "f" :label "Fight"} {:id "a" :label "Acc"}]
                         [{:from "E" :to "out"} {:from "E" :to "I"} {:from "I" :to "f"} {:from "I" :to "a"}]
                         :down))
  ;; parents above children, children centred under their parent
  (map #(get-in l [:boxes % :y]) ["E" "I" "f"]) ;=> [14 98 182]
  (let [{:strs [I f a]} (:boxes l)] (< (Math/abs (- (:x I) (/ (+ (:x f) (:x a)) 2))) 1e-9)) ;=> true
  ;; nothing overlaps within a rank
  (let [{:strs [f a]} (:boxes l)] (>= (- (:x a) (:x f)) (+ (/ (:w a) 2) (/ (:w f) 2)))) ;=> true
  ;; a cycle still ends
  (count (:boxes (diagram-layout [{:id "a"} {:id "b"}] [{:from "a" :to "b"} {:from "b" :to "a"}] :right))) ;=> 2
  )

;;=============================================================================
;; Plot scale
;;=============================================================================

(defn ticks
  "About `n` round values from lo to hi."
  [lo hi n]
  (if (>= lo hi)
    [lo]
    (let [raw  (/ (- hi lo) n)
          mag  (Math/pow 10 (Math/floor (Math/log10 raw)))
          step (apply min-key #(Math/abs (- (/ (- hi lo) %) n)) (map #(* % mag) [1 2 2.5 5 10]))
          from (* step (Math/ceil (/ lo step)))]
      (vec (take-while #(<= % (+ hi 1e-9)) (map #(+ from (* % step)) (range 100)))))))

^:rct/test
(comment
  (ticks 0 1 5) ;=> [0.0 0.2 0.4 0.6000000000000001 0.8 1.0]
  (ticks -1.5 5 5) ;=> [-1.0 0.0 1.0 2.0 3.0 4.0 5.0]
  )

;;=============================================================================
;; Elements
;;=============================================================================

(def ^:dynamic *cite*
  "(fn [n] hiccup) drawing citation [n] in text, or nil: left as written."
  nil)

(defn- cited
  "`s` with its [n] citations drawn by *cite*."
  [s]
  (if-not *cite*
    s
    ;; eager: drawn while *cite* is bound
    (into [:span]
          (map-indexed (fn [i tok]
                         (if-let [[_ n] (re-matches #"\[(\d+)\]" tok)]
                           [:span {:replicant/key i} (*cite* (parse-long n))]
                           [:span {:replicant/key i} tok])))
          (re-seq #"\[\d+\]|[^\[]+|\[" (or s "")))))

(defn- marked
  "`text` with the first `mark` in it highlighted."
  [text mark]
  (if-let [i (and (seq mark) (str/index-of text mark))]
    [:span (subs text 0 i) [:mark (subs text i (+ i (count mark)))] (subs text (+ i (count mark)))]
    [:span text]))

(defn- target-value [e] (.. e -target -value))

(defn- slider [{:keys [index label min max step value]} send!]
  [:label.x-slider
   [:span label]
   [:input {:type "range" :min min :max max :step step :value (str value)
            :on {:input #(send! {:index index :value (parse-double (target-value %))})}}]])

(defn- toggle [{:keys [index label value]} send!]
  [:label.x-toggle
   [:input {:type "checkbox" :checked value
            :on {:change #(send! {:index index :value (.. % -target -checked)})}}]
   [:span label]])

(defn- choice [{:keys [index label options]} send!]
  [:div.x-choice
   (when (seq label) [:span.x-label label])
   [:div.segmented
    (for [[j {:keys [label selected?]}] (map-indexed vector options)]
      [:button.btn {:replicant/key j :class (when selected? "on") :aria-pressed (str (boolean selected?))
                    :on {:click #(send! {:index index :value j})}}
       label])]])

(defn- table [{:keys [index rows cols cells caption clickable?]} send!]
  [:figure.x-table
   [:table
    [:thead [:tr [:th] (for [[j c] (map-indexed vector cols)] [:th {:replicant/key j} c])]]
    [:tbody
     (for [[r row] (map-indexed vector cells)]
       [:tr {:replicant/key r}
        [:th (nth rows r)]
        (for [[c {:keys [text highlight?]}] (map-indexed vector row)]
          [:td (cond-> {:replicant/key c :class [(when highlight? "on") (when clickable? "clickable")]}
                 clickable? (assoc :on {:click #(send! {:index index :row r :col c})}))
           text])])]]
   (when caption [:figcaption (cited caption)])])

(def ^:private plot-w 520)
(def ^:private plot-h 280)
(def ^:private pad {:l 44 :r 16 :t 26 :b 36})

(defn- finite? [x] (and (number? x) #?(:clj (Double/isFinite (double x)) :cljs (js/isFinite x))))

(defn- plot [{:keys [x-min x-max y-min y-max x-label y-label series points]}]
  (let [sx (fn [x] (+ (:l pad) (* (- plot-w (:l pad) (:r pad)) (/ (- x x-min) (- x-max x-min)))))
        sy (fn [y] (- plot-h (:b pad) (* (- plot-h (:t pad) (:b pad)) (/ (- y y-min) (- y-max y-min)))))
        in? (fn [y] (<= y-min y y-max))
        fmt explore/format-value]
    [:figure.x-plot
     [:svg {:viewBox (str "0 0 " plot-w " " plot-h) :role "img"}
      (for [t (ticks y-min y-max 5)]
        [:g {:replicant/key (str "y" t)}
         [:line.grid {:x1 (:l pad) :x2 (- plot-w (:r pad)) :y1 (sy t) :y2 (sy t)}]
         [:text.tick {:x (- (:l pad) 6) :y (+ 4 (sy t)) :text-anchor "end"} (fmt t)]])
      (for [t (ticks x-min x-max 6)]
        [:text.tick {:replicant/key (str "x" t) :x (sx t) :y (- plot-h (:b pad) -16) :text-anchor "middle"} (fmt t)])
      (when (< y-min 0 y-max)
        [:line.axis {:x1 (:l pad) :x2 (- plot-w (:r pad)) :y1 (sy 0) :y2 (sy 0)}])
      [:line.axis {:x1 (:l pad) :x2 (:l pad) :y1 (:t pad) :y2 (- plot-h (:b pad))}]
      (when x-label [:text.axis-label {:x (- plot-w (:r pad)) :y (- plot-h 4) :text-anchor "end"} x-label])
      (when y-label [:text.axis-label {:x 4 :y 14} y-label])
      (for [[i {:keys [points]}] (map-indexed vector series)
            [j run] (map-indexed vector (partition-by #(and (finite? (second %)) (in? (second %))) points))
            :when (and (finite? (second (first run))) (in? (second (first run))))]
        [:polyline {:replicant/key (str i "-" j) :class (str "series s" (mod i 6))
                    :points (str/join " " (map (fn [[x y]] (str (sx x) "," (sy y))) run))}])
      (for [[i {:keys [x y label]}] (map-indexed vector points)
            :when (and (finite? x) (finite? y) (in? y) (<= x-min x x-max))]
        [:g.point {:replicant/key i}
         [:circle {:cx (sx x) :cy (sy y) :r 4}]
         (when label [:text {:x (+ 7 (sx x)) :y (- (sy y) 7)} label])])]
     (when (some :label series)
       [:figcaption.legend
        (for [[i {:keys [label]}] (map-indexed vector series) :when label]
          [:span {:replicant/key i} [:span {:class (str "swatch s" (mod i 6))}] label])])]))

(defn- diagram [{:keys [index nodes edges direction clickable?]} send!]
  (let [{:keys [boxes width height]} (diagram-layout nodes edges direction)
        down? (= :down direction)
        m 8]
    [:figure.x-diagram
     [:svg {:viewBox (str (- m) " " (- m) " " (+ width m m) " " (+ height m m))
            :style {:max-width (str (+ width m m) "px")}}
      [:defs [:marker {:id "x-arrow" :viewBox "0 0 10 10" :refX 9 :refY 5 :markerWidth 7 :markerHeight 7
                       :orient "auto-start-reverse"}
              [:path {:d "M0 0L10 5L0 10z"}]]]
      (for [{:keys [from to label highlight?]} edges
            :let [a (boxes from) b (boxes to)
                  [x1 y1 x2 y2] (if down?
                                  [(:x a) (+ (:y a) (/ (:h a) 2)) (:x b) (- (:y b) (/ (:h b) 2))]
                                  [(+ (:x a) (/ (:w a) 2)) (:y a) (- (:x b) (/ (:w b) 2)) (:y b)])]]
        [:g.edge {:replicant/key (str from "->" to) :class (when highlight? "on")}
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :marker-end "url(#x-arrow)"}]
         ;; beside the line, on the side it leans to
         (when label
           (let [side (if down? (if (neg? (- x2 x1)) -1 1) (if (neg? (- y2 y1)) -1 1))
                 mx (/ (+ x1 x2) 2) my (/ (+ y1 y2) 2)]
             (if down?
               [:text {:x (+ mx (* side 8)) :y (+ my 4) :text-anchor (if (neg? side) "end" "start")} label]
               [:text {:x mx :y (+ my (* side 14)) :text-anchor "middle"} label])))])
      (for [{:keys [id label highlight?]} nodes
            :let [{:keys [x y w h]} (boxes id) lines (str/split-lines (or label ""))]]
        [:g.node (cond-> {:replicant/key id :class [(when highlight? "on") (when clickable? "clickable")]}
                   clickable? (assoc :on {:click #(send! {:index index :id id})}))
         [:rect {:x (- x (/ w 2)) :y (- y (/ h 2)) :width w :height h :rx 6}]
         [:text {:x x :y (+ (- y (/ (* line-h (count lines)) 2)) 12) :text-anchor "middle"}
          (for [[i l] (map-indexed vector lines)]
            [:tspan {:replicant/key i :x x :dy (if (zero? i) 0 line-h)} l])]])]]))

(defn- steps [{:keys [index at count caption]} send!]
  [:div.x-steps
   [:div.x-steps-bar
    [:button.btn {:disabled (zero? at) :on {:click #(send! {:index index :step (dec at)})}} "← Back"]
    [:span.dots
     (for [i (range count)]
       [:button {:replicant/key i :class ["dot" (when (= i at) "on") (when (< i at) "done")]
                 :title (str "Step " (inc i)) :aria-label (str "Step " (inc i))
                 :on {:click #(send! {:index index :step i})}}])]
    [:button.btn.primary {:disabled (>= at (dec count)) :on {:click #(send! {:index index :step (inc at)})}} "Next →"]
    [:span.muted (str (inc at) " / " count)]]
   (when caption [:p.x-caption (cited caption)])])

(defn- predict [{:keys [index prompt options chosen answer correct? explain]} send!]
  [:div.x-predict
   [:p.x-prompt [:span.x-label "Predict first. "] (cited prompt)]
   [:div.options
    (for [[j o] (map-indexed vector options)]
      [:button.btn {:replicant/key j
                    :class [(when (and (some? chosen) (= j answer)) "right")
                            (when (and (= j chosen) (not correct?)) "wrong")]
                    :disabled (some? chosen)
                    :on {:click #(send! {:index index :value j})}}
       o])]
   (when (some? chosen)
     [:p.x-explain [:strong (if correct? "Right. " "Not quite. ")] (cited explain)])])

(defn element
  "Hiccup of one resolved element."
  [{:keys [type index] :as el} send!]
  [:div.x-el {:replicant/key (str index (name type))}
   (case type
     :text    [:p.x-text (cited (:text el))]
     :formula [:div.x-formula (marked (:text el) (:mark el))]
     :slider  (slider el send!)
     :toggle  (toggle el send!)
     :choice  (choice el send!)
     :button  [:button.btn {:on {:click #(send! {:index index})}} (:label el)]
     :table   (table el send!)
     :plot    (plot el)
     :diagram (diagram el send!)
     :steps   (steps el send!)
     :predict (predict el send!)
     :error   [:p.x-error "This part could not be shown: " (:message el)]
     [:p.x-error "Unknown element " (pr-str type)])])

(defn exploration
  "Hiccup of `spec` (normalized) in `state`. opts: :cite (fn [n] hiccup)
  drawing a citation [n]."
  ([spec state send!] (exploration spec state send! {}))
  ([spec state send! {:keys [cite]}]
   (binding [*cite* cite]
     [:div.exploration
      (when-let [intro (:intro spec)] [:p.x-intro (cited (explore/fill {} intro))])
      ;; doall: the citations are drawn while *cite* is bound
      (doall (for [el (explore/view spec state)] (element el send!)))
      [:div.x-foot [:button.btn.quiet {:on {:click #(send! :reset)}} "Start over"]]])))
