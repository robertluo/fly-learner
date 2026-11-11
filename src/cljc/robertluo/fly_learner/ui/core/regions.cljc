(ns robertluo.fly-learner.ui.core.regions
  "The map's regions, drawn as a world map draws its regions: each domain (a
  group of topics, robertluo.fly-learner.concept.domain) an area of land
  around its topics, coloured, and named in large letters. Domains overlap
  (a topic may be in several), so each area is drawn on its own,
  translucent, and the colours mix where they overlap.

  An area is where a field is below zero: the distance to the nearest of its
  topics' bubbles, less a margin, and to the bridges between them (a
  shortest tree over its topics, the long hops left out: far parts are
  islands), joined by a smooth minimum so the shape is a blob, not circles.
  Its outline is traced on a grid (marching squares) into rings, an SVG path.

  Pure and deterministic, so it is tested on the JVM.")

(def ^:private cell "Grid step, in map units." 16)
(def ^:private margin "Land beyond a topic's bubble." 56)
(def ^:private bridge-margin "Half the width of the land joining two topics." 44)
(def ^:private max-bridge "Topics further apart than this are not joined: islands." 420)
(def ^:private smooth "How round the joins are (smooth minimum)." 60)

(def colors "How many region colours the stylesheet has (.region.c0 ..)." 8)

(defn- smin
  "Smooth minimum of `a` and `b` (polynomial, width `k`)."
  [a b k]
  (let [h (max 0.0 (- k (abs (- a b))))]
    (- (min a b) (/ (* h h) (* 4.0 k)))))

(defn- seg-dist
  "Distance from [px py] to the segment [[ax ay] [bx by]]."
  [px py [[ax ay] [bx by]]]
  (let [dx (- bx ax), dy (- by ay)
        l2 (+ (* dx dx) (* dy dy))
        t  (if (zero? l2) 0.0 (-> (/ (+ (* (- px ax) dx) (* (- py ay) dy)) l2) (max 0.0) (min 1.0)))
        qx (+ ax (* t dx)), qy (+ ay (* t dy))]
    (Math/sqrt (+ (* (- px qx) (- px qx)) (* (- py qy) (- py qy))))))

(defn- dist [[ax ay] [bx by]] (Math/sqrt (+ (* (- ax bx) (- ax bx)) (* (- ay by) (- ay by)))))

(defn tree
  "The edges [[i j] ..] of a shortest tree over points `ps` (Prim), leaving
  out those longer than `limit`."
  [ps limit]
  (let [n (count ps)]
    (if (< n 2)
      []
      (loop [in #{0}, out []]
        (if (= n (count in))
          (filterv (fn [[i j]] (<= (dist (ps i) (ps j)) limit)) out)
          (let [[i j] (apply min-key (fn [[i j]] (dist (ps i) (ps j)))
                             (for [i (sort in), j (range n) :when (not (in j))] [i j]))]
            (recur (conj in j) (conj out [i j]))))))))

^:rct/test
(comment
  (tree [[0 0] [10 0] [10 10] [500 500]] 100) ;=> [[0 1] [1 2]]
  (tree [[0 0]] 100) ;=> []
  )

(defn- field
  "The area's field at [x y]: below zero on land. `discs` [[x y r] ..],
  `bridges` [[[ax ay] [bx by]] ..]."
  [discs bridges x y]
  (let [d (reduce (fn [acc [cx cy r]]
                    (smin acc (- (dist [x y] [cx cy]) r margin) smooth))
                  1e9 discs)]
    (reduce (fn [acc s] (smin acc (- (seg-dist x y s) bridge-margin) smooth)) d bridges)))

;; marching squares: corners tl tr br bl (bits 8 4 2 1, set = land); the
;; segments of each case join cell sides :t :r :b :l. Saddles (5, 10) join by
;; the field at the cell's centre.
(def ^:private cases
  {1 [[:l :b]] 2 [[:b :r]] 3 [[:l :r]] 4 [[:t :r]] 6 [[:t :b]] 7 [[:l :t]]
   8 [[:l :t]] 9 [[:t :b]] 11 [[:t :r]] 12 [[:l :r]] 13 [[:b :r]] 14 [[:l :b]]})

(defn- saddle [c centre-land?]
  (case c
    5  (if centre-land? [[:l :t] [:b :r]] [[:t :r] [:l :b]])
    10 (if centre-land? [[:t :r] [:l :b]] [[:l :t] [:b :r]])))

(defn- r1 [x] (/ (Math/round (* 10.0 x)) 10.0))

(defn rings
  "The outlines of where `f` (fn [x y]) is below zero within [x0 y0 x1 y1],
  traced on a grid of `step`: [[[x y] ..] ..], each ring closed (its last
  point joins its first). The box should hold the whole area with a cell to
  spare, so every ring closes."
  [f [x0 y0 x1 y1] step]
  (let [nx (inc (long (Math/ceil (/ (- x1 x0) step))))
        ny (inc (long (Math/ceil (/ (- y1 y0) step))))
        v  (vec (for [j (range ny)] (vec (for [i (range nx)] (double (f (+ x0 (* i step)) (+ y0 (* j step))))))))
        at (fn [i j] (get-in v [j i]))
        ;; where the outline crosses a side, by linear interpolation
        cross (fn [[kind i j]]
                (let [[a b] (if (= kind :h) [(at i j) (at (inc i) j)] [(at i j) (at i (inc j))])
                      t (if (= a b) 0.5 (/ a (- a b)))]
                  (if (= kind :h)
                    [(r1 (+ x0 (* step (+ i t)))) (r1 (+ y0 (* step j)))]
                    [(r1 (+ x0 (* step i))) (r1 (+ y0 (* step (+ j t))))])))
        side  (fn [i j s] (case s :t [:h i j] :b [:h i (inc j)] :l [:v i j] :r [:v (inc i) j]))
        segs  (for [j (range (dec ny)), i (range (dec nx))
                    :let [land? #(neg? %)
                          c (+ (if (land? (at i j)) 8 0) (if (land? (at (inc i) j)) 4 0)
                               (if (land? (at (inc i) (inc j))) 2 0) (if (land? (at i (inc j))) 1 0))]
                    :when (< 0 c 15)
                    [a b] (or (cases c)
                              (saddle c (land? (f (+ x0 (* step (+ i 0.5))) (+ y0 (* step (+ j 0.5)))))))]
                [(side i j a) (side i j b)])
        ;; each crossing is shared by exactly two segments: walk them into rings
        by    (reduce (fn [m [k [a b]]] (-> m (update a (fnil conj []) k) (update b (fnil conj []) k)))
                      {} (map-indexed vector segs))
        segv  (vec segs)]
    (loop [used #{}, out [], k 0]
      (cond
        (>= k (count segv)) out
        (used k)            (recur used out (inc k))
        :else
        (let [[start nxt] (segv k)
              ring (loop [used (conj used k), pts [start], p nxt]
                     (if (= p start)
                       [used pts]
                       (let [k2 (first (remove used (by p)))]
                         (if (nil? k2)
                           [used pts] ; an open end (the box too small): stop here
                           (let [[a b] (segv k2)]
                             (recur (conj used k2) (conj pts p) (if (= a p) b a)))))))]
          (recur (first ring) (conj out (mapv cross (second ring))) (inc k)))))))

^:rct/test
(comment
  ;; a disc of radius 50: one ring, its points about 50 from the centre
  (def rs (rings (fn [x y] (- (Math/sqrt (+ (* x x) (* y y))) 50)) [-80 -80 80 80] 10))
  (count rs) ;=> 1
  (every? (fn [[x y]] (< 48 (Math/sqrt (+ (* x x) (* y y))) 52)) (first rs)) ;=> true
  ;; two discs far apart: two rings
  (count (rings (fn [x y] (min (- (Math/sqrt (+ (* (- x 100) (- x 100)) (* y y))) 30)
                               (- (Math/sqrt (+ (* (+ x 100) (+ x 100)) (* y y))) 30)))
                [-160 -60 160 60] 10))
  ;=> 2
  )

(defn- path-d
  "SVG path data for `rings` (even-odd fill: a ring inside another is a hole)."
  [rings]
  (apply str (for [ring rings :when (> (count ring) 2)
                   :let [[[x y] & more] ring]]
               (str "M" x " " y (apply str (for [[x y] more] (str "L" x " " y))) "Z"))))

(defn- label-box [{:keys [lx ly font label]}]
  (let [w (* 0.7 font (count label))]
    [(- lx (/ w 2)) (- ly font) (+ lx (/ w 2)) (+ ly (* 0.3 font))]))

(defn- boxes-meet? [[a0 b0 a1 b1] [c0 d0 c1 d1]]
  (and (< a0 c1) (< c0 a1) (< b0 d1) (< d0 b1)))

(defn- place-labels
  "Labels that do not cover each other: each (largest domain first) at its
  spot, else moved up or down by a line or two."
  [regions]
  (reduce (fn [out {:keys [ly font] :as r}]
            (let [tries (for [k [0 1 -1 2 -2 3 -3]] (assoc r :ly (+ ly (* k 1.3 font))))
                  ok    (some (fn [t] (when (not-any? #(boxes-meet? (label-box t) (label-box %)) out) t)) tries)]
              (conj out (or ok r))))
          [] regions))

(defn- palette
  "A colour index per region (largest first): one no region it overlaps or
  touches has (`near?`), the least used so far, so far apart regions differ
  too."
  [regions near?]
  (reduce (fn [out r]
            (let [taken (set (keep #(when (near? r %) (:color %)) out))
                  used  (frequencies (map :color out))
                  free  (remove taken (range colors))
                  c     (if (seq free)
                          (apply min-key #(+ (* 100 (get used % 0)) %) free)
                          (mod (count out) colors))]
              (conj out (assoc r :color c))))
          [] regions))

(defn regions
  "What the map draws of `domains` ([{:domain/id :domain/label :domain/gloss
  :topics [id ..] :core [id ..] :at [x y]}], largest first) around `bubbles`
  {id {:x :y :r}}: [{:key :label :gloss :d :color :lx :ly :font :box [x0 y0
  x1 y1]}]. The name goes at :at if given and on land, else over the middle
  of the core. A domain with no name yet is drawn, not named."
  [domains bubbles]
  (let [rs (vec (for [[i {:domain/keys [id label gloss] :keys [topics core at]}] (map-indexed vector domains)
                      :let [bs (vec (keep bubbles topics))]
                      :when (seq bs)
                      :let [ps      (mapv (juxt :x :y) bs)
                            discs   (mapv (juxt :x :y :r) bs)
                            bridges (mapv (fn [[a b]] [(ps a) (ps b)]) (tree ps max-bridge))
                            f       #(field discs bridges %1 %2)
                            reach   (+ margin smooth (* 2 cell))
                            box     [(- (apply min (map #(- (:x %) (:r %)) bs)) reach)
                                     (- (apply min (map #(- (:y %) (:r %)) bs)) reach)
                                     (+ (apply max (map #(+ (:x %) (:r %)) bs)) reach)
                                     (+ (apply max (map #(+ (:y %) (:r %)) bs)) reach)]
                            ;; the name over the middle of its core (by area), if that is land
                            cs      (or (seq (keep bubbles core)) bs)
                            area    (reduce + (map #(* (:r %) (:r %)) cs))
                            mid     [(/ (reduce + (map #(* (:x %) (:r %) (:r %)) cs)) area)
                                     (/ (reduce + (map #(* (:y %) (:r %) (:r %)) cs)) area)]
                            [lx ly] (cond (and at (neg? (apply f at))) at
                                          (neg? (apply f mid))         mid
                                          :else                        ((juxt :x :y) (apply max-key :r cs)))
                            width   (- (nth box 2) (first box))]]
                  {:key    (or id (str "domain-" i))
                   :label  label
                   :gloss  gloss
                   :topics (set topics)
                   :d      (path-d (rings f box cell))
                   :lx     lx
                   :ly     ly
                   :font   (-> (/ (* 0.8 width) (* 0.7 (max 6 (count label)))) (max 30) (min 60))
                   :box    box}))
        near? (fn [a b] (or (some (:topics a) (:topics b)) (boxes-meet? (:box a) (:box b))))]
    (->> (palette rs near?)
         place-labels
         (mapv #(dissoc % :topics)))))

^:rct/test
(comment
  ;; two domains sharing topic :m: each one area, different colours, named
  (def bubbles {:a {:x 0 :y 0 :r 20} :b {:x 120 :y 0 :r 20} :m {:x 240 :y 0 :r 20}
                :x {:x 360 :y 0 :r 20} :y {:x 480 :y 0 :r 20}})
  (def rs (regions [{:domain/id 1 :domain/label "Left" :topics [:a :b :m] :core [:a :b]}
                    {:domain/id 2 :domain/label "Right" :topics [:m :x :y] :core [:x :y]}]
                   bubbles))
  (mapv :key rs) ;=> [1 2]
  (mapv :color rs) ;=> [0 1]
  ;; far apart regions take colours not used yet
  (mapv :color (regions [{:topics [:a] :core [:a]} {:topics [:y] :core [:y]}] bubbles)) ;=> [0 1]
  (every? #(re-find #"^M.*Z$" (:d %)) rs) ;=> true
  ;; one closed outline each: the topics are joined, not islands
  (mapv #(count (re-seq #"M" (:d %))) rs) ;=> [1 1]
  ;; the name sits over its core
  (mapv :lx rs) ;=> [60 420]
  ;; a domain with no name yet is drawn all the same
  (:key (first (regions [{:topics [:a :b] :core [:a :b]}] bubbles))) ;=> "domain-0"
  )
