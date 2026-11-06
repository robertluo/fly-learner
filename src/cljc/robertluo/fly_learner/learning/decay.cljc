(ns robertluo.fly-learner.learning.decay
  "Forgetting, as an addition to learning: `decaying` wraps any
  robertluo.fly-learner.learning/Model, and the score it gives fades with
  time since the item was last recalled. Learning itself knows nothing of
  this; it only asks a model for a score at a time.

  Memory follows the FSRS idea: each item has a stability S (days), and the
  chance of recalling it t days after the last recall is
    R = (1 + t / 9S)^-1         (R = 0.9 when t = S)
  A good answer raises S, more so when it came close to being forgotten
  (the spacing effect); a failed one cuts S. The score is the wrapped
  model's score times R. Evidence other than graded answers (e.g. :seen)
  passes to the wrapped model and leaves memory alone."
  (:require [robertluo.fly-learner.learning :as learning]))

(def ^:private day-ms (* 24 60 60 1000))

(defn retrievability
  "Chance of recall `t` days after the last one, at stability `s` days."
  [s t]
  (/ 1.0 (+ 1 (/ (max 0 t) (* 9 s)))))

(def defaults
  {:first  3.0   ; S after a first fully right answer, days (scaled by its grade)
   :growth 2.0   ; how much a right answer multiplies S, at most
   :pass   0.6   ; grades from here on count as recalled
   :lapse  0.3   ; S is multiplied by this after a failed recall
   :floor  0.2}) ; S never drops below this, days

(defn next-stability
  "Stability after an answer graded `grade` (0..1), `t` days after the last
  recall at stability `s` (nil: first answer)."
  [{:keys [first growth pass lapse floor]} s t grade]
  (cond
    (nil? s)        (max floor (* first grade))
    (>= grade pass) (* s (+ 1 (* growth grade (- 1.05 (retrievability s t)))))
    :else           (max floor (* s lapse))))

(defrecord Decaying [base opts]
  learning/Model
  (init [_] {:base (learning/init base) :stability nil :last nil})
  (observe [_ state {:keys [kind grade at] :as e}]
    (let [state (update state :base #(learning/observe base % e))]
      (if (and (= :answer kind) grade)
        (let [{s :stability last :last} state
              t (when last (/ (- at last) day-ms))]
          (assoc state :stability (next-stability opts s t grade) :last at))
        state)))
  (score [_ {:keys [stability last] :as state} at]
    (let [b (learning/score base (:base state) at)]
      (if stability
        (* b (retrievability stability (/ (- at last) day-ms)))
        b))))

(defn decaying
  "`base` model, with scores fading by forgetting. opts: see `defaults`."
  ([base] (decaying base {}))
  ([base opts] (->Decaying base (merge defaults opts))))

^:rct/test
(comment
  (def day (* 24 60 60 1000))
  (def m (decaying (learning/mastery)))
  (def right (fn [d] {:item :x :kind :answer :grade 1.0 :at (* d day)}))
  (def state (fn [log] (get (learning/states m log) :x)))

  ;; right after an answer: the wrapped score; then it fades
  (def once (state [(right 0)]))
  (learning/score m once 0) ;=> 0.5
  (< (learning/score m once (* 30 day)) (learning/score m once (* 3 day)) 0.5) ;=> true
  ;; R = 0.9 when t = S (3 days after a first right answer)
  (Math/round (* 100 (/ (learning/score m once (* 3 day)) 0.5))) ;=> 90

  ;; spaced reviews build stability: reviewed at days 0, 3, 10 beats 0, 0.1, 0.2
  (> (:stability (state [(right 0) (right 3) (right 10)]))
     (:stability (state [(right 0) (right 0.1) (right 0.2)])))
  ;=> true

  ;; a failure cuts stability
  (< (:stability (state [(right 0) (right 3) {:item :x :kind :answer :grade 0.0 :at (* 4 day)}]))
     (:stability (state [(right 0) (right 3)])))
  ;=> true

  ;; exposure alone does not decay (there is no recall to forget)
  (learning/score m (state [{:item :x :kind :seen :at 0}]) (* 100 day)) ;=> 0.1
  )
