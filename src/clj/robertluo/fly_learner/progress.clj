(ns robertluo.fly-learner.progress
  "Record answers and schedule reviews so weak points keep being challenged.
  Scheduling is a simplified SM-2."
  (:require [datalevin.core :as d])
  (:import (java.time Duration Instant)
           (java.util Date)))

(def initial {:review/ease 2.5 :review/interval 0})

(defn next-review
  "Pure SM-2 step. `correct?` -> grow interval, otherwise reset and lower ease."
  [{:review/keys [ease interval] :as review} correct? ^Instant now]
  (let [ease'     (if correct? ease (max 1.3 (- ease 0.2)))
        interval' (cond (not correct?) 1
                        (zero? interval) 1
                        (= 1 interval) 3
                        :else (long (Math/round (double (* interval ease')))))]
    (assoc review
           :review/ease ease'
           :review/interval interval'
           :review/due (Date/from (.plus now (Duration/ofDays interval'))))))

(defn record-attempt!
  [conn question-id node-id response correct?]
  (let [db     (d/db conn)
        review (or (d/pull db '[*] [:review/node [:node/id node-id]]) initial)
        now    (Instant/now)]
    (d/transact! conn
                 [{:attempt/question [:question/id question-id]
                   :attempt/response response
                   :attempt/correct? correct?
                   :attempt/at       (Date/from now)}
                  (-> (next-review review correct? now)
                      (select-keys [:review/ease :review/interval :review/due])
                      (assoc :review/node [:node/id node-id]))])))

(defn due
  "Node ids due for review, most overdue first."
  [db ^Date now]
  (->> (d/q '[:find ?nid ?due
              :in $ ?now
              :where [?r :review/due ?due]
              [(<= ?due ?now)]
              [?r :review/node ?n]
              [?n :node/id ?nid]]
            db now)
       (sort-by second)
       (mapv first)))
