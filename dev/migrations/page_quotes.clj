(ns migrations.page-quotes
  "One-off (2026-11-09): a quote selected on a page of the source document
  was {:field :pdf}, when every source was a PDF; with EPUBs it is
  {:field :page} (robertluo.fly-learner.ideas.knowledge).

    (plan (d/db conn))      the ideas that would change, transacting nothing
    (migrate! conn)         do it, in one transaction"
  (:require [datalevin.core :as d]))

(defn- renamed [anchors]
  (mapv #(cond-> % (= :pdf (:field %)) (assoc :field :page)) anchors))

(defn plan
  "[[idea-entity new-anchors] ..] for db `db`."
  [db]
  (vec (for [[e anchors] (d/q '[:find ?e ?a :where [?e :idea/id] [?e :idea/anchors ?a]] db)
             :let [new (renamed anchors)]
             :when (not= anchors new)]
         [e new])))

(defn migrate!
  "Rename the field of quotes on a page in the db of `conn`. Returns how many ideas changed."
  [conn]
  (let [p (plan (d/db conn))]
    (when (seq p) (d/transact! conn (for [[e a] p] [:db/add e :idea/anchors a])))
    (count p)))
