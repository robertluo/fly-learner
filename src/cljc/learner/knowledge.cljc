(ns learner.knowledge
  "Shared (JVM + browser) model of the folded knowledge DAG.

  A source (e.g. a PDF) is folded into layers of nodes:
    level 0        birdview: one node summarising the whole source
    level 1..n-1   progressively more detailed summaries
    level n        leaves: original source paragraphs

  Edges:
    :node/children  drill-down edges (higher level -> lower level)
    :node/links     cross links created from shared keywords; these make
                    the structure a DAG rather than a tree")

(def NodeId :uuid)

(def Node
  [:map
   [:node/id NodeId]
   [:node/level :int]
   [:node/title :string]
   [:node/summary {:optional true} :string]
   [:node/text {:optional true} :string]
   [:node/keywords {:optional true} [:set :string]]
   [:node/page {:optional true} :int]
   [:node/order {:optional true} :int]
   [:node/children {:optional true} [:vector NodeId]]
   [:node/links {:optional true} [:vector NodeId]]])

(def QuestionKind
  "`:understand` probes comprehension of a node at any level.
  `:memorize` drills facts that need recall (vocabulary, grammar, ...)."
  [:enum :understand :memorize])

(def Question
  [:map
   [:question/id :uuid]
   [:question/node NodeId]
   [:question/kind QuestionKind]
   [:question/prompt :string]
   [:question/answer :string]])

(defn leaf? [node] (empty? (:node/children node)))
