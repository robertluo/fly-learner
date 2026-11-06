(ns learner.knowledge
  "Shared (JVM + browser) model of the folded knowledge DAG.

  A source (e.g. a PDF) is folded into layers of nodes following its own
  structure:
    level 0     :source     birdview summary of the whole source
    level 1..   :section    chapters, sections, ... (heading kept as title)
                :group      a run of paragraphs inside a long section
    leaves      :paragraph  original source text (leaf depth varies)

  Edges:
    :node/children  drill-down edges (higher level -> lower level)
    :node/links     cross links created from shared keywords; these make
                    the structure a DAG rather than a tree")

(def NodeId :uuid)

(def Node
  [:map
   [:node/id NodeId]
   [:node/level :int]
   [:node/kind [:enum :source :section :group :paragraph]]
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
