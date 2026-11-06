(ns robertluo.fly-learner.ingest.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [robertluo.fly-learner.ingest.pdf :as pdf]))

(deftest clean-text-joins-wrapped-lines
  (is (= "First para spans lines with hyphenated word."
         (pdf/clean-text "First para\nspans lines with hyphen-\nated word."))))

(deftest headings
  (is (= {:depth 2 :title "1.1 System Fω"} (pdf/heading "1.1 System Fω")))
  (is (= 3 (:depth (pdf/heading "2.3.1 Erasure"))))
  (is (nil? (pdf/heading "1.1 System Fω . . . . . . 2")))
  (is (nil? (pdf/heading "2. tags ∩, pair, proj1 represent dependent intersections;")))
  (is (nil? (pdf/heading "2.5 Perfect-information games with two players ............ 81")) "a contents line")
  (is (= 1 (:depth (pdf/heading "OBJECT NORMALIZATION"))))
  (is (nil? (pdf/heading "L R")) "a game tree's labels")
  (is (nil? (pdf/heading "A N N"))))

(deftest contents-pages-are-dropped
  (is (pdf/contents-page? ["2.1 Trees, frames and games ........ 72" "2.2 Backward induction 76"
                           "2.3 Strategies 80" "2.4 Relationship 83" "12.1 Beliefs 425" "Exercises 90"]))
  (is (not (pdf/contents-page? ["A rooted directed tree consists of a set of nodes and edges."
                                "Definition 2.1. A finite extensive form consists of"
                                "a set of nodes, as in Figure 2.1" "and a root" "with arrows" "x = 1"]))))

(deftest running-headers-are-dropped
  (let [body  "is long enough to be a paragraph of its own, not a fragment of a formula or a figure."
        pages (for [p (range 1 7)] [p [(str "GAME THEORY – Giacomo Bonanno\nPage " p " " body)]])]
    (is (= #{"GAME THEORY – Giacomo Bonanno"} (pdf/running-lines pages)))
    (is (= {:kind :text :text (str "Page 1 " body) :page 1} (first (pdf/blocks pages))))))

(deftest chapters-from-numbered-sections
  (is (= [{:kind :heading :depth 1 :title "2 Dynamic games with perfect information" :page 72}
          {:kind :heading :depth 2 :title "2.1 Trees, frames and games" :page 72}
          {:kind :text :text "Often interactions are not simultaneous but sequential, as in chess." :page 72}
          {:kind :heading :depth 2 :title "2.2 Backward induction" :page 76}
          {:kind :heading :depth 1 :title "3 Chapter 3" :page 90}
          {:kind :heading :depth 2 :title "3.1 Imperfect information" :page 90}]
         (pdf/blocks [[72 ["Dynamic games with perfect information" "2.1 Trees, frames and games"
                           "Often interactions are not simultaneous but sequential, as in chess."]]
                      [76 ["2.2 Backward induction"]]
                      [90 ["3.1 Imperfect information"]]])))
  (is (= ["Games with Ordinal Payoffs" "1 Ordinal games in strategic form" "1.1 Game frames and games"]
         (map #(or (:title %) (:text %))
              (pdf/blocks [[12 ["Games with Ordinal Payoffs\nOrdinal games in strategic form\n1.1. Game frames and games"]]])))
      "a chapter opening set as one block; the part title above is not the chapter's")
  (is (= "2 Dynamic games with perfect information"
         (:title (first (pdf/blocks [[72 ["Dynamic games with" "perfect information" "2.1 Trees"]]]))))
      "a title over two lines")
  (is (= ["1 Introduction" "1.1 System Fω"]
         (map :title (pdf/blocks [[10 ["Chapter 1" "INTRODUCTION" "1.1 System Fω"]]])))
      "chapters already there are kept"))

(deftest blocks
  (let [bs (pdf/blocks [[10 ["Chapter 1" "INTRODUCTION" "Type theory is a tool for reasoning about assertions of a domain. It is used for"]]
                        [11 ["11" "reasoning about programs." "1.1 System Fω"
                             "x ::= a" "y ::= b" "z ::= c" "Figure 1.1: Syntax."]]])]
    (is (= [{:kind :heading :depth 1 :title "1 Introduction" :page 10}
            {:kind :text :text "Type theory is a tool for reasoning about assertions of a domain. It is used for reasoning about programs." :page 10}
            {:kind :heading :depth 2 :title "1.1 System Fω" :page 11}
            {:kind :text :text "x ::= a\ny ::= b\nz ::= c\nFigure 1.1: Syntax." :page 11}]
           bs))))

(deftest chapter-title-casing
  (is (= "5 Object Normalization and φ the Foil"
         (:title (first (pdf/blocks [[1 ["Chapter 5" "OBJECT NORMALIZATION AND φ THE FOIL"]]]))))))
