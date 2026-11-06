(ns learner.ingest.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [learner.ingest.pdf :as pdf]))

(deftest clean-text-joins-wrapped-lines
  (is (= "First para spans lines with hyphenated word."
         (pdf/clean-text "First para\nspans lines with hyphen-\nated word."))))

(deftest headings
  (is (= {:depth 2 :title "1.1 System Fω"} (pdf/heading "1.1 System Fω")))
  (is (= 3 (:depth (pdf/heading "2.3.1 Erasure"))))
  (is (nil? (pdf/heading "1.1 System Fω . . . . . . 2")))
  (is (nil? (pdf/heading "2. tags ∩, pair, proj1 represent dependent intersections;"))))

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
