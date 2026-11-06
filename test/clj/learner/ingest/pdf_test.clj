(ns learner.ingest.pdf-test
  (:require [clojure.test :refer [deftest is]]
            [learner.ingest.pdf :as pdf]))

(deftest split-paragraphs-joins-wrapped-lines
  (is (= ["First para spans lines." "Second hyphenated word."]
         (pdf/split-paragraphs "First para\nspans lines.\n\nSecond hyphen-\nated word.\n"))))
