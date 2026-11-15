(ns robertluo.fly-learner.text
  "Text in any script. Chinese and Japanese are written without spaces
  between words, so what splits text into words (or lines into a
  paragraph) knows them: a CJK character is a word of its own, and a line
  break between two of them is no space (a hard-wrapped Chinese paragraph).

  `analyzer` is the knowledge base's full-text analyzer (Datalevin's English
  one, a run of Han characters indexed as its characters and their pairs, so
  that 黃帝 finds 「黃帝者，少典之子」); a db opened with `search-opts`
  uses it (robertluo.fly-learner.db/connect)."
  (:require [clojure.string :as str]
            [datalevin.analyzer :as analyzer]
            [datalevin.udf :as udf]))

(def ^:private cjk
  "A CJK character: Han, kana, CJK punctuation and full-width forms."
  "\\p{IsHan}\\p{IsHiragana}\\p{IsKatakana}\\x{3000}-\\x{303F}\\x{FF00}-\\x{FFEF}")

(def ^:private space
  "Whitespace, the no-break and the ideographic space too."
  "\\s\\x{00A0}\\x{3000}")

(def ^:private cjk-break
  (re-pattern (str "(?<=[" cjk "])[" space "]*\\R[" space "]*(?=[" cjk "])")))

(def ^:private spaces (re-pattern (str "[" space "]+")))

(defn squeeze
  "`s` with its whitespace as single spaces, none where a line breaks
  between two CJK characters; trimmed."
  [s]
  (-> (str s) (str/replace cjk-break "") (str/replace spaces " ") str/trim))

^:rct/test
(comment
  (squeeze "　　黃帝者，少典之子，\n  姓公孫。") ;=> "黃帝者，少典之子，姓公孫。"
  (squeeze " a\n  hard-wrapped\tline ") ;=> "a hard-wrapped line"
  (squeeze "史記 五帝本紀") ;=> "史記 五帝本紀"
  )

(defn cjk?
  "Does `s` hold a Han character or kana?"
  [s]
  (boolean (re-find #"[\p{IsHan}\p{IsHiragana}\p{IsKatakana}]" (str s))))

(defn words
  "The words of `s`, lower case: runs of letters and digits, a CJK
  character being one word."
  [s]
  (re-seq #"[\p{IsHan}\p{IsHiragana}\p{IsKatakana}]|[\p{L}\p{N}&&[^\p{IsHan}\p{IsHiragana}\p{IsKatakana}]]+"
          (str/lower-case (str s))))

^:rct/test
(comment
  (words "The λ-Calculus 2") ;=> ["the" "λ" "calculus" "2"]
  (words "黃帝者，Huang") ;=> ["黃" "帝" "者" "huang"]
  )

(defn tokens
  "About how many model tokens `s` takes: a CJK character about one, other
  text about 4 characters a token."
  [s]
  (let [s (str s)
        n (count (re-seq #"[\p{IsHan}\p{IsHiragana}\p{IsKatakana}\x{3000}-\x{303F}\x{FF00}-\x{FFEF}]" s))]
    (long (+ n (quot (- (count s) n) 4)))))

^:rct/test
(comment
  (tokens "abcdefgh") ;=> 2
  (tokens "黃帝者，少典之子") ;=> 8
  )

;;=============================================================================
;; Full-text search
;;=============================================================================

(def ^:private han-or-word
  (re-pattern (str "\\p{IsHan}+|[^\\p{IsHan}\\p{IsPunctuation}" cjk "]+")))

(defn analyzer
  "Datalevin's English analyzer, each run of Han characters in a term
  split into its characters and each pair of them: [term position offset].
  Other terms are as Datalevin has them, so English text is indexed as
  before."
  [^String s]
  (let [pos (volatile! -1)]
    (vec (mapcat (fn [[^String t _ off]]
                   (if-not (re-find #"\p{IsHan}" t)
                     [[t (vswap! pos inc) off]]
                     (let [m (re-matcher han-or-word t)]
                       (loop [out []]
                         (if-not (.find m)
                           out
                           (let [w (.group m), o (+ (long off) (.start m))]
                             (recur (into out (if (re-find #"^\p{IsHan}" w)
                                                (concat (for [i (range (count w))]
                                                          [(subs w i (inc i)) (vswap! pos inc) (+ o i)])
                                                        (for [i (range (dec (count w)))]
                                                          [(subs w i (+ i 2)) @pos (+ o i)]))
                                                [[w (vswap! pos inc) o]])))))))))
                 (analyzer/en-analyzer s)))))

^:rct/test
(comment
  (mapv first (analyzer "The Lambda calculus")) ;=> ["lambda" "calculus"]
  (mapv first (analyzer "黃帝者，少典")) ;=> ["黃" "帝" "者" "黃帝" "帝者" "少" "典" "少典"]
  (mapv first (analyzer "見 TeX 公式")) ;=> ["見" "tex" "公" "式" "公式"]
  )

(def ^:private analyzer-udf {:udf/lang :clj :udf/kind :analyzer :udf/id :fly-learner/analyzer})

(def ^:private registry
  (doto (udf/create-registry) (udf/register! analyzer-udf analyzer)))

(defn search-opts
  "Options for datalevin.core/get-conn: full-text search with `analyzer`.
  The db keeps the choice (by name; the function itself is bound at each
  opening), so every opening passes them."
  []
  {:search-opts  {:analyzer analyzer-udf}
   :runtime-opts {:udf-registry registry}})
