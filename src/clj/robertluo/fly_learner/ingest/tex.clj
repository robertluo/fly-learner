(ns robertluo.fly-learner.ingest.tex
  "TeX formulas in Markdown, read as GitHub reads them: an extension of
  commonmark-java's parser (`parser-extension`) and HTML renderer
  (`renderer-extension`).

    $x^2$      inline: the $ opening it is followed by a non-space, the $
               closing it preceded by one and followed by no digit; it
               holds no other $ (but \\$), so \"costs $5 or $10\" stays
               text; \\$ is a dollar
    $$x$$      display, in a paragraph or alone on a line
    $$         display, the lines between delimiter lines (a blank line
    \\int ..    ends an unclosed one: then it was text)
    $$

  plus ```math fences (`math-fences!`, on the rendered HTML). Code is never
  read for formulas. A formula is rendered as an element the browser
  typesets (KaTeX), its text the formula in its delimiters, which is what
  the knowledge base reads:

    <span data-tex=\"inline\">$x^2$</span>   <div data-tex=\"display\">$$x$$</div>"
  (:require [clojure.string :as str])
  (:import (clojure.lang IDeref)
           (org.commonmark.node CustomBlock CustomNode Node)
           (org.commonmark.parser Parser$Builder Parser$ParserExtension SourceLine)
           (org.commonmark.parser.beta InlineContentParser InlineContentParserFactory InlineParserState
                                       ParsedInline Scanner)
           (org.commonmark.parser.block AbstractBlockParser BlockContinue BlockParser BlockParserFactory
                                        BlockStart ParserState)
           (org.commonmark.renderer NodeRenderer)
           (org.commonmark.renderer.html HtmlNodeRendererContext HtmlNodeRendererFactory HtmlRenderer$Builder
                                         HtmlRenderer$HtmlRendererExtension HtmlWriter)
           (org.jsoup.nodes Document Element)))

;; Formula nodes: their TeX behind deref, {:tex :display? :closed?}

(defn- inline-node ^Node [tex display?]
  (proxy [CustomNode IDeref] [] (deref [] {:tex tex :display? display? :closed? true})))

(def ^:private inline-class (class (inline-node "" false)))

(defn- content [^Scanner sc from to]
  (.getContent (.getSource sc from to)))

(defn- try-inline
  "At a $ in a paragraph: the formula starting there, or none."
  [^InlineParserState state]
  (let [^Scanner sc (.scanner state)
        n           (.matchMultiple sc \$)
        open        (.position sc)]
    (case n
      1 (if (or (not (.hasNext sc)) (Character/isWhitespace (.peek sc)))
          (ParsedInline/none)
          (loop []
            (if (neg? (.find sc \$))
              (ParsedInline/none)
              (let [before (.peekPreviousCodePoint sc)
                    close  (.position sc)]
                (.next sc)
                (let [after (.peek sc)]
                  (cond
                    ;; an escaped dollar inside the formula
                    (= before (int \\)) (recur)
                    ;; a dollar that cannot close it: no formula holds one ("$5 or $10")
                    (or (Character/isWhitespace before) (Character/isDigit after) (= after \$))
                    (ParsedInline/none)
                    :else
                    (ParsedInline/of (inline-node (content sc open close) false) (.position sc))))))))
      2 (loop []
          (if (neg? (.find sc \$))
            (ParsedInline/none)
            (let [close (.position sc)]
              (if (= 2 (.matchMultiple sc \$))
                (let [tex (content sc open close)]
                  (if (str/blank? tex)
                    (ParsedInline/none)
                    (ParsedInline/of (inline-node tex true) (.position sc))))
                (recur)))))
      (ParsedInline/none))))

(def ^:private inline-factory
  (reify InlineContentParserFactory
    (getTriggerCharacters [_] #{\$})
    (create [_]
      (reify InlineContentParser
        (tryParse [_ state] (try-inline state))))))

(defn- display-parser
  "A $$ block's parser: its lines up to the closing $$ (or a blank line)."
  ^BlockParser []
  (let [lines (atom []), done (atom false), closed (atom false)
        block (proxy [CustomBlock IDeref] []
                (deref [] {:tex (str/trim (str/join "\n" @lines)) :display? true :closed? @closed}))]
    (proxy [AbstractBlockParser] []
      (getBlock [] block)
      (tryContinue [^ParserState st]
        (let [line (str/trim (str (.getContent (.getLine st))))]
          (cond @done                          (BlockContinue/none)
                (str/blank? line)              (BlockContinue/none)
                (str/starts-with? line "$$")   (do (reset! closed true) (BlockContinue/finished))
                :else                          (BlockContinue/atIndex (.getIndex st)))))
      (addLine [^SourceLine line]
        (let [l (str/trimr (str (.getContent line)))]
          (if (str/ends-with? l "$$")
            (do (swap! lines conj (subs l 0 (- (count l) 2))) (reset! done true) (reset! closed true))
            (swap! lines conj l)))))))

(def ^:private display-class (class (.getBlock ^BlockParser (display-parser))))

(def ^:private display-factory
  (reify BlockParserFactory
    (tryStart [_ st _]
      (let [^ParserState st st
            line (str (.getContent (.getLine st)))
            i    (.getNextNonSpaceIndex st)]
        ;; a block: the line opens one ($$ ..), or is one ($$ .. $$); a line
        ;; going on after a formula ($$x$$ is ..) is a paragraph
        (if (and (< (.getIndent st) 4)
                 (let [l (str/trimr (subs line i))]
                   (and (str/starts-with? l "$$")
                        (let [more (subs l 2)]
                          (or (not (str/includes? more "$$"))
                              (= (str/index-of more "$$") (- (count more) 2)))))))
          (.atIndex (BlockStart/of (into-array BlockParser [(display-parser)])) (+ i 2))
          (BlockStart/none))))))

(def parser-extension
  "Formulas for a commonmark Parser (its builder's .extensions)."
  (reify Parser$ParserExtension
    (^void extend [_ ^Parser$Builder b]
      (.customBlockParserFactory b display-factory)
      (.customInlineContentParserFactory b inline-factory))))

(defn- render! [^HtmlWriter w node]
  (let [{:keys [tex display? closed?]} @node
        block? (instance? CustomBlock node)]
    (cond
      (not closed?) (do (.line w) (.tag w "p" {}) (.text w (str "$$" tex)) (.tag w "/p") (.line w))
      block?        (do (.line w) (.tag w "div" {"data-tex" "display"}) (.text w (str "$$" tex "$$"))
                        (.tag w "/div") (.line w))
      :else         (do (.tag w "span" {"data-tex" (if display? "display" "inline")})
                        (.text w (if display? (str "$$" tex "$$") (str "$" tex "$")))
                        (.tag w "/span")))))

(def renderer-extension
  "Formulas for a commonmark HtmlRenderer (its builder's .extensions)."
  (reify HtmlRenderer$HtmlRendererExtension
    (^void extend [_ ^HtmlRenderer$Builder b]
      (.nodeRendererFactory
       b (reify HtmlNodeRendererFactory
           (create [_ ctx]
             (let [w (.getWriter ^HtmlNodeRendererContext ctx)]
               (reify NodeRenderer
                 (getNodeTypes [_] #{inline-class display-class})
                 (render [_ node] (render! w node))))))))))

(defn math-fences!
  "```math fences in `doc` as display formulas."
  [^Document doc]
  (doseq [^Element code (.select doc "pre > code.language-math")]
    (.replaceWith (.parent code)
                  (doto (Element. "div") (.attr "data-tex" "display") (.text (str "$$" (str/trim (.wholeText code)) "$$")))))
  doc)

^:rct/test
(comment
  (def ^:private md
    (let [ext [parser-extension renderer-extension]
          p   (.build (.extensions (org.commonmark.parser.Parser/builder) ext))
          r   (.build (.extensions (org.commonmark.renderer.html.HtmlRenderer/builder) ext))]
      #(str/trim (.render r (.parse p %)))))
  (md "costs $5 or $10, and $x^2$") ;=> "<p>costs $5 or $10, and <span data-tex=\"inline\">$x^2$</span></p>"
  (md "$$\\frac{a}{b}$$ in text, \\$y$ and `$NAME`") ;=> "<p><span data-tex=\"display\">$$\\frac{a}{b}$$</span> in text, $y$ and <code>$NAME</code></p>"
  (md "$$\n\\int_0^1 x\\,dx\n$$\nafter") ;=> "<div data-tex=\"display\">$$\\int_0^1 x\\,dx$$</div>\n<p>after</p>"
  (md "$$ never closed\n\nnext") ;=> "<p>$$never closed</p>\n<p>next</p>"
  )
