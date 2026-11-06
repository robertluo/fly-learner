# Learn anything

This project is a personal learner for knowledge assisted by AI.

## Source of knowledge

Should have multiple sources.

 - PDF files. The first source.

## Knowledge breakdown

Ingest source, fold it by AI models to a multi layered view: DAG based, can store inside a database (datalevin for now).

From highest level (birdview) to detailed (source paragraphs). In this way, the user does not need to read word for word like before - and human always lose track when they doing so.

A higher level can always drill down to lower levels, and linked keyword can turn into links, where user can click.

https://github.com/chenxiachan/textfold is a good reference.

## Learn method for user

Learn means understanding, memorizing. Memorizing comes after real understanding.

AI can generate questions on any level of knowledge on demand, using tests to avoid humans illusion of competent.

 - Record the user's progress, will continue challege on weak point.
 - For memorizing problems (like in foreign language's vocabulary or grammer)


## Tech

In Clojure/Clojurescript. https://github.com/bhauman/clojure-mcp-light

## Roadmap

  1. Backend first: the knowledge base from 1 source (we may have multiple sources, and they can interlink)
  2. Frontend, a.k.a, user interface.

## Progress

### 1. Backend: knowledge base from one source (in progress)

Done:

 - PDF ingest (PDFBox): paragraphs, page numbers dropped, paragraphs cut by page breaks or formulas rejoined, formula fragments kept as one block.
 - Structure recovered from headings ("Chapter 1", "1.1 ..."), since PDFs often have no outline. Cover pages, contents, list of figures, acknowledgments and bibliography are skipped.
 - Folding into a layered DAG that follows the document's own structure: birdview -> chapters -> sections -> paragraph groups -> source paragraphs. Every node above a paragraph has a title, summary and keywords; nodes sharing specific keywords are cross-linked.
 - Stored in Datalevin; re-ingesting a file replaces its earlier version. Each fold is also saved to `data/folds/`.
 - Models: Sonnet 5.5 for ideas that need synthesis (sections, chapters, whole document, quiz questions), Haiku 5.5 for routine paragraph groups. The system prompt (with the document outline) is cached across calls.
 - First source folded: a 116-page thesis -> 1,052 nodes in 68 s for about $0.23 (Opus 5.5 alone: $2.31).

To do:

 - Group paragraphs by length or by Lemma/Proof boundaries instead of fixed runs of 6.
 - Try quiz generation and progress tracking (SM-2 reviews) on real data; both are written but not yet run against it.
 - Multiple sources and links between them.

### 2. Frontend (in progress)

Same stack as [flybot.sg](https://www.flybot.sg) ([source](https://github.com/flybot-sg/lasagna-pattern/tree/main/examples/flybot-site)): Replicant UI with an effect-map dispatch, a single pull-pattern API ([lasagna-pattern](https://github.com/flybot-sg/lasagna-pattern)) over Transit, http-kit + fun-map on the server, Kaocha + Rich Comment Tests. Storage stays on Datalevin.

Done:

 - Browse a source from the birdview down: breadcrumb, summary, keywords, drill-down cards (paragraph text shown inline), related nodes.
 - The original PDF beside it (pdf.js), on the selected node's page, with paging.
 - Keywords are links: each opens every node tagged with it.
 - URLs per node (`/n/<id>`) and keyword (`/k/<keyword>`), back/forward work.

To do:

 - Highlight the selected paragraph on the PDF page (pdf.js text layer).
 - Quiz and review screens.

## Development

Inside `devenv shell` (JDK, Clojure, Node, babashka, clojure-mcp-light):

 - `bb dev` starts an nREPL; in it `(user/start!)` runs the backend (http://localhost:8080) and `(user/cljs!)` the frontend watch (http://localhost:3000)
 - `bb serve` builds the frontend and serves everything from http://localhost:8080
 - `bb test`, `bb lint`
 - Anthropic API key: `ANTHROPIC_API_KEY`, or the macOS keychain item named in `resources/config.edn`
 - Ingest a PDF from the REPL: see the `comment` block in `dev/user.clj`
