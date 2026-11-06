# fly-learner

Learn anything.

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
 - Learning, as layers that know each other as little as possible (checked by a test). What the learner knows is a score from 0 to 1 per item of the graph (each concept, and each relation between two concepts), derived by replaying a log of evidence; the target is the weighted score of the whole graph. Forgetting is an addition: a model wrapping the basic one, so scores fade with time since the last recall (FSRS-style) and reviews are just the most valuable thing to study. What to study next weighs importance, room to grow and how well the prerequisites are known. Quizzes are built on learning alone: a question from an item's material (e.g. how erasure and pseudo-objects relate), a free-text answer graded by Sonnet against key points, with feedback; about $0.005 per question and grading.
 - Concepts: ideas with an identity, separate from the passages that mention them. Fold keywords are only strings ("equality type", "equality types", "untyped equality"), so Sonnet resolves them into concepts, each with a label, a gloss fixing its sense, aliases (the keywords naming it; each keyword names one concept) and broader concepts. On the first source: 406 keywords -> 209 concepts, 126 with a broader concept, 11 keywords ignored as too generic, in 6 calls for $0.24. Concepts are related by shared mentions (normalised PMI, the shared passages kept as evidence), so the graph supports neighbourhoods and the cheapest path between two concepts, each step citing its passages. Both writers make mistakes, so every change, by the model or by hand (merge, split, assign, rename, ignore, broader), is atomic and logged.

To do:

 - Group paragraphs by length or by Lemma/Proof boundaries instead of fixed runs of 6.
 - Mark concepts as seen when their pages are read (`:seen` evidence), so exposure shows in the heat map.
 - Multiple sources and links between them (asking already searches across sources; concepts are resolved across sources).
 - Concept graph: zoom and pan for large graphs (text gets small past ~30 concepts), a path finder between two concepts, curation screens. Retrieval and reviews by concept.

### 2. Frontend (in progress)

Same stack as [flybot.sg](https://www.flybot.sg) ([source](https://github.com/flybot-sg/lasagna-pattern/tree/main/examples/flybot-site)): Replicant UI with an effect-map dispatch, a single pull-pattern API ([lasagna-pattern](https://github.com/flybot-sg/lasagna-pattern)) over Transit, http-kit + fun-map on the server, Kaocha + Rich Comment Tests. Storage stays on Datalevin.

Done:

 - Browse a source from the birdview down: breadcrumb, summary, keywords, drill-down cards (paragraph text shown inline), related nodes.
 - The original PDF beside it (pdf.js), on the selected node's page, with paging. The node's source text is highlighted on the page (a paragraph, a passage's paragraphs, or a section's heading) and scrolled into view; text from ingest is matched against pdf.js's text in chunks, so formulas the two extract differently don't break it (on the first source, 98% of paragraphs have at least 90% of their text found).
 - Concept pages (`/c/<id>`): the concept's gloss, other spellings and every passage mentioning it, beside a concept graph to explore. The graph starts from the concept and its neighbours (broader, narrower, strongest related); clicking a concept opens it and expands its neighbours out of it, clicking again collapses it, and +/− expand or collapse without opening. Collapsing hides what only that concept revealed; expanding it again brings it back. Broader concepts are always above narrower ones, concepts already shown stay put, and changes animate. A node's tags link to its concepts; keywords naming no concept still open every node tagged with them. `/c` lists all concepts with a filter over names and glosses.
 - URLs per node (`/n/<id>`), concept (`/c/<id>`), keyword (`/k/<keyword>`) and answer (`/a/<id>`), back/forward work.
 - Ask the knowledge base from any page. Full-text matches show at once; then Haiku answers from the knowledge base only (graph RAG: full-text search over titles, keywords, summaries and paragraphs, scores spread to parent sections and keyword links, plus each source's overview), citing the nodes it used. The answer comes with its cited subgraph (document › chapter › section › node) to drill into, follow-up questions and other matches. Answers are stored and listed on the home page. Works across every ingested source; about 4–7 s and $0.001 per question.
 - Your ideas on the knowledge: agree or disagree with a concept, a relation, a passage or any sentence you select (in the text or in the PDF, which now has a selectable text layer), or add a note in your own words, linked to your other ideas. Quotes are marked in the text; concept and node pages list your ideas on them and on anything inside them; the concept graph shows them as diamonds by their concept or relation (toggle "My ideas"). Anchors survive the knowledge changing: a merged concept is followed, a re-ingested passage or quote is found again by its text, and a PDF quote no paragraph contains stays on its page.
 - Study page (`/q`): the score of the whole graph, what to study next (with a "Quiz me" for each), and quizzes: a question written by Sonnet from the item's material, a free-text answer (or "I don't know"), then the grade, feedback, the model answer with its key points, and how the item's and the graph's scores moved. A concept page shows how well you know it, with "Quiz me on this". The concept graph is a heat map: concepts fill and relations thicken with their score, and the toolbar and header show the whole graph's score.

To do:

 - Quiz on a relation from the graph: click an edge to be asked how two concepts connect.
 - Ask the assistant about a selection (sentences, concepts), then let the exchange go, keep it as an idea, or add it to knowledge as a `:note` source.
 - Mark PDF quotes on the page; match PDF quotes to paragraphs through formulas (as the highlighting does).

## Development

Inside `devenv shell` (JDK, Clojure, Node, babashka, clojure-mcp-light):

 - `bb dev` starts an nREPL; in it `(user/cljs!)` starts the backend (http://localhost:8080) and the frontend watch, then open http://localhost:3000
 - `bb serve` builds the frontend and serves everything from http://localhost:8080
 - `bb test`, `bb lint`
 - Anthropic API key: `ANTHROPIC_API_KEY`, or the macOS keychain item named in `resources/config.edn`
 - Ingest a PDF from the REPL: see the `comment` block in `dev/user.clj`
