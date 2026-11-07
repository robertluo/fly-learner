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
 - Zoom: the map and every concept graph (a topic's, a concept's) open with names readable (the whole map if it fits so, else around what you are learning); drag or scroll to pan, pinch or the buttons at its bottom right to zoom; zoomed far out, names give way to bubbles. The PDF zooms the same way (50%–300%, kept between visits), staying at the same spot of the page. Full screen (the last button) puts the browser full screen showing only that pane, map, graph or PDF; Esc or the button leaves it.
- The original PDF beside it (pdf.js): every page in one scrolling column, opened at the selected node's page (only the pages near the view are drawn; scrolling turns the page, typing a number in the page box jumps there, and leaving the node's page is one step in history). The node's source text is highlighted on the page (a paragraph, a passage's paragraphs, or a section's heading) and scrolled into view; text from ingest is matched against pdf.js's text in chunks, so formulas the two extract differently don't break it (on the first source, 98% of paragraphs have at least 90% of their text found).
 - Concept pages (`/c/<id>`): the concept's gloss, other spellings and every passage mentioning it, beside a concept graph to explore. The graph starts from the concept and its neighbours (broader, narrower, strongest related); clicking a concept opens it and expands its neighbours out of it, clicking again collapses it, and +/− expand or collapse without opening. Collapsing hides what only that concept revealed; expanding it again brings it back. Broader concepts are always above narrower ones, concepts already shown stay put, and changes animate. A node's tags link to its concepts; keywords naming no concept still open every node tagged with them. `/c` lists all concepts with a filter over names and glosses.
 - URLs per node (`/n/<id>`), concept (`/c/<id>`), keyword (`/k/<keyword>`) and answer (`/a/<id>`), back/forward work.
 - Ask the knowledge base from any page. Full-text matches show at once; then Haiku answers from the knowledge base only (graph RAG: full-text search over titles, keywords, summaries and paragraphs, scores spread to parent sections and keyword links, plus each source's overview), citing the nodes it used. The answer comes with its cited subgraph (document › chapter › section › node) to drill into, follow-up questions and other matches. Answers are stored and listed on the home page. Works across every ingested source; about 4–7 s and $0.001 per question.
 - Your ideas on the knowledge: agree or disagree with a concept, a relation, a passage or any sentence you select (in the text or in the PDF, which now has a selectable text layer), or add a note in your own words, linked to your other ideas. Quotes are marked in the text; concept and node pages list your ideas on them and on anything inside them; the concept graph shows them as diamonds by their concept or relation (toggle "My ideas"). Anchors survive the knowledge changing: a merged concept is followed, a re-ingested passage or quote is found again by its text, and a PDF quote no paragraph contains stays on its page. Ideas on text selected in the PDF are marked on the page in their stance's colour, and listed beside whichever passage is open on that page.
 - Full width: "⇤ Full width" on the PDF or concept-graph toolbar hides the left pane, so the source gets the whole screen (on a small screen, the PDF alone); remembered across visits.
 - Ask about a selection: select text in the article or the PDF and ask about it ("explain this", "what does ι mean here?"). The answer appears beside the selection, grounded in the paragraph it came from and the rest of its page (where a formula usually sits), citing them; background the documents never explain is given briefly and marked as such. Keep the answer as a note on the passage (edit it first), open it as a page with its sources, or let it go (it stays in the ask history).
 - The map (home): every topic of the knowledge, so a growing collection is seen whole. Concepts are clustered into topics by how they connect (Louvain over broader links and co-occurrence), and Sonnet names each one ("Nash equilibrium and refinements", "Reduction and confluence"); on the first two sources, 645 concepts make 32 topics, named for $0.05. Topics are drawn as bubbles sized by their concepts and filled by how well you know them, joined to their closest neighbours and placed by those links alone, whatever book their passages come from: knowledge two sources share is one group of topics, and only unconnected knowledge stands apart. A topic's page shows what it covers, how well you know it, what to study next in it, and its concepts as a graph. "Learn this topic" (a few at a time) puts it in the header and makes "what to study next" come from it. `bb ingest` clusters again: topics there are stay as they are and new concepts join them or form new topics, so a new source never reshuffles the others (clustering from scratch is an option; then a topic that is mostly the same keeps its name, and learning a topic that is gone moves to the one holding most of it).
 - Explorations (`/x`): passages better understood by doing offer "Explore it" on their page. Sonnet builds a small interactive page from the passage's own text, citing it, and it is kept for later. A page is data (state, values derived from it, and elements: sliders, choices, toggles, buttons, tables, plots, diagrams, step-throughs, "predict first" questions) drawn by the app, so it is safe, looks like the rest, and is checked before it is kept: every expression is read and run from the start and from each state one action away; problems go back to the model once. Which passages qualify is judged once per passage by Haiku from the summaries (`bb ingest` does it for new ones). Making one takes about 25 s and $0.04–0.05; "Make it again" takes a request ("a 3×3 game") and keeps the earlier version. Four written by hand show the range: the prisoner's dilemma, mixing in matching pennies, backward induction on an entry game, and normal vs applicative reduction of K z Ω.
 - Study page (`/q`): the score of the whole graph, what to study next (with a "Quiz me" for each), and quizzes: a question written by Sonnet from the item's material, a free-text answer (or "I don't know"), then the grade, feedback, the model answer with its key points, and how the item's and the graph's scores moved. A concept page shows how well you know it, with "Quiz me on this". The concept graph is a heat map: concepts fill and relations thicken with their score, and the toolbar and header show the whole graph's score.

To do:

 - Study inside the flow: quiz on a concept from its page or the passage you read, by how it connects to its neighbours (knowing a concept = knowing its edges); then retire the separate Study page.
 - Concepts that are homonyms across sources ("normal form" of a game vs of a λ-term, "type"): tell the resolver each concept's source, and split the few merged ones.
 - Map: zoom and pan for many topics.
 - Ask about a selection on the concept graph (concepts, relations); add an exchange to knowledge as a `:note` source.
 - Match PDF quotes to paragraphs through formulas (as the highlighting does); show a PDF mark's note on hover.

## Development

Inside `devenv shell` (JDK, Clojure, Node, babashka, clojure-mcp-light):

 - `bb dev` starts an nREPL; in it `(user/cljs!)` starts the backend (http://localhost:8080) and the frontend watch, then open http://localhost:3000
 - `bb serve` builds the frontend and serves everything from http://localhost:8080
 - `bb test`, `bb lint`
 - Anthropic API key: `ANTHROPIC_API_KEY`, or the macOS keychain item named in `resources/config.edn`
 - Ingest a PDF: put it in `data/sources/`, then `bb ingest data/sources/<file>.pdf` (several at once is fine). It folds the document with Claude, resolves its new keywords into concepts, and prints progress and the cost; then reload the app. With `bb dev` running it ingests through that REPL (the app's db; only one process may write it), otherwise in a new JVM. Ingesting the same file again replaces it; your ideas on it are re-attached. Step by step from the REPL: the `comment` block in `dev/user.clj`
