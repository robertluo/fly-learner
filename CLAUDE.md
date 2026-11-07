# fly-learner

See README.md for goals. Roadmap: backend (knowledge base from one source) first; frontend comes after. Clojure (JVM backend) + ClojureScript (shadow-cljs, Replicant) frontend, Datalevin storage.

## Layout

Web stack follows flybot.sg (github.com/flybot-sg/lasagna-pattern, `examples/flybot-site`); Datalevin instead of Datahike.

- `src/clj/robertluo/fly_learner/` — backend
  - `ingest` — `ingest-pdf!`: PDF -> section tree -> fold with Claude -> `data/folds/*.edn` + Datalevin
  - `ingest/pdf` — PDFBox paragraphs -> blocks (`:heading` / `:text`), headings recognised from text
  - `ingest/outline` — blocks -> section tree (drops cover pages, contents, bibliography, ...)
  - `fold` — regroup long sections, bottom-up summaries, flatten to nodes, keyword cross-links, `->tx`
  - `llm` — Anthropic Java SDK; `complete`, `complete-json` (structured outputs), `usage`/`cost` per model.
    Two tiers in `config.edn` `[:anthropic :models]`: `:strong` (Sonnet: sections, chapters, quizzes) and
    `:common` (Haiku: paragraph groups). The system prompt carries a cache breakpoint; keep it identical
    across calls (fold puts the document outline there) and put per-call content in the user prompt.
  - `server/system` — fun-map `life-cycle-map` (db, api-fn, ring app, http-kit); `/files/sources/<id>` serves the PDF
  - `server/api` — lasagna-remote pull API on `/api`: `:sources`, `:asks` (collections; creating an ask runs the
    model), `:concepts` (read-only collection), `:nodes` (with `:node/concepts`; `:node/keywords` keeps only
    keywords naming no concept), `:keywords`, `:search` (lookups), `:learning` (lazy overview), `:universe` (lazy:
    every cluster with its score), `:clusters` (read-only: a cluster's next items), `:goals` (create/delete: learn a
    cluster), `:questions` / `:answers` (creates: write a question, grade an answer), `:explorations` (create:
    make one of a node or remake one with feedback, runs the model; a node carries `:node/explore`: its proposal and
    explorations)
  - `server/ideas` — ideas resolved for the API (`:idea/targets`, `:idea/within` = ancestor nodes); `focus`: a
    selected passage (quote anchor) -> the nodes it is in, for asking about it (`{:asks {nil {.. :ask/anchor a}}}`)
  - `server/study` — app-layer composition of learning + store + quiz + clusters for the API: `overview` (overall,
    scores, next — from the clusters being learned, if any — and goals), `universe` (clusters with score and
    `:learning?`), `cluster` (next items within one), `learn!`, `new-question!` (sent without its model answer),
    `answer!` (grading + scores before/after). A goal is the opaque scope `[:cluster id]`
  - `ask` — graph RAG over the whole store: `retrieve` (Datalevin full-text + spread to parents/links),
    `ask!` (answer with the `:common` model from numbered entries, store as `:ask/*`), `view` (cited subgraph).
    `:focus {:quote :nodes}`: a question about a passage the learner selected; its nodes and the other
    paragraphs on their page come first, the passage is quoted, and background the entries lack may be
    explained, marked as such (stored as `:ask/quote`, `:ask/focus`)
  - `concept` — concepts: ideas with an identity (`:concept/label`, `:concept/gloss`, `:concept/broader`), named by
    keywords (`:concept/aliases`, unique: a keyword names one concept). Mentions are derived: node keyword = alias.
    `resolve!` (model, add-only) assigns unresolved keywords in batches; curation fns (`merge!`, `split!`, `assign!`,
    `rename!`, `retag!`: a homonym, one keyword naming different ideas in different nodes, gets a keyword per sense
    on chosen nodes; `merge!` moves prerequisite claims too, ...) are atomic multi-stage edits (Datalevin checks unique values against the pre-tx state, so an
    alias must leave one concept before joining another, inside `with-transaction`). Every change is an `:edit/*`.
  - `concept/graph` — pure over `load-graph`: `edges` (broader/narrower + NPMI co-occurrence, evidence = shared
    nodes), `neighbourhood`, `path` (Dijkstra), `related`
  - `concept/prereq` — prerequisites (directed: a before b; no path = independent). `judge` (`:common` model, 8
    concepts a call against the whole index tagged with sources, cached) names each concept's direct needs and the
    background it assumes; `consolidate` merges background into hidden concepts (`:concept/origin :inferred`, no
    mentions; the label is an alias, so a later source teaching it gives it mentions); stored as `:prereq/*` claims,
    judged concepts marked `:concept/prereqs-by`. `dag` derived on read: mutual claims drop (learned together), a
    cycle loses the edge the sources' page order contradicts most; `before` = what to learn first, in order;
    `derived` (claims, pages, dag, reasons) once per db version
  - `concept/view` — `index`, `concept` (hierarchy, related with shared-passage counts, prerequisites: `:concept/needs`
    with why, `:concept/before` in order, `:concept/needed-by`; mentions with paths) and `graph` (every concept +
    broader/related/requires edges; the API's lazy `:concept-graph`) for the API
  - `concept/cluster` — topics: `communities` (Louvain local moving over broader + co-occurrence weights; fragments
    under 6 join their most connected neighbour; `:keep` existing clusters fixed), `refresh!` (after an ingest:
    stored clusters stay as they are, new concepts join them or make new ones named by `llm-namer`, so a new source
    never reshuffles other topics; `{:fresh? true}` clusters from scratch, a cluster mostly an old one, by Jaccard,
    keeping its id and name; returns `:successors` {gone-id new-id}), `universe` (clusters with members, dominant
    source, links). Stored as `:cluster/*`; `bb ingest` refreshes them and moves goals on a gone cluster to its
    successor (`study/clusters-replaced!`)
  - `keypoints` — key sentences: `extract!` (`:common` model, a few passages per call, 4 calls at a time) quotes
    1-3 sentences per passage verbatim with a 2-6 word label (the passage's hidden headings); `locate` keeps a quote
    only when found in its paragraph, as the paragraph's own text. Stored on the passage as `:node/keypoints`
    (`[]` for none; a re-ingest drops them with the node). `for-source` -> the `:keypoints` lookup
  - `db` (Datalevin knowledge schema + queries; `per-version`: a heavy read computed once per db version under a lock,
    e.g. `graph/load-graph` (Datalevin 1.2.10 can fail heavy queries run concurrently: "Comparison method violates its
    general contract"); `connect` merges other layers' schemas; `copy!` rebuilds a db under the
    current schema, e.g. after marking an attribute `:db/fulltext` — Datalevin's `re-index` loses full-text entries)
- `src/cljc/robertluo/fly_learner/ui/core/` — frontend logic, JVM-testable: `db` (pure state fns), `pull` (named pull specs),
  `history` (URL <-> state; `/` the map, `/g/<id>` a cluster, `/x/<id>` an exploration), `views` (Replicant hiccup: one flow, map ->
  cluster -> concept -> passage; a concept page's "Learn first": its prerequisite path not yet known, foundations
  first, hidden ones badged "background", and what it leads to; study-next cards mark prerequisites outside the topic; the header holds the map and the clusters being learned; a quiz started anywhere docks at the top of the left
  pane of split views, `quiz-dock`, and stays across pages), `highlight` (find a node's text among pdf.js text items), `viewport` (pan/zoom of a drawing: automatic
  until the learner pans or zooms: whole if readable, else readable names around a focus; names below `readable-px`
  are not drawn; `db/pane-*` for `:map` and `:graph`, which goes automatic again on another graph),
  `explore` (draws an exploration's resolved elements; `diagram-layout`: layered, children under their parents;
  `ticks`), `graph` (concept graph: visible = focus + neighbours of expanded concepts, outline-style; neighbours include what
  a concept needs and some of what needs it; prerequisite edges drawn with an arrow halfway), `layout`
  (incremental force layout on primitive arrays: broader-above and prerequisite-above are constraints (`above-edges`
  drops one that would close a loop), label boxes never overlap;
  unconnected parts laid out apart and packed in rows below the largest, else repulsion strands them far away; tween)
- `src/cljs/robertluo/fly_learner/ui/` — `core` (dispatch-of effects `:db`/`:pull`/`:history`/`:animate`, transit fetch, render,
  mouseup -> text selection), `pdf` (pdf.js: render a page, its selectable text layer, highlight boxes; the source as one scrolling column of
  pages, drawn near the view only: `sync-scroll!` keeps it at the state's page (and at the same spot when pages change
  size: width, zoom), `find-each!` finds texts on a page one by one (key sentences, what is open), `watch-scroll!` turns the page as the learner scrolls; when it pauses the left pane follows: `db/node-at-page` via
  the `:pages` lookup picks the node of the same kind and level on that page, returned whole so the PDF is never
  unmounted), `zoom` (pan/zoom gestures: drag,
  trackpad scroll, pinch = ctrl/⌘ + wheel; `fullscreen!`: the whole page, so popovers work; `db/fullscreen` follows
  the browser's `fullscreenchange` and shows only the pane; the map and the concept graph pan and zoom their viewBox, see `viewport`;
  the PDF zooms its pages' width (`db/pdf-zoom*`, kept in localStorage); the split views' divider is dragged in `views/split-handle`
  (`db/split-*`, the left pane's share, kept in localStorage)), `select` (the
  selection inside a `data-anchor` element as a quote anchor; `<br>` between PDF lines counts as a space)
- `resources/public/` — `index.html` (loads vendored pdf.js as a module), `css/style.css`
- `src/cljc/robertluo/fly_learner/knowledge.cljc` — shared malli schema for nodes

### Layers (enforced by `test/clj/.../architecture_test.clj`)

Each layer knows the one below as little as possible, so any part can be reimplemented:

- **knowledge** (`fold`, `ingest`, `concept*`, `ask`, `db`) — depends on nothing above it
- **learning** (`learning.cljc`, depends on nothing) — score in [0, 1] per item, derived by replaying an evidence
  log (`{:item :kind :at :grade :source}`; kinds open: `:answer`, `:seen`). States what it needs as protocols:
  `Curriculum` (items + weights, prerequisites, material `{:title :about :sources}`) and `Model`
  (`init`/`observe`/`score`; `mastery` is the basic one). `overall` = weighted score, the learner's target;
  `next-items` = value × room to grow × readiness of prerequisites; value = weight + half of what the item unlocks
  (items needing it, not known yet, split among their prerequisites), so foundations come early; prerequisites outside
  a restricted curriculum are offered too (study marks them `:prerequisite?`).
  - `learning.decay` — forgetting as a `Model` wrapping any `Model` (FSRS-style stability); learning knows nothing of it
  - `restricted` — a `Curriculum` limited to some items: a learner focusing on part of it (its own `overall`,
    `next-items`; prerequisites outside still count for readiness)
  - `learning.store` — the evidence log in Datalevin (own schema), and goals: scopes the learner chose to learn now
    (opaque ids)
  - `learning.concepts` — the only bridge: the concept graph as a `Curriculum` (`[:concept id]`, `[:relation a b]`;
    prerequisites = `concept.prereq/dag` for judged concepts, broader otherwise; a hidden concept's material is its
    gloss and what needs it);
    `concepts-items` = the items a set of concepts (a cluster) covers
- **features** (`quiz`: generate questions from an item's material, grade free text into `:answer` evidence) —
  depend on learning only, never on knowledge
- **ideas** (`ideas.cljc`, depends on nothing) — the learner's own voice: an idea takes a stance
  (`:agree`/`:disagree`), is a `:note`, or marks something: `:highlight`, `:bookmark` (a node anchor with the PDF
  `:page`; `ui.core.db/bookmarks`, kept out of the idea lists), on one or more anchors (opaque maps), and may link to other ideas.
  `Anchors` protocol (`target` -> a key ideas are grouped by, `describe`); W3C-style text quotes
  (`quote-selector`, `find-quote`, `mark-segments`) re-find a span after the text changes.
  - `ideas.store` — Datalevin, own schema
  - `ideas.knowledge` — the only bridge: anchors on concepts (follows merges), relations, nodes (re-found by
    title after re-ingest) and quotes in node text/summary or the PDF (re-found by text; a PDF quote no
    paragraph contains stays on its page)
  - ideas and learning know nothing of each other
- **explore** (`explore.cljc`, depends on nothing) — explorations: an interactive view of some knowledge as data
  (`:vars`, `:derive`, `:view` of elements: text, formula, slider, choice, toggle, button, table, plot, diagram,
  steps, predict). Expressions are s-expressions in strings, evaluated by a whitelist interpreter with fuel/depth/
  length bounds (any spec ends); templates put them in braces. `view` (elements with values), `act` (state after an
  action), `check` (shape, every expression readable, a dry run from the start and each state one action away: what
  a model must fix before a spec is kept). `explore.examples`: four specs written by hand (game theory, λ-calculus)
  - `explore.store` — Datalevin, own schema: `:exploration/*` (spec as EDN string, anchor, cites, pitch, feedback,
    previous = the version it remakes; `all` hides remade versions) and `:proposal/*` (one per node judged; kind
    `:none` when not worth it, so a node is judged once)
  - `explore.knowledge` — the only bridge: `propose!` (`:common` model, batches of 30 sections/groups by summary
    -> kind + pitch), `make!` (`:strong` model writes a spec from the node and the text under it as numbered entries
    it cites [n]; a plain JSON reply, since structured outputs reject a schema this size (24 optional params, grammar
    size); `tidy`, `check`, one repair round with the problems, kept only when sound). Anchors `{:node/id
    :source/path :node/title}`: re-found by title after a re-ingest
- **app** (`server.*`, `ui.*`; the UI knows item ids `[:concept id]` / `[:relation a b]` for the heat map) composes: `server.system` merges schemas and picks the model (mastery + decay)
  and the curriculum
- `cli` — command-line tasks (app layer): `ingest!`; `server.system/connect` opens the db with every layer's schema
- `dev/user.clj` — `(start!)` / `(stop!)` / `(cljs!)`, ingest snippet
- Tests: Kaocha runs `test/clj` plus Rich Comment Tests (`^:rct/test` comment blocks in `src/`)

## Commands (inside `devenv shell`)

- `bb dev` — nREPL (backend + shadow-cljs middleware); then `(user/cljs!)`: starts the backend (:8080) if needed and the frontend watch (:3000, proxies pages and `/api` to :8080). `(user/start!)` alone runs just the backend
- `bb watch` — shadow-cljs without a REPL; `bb serve` — release build served by the backend on :8080
- `bb test`, `bb lint`; `bb install` / `bb vendor` copy pdf.js into `resources/public/vendor/pdfjs`
- `bb ingest data/sources/<file>.pdf ...` — `cli/ingest!` (ingest + fold, then `concept/resolve!`, clusters,
  `concept.prereq/judge!` for new concepts, `keypoints/extract!` and `explore.knowledge/propose!` for new passages; prints the cost).
  `bb/ingest_task.clj` evaluates it over nREPL in the running `bb dev` REPL when `.nrepl-port` answers (Datalevin
  must have one writer: the app's), else runs `clojure -M:ingest`. Paths are stored relative to the project
  (`cli/source-path`), so any spelling of a file re-ingests the same source
- `devenv up` — runs `bb dev`

Every JVM alias needs the Datalevin `--add-opens` flags; keep them when adding aliases.

## Clojure REPL Evaluation

The command `clj-nrepl-eval` (from clojure-mcp-light, provided by devenv) evaluates Clojure code via nREPL.
It is on PATH inside `devenv shell`; otherwise call it as `.devenv/profile/bin/clj-nrepl-eval`.
The paren-repair hook in `.claude/settings.json` uses `.devenv/profile/bin` too, so run `devenv shell` once after changing devenv.nix.

**Discover nREPL servers:** `clj-nrepl-eval --discover-ports`

**Evaluate code:** `clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds): `clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.
