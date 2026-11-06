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
    keywords naming no concept), `:keywords`, `:search` (lookups), `:learning` (lazy overview), `:questions` /
    `:answers` (creates: write a question, grade an answer)
  - `server/ideas` — ideas resolved for the API (`:idea/targets`, `:idea/within` = ancestor nodes)
  - `server/study` — app-layer composition of learning + store + quiz for the API: `overview` (overall, scores,
    next), `new-question!` (sent without its model answer), `answer!` (grading + scores before/after)
  - `ask` — graph RAG over the whole store: `retrieve` (Datalevin full-text + spread to parents/links),
    `ask!` (answer with the `:common` model from numbered entries, store as `:ask/*`), `view` (cited subgraph)
  - `concept` — concepts: ideas with an identity (`:concept/label`, `:concept/gloss`, `:concept/broader`), named by
    keywords (`:concept/aliases`, unique: a keyword names one concept). Mentions are derived: node keyword = alias.
    `resolve!` (model, add-only) assigns unresolved keywords in batches; curation fns (`merge!`, `split!`, `assign!`,
    `rename!`, ...) are atomic multi-stage edits (Datalevin checks unique values against the pre-tx state, so an
    alias must leave one concept before joining another, inside `with-transaction`). Every change is an `:edit/*`.
  - `concept/graph` — pure over `load-graph`: `edges` (broader/narrower + NPMI co-occurrence, evidence = shared
    nodes), `neighbourhood`, `path` (Dijkstra), `related`
  - `concept/view` — `index`, `concept` (hierarchy, related with shared-passage counts, mentions with paths) and
    `graph` (every concept + broader/related edges; the API's lazy `:concept-graph`) for the API
  - `db` (Datalevin knowledge schema + queries; `connect` merges other layers' schemas; `copy!` rebuilds a db under the
    current schema, e.g. after marking an attribute `:db/fulltext` — Datalevin's `re-index` loses full-text entries)
- `src/cljc/robertluo/fly_learner/ui/core/` — frontend logic, JVM-testable: `db` (pure state fns), `pull` (named pull specs),
  `history` (URL <-> state), `views` (Replicant hiccup), `highlight` (find a node's text among pdf.js text items),
  `graph` (concept graph: visible = focus + neighbours of expanded concepts, outline-style), `layout`
  (incremental force layout on primitive arrays: broader-above is a constraint, label boxes never overlap; tween)
- `src/cljs/robertluo/fly_learner/ui/` — `core` (dispatch-of effects `:db`/`:pull`/`:history`/`:animate`, transit fetch, render,
  mouseup -> text selection), `pdf` (pdf.js: render a page, its selectable text layer, highlight boxes), `select` (the
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
  `next-items` = weight × room to grow × readiness of prerequisites.
  - `learning.decay` — forgetting as a `Model` wrapping any `Model` (FSRS-style stability); learning knows nothing of it
  - `learning.store` — the evidence log in Datalevin (own schema)
  - `learning.concepts` — the only bridge: the concept graph as a `Curriculum` (`[:concept id]`, `[:relation a b]`)
- **features** (`quiz`: generate questions from an item's material, grade free text into `:answer` evidence) —
  depend on learning only, never on knowledge
- **ideas** (`ideas.cljc`, depends on nothing) — the learner's own voice: an idea takes a stance
  (`:agree`/`:disagree`) or is a `:note`, on one or more anchors (opaque maps), and may link to other ideas.
  `Anchors` protocol (`target` -> a key ideas are grouped by, `describe`); W3C-style text quotes
  (`quote-selector`, `find-quote`, `mark-segments`) re-find a span after the text changes.
  - `ideas.store` — Datalevin, own schema
  - `ideas.knowledge` — the only bridge: anchors on concepts (follows merges), relations, nodes (re-found by
    title after re-ingest) and quotes in node text/summary or the PDF (re-found by text; a PDF quote no
    paragraph contains stays on its page)
  - ideas and learning know nothing of each other
- **app** (`server.*`, `ui.*`; the UI knows item ids `[:concept id]` / `[:relation a b]` for the heat map) composes: `server.system` merges schemas and picks the model (mastery + decay)
  and the curriculum
- `dev/user.clj` — `(start!)` / `(stop!)` / `(cljs!)`, ingest snippet
- Tests: Kaocha runs `test/clj` plus Rich Comment Tests (`^:rct/test` comment blocks in `src/`)

## Commands (inside `devenv shell`)

- `bb dev` — nREPL (backend + shadow-cljs middleware); then `(user/cljs!)`: starts the backend (:8080) if needed and the frontend watch (:3000, proxies pages and `/api` to :8080). `(user/start!)` alone runs just the backend
- `bb watch` — shadow-cljs without a REPL; `bb serve` — release build served by the backend on :8080
- `bb test`, `bb lint`; `bb install` / `bb vendor` copy pdf.js into `resources/public/vendor/pdfjs`
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
