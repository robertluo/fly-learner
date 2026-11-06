# Learner

See README.md for goals. Roadmap: backend (knowledge base from one source) first; frontend comes after. Clojure (JVM backend) + ClojureScript (shadow-cljs, Replicant) frontend, Datalevin storage.

## Layout

Web stack follows flybot.sg (github.com/flybot-sg/lasagna-pattern, `examples/flybot-site`); Datalevin instead of Datahike.

- `src/clj/learner/` — backend
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
    model), `:nodes`, `:keywords`, `:search` (lookups)
  - `ask` — graph RAG over the whole store: `retrieve` (Datalevin full-text + spread to parents/links),
    `ask!` (answer with the `:common` model from numbered entries, store as `:ask/*`), `view` (cited subgraph)
  - `quiz`, `progress` (SM-2 reviews), `db` (Datalevin schema + queries; `copy!` rebuilds a db under the
    current schema, e.g. after marking an attribute `:db/fulltext` — Datalevin's `re-index` loses full-text entries)
- `src/cljc/learner/ui/core/` — frontend logic, JVM-testable: `db` (pure state fns), `pull` (named pull specs),
  `history` (URL <-> state), `views` (Replicant hiccup), `highlight` (find a node's text among pdf.js text items)
- `src/cljs/learner/ui/` — `core` (dispatch-of effects `:db`/`:pull`/`:history`, transit fetch, render), `pdf` (pdf.js: render a page, highlight boxes)
- `resources/public/` — `index.html` (loads vendored pdf.js as a module), `css/style.css`
- `src/cljc/learner/knowledge.cljc` — shared malli schemas for nodes/questions
- `dev/user.clj` — `(start!)` / `(stop!)` / `(cljs!)`, ingest snippet
- Tests: Kaocha runs `test/clj` plus Rich Comment Tests (`^:rct/test` comment blocks in `src/`)

## Commands (inside `devenv shell`)

- `bb dev` — nREPL (backend + shadow-cljs middleware); then `(user/start!)` (:8080) and `(user/cljs!)` (:3000, proxies to :8080)
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
