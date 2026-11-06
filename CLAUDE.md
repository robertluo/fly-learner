# Learner

See README.md for goals. Roadmap: backend (knowledge base from one source) first; frontend comes after. Clojure (JVM backend) + ClojureScript (shadow-cljs, Replicant) frontend, Datalevin storage.

## Layout

- `src/clj/learner/` — backend
  - `ingest` — `ingest-pdf!`: PDF -> section tree -> fold with Claude -> `data/folds/*.edn` + Datalevin
  - `ingest/pdf` — PDFBox paragraphs -> blocks (`:heading` / `:text`), headings recognised from text
  - `ingest/outline` — blocks -> section tree (drops cover pages, contents, bibliography, ...)
  - `fold` — regroup long sections, bottom-up summaries, flatten to nodes, keyword cross-links, `->tx`
  - `llm` — Anthropic Java SDK; `complete`, `complete-json` (structured outputs), `usage`/`cost` per model.
    Two tiers in `config.edn` `[:anthropic :models]`: `:strong` (Sonnet: sections, chapters, quizzes) and
    `:common` (Haiku: paragraph groups). The system prompt carries a cache breakpoint; keep it identical
    across calls (fold puts the document outline there) and put per-call content in the user prompt.
  - `quiz`, `progress` (SM-2 reviews), `db` (Datalevin schema), `server` (reitit API), `main`
- `src/cljc/learner/knowledge.cljc` — shared malli schemas for nodes/questions
- `src/cljs/learner/app.cljs` — browser UI
- `dev/user.clj` — `(go)` / `(halt)` / `(reset)`
- `test/clj`, `test/cljs`

## Commands (inside `devenv shell`)

- `bb nrepl` — backend nREPL; `bb cljs` — shadow-cljs watch on http://localhost:8280
- `bb test`, `bb test-cljs`, `bb lint`, `bb serve`
- `devenv up` — nREPL + shadow-cljs together

The Anthropic API key is read from the macOS keychain (service `learner`, account `anthropic`, set in `resources/config.edn`) unless `ANTHROPIC_API_KEY` is set. Never print or log it.

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
