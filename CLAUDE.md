# fly-learner

See README.md for goals. Roadmap: backend (knowledge base from one source) first; frontend comes after. Clojure (JVM backend) + ClojureScript (shadow-cljs, Replicant) frontend, Datalevin storage.

## Layout

Web stack follows flybot.sg (github.com/flybot-sg/lasagna-pattern, `examples/flybot-site`); Datalevin instead of Datahike.

- `src/clj/robertluo/fly_learner/` — backend
  - `ingest` — `ingest-source!`: a PDF, an EPUB or a folder of Markdown (`formats`, `source-type`: a folder or `.md`
    is `:markdown`, else by extension: `:source/type`) -> blocks -> section tree -> fold with Claude ->
    `data/folds/*.edn` + Datalevin. A source is read as pages whatever its format: from the blocks on, nothing depends
    on it but drawing a page (UI `views.document`; `html-pages?`, `page-html`, `resource` for the HTML ones). A source
    is its document, not its path: `same-source` finds it by content (`content-hash`: a file's bytes, a folder's book;
    `:source/hash`), else by path (changed in place), else by text (moved and changed: a source whose file is gone from
    its path sharing `moved-overlap` of its word 5-grams, `shingles`), and a re-ingest keeps its `:source/id` (a new one
    derives from the hash); the path is only where to read it. `:skip` (a folder's parts left out) is stored,
    `:source/skip`, and used again by a re-ingest of the path. `preview`: chapters, pages and `fold/estimate`, no model
  - `ingest/pdf` — PDFBox paragraphs -> blocks (`:heading` / `:text`, each with its `:page`), headings recognised from text
  - `ingest/book` — a book of HTML documents read as pages, whatever made it: a package `{:title :docs [[name doc]]
    :toc {[name frag] depth} :redirects :outside}`; documents cleaned (`clean`: no scripts/styles/nav, SVG `<object>`
    -> `<img>`), cut into pages (`page-chars` ~3000, a document starts a page) between leaf blocks; `extract` -> blocks
    (heading depth from the toc, its `data-keywords` -> `:keywords`; code/display math (`<math>`,
    `data-tex="display"`) joins the paragraph before; `data-aside` shown, not read; a footnote moves after the
    paragraph citing it, keeping its page), `page-html` -> the same pages as HTML to show (no ids, classes prefixed
    `b-`, links inside the book -> `data-goto` page, others by `:outside` or none, images by URL)
  - `ingest/epub` — jsoup: an EPUB's package (spine documents, toc from nav or NCX) for `ingest/book`; `resource` an entry
  - `ingest/markdown` — a folder of Markdown (a repository's docs) or one file, for `ingest/book`: CommonMark + GFM to
    HTML, a file a chapter; reading order and parts from `docs.json`/`mint.json` (any JSON nav tree), `SUMMARY.md`
    (mdBook/GitBook), else the folder (index first, natural order, folders as parts); part headings put before their
    first chapter; `:skip` titles or files left out; a chapter's headings under it (titled by its one top heading, else
    by the manifest); names from the top of the git repository holding it (unless it ignores the folder): GitHub
    anchors, links elsewhere in the repository open on GitHub at the commit, images served from inside it only;
    `content-hash` = manifest + chapters + skip. Front matter (YAML via clj-yaml, read safely; Hugo's TOML, top
    keys): `fields` -> title, description (a lead paragraph under the chapter heading, `data-lead`: read as its first
    passage), tags (chips, `data-aside`, and the heading's `data-keywords` -> the section's `:tags` -> its keywords,
    `fold/summarize-tree`), authors and date (byline), order (`sidebar_position`, `nav_order`, `weight`, ..; folders by
    Docusaurus `_category_`), drafts left out. `mkdocs.yml` not read yet
  - `ingest/tex` — TeX in Markdown as GitHub reads it, a commonmark-java parser + renderer extension: inline `$..$`
    (no space inside the delimiters, no digit after the closing one, no other `$`: "$5 or $10" is text), `$$..$$`,
    `$$` blocks, ```` ```math ```` fences (`math-fences!`) -> `<span|div data-tex="inline|display">` holding the TeX
    in its delimiters (what the knowledge base reads); the UI typesets them (`ui/html`)
  - `ingest/outline` — blocks -> section tree (drops cover pages, contents, bibliography, ...)
  - `fold` — regroup long sections, bottom-up summaries, flatten to nodes, keyword cross-links, `->tx`; `estimate`:
    a rough cost of folding a tree before any call (calibrated on the first thesis)
  - `llm` — models by provider (`config.edn` `:llm`, `settings`; the older `:anthropic` map still read):
    `:anthropic` (Java SDK: structured outputs, cached system prompt, refusal fallback), `:openai` (any
    OpenAI-compatible server: oMLX, LM Studio, llama.cpp, vLLM; JSON schema mode, else the schema in the prompt,
    remembered per client), `:ollama` (its own API: `:num-ctx`, `:think`); a provider's key from `:api-key-env`,
    `:keychain` or `:api-key`, `:concurrency`, `:timeout-s`, `:body` merged into requests. A model is
    `"provider/id"`, else the `:default` provider's (`route`); `complete`, `complete-json` (lenient `json-reply`
    and one retry for non-Anthropic models), `usage`/`cost` per model (local ones free). Two tiers in
    `[:llm :models]`: `:strong` (sections, chapters, quizzes) and `:common` (paragraph groups). The system prompt
    carries a cache breakpoint; keep it identical across calls (fold puts the document outline there) and put
    per-call content in the user prompt.
  - `server/system` — fun-map `life-cycle-map` (db, api-fn, ring app, http-kit; `:max-body` fits an upload);
    `connect` also points `prompts` at `data/prompts.edn` (config `:prompts-file`)
  - `server/settings` — model settings edited in the app, no config file to touch: `llm` = config.edn's `:llm`
    replaced by `data/settings.edn`'s, keys typed in the app (`data/secrets.edn`, mode 600) given to their providers
    as `:api-key` (they win over env and keychain, `llm/key-source`); `client` (rebuilt when they change),
    `client-ref` (what the system and API hold); `view` (no key: where it comes from and its last 4), `save!`
    (validated; fields the page does not edit kept), `provider-for` (a provider as edited, to list its models)
  - `server/pipeline` — `run!`: ingest sources and everything after (concepts, topics, prerequisites, key
    sentences, exercises, explorations), printing progress and the cost: what `bb ingest` and the settings page run;
    `source-path`
  - `server/ingests` — the settings page adding sources: `upload!` (`POST /files/upload?name=..`, the file as the
    body; kept under `data/sources/uploads/`, a zip unpacked with no entry outside its folder, bounded),
    `candidates` (each PDF/EPUB, the deepest folder holding all the Markdown), `describe` (no model: title, pages,
    rough cost, a folder's `:parts` to leave out, `:known` = already in the library by hash); `start!` a background
    job (one at a time), what `pipeline/run!` prints kept as its log (a fold's progress line replaced), `jobs`
  - `server/files` — `/files/sources/<id>` the original file; an EPUB's or a Markdown folder's `/pages` (JSON array of
    page HTML, cached per version, `ingest/page-version`) and `/res/<name>` (its images, sent with CSP `sandbox`)
  - `server/api` — lasagna-remote pull API on `/api`, built per request from the settings in force (`make-api*`):
    `:settings` (member `{:settings/id :llm}`: `settings/view`, update = `save!`), `:probes` (create: `:probe/list` a
    provider's models, `llm/list-models`; `:probe/try` a tiny request to a model; a failure is `:problem`, since
    `:error` is the API's), `:ingests` (collection: create = ingest uploaded paths under
    `data/sources` with `:ingest/skip`), `:prompts` (collection: update = edit, delete = back to the default),
    `:sources`, `:asks` (collections; creating an ask runs the
    model), `:concepts` (collection: create adds a concept the model missed, delete removes one: `server/curation`), `:nodes` (with `:node/concepts`; `:node/keywords` keeps only
    keywords naming no concept), `:keywords`, `:search` (lookups), `:goto` (lookup: the concepts and book exercises the top box's text names, as typed), `:learning` (lazy overview), `:universe` (lazy:
    every cluster with its score), `:clusters` (a cluster's next items; delete removes a topic and its concepts), `:goals` (create/delete: learn a
    cluster), `:questions` / `:answers` (creates: write a question, grade an answer), `:explorations` (create:
    make one of a node or remake one with feedback, runs the model; a node carries `:node/explore`: its proposal and
    explorations)
  - `server/ideas` — ideas resolved for the API (`:idea/targets`, `:idea/within` = ancestor nodes, `:idea/refs`: the
    concepts its `[[term]]` links name now, by alias, a plural by its singular; a term naming none is left out); `named`:
    the passages whose text names a term of the learner's (the API's `:named`); `:idea/place`: where its passage is in
    its source (path root first, reading order, page), for the notebook; `update!` rewrites one (stance, text, links;
    its anchors and time stay); `focus`: a
    selected passage (quote anchor) -> the nodes it is in, for asking about it (`{:asks {nil {.. :ask/anchor a}}}`)
  - `server/curation` — the learner correcting the knowledge base (API): `add-concept!` (names taken -> error; passages
    naming it; a missing gloss written by the `:common` model from them, `concept/llm-gloss`; prerequisites judged, then
    `cluster/refresh!`: judged first, since with few passages the claims are what connects it), `remove-concept!`,
    `remove-topic!` (and its goal)
  - `server/study` — app-layer composition of learning + store + quiz + clusters for the API: `overview` (overall,
    scores, next — from the clusters being learned, if any — and goals), `universe` (clusters with score and
    `:learning?`), `cluster` (next items within one), `learn!`, `new-question!` (sent without its model answer),
    `answer!` (grading + scores before/after). `new-question!` serves the item's first book exercise not answered yet
    (or the one asked for, `:question/exercise`) before writing one; the overview's `:exercises` = best grades. A goal is the opaque scope `[:cluster id]`
  - `ask` — graph RAG over the whole store: `retrieve` (Datalevin full-text + spread to parents/links; a numbered
    label in the question, `labels` "Exercise 1.1" / "Definition 1.8", ranks the paragraphs starting with it first, then
    the 5 after them, then those mentioning it: full-text sees only "exercise 1 1"),
    `ask!` (answer with the `:common` model from numbered entries, in Markdown with TeX, store as `:ask/*`), `view` (cited subgraph).
    `:focus {:quote :nodes}`: a question about a passage the learner selected; its nodes and the other
    paragraphs on their page come first, the passage is quoted, and background the entries lack may be
    explained, marked as such (stored as `:ask/quote`, `:ask/focus`)
  - `exercises` — the author's exercises and solutions: `segments` (pure, reading order: a line "Exercise 1.5." opens
    one; after the chapter's "Solutions to exercises" the same label opens its solution), `pair`, `found`; `extract!`
    (`:strong` model, a chapter's exercises 5 a call against the concepts the chapter mentions: 1-4 tested concepts,
    a one-line summary, key points from the solution) for sources not looked at (`:source/exercises-by`); `transcribe!`
    (`:strong` model, ~6000 characters a call) writes each one and its solution as Markdown with TeX, tables rebuilt,
    faithfully (`:exercise/text-md`, `:exercise/solution-md`; the quiz shows the source's text, `:verbatim?`, until then). Stored as
    `:exercise/*`, id from the source id + label (stable across re-ingests and moves; `ingest/remove-source!` drops them);
    `merge!` moves them. `of-concept`, `by-label`, `lookup` ("exercise 1.1", "ex 2": chapter 2's)
  - `concept` — concepts: ideas with an identity (`:concept/label`, `:concept/gloss`, `:concept/broader`), named by
    keywords (`:concept/aliases`, unique: a keyword names one concept). Mentions are derived: node keyword = alias.
    `resolve!` (model, add-only) assigns unresolved keywords in batches; curation fns (`merge!`, `split!`, `assign!`,
    `rename!`, `add!`: a concept the model missed, tagged on the passages naming it (`naming-nodes`: whole words, any
    case, a plural allowed), `remove!`: not a concept, its names ignored from then on (`:ignore? false` for background
    nothing needs any more), `retag!`: a homonym, one keyword naming different ideas in different nodes, gets a keyword per sense
    on chosen nodes; `merge!` moves prerequisite claims too, ...) are atomic multi-stage edits (Datalevin checks unique values against the pre-tx state, so an
    alias must leave one concept before joining another, inside `with-transaction`). Every change is an `:edit/*`.
  - `concept/graph` — pure over `load-graph`: `edges` (broader/narrower + NPMI co-occurrence, evidence = shared
    nodes), `neighbourhood`, `path` (Dijkstra), `related`, `strongest-related`, `edge-list` (every edge as data,
    the prerequisite dag given; `concept.prereq/edge-list` reads it from a db: what clustering and the curriculum use)
  - `concept/prereq` — prerequisites (directed: a before b; no path = independent). `judge` (`:common` model, 8
    concepts a call against the whole index tagged with sources, cached) names each concept's direct needs and the
    background it assumes; `consolidate` merges background into hidden concepts (`:concept/origin :inferred`, no
    mentions; the label is an alias, so a later source teaching it gives it mentions); stored as `:prereq/*` claims,
    judged concepts marked `:concept/prereqs-by`. `dag` derived on read: mutual claims drop (learned together), a
    cycle loses the edge the sources' page order contradicts most; `before` = what to learn first, in order;
    `derived` (claims, pages, dag, reasons) once per db version; `audit` (model checks each claim's direction, stores nothing; run :common, then
    `:tier :strong :claims` the flagged for a second opinion); `claim!` / `unclaim!`: human corrections (edit log)
  - `concept/view` — `index`, `concept` (hierarchy, related with shared-passage counts, prerequisites: `:concept/needs`
    with why, `:concept/before` in order, `:concept/needed-by`; mentions with paths), `lookup`
    (by label or alias: same, prefix, word prefix, contains; then most mentioned) and `graph` (every concept +
    `edge-list`; the API's lazy `:concept-graph`) for the API. Display only: nothing below the app requires it
    (architecture test); `server.api` adds a concept's `:concept/exercises`
  - `concept/cluster` — topics: `communities` (Louvain local moving over broader + co-occurrence weights; fragments
    under 6 join their most connected neighbour; `:keep` existing clusters fixed), `refresh!` (after an ingest:
    stored clusters stay as they are, new concepts join them or make new ones named by `llm-namer`, so a new source
    never reshuffles other topics; `{:fresh? true}` clusters from scratch, a cluster mostly an old one, by Jaccard,
    keeping its id and name; returns `:successors` {gone-id new-id}), `universe` (clusters with members, dominant
    source, links). Stored as `:cluster/*`; `bb ingest` refreshes them and moves goals on a gone cluster to its
    successor (`study/clusters-replaced!`); `remove!` a topic that is no knowledge and its concepts,
    `remove-concepts!` concepts and the background only they needed (they leave their clusters)
  - `keypoints` — key sentences: `extract!` (`:common` model, a few passages per call, 4 calls at a time) quotes
    1-3 sentences per passage verbatim with a 2-6 word label (the passage's hidden headings); `locate` keeps a quote
    only when found in its paragraph, as the paragraph's own text. Stored on the passage as `:node/keypoints`
    (`[]` for none; a re-ingest drops them with the node). `for-source` -> the `:keypoints` lookup
  - `db` (Datalevin knowledge schema + queries; `per-version`: a heavy read computed once per db version under a lock,
    e.g. `graph/load-graph` (Datalevin 1.2.10 can fail heavy queries run concurrently: "Comparison method violates its
    general contract"); `connect` merges other layers' schemas; `copy!` rebuilds a db under the
    current schema, e.g. after marking an attribute `:db/fulltext` — Datalevin's `re-index` loses full-text entries)
- `src/cljc/robertluo/fly_learner/ui/core/` — frontend logic, JVM-testable: `db` (pure state fns), `api` (the backend's wire
  format, the only place that knows it: requests as data, `fetch`/`create`/`change`/`delete`/`batch`, `pattern` = today's
  lasagna-remote pull pattern, `answer` reads the reply back, `send!` transit+json to `/api`, `source-url`; a new API
  format changes this namespace only, architecture test), `pull` (what to fetch when: `{:request :then}`, `:then` gets
  the answer; named ops in `resolve-pull`, `idea-create`/`idea-delete`/`goal` take arguments),
  `history` (URL <-> state; `/` the map, `/g/<id>` a cluster, `/x/<id>` an exploration, `/b` the notebook, `/s` settings, `/t/<term>` a term of the
  learner's; `re-matches` in ClojureScript is not anchored like the JVM's: no lazy `(.+?)` groups), `views` (Replicant hiccup,
  the root composes the header, popover and one view per route from `views.*`: `template` (generic: icons, score
  bars, zoom controls, the split divider; requires nothing of the app), `markdown` (rich text: what the model writes is
  Markdown with GFM tables and TeX, `$..$`/`$$..$$`, via nextjournal/markdown to hiccup; citations `[n]` become links
  through `:cite`; KaTeX draws formulas in the browser, vendored css + fonts), `nav` (links and effects to places), `panes`
  (split views, pannable drawings, full screen), `node`, `document` (the source beside the text, page by page,
  whatever its format: toolbar, page box, marks (key sentences, quotes, the open bar); `source-doc` -> `{:kind :pdf|:html
  :url}` picks how a page is drawn: pdf.js canvas + text layer, or the server's HTML page; db `doc-*`, state `:doc`), `ideas`, `selection`, `ask`, `quiz`, `concept`,
  `graph`, `home`, `explorations`, `settings` (the models: each tier's provider and model (suggested from what the
  provider serves), providers from presets with address, key, more; List its models, Try a tier; `db/models-draft`,
  `models-request`, `pull/models-save`; drop files: each upload's candidates, checked, parts to leave out,
  Ingest; jobs with their log, polled every 2 s while one runs (`:ingests-poll`, wherever the learner is; when all
  are done the knowledge is fetched again), the library, the models, the prompts by group with an editor:
  `db/prompt-*`, `pull/prompt-save`, `prompt-reset`; the header's gear, top right, turns while ingesting), `notebook` (what the learner wrote, as a book: by source and chapter in reading
  order, or newest first; highlights if asked; each note rewritten in place, the composer with `db/draft-edit`; downloaded
  as one Markdown file, `db/notebook-markdown`, `[[..]]` kept); one flow, map ->
  cluster -> concept -> passage; a concept page's "Learn first": its prerequisite path not yet known, foundations
  first, hidden ones badged "background", and what it leads to; "Exercises from the book" (the quiz says it is from
  the book, counts double, links to its page, shows the exercise it builds on, grades against the author's solution); study-next cards mark prerequisites outside the topic; the header holds the map, the concept index and the clusters being learned; the top box suggests as you type the concepts and
  book exercises its text names (`goto-list`: a concept opens, an exercise opens its passage with its quiz docked), Enter asks; a quiz started anywhere docks at the top of the left
  pane of split views, `quiz-dock`, and stays across pages), `highlight` (find a node's text among a page's text items: pdf.js's, or an HTML page's text nodes), `viewport` (pan/zoom of a drawing: automatic
  until the learner pans or zooms: whole if readable, else readable names around a focus; names below `readable-px`
  are not drawn; `db/pane-*` for `:map` and `:graph`, which goes automatic again on another graph),
  `explore` (draws an exploration's resolved elements; `diagram-layout`: layered, children under their parents;
  `ticks`), `graph` (concept graph: visible = focus + neighbours of expanded concepts, outline-style; neighbours include what
  a concept needs and some of what needs it; prerequisite edges drawn with an arrow halfway), `layout`
  (incremental force layout on primitive arrays: broader-above and prerequisite-above are constraints (`above-edges`
  drops one that would close a loop), label boxes never overlap;
  unconnected parts laid out apart and packed in rows below the largest, else repulsion strands them far away; tween)
- `src/cljs/robertluo/fly_learner/ui/` — `core` (dispatch-of effects `:db`/`:pull`/`:history`/`:animate`/`:upload` (`api/upload!`)/`:later` (effects after a delay), requests via `api/send!`, render,
  mouseup -> text selection), `pdf` (pdf.js: render a page, its selectable text layer, `page-text`), `html` (an EPUB's or
  a Markdown folder's pages: fetched once, formulas typeset by KaTeX then (`typeset`), `page-text` from the page's text
  nodes, a formula's being its TeX annotation, not its glyphs, so it matches the passage text; `when-laid-out!`), `pages` (whatever draws them: `find-each!`,
  `ensure-highlight!` over a kind's `page-text`, as boxes in page fractions; the source as one scrolling column of
  pages (PDF pages drawn near the view only, HTML ones always there): `sync-scroll!` keeps it at the state's page (and at the same spot when pages change
  size: width, zoom), `find-each!` finds texts on a page one by one (key sentences, what is open), `watch-scroll!` turns the page as the learner scrolls; when it pauses the left pane follows: `db/node-at-page` via
  the `:pages` lookup picks the node of the same kind and level on that page, returned whole so the document is never
  unmounted), `zoom` (pan/zoom gestures: drag,
  trackpad scroll, pinch = ctrl/⌘ + wheel; `fullscreen!`: the whole page, so popovers work; `db/fullscreen` follows
  the browser's `fullscreenchange` and shows only the pane; the map and the concept graph pan and zoom their viewBox, see `viewport`;
  the source zooms its pages' width (`db/doc-zoom*`, kept in localStorage; an HTML page's type is sized with it, `cqw`); the split views' divider is dragged in `views/split-handle`
  (`db/split-*`, the left pane's share, kept in localStorage)), `select` (the
  selection inside a `data-anchor` element as a quote anchor; `<br>` between pdf.js lines counts as a space)
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
  a restricted curriculum are offered too (study marks them `:prerequisite?`). Evidence may carry `:weight` (an answer
  of weight 2 moves mastery as far as two); material may carry `:exercises` (questions the sources' authors wrote).
  - `learning.decay` — forgetting as a `Model` wrapping any `Model` (FSRS-style stability); learning knows nothing of it
  - `restricted` — a `Curriculum` limited to some items: a learner focusing on part of it (its own `overall`,
    `next-items`; prerequisites outside still count for readiness)
  - `learning.store` — the evidence log in Datalevin (own schema), and goals: scopes the learner chose to learn now
    (opaque ids)
  - `learning.concepts` — the only bridge: the concept graph as a `Curriculum` (`[:concept id]`, `[:relation a b]`;
    prerequisites = `concept.prereq/dag` for judged concepts, broader otherwise; a hidden concept's material is its
    gloss and what needs it; a concept's material carries its book exercises, every concept they test as `:items`,
    the exercises they build on as `:context`);
    `concepts-items` = the items a set of concepts (a cluster) covers
- **features** (`quiz`: generate questions from an item's material, or `authored`: the material's exercises, weight
  `authored-weight` 2, graded against the author's solution; grade free text into `:answer` evidence, one per item
  tested, `evidence`, which the caller records; `attempts`: best grade per authored question, from the evidence log) —
  depend on learning only, never on knowledge nor the evidence store
- **ideas** (`ideas.cljc`, depends on nothing) — the learner's own voice: an idea takes a stance
  (`:agree`/`:restate`/`:disagree`), is a `:note`, or marks something: `:highlight`, `:bookmark` (a node anchor with the source
  `:page`; `ui.core.db/bookmarks`, kept out of the idea lists), on one or more anchors (opaque maps), and may link to other ideas.
  `Anchors` protocol (`target` -> a key ideas are grouped by, `describe`); W3C-style text quotes
  (`quote-selector`, `find-quote`, `mark-segments`) re-find a span after the text changes. What the learner writes
  (`:idea/text`) is Markdown, one stance a note; `[[a term]]` / `[[a term|shown]]` links what the term names
  (`wikilinks`; UI: the composer suggests concepts as `[[` is typed, `db/wiki-*`, and `views.markdown` `:wikilink` draws
  them); `[[A]] needs [[B]]` (requires, depends on, builds on, assumes; `[[B]] before [[A]]`) states a prerequisite
  (`claims`). What the notes say is derived from them on read, never stored (UI `db/my-terms`: links naming no concept,
  the learner's own terms, page `/t/<term>`; `db/my-claims`, `db/in-my-notes`: "In your notes" on a concept or term page),
  and stays in the learner's layer: not concepts nor `:prereq/*` claims, so the curriculum, topics and the model never
  see it. A term the sources do name can be made a concept on its page (curation's add, the learner's choice).
  Writing never moves scores: ideas and learning stay independent (a design decision, not a gap).
  - `ideas.store` — Datalevin, own schema
  - `ideas.knowledge` — the only bridge: anchors on concepts (follows merges), relations, nodes (re-found by
    title after re-ingest) and quotes in node text/summary or on a page of the source, `:field :page` (re-found by text; a page quote no
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
    :source/id :node/title}`: re-found by title in the source after a re-ingest
- **app** (`server.*`, `ui.*`; the UI knows item ids `[:concept id]` / `[:relation a b]` for the heat map) composes: `server.system` merges schemas and picks the model (mastery + decay)
  and the curriculum
- `prompts` — the system prompts given to the models (infra: any layer may use it): each namespace registers its own
  (`prompt!` id, group, title, about, default) and reads it at call time (`text`): the learner's version (`edit!`,
  kept in `data/prompts.edn` via `use-file!`) else the default; `reset!`, `all`
- `cli` — command-line tasks (app layer): `ingest!` (options, `--outline`, then `server.pipeline/run!`); `server.system/connect` opens the db with every layer's schema
- `dev/user.clj` — `(start!)` / `(stop!)` / `(cljs!)`, ingest snippet
- Tests: Kaocha runs `test/clj` plus Rich Comment Tests (`^:rct/test` comment blocks in `src/`)

## Commands (inside `devenv shell`)

- `bb dev` — nREPL (backend + shadow-cljs middleware); then `(user/cljs!)`: starts the backend (:8080) if needed and the frontend watch (:3000, proxies pages and `/api` to :8080). `(user/start!)` alone runs just the backend
- `bb watch` — shadow-cljs without a REPL; `bb serve` — release build served by the backend on :8080
- `bb jar` — release frontend + `clojure -T:build uber` (`build.clj`: AOT server + cli, the dev build's `js/cljs-runtime`
  left out, Datalevin flags in the manifest) -> `target/fly-learner.jar`, `java -jar` with `data/` under the working dir
- `bb image [tag]` — the OCI image from `Dockerfile` with docker, podman or Apple's `container` (first on PATH; `container
  system start` first): the jar built on `$BUILDPLATFORM` (clojure + node + bb images), run on
  `eclipse-temurin:21-jre-noble` (glibc: Datalevin's native lib) as `ubuntu`, `/app/data` the volume
  (`docker/entrypoint.sh` starts as root to chown a root-owned new volume, as Apple's `container` makes them, then
  `setpriv` to `ubuntu`), `JDK_JAVA_OPTIONS` carries the flags; `.dockerignore` lets in the sources only;
  `compose.yaml` (`host.docker.internal` for a local model server); `.github/workflows/image.yml` pushes
  `ghcr.io/<owner>/fly-learner` (main -> latest, `v*` tags -> semver) for amd64 + arm64. No keychain in a container:
  `llm/keychain-password` gives nil when `security` is missing
- `fly-up [image]` / `fly-down` / `fly-logs` (devenv scripts) — the image in Apple's `container` on this project's `data/`
  (default `ghcr.io/robertluo/fly-learner:latest`, or `FLY_IMAGE`; `FLY_PORT` 8080, `FLY_MEMORY` 4G); refuses while the
  dev REPL answers (one writer); passes `ANTHROPIC_API_KEY` from the environment, else the keychain, by name (`-e NAME`)
- `bb test`, `bb lint`; `bb install` / `bb vendor` copy pdf.js into `resources/public/vendor/pdfjs`
- `bb ingest data/sources/<file>.pdf|.epub|<folder of Markdown> ... [--skip <part>,..] [--outline]` — `cli/ingest!`
  (`--outline`: only print chapters, pages and a rough cost, no model nor db; `--skip`: a folder's parts or files left
  out, remembered for the path. A repository's docs folder: clone it (sparse: `git clone --depth 1 --filter=blob:none
  --sparse <url> data/sources/<repo>`, `git -C .. sparse-checkout set <folder>`) and ingest the folder) (ingest + fold, then `concept/resolve!`, clusters,
  `concept.prereq/judge!` for new concepts, `keypoints/extract!`, `exercises/extract!` + `exercises/transcribe!` for new sources and `explore.knowledge/propose!` for new passages; prints the cost).
  `bb/ingest_task.clj` evaluates it over nREPL in the running `bb dev` REPL when `.nrepl-port` answers (Datalevin
  must have one writer: the app's), else runs `clojure -M:ingest`. Paths are stored relative to the project
  (`cli/source-path`); a moved or renamed file re-ingests the same source (found by content).
  `dev/migrations/source_identity.clj` brought a db from before (path identity) up to this, once
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
