# ecoeval — Design

## In plain terms

You have two sets of results from the same papers: one the AI produced, one a
person produced by hand. You want to know how well the AI did.

You can't just lay the two tables side by side. People — and AI — often
describe the same thing in different ways: a different spelling, a name written
out in full on one side and shortened on the other, a whole sentence worded
differently but meaning exactly the same. The rows don't line up on their own,
even when they're describing the same thing.

The app handles that two ways. Text that's close but not identical is caught by
**fuzzy matching**, so a small difference in spelling or spacing doesn't get
counted as a mistake. Where the wording is completely different but the meaning
might be the same, it **asks an AI to judge** whether the two are describing the
same thing — and shows you its reasoning, so you can check it.

That lets it work in two steps. **First it pairs up the rows**, matching each AI
row to the human row describing the same thing, and shows you what it came up
with. You can break any pairing it got wrong and make any it missed — you have
the final say on every one. **Then it scores the result**: how often the AI
found the right things, how often it filled in each field correctly, and which
fields it gets wrong most.

You don't have to check its work row by row. There's a button that says "resolve
everything," and most of the time that's all you want — press it, read the
numbers, done. Going through papers one at a time is for when you want to see
*why* it's getting things wrong, so you can fix the setup and run again.

It also tells you something beyond the score. Sometimes the AI isn't the
problem — the person recorded a kind of answer nobody ever told the AI to look
for. That's worth knowing, because it's fixable before the next run, and no
amount of improving the AI would have helped.

It's built to be used more than once: run it, see what's wrong, change
something, run it again.

---

## Goal

A dashboard for evaluating AI extraction output against a human gold standard.
Sibling to `ecoextract` (extraction) and `ecoreview` (human review).

Entry point: `ecoeval::run_eval_app()`

**Keep it simple.** The app has one job: measure how accurate the AI extraction
was. Everything below serves that.

Before any accuracy number means anything, we have to decide which AI row
corresponds to which gold row. That is the core of the app, and it is why the
human stays in the loop: a person looking at two rows side by side can tell they
describe the same interaction even when the text doesn't match. An automated
matcher often can't.

## What it produces

1. **Accuracy numbers** — did the AI find the right records, and get the values
   right?
2. **Signals that help you read those numbers** — a category the human recorded
   that the schema has no slot for, a field humans always fill and the AI
   rarely does, two datasets that disagree about what counts as one record.

The second is **context, not a second score.** The app surfaces these and shows
you the evidence; it does not adjudicate them or fold them into a metric. A
recurring value the schema can't express might mean the schema needs a new
category — or that the annotator was inconsistent. You decide which, and the
numbers read differently depending on the answer.

Both matter, because this is an **iterative tool**: run it, learn something,
fix the schema or the prompt, re-extract, run it again.

---

## Inputs

Four data inputs, symmetric across the two sources, plus the schema:

| | Paper list | Record list |
|---|---|---|
| **AI** | papers processed | records extracted |
| **Gold** | papers reviewed | records recorded |

For an ecoextract database the paper list is free — it's the `documents`
table, which knows every paper processed regardless of whether extraction
produced records. For flat CSV/Excel input it's a separate file.

### Plus the schema — required

`schema.json` is what the AI was extracting against, and it's a **required
fifth input**. It drives:

- `x-unique-fields` → default linkage suggestion and the collapse check
- field types → comparator defaults
- `enum` → the K×K matrix decision, and conformance checks on both sides

It isn't stored in the ecoextract database today, so it must be located on disk
or uploaded. Once ecoextract #137 lands (schema and prompt stored in the DB with
hash linking) it travels with the AI database and the upload step disappears.

### Why the paper lists matter

Paper lists are optional. They **widen scope** — they don't change how anything
is scored.

Each source's paper set is its paper list when supplied, otherwise the papers
appearing in its records. Scope is the intersection either way.

**With neither list**, scope is papers where *both* sources have records. A
paper someone read and found nothing in has zero rows, so it looks identical to
one they never opened and drops out. That's fine and self-consistent: the
evaluation is over papers where both parties found something, and the exclusion
is symmetric — the AI's worst over-extraction and its worst under-extraction
both fall outside scope. No caveat needed beyond reporting what's in scope.

**Each list added widens scope in one direction**, letting you see a case that
was previously invisible:

| List supplied | Papers it brings into scope | What that lets you catch |
|---|---|---|
| Gold | Human reviewed, found nothing | AI **over**-extraction — records where the human found none |
| AI | AI processed, found nothing | AI **under**-extraction — papers it whiffed entirely |

Those are worth seeing. When the AI finds records in a paper the human left
empty, it's either over-extraction (a false positive that should count) or a
genuine gap in the gold standard (something the reviewer missed and should
add). Both are findings.

**Only a lopsided pairing needs a warning.** If exactly one side supplies a
list, coverage is wider in one direction than the other, so quantify it:

> The AI has no paper list, so 153 papers where it produced records but the
> gold standard has none are in scope, while papers it processed and found
> nothing in are not. Coverage is wider for over-extraction than
> under-extraction.

Persistent (travels with exported results, not a dismissible popup) and
self-extinguishing — it disappears once both lists are present.

---

## Scope

**Papers: intersection. Records: union.**

Scope is the papers present in both paper sets — each source's paper list when
supplied, otherwise the papers appearing in its records. Within those papers,
every record from both sides appears; some pair up, some don't.

Papers are a filter, not a scored entity. There is no paper-level confusion
matrix, and papers outside the intersection are excluded, never penalized.
Gold-only papers simply narrow the comparison set; they carry no warning.

```
Scope: 47 papers evaluated

  47   in both    — evaluated
 153   AI only    — excluded
   4   gold only  — excluded
```

Paper alignment determines scope, not score. Its asymmetry is worth knowing: a
*missed* paper match just shrinks the evaluation set, but a *wrong* paper match
compares one paper's records against another's and produces garbage. That's
what the paper-link review guards against.

---

## Workflow

1. **Load** — four data inputs plus `schema.json`
2. **Map metadata fields** — must precede paper alignment; we need to know
   which gold column holds the DOI, author, year
3. **Paper alignment** — fastLink on paper metadata, human accepts. Sets scope.
4. **Map record fields** — column mapping, column selection, comparator config,
   pick linkage fields
5. **Comparison view** — one paper at a time; alignment review *and* results on
   one screen
6. **Dashboard** — aggregate metrics across all papers, updating as review
   progresses

---

## Stage 4 detail

### Field mapping

1:1 mapping plus "ignore". Pre-populate with exact and fuzzy name matches.
Many-to-one mapping (gold's single `Location` → AI's `country`/`region`/`site`)
is **not supported** in v1; users pre-process. Known limitation.

### Comparators

Without normalization the first run is a wall of disagreement that's mostly
formatting noise. Defaults come from JSON Schema type — the only inference
available without domain knowledge, and enough:

| Type | Default comparator |
|---|---|
| `enum` | Exact |
| `string` | Trimmed, case-insensitive |
| `number` / `integer` | Numeric tolerance ±ε |
| date-formatted string | Parse, then compare |
| `array` | Set comparison (exact set / Jaccard / any-overlap) |
| free text | Fuzzy ≥ threshold (`stringdist`) |

**Threshold picking gets a chart.** When a column uses a fuzzy comparator, plot
the distribution of similarity scores across that column's actual pairs so the
user places the cutoff by looking at their own data rather than guessing.
(Borrowed from Splink — see the charts section.)

#### The cascade

Comparison is a ladder, not a single test: **exact → normalized → fuzzy →
LLM judge**. Each rung sees only what the previous couldn't resolve, so the
expensive rungs run on a small minority of cells.

Two LLM-backed strategies sit at the top, suiting different columns:

| | Mechanism | Cache key | Best for |
|---|---|---|---|
| **Normalize-then-compare** | map both values to a canonical form, then compare | per **value** | species names — a canonical form exists, and per-value caching is far cheaper |
| **LLM judge** | compare the pair directly, return verdict + rationale | per **pair** | supporting sentences, free prose — no canonical form to map to |

`all_supporting_source_sentences` is the motivating case: two records can point
at the same fact with entirely different text, so string distance is hopeless
and a judge is the only sensible comparator.

Rules for the judge:

- **Verdict stays binary.** The rationale carries nuance; a fifth colour would
  break the scheme.
- **Cache verdicts into `run_config.json`.** LLM output varies between calls,
  so a reproducible run needs them frozen rather than re-derived.
- **Rationale surfaces in the cell modal**, alongside both values and which rung
  of the cascade decided it. That makes every yellow cell auditable.

`ecoreview::standardize_name_vector()` already does LLM name harmonization via
`ellmer`, so the plumbing exists — but it's biology-specific, so users assign it
to chosen columns rather than getting it by default. Comparators should be
pluggable so other domains can supply their own.

#### Where each rung runs — normalization comes before matching

fastLink already does string-distance comparison internally
(`stringdist.match` / `partial.match`, Jaro-Winkler with cutoffs), so
`"Myotis lucifugus"` vs `"Myotis lucifigus"` is scored as agreement by the
matcher without help. **Plain fuzzy needs no pre-step.**

What fastLink cannot see through is semantic equivalence with low string
similarity — `"Myotis lucifugus"` vs `"little brown bat"`,
`"Pipistrellus subflavus"` vs `"Perimyotis subflavus"` after a genus
reassignment, or a binomial against its abbreviation. Left un-normalized, the
matcher systematically fails to pair records that obviously correspond, on
exactly the fields it depends on most.

**The judge can't move earlier.** Alignment evaluates every candidate pair
within a paper — 5 AI × 5 gold is 25 pairs × N linkage fields of LLM calls just
to build the comparison matrix. Normalization is per *value*: 10 values, 10
calls, then compare cheaply. Normalization scales to N²; the judge only scales
to N.

So the order is:

1. User picks linkage fields and comparators
2. **Normalize linkage fields** on both sides — per value, cached, a no-op when
   no normalizing comparator was assigned
3. Run fastLink on the normalized values
4. Score cells with the cheap rungs; the judge fires per paper as the user
   reaches it, on paired rows only, and only on cells the cheaper rungs
   couldn't resolve

Two consequences:

- **Display originals, compare normalized.** The grid shows what was actually
  recorded; the modal shows the normalized forms and which drove the verdict.
- **The collapse check runs on normalized values.** `"M. lucifugus"` and
  `"Myotis lucifugus"` should collapse together; running it raw undercounts.

### Schema conformance — pre-flight, both sides

Validate AI and gold values against the schema (enum membership, type). Run it
as a pre-flight check, warn if either side has exceptions, and **mark the
offending cells in the grid**.

The warning lists the non-conforming values **with their frequencies**, because
that's all the user needs to read the situation themselves:

```
interaction_type — 3 values not in schema enum

  commensalism   47×   (gold)
  phoresy        12×   (gold)
  predatoin       1×   (gold)
```

A value recurring 47 times is a category the schema is missing. A single
`predatoin` is a typo. No classifier, no confirmation flow — the counts make it
obvious, and the two have opposite fixes (edit the schema vs. clean the gold
standard).

**Nothing is adjusted for in the metrics.** Scoring treats both sides at face
value:

- **AI side** — an invalid value is simply a **true error**, scored as wrong
  like any other. Structured output should have prevented it, so it's also
  worth flagging as a pipeline problem, but it needs no special accounting.
- **Gold side** — the user goes and fixes their gold standard. Not something
  the tool models.

That's the whole design: warn, mark the cells, let the human act. No ceiling
arithmetic, no reclassification, no confirmation flow.

Note the *scoring* side already absorbs formatting variance anyway — the
normalized comparator scores `"Predation"` against `"predation"` as agreement
regardless. This check exists to surface noise worth cleaning and categories
worth adding.

### Linkage fields

The user picks them from the mapped column list. `x-unique-fields` from the
schema is a **default suggestion when present, nothing more** — it may be
absent entirely (it's an optional JSON Schema extension), and it was written to
control the AI pipeline's deduplication, not to define correspondence with an
independently-built human dataset.

### Granularity check

Group gold records by the chosen linkage fields within each paper and count
collapses. If many gold rows collapse together, the two datasets disagree about
what a record *is* — the human distinguished records by something the linkage
key doesn't capture. Warn; the user adjusts their field selection.

Papers in a paper list with no records are expected, not a violation.

---

## Stage 5 — the comparison view

This is the heart of the app. Alignment review and results are the same screen:
judging a link is far easier with every field visible than in an abstract table
of confidence scores.

### One paper at a time

Review is **paper by paper**, with a navigator to move through the scoped set —
the same rhythm as ecoreview's document-at-a-time flow, which users already
know.

That keeps everything bounded. A single paper holds at most ~100 records and
usually far fewer, so the grid is small, rendering is unremarkable, and recompute
on every edit is free. There is never a ten-thousand-row grid to virtualize.

A **progress indicator** sits alongside, tracking the three states described
under *Two ways to use this* — *"47 of 47 judged · 3 reviewed"* — so the user
always knows how the current numbers were produced.

Paper-by-paper is the **iteration** path, not a mandatory gate. Someone who only
wants an accuracy figure never has to open this screen.

### The grid — for the current paper

- **Rows**: every record from both sides for this paper — matched pairs, then
  AI-only, then gold-only.
- **Columns**: evaluated fields, with **identity columns** — the linkage fields
  chosen in Stage 4 — pinned left and rendered as text (AI value over gold
  value), the way a spreadsheet freezes ID columns. That's what makes the scan
  fast.
- Remaining columns are color-coded; click any cell for a modal.
- Gold columns with no AI counterpart (and vice versa) are excluded from the
  grid entirely — there is nothing to compare — but they're reported as a
  finding so the omission is visible.
- Optional toggle to expand every row into two lines (AI / gold) when reading
  values across the whole grid.

| Color | Meaning |
|---|---|
| Green | Matched pair — values agree under that column's comparator |
| Yellow | Matched pair — values disagree |
| Orange | Gold-only record |
| Purple | AI-only record |

**Schema violations are marked, not coloured.** A value failing enum or type
validation gets a marker or outline on the cell rather than a fifth fill
colour — validity is orthogonal to match state, a cell can be both, and either
side's value (or both) may be the offender. Fill carries agreement; the marker
carries validity.

**The cell modal** shows both values, which rung of the comparator cascade
decided the result, and — where the LLM judge was the decider — its rationale.
Any schema violation is named there too. That makes every yellow cell
auditable, which matters when someone challenges a number.

### The three operations

At the **row** level, the user fixes the pairing:

- **Reject a link** → the pair de-links into an orange row and a purple row
- **Link two orphans** → select an AI-only row and a gold-only row, join them

At the **cell** level, the user fixes the verdict:

- **Override a cell** → for any cell in contention, assert that the two values
  really do mean the same thing (or really don't), overriding the colour the
  comparator or judge assigned

All three recompute every metric immediately, and all three persist in
`run_config.json` alongside the mappings.

**Linking matters more than rejecting.** Rejection can only make the alignment
sparser. The pairs a matcher misses are exactly the ones whose identifiers
disagree — and those are the ones a human can recognize as the same entity.
Without manual linking, the user can only correct in one direction.

**The cell override is what makes the judge safe to rely on.** An LLM verdict
is a proposal; the human has the last word, and every override is recorded
rather than silently applied.

Show a small **manual-correction count** ("12 links rejected, 5 added, 9 cells
overridden"). Since every one of those moves a number, a hand-tuned result
should say so.

### The matcher

fastLink, blocked per paper, **permissive — no similarity floor**. It proposes
pairings even when key identifiers disagree, and the human judges. Dropping a
pair because the identifiers look wrong is precisely the decision the human
should be making with the values in front of them.

**fastLink is the matcher, not the reporter.** We use the linkage algorithm and
the per-link posterior probabilities (which drive sort-by-confidence in the
review). We do not surface its diagnostics — see the fastLink notes below.

Alignment quality is a plain status line, not a matrix:

> Alignment: 47 of 50 links high confidence · 3 need review →

### Two ways to use this — both first-class

The judge is **trusted by default**. Once alignment is done, it resolves
contested cells on its own; the human override exists for when it gets one
wrong, not as a step everyone must walk.

That gives two legitimate paths, and neither is the shortcut:

**"How accurate is it?"** — press **Resolve all differences**. The judge runs
across every scoped paper, the dashboard fills in, and the user never opens a
single paper. This is the common case and should feel like the main road.

**"Why is it wrong?"** — walk the papers, read the mismatches, spot the
patterns worth fixing before the next extraction run. This is what you do when
you're iterating on the schema or prompt rather than reporting a number.

Either way the judge fires **once per paper**, on paired rows only, and only on
cells the cheaper rungs couldn't settle — lazily as papers are opened, or all at
once from the batch action. Same work, different trigger.

Batch needs the usual courtesies: an **estimated call count shown before it
runs**, a progress indicator, and resumability — verdicts cache into
`run_config.json` as they complete, so an interrupted run picks up where it
stopped.

#### The batch run doubles as triage

A batch pass knows which pairs disagree on *everything*, which is the strongest
cheap signal that a pairing is wrong. So it should emit a short list — *"6 pairs
disagree on every field; 3 papers look off"* — turning "review all 47 papers or
none" into "look at these three."

That's the bridge between the two paths: run batch, get numbers, then spend
review effort only where the numbers say it's worth spending.

#### Progress has three states, not two

Worth distinguishing on the dashboard, because they mean different things:

| State | Meaning |
|---|---|
| **Unjudged** | cheap comparator rungs only; contested cells unresolved |
| **Judged** | the LLM settled the contested cells |
| **Reviewed** | a person looked at it |

*"47 of 47 papers judged · 3 reviewed"* is an honest description of a fast-path
result, and it tells a later reader exactly how the numbers were produced
without editorialising about it.

#### What the fast path costs you

Skipping review means accepting the matcher's pairings. Per the accounting
below, **field-level metrics are largely robust to that** — a wrongly-paired
row that disagrees everywhere scores the same as two unpaired rows.
**Record-level precision and recall are not**, since pairing converts 1 FP + 1
FN into 1 TP.

So the fast path yields field-level numbers you can lean on and record-level
numbers that inherit whatever fastLink decided. Worth knowing; not worth a
warning banner.

### One-to-one only

Matching is 1:1. Leftovers on either side are false positives and false
negatives — because that is what they are. No duplicate-cluster category, no
`dedupeMatches`, no special accounting.

An extra AI record *is* a false positive; whether it came from a hallucination
or a dedup failure is a diagnosis the user makes from the collapse warning and
the duplicate rate, not something the metric needs to model.

---

## Metrics

### One unified accounting

Computed over the **entire set** — every record from both sides, matched or
not. Per column:

| Cell | Contributes |
|---|---|
| Green — matched, agree | **TP** |
| Yellow — matched, disagree | **FP + FN** |
| Purple — AI-only with a value | **FP** |
| Orange — gold-only with a value | **FN** |
| Both blank in a matched pair | TN (drops out of P/R/F1) |

Yellow counting as both is the standard multi-class treatment: a
misclassification is a false positive for the value asserted and a false
negative for the value missed.

**Useful property:** a wrongly-linked pair that disagrees on everything
produces FP + FN per column — the same as leaving those rows unlinked (purple
gives FP, orange gives FN). So linking earns nothing except where fields
genuinely agree, and bad links cannot inflate field accuracy. Field-level
metrics are largely robust to alignment error. Record-level metrics are not —
pairing two records converts 1 FP + 1 FN into 1 TP — which is exactly the
judgment the human review exists for.

### Two confusion matrices, both about extraction

Plainly labelled. There is no third matrix about linkage quality — users
should not have to learn that distinction.

- **Records** — did the AI find the right rows? (matched / AI-only / gold-only)
- **Fields** — did it fill them in correctly? (per column, plus aggregate)

### Column-level matrices

Click a column header. Form adapts to the column's type:

- **Enum / boolean / low-cardinality** → true K×K, gold class × AI class. Shows
  which values get confused with which. Needs an explicit **"other / not in
  schema"** row and column so out-of-enum values appear rather than being
  silently dropped — those are among the most interesting cells on the chart.
- **Free text / high-cardinality** → K×K over thousands of species names is
  useless, so 2×2 on presence/absence — with the top-left cell **split into
  "correct" and "wrong value"**. That split is what makes it useful: it
  separates *didn't populate the field* from *populated it wrong*, which have
  entirely different causes.
- **Numeric / date** → presence/absence 2×2 plus an error distribution.

Flag columns where a single class dominates (accuracy is trivially high and
uninformative) and columns with too few matched records to mean anything.

### Aggregate

Two numbers, plain labels — no micro/macro jargon:

- **Overall accuracy (all cells)**
- **Average across columns (each column weighted equally)**

They diverge when fill rates are uneven, and the gap is itself informative.

### Column-wise accuracy chart

Sorted worst → best. The triage view — but read it carefully, because a column
that is uniformly yellow has three possible causes and the tool can't tell them
apart:

1. **A misconfigured comparator** — exact matching on a date column with two
   formats, or a fuzzy threshold set too tight. On a first run this is the most
   likely explanation, and it's the cheapest to check.
2. **An ambiguous field description** — the prompt didn't pin down what to put
   there.
3. **The model genuinely failing** on that field.

Check them in that order. The column chart tells you *where* to look, not why.

---

## Findings

Findings are output, not gates. They're grouped by **who acts on them** — the
schema, the model, or the gold standard. Nothing here adjusts a metric; the
groups exist so the user knows where to go next.

**Schema / prompt findings** — recurring gold values outside a field's `enum`
(a category the schema is missing) · granularity mismatch from the collapse
check · gold fields with no schema counterpart · fill-rate asymmetry (humans
always populate it, the AI rarely — usually a field-description problem) ·
uniformly wrong columns · per-paper cardinality skew · type mismatches

**Model findings** — field accuracy on well-specified columns · record-level
precision/recall · which values get confused · AI values that violate the
schema · duplicate rate (`ecoreview::compute_duplicate_rate()` — the pipeline's
own dedup failing)

**Gold standard findings** — one-off values outside the enum (typos, format
variants) · records with unresolvable paper references · anything suggesting
the reference data needs cleaning rather than the schema needs changing

**Source consistency checks** (per source, internal — these run at load,
alongside the schema conformance check described under Stage 4) — every paper
referenced in a record list must appear in that source's paper list. Records
with a null or unresolvable paper reference. Duplicate paper IDs.

The paper-list violation deserves care. The obvious repair is to union the
missing papers in — if a paper produced records it was obviously reviewed. But
say the consequence out loud rather than quietly patching:

> A review list that omits papers *with* records cannot be trusted to include
> papers *without* records.

And the empty papers are the entire reason the list exists. Offer the union,
but flag that the list's reliability is in question.

### The two things that actually block

Everything above is output. Only two conditions stop the app rather than
informing it, because neither leaves anything to compare:

- **Zero paper overlap** between the two sources
- **No mappable fields** between the two record sets

---

## Iteration

Two things make this an improvement tool rather than a report generator:

**Runs are versioned and diffable.** The session artifact becomes a run
history, and the dashboard diffs runs:

> F1 0.71 → 0.78 · `interaction_type` accuracy 0.62 → 0.91
> (added `commensalism` and `phoresy` to the enum)

Without run-over-run comparison, iterating is blind.

**Schema findings emit an applicable patch**, not prose:

```
x-unique-fields:        + "location"
interaction_type.enum:  + "commensalism", + "phoresy"
bat_species_common_name.description:  ambiguous — 34 confusions with scientific name
```

The user reviews and applies. This closes the loop mechanically instead of
leaving them to translate a dashboard into schema edits by hand.

**Session state must persist** — field mappings, comparator config, linkage
field selection, and every manual link/reject decision. The review steps are
real human labour and must survive a refresh. Persisting them also makes an
evaluation reproducible and exportable.

---

## Export

The session artifact and the export bundle are the same object, so save,
restore, and download share one implementation.

```
ecoeval_run_2026-08-31/
  aligned_table.csv        one row per aligned record, mirrors the grid
  aligned_table_long.csv   tidy: one row per record × field, for pivoting
  metrics.xlsx             one sheet per field + summary sheet
  findings.csv             findings, grouped schema / model / gold
  schema_patch.json        proposed schema edits (when a schema was supplied)
  plots/                   column accuracy, per-field confusion matrices (PNG)
  run_config.json          mappings, comparators, linkage fields, judge verdict
                           cache, every manual link/reject decision, scope
  README.txt               what's here, scope statement, any active warnings
```

- **`metrics.xlsx`** is the tabbed workbook: one sheet per field holding that
  field's confusion matrix and its precision / recall / F1 / n, plus a summary
  sheet listing every column as a row with the two aggregates.
- **`run_config.json` is what makes a run reproducible.** Reload it and you are
  exactly where you were — manual link decisions and cached LLM verdicts
  included. It's also what the run-over-run diff reads.
- **`README.txt`** carries the scope statement and any active warnings, so
  caveats travel with the export instead of living only on screen.

All plots individually downloadable as PNG from the dashboard as well.

---

## Charts worth borrowing

Most of [Splink's chart gallery](https://moj-analytical-services.github.io/splink/charts/index.html)
is Fellegi-Sunter model internals — match weights, m/u parameters, waterfall,
TF adjustment, ROC against labelled link data, cluster studio. That is exactly
the category we already decided not to surface, so it confirms the decision
rather than supplying ideas.

Three are worth taking:

1. **Comparator Score Threshold Chart** — the best of them. Plots the
   distribution of similarity scores across actual candidate pairs so a cutoff
   is chosen by looking at real data rather than guessing. Belongs directly in
   comparator configuration.
2. **Profile Columns / Completeness Chart** — per-column fill rates and value
   distributions, rendered side by side AI vs gold. Already wanted for the
   fill-rate asymmetry finding.
3. **Unlinkables Chart** — Splink flags records too poor to link even to
   themselves. Our analogue is linkage identifiability: *"5 records in this
   paper are indistinguishable on your chosen linkage fields."*

**Stack:** Splink renders with Vega-Lite/Altair. `ggplot2` (4.0.2) and `plotly`
(4.12.0, already an ecoreview dependency) are sufficient — confusion matrices
are `geom_tile`. No new charting dependency, and `yardstick` isn't worth adding
for what amounts to a tile plot.

---

## What fastLink gives us

Verified against **fastLink 0.6.1**.

**Used:** the linkage algorithm itself, per-paper blocking, and per-link
posterior probabilities for sorting the review.

**Not used:** `confusion()` returns a linkage-quality matrix with fractional
counts (50.0, 0.3, 299.7 — EM-posterior expected values, not tallies) that
needs a paragraph to explain and changes no user decision. `summary()`'s
threshold table matters little now that we impose no similarity floor.

**Broken:** `inspectEM()` errors in 0.6.1 (`object 'em' not found`, a scoping
bug). Don't build on it.

**Possibly useful later:** `dedupeMatches(linprog = TRUE)` implements Winkler's
linear-programming solution if 1:many ever needs handling.

fastLink ships tables, not plots — there is no graphical output to reuse.

---

## Reuse from ecoreview

`R/benchmark.R` already covers *replicate variability* (same papers, N trials,
self-consistency). ecoeval is the complementary axis and shares machinery:

| Function | Signature | Reuse |
|---|---|---|
| `standardize_name_vector(names, ...)` | takes a vector | **Direct** — drops straight into the normalizing comparator |
| `collect_benchmark_results(benchmark_dir)` | reads `_trial_N.db` from a directory | **Logic only** — replicate-pipeline shaped, not a drop-in |
| `compute_duplicate_rate(benchmark_dir, ...)` | reads `benchmark_records.csv` from a directory | **Logic only** — the group-and-count on unique fields transfers, the signature doesn't |

Worth being honest about: only the first is reusable as-is. The other two are
directory-oriented pipeline functions and would need refactoring to take data
frames, or reimplementing. Budget accordingly.

Leaning: leave them in ecoreview and depend on it.

---

## Architecture

- New package `ecoeval`
- Entry: `ecoeval::run_eval_app(ai = NULL, gold = NULL, schema = NULL)` — all
  optional at launch; the app collects whatever is missing, but `schema` must be
  supplied before the comparison stage
- Wizard with a persistent progress rail; stages revisitable without redoing
  everything downstream

### Dependency posture

`ecoextract` and `ecoreview` sit in **Suggests, not Imports**, and calls are
guarded with `requireNamespace()`. Reasons:

- ecoeval must work on a plain CSV gold standard and a plain CSV record set. Its
  only hard requirement is a record table plus the `schema.json` it was
  extracted against — an ecoextract database is a convenience, not a premise.
- Hard-importing two GitHub-only packages makes ecoeval uninstallable whenever
  either sibling is mid-refactor.

What each is used for when present:

| Package | Used for |
|---|---|
| `ecoextract` | reading a `.db` input — `get_documents()` for the AI paper list, record export |
| `ecoreview` | `standardize_name_vector()` as the normalizing comparator |
| `ellmer` | the LLM judge and the normalizer underneath it |

**Note the coupling:** `ecoreview::standardize_name_vector()` is the only
direct-reuse function, so a signature change there breaks ecoeval's normalizing
comparator. Worth a comment in both places.

### Shiny structure — modularise from the start

ecoreview keeps its entire app in one `inst/app/app.R`, now **2,772 lines**.
That works but is genuinely painful to edit; this session spent real effort on
surgical string-matching inside it.

ecoeval has a natural module boundary per stage, so take it:

```
inst/app/
  app.R              thin — assembles modules, holds the top-level reactives
  modules/
    mod_load.R           the five inputs + consistency checks
    mod_map_metadata.R   metadata field mapping
    mod_align_papers.R   paper alignment, sets scope
    mod_map_records.R    field mapping, comparators, linkage fields
    mod_compare.R        the per-paper grid — the big one
    mod_dashboard.R      metrics, matrices, charts
  www/
R/
  run_eval_app.R     launcher, argument handling
  comparators.R      the cascade — pure functions, no Shiny
  alignment.R        fastLink wrappers, per-paper blocking
  metrics.R          the unified FP/FN accounting — pure functions
  schema.R           schema parsing, conformance checks, type→comparator defaults
  session.R          run_config.json read/write
  export.R           the bundle
```

**Keep the scoring engine free of Shiny.** `comparators.R`, `metrics.R`, and
`schema.R` should be plain functions over data frames, so they're unit-testable
without launching an app and reusable from a script. The app becomes a shell
over a library, which is also what makes a headless batch mode possible later.

### Project conventions — mirror the family

- **pkgdown**: `_pkgdown.yml` with `bootswatch: flatly` and `navbar: bg: primary`
  to match ecoextract/ecoreview. Deployed with `pkgdown::deploy_to_branch()` to
  `gh-pages` (repo Pages source: `gh-pages` / root). The generated site is
  gitignored on `main` (`docs/*`) since it lives on the branch, with an
  exception for this file (`!DESIGN.md`).
- **NEWS.md**: `# ecoeval X.Y.Z` h1 per release — pkgdown requires it. The
  resulting MD025 "multiple top-level headings" lint warnings are expected false
  positives; `.markdownlint.json` is copied from ecoreview.
- **Version + install loop**: bump the version in `DESCRIPTION` on every change
  you intend to test, then `renv::install("n8layman/ecoeval")`. renv caches by
  version, so an unbumped change silently reinstalls the old build.
  `devtools::load_all()` is not sufficient for testing app behaviour.
- **Vignettes**, mirroring ecoreview's three-guide shape:
  - `ecoeval-workflow.Rmd` — installation, inputs, running an evaluation
  - `aligning-records.Rmd` — why alignment is the hard part, the comparator
    cascade, when to review by hand
  - `metrics.Rmd` — how the FP/FN accounting works, reading the matrices
- **Secrets**: `.Rprofile` sources `.env` via `readRenviron()`; `.env` is
  gitignored and `.env.example` documents the keys. Only needed for the judge
  and normalizer.

---

## Implementation roadmap

Tracked as issues on
[n8layman/ecoeval](https://github.com/n8layman/ecoeval/issues). Each issue
carries the decisions relevant to it, so it can be worked without reading this
whole document — but the reasoning behind those decisions is here.

**Design and scaffold**

| # | Issue | Section | State |
|---|---|---|---|
| [1](https://github.com/n8layman/ecoeval/issues/1) | Port the design document into the repo | *this file* | done |
| [2](https://github.com/n8layman/ecoeval/issues/2) | Package scaffold | Architecture | done |

**Core pipeline — build in this order, each depends on the last**

| # | Issue | Section | State |
|---|---|---|---|
| [3](https://github.com/n8layman/ecoeval/issues/3) | Load the five inputs | Inputs | done — `io.R`, `mod_load.R` |
| [4](https://github.com/n8layman/ecoeval/issues/4) | Pre-flight consistency checks | Schema conformance; Findings | done — `schema.R`, `findings.R` |
| [5](https://github.com/n8layman/ecoeval/issues/5) | Metadata mapping + paper alignment (sets scope) | Scope; Workflow 2–3 | done — `mod_map_metadata.R`, `mod_align_papers.R` |
| [6](https://github.com/n8layman/ecoeval/issues/6) | Record field mapping, columns, comparator config | Stage 4 detail | done — `mod_map_records.R` |
| [7](https://github.com/n8layman/ecoeval/issues/7) | Comparator cascade | The cascade; Where each rung runs | done — `comparators.R`, `judge.R`; LLM rungs unexercised against a live API |
| [8](https://github.com/n8layman/ecoeval/issues/8) | Record alignment — fastLink blocked per paper | The matcher; One-to-one only | done — `alignment.R` |
| [9](https://github.com/n8layman/ecoeval/issues/9) | Metrics engine | Metrics | done — `metrics.R` |

**UI**

| # | Issue | Section | State |
|---|---|---|---|
| [10](https://github.com/n8layman/ecoeval/issues/10) | Comparison grid — one paper at a time | Stage 5 | done — `mod_compare.R` |
| [11](https://github.com/n8layman/ecoeval/issues/11) | The three manual operations | The three operations | done |
| [12](https://github.com/n8layman/ecoeval/issues/12) | "Resolve all differences" — batch judge | Two ways to use this | done — needs an API key to exercise |
| [13](https://github.com/n8layman/ecoeval/issues/13) | Dashboard: confusion matrices and charts | Metrics; Charts worth borrowing | done — `mod_dashboard.R`, `plots.R` |

**Output and iteration**

| # | Issue | Section | State |
|---|---|---|---|
| [14](https://github.com/n8layman/ecoeval/issues/14) | Session persistence and export bundle | Export | done — `session.R`, `export.R` |
| [15](https://github.com/n8layman/ecoeval/issues/15) | Run-over-run diff and schema patch | Iteration | done — `diff_runs()`, `schema_patch()` |

### Two things the build learned

Both are in the code with comments, and both are worth knowing before touching
the matcher.

**The linkage model must be fitted over the whole corpus, then applied per
block.** fastLink's EM learns which fields discriminate *from the data it is
given*, and a per-paper block holds a handful of records. Fitting per paper, it
will confidently pair the wrong two records out of two — on the fixtures it
paired a species that matched exactly against one that did not, because the
other linkage field happened to near-match. `fit_linkage_model()` estimates once
over every scoped record; `link_block()` applies that model per paper.

**fastLink is not deterministic.** It clusters string-distance values
internally and that clustering is randomly initialised, so two identical calls
returned different pairings. `run_config.json` promises reproducibility, so
`ecoeval_seed()` pins the seed for every call into fastLink, restoring the
caller's RNG state afterwards.

### Departures from this document, and why

* **Free-text columns default to `judge`, not `fuzzy`.** The comparator table
  says free text gets fuzzy, but the cascade section names supporting sentences
  as the motivating case for the judge. Defaulting to `judge` satisfies both:
  the cascade still runs exact, normalized and fuzzy first, so nothing costs
  money until they have failed, and a run with no API key reports those cells as
  *unjudged* rather than scoring reworded prose as wrong.
* **A blank on one side of a matched pair is not a full disagreement.** The
  accounting table says yellow contributes FP + FN, but the purple and orange
  rows both carry the qualifier "with a value". Applying it consistently: a pair
  where the AI is blank and the gold has a value contributes FN only, and the
  reverse contributes FP only. Both still render yellow.
* **Triage also flags pairs whose identity columns all disagree**, not only
  pairs that agree on nothing. The narrower rule missed a genuinely mispaired
  record in the fixtures that happened to agree on country and year.

---

## Deliberately out of scope

Recorded so they don't creep back:

- **Leave-one-field-out linkage** — corrects a selection bias that mostly
  disappears once there's no similarity floor and a human reviews every row.
  Doesn't compose with human review (LOFO alignments propose pairs the human
  never saw) and breaks the heatmap by putting several alignments in play at
  once.
- **Inferring the gold standard's implicit record key** — subset search with
  overfitting and low-confidence caveats, to replace a choice the user is
  already making by hand. The collapse check is the cheap version that survives.
- **Gold-standard exhaustiveness machinery** — no assertion flag, no gating of
  precision metrics, no adjudication sampling, no coverage estimation. A pile of
  purple AI-only rows *is* the signal; humans go update the gold standard.
- **Linkage confusion matrix** — replaced by a one-line status.
- **Threshold-sensitivity plot** — little left to vary without a floor.
- **Paper-level scoring** — papers are scope, not a scored entity.
- **Selection-bias caveats on column accuracy** — the human decides every link
  explicitly, so there's no hidden conditioning to disclose.
- **n:m / duplicate clusters** — matching is 1:1 and leftovers are FP/FN.
- **Schema ceiling and accounting for schema violations** — an AI value that
  breaks the schema is just an error; a gold value that breaks it is the user's
  data to fix. Warn and mark the cells; don't build arithmetic on top.
- **Assuming gold provenance** (page number, source sentence) — it may or may
  not exist. If present it's a strong linkage field and the user selects it like
  any other column; nothing depends on it.
