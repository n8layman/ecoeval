# ecoeval (development version)

## Setup inputs as arguments (#16)

* `run_eval_app()` accepts every input the setup screens ask for: `paper_key`,
  `paper_map`, `mapping`, `comparator_config`, `linkage_fields`, `fields`,
  `normalizers`, `skip`, and `judge`. It runs the stages it was given inputs
  for and opens on the first it was not -- straight on the comparison when
  everything is supplied. `skip_setup = TRUE` takes the defaults for anything
  left out. `evaluate_extraction()` takes the same arguments.
* `ai`, `gold`, and the paper lists may be data frames as well as paths, so
  records can be shaped in R before the evaluation.
* Paper links: `paper_map` supplies them; otherwise `auto_accept` (the
  default) accepts the matcher's high-confidence links and says how many it
  left out.
* Per-field `normalizers`: functions applied to both sides before matching and
  comparison. Cells now carry `ai_original` and `gold_original` beside the
  compared values, and the grid, tooltips, cell modal, and `aligned_table()`
  show what each side actually said. This also fixes the built-in LLM name
  normaliser, which used to overwrite the values shown.
* `skip` turns off processing steps a run does not need: `"normalize"`,
  `"conformance"`, `"granularity"`. `judge = NULL` turns off every LLM step.
* The pipeline is now a set of stage functions -- `load_inputs()`,
  `choose_paper_keys()`, `place_papers()`, `propose_paper_links()`,
  `set_scope()`, `configure_fields()`, `score_evaluation()`, chained by
  `setup_evaluation()` -- that the app's screens and `evaluate_extraction()`
  share, so a scripted run and a clicked-through one are the same computation.
* Reloading a `run_config` now restores the input paths, paper keys, paper
  links, and field configuration, not only the manual decisions; the field
  configuration now records each field's column mapping.
* Fix: an accepted paper link between two different identifiers is now in
  scope. Previously only links whose identifiers matched exactly counted, so
  accepting a cross-identifier link the matcher proposed had no effect.

# ecoeval 0.1.0

First working version. The evaluation runs end to end: load two record sets and
a schema, align the papers, align the records, score every cell, and read the
numbers.

## The scoring engine

Plain functions over data frames, with no Shiny anywhere in them, so an
evaluation can be scripted and every part of it is unit-tested without
launching an app. `evaluate_extraction()` runs the whole pipeline headlessly.

* `read_schema()` locates the record object inside a JSON Schema and derives a
  comparator default per field from its type. `check_conformance()` reports
  values outside a field's `enum` **with their frequencies**, which is what
  lets a user tell a missing category from a typo.
* `compare_pair()` implements the cascade -- exact, then trimmed and
  case-insensitive, then fuzzy, then the LLM judge -- where each rung sees only
  what the previous could not resolve, and a field's comparator names the
  highest rung it may climb to.
* `align_records()` matches with fastLink, blocked per paper, permissive, 1:1.
* `score_cells()` and `field_metrics()` implement one unified accounting over
  every record from both sides, matched or not.
* `collect_findings()` groups findings by who acts on them -- the schema, the
  model, or the gold standard. `schema_patch()` emits an applicable patch
  rather than prose.
* `export_bundle()` writes the bundle, and `run_config.json` inside it makes a
  run reproducible: mappings, comparators, manual link decisions, and cached
  judge verdicts.

## The app

`run_eval_app()` is a six-stage wizard with a persistent progress rail, one
module per stage under `inst/app/modules/`. **What identifies a paper is
detected, not asked for**: `suggest_paper_key()` works down a fixed priority
list -- DOI, then file name, then title, then first author + year -- over every
table at once, so both sources are keyed the same way and their keys are
comparable. An identifier may span more than one column, and each column is
normalised for the role it plays, so `https://doi.org/10.1000/P01` and
`10.1000/p01` are one paper. The column picker is still there, folded away, for
when detection is wrong.

**Two heatmaps, and the difference between them is the point.**

The comparison view shows **one paper at a time**: rows are its records --
matched pairs, then AI-only, then gold-only, named from the identity columns --
columns are the scored fields, and every tile is a single cell. That is where
the four colours mean what they say: **green** the two sides agree, **purple**
both have a value and they differ, **yellow** only the gold standard has a
value, **orange** only the AI does. Seven cell states, four colours -- whether a
one-sided value came from an unpaired record or a blank in a paired one is a
detail of the alignment, not of the finding, and a column neither side filled
in is agreement about absence (colour only: a both-blank cell is still a true
negative and still drops out of accuracy). Schema violations are marked with a
dot rather than coloured, because validity is orthogonal to agreement. Clicking
a tile opens what the AI got, what the gold standard got, **the sentences each
side quoted for them**, which rung of the cascade decided it and against what
cutoff, and the three manual operations -- reject a link, link two orphans,
override a cell -- each recomputing every metric immediately.

The dashboard opens on **the overview**: every paper against every column,
shaded by the share of that paper's cells that agree. It cannot use the four
colours, because a tile there covers several records and a tile holding nine
agreements and one disagreement would paint identically to one holding ten
disagreements. The rate is the accuracy `field_metrics()` reports, so the map
and the numbers under it are the same arithmetic twice. Both axes sort
worst-first: a pale vertical band is a column that fails everywhere (usually a
comparator or schema problem), a pale horizontal one is a paper that fails
everywhere (usually a bad alignment). Clicking a tile opens that paper in the
comparison view with the column outlined.

The cells underneath are the confusion matrix, and the colours are its boxes:
green a true positive, yellow a false negative, orange a false positive, purple
both at once, a mutually blank cell the true negative that drops out. **The
matrix sits under the chart**, drawn as its four boxes with each painted the
colour of the cells it counts, and the arithmetic written out below it.
`confusion_totals()` ties out with `field_metrics()` and `aggregate_metrics()`
by construction. **Clicking a column name** narrows the matrix to that column
and moves the per-column detail panel with it. The hues are checked for
colour-vision separation rather than picked by eye -- green against orange is
the pair that needed the work, and the ramp's light end stops short of white so
a zero-agreement tile is the one you see, not the one you miss.

Press **Use the bundled example** on the load screen to try it against the
synthetic fixtures, each row of which exercises a specific case.

## Two things worth calling out

* **The matcher is now reproducible.** fastLink clusters string distances
  internally and that clustering is randomly initialised, so two identical
  calls could return different pairings. ecoeval pins the seed
  (`ecoeval_seed()`) and restores the caller's RNG state afterwards.
* **The linkage model is fitted once over the whole corpus, then applied per
  paper.** fastLink's EM learns which fields discriminate from the data it is
  given, and a per-paper block holds a handful of records -- on two records it
  will confidently pair the wrong ones.

# ecoeval 0.0.0.9000

Initial scaffold. See `DESIGN.md` for the design.
