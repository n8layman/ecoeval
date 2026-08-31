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
module per stage under `inst/app/modules/`. The comparison grid shows one paper
at a time with the identity columns pinned left, colour by agreement and an
outline for schema violations, a cell modal naming which rung decided each
verdict, and the three manual operations -- reject a link, link two orphans,
override a cell -- each recomputing every metric immediately.

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
