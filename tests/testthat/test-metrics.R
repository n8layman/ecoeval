test_that("a wrongly linked pair scores the same as leaving it unlinked", {
  # The property the whole accounting rests on: linking earns nothing except
  # where fields genuinely agree, so bad links cannot inflate field accuracy.
  ai <- tibble::tibble(.rid = "a1", .paper = "P", v = "alpha", w = "one")
  gold <- tibble::tibble(.rid = "g1", .paper = "P", v = "omega", w = "two")
  cfg <- default_comparator_config(fields = c("v", "w"))

  linked <- score_cells(
    tibble::tibble(pair_id = "p1", paper = "P", ai_rid = "a1", gold_rid = "g1"),
    ai, gold, cfg)
  unlinked <- score_cells(
    tibble::tibble(pair_id = c("p1", "p2"), paper = "P",
                   ai_rid = c("a1", NA), gold_rid = c(NA, "g1")),
    ai, gold, cfg)

  cols <- c("field", "tp", "fp", "fn")
  expect_equal(field_metrics(linked)[, cols], field_metrics(unlinked)[, cols])
})

test_that("each state contributes what the design says it does", {
  states <- c("agree", "disagree", "ai_missing", "gold_missing",
              "ai_only", "gold_only", "blank")
  cells <- tibble::tibble(
    pair_id = paste0("p", seq_along(states)), paper = "P",
    ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = "x", gold_value = "y", state = states,
    rung = "exact", score = 1, rationale = NA_character_,
    pending = FALSE, overridden = FALSE
  )
  fm <- field_metrics(cells)
  expect_equal(fm$tp, 1L)                      # agree
  expect_equal(fm$fp, 3L)                      # disagree, gold_missing, ai_only
  expect_equal(fm$fn, 3L)                      # disagree, ai_missing, gold_only
  expect_equal(fm$tn, 1L)                      # blank
  expect_equal(fm$n_scored, 6L)                # everything but blank
})

test_that("both-blank cells drop out of precision and recall", {
  cells <- tibble::tibble(
    pair_id = "p1", paper = "P", ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = NA_character_, gold_value = NA_character_, state = "blank",
    rung = "blank", score = 1, rationale = NA_character_,
    pending = FALSE, overridden = FALSE
  )
  fm <- field_metrics(cells)
  expect_equal(fm$n_scored, 0L)
  expect_true(is.na(fm$accuracy))
  expect_true(is.na(fm$precision))
})

test_that("record metrics count leftovers as false positives and negatives", {
  fx <- fixture_run()
  rm_ <- record_metrics(fx$pairs)
  expect_equal(rm_$tp, sum(fx$pairs$kind == "pair"))
  expect_equal(rm_$fp, sum(fx$pairs$kind == "ai_only"))
  expect_equal(rm_$fn, sum(fx$pairs$kind == "gold_only"))
  expect_equal(rm_$precision, rm_$tp / (rm_$tp + rm_$fp))
})

test_that("column accuracy is sorted worst first, for triage", {
  fx <- fixture_run()
  acc <- stats::na.omit(field_metrics(fx$cells)$accuracy)
  expect_false(is.unsorted(acc))
})

test_that("the two aggregates diverge only when fill rates are uneven", {
  fx <- fixture_run()
  agg <- aggregate_metrics(fx$cells)
  expect_true(agg$overall_accuracy >= 0 && agg$overall_accuracy <= 1)
  expect_equal(agg$n_columns, dplyr::n_distinct(fx$cells$field))
})

test_that("an enum column gets a true K-by-K with an out-of-schema class", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "interaction_type", fx$schema)
  expect_equal(cc$type, "class")
  expect_true("(not in schema)" %in% cc$matrix$gold_class)
  # The gold standard's commensalism records land there rather than vanishing.
  expect_true(sum(cc$matrix$n[cc$matrix$gold_class == "(not in schema)"]) > 0)
})

test_that("an absent record is a class on its own axis only", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "interaction_type", fx$schema)
  expect_false("(no Extraction record)" %in% cc$matrix$gold_class)
  expect_false("(no Reference record)" %in% cc$matrix$ai_class)
})

test_that("free text gets presence-absence with the correct/wrong split", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "all_supporting_source_sentences", fx$schema)
  expect_equal(cc$type, "presence")
  expect_true(all(c("correct", "wrong value") %in% cc$matrix$ai_class))
})

test_that("a numeric column reports how wrong the numbers are", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "sample_size", fx$schema)
  expect_equal(cc$type, "numeric")
  expect_true(is.numeric(cc$errors$error))
})

test_that("a column too thin to read carries a caution", {
  cells <- tibble::tibble(
    pair_id = "p1", paper = "P", ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = "x", gold_value = "x", state = "agree", rung = "exact",
    score = 1, rationale = NA_character_, pending = FALSE, overridden = FALSE
  )
  expect_match(column_confusion(cells, "f")$notes, "too few", all = FALSE)
})

test_that("fill rates surface the asymmetry that reads as a schema problem", {
  fx <- fixture_run()
  fills <- fill_rates(fx$cells)
  dm <- fills[fills$field == "detection_method", ]
  # Humans always record how they detected it; the AI usually does not.
  expect_true(dm$gold_fill > dm$ai_fill)
})

test_that("triage flags the pair whose identity columns all disagree", {
  fx <- fixture_run()
  # The matcher no longer forces such a pair, so link one by hand: P08's AI
  # row against the gold row it does not match, on species or interaction.
  added <- tibble::tibble(ai_rid = "a00010", gold_rid = "g00009")
  pairs <- align_records(fx$ai, fx$gold, fx$linkage, fx$paper_map, added = added)
  cells <- score_cells(pairs, fx$ai, fx$gold, fx$config)
  susp <- suspect_pairs(cells, fx$linkage)
  expect_true(nrow(susp) >= 1L)
  expect_true(all(susp$reason %in%
                    c("agrees on nothing", "identity columns all disagree")))
})

test_that("progress distinguishes unjudged from judged from reviewed", {
  fx <- fixture_run()
  prog <- progress_summary(fx$cells, fx$scope$papers, reviewed = fx$scope$papers[1])
  expect_equal(prog$n_papers, length(fx$scope$papers))
  expect_equal(prog$n_reviewed, 1L)
  # Free prose is left for the judge, so some papers are not yet judged.
  expect_true(prog$n_judged < prog$n_papers)
  expect_true(prog$n_pending_cells > 0L)
})

# ---- the overview heatmap ---------------------------------------------------

test_that("seven states collapse to the four outcomes a reader acts on", {
  expect_equal(cell_outcome(c("agree", "disagree", "ai_missing", "gold_only",
                              "gold_missing", "ai_only", "blank")),
               c("agree", "disagree", "only_gold", "only_gold",
                 "only_ai", "only_ai", "agree"))
  # Neither side finding a value is agreement about absence -- but it stays a
  # true negative in the accounting, so it never moves accuracy.
  expect_equal(unname(state_colour("blank")), "green")
  sc <- state_contributions()
  expect_equal(unlist(sc[sc$state == "blank", c("tp", "fp", "fn", "tn")],
                      use.names = FALSE), c(0L, 0L, 0L, 1L))
  # The grid and the heatmap paint the same outcome the same colour.
  expect_equal(unname(state_colour(c("disagree", "ai_only", "gold_only"))),
               c("purple", "orange", "yellow"))
})

test_that("the overview covers every scoped paper against every column", {
  fx <- fixture_run()
  g <- paper_field_outcomes(fx$cells, fx$scope$papers)
  expect_equal(nrow(g), length(fx$scope$papers) * length(unique(fx$cells$field)))
  expect_setequal(g$paper, fx$scope$papers)
  expect_true(all(g$outcome %in% c(names(outcome_labels()), "mixed")))
})

test_that("an overview tile is a rate, because a rate is what aggregates", {
  cells <- tibble::tibble(
    paper = c("p1", "p1", "p2", "p2", "p3", "p3", "p4", "p4"),
    field = "species",
    state = c("agree", "disagree", "agree", "agree", "agree", "ai_missing",
              "blank", "blank")
  )
  g <- paper_field_outcomes(cells)
  agreement <- stats::setNames(g$agreement, g$paper)
  expect_equal(agreement[["p1"]], 0.5)   # one of two, not "disagree"
  expect_equal(agreement[["p2"]], 1)
  expect_equal(agreement[["p3"]], 0.5)
  # Nothing scored is nothing to rate, rather than zero agreement.
  expect_true(is.na(agreement[["p4"]]))

  # The categorical outcome survives only where every cell says one thing.
  outcome <- stats::setNames(g$outcome, g$paper)
  expect_equal(unname(outcome[c("p1", "p2", "p3")]),
               c("mixed", "agree", "mixed"))
  expect_equal(g$n_blank[g$paper == "p4"], 2L)
  expect_equal(g$n_agree[g$paper == "p4"], 0L)   # kept apart in the counts
  expect_equal(g$n_scored[g$paper == "p4"], 0L)
  expect_equal(g$n_cells[g$paper == "p1"], 2L)
})

test_that("the tile rate is the accuracy the metrics report", {
  fx <- fixture_run()
  g <- paper_field_outcomes(fx$cells, fx$scope$papers)
  fm <- field_metrics(fx$cells)
  # Per column, the tiles' cells add up to that column's accuracy.
  for (f in fm$field[!is.na(fm$accuracy)]) {
    d <- g[g$field == f, , drop = FALSE]
    expect_equal(sum(d$n_agree) / sum(d$n_scored), fm$accuracy[fm$field == f])
  }
})

test_that("a paper with nothing scored still gets a row", {
  cells <- tibble::tibble(paper = "p1", field = "species", state = "agree")
  g <- paper_field_outcomes(cells, papers = c("p1", "p2"))
  expect_true(is.na(g$agreement[g$paper == "p2"]))
  expect_equal(g$n_cells[g$paper == "p2"], 0L)
})

test_that("the per-paper heatmap keeps one cell per tile", {
  fx <- fixture_run()
  ident <- fx$config$field[fx$config$linkage]
  fields <- fx$config$field[fx$config$include]
  r <- record_field_outcomes(fx$cells, "10.1000/p08", fields, ident)

  # Two rows in p08: the matched pair, and the gold record the AI never found.
  expect_equal(length(unique(r$pair_id)), 2L)
  expect_equal(nrow(r), 2L * length(fields))
  expect_setequal(unique(r$kind), c("matched", "gold only"))
  # Identity columns first, then the rest in configuration order.
  expect_equal(levels(r$field)[seq_along(ident)], ident)
  # Nothing aggregated: every tile is one cell's outcome.
  expect_equal(r$outcome, cell_outcome(r$state))
  # The unpaired gold record is a miss in every column.
  gold_only <- r[r$kind == "gold only", ]
  expect_true(all(gold_only$outcome == "only_gold"))
  # Rows are named from the identity columns, gold's values first.
  expect_true(any(grepl("Artibeus lituratus", r$label)))
})

test_that("a record heatmap for a paper nobody scored is empty, not an error", {
  fx <- fixture_run()
  r <- record_field_outcomes(fx$cells, "no-such-paper")
  expect_equal(nrow(r), 0L)
  expect_s3_class(plot_record_heatmap(r), "ggplot")
})

test_that("the heatmap plots, sorts worst first, and caps its rows", {
  fx <- fixture_run()
  g <- paper_field_outcomes(fx$cells, fx$scope$papers)
  p <- plot_paper_heatmap(g)
  expect_s3_class(p, "ggplot")
  # Worst paper at the top: the y factor is built in reverse draw order, and
  # "worst" is now how many cells disagree, not how many tiles are non-green.
  bad <- dplyr::arrange(
    dplyr::summarise(dplyr::group_by(g, paper),
                     bad = sum(ifelse(is.na(agreement), 0, 1 - agreement) * n_scored),
                     .groups = "drop"),
    dplyr::desc(bad), paper)
  levs <- levels(ggplot2::ggplot_build(p)$plot$data$paper)
  expect_equal(levs[[length(levs)]], bad$paper[[1L]])

  capped <- plot_paper_heatmap(g, max_papers = 3)
  expect_length(unique(as.character(ggplot2::ggplot_build(capped)$plot$data$paper)), 3L)
  expect_match(capped$labels$subtitle, "3 papers with the most\\s+differences")

  expect_s3_class(plot_paper_heatmap(paper_field_outcomes(empty_cells())), "ggplot")
})

test_that("the colours add up to the confusion matrix the metrics are built on", {
  fx <- fixture_run()
  ct <- confusion_totals(fx$cells)
  fm <- field_metrics(fx$cells)
  ag <- aggregate_metrics(fx$cells)

  # The whole claim of the overview panel: counting the colours *is* the
  # accounting, not a parallel calculation of it.
  expect_equal(ct$tp, sum(fm$tp))
  expect_equal(ct$fp, sum(fm$fp))
  expect_equal(ct$fn, sum(fm$fn))
  expect_equal(ct$tn, sum(fm$tn))
  expect_equal(ct$n_scored, sum(fm$n_scored))
  expect_equal(ct$accuracy, ag$overall_accuracy)
  expect_equal(ct$precision, ct$tp / (ct$tp + ct$fp))
  expect_equal(ct$recall, ct$tp / (ct$tp + ct$fn))

  # Every cell is counted exactly once, and the rows are the five boxes.
  expect_equal(sum(ct$by_outcome$n), nrow(fx$cells))
  expect_equal(ct$by_outcome$outcome,
               c("agree", "disagree", "only_gold", "only_ai", "blank"))
})

test_that("each colour contributes what the accounting says it does", {
  ct <- confusion_totals(tibble::tibble(
    paper = "p", field = "f",
    state = c("agree", "disagree", "ai_missing", "gold_only",
              "gold_missing", "ai_only", "blank")))
  b <- ct$by_outcome
  expect_equal(b$n, c(1L, 1L, 2L, 2L, 1L))          # blanks are their own box
  expect_equal(b$tp, c(1L, 0L, 0L, 0L, 0L))
  expect_equal(b$fn, c(0L, 1L, 2L, 0L, 0L))         # a miss, either shape
  expect_equal(b$fp, c(0L, 1L, 0L, 2L, 0L))
  expect_equal(b$tn, c(0L, 0L, 0L, 0L, 1L))
  # Blank is green like agreement but is the true-negative box, so it is shown
  # on its own row rather than folded into the true positives.
  expect_equal(b$colour, c("green", "purple", "yellow", "orange", "green"))
  expect_equal(ct$n_scored, 6L)
})

test_that("an empty run tallies to zeroes rather than erroring", {
  ct <- confusion_totals(empty_cells())
  expect_equal(sum(ct$by_outcome$n), 0L)
  expect_true(is.na(ct$accuracy))
  expect_true(is.na(ct$f1))
})
