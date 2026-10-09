# Integration test for the Shiny app.
#
# Drives the real stage modules with shiny::testServer, walking the wizard the
# way a user does: load the bundled example, map the metadata, set scope, map
# the record fields, then read the dashboard. It catches the class of bug that
# only shows up once the modules are wired to each other.

test_that("the app state starts on the load stage with nothing unlocked", {
  skip_without_app()
  rv <- shiny::isolate(new_app_state())
  reach <- shiny::isolate(stage_reachable(rv))
  expect_equal(shiny::isolate(rv$stage), "load")
  expect_true(reach$load$open)
  expect_false(reach$metadata$open)
  expect_false(reach$dashboard$open)
})

test_that("loading the bundled example fills the raw inputs", {
  skip_without_app()
  rv <- new_app_state()
  shiny::testServer(mod_load_server, args = list(id = "load", rv = rv), {
    session$setInputs(
      ai = fixture("ai_records.csv"),
      gold = fixture("gold_records.csv"),
      schema = fixture("schema.json"),
      ai_papers = fixture("ai_papers.csv"),
      gold_papers = fixture("gold_papers.csv")
    )
    session$setInputs(load = 1)
    expect_equal(nrow(rv$ai_raw), 12L)
    expect_equal(nrow(rv$gold_raw), 13L)
    expect_s3_class(rv$schema, "ecoeval_schema")
    expect_equal(nrow(rv$ai_papers_raw), 11L)
    # Loading moves the user on rather than leaving them to find the next step.
    expect_equal(rv$stage, "metadata")
  })
})

test_that("a missing record set fails loudly instead of half-loading", {
  skip_without_app()
  rv <- new_app_state()
  shiny::testServer(mod_load_server, args = list(id = "load", rv = rv), {
    session$setInputs(ai = "", gold = "", schema = fixture("schema.json"))
    session$setInputs(load = 1)
    expect_null(rv$ai_raw)
    expect_match(output$load_status, "required")
  })
})

test_that("the paper identifier is detected without being asked for", {
  skip_without_app()
  rv <- loaded_state()
  shiny::testServer(mod_map_metadata_server,
                    args = list(id = "metadata", rv = rv), {
    # No inputs set: the stage detects the identifier for all four tables.
    session$setInputs(apply = 1)
    expect_equal(rv$ai_paper_key$strategy, "doi")
    expect_true(all(c(".rid", ".paper") %in% names(rv$ai)))
    expect_false(anyNA(rv$ai$.paper))
    expect_equal(nrow(rv$ai_papers), 11L)
    # Alignment metadata comes from the same detection, not from a form.
    expect_setequal(rv$metadata_fields, c("title", "year"))
    expect_equal(rv$stage, "papers")
  })
})

test_that("the detected identifier can be overridden by hand", {
  skip_without_app()
  rv <- loaded_state()
  shiny::testServer(mod_map_metadata_server,
                    args = list(id = "metadata", rv = rv), {
    session$setInputs(ai_papers_cols = c("title", "year"))
    session$setInputs(apply = 1)
    expect_equal(rv$ai_papers_key$columns, c("title", "year"))
    expect_equal(rv$ai_paper_key$columns, "doi")
  })
})

test_that("accepting the paper links sets scope", {
  skip_without_app()
  rv <- mapped_state()
  shiny::testServer(mod_align_papers_server,
                    args = list(id = "papers", rv = rv), {
    # The matcher's verdict: P11 and P12 are different papers, and its guess
    # that they are one falls below the cutoff.
    session$setInputs(apply = 1)
    expect_equal(length(rv$scope$papers), 10L)
    expect_equal(rv$scope$ai_only, "10.1000/p11")
    expect_equal(rv$scope$gold_only, "10.1000/p12")
    expect_null(rv$blocked)
    expect_equal(rv$stage, "fields")
  })
})

test_that("an accepted link between two identifiers brings both into scope", {
  skip_without_app()
  rv <- mapped_state()
  shiny::testServer(mod_align_papers_server,
                    args = list(id = "papers", rv = rv), {
    session$setInputs(accept_all = 1)
    session$setInputs(apply = 1)
    expect_equal(length(rv$scope$papers), 11L)
    expect_true("10.1000/p11" %in% rv$scope$papers)
    expect_length(rv$scope$ai_only, 0L)
    expect_length(rv$scope$gold_only, 0L)
    hit <- rv$paper_map[rv$paper_map$ai_paper == "10.1000/p11", ]
    expect_equal(hit$gold_paper, "10.1000/p12")
  })
})

test_that("rejecting every paper link blocks rather than scoring nothing", {
  skip_without_app()
  rv <- mapped_state()
  shiny::testServer(mod_align_papers_server,
                    args = list(id = "papers", rv = rv), {
    session$setInputs(accept_none = 1)
    session$setInputs(apply = 1)
    expect_match(rv$blocked, "nothing to compare")
  })
})

test_that("applying the field mapping aligns and scores", {
  skip_without_app()
  rv <- scoped_state()
  shiny::testServer(mod_map_records_server, args = list(id = "fields", rv = rv), {
    session$setInputs(apply = 1)
    expect_true(nrow(rv$pairs) > 0L)
    expect_true(nrow(rv$cells) > 0L)
    expect_setequal(unique(rv$pairs$kind), c("pair", "ai_only", "gold_only"))
    expect_true(nrow(rv$conformance) > 0L)
    expect_true(nrow(rv$collapses) > 0L)   # the two P09 gold rows
    expect_equal(rv$stage, "dashboard")
  })
})

test_that("the field mapping refuses to run with no identity column", {
  skip_without_app()
  rv <- scoped_state()
  shiny::testServer(mod_map_records_server, args = list(id = "fields", rv = rv), {
    n <- nrow(shiny::isolate(rv$schema$fields))
    off <- stats::setNames(rep(list(FALSE), n), paste0("lnk_", seq_len(n)))
    do.call(session$setInputs, off)
    session$setInputs(apply = 1)
    expect_null(rv$pairs)
    expect_match(output$status, "identity column")
  })
})

test_that("the dashboard renders its numbers, charts and findings", {
  skip_without_app()
  rv <- scored_state()
  shiny::testServer(mod_dashboard_server,
                    args = list(id = "dashboard", rv = rv), {
    session$setInputs(column = "interaction_type")
    expect_true(!is.null(output$column_accuracy$src))
    expect_true(!is.null(output$confusion$src))
    expect_true(nchar(output$confusion_interactive) > 0L)
    # The overview heatmap: every scoped paper, hoverable at this size.
    expect_true(grepl("plotly", as.character(output$heatmap_ui$html)))
    expect_true(nchar(output$heatmap) > 0L)
    # The matrix under it is the same colours counted, and it shows the sums.
    tally <- as.character(output$confusion_matrix$html)
    ct <- ecoeval::confusion_totals(rv$cells)
    expect_true(grepl("Precision", tally))
    expect_true(grepl(sprintf("%d / %d", ct$tp, ct$tp + ct$fp), tally, fixed = TRUE))
    expect_equal(ct$accuracy, ecoeval::aggregate_metrics(rv$cells)$overall_accuracy)
    expect_true(nchar(output$export_status) > 0L)
    expect_true(grepl("Findings", as.character(output$findings$html)))
    # The grouped findings reach the screen, not just the tibble.
    expect_true(grepl("commensalism", as.character(output$findings$html)))
  })
})

test_that("the comparison heatmap renders one paper at a time, one cell a tile", {
  skip_without_app()
  rv <- scored_state()
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    d <- heat()
    n_rows <- sum(rv$pairs$paper == rv$current_paper)
    # Nothing is aggregated here: one tile per record per scored column.
    expect_equal(nrow(d), n_rows * length(scored_fields(rv)))
    expect_equal(length(unique(d$label)), n_rows)
    # Identity columns come first, the way the grid used to pin them left.
    expect_equal(levels(d$field)[seq_along(identity_fields(rv))],
                 identity_fields(rv))
    expect_true(grepl("plotly", as.character(output$heatmap_ui$html)))
  })
})

test_that("rejecting a link from the cell modal splits the row and rescores", {
  skip_without_app()
  rv <- scored_state()
  shiny::isolate(rv$current_paper <- rv$pairs$paper[rv$pairs$kind == "pair"][[1L]])
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    before <- sum(rv$pairs$kind == "pair")
    pid <- rv$pairs$pair_id[rv$pairs$paper == rv$current_paper &
                              rv$pairs$kind == "pair"][[1L]]
    select_cell(pid, identity_fields(rv)[[1L]])
    session$setInputs(reject = 1)
    expect_equal(sum(rv$pairs$kind == "pair"), before - 1L)
    expect_equal(nrow(rv$rejected), 1L)
  })
})

test_that("a cell override has the last word and is recorded", {
  skip_without_app()
  rv <- scored_state()
  disagreement <- shiny::isolate(rv$cells[rv$cells$state == "disagree", ][1, ])
  shiny::isolate(rv$current_paper <- disagreement$paper)
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    select_cell(disagreement$pair_id, disagreement$field)
    session$setInputs(say_same = 1)
    row <- rv$cells[rv$cells$pair_id == disagreement$pair_id &
                      rv$cells$field == disagreement$field, ]
    expect_equal(row$state, "agree")
    expect_true(row$overridden)
    expect_equal(nrow(rv$overrides), 1L)
  })
})

test_that("clicking a tile is what opens the cell, twice over if need be", {
  skip_without_app()
  rv <- scored_state()
  shiny::isolate(rv$current_paper <- rv$pairs$paper[rv$pairs$kind == "pair"][[1L]])
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    d <- heat()
    key <- paste(d$pair_id[[1L]], as.character(d$field)[[1L]], sep = "")
    tile <- ecoeval::parse_tile_key(key)
    select_cell(tile$row, tile$field)
    expect_equal(selected_cell()$pair_id, d$pair_id[[1L]])
    first <- selected_cell()$nonce
    # The same tile again has to reopen the modal, which a plain reactiveVal
    # would not do -- hence the nonce.
    select_cell(tile$row, tile$field)
    expect_true(selected_cell()$nonce > first)
  })
})

test_that("the dashboard diffs against a previous run", {
  skip_without_app()
  rv <- scored_state()
  # A run recorded before a schema fix, with worse numbers.
  before <- capture_run_config(
    new_run_config(),
    metrics = list(overall_accuracy = 0.40, record_f1 = 0.71))
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(before, path)

  shiny::testServer(mod_dashboard_server,
                    args = list(id = "dashboard", rv = rv), {
    session$setInputs(column = "interaction_type")
    session$setInputs(previous = data.frame(name = "run_config.json",
                                            datapath = path,
                                            stringsAsFactors = FALSE))
    html <- as.character(output$diff$html)
    expect_match(html, "Overall accuracy")
    expect_match(html, "40.0%")
  })
})

test_that("an unreadable previous run says so instead of erroring", {
  skip_without_app()
  rv <- scored_state()
  bad <- withr::local_tempfile(fileext = ".json")
  writeLines("not json at all", bad)
  shiny::testServer(mod_dashboard_server,
                    args = list(id = "dashboard", rv = rv), {
    session$setInputs(column = "interaction_type")
    session$setInputs(previous = data.frame(name = "x.json", datapath = bad,
                                            stringsAsFactors = FALSE))
    expect_match(as.character(output$diff$html), "Could not read")
  })
})

test_that("clicking a cell shows both values and both sets of quotes", {
  skip_without_app()
  rv <- scored_state()
  cells <- shiny::isolate(rv$cells)
  ev <- ecoeval::evidence_field(unique(cells$field))
  expect_equal(ev, "all_supporting_source_sentences")

  # A cell where the two sides disagree, so there is something to settle.
  d <- cells[cells$field == "location_country" & cells$state == "disagree", ][1, ]
  html <- as.character(cell_modal_body(cells, d$pair_id, "location_country",
                                       evidence = ev))

  expect_true(grepl("Gold standard", html))
  expect_true(grepl("Supporting sentences", html))
  # The quoted text itself, from both sides, not just the labels.
  quote <- cells[cells$pair_id == d$pair_id & cells$field == ev, ]
  expect_true(grepl(substr(quote$ai_value[[1L]], 1, 25), html, fixed = TRUE))
  expect_true(grepl(substr(quote$gold_value[[1L]], 1, 25), html, fixed = TRUE))
  # The column being clicked is not repeated as its own evidence block.
  own <- as.character(cell_modal_body(cells, d$pair_id, ev, evidence = ev))
  expect_false(grepl("Supporting sentences", own))
})

test_that("a cell that is no longer there says so rather than erroring", {
  skip_without_app()
  rv <- scored_state()
  cells <- shiny::isolate(rv$cells)
  html <- as.character(cell_modal_body(cells, "no-such-pair", "location_country"))
  expect_true(grepl("no longer", html))
})

test_that("a clicked tile key round-trips through either plot", {
  skip_without_app()
  rv <- scored_state()
  cells <- shiny::isolate(rv$cells)
  g <- ecoeval::paper_field_outcomes(cells, shiny::isolate(rv$scope$papers))
  p <- ecoeval::plot_paper_heatmap(g)
  tile <- ecoeval::parse_tile_key(p$data$key[[1L]])
  expect_equal(tile$row, as.character(p$data$paper[[1L]]))
  expect_equal(tile$field, as.character(p$data$field[[1L]]))

  # The same key format identifies a record in the per-paper heatmap, which is
  # what lets one parser serve both.
  r <- ecoeval::record_field_outcomes(cells, "10.1000/p08")
  pr <- ecoeval::plot_record_heatmap(r)
  rtile <- ecoeval::parse_tile_key(pr$data$key[[1L]])
  expect_equal(rtile$row, pr$data$pair_id[[1L]])
  expect_equal(rtile$field, as.character(pr$data$field[[1L]]))
})

test_that("clicking a column name scopes the confusion matrix to that column", {
  skip_without_app()
  rv <- scored_state()
  shiny::testServer(mod_dashboard_server,
                    args = list(id = "dashboard", rv = rv), {
    session$setInputs(column = "interaction_type")
    expect_match(as.character(output$confusion_matrix$html), "every column")

    # The plot abbreviates long column names, so the click arrives shortened.
    field <- "all_supporting_source_sentences"
    session$setInputs(column_click = paste0(substr(field, 1, 21), "…"))
    html <- as.character(output$confusion_matrix$html)
    expect_match(html, field)
    # The counts are that column's, not the whole run's.
    ct <- ecoeval::confusion_totals(rv$cells, field)
    expect_true(grepl(sprintf("%d / %d", ct$tp, ct$tp + ct$fp), html, fixed = TRUE))
    expect_true(ct$tp < ecoeval::confusion_totals(rv$cells)$tp)

    session$setInputs(matrix_all = 1)
    expect_match(as.character(output$confusion_matrix$html), "every column")
  })
})

test_that("an abbreviated axis label finds its column again", {
  skip_without_app()
  fields <- c("year_observed", "all_supporting_source_sentences")
  expect_equal(field_from_label("all_supporting_source…", fields),
               "all_supporting_source_sentences")
  expect_equal(field_from_label("year_observed", fields), "year_observed")
  expect_null(field_from_label("nothing_like_it", fields))
  expect_null(field_from_label("", fields))
})

test_that("the matrix shows the five boxes, coloured like the tiles", {
  skip_without_app()
  n <- c(agree = 57L, disagree = 21L, only_gold = 26L, only_ai = 14L, blank = 2L)
  html <- as.character(confusion_matrix_ui(n))
  for (colour in c("eco-green", "eco-purple", "eco-yellow", "eco-orange")) {
    expect_true(grepl(colour, html, fixed = TRUE))
  }
  expect_true(grepl("true negative", html))
  expect_true(grepl(">57<", html))
})

test_that("the interactive heatmap registers the click event it listens for", {
  skip_without_app()
  rv <- scored_state()
  g <- ecoeval::paper_field_outcomes(shiny::isolate(rv$cells),
                                     shiny::isolate(rv$scope$papers))
  w <- interactive_heatmap(ecoeval::plot_paper_heatmap(g), "eco_heatmap",
                           "dashboard-column_click")
  # Without this registration plotly warns and event_data() stays empty, so
  # clicking a tile would do nothing at all.
  expect_true("plotly_click" %in% unlist(w$x$shinyEvents))
  expect_equal(w$x$source, "eco_heatmap")
  # And the column names carry their own handler.
  js <- paste(vapply(w$jsHooks$render, function(h) h$code, character(1)),
              collapse = " ")
  expect_true(grepl("dashboard-column_click", js, fixed = TRUE))
  expect_true(grepl("xtick", js, fixed = TRUE))
})

test_that("the cell modal says what happened, not just what decided it", {
  skip_without_app()
  rv <- scored_state()
  cells <- shiny::isolate(rv$cells)
  cfg <- shiny::isolate(rv$comparators)
  ev <- "all_supporting_source_sentences"
  # The p08 pair whose sentences the fuzzy rung could not settle.
  pid <- cells$pair_id[cells$paper == "10.1000/p08" & cells$field == ev &
                         cells$state == "disagree"][[1L]]
  html <- as.character(cell_modal_body(cells, pid, ev, evidence = ev,
                                       config = cfg))

  # The verdict in words and what it costs, not a colour to decode.
  expect_true(grepl("They differ", html))
  expect_true(grepl("false positive and false negative", html))
  # A similarity score means nothing without the cutoff it was measured against.
  expect_true(grepl("0.660", html))
  expect_true(grepl("0.85 cutoff", html))
  # And an unsettled cell says it is provisional rather than looking decided.
  expect_true(grepl("judge has not", html))
})

test_that("the modal names a schema violation rather than only marking it", {
  skip_without_app()
  rv <- scored_state()
  cells <- shiny::isolate(rv$cells)
  conf <- shiny::isolate(rv$conformance)
  skip_if(is.null(conf) || !nrow(conf), "no violations in the fixtures")
  bad <- conf[1, ]
  hit <- cells[cells$field == bad$field &
                 ecoeval::canonicalise(
                   if (bad$source == "ai") cells$ai_value else cells$gold_value
                 ) == ecoeval::canonicalise(bad$value), ][1, ]
  keys <- paste(conf$source, conf$field, ecoeval::canonicalise(conf$value))
  html <- as.character(cell_modal_body(cells, hit$pair_id, hit$field,
                                       violations = keys))
  expect_true(grepl("fails schema validation", html))
})

# ---- launching with setup inputs ---------------------------------------------

test_that("a launch with everything supplied opens on the comparison", {
  skip_without_app()
  rv <- shiny::isolate(new_app_state())
  run <- setup_evaluation(
    ai = fixture("ai_records.csv"), gold = fixture("gold_records.csv"),
    schema = fixture("schema.json"), paper_key = "doi",
    linkage_fields = "interaction_type", skip_setup = FALSE
  )
  shiny::isolate({
    seed_state(rv, run)
    expect_equal(rv$stage, "compare")
    expect_true(stage_reachable(rv)$compare$open)
    expect_equal(identity_fields(rv), "interaction_type")
    expect_equal(rv$config$inputs$ai_paper_key, "doi")
    expect_false(is.null(rv$ai_shown))
  })
})

test_that("a launch short of a stage opens on that stage, pre-filled", {
  skip_without_app()
  rv <- shiny::isolate(new_app_state())
  run <- setup_evaluation(
    ai = fixture("ai_records.csv"), gold = fixture("gold_records.csv"),
    schema = fixture("schema.json"), paper_key = "doi", skip_setup = FALSE,
    normalizers = list(location_country = toupper)
  )
  shiny::isolate({
    rv$normalizers <- list(location_country = toupper)
    seed_state(rv, run)
    expect_equal(rv$stage, "fields")
    expect_false(stage_reachable(rv)$fields$done)
  })
  shiny::testServer(mod_map_records_server, args = list(id = "fields", rv = rv), {
    session$setInputs(apply = 1)
    expect_equal(rv$stage, "dashboard")
    lc <- rv$cells[rv$cells$field == "location_country" & !is.na(rv$cells$ai_rid), ]
    expect_true(all(lc$ai_value == toupper(lc$ai_original), na.rm = TRUE))
    # Resetting drops the launch configuration for the schema's defaults.
    rv$field_seed$linkage <- FALSE
    session$setInputs(reset = 1)
    expect_true(any(isolate(rv$field_seed %||% list(linkage = TRUE))$linkage))
  })
})

test_that("launching runs the setup stages from the arguments", {
  skip_without_app()
  rv <- shiny::isolate(new_app_state())
  shiny::isolate({
    rv$args <- list(ai = fixture("ai_records.csv"), gold = fixture("gold_records.csv"),
                    schema = fixture("schema.json"), paper_key = "doi",
                    linkage_fields = "interaction_type")
  })
  run <- launch_state(rv)
  expect_equal(run$stage, "scored")
  expect_equal(shiny::isolate(rv$stage), "compare")

  # Nothing supplied: nothing runs, and the app opens on the load screen.
  rv <- shiny::isolate(new_app_state())
  expect_null(launch_state(rv))
  expect_equal(shiny::isolate(rv$stage), "load")

  # A stage that fails opens on its screen with the reason.
  rv <- shiny::isolate(new_app_state())
  shiny::isolate(rv$args <- list(ai = "missing.csv", gold = "missing.csv",
                                 schema = fixture("schema.json")))
  run <- launch_state(rv)
  expect_equal(shiny::isolate(rv$stage), "load")
  expect_match(run$message, "not found")
})

test_that("arguments win over a restored run, which fills in the rest", {
  skip_without_app()
  cfg <- new_run_config()
  cfg$inputs$ai <- "saved_ai.csv"
  cfg$inputs$gold <- "saved_gold.csv"
  cfg$inputs$ai_paper_key <- list("doi")
  restored <- list(
    comparators = default_comparator_config(read_schema(fixture("schema.json"))),
    paper_map = tibble::tibble(ai_paper = "a", gold_paper = "a")
  )
  launch <- launch_inputs(list(ai = "given.csv", skip_setup = TRUE), cfg, restored)
  expect_equal(launch$ai, "given.csv")
  expect_equal(launch$gold, "saved_gold.csv")
  expect_equal(launch$paper_key, list(ai = "doi"))
  expect_equal(launch$paper_map$ai_paper, "a")
  expect_equal(nrow(launch$comparator_config), 8L)
  expect_true(launch$skip_setup)
})

test_that("the judge can be supplied or switched off", {
  skip_without_app()
  rv <- scored_state()
  shiny::isolate({
    rv$judge_mode <- "off"
    expect_null(current_judge(rv))
    expect_false(llm_allowed(rv))
    j <- function(...) list(agree = TRUE, rationale = "ok")
    rv$judge_mode <- "supplied"
    rv$judge <- j
    expect_identical(current_judge(rv), j)
  })
})

test_that("a data frame handed to run_eval_app() loads in place of a path", {
  skip_without_app()
  rv <- new_app_state()
  shiny::isolate(rv$args <- list(ai = utils::read.csv(fixture("ai_records.csv"))))
  shiny::testServer(mod_load_server, args = list(id = "load", rv = rv), {
    session$setInputs(ai = FRAME_PLACEHOLDER, gold = fixture("gold_records.csv"),
                      schema = fixture("schema.json"))
    session$setInputs(load = 1)
    expect_equal(nrow(rv$ai_raw), 12L)
    expect_null(rv$config$inputs$ai)
  })
})
