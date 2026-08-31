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

test_that("mapping the metadata puts both sources into canonical form", {
  skip_without_app()
  rv <- loaded_state()
  shiny::testServer(mod_map_metadata_server,
                    args = list(id = "metadata", rv = rv), {
    session$setInputs(ai_paper_col = "doi", gold_paper_col = "doi",
                      ai_papers_col = "doi", gold_papers_col = "doi",
                      ai_meta = c("title", "year"),
                      gold_meta = c("title", "year"))
    session$setInputs(apply = 1)
    expect_true(all(c(".rid", ".paper") %in% names(rv$ai)))
    expect_equal(nrow(rv$ai_papers), 11L)
    expect_equal(rv$stage, "papers")
  })
})

test_that("accepting the paper links sets scope", {
  skip_without_app()
  rv <- mapped_state()
  shiny::testServer(mod_align_papers_server,
                    args = list(id = "papers", rv = rv), {
    session$setInputs(accept_all = 1)
    session$setInputs(apply = 1)
    expect_equal(length(rv$scope$papers), 10L)
    expect_equal(rv$scope$ai_only, "10.1000/p11")
    expect_equal(rv$scope$gold_only, "10.1000/p12")
    expect_null(rv$blocked)
    expect_equal(rv$stage, "fields")
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
    expect_true(nchar(output$export_status) > 0L)
    expect_true(grepl("Findings", as.character(output$findings$html)))
    # The grouped findings reach the screen, not just the tibble.
    expect_true(grepl("commensalism", as.character(output$findings$html)))
  })
})

test_that("the comparison grid renders one paper at a time", {
  skip_without_app()
  rv <- scored_state()
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    session$setInputs(expand = TRUE)
    g <- grid_data()
    expect_equal(nrow(g$table), sum(rv$pairs$paper == rv$current_paper))
    # Identity columns are pinned left, right after the row and kind columns.
    expect_equal(names(g$table)[3:4], identity_fields(rv))
    expect_true(any(grepl("eco-cell eco-", unlist(g$table))))
  })
})

test_that("rejecting a link splits the row and rescores immediately", {
  skip_without_app()
  rv <- scored_state()
  shiny::isolate(rv$current_paper <- rv$pairs$paper[rv$pairs$kind == "pair"][[1L]])
  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    session$setInputs(expand = TRUE)
    before <- sum(rv$pairs$kind == "pair")
    # Cell selection is zero-based, and column 2 is the first identity column.
    session$setInputs(grid_cells_selected = matrix(c(0L, 2L), nrow = 1))
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
    session$setInputs(expand = TRUE)
    g <- grid_data()
    i <- match(disagreement$pair_id, g$pair_ids)
    j <- match(disagreement$field, g$fields)
    skip_if(is.na(i) || is.na(j), "the chosen cell is not on this page")
    session$setInputs(grid_cells_selected = matrix(c(i - 1L, j - 1L), nrow = 1))
    session$setInputs(say_same = 1)
    row <- rv$cells[rv$cells$pair_id == disagreement$pair_id &
                      rv$cells$field == disagreement$field, ]
    expect_equal(row$state, "agree")
    expect_true(row$overridden)
    expect_equal(nrow(rv$overrides), 1L)
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
