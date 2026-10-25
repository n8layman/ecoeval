# Rolling the extraction up to the reference's grain (#29, #39).

# A paper the extraction records in more detail than the reference: twelve
# rows of one species split by size class, where the reference has one row
# with the total, plus a second species recorded the same way on both sides.
lemis_case <- function() {
  ai <- tibble::tibble(
    .rid = sprintf("a%05d", 1:13),
    .paper = "P1",
    species = c(rep("Pterois volitans", 12), "Amphiprion ocellaris"),
    port = c(rep("Miami", 12), "Miami"),
    size_class = c(paste("size", 1:12), NA),
    quantity = c(rep(10, 11), 30, 5),
    date = c(rep("2020-01-05", 11), "2020-01-06", "2020-01-05")
  )
  gold <- tibble::tibble(
    .rid = c("g00001", "g00002"),
    .paper = "P1",
    species = c("Pterois volitans", "Amphiprion ocellaris"),
    port = "Miami",
    quantity = c(140, 5),
    date = "2020-01-05"
  )
  list(ai = ai, gold = gold,
       paper_map = tibble::tibble(ai_paper = "P1", gold_paper = "P1"))
}

lemis_config <- function() {
  cfg <- default_comparator_config(fields = c("species", "port", "quantity", "date"))
  cfg$linkage <- cfg$field %in% c("species", "port")
  cfg$comparator[cfg$field == "quantity"] <- "numeric"
  cfg
}

test_that("rolling up groups each paper's records and adds up the quantities", {
  x <- lemis_case()
  rolled <- rollup_extraction(x$ai, by = c("species", "port"), sum = "quantity")
  expect_equal(nrow(rolled), 2L)
  expect_equal(rolled$.n_records, c(12L, 1L))
  expect_equal(rolled$quantity, c(140, 5))
  # Each rolled-up row knows the extraction records it holds.
  expect_equal(attr(rolled, "member_rids")[[1L]], x$ai$.rid[1:12])
  # A field whose members differ keeps every distinct value.
  expect_setequal(rolled$date[[1L]], c("2020-01-05", "2020-01-06"))
  expect_equal(rolled$date[[2L]], "2020-01-05")
})

test_that("a blank quantity is skipped, and a group of blanks has no total", {
  x <- lemis_case()
  x$ai$quantity[1:2] <- NA
  x$ai$quantity[13] <- NA
  rolled <- rollup_extraction(x$ai, by = "species", sum = "quantity")
  expect_equal(rolled$quantity, c(120, NA))
})

test_that("rolling up needs fields that exist", {
  x <- lemis_case()
  expect_error(rollup_extraction(x$ai, by = "nope"), class = "ecoeval_error")
  expect_error(rollup_extraction(x$ai, by = character(0)), class = "ecoeval_error")
})

test_that("the values as read roll up with the groups the compared values formed", {
  x <- lemis_case()
  shown <- x$ai
  # A normaliser made two spellings the same; the values as read still differ.
  shown$species[2] <- "P. volitans"
  rolled <- rollup_extraction(x$ai, by = "species", sum = "quantity")
  again <- rollup_extraction(shown, by = "species", sum = "quantity",
                             members = attr(rolled, "members"))
  expect_equal(nrow(again), nrow(rolled))
  expect_equal(again$quantity, rolled$quantity)
})

test_that("a mixed value agrees when the reference value is among the members", {
  x <- lemis_case()
  rolled <- rollup_extraction(x$ai, by = c("species", "port"), sum = "quantity")
  pairs <- tibble::tibble(pair_id = "p1", paper = "P1",
                          ai_rid = rolled$.rid[[1L]], gold_rid = "g00001")
  cells <- score_cells(pairs, rolled, x$gold, lemis_config())
  date <- cells[cells$field == "date", ]
  expect_true(date$mixed)
  expect_equal(date$state, "agree")
  expect_false(any(cells$mixed[cells$field != "date"]))

  x$gold$date[[1L]] <- "2021-01-01"
  date <- score_cells(pairs, rolled, x$gold, lemis_config())
  expect_equal(date$state[date$field == "date"], "disagree")
})

test_that("rolled up, the twelve rows pair with the reference and the total agrees", {
  x <- lemis_case()
  cfg <- lemis_config()
  plain <- align_records(x$ai, x$gold, c("species", "port"), x$paper_map)
  expect_equal(sum(plain$kind == "ai_only"), 11L)

  rolled <- rollup_extraction(x$ai, by = c("species", "port"), sum = "quantity")
  pairs <- align_records(rolled, x$gold, c("species", "port"), x$paper_map)
  expect_equal(sum(pairs$kind == "pair"), 2L)
  expect_equal(sum(pairs$kind == "ai_only"), 0L)
  cells <- score_cells(pairs, rolled, x$gold, cfg)
  expect_true(all(cells$state[cells$field == "quantity"] == "agree"))
})

test_that("the suggestion finds the roll-up and the quantity to add up", {
  x <- lemis_case()
  s <- suggest_rollup(x$ai, x$gold, c("species", "port"), "quantity", x$paper_map)
  expect_setequal(s$by, c("species", "port"))
  expect_equal(s$sum, "quantity")
  expect_equal(s$baseline, 1L)
  expect_equal(s$candidates$matched[[1L]], 2L)
  expect_type(s$summary, "character")
})

test_that("no suggestion when the two sides already match row for row", {
  x <- lemis_case()
  one_each <- x$ai[12:13, ]
  s <- suggest_rollup(one_each, x$gold, c("species", "port"), "quantity", x$paper_map)
  expect_null(s$by)
  expect_null(s$summary)
})

test_that("a suggestion becomes a finding when the extraction is not rolled up", {
  x <- lemis_case()
  s <- suggest_rollup(x$ai, x$gold, c("species", "port"), "quantity", x$paper_map)
  pairs <- align_records(x$ai, x$gold, c("species", "port"), x$paper_map)
  cells <- score_cells(pairs, x$ai, x$gold, lemis_config())
  without <- collect_findings(cells, pairs)
  with <- collect_findings(cells, pairs, rollup_suggestion = s)
  expect_equal(nrow(with), nrow(without) + 1L)
  expect_equal(with$n[!with$detail %in% without$detail], 1L)
})

test_that("the pipeline suggests the roll-up, and rolled up the total agrees", {
  f <- lemis_files()
  plain <- setup_evaluation(f$ai, f$gold, f$schema, paper_key = "doi")
  expect_equal(plain$stage, "scored")
  expect_equal(sum(plain$scored$pairs$kind == "ai_only"), 11L)
  s <- plain$scored$rollup_suggestion
  expect_setequal(s$by, c("species", "port"))
  expect_equal(s$sum, "quantity")
  expect_null(plain$scored$rollup)

  rolled <- setup_evaluation(f$ai, f$gold, f$schema, paper_key = "doi",
                             rollup_by = s$by, rollup_sum = s$sum)
  sc <- rolled$scored
  expect_equal(sum(sc$pairs$kind == "pair"), 2L)
  expect_equal(sum(sc$pairs$kind == "ai_only"), 0L)
  expect_true(all(sc$cells$state[sc$cells$field == "quantity"] == "agree"))
  # The values as read are rolled up too, and the members are kept.
  expect_equal(nrow(sc$ai_shown), 2L)
  expect_equal(lengths(sc$rollup$members$members), c(12L, 1L))
  expect_equal(nrow(sc$rollup$records), 13L)
})

test_that("a run configuration keeps the roll-up", {
  path <- withr::local_tempfile(fileext = ".json")
  cfg <- capture_run_config(new_run_config(),
                            rollup = list(by = c("species", "port"), sum = "quantity",
                                          on = FALSE))
  write_run_config(cfg, path)
  back <- read_run_config(path)
  expect_equal(back$rollup$by, c("species", "port"))
  expect_equal(back$rollup$sum, "quantity")
  expect_false(back$rollup$on)
})

test_that("the comparison view switches the roll-up on and off", {
  skip_without_app()
  f <- lemis_files()
  rv <- shiny::isolate(new_app_state())
  shiny::isolate(rv$args <- list(ai = f$ai, gold = f$gold, schema = f$schema,
                                 paper_key = "doi", skip_setup = TRUE,
                                 rollup_by = c("species", "port"),
                                 rollup_sum = "quantity"))
  shiny::isolate(rv$rollup <- launch_rollup(rv$args))
  launch_state(rv)
  expect_equal(shiny::isolate(rv$stage), "compare")
  expect_equal(shiny::isolate(nrow(rv$rollup_members)), 2L)

  shiny::testServer(mod_compare_server, args = list(id = "compare", rv = rv), {
    rolled_rid <- rv$pairs$ai_rid[rv$pairs$kind == "pair"][[1L]]
    # A rolled-up row's quotes come from every record in it.
    members <- member_rids(rv, rolled_rid)
    expect_length(members, 12L)
    q <- pair_quotes(rv$ai_raw, rv$gold_raw, members, "g00001", evidence_cols(rv))
    expect_length(q$ai, 12L)
    expect_equal(nrow(member_records(rv, rolled_rid)), 12L)

    session$setInputs(rollup_on = FALSE)
    expect_false(rv$rollup$on)
    expect_null(rv$rollup_members)
    expect_equal(sum(rv$pairs$kind == "ai_only"), 11L)

    session$setInputs(rollup_on = TRUE)
    expect_equal(sum(rv$pairs$kind == "ai_only"), 0L)
  })
})
