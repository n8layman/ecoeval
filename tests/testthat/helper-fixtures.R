# Paths to the synthetic fixtures in inst/extdata. Each row of those files
# exists to exercise a specific case -- see data-raw/make_fixtures.R for the
# index of which case lives where.

fixture <- function(name) {
  p <- system.file("extdata", name, package = "ecoeval")
  if (nzchar(p) && file.exists(p)) return(p)
  # Running from the source tree, e.g. under devtools::test().
  for (candidate in c(file.path("..", "..", "inst", "extdata", name),
                      file.path("inst", "extdata", name))) {
    if (file.exists(candidate)) return(candidate)
  }
  skip(paste0("Fixture not found: ", name))
}

# Almost every test wants the same aligned and scored fixtures, so compute
# them once.
.fixture_cache <- new.env(parent = emptyenv())

#' The fixtures loaded and put into canonical form, ready to align and score.
fixture_run <- function() {
  if (!is.null(.fixture_cache$run)) return(.fixture_cache$run)
  .fixture_cache$run <- build_fixture_run()
  .fixture_cache$run
}

build_fixture_run <- function() {
  schema <- read_schema(fixture("schema.json"))
  ai_raw <- read_table_any(fixture("ai_records.csv"))
  gold_raw <- read_table_any(fixture("gold_records.csv"))

  map <- suggest_mapping(setdiff(names(gold_raw), "doi"), schema$fields$field)
  mapping <- stats::setNames(map$from[!is.na(map$to)], map$to[!is.na(map$to)])

  ai <- prepare_records(ai_raw, "doi",
                        stats::setNames(schema$fields$field, schema$fields$field),
                        prefix = "a")
  gold <- prepare_records(gold_raw, "doi", mapping, prefix = "g")

  ai_papers <- prepare_papers(read_table_any(fixture("ai_papers.csv")), "doi",
                              c(title = "title", year = "year"))
  gold_papers <- prepare_papers(read_table_any(fixture("gold_papers.csv")), "doi",
                                c(title = "title", year = "year"))

  scope <- compute_scope(paper_set(ai, ai_papers), paper_set(gold, gold_papers))
  paper_map <- tibble::tibble(ai_paper = scope$papers, gold_paper = scope$papers)

  fields <- intersect(schema$fields$field, intersect(names(ai), names(gold)))
  config <- default_comparator_config(schema, fields = fields)
  linkage <- config$field[config$linkage]

  pairs <- align_records(ai, gold, linkage, paper_map)
  cells <- score_cells(pairs, ai, gold, config)

  list(schema = schema, ai = ai, gold = gold, ai_papers = ai_papers,
       gold_papers = gold_papers, scope = scope, paper_map = paper_map,
       config = config, linkage = linkage, pairs = pairs, cells = cells)
}

# ---- app state at each stage of the wizard ----------------------------------
# Built by running the real pipeline, not by hand, so the app tests start from
# state the app could actually have produced.

loaded_state <- function() shiny::isolate({
  skip_without_app()
  rv <- new_app_state()
  rv$schema <- read_schema(fixture("schema.json"))
  rv$ai_raw <- read_table_any(fixture("ai_records.csv"))
  rv$gold_raw <- read_table_any(fixture("gold_records.csv"))
  rv$ai_papers_raw <- read_table_any(fixture("ai_papers.csv"))
  rv$gold_papers_raw <- read_table_any(fixture("gold_papers.csv"))
  rv
})

mapped_state <- function() shiny::isolate({
  rv <- loaded_state()
  rv$ai_paper_key <- paper_key("doi"); rv$gold_paper_key <- paper_key("doi")
  rv$ai <- prepare_records(rv$ai_raw, "doi", prefix = "a")
  rv$gold <- prepare_records(rv$gold_raw, "doi", prefix = "g")
  rv$ai_papers <- prepare_papers(rv$ai_papers_raw, "doi",
                                 c(title = "title", year = "year"))
  rv$gold_papers <- prepare_papers(rv$gold_papers_raw, "doi",
                                   c(title = "title", year = "year"))
  rv$metadata_fields <- c("title", "year")
  rv
})

scoped_state <- function() shiny::isolate({
  rv <- mapped_state()
  rv$scope <- compute_scope(paper_set(rv$ai, rv$ai_papers),
                            paper_set(rv$gold, rv$gold_papers))
  rv$paper_map <- tibble::tibble(ai_paper = rv$scope$papers,
                                 gold_paper = rv$scope$papers)
  rv
})

scored_state <- function() shiny::isolate({
  rv <- scoped_state()
  fx <- fixture_run()
  rv$ai <- fx$ai
  rv$gold <- fx$gold
  rv$comparators <- fx$config
  rv$comparators$ai_col <- fx$config$field
  rv$comparators$gold_col <- fx$config$field
  rv$pairs <- fx$pairs
  rv$cells <- fx$cells
  rv$conformance <- dplyr::bind_rows(
    check_conformance(fx$ai, fx$schema, "ai"),
    check_conformance(fx$gold, fx$schema, "gold")
  )
  rv$collapses <- granularity_check(fx$gold, fx$linkage, "gold")
  rv$current_paper <- rv$scope$papers[[1L]]
  rv
})
