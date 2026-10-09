# The export bundle.
#
# The session artifact and the export bundle are the same object, so save,
# restore, and download share one implementation. README.txt carries the scope
# statement and any active warnings, so caveats travel with the export instead
# of living only on screen.

#' One row per aligned record, mirroring the grid
#'
#' @param cells A cell tibble from [score_cells()].
#' @param pairs A pair tibble from [align_records()].
#' @return A wide tibble: the pair's identity columns, then `ai_<field>`,
#'   `gold_<field>`, and `state_<field>` for every scored field. Values are
#'   what each side said, before any normaliser, as the grid shows them.
#' @export
aligned_table <- function(cells, pairs) {
  if (!nrow(cells)) return(tibble::tibble())
  fields <- unique(cells$field)
  base <- pairs[, c("pair_id", "paper", "ai_rid", "gold_rid", "kind",
                    "posterior", "matcher", "link_source")]
  for (f in fields) {
    sub <- cells[cells$field == f, , drop = FALSE]
    idx <- match(base$pair_id, sub$pair_id)
    base[[paste0("ai_", f)]] <- original_values(sub, "ai")[idx]
    base[[paste0("gold_", f)]] <- original_values(sub, "gold")[idx]
    base[[paste0("state_", f)]] <- sub$state[idx]
  }
  base
}

#' Tidy form: one row per record by field
#'
#' The shape to pivot from when someone wants a cut the dashboard does not
#' offer.
#'
#' @param cells A cell tibble from [score_cells()].
#' @return `cells` with a `colour` column added.
#' @export
aligned_table_long <- function(cells) {
  if (!nrow(cells)) return(cells)
  cells$colour <- unname(state_colour(cells$state))
  cells
}

#' Write the metrics workbook
#'
#' One sheet per field holding that field's confusion matrix and its precision,
#' recall, F1 and n, plus a summary sheet listing every column as a row with
#' the two aggregates.
#'
#' @param path Destination `.xlsx`.
#' @param cells A cell tibble.
#' @param pairs A pair tibble.
#' @param schema An `ecoeval_schema`, or `NULL`.
#' @return `path`, invisibly.
#' @export
write_metrics_workbook <- function(path, cells, pairs, schema = NULL) {
  wb <- openxlsx::createWorkbook()
  fm <- field_metrics(cells)
  agg <- aggregate_metrics(cells)
  rm_ <- record_metrics(pairs)

  openxlsx::addWorksheet(wb, "summary")
  openxlsx::writeData(wb, "summary", "Aggregate", startRow = 1)
  openxlsx::writeData(wb, "summary", agg, startRow = 2)
  openxlsx::writeData(wb, "summary", "Records", startRow = 5)
  openxlsx::writeData(wb, "summary", rm_, startRow = 6)
  openxlsx::writeData(wb, "summary", "Fields", startRow = 9)
  openxlsx::writeData(wb, "summary", fm, startRow = 10)

  for (f in fm$field) {
    sheet <- substr(gsub("[\\[\\]:*?/\\\\]", "_", f), 1, 31)
    if (sheet %in% names(wb)) next
    openxlsx::addWorksheet(wb, sheet)
    cc <- column_confusion(cells, f, schema)
    wide <- confusion_wide(cc$matrix)
    openxlsx::writeData(wb, sheet, paste0("Confusion matrix (", cc$type, ")"),
                        startRow = 1)
    openxlsx::writeData(wb, sheet, wide, startRow = 2, rowNames = TRUE)
    row <- 3 + nrow(wide) + 1
    openxlsx::writeData(wb, sheet, "Metrics", startRow = row)
    openxlsx::writeData(wb, sheet, fm[fm$field == f, , drop = FALSE],
                        startRow = row + 1)
    if (length(cc$notes)) {
      openxlsx::writeData(wb, sheet, "Notes", startRow = row + 4)
      openxlsx::writeData(wb, sheet, tibble::tibble(note = cc$notes),
                          startRow = row + 5)
    }
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Long confusion matrix as a wide grid
#'
#' @param m A long matrix tibble with `gold_class`, `ai_class`, `n`.
#' @return A base data frame with gold classes as row names.
#' @keywords internal
#' @noRd
confusion_wide <- function(m) {
  if (is.null(m) || !nrow(m)) return(data.frame())
  gold <- unique(m$gold_class)
  ai <- unique(m$ai_class)
  out <- matrix(0L, nrow = length(gold), ncol = length(ai),
                dimnames = list(gold, ai))
  out[cbind(match(m$gold_class, gold), match(m$ai_class, ai))] <- as.integer(m$n)
  as.data.frame(out)
}

#' The README that travels with an export
#'
#' @param scope The result of [compute_scope()].
#' @param warnings Active warnings.
#' @param progress A tibble from [progress_summary()].
#' @param corrections The string from [correction_count()], or `NULL`.
#' @param agg A tibble from [aggregate_metrics()].
#' @param rm_ A tibble from [record_metrics()].
#' @return A character vector of lines.
#' @export
bundle_readme <- function(scope, warnings = character(0), progress = NULL,
                          corrections = NULL, agg = NULL, rm_ = NULL) {
  lines <- c(
    "ecoeval evaluation run",
    paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("ecoeval ", ecoeval_version()),
    "",
    "WHAT IS HERE",
    "  aligned_table.csv       one row per aligned record, mirrors the grid",
    "  aligned_table_long.csv  tidy: one row per record x field, for pivoting",
    "  metrics.xlsx            one sheet per field plus a summary sheet",
    "  findings.csv            findings, grouped schema / model / gold",
    "  schema_patch.json       proposed schema edits",
    "  plots/                  column accuracy and per-field confusion matrices",
    "  run_config.json         everything needed to reproduce this run",
    "",
    "SCOPE",
    sprintf("  %4d papers in both    -- evaluated", length(scope$papers)),
    sprintf("  %4d papers AI only    -- excluded", length(scope$ai_only)),
    sprintf("  %4d papers gold only  -- excluded", length(scope$gold_only)),
    "",
    "  Papers are a filter, not a scored entity. Papers outside the",
    "  intersection are excluded, never penalised."
  )
  if (!is.null(agg) && nrow(agg)) {
    lines <- c(lines, "", "HEADLINE NUMBERS",
      sprintf("  Overall accuracy (all cells)        %s", pct(agg$overall_accuracy)),
      sprintf("  Average across columns              %s", pct(agg$column_mean_accuracy)))
  }
  if (!is.null(rm_) && nrow(rm_)) {
    lines <- c(lines,
      sprintf("  Record precision / recall / F1      %s / %s / %s",
              pct(rm_$precision), pct(rm_$recall), pct(rm_$f1)))
  }
  if (!is.null(progress) && nrow(progress)) {
    lines <- c(lines, "", "HOW THESE NUMBERS WERE PRODUCED",
      sprintf("  %d of %d papers judged, %d reviewed by a person",
              progress$n_judged, progress$n_papers, progress$n_reviewed))
    if (progress$n_pending_cells > 0L) {
      lines <- c(lines, sprintf(
        "  %d cells are still unjudged -- only the cheap comparator rungs ran on them.",
        progress$n_pending_cells))
    }
  }
  if (!is.null(corrections)) {
    lines <- c(lines, sprintf("  Manual corrections: %s", corrections))
  }
  if (length(warnings)) {
    lines <- c(lines, "", "ACTIVE WARNINGS",
               paste0("  - ", warnings))
  }
  lines
}

#' @keywords internal
#' @noRd
pct <- function(x) if (length(x) != 1L || is.na(x)) "n/a" else sprintf("%.1f%%", 100 * x)

#' Write the whole export bundle
#'
#' @param dir Directory to create. Defaults to `ecoeval_run_<date>` in the
#'   working directory.
#' @param cells,pairs The scored evaluation.
#' @param config The run configuration to freeze alongside it.
#' @param findings A tibble from [collect_findings()].
#' @param scope The result of [compute_scope()].
#' @param schema An `ecoeval_schema`, or `NULL`.
#' @param patch A list from [schema_patch()], or `NULL`.
#' @param warnings Active warnings.
#' @param progress A tibble from [progress_summary()].
#' @param corrections The string from [correction_count()].
#' @param plots Whether to render the PNG plots.
#'
#' @return The bundle directory, invisibly.
#' @export
export_bundle <- function(dir = NULL, cells, pairs, config,
                          findings = NULL, scope = NULL, schema = NULL,
                          patch = NULL, warnings = character(0),
                          progress = NULL, corrections = NULL,
                          plots = TRUE) {
  if (is.null(dir)) {
    dir <- paste0("ecoeval_run_", format(Sys.Date(), "%Y-%m-%d"))
  }
  fs::dir_create(dir)
  scope <- scope %||% list(papers = unique(pairs$paper), ai_only = character(0),
                           gold_only = character(0))

  readr::write_csv(aligned_table(cells, pairs), fs::path(dir, "aligned_table.csv"))
  readr::write_csv(aligned_table_long(cells), fs::path(dir, "aligned_table_long.csv"))
  write_metrics_workbook(fs::path(dir, "metrics.xlsx"), cells, pairs, schema)
  readr::write_csv(findings %||% collect_findings(cells, pairs),
                   fs::path(dir, "findings.csv"))
  if (!is.null(patch)) {
    jsonlite::write_json(patch, fs::path(dir, "schema_patch.json"),
                         auto_unbox = TRUE, pretty = TRUE)
  }
  write_run_config(config, fs::path(dir, "run_config.json"))

  if (plots) {
    pdir <- fs::path(dir, "plots")
    fs::dir_create(pdir)
    fm <- field_metrics(cells)
    save_plot(fs::path(pdir, "column_accuracy.png"), plot_column_accuracy(fm),
              height = max(3, 0.4 * nrow(fm) + 1.5))
    save_plot(fs::path(pdir, "completeness.png"), plot_completeness(fill_rates(cells)),
              height = max(3, 0.4 * nrow(fm) + 1.8))
    save_plot(fs::path(pdir, "record_outcome.png"), plot_record_outcome(record_metrics(pairs)),
              height = 3)
    for (f in fm$field) {
      cc <- column_confusion(cells, f, schema)
      save_plot(fs::path(pdir, paste0("confusion_", fs::path_sanitize(f), ".png")),
                plot_confusion(cc))
      if (!is.null(cc$errors) && nrow(cc$errors)) {
        save_plot(fs::path(pdir, paste0("error_", fs::path_sanitize(f), ".png")),
                  plot_error_distribution(cc$errors, f), height = 3)
      }
    }
  }

  writeLines(
    bundle_readme(scope, warnings, progress, corrections,
                  aggregate_metrics(cells), record_metrics(pairs)),
    fs::path(dir, "README.txt")
  )
  invisible(dir)
}

#' @keywords internal
#' @noRd
save_plot <- function(path, plot, width = 8, height = 5) {
  tryCatch(
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = 150,
                    bg = "white"),
    error = function(e) invisible(NULL)
  )
}

#' Zip an export bundle
#'
#' @param dir A bundle directory from [export_bundle()].
#' @param zipfile Destination `.zip`.
#' @return `zipfile`, invisibly.
#' @export
zip_bundle <- function(dir, zipfile = paste0(dir, ".zip")) {
  old <- setwd(fs::path_dir(dir))
  on.exit(setwd(old), add = TRUE)
  utils::zip(zipfile = fs::path_abs(zipfile, old), files = fs::path_file(dir),
             flags = "-r9Xq")
  invisible(zipfile)
}
