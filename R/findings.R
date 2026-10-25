# Findings.
#
# Findings are output, not gates. They are grouped by WHO ACTS on them -- the
# schema, the model, or the gold standard -- so the user knows where to go
# next. Nothing here adjusts a metric.
#
# Only two conditions stop the app rather than informing it, because neither
# leaves anything to compare: zero paper overlap, and no mappable fields.

#' Assemble the findings table
#'
#' @param cells A cell tibble from [score_cells()].
#' @param pairs A pair tibble from [align_records()].
#' @param conformance Rows from [check_conformance()] for both sources.
#' @param schema An `ecoeval_schema`, or `NULL`.
#' @param gold_fields Canonical field names present in the gold standard.
#' @param collapses Rows from [granularity_check()].
#' @param dropped_fields Fields excluded from the grid because only one side
#'   had them.
#' @param linkage_fields The identity columns, so triage can flag pairs whose
#'   identity columns all disagree.
#' @param recurring_at Count at which an out-of-enum value reads as a missing
#'   category rather than a typo.
#' @param labels Side labels for the wording; see [side_labels()]. The `group`
#'   codes stay `"schema"`, `"model"`, `"gold"`.
#' @param rollup_suggestion The result of [suggest_rollup()], when the
#'   extraction was not rolled up. A suggestion becomes a finding.
#'
#' @return A tibble with `group` (`"schema"`, `"model"`, or `"gold"`),
#'   `finding`, `detail`, `n`, sorted by group then descending `n`.
#' @export
collect_findings <- function(cells,
                             pairs,
                             conformance = NULL,
                             schema = NULL,
                             gold_fields = character(0),
                             collapses = NULL,
                             dropped_fields = character(0),
                             linkage_fields = character(0),
                             recurring_at = 3L,
                             labels = current_labels(),
                             rollup_suggestion = NULL) {
  labels <- as_side_labels(labels)
  out <- list()
  add <- function(group, finding, detail, n = NA_integer_) {
    out[[length(out) + 1L]] <<- tibble::tibble(
      group = group, finding = finding, detail = detail, n = as.integer(n)
    )
  }

  # --- Schema and prompt -------------------------------------------------
  if (!is.null(conformance) && nrow(conformance)) {
    gold_enum <- conformance[conformance$source == "gold" &
                               conformance$reason == "not_in_enum", , drop = FALSE]
    for (i in seq_len(nrow(gold_enum))) {
      if (gold_enum$n[[i]] < recurring_at) next
      add("schema", sprintf("Recurring %s value outside the schema enum", labels$gold),
          sprintf("%s: '%s' appears %d times. The schema probably needs this value.",
                  gold_enum$field[[i]], gold_enum$value[[i]], gold_enum$n[[i]]),
          gold_enum$n[[i]])
    }
  }
  if (!is.null(schema) && length(gold_fields)) {
    orphan <- unschematised_fields(gold_fields, schema)
    for (f in orphan) {
      add("schema", sprintf("%s field with no schema counterpart", labels$gold),
          sprintf("%s records '%s'; the schema has no slot for it.", labels$gold, f))
    }
  }
  for (f in dropped_fields) {
    add("schema", "Field excluded from the comparison",
        sprintf("'%s' exists on only one side, so there is nothing to compare.", f))
  }
  if (!is.null(collapses) && nrow(collapses)) {
    n_papers <- length(unique(collapses$paper))
    add("schema", "Granularity mismatch on the linkage fields",
        sprintf(paste0("In %2$d papers, %1$d groups of records share the same ",
                       "identity column values, so those columns cannot tell ",
                       "the records apart. Add another identity column."),
                nrow(collapses), n_papers),
        nrow(collapses))
  }
  fills <- fill_rates(cells)
  for (i in seq_len(nrow(fills))) {
    gap <- fills$gap[[i]]
    if (is.na(gap) || gap < 0.4) next
    add("schema", "Fill-rate asymmetry",
        sprintf(paste0("%s fills in '%s' %.0f%% of the time, %s %.0f%%. ",
                       "Check the field's description in the schema first."),
                labels$gold, fills$field[[i]], 100 * fills$gold_fill[[i]],
                labels$ai, 100 * fills$ai_fill[[i]]),
        round(100 * gap))
  }

  # Several extraction records that look like parts of one reference record.
  if (length(rollup_suggestion$by)) {
    add("schema", sprintf("%s records in more detail than %s", labels$ai, labels$gold),
        paste0(rollup_suggestion$summary, " Try rollup_by = c(",
               paste0('"', rollup_suggestion$by, '"', collapse = ", "), ")",
               if (length(rollup_suggestion$sum))
                 paste0(" and rollup_sum = c(",
                        paste0('"', rollup_suggestion$sum, '"', collapse = ", "), ")"),
               "."),
        rollup_suggestion$candidates$merged[[1L]])
  }

  # --- Model -------------------------------------------------------------
  fm <- field_metrics(cells)
  for (i in seq_len(nrow(fm))) {
    if (is.na(fm$accuracy[[i]]) || fm$accuracy[[i]] > 0.2 || fm$n_scored[[i]] < 5L) next
    add("model", "Column is almost always wrong",
        sprintf(paste0("'%s' is correct on %.0f%% of %d scored cells. Check the ",
                       "comparator first, then the field description, then the model."),
                fm$field[[i]], 100 * fm$accuracy[[i]], fm$n_scored[[i]]),
        fm$n_scored[[i]])
  }
  if (!is.null(conformance) && nrow(conformance)) {
    ai_bad <- conformance[conformance$source == "ai", , drop = FALSE]
    if (nrow(ai_bad)) {
      add("model", sprintf("%s values that violate the schema", labels$ai),
          sprintf(paste0("%d distinct values across %d fields fail enum or type ",
                         "validation. Structured output should prevent this, so ",
                         "check the extraction pipeline too."),
                  nrow(ai_bad), length(unique(ai_bad$field))),
          sum(ai_bad$n))
    }
  }
  rm_ <- record_metrics(pairs)
  if (rm_$fp > 0 || rm_$fn > 0) {
    add("model", "Record-level precision and recall",
        sprintf("%d matched, %d %s%s, %d %s%s. Precision %.2f, recall %.2f.",
                rm_$tp, rm_$fp, side_only(labels, "ai"),
                cm_tag(labels, "false positives"),
                rm_$fn, side_only(labels, "gold"),
                cm_tag(labels, "false negatives"),
                rm_$precision, rm_$recall),
        rm_$fp + rm_$fn)
  }
  susp <- suspect_pairs(cells, linkage_fields)
  if (nrow(susp)) {
    add("model", "Pairings that look wrong",
        sprintf(paste0("%d pairs across %d papers look paired wrongly (%s). ",
                       "Start your review with these."),
                nrow(susp), length(unique(susp$paper)),
                paste(unique(susp$reason), collapse = "; ")),
        nrow(susp))
  }

  # --- Gold standard -----------------------------------------------------
  if (!is.null(conformance) && nrow(conformance)) {
    oneoff <- conformance[conformance$source == "gold" &
                            conformance$n < recurring_at, , drop = FALSE]
    for (i in seq_len(nrow(oneoff))) {
      add("gold", "One-off value outside the schema",
          sprintf("%s: '%s' appears %d time(s). Probably a typo or a different spelling.",
                  oneoff$field[[i]], oneoff$value[[i]], oneoff$n[[i]]),
          oneoff$n[[i]])
    }
  }

  if (!length(out)) {
    return(empty_tbl(group = character(), finding = character(),
                     detail = character(), n = integer()))
  }
  res <- dplyr::bind_rows(out)
  res$group <- factor(res$group, levels = c("schema", "model", "gold"))
  res <- dplyr::arrange(res, .data$group, dplyr::desc(dplyr::coalesce(.data$n, 0L)))
  res$group <- as.character(res$group)
  res
}

#' Internal consistency checks on one source
#'
#' Run at load, alongside the schema conformance check. Every paper referenced
#' in a record list should appear in that source's paper list; records with a
#' null or unresolvable paper reference are unusable; duplicate paper IDs make
#' scope ambiguous.
#'
#' @param records A canonical record tibble.
#' @param papers A canonical paper tibble, or `NULL`.
#' @param source Label for the report.
#'
#' @return A tibble with `source`, `check`, `detail`, `n`.
#' @export
source_consistency <- function(records, papers = NULL, source = "ai") {
  out <- list()
  add <- function(check, detail, n) {
    out[[length(out) + 1L]] <<- tibble::tibble(
      source = source, check = check, detail = detail, n = as.integer(n)
    )
  }

  n_null <- sum(is.na(records$.paper))
  if (n_null) {
    add("unresolvable_paper",
        sprintf("%d records have no paper reference and cannot be placed in scope.", n_null),
        n_null)
  }
  if (!is.null(papers) && nrow(papers)) {
    dup <- sum(duplicated(papers$.paper))
    if (dup) {
      add("duplicate_paper_id",
          sprintf("%d duplicate paper identifiers in the paper list.", dup), dup)
    }
    missing <- setdiff(unique(records$.paper[!is.na(records$.paper)]), papers$.paper)
    if (length(missing)) {
      add("records_outside_paper_list",
          sprintf(paste0("%d papers have records but do not appear in the paper ",
                         "list. If the list misses papers that have records, it ",
                         "may also miss papers where nothing was found, which ",
                         "are what the list is for."),
                  length(missing)),
          length(missing))
    }
  }
  if (!length(out)) {
    return(empty_tbl(source = character(), check = character(),
                     detail = character(), n = integer()))
  }
  dplyr::bind_rows(out)
}

#' The two conditions that stop the app
#'
#' Everything else is output. These two leave nothing to compare.
#'
#' @param scope The result of [compute_scope()].
#' @param mapped_fields Canonical fields present on both sides.
#' @return `NULL` when the run can proceed, otherwise a character scalar
#'   explaining what is missing.
#' @export
blocking_condition <- function(scope, mapped_fields) {
  if (!length(scope$papers)) {
    return(paste0(
      "No papers are present in both sources, so there is nothing to compare. ",
      "Check that the paper identifier columns hold the same kind of value on ",
      "both sides."
    ))
  }
  if (!length(mapped_fields)) {
    return(paste0(
      "No fields map between the two record sets, so there is nothing to ",
      "score. Map at least one gold column onto an AI column."
    ))
  }
  NULL
}

#' Schema patch proposed by the findings
#'
#' Turns the schema findings into a list of proposed edits to apply to the
#' schema: values to add to enums, fields whose descriptions may be unclear,
#' and identity fields that need another field.
#'
#' @param conformance Rows from [check_conformance()].
#' @param collapses Rows from [granularity_check()].
#' @param cells A cell tibble.
#' @param schema An `ecoeval_schema`.
#' @param recurring_at Count at which a value earns a place in the enum.
#'
#' @return A list ready for [jsonlite::toJSON()], with an `enum_additions`
#'   element per field, an optional `x-unique-fields` note, and
#'   `description_review` for columns that look ambiguous.
#' @export
schema_patch <- function(conformance, collapses, cells, schema,
                         recurring_at = 3L) {
  patch <- list(enum_additions = list(), description_review = list())

  if (!is.null(conformance) && nrow(conformance)) {
    rec <- conformance[conformance$source == "gold" &
                         conformance$reason == "not_in_enum" &
                         conformance$n >= recurring_at, , drop = FALSE]
    for (f in unique(rec$field)) {
      patch$enum_additions[[f]] <- as.character(rec$value[rec$field == f])
    }
  }
  if (!is.null(collapses) && nrow(collapses)) {
    patch[["x-unique-fields"]] <- paste0(
      "Records collapse on the current key in ", length(unique(collapses$paper)),
      " papers. Add another field to tell them apart."
    )
  }
  fm <- field_metrics(cells)
  for (i in seq_len(nrow(fm))) {
    if (is.na(fm$accuracy[[i]]) || fm$accuracy[[i]] > 0.5 || fm$n_scored[[i]] < 5L) next
    patch$description_review[[fm$field[[i]]]] <- sprintf(
      "may be unclear: %d of %d scored cells disagree",
      fm$n_scored[[i]] - fm$tp[[i]], fm$n_scored[[i]]
    )
  }
  patch
}
