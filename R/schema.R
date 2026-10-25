#' Read the extraction schema
#'
#' Reads the `schema.json` the AI extracted against. The schema is a required
#' input: it drives comparator defaults, enum conformance checks, and the
#' default linkage-field suggestion.
#'
#' The record object is located tolerantly. A JSON Schema describing a whole
#' extraction usually wraps the record in an array property (`records`,
#' `items`, ...); a schema describing a single record is accepted directly.
#'
#' @param path Path to a `schema.json` file, or a list already parsed from one.
#'
#' @return An object of class `ecoeval_schema`: a list with `fields` (a tibble,
#'   one row per field), `unique_fields` (character, from `x-unique-fields`, or
#'   empty), `raw` (the parsed schema), and `path`.
#' @export
read_schema <- function(path) {
  # Checked here, because jsonlite reads a string that is not a file as JSON
  # text and reports the path itself as malformed.
  if (!is.list(path) && !file.exists(path)) eco_abort(paste0("No schema at ", path))
  raw <- if (is.list(path)) path else jsonlite::fromJSON(path, simplifyVector = FALSE)
  node <- locate_record_object(raw)
  if (is.null(node)) {
    eco_abort(
      "No object with `properties` found in the schema -- is this a JSON Schema?",
      class = "ecoeval_bad_schema"
    )
  }
  structure(
    list(
      fields = schema_fields(node),
      unique_fields = as.character(node[["x-unique-fields"]] %||% character(0)),
      raw = raw,
      node = node,
      path = if (is.character(path)) path else NA_character_
    ),
    class = "ecoeval_schema"
  )
}

#' @export
print.ecoeval_schema <- function(x, ...) {
  cat("<ecoeval_schema>", nrow(x$fields), "fields\n")
  if (length(x$unique_fields)) {
    cat("  x-unique-fields:", paste(x$unique_fields, collapse = ", "), "\n")
  }
  print(x$fields[, c("field", "type", "n_enum", "comparator")])
  invisible(x)
}

#' Locate the object describing one record inside a JSON Schema
#'
#' Prefers the `items` of an array property, since an extraction schema
#' normally wraps records in an array. Falls back to the schema root, then to a
#' depth-first search for the object with the most properties.
#'
#' @param x A parsed JSON Schema.
#' @return The record-describing sub-object, or `NULL`.
#' @keywords internal
#' @noRd
locate_record_object <- function(x) {
  best <- NULL
  best_n <- -1L

  walk <- function(node, depth = 0L, in_array = FALSE) {
    if (!is.list(node) || depth > 8L) return(invisible(NULL))
    if (is.list(node$properties) && length(node$properties)) {
      # An array's `items` outranks the wrapper it sits in.
      n <- length(node$properties) + if (in_array) 1000L else 0L
      if (n > best_n) {
        best_n <<- n
        best <<- node
      }
    }
    if (is.list(node$items)) walk(node$items, depth + 1L, in_array = TRUE)
    if (is.list(node$properties)) {
      for (child in node$properties) walk(child, depth + 1L, in_array = FALSE)
    }
    for (key in c("$defs", "definitions")) {
      if (is.list(node[[key]])) for (child in node[[key]]) walk(child, depth + 1L)
    }
    invisible(NULL)
  }

  walk(x)
  best
}

#' Field table for a record object
#'
#' @param node The record-describing sub-object of a JSON Schema.
#' @return A tibble with one row per field: `field`, `type`, `format`,
#'   `description`, `enum` (list column), `n_enum`, `comparator`, and the
#'   comparator parameter defaults.
#' @keywords internal
#' @noRd
schema_fields <- function(node) {
  props <- node$properties %||% list()
  required <- as.character(node$required %||% character(0))
  if (!length(props)) {
    return(empty_tbl(
      field = character(), type = character(), format = character(),
      description = character(), enum = list(), n_enum = integer(),
      required = logical(), comparator = character()
    ))
  }

  rows <- lapply(names(props), function(nm) {
    p <- props[[nm]]
    type <- schema_type(p)
    # An array of enums carries its enum on `items`.
    enum <- p$enum %||% p$items$enum %||% NULL
    enum <- if (is.null(enum)) character(0) else as.character(unlist(enum))
    fmt <- as.character(p$format %||% p$items$format %||% NA_character_)
    tibble::tibble(
      field = nm,
      type = type,
      format = fmt,
      description = as.character(p$description %||% NA_character_),
      enum = list(enum),
      n_enum = length(enum),
      required = nm %in% required
    )
  })
  out <- dplyr::bind_rows(rows)
  out$comparator <- default_comparator(out)
  out
}

#' Normalise a JSON Schema `type` to a single string
#'
#' Union types (`["string", "null"]`) collapse to the non-null member, which is
#' how extraction schemas express optionality.
#'
#' @param p A property sub-schema.
#' @return A length-one character type.
#' @keywords internal
#' @noRd
schema_type <- function(p) {
  ty <- unlist(p$type %||% NA_character_)
  ty <- setdiff(as.character(ty), "null")
  if (!length(ty)) {
    ty <- if (!is.null(p$enum)) "string" else if (!is.null(p$properties)) "object" else "string"
  }
  ty[[1L]]
}

#' Default comparator for each field
#'
#' Chosen from each field's JSON Schema type (see `DESIGN.md`, "Comparators").
#' Free text is told apart from short strings by the field's name, since JSON
#' Schema has no type for prose.
#'
#' Free text defaults to `"judge"`. The exact, normalized and fuzzy steps still
#' run first, so the AI reviewer only sees what they could not match. Without an
#' API key those cells are marked as unjudged. Two records can describe the
#' same fact in entirely different words, so string similarity does not work
#' for prose.
#'
#' @param fields A field tibble (the `fields` element of an `ecoeval_schema`),
#'   or the schema itself.
#' @return A character vector of comparator names, one per field.
#' @export
default_comparator <- function(fields) {
  if (inherits(fields, "ecoeval_schema")) fields <- fields$fields
  vapply(seq_len(nrow(fields)), function(i) {
    type <- fields$type[[i]]
    enum <- fields$enum[[i]]
    fmt <- fields$format[[i]]
    nm <- fields$field[[i]]
    if (length(enum)) return("exact")
    if (type %in% c("number", "integer")) return("numeric")
    if (type == "boolean") return("exact")
    if (type == "array") return("set")
    if (!is.na(fmt) && fmt %in% c("date", "date-time")) return("date")
    if (is_free_text_name(nm)) return("judge")
    "normalized"
  }, character(1))
}

#' Does a field name look like free prose?
#'
#' @param nm A character vector of field names.
#' @return A logical vector.
#' @keywords internal
#' @noRd
is_free_text_name <- function(nm) {
  grepl(
    "sentence|quote|excerpt|passage|abstract|summary|note|comment|remark|rationale|justification|description|detail|context|method",
    nm,
    ignore.case = TRUE
  )
}

#' Default comparator configuration for a set of fields
#'
#' Produces the per-field configuration the comparator cascade and the app
#' both read: which rung to stop at, its parameters, whether the field takes
#' part in linkage, and whether it is scored at all.
#'
#' @param schema An `ecoeval_schema`, or `NULL` when no schema is available.
#' @param fields Character vector of fields to configure. Defaults to the
#'   schema's fields.
#' @param linkage Character vector of linkage fields. Defaults to the schema's
#'   `x-unique-fields` when present -- a suggestion, nothing more.
#'
#' @return A tibble with one row per field and columns `field`, `comparator`,
#'   `threshold`, `tolerance`, `set_mode`, `normalizer`, `linkage`, `include`.
#' @export
default_comparator_config <- function(schema = NULL, fields = NULL, linkage = NULL) {
  if (is.null(fields)) {
    if (is.null(schema)) eco_abort("Supply `schema` or `fields`.")
    fields <- schema$fields$field
  }
  known <- if (is.null(schema)) character(0) else schema$fields$field
  comparator <- vapply(fields, function(f) {
    i <- match(f, known)
    if (is.na(i)) if (is_free_text_name(f)) "judge" else "normalized" else schema$fields$comparator[[i]]
  }, character(1), USE.NAMES = FALSE)

  if (is.null(linkage)) linkage <- if (is.null(schema)) character(0) else schema$unique_fields
  linkage <- intersect(linkage, fields)

  tibble::tibble(
    field = fields,
    comparator = comparator,
    threshold = 0.85,
    tolerance = 1e-6,
    set_mode = "jaccard",
    normalizer = "none",
    linkage = fields %in% linkage,
    include = TRUE
  )
}

#' Check values against the schema
#'
#' A pre-flight check run on both sources. Reports values outside a field's
#' `enum` and values whose type cannot be coerced, **with their frequencies** --
#' the counts are what let a user tell a missing category (recurring) from a
#' typo (one-off). Nothing here adjusts a metric.
#'
#' @param records A record tibble whose columns are schema field names.
#' @param schema An `ecoeval_schema`.
#' @param source Label recorded on each row, e.g. `"ai"` or `"gold"`.
#'
#' @return A tibble with `source`, `field`, `value`, `n`, and `reason`
#'   (`"not_in_enum"` or `"bad_type"`), sorted by field then descending count.
#' @export
check_conformance <- function(records, schema, source = "ai") {
  out <- list()
  for (i in seq_len(nrow(schema$fields))) {
    field <- schema$fields$field[[i]]
    if (!field %in% names(records)) next
    col <- records[[field]]
    enum <- schema$fields$enum[[i]]
    type <- schema$fields$type[[i]]

    values <- if (schema$fields$type[[i]] == "array" || is.list(col)) {
      unlist(lapply(col, as_set), use.names = FALSE)
    } else {
      as.character(col)
    }
    values <- values[!is_blank(values)]
    if (!length(values)) next

    bad <- character(0)
    reason <- character(0)
    if (length(enum)) {
      offenders <- values[!values %in% enum]
      bad <- c(bad, offenders)
      reason <- c(reason, rep("not_in_enum", length(offenders)))
    } else if (type %in% c("number", "integer")) {
      offenders <- values[is.na(suppressWarnings(as.numeric(values)))]
      bad <- c(bad, offenders)
      reason <- c(reason, rep("bad_type", length(offenders)))
    } else if (type == "boolean") {
      ok <- tolower(values) %in% c("true", "false", "t", "f", "yes", "no", "0", "1")
      bad <- c(bad, values[!ok])
      reason <- c(reason, rep("bad_type", sum(!ok)))
    } else if (!is.na(schema$fields$format[[i]]) &&
               schema$fields$format[[i]] %in% c("date", "date-time")) {
      offenders <- values[is.na(parse_date_loose(values))]
      bad <- c(bad, offenders)
      reason <- c(reason, rep("bad_type", length(offenders)))
    }
    if (!length(bad)) next

    out[[length(out) + 1L]] <- dplyr::count(
      tibble::tibble(source = source, field = field, value = bad,
                     reason = reason),
      .data$source, .data$field, .data$value, .data$reason, name = "n"
    )
  }
  if (!length(out)) {
    return(empty_tbl(source = character(), field = character(),
                     value = character(), reason = character(), n = integer()))
  }
  dplyr::arrange(dplyr::bind_rows(out), .data$field, dplyr::desc(.data$n))
}

#' Fields the schema knows nothing about
#'
#' Gold columns with no schema counterpart are a schema finding: the human
#' recorded a kind of answer nobody told the AI to look for.
#'
#' @param fields Character vector of field names in use.
#' @param schema An `ecoeval_schema`.
#' @return The subset of `fields` absent from the schema.
#' @export
unschematised_fields <- function(fields, schema) {
  setdiff(fields, schema$fields$field)
}

#' The column that quotes the paper
#'
#' Extraction schemas in this family carry a field holding the sentences the
#' value was read out of -- `all_supporting_source_sentences` in the bundled
#' example. When the AI and the gold standard disagree, the quoted text usually
#' shows which of them read the paper correctly, so the cell pop-up shows it
#' beside each value.
#'
#' Found by name, most specific first. Returns `NA` when no column looks like
#' one; the supporting sentences are optional.
#'
#' @param fields Character vector of field names in use.
#' @return A field name, or `NA_character_`.
#' @examples
#' evidence_field(c("species", "all_supporting_source_sentences"))
#' evidence_field(c("species", "count"))
#' @export
evidence_field <- function(fields) {
  fields <- as.character(fields)
  sq <- squash_name(fields)
  for (pat in c("supportingsourcesentence", "sourcesentence", "supportingsentence",
                "supportingquote", "supportingtext", "^supporting", "sentence",
                "^evidence", "evidence", "^quote", "quotation", "excerpt",
                "^snippet", "sourcetext")) {
    hit <- which(grepl(pat, sq))
    if (length(hit)) return(fields[[hit[[1L]]]])
  }
  NA_character_
}

#' The column holding each side's supporting sentences
#'
#' The cell pop-up shows the sentences each side quoted for a value. Which
#' column holds them can be named, for both sides or one side at a time; a side
#' not named is found by [evidence_field()], so naming nothing changes nothing.
#'
#' @param evidence_col `NULL` to find both by name; one column name for both
#'   sides; or one per side, `c(ai = "supporting_text", gold = NA)`, where `NA`
#'   means that side has none and a side left out is found by name.
#' @param ai,gold Each side's column names, or its record table.
#' @return A named character vector, `c(ai = , gold = )`, `NA` where a side has
#'   none. A named column is returned whether or not the table has it, so a
#'   typo can be reported rather than silently showing nothing.
#' @examples
#' evidence_columns(NULL, c("species", "all_supporting_source_sentences"), "species")
#' evidence_columns(c(ai = "context", gold = NA), "context", "notes")
#' @export
evidence_columns <- function(evidence_col = NULL, ai = character(0),
                             gold = character(0)) {
  spec <- as_evidence_spec(evidence_col)
  cols <- function(x) if (is.data.frame(x)) names(x) else as.character(x)
  found <- list(ai = cols(ai), gold = cols(gold))
  vapply(c(ai = "ai", gold = "gold"), function(side) {
    spec[[side]] %||% evidence_field(found[[side]])
  }, character(1))
}

#' Check an `evidence_col` argument and put it in one shape
#'
#' @param evidence_col As for [evidence_columns()].
#' @return A list with an `ai` and a `gold` element each holding a column
#'   name, `NA` for none, or `NULL` (or absent) to find it by name.
#' @keywords internal
#' @noRd
as_evidence_spec <- function(evidence_col) {
  if (!length(evidence_col)) return(list())
  bad <- function() {
    eco_abort(paste0("`evidence_col` is a column name, or one per side: ",
                     "c(ai = \"supporting_text\", gold = NA)."))
  }
  one <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) != 1L || !(is.character(x) || is.na(x))) bad()
    x <- as.character(x)
    if (!is.na(x) && !nzchar(x)) bad()
    x
  }
  nms <- names(evidence_col)
  if (is.null(nms)) {
    if (length(evidence_col) != 1L) bad()
    x <- one(evidence_col[[1L]])
    return(list(ai = x, gold = x))
  }
  if (!all(nms %in% c("ai", "gold")) || anyDuplicated(nms)) bad()
  lapply(as.list(evidence_col), one)
}
