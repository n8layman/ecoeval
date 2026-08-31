# The two LLM-backed rungs.
#
# Two strategies sit at the top of the cascade, suiting different columns:
#
#   normalize-then-compare  map both values to a canonical form, then compare
#                           cheaply. Cached per VALUE. For species names, where
#                           a canonical form exists.
#   LLM judge               compare the pair directly, return verdict plus
#                           rationale. Cached per PAIR. For supporting
#                           sentences and free prose, where there is no
#                           canonical form to map to.
#
# The judge cannot move earlier in the pipeline. Alignment evaluates every
# candidate pair within a paper -- 5 AI records against 5 gold records is 25
# pairs times N linkage fields of LLM calls just to build the comparison
# matrix. Normalisation is per value: 10 values, 10 calls, then compare
# cheaply. Normalisation scales to N; the judge would scale to N squared.
#
# So normalisation runs BEFORE matching, on linkage fields, and the judge runs
# AFTER, on paired rows only, and only on cells the cheaper rungs could not
# settle.

#' Default model for the judge and the normaliser
#'
#' @return A model identifier string.
#' @export
default_judge_model <- function() {
  Sys.getenv("ECOEVAL_JUDGE_MODEL", "claude-sonnet-5")
}

#' Is an LLM available?
#'
#' @return `TRUE` when ellmer is installed and an API key is set.
#' @export
judge_available <- function() {
  requireNamespace("ellmer", quietly = TRUE) &&
    nzchar(Sys.getenv("ANTHROPIC_API_KEY"))
}

#' Build an LLM judge
#'
#' The judge decides whether two values describe the same thing. Its verdict is
#' **binary** -- the rationale carries the nuance, and a fifth colour would
#' break the grid's scheme. Every verdict is a proposal: the human override
#' exists for when it gets one wrong, and every override is recorded.
#'
#' @param model Model identifier. Defaults to [default_judge_model()].
#' @param context A sentence describing the domain, folded into the system
#'   prompt. The default is deliberately generic.
#' @param descriptions Named character vector of field descriptions from the
#'   schema, so the judge knows what the column is supposed to hold.
#'
#' @return A function `(field, a, b)` returning a list with `agree` and
#'   `rationale`, suitable for passing to [score_cells()]. Returns `NULL` when
#'   no LLM is available.
#' @export
make_judge <- function(model = default_judge_model(),
                       context = NULL,
                       descriptions = character(0)) {
  if (!judge_available()) return(NULL)
  system_prompt <- paste0(
    "You are adjudicating a comparison between two records of the same ",
    "finding: one extracted by an AI, one recorded by a human expert. ",
    "Decide whether the two values MEAN THE SAME THING. Different wording, ",
    "spelling, abbreviation, synonym, or level of detail is still the same ",
    "thing when the substantive claim is identical. A different claim, a ",
    "different entity, or a materially different level of specificity is ",
    "not. Be decisive: the verdict is binary. Keep the rationale to one ",
    "sentence.",
    if (!is.null(context)) paste0("\n\nDomain context: ", context) else ""
  )
  type <- ellmer::type_object(
    same = ellmer::type_boolean("TRUE if the two values mean the same thing."),
    rationale = ellmer::type_string("One sentence explaining the verdict.")
  )

  function(field, a, b) {
    desc <- descriptions[[field]] %||% NULL
    prompt <- paste0(
      "Field: ", field,
      if (!is.null(desc) && !is.na(desc)) paste0("\nField description: ", desc) else "",
      "\n\nAI value: ", a,
      "\nHuman value: ", b
    )
    chat <- ellmer::chat_anthropic(system_prompt = system_prompt, model = model,
                                   echo = "none")
    res <- chat$chat_structured(prompt, type = type)
    list(agree = isTRUE(res$same), rationale = as.character(res$rationale))
  }
}

#' Normalisers available for the pre-matching step
#'
#' @return A named character vector: names are labels, values are ids.
#' @export
normalizer_choices <- function() {
  c("None" = "none",
    "Canonical scientific name (LLM)" = "llm_name")
}

#' Normalise a vector of values before matching
#'
#' Normalisation is a **pre-matching** step, run per value and cached, because
#' fastLink cannot see through semantic equivalence with low string similarity:
#' `"Myotis lucifugus"` against `"little brown bat"`, or a binomial against its
#' abbreviation. Left un-normalised, the matcher systematically fails to pair
#' records that obviously correspond, on exactly the fields it depends on most.
#'
#' Plain fuzzy needs no pre-step -- fastLink already does string-distance
#' comparison internally, so `"Myotis lucifugus"` against `"Myotis lucifigus"`
#' is scored as agreement without help.
#'
#' `"llm_name"` uses `ecoreview::standardize_name_vector()` when ecoreview is
#' installed, and falls back to a direct ellmer call otherwise.
#'
#' **Coupling note:** `standardize_name_vector()` is the only function ecoeval
#' reuses directly from ecoreview. A signature change there breaks this.
#'
#' @param x A character vector of values.
#' @param normalizer A normaliser id; see [normalizer_choices()].
#' @param cache An environment used to memoise per value across calls.
#' @param model Model identifier for the fallback path.
#'
#' @return A character vector the same length as `x`. Unchanged when the
#'   normaliser is `"none"` or no LLM is available.
#' @export
normalize_values <- function(x, normalizer = "none", cache = NULL,
                             model = default_judge_model()) {
  x <- as.character(x)
  if (identical(normalizer, "none") || !length(x)) return(x)
  if (!judge_available()) return(x)

  todo <- unique(x[!is_blank(x)])
  if (!is.null(cache)) todo <- todo[!vapply(todo, function(v) {
    !is.null(cache[[normalize_key(normalizer, v)]])
  }, logical(1))]

  if (length(todo)) {
    canon <- tryCatch(
      normalize_batch(todo, normalizer, model),
      error = function(e) stats::setNames(todo, todo)
    )
    if (!is.null(cache)) {
      for (v in names(canon)) assign(normalize_key(normalizer, v), canon[[v]], envir = cache)
    }
  }

  vapply(x, function(v) {
    if (isTRUE(is_blank(v))) return(v)
    hit <- if (!is.null(cache)) cache[[normalize_key(normalizer, v)]] else NULL
    as.character(hit %||% v)
  }, character(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
normalize_key <- function(normalizer, value) paste(normalizer, value, sep = " <|> ")

#' @keywords internal
#' @noRd
normalize_batch <- function(values, normalizer, model) {
  if (identical(normalizer, "llm_name") &&
      requireNamespace("ecoreview", quietly = TRUE)) {
    fn <- tryCatch(getExportedValue("ecoreview", "standardize_name_vector"),
                   error = function(e) NULL)
    if (is.function(fn)) {
      out <- tryCatch(as.character(fn(values)), error = function(e) NULL)
      if (!is.null(out) && length(out) == length(values)) {
        return(stats::setNames(out, values))
      }
    }
  }
  chat <- ellmer::chat_anthropic(
    system_prompt = paste0(
      "You canonicalise biological names. For each input, return the accepted ",
      "scientific binomial in Genus species form. Resolve common names, ",
      "abbreviations, misspellings, and superseded synonyms. If an input is ",
      "not a taxon name, return it unchanged."
    ),
    model = model, echo = "none"
  )
  type <- ellmer::type_object(
    names = ellmer::type_array(
      ellmer::type_string(),
      description = "Canonical form of each input, in the same order."
    )
  )
  res <- chat$chat_structured(
    paste0("Canonicalise these, one per line, in order:\n",
           paste(values, collapse = "\n")),
    type = type
  )
  out <- as.character(res$names)
  if (length(out) != length(values)) return(stats::setNames(values, values))
  stats::setNames(out, values)
}

#' How many judge calls a run would cost
#'
#' Shown before the batch action runs, because an operation that spends money
#' should say how much before it starts.
#'
#' @param cells A cell tibble scored without a judge.
#' @return An integer count of distinct value pairs still awaiting a verdict.
#' @export
estimate_judge_calls <- function(cells) {
  if (!nrow(cells)) return(0L)
  pend <- cells[cells$pending, , drop = FALSE]
  if (!nrow(pend)) return(0L)
  length(unique(pair_key(pend$field, pend$ai_value, pend$gold_value)))
}

#' A fresh verdict cache
#'
#' LLM output varies between calls, so a reproducible run needs verdicts frozen
#' rather than re-derived. The cache is serialised into `run_config.json`.
#'
#' @param verdicts A named list of cached verdicts to seed with.
#' @return An environment.
#' @export
new_cache <- function(verdicts = list()) {
  e <- new.env(parent = emptyenv())
  for (nm in names(verdicts)) assign(nm, verdicts[[nm]], envir = e)
  e
}

#' Serialise a cache to a plain list
#'
#' @param cache An environment from [new_cache()].
#' @return A named list.
#' @export
cache_as_list <- function(cache) {
  if (is.null(cache)) return(list())
  as.list(cache, all.names = TRUE)
}
