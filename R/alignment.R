# Record and paper alignment.
#
# fastLink is the matcher, not the reporter: we use the linkage algorithm and
# the per-link posterior probabilities that drive sort-by-confidence in review,
# and we do not surface its diagnostics. Blocking is per paper, and the matcher
# is deliberately permissive -- no similarity floor -- because dropping a pair
# because the identifiers look wrong is precisely the decision the human should
# be making with the values in front of them.

#' Similarity matrix between two record sets
#'
#' Mean string similarity across the linkage fields, used to sort candidate
#' pairs and as the fallback matcher when fastLink's EM cannot run.
#'
#' @param a,b Record tibbles.
#' @param fields Linkage fields present in both.
#' @return A numeric matrix, `nrow(a)` by `nrow(b)`.
#' @keywords internal
#' @noRd
pair_score_matrix <- function(a, b, fields) {
  m <- matrix(0, nrow = nrow(a), ncol = nrow(b))
  fields <- intersect(fields, intersect(names(a), names(b)))
  if (!length(fields) || !nrow(a) || !nrow(b)) return(m)
  for (f in fields) {
    av <- vapply(seq_len(nrow(a)), function(i) as_scalar_chr(cell_value(a, i, f)), character(1))
    bv <- vapply(seq_len(nrow(b)), function(i) as_scalar_chr(cell_value(b, i, f)), character(1))
    s <- outer(av, bv, function(x, y) similarity(x, y))
    s[is.na(s)] <- 0
    m <- m + s
  }
  m / length(fields)
}

#' Greedily take a 1:1 assignment from a score matrix
#'
#' Matching is 1:1 and leftovers are false positives and false negatives,
#' because that is what they are. Greedy on descending score; ties resolve by
#' row order, which keeps the result deterministic.
#'
#' @param m A score matrix.
#' @param floor_score Minimum score to propose a pair. Zero by default -- the
#'   matcher proposes even when identifiers disagree, and the human judges.
#' @return A tibble with `i`, `j`, `score`.
#' @keywords internal
#' @noRd
greedy_assign <- function(m, floor_score = 0) {
  out <- list()
  if (!length(m)) return(empty_tbl(i = integer(), j = integer(), score = numeric()))
  used_i <- logical(nrow(m))
  used_j <- logical(ncol(m))
  ord <- order(-as.vector(m), seq_along(m))
  for (idx in ord) {
    i <- ((idx - 1L) %% nrow(m)) + 1L
    j <- ((idx - 1L) %/% nrow(m)) + 1L
    if (used_i[i] || used_j[j]) next
    if (m[i, j] < floor_score) next
    used_i[i] <- TRUE
    used_j[j] <- TRUE
    out[[length(out) + 1L]] <- c(i = i, j = j, score = m[i, j])
    if (all(used_i) || all(used_j)) break
  }
  if (!length(out)) return(empty_tbl(i = integer(), j = integer(), score = numeric()))
  d <- as.data.frame(do.call(rbind, out))
  tibble::tibble(i = as.integer(d$i), j = as.integer(d$j), score = as.numeric(d$score))
}

#' The seed the matcher runs under
#'
#' fastLink clusters string-distance values internally, and that clustering is
#' randomly initialised -- so two identical calls can return different
#' pairings. A run that cannot be reproduced is not much use as evidence, and
#' `run_config.json` promises exactly that reproducibility, so ecoeval pins the
#' seed for every call into fastLink.
#'
#' Set `options(ecoeval.seed = )` to change it.
#'
#' @return An integer seed.
#' @export
ecoeval_seed <- function() {
  as.integer(getOption("ecoeval.seed", 20250831L))
}

#' Evaluate an expression under a fixed seed, leaving the caller's RNG alone
#'
#' Seeding globally would silently change the results of anything else the user
#' is doing in the same session, so the previous RNG state is put back.
#'
#' @param seed Integer seed.
#' @param f A function of no arguments.
#' @return The value of `f()`.
#' @keywords internal
#' @noRd
with_local_seed <- function(seed, f) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }
  set.seed(seed)
  f()
}

#' Fit the linkage model once over the whole corpus
#'
#' fastLink's EM learns which fields discriminate from the data it is given.
#' Per-paper blocks hold a handful of records each -- far too few for that, and
#' on two records the EM will confidently pair the wrong ones. Fitting once
#' over every scoped record and then *applying* that model per paper is the
#' correct division of labour: pooled estimation, blocked application.
#'
#' @param ai,gold Record tibbles.
#' @param fields Linkage fields.
#' @param seed Seed for fastLink's internal clustering; see [ecoeval_seed()].
#' @return A `fastLink.EM` object, or `NULL` when the model cannot be fit --
#'   in which case the similarity matcher takes over.
#' @export
fit_linkage_model <- function(ai, gold, fields, seed = ecoeval_seed()) {
  fields <- intersect(fields, intersect(names(ai), names(gold)))
  if (!length(fields) || nrow(ai) < 2L || nrow(gold) < 2L) return(NULL)
  dfa <- linkage_frame(ai, fields)
  dfb <- linkage_frame(gold, fields)
  with_local_seed(seed, function() {
    quietly(function() {
      tryCatch(
        fastLink::fastLink(
          dfA = dfa, dfB = dfb, varnames = fields,
          stringdist.match = fields,
          estimate.only = TRUE, verbose = FALSE, n.cores = 1L
        ),
        error = function(e) NULL
      )
    })
  })
}

#' Run an expression with fastLink's console chatter suppressed
#'
#' fastLink reports progress with `cat()`, which `suppressMessages()` does not
#' catch and which would otherwise flood a Shiny console on every paper.
#'
#' @param f A function of no arguments.
#' @return The value of `f()`.
#' @keywords internal
#' @noRd
quietly <- function(f) {
  out <- NULL
  utils::capture.output(
    suppressWarnings(suppressMessages(out <- f())),
    file = nullfile()
  )
  out
}

#' Character data frame of the linkage fields
#'
#' @param df A record tibble.
#' @param fields Linkage fields.
#' @return A base data frame of character columns.
#' @keywords internal
#' @noRd
linkage_frame <- function(df, fields) {
  out <- as.data.frame(
    lapply(fields, function(f) {
      v <- vapply(seq_len(nrow(df)), function(i) as_scalar_chr(cell_value(df, i, f)),
                  character(1))
      v[is_blank(v)] <- NA_character_
      v
    }),
    stringsAsFactors = FALSE
  )
  names(out) <- fields
  out
}

#' Link two record sets within one block
#'
#' Applies the pooled model to a single paper. fastLink's EM needs enough
#' records and enough variation to converge; when no model could be fit, or
#' when applying it to this block fails, the similarity matcher does the work
#' instead. That is not an error condition -- it is the cheap matcher earning
#' its keep on a two-row paper.
#'
#' @param a,b Record tibbles for a single block.
#' @param fields Linkage fields.
#' @param em A model from [fit_linkage_model()], or `NULL`.
#' @param seed Seed for fastLink's internal clustering.
#' @return A tibble with `i`, `j`, `posterior`, `matcher`.
#' @keywords internal
#' @noRd
link_block <- function(a, b, fields, em = NULL, seed = ecoeval_seed()) {
  fields <- intersect(fields, intersect(names(a), names(b)))
  scores <- pair_score_matrix(a, b, fields)
  fallback <- function() {
    g <- greedy_assign(scores)
    g$posterior <- g$score
    g$matcher <- "similarity"
    g[, c("i", "j", "posterior", "matcher")]
  }
  if (!length(fields) || is.null(em) || !nrow(a) || !nrow(b)) return(fallback())

  dfa <- linkage_frame(a, fields)
  dfb <- linkage_frame(b, fields)
  out <- with_local_seed(seed, function() {
    quietly(function() {
      tryCatch(
        fastLink::fastLink(
          dfA = dfa, dfB = dfb, varnames = fields,
          stringdist.match = fields, em.obj = em,
          # No similarity floor: propose everything, let the human judge.
          threshold.match = 1e-6,
          dedupe.matches = TRUE, verbose = FALSE, n.cores = 1L
        ),
        error = function(e) NULL
      )
    })
  })
  # fastLink returns an empty list rather than a zero-row frame when the model
  # likes nothing in this block, so check the shape before reading it.
  if (is.null(out) || !is.data.frame(out$matches) || !nrow(out$matches)) {
    return(fallback())
  }

  res <- tibble::tibble(
    i = as.integer(out$matches$inds.a),
    j = as.integer(out$matches$inds.b),
    posterior = as.numeric(out$posterior %||% rep(NA_real_, nrow(out$matches))),
    matcher = "fastLink"
  )
  res <- enforce_one_to_one(res)

  # fastLink stops at the pairs its model likes; sweep up the rest greedily so
  # every record gets a proposal to accept or reject.
  left_i <- setdiff(seq_len(nrow(a)), res$i)
  left_j <- setdiff(seq_len(nrow(b)), res$j)
  if (length(left_i) && length(left_j)) {
    sub <- greedy_assign(scores[left_i, left_j, drop = FALSE])
    if (nrow(sub)) {
      res <- dplyr::bind_rows(res, tibble::tibble(
        i = left_i[sub$i], j = left_j[sub$j],
        posterior = sub$score, matcher = "similarity"
      ))
    }
  }
  res
}

#' Drop links that would make the assignment many-to-one
#'
#' @param res A tibble with `i`, `j`, `posterior`.
#' @return `res` reduced to a 1:1 assignment, highest posterior kept.
#' @keywords internal
#' @noRd
enforce_one_to_one <- function(res) {
  if (!nrow(res)) return(res)
  res <- res[order(-dplyr::coalesce(res$posterior, 0)), , drop = FALSE]
  keep <- !duplicated(res$i) & !duplicated(res$j)
  res[keep, , drop = FALSE]
}

#' Align records between the two sources
#'
#' Blocked per paper, 1:1, permissive. Every record from both sides appears in
#' the result: some rows are pairs, some are AI-only, some are gold-only.
#'
#' @param ai,gold Record tibbles with `.rid` and `.paper` columns.
#' @param linkage_fields Character vector of fields to link on.
#' @param paper_map A tibble with `ai_paper` and `gold_paper` defining scope.
#'   When `NULL`, papers are matched on identical `.paper` values.
#' @param rejected A tibble of human-rejected links with `ai_rid`, `gold_rid`.
#' @param added A tibble of human-added links with `ai_rid`, `gold_rid`. These
#'   win over any automatic link that conflicts with them.
#' @param seed Seed for the matcher; see [ecoeval_seed()]. Pinned by default so
#'   two runs of the same configuration produce the same pairings.
#'
#' @return A tibble with `pair_id`, `paper`, `ai_rid`, `gold_rid`, `posterior`,
#'   `matcher`, and `link_source` (`"auto"`, `"manual"`, or `"unpaired"`),
#'   ordered pairs first, then AI-only, then gold-only.
#' @export
align_records <- function(ai, gold, linkage_fields,
                          paper_map = NULL, rejected = NULL, added = NULL,
                          seed = ecoeval_seed()) {
  if (is.null(paper_map)) {
    common <- intersect(unique(ai$.paper), unique(gold$.paper))
    paper_map <- tibble::tibble(ai_paper = common, gold_paper = common)
  }
  if (!nrow(paper_map)) return(empty_pairs())

  in_ai <- ai[ai$.paper %in% paper_map$ai_paper, , drop = FALSE]
  in_gold <- gold[gold$.paper %in% paper_map$gold_paper, , drop = FALSE]
  em <- fit_linkage_model(in_ai, in_gold, linkage_fields, seed)

  links <- list()
  for (p in seq_len(nrow(paper_map))) {
    a <- ai[ai$.paper == paper_map$ai_paper[[p]], , drop = FALSE]
    b <- gold[gold$.paper == paper_map$gold_paper[[p]], , drop = FALSE]
    if (!nrow(a) || !nrow(b)) next
    res <- link_block(a, b, linkage_fields, em, seed)
    if (!nrow(res)) next
    links[[length(links) + 1L]] <- tibble::tibble(
      paper = paper_map$ai_paper[[p]],
      ai_rid = a$.rid[res$i],
      gold_rid = b$.rid[res$j],
      posterior = res$posterior,
      matcher = res$matcher,
      link_source = "auto"
    )
  }
  links <- if (length(links)) dplyr::bind_rows(links) else empty_links()

  links <- drop_links(links, rejected)
  links <- add_links(links, added, ai, gold)

  assemble_pairs(links, ai, gold, paper_map)
}

#' @keywords internal
#' @noRd
drop_links <- function(links, rejected) {
  if (is.null(rejected) || !nrow(rejected) || !nrow(links)) return(links)
  key <- paste(links$ai_rid, links$gold_rid)
  links[!key %in% paste(rejected$ai_rid, rejected$gold_rid), , drop = FALSE]
}

#' @keywords internal
#' @noRd
add_links <- function(links, added, ai, gold) {
  if (is.null(added) || !nrow(added)) return(links)
  # A manual link is the human's decision; anything it conflicts with goes.
  links <- links[!links$ai_rid %in% added$ai_rid &
                   !links$gold_rid %in% added$gold_rid, , drop = FALSE]
  papers <- ai$.paper[match(added$ai_rid, ai$.rid)]
  dplyr::bind_rows(links, tibble::tibble(
    paper = papers,
    ai_rid = as.character(added$ai_rid),
    gold_rid = as.character(added$gold_rid),
    posterior = NA_real_,
    matcher = "manual",
    link_source = "manual"
  ))
}

#' Expand a link table into the full pair-and-orphan layout
#'
#' @param links Accepted links.
#' @param ai,gold Record tibbles.
#' @param paper_map Scope.
#' @return The pair tibble described in [align_records()].
#' @keywords internal
#' @noRd
assemble_pairs <- function(links, ai, gold, paper_map) {
  ai_in <- ai[ai$.paper %in% paper_map$ai_paper, , drop = FALSE]
  gold_in <- gold[gold$.paper %in% paper_map$gold_paper, , drop = FALSE]
  gold_paper_of_ai <- stats::setNames(paper_map$ai_paper, paper_map$gold_paper)

  paired <- if (nrow(links)) {
    tibble::tibble(
      paper = links$paper,
      ai_rid = links$ai_rid,
      gold_rid = links$gold_rid,
      posterior = links$posterior,
      matcher = links$matcher,
      link_source = links$link_source
    )
  } else empty_links()[, c("paper", "ai_rid", "gold_rid", "posterior", "matcher", "link_source")]

  ai_only <- ai_in[!ai_in$.rid %in% paired$ai_rid, , drop = FALSE]
  gold_only <- gold_in[!gold_in$.rid %in% paired$gold_rid, , drop = FALSE]

  orphans <- dplyr::bind_rows(
    tibble::tibble(
      paper = ai_only$.paper, ai_rid = ai_only$.rid, gold_rid = NA_character_,
      posterior = NA_real_, matcher = NA_character_, link_source = "unpaired"
    ),
    tibble::tibble(
      paper = unname(gold_paper_of_ai[gold_only$.paper]),
      ai_rid = NA_character_, gold_rid = gold_only$.rid,
      posterior = NA_real_, matcher = NA_character_, link_source = "unpaired"
    )
  )

  out <- dplyr::bind_rows(paired, orphans)
  if (!nrow(out)) return(empty_pairs())
  out$kind <- ifelse(!is.na(out$ai_rid) & !is.na(out$gold_rid), "pair",
                     ifelse(is.na(out$gold_rid), "ai_only", "gold_only"))
  out <- out[order(match(out$kind, c("pair", "ai_only", "gold_only")),
                   -dplyr::coalesce(out$posterior, -1)), , drop = FALSE]
  out$pair_id <- sprintf("p%04d", seq_len(nrow(out)))
  out[, c("pair_id", "paper", "ai_rid", "gold_rid", "posterior", "matcher",
          "link_source", "kind")]
}

#' @keywords internal
#' @noRd
empty_links <- function() {
  empty_tbl(paper = character(), ai_rid = character(), gold_rid = character(),
            posterior = numeric(), matcher = character(), link_source = character())
}

#' @keywords internal
#' @noRd
empty_pairs <- function() {
  empty_tbl(pair_id = character(), paper = character(), ai_rid = character(),
            gold_rid = character(), posterior = numeric(), matcher = character(),
            link_source = character(), kind = character())
}

#' Align the two paper lists
#'
#' Paper alignment determines scope, not score. Its asymmetry is worth knowing:
#' a missed paper match just shrinks the evaluation set, but a wrong paper
#' match compares one paper's records against another's and produces garbage.
#'
#' @param ai_papers,gold_papers Tibbles of paper metadata, each with a `.paper`
#'   identifier column.
#' @param fields Metadata fields to link on -- DOI, title, author, year.
#' @param min_posterior Links below this confidence are returned with
#'   `accepted = FALSE` for the human to confirm.
#' @param seed Seed for the matcher; see [ecoeval_seed()].
#'
#' @return A tibble with `ai_paper`, `gold_paper`, `posterior`, `matcher`,
#'   `accepted`.
#' @export
align_papers <- function(ai_papers, gold_papers, fields, min_posterior = 0.85,
                         seed = ecoeval_seed()) {
  fields <- intersect(fields, intersect(names(ai_papers), names(gold_papers)))
  if (!nrow(ai_papers) || !nrow(gold_papers)) {
    return(empty_tbl(ai_paper = character(), gold_paper = character(),
                     posterior = numeric(), matcher = character(),
                     accepted = logical()))
  }
  if (!length(fields)) {
    # Nothing to link on but the identifiers themselves.
    common <- intersect(ai_papers$.paper, gold_papers$.paper)
    return(tibble::tibble(ai_paper = common, gold_paper = common,
                          posterior = 1, matcher = "identifier",
                          accepted = TRUE))
  }
  em <- fit_linkage_model(ai_papers, gold_papers, fields, seed)
  res <- link_block(ai_papers, gold_papers, fields, em, seed)
  if (!nrow(res)) {
    return(empty_tbl(ai_paper = character(), gold_paper = character(),
                     posterior = numeric(), matcher = character(),
                     accepted = logical()))
  }
  tibble::tibble(
    ai_paper = ai_papers$.paper[res$i],
    gold_paper = gold_papers$.paper[res$j],
    posterior = res$posterior,
    matcher = res$matcher,
    accepted = dplyr::coalesce(res$posterior, 0) >= min_posterior
  )
}

#' Granularity check on the chosen linkage fields
#'
#' Group records by the linkage fields within each paper and count collapses.
#' If many rows collapse together the two datasets disagree about what a record
#' *is* -- the human distinguished records by something the key doesn't
#' capture. This runs on normalised values, since `"M. lucifugus"` and
#' `"Myotis lucifugus"` should collapse together and running it raw undercounts.
#'
#' @param records A record tibble with `.paper`.
#' @param linkage_fields The chosen linkage fields.
#' @param source Label for the report.
#'
#' @return A tibble with `source`, `paper`, `key`, `n` for every group holding
#'   more than one record.
#' @export
granularity_check <- function(records, linkage_fields, source = "gold") {
  fields <- intersect(linkage_fields, names(records))
  if (!nrow(records) || !length(fields)) {
    return(empty_tbl(source = character(), paper = character(),
                     key = character(), n = integer()))
  }
  key <- apply(
    vapply(fields, function(f) {
      canonicalise(vapply(seq_len(nrow(records)), function(i) {
        as_scalar_chr(cell_value(records, i, f))
      }, character(1)))
    }, character(nrow(records))),
    1, paste, collapse = " | "
  )
  counts <- dplyr::count(
    tibble::tibble(source = source, paper = records$.paper, key = key),
    .data$source, .data$paper, .data$key, name = "n"
  )
  dplyr::arrange(dplyr::filter(counts, .data$n > 1L), dplyr::desc(.data$n))
}

#' Records indistinguishable from each other on the linkage fields
#'
#' Our analogue of Splink's unlinkables chart: "5 records in this paper are
#' indistinguishable on your chosen linkage fields."
#'
#' @inheritParams granularity_check
#' @return A tibble with `source`, `paper`, `n_indistinguishable`.
#' @export
identifiability_check <- function(records, linkage_fields, source = "ai") {
  g <- granularity_check(records, linkage_fields, source)
  if (!nrow(g)) {
    return(empty_tbl(source = character(), paper = character(),
                     n_indistinguishable = integer()))
  }
  dplyr::summarise(
    dplyr::group_by(g, .data$source, .data$paper),
    n_indistinguishable = sum(.data$n),
    .groups = "drop"
  )
}
