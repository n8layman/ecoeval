# Record and paper alignment.
#
# A Fellegi-Sunter matcher, blocked per paper. Each identity column of a
# candidate pair is scored agree / close / differ by string similarity, and the
# model says how much each outcome on each column counts for or against the two
# rows being the same record. A pair is linked only when the model thinks that
# more likely than not. Gold standards hold rows the extraction missed, and
# extractions hold rows the gold standard does not: pairing those with
# whatever else is left in the paper would hide a missed or extra record inside
# a "disagreement". What the model leaves apart, the human can link; what it
# joins wrongly, the human can reject.
#
# The model is fitted once over every in-scope record, and its two halves are
# learned from different pairs because each is learnable from different data:
#
# * how often two *different* records agree by chance (u) -- from pairs in
#   different papers, which are certainly not the same record;
# * how often the *same* record agrees with itself (m), and how many of the
#   within-paper pairs are the same record -- by EM over the within-paper
#   pairs, held near a prior so that a small corpus cannot talk the model into
#   nonsense. A paper holds a handful of records, far too few to learn from on
#   its own, so the model is pooled and applied per paper.
#
# This replaces fastLink, which ecoeval used to call once per paper. Each call
# cost most of a second in fixed overhead, and applying a fitted model to a
# block looked its probabilities up by the order the agreement levels happened
# to appear in that block, so a block whose first pair agreed was scored as if
# it disagreed.

#' Agreement levels, in order of increasing evidence for a match
#' @keywords internal
#' @noRd
LINK_LEVELS <- c("differ", "close", "agree")

#' Similarity at or above which two values are close, and agree
#'
#' On the same scale as [similarity()], with the cut points fastLink uses.
#' @keywords internal
#' @noRd
LINK_CUTS <- c(close = 0.88, agree = 0.94)

#' Prior beliefs, each worth `LINK_PRIOR_WEIGHT` pairs of evidence
#'
#' The same record mostly agrees with itself on its identity columns; two
#' different records mostly do not. The data moves these as soon as there is
#' enough of it.
#' @keywords internal
#' @noRd
LINK_M_PRIOR <- c(differ = 0.05, close = 0.10, agree = 0.85)
LINK_U_PRIOR <- c(differ = 0.85, close = 0.05, agree = 0.10)
LINK_PRIOR_WEIGHT <- 10

#' The agreement level of each pair of values
#'
#' @param a,b Character vectors of the same length; `NA` is missing.
#' @return An integer vector indexing [LINK_LEVELS], `NA` where either value
#'   is missing -- a missing value is no evidence either way.
#' @keywords internal
#' @noRd
agreement_level <- function(a, b) {
  out <- rep(NA_integer_, length(a))
  ok <- !is.na(a) & !is.na(b)
  if (!any(ok)) return(out)
  # Records repeat their identity values; score each distinct pair once.
  key <- paste(a[ok], b[ok], sep = "\r")
  first <- !duplicated(key)
  s <- similarity(a[ok][first], b[ok][first])
  lvl <- 1L + (s >= LINK_CUTS[["close"]]) + (s >= LINK_CUTS[["agree"]])
  out[ok] <- as.integer(lvl[match(key, key[first])])
  out
}

#' Character data frame of the linkage fields
#'
#' @param df A record tibble.
#' @param fields Linkage fields.
#' @return A base data frame of character columns, `NA` where blank.
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

#' Every within-paper pair of records
#'
#' @param ai,gold Record tibbles with `.paper`.
#' @param paper_map A tibble with `ai_paper` and `gold_paper`.
#' @return A data frame with `block` (the `paper_map` row), `i`, and `j` (rows
#'   of `ai` and `gold`).
#' @keywords internal
#' @noRd
candidate_pairs <- function(ai, gold, paper_map) {
  ai_rows <- split(seq_len(nrow(ai)), ai$.paper)
  gold_rows <- split(seq_len(nrow(gold)), gold$.paper)
  blocks <- lapply(seq_len(nrow(paper_map)), function(p) {
    a <- paper_map$ai_paper[[p]]
    b <- paper_map$gold_paper[[p]]
    if (is.na(a) || is.na(b)) return(NULL)
    i <- ai_rows[[a]]
    j <- gold_rows[[b]]
    if (!length(i) || !length(j)) return(NULL)
    data.frame(block = p, i = rep(i, times = length(j)),
               j = rep(j, each = length(i)))
  })
  blocks <- blocks[!vapply(blocks, is.null, logical(1))]
  if (!length(blocks)) return(data.frame(block = integer(), i = integer(), j = integer()))
  do.call(rbind, blocks)
}

#' How often two record sets agree on a field by chance, over all pairs
#'
#' Counted over distinct values rather than pairs of records, so the cost is
#' the number of distinct values squared, not the number of records. Past
#' `max_values` distinct values on a side, an evenly spaced subset estimates
#' the proportions.
#'
#' @param a,b Character vectors of one field's values; `NA` is missing.
#' @return Counts by level, scaled to all non-missing pairs.
#' @keywords internal
#' @noRd
chance_agreement <- function(a, b, max_values = 2000L) {
  ta <- table(a[!is.na(a)])
  tb <- table(b[!is.na(b)])
  out <- stats::setNames(numeric(length(LINK_LEVELS)), LINK_LEVELS)
  if (!length(ta) || !length(tb)) return(out)
  thin <- function(t) {
    if (length(t) <= max_values) return(t)
    t[unique(round(seq(1, length(t), length.out = max_values)))]
  }
  sa <- thin(ta)
  sb <- thin(tb)
  s <- 1 - stringdist::stringdistmatrix(canonicalise(names(sa)),
                                        canonicalise(names(sb)), method = "jw")
  lvl <- 1L + (s >= LINK_CUTS[["close"]]) + (s >= LINK_CUTS[["agree"]])
  w <- outer(as.numeric(sa), as.numeric(sb))
  share <- vapply(seq_along(LINK_LEVELS), function(l) sum(w[lvl == l]), numeric(1))
  out[] <- share / sum(w) * sum(ta) * sum(tb)
  out
}

#' A pair's log odds of being the same record
#'
#' @param G An integer matrix of agreement levels, one column per field.
#' @param model An `ecoeval_linkage_model`.
#' @return A numeric vector, one per row of `G`.
#' @keywords internal
#' @noRd
match_log_odds <- function(G, model) {
  lo <- rep(stats::qlogis(model$lambda), nrow(G))
  for (k in seq_along(model$fields)) {
    w <- log(model$m[[k]]) - log(model$u[[k]])
    g <- G[, k]
    ok <- !is.na(g)
    lo[ok] <- lo[ok] + w[g[ok]]
  }
  lo
}

#' Estimate the linkage model from the candidate pairs
#'
#' @param av,bv Linkage frames of every in-scope record on each side.
#' @param G Agreement levels of the candidate pairs.
#' @param cand The candidate pairs.
#' @return An `ecoeval_linkage_model`.
#' @keywords internal
#' @noRd
estimate_linkage_model <- function(av, bv, G, cand, max_iter = 500L) {
  fields <- names(av)
  K <- length(fields)
  prior_m <- LINK_PRIOR_WEIGHT * LINK_M_PRIOR
  prior_u <- LINK_PRIOR_WEIGHT * LINK_U_PRIOR

  # u: chance agreement among pairs that cannot be the same record. With too
  # few of those -- one paper, or paper alignment, where everything is one
  # block -- u is learned by the EM along with m instead.
  learn_u <- logical(K)
  u <- lapply(seq_len(K), function(k) {
    all_pairs <- chance_agreement(av[[k]], bv[[k]])
    within <- tabulate(G[, k], nbins = length(LINK_LEVELS))
    across <- pmax(all_pairs - within, 0)
    if (sum(across) < sum(within)) {
      learn_u[[k]] <<- TRUE
      across <- all_pairs
    }
    stats::setNames((across + prior_u) / (sum(across) + LINK_PRIOR_WEIGHT), LINK_LEVELS)
  })

  # EM over the distinct agreement patterns, weighted by how often each occurs.
  code <- as.vector(ifelse(is.na(G), 0L, G) %*% (4^(seq_len(K) - 1L)))
  first <- !duplicated(code)
  P <- G[first, , drop = FALSE]
  n <- tabulate(match(code, code[first]), nbins = sum(first))

  # Start from every record that could have a partner having one.
  per_block <- split(cand, cand$block)
  could <- sum(vapply(per_block, function(b) {
    min(length(unique(b$i)), length(unique(b$j)))
  }, numeric(1)))
  clamp <- function(x) min(max(x, 1e-4), 1 - 1e-4)
  model <- structure(
    list(fields = fields,
         m = rep(list(stats::setNames(LINK_M_PRIOR, LINK_LEVELS)), K),
         u = u, lambda = clamp(could / nrow(cand)), n_pairs = nrow(cand)),
    class = "ecoeval_linkage_model"
  )
  level_sums <- function(w, k) {
    vapply(seq_along(LINK_LEVELS), function(l) sum(w[which(P[, k] == l)]), numeric(1))
  }
  for (iter in seq_len(max_iter)) {
    z <- stats::plogis(match_log_odds(P, model)) * n
    m <- lapply(seq_len(K), function(k) {
      w <- level_sums(z, k)
      stats::setNames((w + prior_m) / (sum(w) + LINK_PRIOR_WEIGHT), LINK_LEVELS)
    })
    u <- lapply(seq_len(K), function(k) {
      if (!learn_u[[k]]) return(model$u[[k]])
      w <- level_sums(n - z, k)
      stats::setNames((w + prior_u) / (sum(w) + LINK_PRIOR_WEIGHT), LINK_LEVELS)
    })
    lambda <- clamp(sum(z) / sum(n))
    change <- max(abs(lambda - model$lambda), abs(unlist(m) - unlist(model$m)),
                  abs(unlist(u) - unlist(model$u)))
    model$m <- m
    model$u <- u
    model$lambda <- lambda
    if (change < 1e-8) break
  }
  names(model$m) <- names(model$u) <- fields
  model
}

#' Score candidate pairs with a pooled linkage model
#'
#' @param a,b Record tibbles; `cand` indexes their rows.
#' @param fields Linkage fields.
#' @param cand Candidate pairs from [candidate_pairs()].
#' @return `cand` with `posterior`, the probability each pair is the same
#'   record (`NA` when there was nothing to link on), and the model as the
#'   `"model"` attribute.
#' @keywords internal
#' @noRd
score_candidates <- function(a, b, fields, cand) {
  fields <- intersect(fields, intersect(names(a), names(b)))
  cand$posterior <- rep(NA_real_, nrow(cand))
  if (!length(fields) || !nrow(cand)) return(cand)
  av <- linkage_frame(a, fields)
  bv <- linkage_frame(b, fields)
  G <- matrix(
    unlist(lapply(fields, function(f) agreement_level(av[[f]][cand$i], bv[[f]][cand$j]))),
    nrow = nrow(cand), dimnames = list(NULL, fields)
  )
  model <- estimate_linkage_model(av, bv, G, cand)
  cand$posterior <- stats::plogis(match_log_odds(G, model))
  attr(cand, "model") <- model
  cand
}

#' Fit the record linkage model
#'
#' The model [align_records()] links with, for inspecting what it learned: for
#' each identity column, how strongly agreeing on it -- or not -- counts
#' towards two rows being the same record.
#'
#' @param ai,gold Record tibbles with `.paper`.
#' @param fields Linkage fields.
#' @param paper_map Which papers' records may pair; as in [align_records()].
#' @return An `ecoeval_linkage_model`: per-field agreement probabilities for
#'   the same record (`m`) and for different records (`u`), and `lambda`, the
#'   share of within-paper pairs that are the same record. `NULL` when there
#'   is nothing to link on.
#' @examples
#' ai <- tibble::tibble(.paper = c("P1", "P1", "P2"),
#'                      sp = c("Myotis lucifugus", "Eptesicus fuscus", "Lasiurus borealis"))
#' gold <- tibble::tibble(.paper = c("P1", "P2", "P2"),
#'                        sp = c("Myotis lucifigus", "Lasiurus borealis", "Lasiurus cinereus"))
#' fit_linkage_model(ai, gold, "sp")
#' @export
fit_linkage_model <- function(ai, gold, fields, paper_map = NULL) {
  paper_map <- paper_map %||% common_paper_map(ai, gold)
  ai <- ai[ai$.paper %in% paper_map$ai_paper, , drop = FALSE]
  gold <- gold[gold$.paper %in% paper_map$gold_paper, , drop = FALSE]
  attr(score_candidates(ai, gold, fields, candidate_pairs(ai, gold, paper_map)), "model")
}

#' @export
print.ecoeval_linkage_model <- function(x, ...) {
  cat(sprintf("<linkage model> %d within-paper pairs, %.0f%% estimated to be the same record\n",
              x$n_pairs, 100 * x$lambda))
  w <- vapply(seq_along(x$fields), function(k) log(x$m[[k]]) - log(x$u[[k]]),
              numeric(length(LINK_LEVELS)))
  w <- matrix(w, ncol = length(x$fields), dimnames = list(LINK_LEVELS, x$fields))
  cat("Evidence for a match (log odds) by agreement on each field:\n")
  print(round(t(w), 2))
  invisible(x)
}

#' Papers that share an identifier
#' @keywords internal
#' @noRd
common_paper_map <- function(ai, gold) {
  common <- intersect(unique(ai$.paper), unique(gold$.paper))
  tibble::tibble(ai_paper = common, gold_paper = common)
}

#' Keep a 1:1 set of pairs, most probable first
#'
#' Greedy on descending posterior; ties resolve by row order, which keeps the
#' result deterministic.
#'
#' @param links A data frame with `i`, `j`, `posterior`.
#' @return `links` reduced to a 1:1 assignment, in order of posterior.
#' @keywords internal
#' @noRd
one_to_one <- function(links) {
  if (!nrow(links)) return(links)
  links <- links[order(-dplyr::coalesce(links$posterior, -1), links$i, links$j), ,
                 drop = FALSE]
  used_i <- logical(max(links$i))
  used_j <- logical(max(links$j))
  keep <- logical(nrow(links))
  for (r in seq_len(nrow(links))) {
    i <- links$i[[r]]
    j <- links$j[[r]]
    if (used_i[[i]] || used_j[[j]]) next
    used_i[[i]] <- TRUE
    used_j[[j]] <- TRUE
    keep[[r]] <- TRUE
  }
  links[keep, , drop = FALSE]
}

#' Align records between the two sources
#'
#' Blocked per paper and 1:1. A pair is linked only when the linkage model
#' (see [fit_linkage_model()]) puts the chance that the two rows are the same
#' record at `min_posterior` or more; everything else stays unpaired, as the
#' record one side has and the other does not. Every record from both sides
#' appears in the result: some rows are pairs, some are AI-only, some are
#' gold-only.
#'
#' @param ai,gold Record tibbles with `.rid` and `.paper` columns.
#' @param linkage_fields Character vector of fields to link on.
#' @param paper_map A tibble with `ai_paper` and `gold_paper` defining scope.
#'   When `NULL`, papers are matched on identical `.paper` values.
#' @param rejected A tibble of human-rejected links with `ai_rid`, `gold_rid`.
#'   A rejected pair is never linked; either record may still pair with
#'   another.
#' @param added A tibble of human-added links with `ai_rid`, `gold_rid`. These
#'   win over any automatic link that conflicts with them.
#' @param min_posterior The probability of being the same record a pair needs
#'   to be linked. The default, 0.5, links a pair when that is more likely
#'   than not.
#'
#' @return A tibble with `pair_id`, `paper`, `ai_rid`, `gold_rid`, `posterior`,
#'   `matcher`, and `link_source` (`"auto"`, `"manual"`, or `"unpaired"`),
#'   ordered pairs first, then AI-only, then gold-only.
#' @export
align_records <- function(ai, gold, linkage_fields,
                          paper_map = NULL, rejected = NULL, added = NULL,
                          min_posterior = 0.5) {
  paper_map <- paper_map %||% common_paper_map(ai, gold)
  if (!nrow(paper_map)) return(empty_pairs())

  in_ai <- ai[ai$.paper %in% paper_map$ai_paper, , drop = FALSE]
  in_gold <- gold[gold$.paper %in% paper_map$gold_paper, , drop = FALSE]
  cand <- score_candidates(in_ai, in_gold, linkage_fields,
                           candidate_pairs(in_ai, in_gold, paper_map))
  cand <- cand[!is.na(cand$posterior) & cand$posterior >= min_posterior, , drop = FALSE]

  links <- tibble::tibble(
    i = cand$i, j = cand$j,
    paper = paper_map$ai_paper[cand$block],
    ai_rid = in_ai$.rid[cand$i],
    gold_rid = in_gold$.rid[cand$j],
    posterior = cand$posterior,
    matcher = "model",
    link_source = "auto"
  )
  # The human's decisions come before the 1:1 assignment, so a record freed
  # by a rejection, or displaced by a manual link, can still find its partner.
  links <- drop_links(links, rejected)
  if (!is.null(added) && nrow(added)) {
    links <- links[!links$ai_rid %in% added$ai_rid &
                     !links$gold_rid %in% added$gold_rid, , drop = FALSE]
  }
  links <- one_to_one(links)
  links <- links[, names(empty_links()), drop = FALSE]
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
#' So every paper is offered its most likely partner, and only the confident
#' links are accepted without a human looking.
#'
#' @param ai_papers,gold_papers Tibbles of paper metadata, each with a `.paper`
#'   identifier column.
#' @param fields Metadata fields to link on -- DOI, title, author, year.
#' @param min_posterior Links below this confidence are returned with
#'   `accepted = FALSE` for the human to confirm.
#'
#' @return A tibble with `ai_paper`, `gold_paper`, `posterior`, `matcher`,
#'   `accepted`.
#' @export
align_papers <- function(ai_papers, gold_papers, fields, min_posterior = 0.85) {
  empty <- empty_tbl(ai_paper = character(), gold_paper = character(),
                     posterior = numeric(), matcher = character(),
                     accepted = logical())
  fields <- intersect(fields, intersect(names(ai_papers), names(gold_papers)))
  if (!nrow(ai_papers) || !nrow(gold_papers)) return(empty)
  if (!length(fields)) {
    # Nothing to link on but the identifiers themselves.
    common <- intersect(ai_papers$.paper, gold_papers$.paper)
    return(tibble::tibble(ai_paper = common, gold_paper = common,
                          posterior = 1, matcher = "identifier",
                          accepted = TRUE))
  }
  na <- nrow(ai_papers)
  nb <- nrow(gold_papers)
  cand <- data.frame(block = 1L, i = rep(seq_len(na), times = nb),
                     j = rep(seq_len(nb), each = na))
  res <- one_to_one(score_candidates(ai_papers, gold_papers, fields, cand))
  if (!nrow(res)) return(empty)
  tibble::tibble(
    ai_paper = ai_papers$.paper[res$i],
    gold_paper = gold_papers$.paper[res$j],
    posterior = res$posterior,
    matcher = "model",
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
