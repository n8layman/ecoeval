# Charts.
#
# Three of Splink's charts are worth borrowing; the rest of its gallery is
# Fellegi-Sunter model internals, which is exactly the category we decided not
# to surface. ggplot2 and plotly are sufficient -- confusion matrices are
# geom_tile, and no new charting dependency earns its place.

#' The palette the grid and the charts share
#'
#' Fill carries agreement; schema violations are marked, not coloured. The four
#' outcome hues are checked for colour-vision separation rather than chosen by
#' eye: the hard pair is green against orange, which protanopia and
#' deuteranopia both flatten, so the orange is deep enough to separate on
#' lightness as well as hue.
#'
#' @return A named character vector of hex colours.
#' @export
ecoeval_palette <- function() {
  c(
    green = "#2f9e5b", yellow = "#dfa60d", orange = "#a83a17",
    purple = "#6f56b0", blank = "#e9ecef",
    ai = "#2c7fb8", gold = "#7a5195", grid = "#d9dee3", ink = "#2f3640"
  )
}

#' @keywords internal
#' @noRd
eco_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(colour = "#6b7280"),
      axis.title = ggplot2::element_text(colour = "#6b7280")
    )
}

#' Column-wise accuracy, worst first
#'
#' The triage view -- but read it carefully. A column that is uniformly wrong
#' has three possible causes and the chart cannot tell them apart: a
#' misconfigured comparator, an ambiguous field description, or the model
#' genuinely failing. Check them in that order; the chart says *where* to look,
#' not why.
#'
#' @param fm A tibble from [field_metrics()].
#' @return A ggplot.
#' @export
plot_column_accuracy <- function(fm) {
  if (!nrow(fm)) return(empty_plot("No columns scored yet."))
  d <- fm[!is.na(fm$accuracy), , drop = FALSE]
  if (!nrow(d)) return(empty_plot("No scored cells yet."))
  d$field <- factor(d$field, levels = d$field[order(d$accuracy)])
  ggplot2::ggplot(d, ggplot2::aes(x = .data$field, y = .data$accuracy)) +
    ggplot2::geom_col(fill = ecoeval_palette()[["ai"]], width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f%%", 100 * .data$accuracy)),
                       hjust = -0.15, size = 3.2, colour = "#6b7280") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                limits = c(0, 1.1), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Accuracy over scored cells",
                  title = "Column accuracy, worst first") +
    eco_theme()
}

#' A confusion matrix as a tile plot
#'
#' @param cc The result of [column_confusion()].
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_confusion <- function(cc, labels = current_labels()) {
  labels <- as_side_labels(labels)
  m <- cc$matrix
  if (is.null(m) || !nrow(m)) return(empty_plot("Nothing to plot for this column."))
  m$label <- ifelse(m$n > 0, as.character(m$n), "")
  subtitle <- switch(
    cc$type,
    class = sprintf("%s class by %s class. '(not in schema)' holds values the enum has no slot for.",
                    labels$gold, labels$ai),
    presence = "Presence and absence. The populated-by-both cell is split into correct and wrong value.",
    numeric = "Presence and absence; see the error distribution for how wrong the numbers are.",
    NULL
  )
  ggplot2::ggplot(m, ggplot2::aes(x = .data$ai_class, y = .data$gold_class,
                                  fill = .data$n)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label), size = 3.4,
                       colour = ecoeval_palette()[["ink"]]) +
    ggplot2::scale_fill_gradient(low = "#f2f6fa", high = ecoeval_palette()[["ai"]],
                                 guide = "none") +
    ggplot2::labs(x = labels$ai, y = labels$gold, title = cc$field,
                  subtitle = subtitle) +
    eco_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Similarity scores across a column's actual pairs
#'
#' The best of Splink's charts, and the reason it is worth borrowing: it lets a
#' user place a fuzzy cutoff by looking at their own data rather than guessing.
#'
#' @param profile A tibble from [similarity_profile()].
#' @param threshold The current cutoff, drawn as a line.
#' @return A ggplot.
#' @export
plot_threshold <- function(profile, threshold = 0.85) {
  if (!nrow(profile) || all(is.na(profile$score))) {
    return(empty_plot("No comparable pairs for this column yet."))
  }
  ggplot2::ggplot(profile, ggplot2::aes(x = .data$score)) +
    ggplot2::geom_histogram(bins = 30, fill = ecoeval_palette()[["ai"]],
                            colour = "white") +
    ggplot2::geom_vline(xintercept = threshold, colour = ecoeval_palette()[["orange"]],
                        linewidth = 0.9) +
    ggplot2::annotate("text", x = threshold, y = Inf, label = "  cutoff",
                      hjust = 0, vjust = 1.6, colour = ecoeval_palette()[["orange"]],
                      size = 3.4) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Similarity", y = "Pairs",
                  title = "Where to put the cutoff",
                  subtitle = "Every pair in scope for this column") +
    eco_theme()
}

#' Fill rates, one side against the other
#'
#' @param fills A tibble from [fill_rates()].
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_completeness <- function(fills, labels = current_labels()) {
  labels <- as_side_labels(labels)
  if (!nrow(fills)) return(empty_plot("No columns to profile yet."))
  d <- tibble::tibble(
    field = rep(fills$field, 2),
    source = factor(rep(c(labels$ai, labels$gold), each = nrow(fills)),
                    levels = c(labels$ai, labels$gold)),
    fill = c(fills$ai_fill, fills$gold_fill)
  )
  d$field <- factor(d$field, levels = rev(fills$field))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$field, y = .data$fill,
                                  fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7) +
    ggplot2::scale_fill_manual(values = stats::setNames(
                                 unname(ecoeval_palette()[c("ai", "gold")]),
                                 c(labels$ai, labels$gold)),
                               name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Populated",
                  title = "Who fills in what",
                  subtitle = "A field one side always populates and the other rarely does is usually a field-description problem") +
    eco_theme()
}

#' Signed error for a numeric column
#'
#' @param errors The `errors` element of a [column_confusion()] result.
#' @param field Column name, for the title.
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_error_distribution <- function(errors, field = "", labels = current_labels()) {
  labels <- as_side_labels(labels)
  if (is.null(errors) || !nrow(errors)) {
    return(empty_plot("No pairs where both sides had a number."))
  }
  ggplot2::ggplot(errors, ggplot2::aes(x = .data$error)) +
    ggplot2::geom_histogram(bins = 25, fill = ecoeval_palette()[["ai"]],
                            colour = "white") +
    ggplot2::geom_vline(xintercept = 0, colour = ecoeval_palette()[["ink"]],
                        linewidth = 0.6) +
    ggplot2::labs(x = sprintf("%s minus %s", labels$ai, labels$gold), y = "Pairs",
                  title = paste0("How wrong the numbers are", 
                                 if (nzchar(field)) paste0(": ", field) else "")) +
    eco_theme()
}

#' Record-level outcome, as one stacked bar
#'
#' @param rm_ A tibble from [record_metrics()].
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_record_outcome <- function(rm_, labels = current_labels()) {
  labels <- as_side_labels(labels)
  only_ai <- paste0(side_only(labels, "ai"), cm_tag(labels, "FP"))
  only_gold <- paste0(side_only(labels, "gold"), cm_tag(labels, "FN"))
  d <- tibble::tibble(
    outcome = factor(c("Matched", only_ai, only_gold),
                     levels = c(only_gold, only_ai, "Matched")),
    n = c(rm_$tp, rm_$fp, rm_$fn)
  )
  if (sum(d$n) == 0L) return(empty_plot("No records in scope."))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$outcome,
                                  fill = .data$outcome)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n), hjust = -0.3, size = 3.4,
                       colour = "#6b7280") +
    ggplot2::scale_fill_manual(values = stats::setNames(
      unname(ecoeval_palette()[c("green", "purple", "orange")]),
      c("Matched", only_ai, only_gold)
    ), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = "Records", y = NULL,
                  title = "Records matched, and on one side only") +
    eco_theme()
}

#' Separator joining a tile's paper and column into its click key
#'
#' A unit separator, because a paper key or a column name can contain anything
#' a person might otherwise reach for.
#'
#' @keywords internal
#' @noRd
TILE_KEY_SEP <- "\u001f"

#' Split a clicked tile's key back into its row and column
#'
#' Both heatmaps give every tile a `key`, which is what a `plotly_click` event
#' hands back. This turns it into the pair of values that identifies the tile,
#' so a click can be answered with the cells behind it. The `row` is a paper in
#' [plot_paper_heatmap()] and a record in [plot_record_heatmap()].
#'
#' @param key A key from a `plotly_click` event.
#' @return A list with `row` and `field`, or `NULL` when it does not parse.
#' @examples
#' parse_tile_key(paste("10.1000/p01", "species", sep = "\u001f"))
#' @export
parse_tile_key <- function(key) {
  key <- as.character(unlist(key))
  if (!length(key)) return(NULL)
  parts <- strsplit(key[[1L]], TILE_KEY_SEP, fixed = TRUE)[[1L]]
  if (length(parts) != 2L) return(NULL)
  list(row = parts[[1L]], field = parts[[2L]])
}

#' The whole run as one picture: papers against columns
#'
#' The overview, and a **summary** rather than a verdict. Rows are papers,
#' columns are the gold standard's columns, and every tile is one paper's
#' records for one column, shaded by how much of it agrees: dark where the two
#' sides match on everything, pale where they match on little, grey where there
#' was nothing to score.
#'
#' It is a rate rather than one of the four outcome colours because a tile
#' covers several records, and those colours describe a single cell -- a tile
#' holding nine agreements and one disagreement would paint the same as one
#' holding ten disagreements. The four colours live in
#' [plot_record_heatmap()], where a tile really is one cell. The rate is the
#' same accuracy [field_metrics()] reports, computed over that paper's cells for
#' that column, so the map and the numbers under it cannot disagree.
#'
#' Both axes are sorted by how much disagrees, worst first. That is the useful
#' ordering rather than a cosmetic one: it pulls a column that fails everywhere
#' to the left as a pale vertical band and a paper that fails everywhere to the
#' top as a pale horizontal one, and those two patterns have different causes --
#' a column that is bad across every paper is usually a comparator or a schema
#' problem, a paper that is bad across every column is usually a bad alignment.
#'
#' Hovering a tile breaks down the rows behind it, where a row is one line of
#' that paper's comparison -- a matched pair counts once, an unmatched record
#' counts as itself.
#'
#' @param grid A tibble from [paper_field_outcomes()].
#' @param max_papers Show at most this many papers, worst first; `NULL` shows
#'   all of them. The subtitle says when papers were left out.
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_paper_heatmap <- function(grid, max_papers = 60, labels = current_labels()) {
  labels <- as_side_labels(labels)
  if (!NROW(grid)) return(empty_plot("Nothing scored yet."))
  d <- grid
  # Sort on what disagrees, so an unscored tile does not read as a problem.
  d$problem <- ifelse(is.na(d$agreement), 0, 1 - d$agreement) * d$n_scored

  by_paper <- dplyr::arrange(
    dplyr::summarise(dplyr::group_by(d, .data$paper),
                     bad = sum(.data$problem), .groups = "drop"),
    dplyr::desc(.data$bad), .data$paper
  )
  dropped <- 0L
  if (!is.null(max_papers) && nrow(by_paper) > max_papers) {
    dropped <- nrow(by_paper) - max_papers
    by_paper <- by_paper[seq_len(max_papers), , drop = FALSE]
    d <- d[d$paper %in% by_paper$paper, , drop = FALSE]
  }
  by_field <- dplyr::arrange(
    dplyr::summarise(dplyr::group_by(d, .data$field),
                     bad = sum(.data$problem), .groups = "drop"),
    dplyr::desc(.data$bad), .data$field
  )

  # Worst paper at the top, which is the reverse of the factor order ggplot
  # draws a discrete y axis in.
  d$paper <- factor(d$paper, levels = rev(by_paper$paper))
  d$field <- factor(d$field, levels = by_field$field)
  # A "row" is one line of that paper's comparison: a matched AI-and-gold pair
  # counts once, an unmatched record counts as itself. The parts add up to it.
  d$text <- paste(
    as.character(d$paper), as.character(d$field),
    ifelse(is.na(d$agreement), "Nothing scored",
           sprintf("%s of cells agree", fmt_share(d$agreement))),
    ifelse(
      d$n_cells == 0L, "No records on either side",
      sprintf("%d %s: %d agree, %d differ, %d %s, %d %s%s",
              d$n_cells, ifelse(d$n_cells == 1L, "row", "rows"), d$n_agree,
              d$n_disagree, d$n_only_gold, side_only(labels, "gold"),
              d$n_only_ai, side_only(labels, "ai"),
              ifelse(d$n_blank > 0L,
                     sprintf(", %d blank on both sides", d$n_blank), ""))
    ),
    sep = "\n"
  )

  subtitle <- paste(
    "Worst papers at the top, worst columns at the left.",
    "Dark is agreement; pale is where the two sides part company."
  )
  if (dropped > 0L) {
    subtitle <- paste0(subtitle, " Showing the ", nrow(by_paper),
                       " papers with the most differences; ", dropped, " more not shown.")
  }
  subtitle <- paste(strwrap(subtitle, width = 96), collapse = "\n")

  # `key` is what a click comes back as, so a tile can be looked up again.
  d$key <- paste(d$paper, d$field, sep = TILE_KEY_SEP)

  ggplot2::ggplot(d, ggplot2::aes(x = .data$field, y = .data$paper,
                                  fill = .data$agreement, text = .data$text,
                                  key = .data$key)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
    ggplot2::scale_fill_gradient(
      low = AGREEMENT_RAMP[["low"]], high = AGREEMENT_RAMP[["high"]],
      limits = c(0, 1), labels = function(x) paste0(100 * x, "%"),
      na.value = AGREEMENT_RAMP[["none"]], name = "Cells that agree",
      guide = ggplot2::guide_colourbar(barheight = grid::unit(7, "pt"),
                                       barwidth = grid::unit(120, "pt"),
                                       title.vjust = 1)
    ) +
    # Column names sit above the chart, where a table puts its headers.
    ggplot2::scale_x_discrete(labels = function(x) truncate_label(x, 22, "start"),
                              expand = c(0, 0), position = "top") +
    ggplot2::scale_y_discrete(labels = function(x) truncate_label(x, 46),
                              expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = "Papers",
                  title = "Every paper, every column", subtitle = subtitle) +
    eco_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9),
      # Vertical, not angled: a header row has to sit in a predictable band, and
      # angled labels grow into the subtitle as the names get longer.
      axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5,
                                              size = 9),
      axis.text.y = ggplot2::element_text(size = 8.5),
      axis.title.y = ggplot2::element_text(colour = "#6b7280", size = 10),
      plot.subtitle = ggplot2::element_text(colour = "#6b7280",
                                            margin = ggplot2::margin(b = 12))
    )
}

#' One paper, every record against every column
#'
#' The view the four outcome colours are for: rows are the records of a single
#' paper -- matched pairs first, then the ones only the AI produced, then the
#' ones only the gold standard has -- columns are the scored fields with the
#' identity columns first, and **every tile is exactly one cell**. Nothing is
#' aggregated, so green means these two values agree and orange means the AI
#' asserted a value the gold standard does not have, with no averaging in
#' between.
#'
#' Schema violations are marked, not coloured: a violating tile carries a dot,
#' because validity is orthogonal to agreement and a cell can be both. Fill
#' carries agreement; the marker carries validity.
#'
#' @param grid A tibble from [record_field_outcomes()].
#' @param violations Keys of cells that fail schema validation, as
#'   `paste(pair_id, field)`.
#' @param focus A column to outline, typically the one clicked in the overview.
#' @param paper The paper's identifier, for the title.
#' @param labels Side labels; see [side_labels()].
#' @return A ggplot.
#' @export
plot_record_heatmap <- function(grid, violations = character(0), focus = NULL,
                                paper = NULL, labels = current_labels()) {
  labels <- as_side_labels(labels)
  if (!NROW(grid)) return(empty_plot("Nothing to compare in this paper."))
  d <- grid
  d$label <- factor(d$label, levels = rev(unique(d$label)))
  d$field <- factor(d$field, levels = unique(as.character(d$field)))
  d$outcome <- factor(d$outcome, levels = names(outcome_labels(labels)))
  d$key <- paste(d$pair_id, d$field, sep = TILE_KEY_SEP)
  d$bad <- paste(d$pair_id, d$field) %in% violations

  value <- function(x) ifelse(is.na(x) | !nzchar(x), "—",
                              truncate_label(x, 70, "start"))
  # What each side said, and what it was compared as when a normaliser
  # changed it.
  said <- function(side) {
    orig <- original_values(d, side)
    compared <- d[[paste0(side, "_value")]]
    changed <- !is.na(compared) & !is.na(orig) & compared != orig
    paste0(value(orig), ifelse(changed, paste0(" (compared as ", value(compared), ")"), ""))
  }
  d$text <- paste(
    sprintf("%s (%s)", as.character(d$label), record_kind_label(d$kind, labels)),
    as.character(d$field),
    unname(outcome_labels(labels)[as.character(d$outcome)]),
    paste0(labels$ai, ": ", said("ai")),
    paste0(labels$gold, ": ", said("gold")),
    ifelse(d$bad, "Fails schema validation", ""),
    sep = "\n"
  )

  pal <- ecoeval_palette()
  # Filled by the outcome's label rather than its code: ggplotly() names its
  # legend entries from the data and ignores a scale's `labels`, so a code
  # would show through as "only_ai".
  shown <- outcome_labels(labels)
  fills <- stats::setNames(
    unname(pal[c("green", "purple", "yellow", "orange")]),
    shown[c("agree", "disagree", "only_gold", "only_ai")]
  )
  d$fill <- factor(unname(shown[as.character(d$outcome)]), levels = unname(shown))

  # ggplot draws a legend key from a layer's data, so an outcome this paper
  # happens not to contain would get a label and no colour. A zero-sized tile
  # per outcome gives every key something to draw and shows nothing.
  stub <- tibble::tibble(
    field = factor(levels(d$field)[[1L]], levels = levels(d$field)),
    label = factor(levels(d$label)[[1L]], levels = levels(d$label)),
    fill = factor(names(fills), levels = unname(shown))
  )

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$field, y = .data$label,
                                       fill = .data$fill, text = .data$text,
                                       key = .data$key)) +
    ggplot2::geom_tile(data = stub, ggplot2::aes(x = .data$field, y = .data$label,
                                                 fill = .data$fill),
                       width = 0, height = 0, inherit.aes = FALSE) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.7, show.legend = FALSE)
  if (!is.null(focus) && focus %in% levels(d$field)) {
    p <- p + ggplot2::geom_tile(
      data = d[as.character(d$field) == focus, , drop = FALSE],
      colour = pal[["ink"]], linewidth = 0.9, fill = NA, show.legend = FALSE)
  }
  if (any(d$bad)) {
    p <- p + ggplot2::geom_point(data = d[d$bad, , drop = FALSE],
                                 colour = "#d9534f", size = 1.6,
                                 show.legend = FALSE)
  }
  p +
    # limits, not just drop = FALSE: an outcome absent from this paper still
    # belongs in the legend, so that four colours always mean four things.
    ggplot2::scale_fill_manual(values = fills, limits = names(fills),
                               drop = FALSE, name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::scale_x_discrete(labels = function(x) truncate_label(x, 22, "start"),
                              expand = c(0, 0), position = "top") +
    ggplot2::scale_y_discrete(labels = function(x) truncate_label(x, 44),
                              expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = if (is.null(paper)) "Every record, every column"
                          else paste("Every record in", paper),
                  subtitle = paste(strwrap(paste(
                    "One tile is one value from one record. Click it for both",
                    "values and the sentences each side quoted."
                  ), width = 96), collapse = "\n")) +
    eco_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 9),
      axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5,
                                              size = 9),
      axis.text.y = ggplot2::element_text(size = 8.5),
      plot.subtitle = ggplot2::element_text(colour = "#6b7280",
                                            margin = ggplot2::margin(b = 12))
    )
}

#' The ramp the overview shades agreement on
#'
#' One hue, light to dark, so the encoding is a magnitude rather than a
#' category. The light end stops well short of white: a tile at zero agreement
#' is the one a reader most needs to see, and a near-white tile on a white page
#' is the one they see least. `none` is for a tile with nothing to score, and is
#' a neutral clearly outside the ramp rather than a paler step of it.
#'
#' Checked with the palette validator in ordinal mode: monotone lightness,
#' visible step gaps, a single hue, and a light end that clears the surface.
#'
#' @keywords internal
#' @noRd
AGREEMENT_RAMP <- c(low = "#79bd98", high = "#1c6b3f", none = "#dfe3e8")

#' A share as a percentage, for a tooltip
#'
#' @keywords internal
#' @noRd
fmt_share <- function(x) {
  ifelse(is.na(x), "—", sprintf("%.0f%%", 100 * x))
}

#' Shorten a label for an axis, keeping the informative end
#'
#' Paper keys are DOIs, file names and titles, where the end distinguishes one
#' from its neighbours more often than the start does; column names are the
#' other way round.
#'
#' @param x A character vector.
#' @param n Maximum characters.
#' @param keep Which end to keep: `"end"` for a paper key, `"start"` for a
#'   column name, where the distinguishing part is at the front.
#' @return A character vector.
#' @keywords internal
#' @noRd
truncate_label <- function(x, n = 46, keep = c("end", "start")) {
  keep <- match.arg(keep)
  x <- as.character(x)
  long <- !is.na(x) & nchar(x) > n
  x[long] <- if (keep == "end") {
    paste0("\u2026", substr(x[long], nchar(x[long]) - n + 2L, nchar(x[long])))
  } else {
    paste0(substr(x[long], 1L, n - 1L), "\u2026")
  }
  x
}

#' A placeholder panel carrying an explanation
#'
#' @param msg Text to display.
#' @return A ggplot.
#' @keywords internal
#' @noRd
empty_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, colour = "#6b7280",
                      size = 4) +
    ggplot2::theme_void()
}
