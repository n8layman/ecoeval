# Charts.
#
# Three of Splink's charts are worth borrowing; the rest of its gallery is
# Fellegi-Sunter model internals, which is exactly the category we decided not
# to surface. ggplot2 and plotly are sufficient -- confusion matrices are
# geom_tile, and no new charting dependency earns its place.

#' The palette the grid and the charts share
#'
#' Fill carries agreement; schema violations are marked, not coloured.
#'
#' @return A named character vector of hex colours.
#' @export
ecoeval_palette <- function() {
  c(
    green = "#2e9e5b", yellow = "#e8b53a", orange = "#e07b39",
    purple = "#8e6fbe", blank = "#e9ecef",
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
#' @return A ggplot.
#' @export
plot_confusion <- function(cc) {
  m <- cc$matrix
  if (is.null(m) || !nrow(m)) return(empty_plot("Nothing to plot for this column."))
  m$label <- ifelse(m$n > 0, as.character(m$n), "")
  subtitle <- switch(
    cc$type,
    class = "Gold class by AI class. '(not in schema)' holds values the enum has no slot for.",
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
    ggplot2::labs(x = "AI", y = "Gold standard", title = cc$field,
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

#' Fill rates, AI against gold
#'
#' @param fills A tibble from [fill_rates()].
#' @return A ggplot.
#' @export
plot_completeness <- function(fills) {
  if (!nrow(fills)) return(empty_plot("No columns to profile yet."))
  d <- tibble::tibble(
    field = rep(fills$field, 2),
    source = rep(c("AI", "Gold"), each = nrow(fills)),
    fill = c(fills$ai_fill, fills$gold_fill)
  )
  d$field <- factor(d$field, levels = rev(fills$field))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$field, y = .data$fill,
                                  fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.7) +
    ggplot2::scale_fill_manual(values = c(AI = ecoeval_palette()[["ai"]],
                                          Gold = ecoeval_palette()[["gold"]]),
                               name = NULL) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                                limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Populated",
                  title = "Who fills in what",
                  subtitle = "A field humans always populate and the AI rarely does is usually a field-description problem") +
    eco_theme()
}

#' Signed error for a numeric column
#'
#' @param errors The `errors` element of a [column_confusion()] result.
#' @param field Column name, for the title.
#' @return A ggplot.
#' @export
plot_error_distribution <- function(errors, field = "") {
  if (is.null(errors) || !nrow(errors)) {
    return(empty_plot("No pairs where both sides had a number."))
  }
  ggplot2::ggplot(errors, ggplot2::aes(x = .data$error)) +
    ggplot2::geom_histogram(bins = 25, fill = ecoeval_palette()[["ai"]],
                            colour = "white") +
    ggplot2::geom_vline(xintercept = 0, colour = ecoeval_palette()[["ink"]],
                        linewidth = 0.6) +
    ggplot2::labs(x = "AI minus gold", y = "Pairs",
                  title = paste0("How wrong the numbers are", 
                                 if (nzchar(field)) paste0(": ", field) else "")) +
    eco_theme()
}

#' Record-level outcome, as one stacked bar
#'
#' @param rm_ A tibble from [record_metrics()].
#' @return A ggplot.
#' @export
plot_record_outcome <- function(rm_) {
  d <- tibble::tibble(
    outcome = factor(c("Matched", "AI only (FP)", "Gold only (FN)"),
                     levels = c("Gold only (FN)", "AI only (FP)", "Matched")),
    n = c(rm_$tp, rm_$fp, rm_$fn)
  )
  if (sum(d$n) == 0L) return(empty_plot("No records in scope."))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$n, y = .data$outcome,
                                  fill = .data$outcome)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = .data$n), hjust = -0.3, size = 3.4,
                       colour = "#6b7280") +
    ggplot2::scale_fill_manual(values = c(
      "Matched" = ecoeval_palette()[["green"]],
      "AI only (FP)" = ecoeval_palette()[["purple"]],
      "Gold only (FN)" = ecoeval_palette()[["orange"]]
    ), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(x = "Records", y = NULL, title = "Did the AI find the right rows?") +
    eco_theme()
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
