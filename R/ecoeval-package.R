#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

#' Packages the Shiny app needs at runtime
#'
#' `R CMD check` scans `R/` but not `inst/app/`, so the packages the app depends
#' on look unused from here. They are not: the grid is DT, the file pickers are
#' shinyFiles, the disabled states are shinyjs, cell values are escaped with
#' htmltools, and the dashboard's matrices are made interactive with plotly.
#'
#' @return `NULL`, invisibly. Never called.
#' @keywords internal
#' @noRd
app_runtime_imports <- function() {
  list(
    DT::datatable,
    htmltools::htmlEscape,
    plotly::ggplotly,
    shinyFiles::shinyFilesButton,
    shinyjs::useShinyjs
  )
  invisible(NULL)
}
