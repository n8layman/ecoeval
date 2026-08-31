# Loading the Shiny app's stage modules for the integration tests.
#
# app.R attaches shiny and then sources the modules, so the tests give them the
# same enclosure rather than relying on whatever happens to be attached.

app_modules_loaded <- local({
  loaded <- FALSE
  function() {
    if (loaded) return(TRUE)
    dir <- app_module_dir()
    if (is.null(dir)) return(FALSE)
    # app.R attaches shiny before sourcing the modules, so give them the same
    # enclosure here: an environment whose parent is shiny's namespace.
    app_env <- new.env(parent = asNamespace("shiny"))
    for (f in list.files(dir, pattern = "[.][Rr]$", full.names = TRUE)) {
      sys.source(f, envir = app_env)
    }
    for (nm in ls(app_env, all.names = TRUE)) {
      assign(nm, get(nm, envir = app_env), envir = globalenv())
    }
    loaded <<- TRUE
    TRUE
  }
})

app_module_dir <- function() {
  for (candidate in c(system.file("app", "modules", package = "ecoeval"),
                      file.path("..", "..", "inst", "app", "modules"),
                      file.path("inst", "app", "modules"))) {
    if (nzchar(candidate) && dir.exists(candidate)) return(candidate)
  }
  NULL
}

skip_without_app <- function() {
  skip_if_not(requireNamespace("shiny", quietly = TRUE), "shiny not installed")
  skip_if_not(app_modules_loaded(), "app modules not found")
}

