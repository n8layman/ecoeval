# Stage 4 -- map the record fields, configure the comparators, pick the
# linkage fields.
#
# Mapping is 1:1 plus "ignore". Many-to-one (a single gold Location column
# feeding the AI's country/region/site) is not supported; users pre-process.
#
# Comparator defaults come from the JSON Schema type -- the only inference
# available without domain knowledge, and enough. Without normalisation the
# first run is a wall of disagreement that is mostly formatting noise.
#
# Linkage fields are the user's choice. x-unique-fields is a default suggestion
# and nothing more: it may be absent entirely, and it was written to control
# the AI pipeline's deduplication, not to define correspondence with an
# independently built human dataset.

mod_map_records_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("conformance")),
    eco_panel(
      "Fields, comparators, and identity columns",
      paste("Identity columns are what the matcher links on and what gets",
            "pinned to the left of the grid. A comparator names the highest",
            "rung the cascade may climb to: exact, then trimmed and",
            "case-insensitive, then fuzzy, then the LLM judge. Each rung sees",
            "only what the previous could not resolve."),
      div(style = "overflow-x:auto;", uiOutput(ns("config_table"))),
      div(style = "margin-top:14px; display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
          actionButton(ns("apply"), "Align records and score", class = "btn-primary"),
          actionButton(ns("reset"), "Reset to schema defaults"),
          div(class = "eco-status", textOutput(ns("status"), inline = TRUE)))
    ),
    uiOutput(ns("threshold_panel")),
    uiOutput(ns("granularity"))
  )
}

mod_map_records_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reset_token <- reactiveVal(0L)

    # ---- what the schema and the two column sets suggest --------------------
    suggestion <- reactive({
      req(rv$schema, rv$ai_raw, rv$gold_raw)
      reset_token()
      fields <- rv$schema$fields$field
      ai_cols <- setdiff(names(rv$ai_raw), rv$ai_paper_key$columns)
      gold_cols <- setdiff(names(rv$gold_raw), rv$gold_paper_key$columns)
      ai_map <- ecoeval::suggest_mapping(fields, ai_cols)
      gold_map <- ecoeval::suggest_mapping(fields, gold_cols)
      cfg <- ecoeval::default_comparator_config(rv$schema, fields = fields)
      cfg$ai_col <- ai_map$to
      cfg$gold_col <- gold_map$to
      # A field neither source has is nothing to compare, so it starts off.
      cfg$include <- !is.na(cfg$ai_col) & !is.na(cfg$gold_col)
      cfg$linkage <- cfg$linkage & cfg$include
      cfg
    })

    output$config_table <- renderUI({
      cfg <- suggestion()
      ai_cols <- c("(ignore)" = "", setdiff(names(rv$ai_raw), rv$ai_paper_key$columns))
      gold_cols <- c("(ignore)" = "", setdiff(names(rv$gold_raw), rv$gold_paper_key$columns))

      # n_enum lives on the schema, not the config; join it for the label.
      cfg$n_enum <- rv$schema$fields$n_enum[match(cfg$field, rv$schema$fields$field)]

      row_ui <- function(i) {
        f <- cfg$field[[i]]
        tags$tr(
          tags$td(checkboxInput(ns(paste0("inc_", i)), NULL, value = cfg$include[[i]])),
          tags$td(span(class = "eco-field-name", f),
                  if (isTRUE((cfg$n_enum[[i]] %||% 0L) > 0L))
                    span(style = "color:#6b7280; font-size:10.5px;",
                         sprintf(" enum[%d]", cfg$n_enum[[i]]))),
          tags$td(selectInput(ns(paste0("ai_", i)), NULL, choices = ai_cols,
                              selected = cfg$ai_col[[i]] %||% "")),
          tags$td(selectInput(ns(paste0("gold_", i)), NULL, choices = gold_cols,
                              selected = cfg$gold_col[[i]] %||% "")),
          tags$td(selectInput(ns(paste0("cmp_", i)), NULL,
                              choices = ecoeval::comparator_choices(),
                              selected = cfg$comparator[[i]])),
          tags$td(numericInput(ns(paste0("thr_", i)), NULL,
                               value = cfg$threshold[[i]], min = 0, max = 1,
                               step = 0.01)),
          tags$td(selectInput(ns(paste0("nrm_", i)), NULL,
                              choices = ecoeval::normalizer_choices(),
                              selected = cfg$normalizer[[i]])),
          tags$td(checkboxInput(ns(paste0("lnk_", i)), NULL, value = cfg$linkage[[i]]))
        )
      }

      tags$table(
        class = "eco-cfg",
        tags$thead(tags$tr(
          tags$th("Use"), tags$th("Field"), tags$th("AI column"),
          tags$th("Gold column"), tags$th("Comparator"), tags$th("Cutoff"),
          tags$th("Normalise"), tags$th("Identity")
        )),
        tags$tbody(lapply(seq_len(nrow(cfg)), row_ui))
      )
    })

    observeEvent(input$reset, {
      reset_token(reset_token() + 1L)
      showNotification("Reset to the schema's defaults.", type = "message")
    })

    # ---- read the widgets back out -----------------------------------------
    current_config <- reactive({
      cfg <- suggestion()
      n <- nrow(cfg)
      get1 <- function(prefix, i, fallback) {
        v <- input[[paste0(prefix, "_", i)]]
        if (is.null(v)) fallback else v
      }
      for (i in seq_len(n)) {
        cfg$include[[i]] <- isTRUE(get1("inc", i, cfg$include[[i]]))
        cfg$linkage[[i]] <- isTRUE(get1("lnk", i, cfg$linkage[[i]]))
        cfg$comparator[[i]] <- get1("cmp", i, cfg$comparator[[i]])
        cfg$normalizer[[i]] <- get1("nrm", i, cfg$normalizer[[i]])
        thr <- suppressWarnings(as.numeric(get1("thr", i, cfg$threshold[[i]])))
        cfg$threshold[[i]] <- if (is.na(thr)) 0.85 else thr
        ai_c <- get1("ai", i, cfg$ai_col[[i]])
        gold_c <- get1("gold", i, cfg$gold_col[[i]])
        cfg$ai_col[[i]] <- if (identical(ai_c, "")) NA_character_ else ai_c
        cfg$gold_col[[i]] <- if (identical(gold_c, "")) NA_character_ else gold_c
      }
      cfg$include <- cfg$include & !is.na(cfg$ai_col) & !is.na(cfg$gold_col)
      cfg$linkage <- cfg$linkage & cfg$include
      cfg
    })

    status <- reactiveVal("")
    output$status <- renderText(status())

    # ---- schema conformance, run as a pre-flight on both sides -------------
    output$conformance <- renderUI({
      req(rv$ai, rv$gold, rv$schema)
      conf <- rv$conformance
      if (is.null(conf) || !nrow(conf)) return(NULL)
      by_field <- split(conf, conf$field)
      eco_panel(
        "Schema conformance",
        paste("Nothing here is adjusted for in the metrics. A value recurring",
              "many times is a category the schema is missing; a single one is",
              "a typo. The counts make the difference obvious, and the two",
              "have opposite fixes."),
        lapply(names(by_field), function(f) {
          d <- by_field[[f]]
          tagList(
            tags$strong(sprintf("%s -- %d value(s) the schema rejects", f, nrow(d))),
            tags$pre(
              class = "eco-status",
              paste(sprintf("  %-24s %4dx   (%s)", d$value, d$n, d$source),
                    collapse = "\n")
            )
          )
        })
      )
    })

    # ---- apply --------------------------------------------------------------
    observeEvent(input$apply, {
      cfg <- current_config()
      if (!any(cfg$include)) {
        rv$blocked <- ecoeval::blocking_condition(rv$scope, character(0))
        return(invisible(NULL))
      }
      if (!any(cfg$linkage)) {
        status("Pick at least one identity column for the matcher to link on.")
        return(invisible(NULL))
      }

      withProgress(message = "Aligning and scoring", value = 0, {
        use <- cfg[cfg$include, , drop = FALSE]

        incProgress(0.1, detail = "reading columns")
        rv$ai <- ecoeval::prepare_records(
          rv$ai_raw, rv$ai_paper_key,
          stats::setNames(use$ai_col, use$field), prefix = "a")
        rv$gold <- ecoeval::prepare_records(
          rv$gold_raw, rv$gold_paper_key,
          stats::setNames(use$gold_col, use$field), prefix = "g")

        # Normalisation is a pre-matching step, per value and cached: fastLink
        # cannot see through "Myotis lucifugus" against "little brown bat", and
        # those are exactly the fields it depends on most.
        incProgress(0.15, detail = "normalising identity columns")
        norm_fields <- use$field[use$normalizer != "none"]
        for (f in norm_fields) {
          nrm <- use$normalizer[[match(f, use$field)]]
          rv$ai[[f]] <- ecoeval::normalize_values(rv$ai[[f]], nrm, rv$norm_cache)
          rv$gold[[f]] <- ecoeval::normalize_values(rv$gold[[f]], nrm, rv$norm_cache)
        }

        incProgress(0.2, detail = "checking the schema")
        rv$conformance <- dplyr::bind_rows(
          ecoeval::check_conformance(rv$ai, rv$schema, "ai"),
          ecoeval::check_conformance(rv$gold, rv$schema, "gold")
        )

        linkage <- use$field[use$linkage]
        rv$collapses <- ecoeval::granularity_check(rv$gold, linkage, "gold")

        incProgress(0.3, detail = "matching records")
        rv$pairs <- ecoeval::align_records(rv$ai, rv$gold, linkage, rv$paper_map,
                                           rejected = rv$rejected, added = rv$added)

        incProgress(0.2, detail = "scoring cells")
        rv$cells <- ecoeval::score_cells(rv$pairs, rv$ai, rv$gold, use,
                                         judge = NULL, cache = rv$judge_cache,
                                         overrides = rv$overrides)
      })

      rv$comparators <- cfg
      rv$blocked <- NULL
      rv$current_paper <- rv$scope$papers[[1L]]
      rv$dirty <- rv$dirty + 1L
      status(sprintf("%d records aligned into %d rows.",
                     nrow(rv$ai) + nrow(rv$gold), nrow(rv$pairs)))
      rv$stage <- "dashboard"
    })

    # ---- the threshold-picking chart ---------------------------------------
    output$threshold_panel <- renderUI({
      cfg <- current_config()
      fuzzy <- cfg$field[cfg$include & cfg$comparator %in% c("fuzzy", "judge", "set")]
      if (!length(fuzzy) || is.null(rv$pairs)) return(NULL)
      eco_panel(
        "Where to put the cutoff",
        paste("The distribution of similarity across this column's actual",
              "pairs, so the cutoff is placed by looking at your own data",
              "rather than guessing."),
        selectInput(ns("threshold_field"), NULL, choices = fuzzy, width = "320px"),
        plotOutput(ns("threshold_plot"), height = "260px")
      )
    })

    output$threshold_plot <- renderPlot({
      req(input$threshold_field, rv$pairs, rv$ai, rv$gold)
      cfg <- current_config()
      i <- match(input$threshold_field, cfg$field)
      ecoeval::plot_threshold(
        ecoeval::similarity_profile(rv$pairs, rv$ai, rv$gold, input$threshold_field),
        threshold = cfg$threshold[[i]]
      )
    })

    # ---- the granularity check ---------------------------------------------
    output$granularity <- renderUI({
      g <- rv$collapses
      if (is.null(g) || !nrow(g)) return(NULL)
      eco_panel(
        "Granularity",
        paste("Gold records that collapse together on the identity columns.",
              "If many do, the two datasets disagree about what a record is --",
              "the human distinguished records by something the key does not",
              "capture. Add a field to the identity columns and try again."),
        tags$pre(class = "eco-status",
                 paste(sprintf("  %-28s %d records share one key", g$paper, g$n),
                       collapse = "\n"))
      )
    })
  })
}
