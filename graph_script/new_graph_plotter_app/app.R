# Dependencies (deploy comment; install manually as needed):
# shiny, bslib, shinyjs, shinyWidgets, here, dplyr, tidyr, readxl, writexl,
# openxlsx, DT, ggplot2, ggthemes, countrycode, stringr, glue, rlang, purrr,
# showtext, svglite, cli, tibble — plus packages from assert_packages in plotting files.

library(shiny)
library(bslib)
library(shinyjs)
library(here)
library(dplyr)
library(DT)
library(glue)
library(rlang)
library(openxlsx)
library(jsonlite)

plotting_libs <- c(
  "dplyr", "reshape2", "ggplot2", "ggthemes", "countrycode", "readxl", "tidyr",
  "data.table", "writexl", "unikn", "ggtext", "svglite", "stringr", "directlabels",
  "fanplot", "forcats", "glue", "readr", "showtext", "ggfan", "purrr", "tibble", "cli"
)
for (lib in plotting_libs) {
  suppressPackageStartupMessages(library(lib, character.only = TRUE))
}

here::i_am("app.R")
source(here("check_graphplan.R"))
source(here("prepare_elements.R"))
source(here("plot_themes.R"))
source(here("plot_types.R"))
source(here("service.R"))

# Constants aligned with graph_script/do_plot.R (also defined in service.R for helpers)
sheet_keys   <- c(y = "y", q = "q", m = "m")
peers_fname  <- here("1_peers_params.xlsx")
data_fname   <- here("Filled_DB.rds")
data_d_fname <- here("Filled_DB_d.rds")

# Filled DB loads once at app startup (module scope). Server must use `FD` only —
# do not call importData() in shinyServer (grep app.R: importData appears only here).
FD <- profile_step(
  "cold_start.importData",
  importData(
    yqm_file   = data_fname,
    d_file     = data_d_fname,
    sheet_keys = sheet_keys,
    format     = "auto",
    add_time   = TRUE
  )
)
if (plotter_profile_enabled()) {
  message("Filled_DB loaded once at startup (not reloaded in server).")
}

countries_tbl <- FD$extdata_y |>
  dplyr::distinct(country, country_id) |>
  dplyr::arrange(country)

country_choices <- stats::setNames(countries_tbl$country_id, countries_tbl$country)
default_country_id <- countries_tbl$country_id[countries_tbl$country == "Russian Federation"][1]
if (is.na(default_country_id) || !nzchar(default_country_id)) {
  default_country_id <- "RU"
}
country_iso3c_from_id <- function(country_id) {
  countrycode::countrycode(country_id, "iso2c", "iso3c", warn = FALSE)
}

indicators_tbl <- FD$dict |>
  dplyr::select(indicator, indicator_code, source_frequency) |>
  dplyr::distinct()

indicator_choices <- stats::setNames(
  indicators_tbl$indicator_code,
  indicators_tbl$indicator
)

graph_group_choices <- stats::setNames(unname(graph_groups), names(graph_groups))

ensure_indicator_choices <- function(choices, codes, indicators_tbl) {
  codes <- codes[!is.na(codes) & nzchar(codes)]
  missing <- setdiff(codes, unname(choices))
  if (!length(missing)) return(choices)
  extra_rows <- indicators_tbl |>
    dplyr::filter(.data$indicator_code %in% missing)
  c(choices, stats::setNames(extra_rows$indicator_code, extra_rows$indicator))
}

# --------------------------- UI -----------------------------------------------

ui <- bslib::page_navbar(
  id = "main_nav",
  title = "Country Graph Plotter 2.0",
  window_title = "Country Graph Plotter 2.0",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  navbar_options = bslib::navbar_options(position = "fixed-top"),
  # Top padding clears fixed-top navbar (body padding is overridden by bslib inline styles).
  padding = c("5.5rem", "1rem", "1rem", "1rem"),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .import-top-row { align-items: stretch; }
      .import-summary-col {
        display: flex;
        flex-direction: column;
        min-height: 100%;
      }
      .import-summary-cards {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem;
        flex: 1;
        align-content: flex-start;
      }
      .validation-metric-card {
        flex: 1 1 calc(50% - 0.375rem);
        min-width: 7rem;
        border-radius: 8px;
        padding: 0.85rem 1rem;
        border: 1px solid #dee2e6;
        background: #fff;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
      }
      .validation-metric-value {
        font-size: 1.75rem;
        font-weight: 700;
        line-height: 1.2;
      }
      .validation-metric-label {
        font-size: 0.8rem;
        color: #6c757d;
        margin-top: 0.15rem;
      }
      .validation-metric-active { background-color: #d4edda; border-color: #c3e6cb; }
      .validation-metric-buildable { background-color: #d1ecf1; border-color: #bee5eb; }
      .validation-metric-errors { background-color: #f8d7da; border-color: #f5c6cb; }
      .validation-metric-inactive { background-color: #e2e3e5; border-color: #d6d8db; }
      .validation-metric-total { background-color: #f8f9fa; }
      .editor-mode-toolbar {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 0.5rem;
        margin-bottom: 0.75rem;
      }
      .editor-mode-badge {
        display: inline-block;
        padding: 0.35rem 0.75rem;
        border-radius: 4px;
        font-size: 0.85rem;
        font-weight: 600;
        background: #e9ecef;
        color: #495057;
        margin-left: auto;
      }
      .editor-tsv-row .form-group { margin-bottom: 0; }
    "))
  ),

  bslib::nav_panel(
    title = "Graphplan import & validation",
    value = "tab_import",
    fluidPage(
      fluidRow(
        class = "import-top-row",
        column(
          6,
          fileInput(
            "graphplan_file",
            "Upload graph plan (2_graphlib.xlsx)",
            accept = c(".xlsx")
          ),
          selectizeInput(
            "country_choice",
            "Choose country",
            choices  = country_choices,
            selected = default_country_id,
            options  = list(placeholder = "Select country")
          ),
          actionButton("validate_btn", "Validate", class = "btn-primary")
        ),
        column(
          6,
          class = "import-summary-col",
          uiOutput("validation_summary_ui")
        )
      ),
      fluidRow(
        column(12, DT::dataTableOutput("validation_table"))
      )
    )
  ),

  bslib::nav_panel(
    title = "Graph gallery",
    value = "tab_gallery",
    fluidPage(
      tags$style(HTML("
        .gallery-grid { margin-top: 0.5rem; }
        .gallery-card {
          border: 1px solid #dee2e6;
          border-radius: 8px;
          padding: 10px 10px 6px;
          margin-bottom: 18px;
          background: #fff;
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
          height: 100%;
        }
        .gallery-card-title {
          font-size: 0.85rem;
          font-weight: 600;
          margin: 0 0 8px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .gallery-card-plot {
          width: 100%;
          overflow: hidden;
          border-radius: 4px;
          background: #f8f9fa;
        }
        .gallery-card-plot .shiny-plot-output img {
          width: 100% !important;
          height: auto !important;
          object-fit: contain;
        }
        .gallery-card-error { margin-bottom: 18px; }
        .gallery-card-actions { margin-top: 6px; display: flex; flex-wrap: wrap; gap: 4px; }
        .gallery-card-actions .btn { font-size: 0.75rem; padding: 2px 8px; }
        .gallery-status-ok { background: #198754; }
        .gallery-status-err { background: #dc3545; }
      ")),
      div(
        style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;",
        textInput("gallery_action", label = NULL, value = "")
      ),
      div(
        style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;",
        downloadButton("gallery_single_download", "Download graph")
      ),
      actionButton("build_valid_btn", "Build valid graphs"),
      br(), br(),
      uiOutput("gallery_ui")
    )
  ),

  bslib::nav_panel(
    title = "Graph editor",
    value = "tab_editor",
    fluidPage(
      div(
        class = "editor-mode-toolbar",
        actionButton("ed_new_graph", "New graph", class = "btn-default"),
        actionButton("ed_save_row", "Save to graphplan", class = "btn-primary"),
        uiOutput("editor_mode_ui")
      ),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          selectizeInput(
            "ed_graph_type", "Graph type",
            choices = graph_types,
            options = list(placeholder = "Select graph type")
          ),
          selectizeInput(
            "ed_data_frequency", "Data freq",
            choices = c(" ", "y", "q", "m", "d")
          ),
          selectInput(
            "ed_ind_group", "Indicator group",
            choices = indicator_groups,
            selected = ""
          ),
          selectizeInput(
            "ed_indicators", "Indicators",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select indicators",
              maxOptions = 200
            )
          ),
          textInput("ed_time_fix", "Time fix", ""),
          h4("Peers"),
          fluidRow(
            column(
              6,
              selectizeInput("ed_peers", "Peer group", choices = peers_choice)
            ),
            column(6, textInput("ed_peers_formula", "Formula", ""))
          ),
          selectizeInput(
            "ed_peers_custom", "Custom peers (ISO2)",
            choices = country_choices,
            multiple = TRUE,
            options = list(placeholder = "Custom peer countries")
          ),
          checkboxInput("ed_all", "Show all countries", FALSE),
          h4("Style"),
          fluidRow(
            column(3, textInput("ed_x_min", "X min", "")),
            column(3, textInput("ed_x_max", "X max", "")),
            column(3, numericInput("ed_y_min", "Y min", value = NA, width = "100%")),
            column(3, numericInput("ed_y_max", "Y max", value = NA, width = "100%"))
          ),
          fluidRow(
            column(4, selectInput("ed_trend_type", "Trend", choices = trend_types_ui, selected = "")),
            column(8, selectInput("ed_theme", "Style preset", choices = theme_types, selected = "ipsum"))
          ),
          fluidRow(
            column(
              8,
              selectizeInput(
                "ed_sec_y_axis_ind", "2nd Y-axis",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Optional",
                  maxOptions = 200
                )
              )
            ),
            column(4, numericInput("ed_sec_y_axis_coeff", "Axis mult", value = NA, width = "100%"))
          ),
          fluidRow(
            column(4, checkboxInput("ed_x_log", "X log", FALSE)),
            column(3, checkboxInput("ed_y_log", "Y log", FALSE)),
            column(5, checkboxInput("ed_swap_axis", "Swap axis", FALSE))
          ),
          fluidRow(
            column(4, checkboxInput("ed_recession", "Recession", FALSE)),
            column(3, checkboxInput("ed_index", "Index", FALSE)),
            column(5, checkboxInput("ed_long_legend", "Long legend", FALSE))
          ),
          fluidRow(
            column(6, checkboxInput("ed_short_names", "Short indicator names", FALSE)),
            column(6, checkboxInput("ed_vert_lab", "Vertical X labels", FALSE))
          )
        ),
        mainPanel(
          width = 8,
          fluidRow(
            class = "editor-tsv-row",
            column(2, actionButton("ed_plot_btn", "Update plot", class = "btn-primary")),
            column(2, actionButton("ed_fill_btn", "Help with defaults")),
            column(2, actionButton("ed_import_row_btn", "Import row (TSV)")),
            column(6, textInput("ed_graph_plan_tsv", "Graph plan row (TSV)", ""))
          ),
          br(),
          plotOutput("ed_graph_plot", height = "480px"),
          br(),
          fluidRow(
            column(4, downloadButton("ed_download_png", "Download png")),
            column(4, downloadButton("ed_download_data", "Download data")),
            column(4, actionButton("ed_export_row_btn", "Export row (TSV)"))
          ),
          verbatimTextOutput("ed_row_validation"),
          br(),
          fluidRow(
            column(5, textAreaInput("ed_graph_title", "Graph title", "Graph Title", rows = 2)),
            column(2, selectizeInput("ed_graph_group", "Graph group", choices = graph_group_choices)),
            column(3, textInput("ed_graph_name_suffix", "File name suffix", "goodgraph")),
            column(2, selectInput("ed_orientation", "Orientation", choices = orient_types))
          ),
          checkboxInput("ed_show_title", "Show title", TRUE),
          checkboxInput("ed_active", "Active row", TRUE)
        )
      )
    )
  ),

  bslib::nav_panel(
    title = "Export",
    value = "tab_export",
    fluidPage(
      fluidRow(
        column(
          6,
          h4("Graph images"),
          selectInput(
            "export_scope",
            "Graphs to include",
            choices = c(
              "All built graphs" = "all_built",
              "Built graphs (ok status only)" = "valid_only",
              "Selected graphs" = "gallery_selected"
            ),
            selected = "all_built"
          ),
          conditionalPanel(
            condition = "input.export_scope == 'gallery_selected'",
            selectizeInput(
              "export_selected_graphs",
              "Select graphs",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Choose built graphs")
            )
          ),
          selectInput("export_device", "Image format", choices = c("png", "jpeg"), selected = "png"),
          downloadButton("download_graphs_zip", "Download graphs (zip)", class = "btn-primary"),
          downloadButton("download_graph_data_xlsx", "Download graph data (xlsx)")
        ),
        column(
          6,
          h4("Graphplan"),
          helpText("Exports library + info sheets (2_graphlib.xlsx format). Does not modify Filled_DB.rds."),
          downloadButton("download_graphplan_xlsx", "Download graphplan (xlsx)", class = "btn-primary"),
          downloadButton("download_recipes_tsv", "Download recipes (TSV)"),
          checkboxInput("export_recipes_active_only", "Recipes: active rows only", FALSE)
        )
      ),
      fluidRow(
        column(12, h4("Report"), verbatimTextOutput("export_report"))
      )
    )
  )
)

# --------------------------- Server -----------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    graphplan       = NULL,
    graphplan_info  = NULL,
    graphplan_baseline = NULL,
    country_iso3c   = country_iso3c_from_id(default_country_id),
    validation      = NULL,
    built           = list(),
    selected_row_id = NULL,
    editor_mode     = "new",
    editor_preview  = NULL,
    editor_row_validation = NULL,
    editor_bootstrapped = FALSE,
    editor_selectize_ready = FALSE,
    gallery_download_name = NULL,
    dirty           = FALSE,
    plan_validation_revision = 0L,
    editor_validation_cache_key = NULL
  )

  bump_plan_validation_revision <- function(from_revision = NULL) {
    prev <- if (is.null(from_revision)) {
      rv$plan_validation_revision
    } else {
      from_revision
    }
    next_rev <- (prev %||% 0L) + 1L
    rv$plan_validation_revision <- next_rev
    rv$editor_validation_cache_key <- NULL
    invisible(next_rev)
  }

  clear_editor_validation_cache <- function() {
    rv$editor_validation_cache_key <- NULL
    rv$editor_row_validation <- NULL
  }

  observeEvent(input$country_choice, {
    rv$country_iso3c <- country_iso3c_from_id(input$country_choice)
    bump_plan_validation_revision()
  }, ignoreNULL = FALSE)

  navigate_to_tab <- function(tab_value) {
    bslib::nav_select(id = "main_nav", selected = tab_value, session = session)
  }

  navigate_to_gallery_tab <- function() {
    navigate_to_tab("tab_gallery")
  }

  navigate_to_editor_tab <- function() {
    navigate_to_tab("tab_editor")
  }

  run_after_tab_switch <- function(expr) {
    quo_expr <- rlang::enquo(expr)
    session$onFlushed(
      function() {
        shiny::withReactiveDomain(session, {
          rlang::eval_tidy(quo_expr)
        })
      },
      once = TRUE
    )
  }

  observeEvent(input$graphplan_file, {
    req(input$graphplan_file)
    imported <- read_graphplan_file(input$graphplan_file$datapath, dict = FD$dict)
    rv$graphplan <- imported$plan
    rv$graphplan_info <- imported$info
    rv$graphplan_baseline <- graphplan_snapshot(imported$plan)
    rv$validation <- NULL
    rv$built <- list()
    rv$dirty <- TRUE
    bump_plan_validation_revision()
    clear_editor_validation_cache()
    showNotification("Graphplan loaded.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$validate_btn, {
    shiny::validate(
      shiny::need(!is.null(rv$graphplan) && nrow(rv$graphplan) > 0, "Load or create a graphplan first."),
      shiny::need(!is.null(input$country_choice) && nzchar(input$country_choice), "Select a country.")
    )
    iso3 <- country_iso3c_from_id(input$country_choice)
    rv$country_iso3c <- iso3
    rv$validation <- profile_step(
      "import.validate_graphplan_for_app",
      validate_graphplan_for_app(
        graphplan     = rv$graphplan,
        FD            = FD,
        country_iso3c = iso3,
        peers_fname   = peers_fname
      )
    )
    rv$graphplan <- rv$validation$plan
    bump_plan_validation_revision()
    clear_editor_validation_cache()
    s <- rv$validation$summary
    showModal(modalDialog(
      title = "Validation complete",
      tags$p(glue(
        "Active: {s$n_active}, buildable: {s$n_buildable}, ",
        "errors: {s$n_errors}, inactive: {s$n_inactive}"
      )),
      footer = tagList(
        modalButton("Fix issues first"),
        actionButton("validate_build_continue", "Build valid graphs", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$validate_build_continue, {
    removeModal()
    shinyjs::click("build_valid_btn")
  })

  validation_metric_card <- function(label, value, card_class) {
    tags$div(
      class = paste("validation-metric-card", card_class),
      tags$div(class = "validation-metric-value", format(value, big.mark = ",")),
      tags$div(class = "validation-metric-label", label)
    )
  }

  output$validation_summary_ui <- renderUI({
    if (is.null(rv$validation)) {
      return(NULL)
    }
    s <- rv$validation$summary
    tags$div(
      class = "import-summary-cards",
      validation_metric_card("Active", s$n_active[[1]], "validation-metric-active"),
      validation_metric_card("Buildable", s$n_buildable[[1]], "validation-metric-buildable"),
      validation_metric_card("Errors", s$n_errors[[1]], "validation-metric-errors"),
      validation_metric_card("Inactive", s$n_inactive[[1]], "validation-metric-inactive"),
      validation_metric_card("Total rows", s$n_rows[[1]], "validation-metric-total")
    )
  })

  output$validation_table <- DT::renderDataTable({
    req(rv$validation)
    rs <- rv$validation$row_status
    display_names <- c(
      row_id = "Row",
      graph_name = "Graph",
      active = "Active",
      check_status = "Status",
      can_build = "Build",
      messages = "Details"
    )
    for (nm in names(display_names)) {
      if (nm %in% names(rs)) {
        names(rs)[names(rs) == nm] <- display_names[[nm]]
      }
    }
    DT::datatable(
      rs,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        dom = "t"
      ),
      rownames = FALSE
    ) |>
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          c("valid", "warning", "error", "inactive"),
          c("#d4edda", "#fff3cd", "#f8d7da", "#e2e3e5")
        )
      )
  })

  gallery_output_id <- function(nm) {
    paste0("gallery_plot_", gsub("[^A-Za-z0-9]", "_", nm))
  }

  cancel_gallery_build_job <- function() {
    job <- session$userData$gallery_build_job
    if (!is.null(job)) {
      job$cancelled <- TRUE
    }
  }

  start_incremental_gallery_build <- function(can_build_ids,
                                              row_meta,
                                              build_graphplan,
                                              build_country_iso3c) {
    if (!requireNamespace("later", quietly = TRUE)) {
      showNotification(
        "Incremental gallery build requires the later package (bundled with shiny).",
        type = "error"
      )
      return(invisible(FALSE))
    }

    cancel_gallery_build_job()

    n_build <- length(can_build_ids)
    job <- list(
      cancelled = FALSE,
      i = 0L,
      n = n_build,
      can_build_ids = can_build_ids,
      row_meta = row_meta,
      graphplan = build_graphplan,
      country_iso3c = build_country_iso3c,
      built = list(),
      t0 = proc.time(),
      progress = shiny::Progress$new(
        session,
        min = 0,
        max = n_build
      )
    )
    job$progress$set(
      0,
      message = "Building graphs...",
      detail = glue::glue("Preparing {n_build} graph(s)...")
    )
    session$userData$gallery_build_job <- job
    rv$built <- list()

    build_step <- function() {
      shiny::withReactiveDomain(session, {
        job <- session$userData$gallery_build_job
        if (is.null(job) || isTRUE(job$cancelled)) {
          if (!is.null(job)) job$progress$close()
          session$userData$gallery_build_job <- NULL
          return()
        }

        job$i <- job$i + 1L
        idx <- job$i

        if (idx > job$n) {
          job$progress$close()
          rv$built <- job$built
          session$userData$gallery_build_job <- NULL
          if (plotter_profile_enabled()) {
            message(sprintf(
              "[profile] gallery.build_valid_batch (%d rows): %.2fs",
              job$n,
              (proc.time() - job$t0)[["elapsed"]]
            ))
          }
          showNotification(
            paste("Built", length(job$built), "graph(s)."),
            type = "message"
          )
          return()
        }

        rid <- job$can_build_ids[[idx]]
        gname <- job$row_meta$graph_name[[idx]] %||% paste0("row_", rid)
        job$progress$set(
          idx - 1L,
          message = "Building graphs...",
          detail = glue::glue("Graph {idx}/{job$n}: row {rid} — {gname}")
        )

        row <- job$graphplan[rid, , drop = FALSE]
        item <- build_graph_row(
          graphplan_row = row,
          FD = FD,
          country_iso3c = job$country_iso3c,
          peers_fname = peers_fname
        )
        key <- item$graph_name %||% paste0("row_", rid)
        job$built[[key]] <- c(
          item,
          list(row_id = rid, status = if (isTRUE(item$ok)) "ok" else "error")
        )
        rv$built <- job$built
        session$userData$gallery_build_job <- job

        job$progress$set(
          idx,
          message = "Building graphs...",
          detail = glue::glue("Done {idx}/{job$n}: {gname}")
        )

        later::later(build_step, delay = 0.05)
      })
    }

    later::later(build_step, delay = 0)
    invisible(TRUE)
  }

  observeEvent(input$build_valid_btn, {
    shiny::validate(
      shiny::need(!is.null(rv$validation), "Run validation first."),
      shiny::need(!is.null(rv$country_iso3c), "Select a country and validate.")
    )
    row_ids <- rv$validation$row_status |>
      dplyr::filter(.data$can_build) |>
      dplyr::pull(.data$row_id)
    if (length(row_ids) == 0) {
      showNotification("No buildable rows.", type = "warning")
      return()
    }

    navigate_to_gallery_tab()
    build_row_ids <- row_ids
    build_graphplan <- rv$graphplan
    build_validation <- rv$validation
    build_country_iso3c <- rv$country_iso3c

    run_after_tab_switch({
      can_build_ids <- build_validation$row_status |>
        dplyr::filter(.data$can_build, .data$row_id %in% build_row_ids) |>
        dplyr::pull(.data$row_id)
      if (length(can_build_ids) == 0) {
        showNotification("No buildable rows.", type = "warning")
        return()
      }
      row_meta <- build_validation$row_status |>
        dplyr::filter(.data$row_id %in% can_build_ids) |>
        dplyr::arrange(match(.data$row_id, can_build_ids))

      start_incremental_gallery_build(
        can_build_ids      = can_build_ids,
        row_meta           = row_meta,
        build_graphplan    = build_graphplan,
        build_country_iso3c = build_country_iso3c
      )
    })
  })

  gallery_thumb_height <- "260px"

  gallery_action_js <- function(action, graph_name) {
    payload <- jsonlite::toJSON(
      list(action = action, name = graph_name, t = as.numeric(Sys.time())),
      auto_unbox = TRUE
    )
    esc <- gsub("\\\\", "\\\\\\\\", payload, fixed = TRUE)
    esc <- gsub("'", "\\\\'", esc, fixed = TRUE)
    glue::glue(
      "Shiny.setInputValue('gallery_action', '{esc}', {{priority: 'event'}}); return false;"
    )
  }

  normalize_gallery_action <- function(act) {
    if (is.null(act)) return(NULL)
    action <- act$action %||% act[["action"]]
    name <- act$name %||% act[["name"]]
    if (is.null(action) || is.null(name)) return(NULL)
    list(
      action = as.character(action)[1],
      name   = as.character(name)[1]
    )
  }

  parse_gallery_action <- function(raw) {
    if (is.null(raw)) return(NULL)
    raw_chr <- trimws(as.character(raw))
    if (!nzchar(raw_chr)) return(NULL)
    if (is.list(raw) && !is.null(raw$action)) {
      return(normalize_gallery_action(raw))
    }
    tryCatch(
      normalize_gallery_action(
        jsonlite::fromJSON(raw_chr, simplifyVector = FALSE)
      ),
      error = function(e) NULL
    )
  }

  open_graph_in_editor <- function(row_id, graph_name = NULL, built_item = NULL) {
    if (is.null(rv$graphplan) || nrow(rv$graphplan) < row_id) {
      showNotification("Graphplan row not found.", type = "error")
      return(invisible(FALSE))
    }

    navigate_to_editor_tab()

    edit_row_id <- row_id
    edit_graph_name <- graph_name
    edit_built_item <- built_item
    edit_graphplan <- rv$graphplan
    edit_row <- edit_graphplan[edit_row_id, , drop = FALSE]
    edit_country_iso3c <- rv$country_iso3c
    edit_plan_validation_revision <- rv$plan_validation_revision

    run_after_tab_switch({
      row <- edit_row
      rv$selected_row_id <- edit_row_id
      rv$editor_mode <- "edit"
      st <- graphplan_row_to_editor_state(row)
      apply_editor_state(session, st, indicator_choices, country_choices)
      updateSelectInput(session, "ed_ind_group", selected = "")

      if (!is.null(edit_country_iso3c)) {
        rv$editor_row_validation <- validate_graphplan_row(
          row,
          FD,
          edit_country_iso3c,
          peers_fname,
          graphplan = edit_graphplan,
          row_id = edit_row_id,
          editor_mode = "edit"
        )
        rv$editor_validation_cache_key <- editor_validation_cache_key(
          row,
          edit_country_iso3c,
          edit_graphplan,
          row_id = edit_row_id,
          editor_mode = "edit",
          plan_validation_revision = edit_plan_validation_revision
        )
      }

      if (!is.null(edit_built_item) && isTRUE(edit_built_item$ok) && !is.null(edit_built_item$graph)) {
        rv$editor_preview <- edit_built_item
      } else if (!is.null(edit_country_iso3c)) {
        built <- build_graph_row(row, FD, edit_country_iso3c, peers_fname)
        if (isTRUE(built$ok)) {
          rv$editor_preview <- built
        } else {
          rv$editor_preview <- NULL
          showNotification(
            built$error %||% "Could not build graph in editor.",
            type = "warning",
            duration = NULL
          )
        }
      } else {
        rv$editor_preview <- NULL
        showNotification(
          "Select country on Import tab to preview the graph.",
          type = "warning"
        )
      }

      showNotification(
        glue("Editing '{edit_graph_name %||% st$graph_name_suffix}' (row {edit_row_id})."),
        type = "message"
      )
    })

    invisible(TRUE)
  }

  gallery_card_ui <- function(nm, item) {
    status_badge <- if (isTRUE(item$ok)) {
      tags$span(class = "badge gallery-status-ok", "ok")
    } else {
      tags$span(class = "badge gallery-status-err", "error")
    }
    actions <- tags$div(
      class = "gallery-card-actions",
      tags$a(
        class = "btn btn-default btn-sm", href = "#",
        onclick = gallery_action_js("edit", nm),
        "Edit"
      ),
      tags$a(
        class = "btn btn-warning btn-sm", href = "#",
        onclick = gallery_action_js("deactivate", nm),
        "Deactivate"
      ),
      if (isTRUE(item$ok)) {
        tags$a(
          class = "btn btn-info btn-sm", href = "#",
          onclick = gallery_action_js("download", nm),
          "Download"
        )
      }
    )
    if (isTRUE(item$ok)) {
      column(
        width = 4,
        tags$div(
          class = "gallery-card",
          tags$div(
            class = "gallery-card-title",
            tags$span(nm, title = nm),
            status_badge
          ),
          tags$div(
            class = "gallery-card-plot",
            plotOutput(gallery_output_id(nm), height = gallery_thumb_height)
          ),
          actions
        )
      )
    } else {
      column(
        width = 4,
        tags$div(
          class = "gallery-card gallery-card-error",
          tags$div(class = "gallery-card-title", tags$strong(nm), status_badge),
          tags$p(class = "text-danger small", item$error %||% "build failed"),
          actions
        )
      )
    }
  }

  output$gallery_ui <- renderUI({
    if (length(rv$built) == 0) {
      return(p("No graphs built yet. Validate and use \"Build valid graphs\"."))
    }
    nms <- names(rv$built)
    row_groups <- split(nms, ceiling(seq_along(nms) / 3L))
    rows <- lapply(row_groups, function(group) {
      fluidRow(class = "gallery-grid", lapply(group, function(nm) {
        gallery_card_ui(nm, rv$built[[nm]])
      }))
    })
    do.call(tagList, rows)
  })

  observe({
    for (nm in names(rv$built)) {
      local({
        plot_name <- nm
        oid <- gallery_output_id(plot_name)
        output[[oid]] <- renderPlot({
          req(rv$built[[plot_name]]$ok, rv$built[[plot_name]]$graph)
          rv$built[[plot_name]]$graph
        })
      })
    }
  })

  observeEvent(input$gallery_action, {
    act <- parse_gallery_action(input$gallery_action)
    if (is.null(act)) {
      return()
    }
    nm <- as.character(act$name %||% act[["name"]] %||% "")
    if (!nzchar(nm)) {
      showNotification("Gallery action missing graph name.", type = "error")
      return()
    }
    action <- as.character(act$action %||% act[["action"]] %||% "")
    item <- rv$built[[nm]]
    if (is.null(item)) {
      showNotification(glue("Graph '{nm}' not found in gallery cache."), type = "error")
      return()
    }

    if (identical(action, "edit")) {
      if (is.null(item$row_id)) {
        showNotification("Cannot open graph in editor (missing row id).", type = "error")
        return()
      }
      open_graph_in_editor(item$row_id, graph_name = nm, built_item = item)
      return()
    }

    if (identical(action, "deactivate")) {
      if (is.null(item$row_id) || is.null(rv$graphplan)) {
        showNotification("Cannot deactivate (no graphplan).", type = "error")
        return()
      }
      rv$graphplan <- soft_delete_row(rv$graphplan, item$row_id)
      rv$built[[nm]] <- NULL
      rv$dirty <- TRUE
      if (!is.null(rv$country_iso3c)) {
        rv$validation <- profile_step(
          "gallery.deactivate.validate_graphplan_for_app",
          validate_graphplan_for_app(
            rv$graphplan, FD, rv$country_iso3c, peers_fname
          )
        )
        rv$graphplan <- rv$validation$plan
        bump_plan_validation_revision()
        clear_editor_validation_cache()
      }
      showNotification(glue("Deactivated '{nm}'."), type = "message")
      return()
    }

    if (identical(action, "download")) {
      if (!isTRUE(item$ok)) {
        showNotification("Graph is not available for download.", type = "warning")
        return()
      }
      rv$gallery_download_name <- nm
      shinyjs::delay(10, shinyjs::click("gallery_single_download"))
      return()
    }

    showNotification(glue("Unknown gallery action: {action}"), type = "warning")
  }, ignoreInit = TRUE)

  output$gallery_single_download <- downloadHandler(
    filename = function() {
      paste0(rv$gallery_download_name %||% "graph", ".png")
    },
    content = function(file) {
      item <- rv$built[[rv$gallery_download_name]]
      req(item$ok, item$graph, item$graph_params)
      ggplot2::ggsave(
        filename = file,
        plot = item$graph,
        device = "png",
        width = item$graph_params$width,
        height = item$graph_params$height,
        units = "px",
        dpi = 150
      )
    }
  )

  collect_editor_state <- function() {
    list(
      graph_name_suffix = input$ed_graph_name_suffix,
      graph_title       = input$ed_graph_title,
      graph_type        = input$ed_graph_type,
      graph_group_short = input$ed_graph_group,
      data_frequency    = input$ed_data_frequency,
      indicators        = input$ed_indicators,
      time_fix          = input$ed_time_fix,
      peers             = input$ed_peers,
      peers_custom      = input$ed_peers_custom,
      peers_formula     = input$ed_peers_formula,
      all               = input$ed_all,
      x_log             = input$ed_x_log,
      y_log             = input$ed_y_log,
      x_min             = input$ed_x_min,
      x_max             = input$ed_x_max,
      y_min             = input$ed_y_min,
      y_max             = input$ed_y_max,
      trend_type        = input$ed_trend_type,
      index             = input$ed_index,
      recession         = input$ed_recession,
      sec_y_ind         = input$ed_sec_y_axis_ind,
      sec_y_coeff       = input$ed_sec_y_axis_coeff,
      swap_axis         = input$ed_swap_axis,
      long_legend       = input$ed_long_legend,
      vert_lab          = input$ed_vert_lab,
      short_names       = input$ed_short_names,
      theme             = input$ed_theme,
      orientation       = input$ed_orientation,
      show_title        = input$ed_show_title,
      active            = input$ed_active
    )
  }

  apply_editor_state <- function(session, state, indicator_choices, country_choices) {
    updateTextInput(session, "ed_graph_name_suffix", value = state$graph_name_suffix %||% "")
    updateTextAreaInput(session, "ed_graph_title", value = state$graph_title %||% "")
    updateSelectizeInput(session, "ed_graph_type", selected = state$graph_type)
    updateSelectizeInput(session, "ed_graph_group", selected = state$graph_group_short)
    updateSelectizeInput(session, "ed_data_frequency", selected = state$data_frequency)
    ic <- editor_indicator_choices(
      indicators_tbl,
      frequency = state$data_frequency %||% " ",
      ind_group = ""
    )
    sel_inds <- state$indicators %||% character()
    ind_choices <- ensure_indicator_choices(ic$choices, sel_inds, indicators_tbl)
    updateSelectizeInput(
      session, "ed_indicators",
      choices = ind_choices,
      selected = sel_inds
    )
    updateTextInput(session, "ed_time_fix", value = state$time_fix %||% "")
    updateSelectizeInput(session, "ed_peers", selected = state$peers)
    updateTextInput(session, "ed_peers_formula", value = state$peers_formula %||% "")
    updateSelectizeInput(session, "ed_peers_custom", selected = state$peers_custom)
    updateCheckboxInput(session, "ed_all", value = state$all)
    updateTextInput(session, "ed_x_min", value = state$x_min %||% "")
    updateTextInput(session, "ed_x_max", value = state$x_max %||% "")
    updateNumericInput(session, "ed_y_min", value = state$y_min)
    updateNumericInput(session, "ed_y_max", value = state$y_max)
    updateSelectInput(session, "ed_trend_type", selected = state$trend_type %||% "")
    updateSelectInput(session, "ed_theme", selected = state$theme %||% "ipsum")
    sec_choices <- sec_y_choices_from_indicators(indicators_tbl, sel_inds)
    sec_choices <- ensure_indicator_choices(sec_choices, state$sec_y_ind %||% character(), indicators_tbl)
    updateSelectizeInput(
      session, "ed_sec_y_axis_ind",
      choices = sec_choices,
      selected = state$sec_y_ind %||% character()
    )
    updateNumericInput(session, "ed_sec_y_axis_coeff", value = state$sec_y_coeff)
    updateCheckboxInput(session, "ed_x_log", value = state$x_log)
    updateCheckboxInput(session, "ed_y_log", value = state$y_log)
    updateCheckboxInput(session, "ed_recession", value = state$recession)
    updateCheckboxInput(session, "ed_index", value = state$index)
    updateCheckboxInput(session, "ed_swap_axis", value = state$swap_axis)
    updateCheckboxInput(session, "ed_long_legend", value = state$long_legend)
    updateCheckboxInput(session, "ed_vert_lab", value = state$vert_lab)
    updateCheckboxInput(session, "ed_short_names", value = state$short_names)
    updateSelectInput(session, "ed_orientation", selected = state$orientation)
    updateCheckboxInput(session, "ed_show_title", value = state$show_title)
    updateCheckboxInput(session, "ed_active", value = state$active)
  }

  output$editor_mode_ui <- renderUI({
    label <- if (rv$editor_mode == "edit" && !is.null(rv$selected_row_id)) {
      glue("Edit row {rv$selected_row_id}")
    } else {
      "New graph"
    }
    tags$span(class = "editor-mode-badge", label)
  })

  update_editor_indicators <- function(freq = NULL,
                                       ind_group = NULL,
                                       keep_selected = TRUE,
                                       selected_indicators = NULL) {
    if (is.null(freq)) freq <- input$ed_data_frequency
    if (is.null(ind_group)) ind_group <- input$ed_ind_group
    ic <- editor_indicator_choices(indicators_tbl, frequency = freq, ind_group = ind_group)
    selected <- if (!is.null(selected_indicators)) {
      selected_indicators
    } else if (!is.null(ic$selected)) {
      ic$selected
    } else if (isTRUE(keep_selected)) {
      input$ed_indicators
    } else {
      character()
    }
    ind_choices <- ensure_indicator_choices(ic$choices, selected, indicators_tbl)
    updateSelectizeInput(
      session, "ed_indicators",
      choices = ind_choices,
      selected = selected
    )
  }

  observeEvent(
    list(input$ed_data_frequency, input$ed_ind_group),
    update_editor_indicators(),
    ignoreInit = TRUE
  )

  observeEvent(input$ed_indicators, {
    sec_choices <- sec_y_choices_from_indicators(indicators_tbl, input$ed_indicators)
    sec_selected <- intersect(
      input$ed_sec_y_axis_ind %||% character(),
      unname(sec_choices)
    )
    sec_choices <- ensure_indicator_choices(sec_choices, sec_selected, indicators_tbl)
    updateSelectizeInput(
      session, "ed_sec_y_axis_ind",
      choices = sec_choices,
      selected = sec_selected
    )
  }, ignoreInit = TRUE)

  observe({
    if (isTRUE(rv$editor_selectize_ready)) return()
    rv$editor_selectize_ready <- TRUE
    update_editor_indicators(freq = "y", ind_group = "", keep_selected = FALSE)
  })

  observeEvent(input$ed_new_graph, {
    rv$editor_preview <- NULL
    clear_editor_validation_cache()
    rv$selected_row_id <- NULL
    rv$editor_mode <- "new"
    st <- graphplan_row_to_editor_state(
      tibble::tibble(
        graph_name = "ec_newgraph", graph_title = "Graph Title",
        graph_type = graph_types[1], graph_group = "macro",
        data_frequency = "y", indicators = "gdp_g", active = 1L
      )
    )
    apply_editor_state(session, st, indicator_choices, country_choices)
    updateTextInput(session, "ed_graph_plan_tsv", value = "")
    updateSelectInput(session, "ed_ind_group", selected = "")
    update_editor_indicators(
      freq = st$data_frequency,
      ind_group = "",
      selected_indicators = st$indicators
    )
  })

  observeEvent(input$ed_fill_btn, {
    st <- collect_editor_state()
    st <- apply_editor_fill_defaults(st)
    apply_editor_state(session, st, indicator_choices, country_choices)
    clear_editor_validation_cache()
  })

  observeEvent(input$ed_import_row_btn, {
    req(nzchar(input$ed_graph_plan_tsv))
    tryCatch({
      row <- parse_graphplan_row_tsv(input$ed_graph_plan_tsv)
      st <- graphplan_row_to_editor_state(row)
      apply_editor_state(session, st, indicator_choices, country_choices)
      updateSelectInput(session, "ed_ind_group", selected = "")
      update_editor_indicators(
        freq = st$data_frequency,
        ind_group = "",
        selected_indicators = st$indicators
      )
      clear_editor_validation_cache()
      showNotification("Row imported into editor.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
    })
  })

  observeEvent(input$ed_export_row_btn, {
    tryCatch({
      row <- editor_inputs_to_graphplan_row(collect_editor_state(), dict = FD$dict)
      updateTextInput(session, "ed_graph_plan_tsv", value = graphplan_row_to_tsv(row))
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error")
    })
  })

  observeEvent(input$ed_plot_btn, {
    shiny::validate(
      shiny::need(!is.null(rv$country_iso3c), "Select country on Import tab and validate."),
      shiny::need(!is.null(input$ed_graph_type), "Choose graph type.")
    )
    row <- editor_inputs_to_graphplan_row(collect_editor_state(), dict = FD$dict)
    validation_cache_key <- editor_validation_cache_key(
      row,
      rv$country_iso3c,
      rv$graphplan,
      row_id = rv$selected_row_id,
      editor_mode = rv$editor_mode,
      plan_validation_revision = rv$plan_validation_revision
    )
    validation_cache_hit <- !is.null(rv$editor_row_validation) &&
      !is.null(rv$editor_validation_cache_key) &&
      identical(rv$editor_validation_cache_key, validation_cache_key)

    if (validation_cache_hit) {
      if (plotter_profile_enabled()) {
        message("[profile] editor.update_plot.validate_graphplan_row: skipped (cache)")
      }
    } else {
      rv$editor_row_validation <- profile_step(
        "editor.update_plot.validate_graphplan_row",
        validate_graphplan_row(
          row,
          FD,
          rv$country_iso3c,
          peers_fname,
          graphplan = rv$graphplan,
          row_id = rv$selected_row_id,
          editor_mode = rv$editor_mode
        )
      )
      rv$editor_validation_cache_key <- validation_cache_key
    }

    built <- profile_step(
      "editor.update_plot.build_graph_row",
      build_graph_row(row, FD, rv$country_iso3c, peers_fname)
    )
    if (!built$ok) {
      showNotification(built$error, type = "error", duration = NULL)
      rv$editor_preview <- NULL
      return()
    }
    rv$editor_preview <- built
  })

  output$ed_row_validation <- renderPrint({
    rv$editor_row_validation
  })

  output$ed_graph_plot <- renderPlot({
    req(rv$editor_preview$ok, rv$editor_preview$graph)
    rv$editor_preview$graph
  })

  output$ed_download_png <- downloadHandler(
    filename = function() {
      g <- rv$editor_preview$graph_params$graph_name %||% "graph"
      paste0(g, ".png")
    },
    content = function(file) {
      gp <- rv$editor_preview$graph_params
      ggplot2::ggsave(
        filename = file, plot = rv$editor_preview$graph,
        device = "png", width = gp$width, height = gp$height, units = "px", dpi = 150
      )
    }
  )

  output$ed_download_data <- downloadHandler(
    filename = function() {
      paste0(rv$editor_preview$graph_params$graph_name %||% "graph", "_data.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(rv$editor_preview$data, path = file)
    }
  )

  observeEvent(input$ed_save_row, {
    shiny::validate(
      shiny::need(!is.null(rv$country_iso3c), "Select country and validate on Import tab."),
      shiny::need(!is.null(rv$graphplan) || rv$editor_mode == "new", "Load a graphplan or create a new graph.")
    )
    if (is.null(rv$graphplan)) rv$graphplan <- empty_graphplan()

    row <- editor_inputs_to_graphplan_row(collect_editor_state(), dict = FD$dict)
    row_val <- validate_graphplan_row(
      row,
      FD,
      rv$country_iso3c,
      peers_fname,
      graphplan = rv$graphplan,
      row_id = rv$selected_row_id,
      editor_mode = rv$editor_mode
    )
    if (!is.null(row_val) && !isTRUE(row_val$can_build)) {
      showNotification(
        glue("Row not saved: {row_val$messages %||% 'validation failed'}"),
        type = "error", duration = NULL
      )
      return()
    }

    save_editor_mode <- rv$editor_mode
    save_selected_row_id <- rv$selected_row_id
    save_country_iso3c <- rv$country_iso3c
    save_row <- row
    save_graphplan <- rv$graphplan
    save_built <- rv$built
    save_plan_revision <- rv$plan_validation_revision %||% 0L
    save_next_revision <- save_plan_revision + 1L

    navigate_to_gallery_tab()

    run_after_tab_switch({
      save_msg <- NULL
      gallery_refresh <- NULL
      withProgress(message = "Saving to graphplan...", value = 0, {
        save_result <- profile_step("save.end_to_end", {
          incProgress(0.15, detail = "Updating graphplan row")
          graphplan <- save_graphplan

          if (save_editor_mode == "edit" && !is.null(save_selected_row_id)) {
            graphplan <- update_graphplan_row(graphplan, save_selected_row_id, save_row)
            saved_row_id <- save_selected_row_id
            save_msg <- "Graphplan row updated."
          } else {
            graphplan <- append_graphplan_row(graphplan, save_row)
            saved_row_id <- nrow(graphplan)
            save_msg <- "New row appended to graphplan."
          }

          incProgress(0.45, detail = "Validating graphplan")
          validation <- profile_step(
            "save.validate_graphplan_for_app",
            validate_graphplan_for_app(
              graphplan, FD, save_country_iso3c, peers_fname
            )
          )
          graphplan <- validation$plan

          incProgress(0.75, detail = "Updating gallery")
          gallery_refresh <- profile_step(
            "save.refresh_gallery_built_for_row",
            refresh_gallery_built_for_row(
              built_list    = save_built,
              row_id        = saved_row_id,
              graphplan     = graphplan,
              FD            = FD,
              country_iso3c = save_country_iso3c,
              peers_fname   = peers_fname
            )
          )

          list(
            graphplan = graphplan,
            saved_row_id = saved_row_id,
            validation = validation,
            gallery_refresh = gallery_refresh,
            save_msg = save_msg
          )
        })

        save_msg <- save_result$save_msg
        rv$graphplan <- save_result$graphplan
        rv$selected_row_id <- save_result$saved_row_id
        rv$editor_mode <- "edit"
        rv$dirty <- TRUE
        rv$validation <- save_result$validation
        gallery_refresh <- save_result$gallery_refresh
        rv$built <- gallery_refresh$built_list
        rv$editor_preview <- gallery_refresh$editor_preview
        rv$plan_validation_revision <- save_next_revision
        rv$editor_validation_cache_key <- NULL
        rv$editor_row_validation <- NULL
        incProgress(1)
      })
      showNotification(save_msg, type = "message")
      if (!is.null(gallery_refresh) && !is.na(gallery_refresh$error)) {
        showNotification(
          gallery_refresh$error %||% "Row saved but graph could not be rebuilt for gallery.",
          type = "warning"
        )
      }
    })
  })

  observe({
    if (isTRUE(rv$editor_bootstrapped)) return()
    rv$editor_bootstrapped <- TRUE
    if (is.null(rv$graphplan)) {
      rv$editor_mode <- "new"
      st <- graphplan_row_to_editor_state(
        tibble::tibble(
          graph_name = "ec_newgraph",
          graph_title = "Graph Title",
          graph_type = graph_types[1],
          graph_group = "macro",
          data_frequency = "y",
          indicators = "gdp_g",
          active = 1L
        )
      )
      apply_editor_state(session, st, indicator_choices, country_choices)
    }
  })

  observe({
    built_names <- names(rv$built)
    selected <- input$export_selected_graphs %||% character()
    selected <- intersect(selected, built_names)
    updateSelectizeInput(
      session,
      "export_selected_graphs",
      choices = built_names,
      selected = selected
    )
  })

  export_built_for_download <- function() {
    scope <- input$export_scope %||% "all_built"
    selected <- input$export_selected_graphs %||% character()
    if (identical(scope, "gallery_selected") && length(selected) == 0) {
      shiny::validate(shiny::need(FALSE, "Select at least one graph for export."))
    }
    filter_built_graphs(
      built = rv$built,
      scope = scope,
      validation = rv$validation,
      selected_names = selected
    )
  }

  output$export_report <- renderPrint({
    compute_export_report(
      graphplan      = rv$graphplan,
      validation     = rv$validation,
      built          = rv$built,
      dirty          = rv$dirty,
      baseline       = rv$graphplan_baseline,
      export_scope   = input$export_scope %||% "all_built",
      selected_names = input$export_selected_graphs %||% character()
    )
  })

  output$download_graphs_zip <- downloadHandler(
    filename = function() paste0("graphs_", Sys.Date(), ".zip"),
    content = function(file) {
      scoped <- export_built_for_download()
      shiny::validate(shiny::need(length(scoped) > 0, "No graphs match the export scope."))
      export_built_graphs_zip(
        built    = scoped,
        zip_path = file,
        device   = input$export_device
      )
    }
  )

  output$download_graph_data_xlsx <- downloadHandler(
    filename = function() paste0("graph_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      scoped <- export_built_for_download()
      shiny::validate(shiny::need(length(scoped) > 0, "No graphs match the export scope."))
      export_graph_data_xlsx(scoped, path = file)
      shiny::validate(shiny::need(file.exists(file) && file.info(file)$size > 0,
                                  "No graph data available for export."))
    }
  )

  output$download_graphplan_xlsx <- downloadHandler(
    filename = function() "2_graphlib.xlsx",
    content = function(file) {
      shiny::validate(shiny::need(!is.null(rv$graphplan), "No graphplan to export."))
      info <- rv$graphplan_info
      if (is.null(info)) info <- default_graphplan_info()
      export_graphplan_xlsx(
        plan = rv$graphplan,
        path = file,
        info = info
      )
    }
  )

  output$download_recipes_tsv <- downloadHandler(
    filename = function() paste0("graphplan_recipes_", Sys.Date(), ".tsv"),
    content = function(file) {
      shiny::validate(shiny::need(!is.null(rv$graphplan), "No graphplan to export."))
      text <- export_graphplan_recipes_text(
        plan = rv$graphplan,
        include_inactive = !isTRUE(input$export_recipes_active_only)
      )
      shiny::validate(shiny::need(nzchar(text), "No rows to export as recipes."))
      writeLines(text, con = file, useBytes = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
