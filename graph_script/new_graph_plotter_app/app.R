# Dependencies: see DEPLOY.md (Connect Cloud allowlist vs self-hosted manifest).
# Cloud-minimal plotting_libs — legacy graph_plotter packages removed (reshape2, etc.).

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
  "ggplot2", "countrycode", "readxl", "tidyr", "writexl", "ggtext",
  "stringr", "purrr", "tibble", "cli"
)
for (lib in plotting_libs) {
  suppressPackageStartupMessages(library(lib, character.only = TRUE))
}

here::i_am("app.R")
source(here("deploy_paths.R"))
source(here("check_graphplan.R"))
source(here("prepare_elements.R"))
source(here("plot_themes.R"))
source(here("plot_types.R"))
source(here("service.R"))

# Constants aligned with graph_script/do_plot.R (also defined in service.R for helpers)
sheet_keys <- c(y = "y", q = "q", m = "m")
deploy_paths <- assert_deploy_data_files(resolve_deploy_paths())
peers_fname  <- deploy_paths$peers_fname
data_fname   <- deploy_paths$data_fname
data_d_fname <- deploy_paths$data_d_fname

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

country_iso3c_from_choice_if_valid <- function(country_choice) {
  country_id <- as.character(country_choice)[1]
  if (is.null(country_id) || is.na(country_id) || !nzchar(country_id)) {
    return(NULL)
  }
  
  iso3 <- normalize_iso3_strict(country_iso3c_from_id(country_id) %||% "")
  if (!is_valid_iso3c_scalar(iso3)) {
    return(NULL)
  }
  
  iso3[[1]]
}

country_label_from_id <- function(country_id, choices = country_choices) {
  cid <- as.character(country_id)[1]
  if (is.null(cid) || is.na(cid) || !nzchar(cid)) {
    return(NA_character_)
  }
  idx <- match(cid, choices, nomatch = 0L)
  if (idx < 1L) {
    return(cid)
  }
  nm <- names(choices)[idx]
  if (is.null(nm) || !nzchar(nm)) cid else nm
}

country_label_from_iso3c <- function(iso3c, choices = country_choices) {
  iso3 <- as.character(iso3c)[1]
  if (is.null(iso3) || is.na(iso3) || !nzchar(iso3)) {
    return(NA_character_)
  }
  cid <- countrycode::countrycode(iso3, "iso3c", "iso2c", warn = FALSE)
  if (is.null(cid) || is.na(cid) || !nzchar(cid)) {
    return(NA_character_)
  }
  country_label_from_id(cid, choices)
}

indicator_catalog <- build_indicator_catalog_from_dict(FD$dict)

indicator_choices <- stats::setNames(
  indicator_catalog$indicator_code,
  indicator_catalog$label
)

graph_group_choices <- stats::setNames(unname(graph_groups), names(graph_groups))

ensure_indicator_choices <- function(choices, codes, indicator_catalog) {
  codes <- codes[!is.na(codes) & nzchar(codes)]
  missing <- setdiff(codes, unname(choices))
  if (!length(missing)) return(choices)
  extra_rows <- indicator_catalog |>
    dplyr::filter(.data$indicator_code %in% missing) |>
    dplyr::distinct(.data$indicator_code, .keep_all = TRUE)
  c(choices, stats::setNames(extra_rows$indicator_code, extra_rows$label))
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
      .graphplan-bulk-toolbar {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 0.35rem 0.5rem;
        margin-top: 0.5rem;
      }
      .graphplan-bulk-toolbar .btn { margin: 0; }
      .gallery-build-row .btn { margin: 0; }
      .editor-preview-card .editor-toolbar-row {
        display: flex;
        flex-wrap: wrap;
        align-items: flex-end;
        gap: 0.45rem 0.6rem;
        margin-bottom: 0.65rem;
        max-width: 100%;
        overflow-x: hidden;
      }
      .editor-toolbar-actions {
        display: flex;
        flex-wrap: nowrap;
        gap: 0.35rem;
        flex: 0 0 auto;
      }
      .editor-toolbar-actions .btn {
        margin: 0;
        white-space: nowrap;
        font-size: 0.875rem;
        padding: 0.375rem 0.55rem;
      }
      .editor-toolbar-tsv {
        flex: 1 1 10rem;
        min-width: 0;
        max-width: 100%;
      }
      .editor-toolbar-tsv .form-group {
        margin-bottom: 0;
      }
      .editor-toolbar-country {
        flex: 0 1 auto;
        min-width: 0;
        max-width: min(12rem, 35%);
      }
      .editor-toolbar-country .editor-country-context,
      .editor-toolbar-country .shiny-text-output {
        display: block;
        margin: 0;
        padding-bottom: 2px;
        min-height: 34px;
        line-height: 34px;
        font-size: 0.85rem;
        font-weight: 600;
        color: #495057;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .gallery-build-row .shiny-text-output {
        margin: 0;
        color: #6c757d;
        font-size: 0.9rem;
        line-height: 1.4;
      }
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
      .import-summary-cards--two-rows {
        flex-direction: column;
        flex-wrap: nowrap;
      }
      .validation-summary-row {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem;
        width: 100%;
      }
      .validation-summary-row .validation-metric-card {
        flex: 1 1 calc(50% - 0.375rem);
        min-width: 7rem;
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
      /* Import validation DT (phase 17.3): active row tint; Status/Details keep priority styles */
      #validation_table table.dataTable tbody tr.import-validation-row-active td:not(.dt-col-status) {
        box-shadow: inset 0 0 0 9999px rgba(13, 110, 253, 0.09);
      }
      #validation_table table.dataTable tbody tr.import-validation-row-error td.dt-col-details {
        background-color: #f8d7da !important;
        color: #721c24;
      }
      .gallery-status-import-error { background: #dc3545; }
      .gallery-card-badges {
        display: inline-flex;
        flex-wrap: wrap;
        gap: 0.35rem;
        align-items: center;
        margin-left: auto;
        flex-shrink: 0;
      }
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
        flex: 1 1 100%;
      }
      .editor-preview-plot {
        width: 100%;
        max-width: 100%;
        overflow: hidden;
        border-radius: 4px;
        background: #f8f9fa;
        margin-bottom: 0.75rem;
        line-height: 0;
        text-align: center;
      }
      .editor-preview-plot--empty {
        min-height: 0;
        padding: 0;
        background: transparent;
      }
      .editor-preview-plot .shiny-image-output {
        display: inline-block;
        max-width: 100%;
        max-height: var(--editor-preview-max-h);
        margin: 0 auto;
        overflow: hidden;
      }
      .editor-preview-plot img {
        max-width: 100%;
        max-height: var(--editor-preview-max-h);
        object-fit: contain;
        display: block;
        margin: 0 auto;
      }
      .editor-layout.bslib-sidebar-layout {
        margin-top: 0;
      }
      .editor-layout .editor-sidebar,
      .editor-layout .editor-sidebar > .sidebar-content {
        overflow-x: hidden;
        max-width: 100%;
      }
      .editor-layout .editor-sidebar .form-group,
      .editor-layout .editor-sidebar .selectize-control {
        max-width: 100%;
        min-width: 0;
      }
      .editor-layout .editor-sidebar .bslib-grid {
        margin-left: 0;
        margin-right: 0;
      }
      .editor-layout .editor-main {
        display: flex;
        flex-direction: column;
        gap: 0.75rem;
        min-width: 0;
        padding-top: 0;
      }
      .editor-preview-card .card-body {
        padding: 0.5rem 0.75rem;
        background: #f8f9fa;
      }
      .editor-preview-card .editor-preview-plot {
        margin-bottom: 0;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        background: #fff;
      }
      .editor-input-label {
        display: inline-flex;
        align-items: center;
        gap: 0.2rem;
        flex-wrap: wrap;
      }
      .editor-hint-btn {
        font-size: 0.75rem;
        font-weight: 700;
        line-height: 1;
        min-width: 1.1rem;
        text-decoration: none !important;
        color: #6c757d !important;
      }
      .editor-hint-btn:hover { color: #0d6efd !important; }
    ")),
    tags$style(HTML(glue(
      ".editor-preview-plot {{",
      "  --editor-preview-max-h: min({editor_preview_max_height_px}px, calc(100vh - {editor_preview_vh_chrome_rem}rem));",
      "}}",
      ".editor-preview-plot .shiny-image-output,",
      ".editor-preview-plot img {{",
      "  max-height: var(--editor-preview-max-h);",
      "}}",
      .sep = "\n"
    )))
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
          div(
            class = "graphplan-bulk-toolbar",
            actionButton("validate_btn", "Validate", class = "btn-primary"),
            actionButton("import_make_all_active_btn", "Make all active"),
            actionButton("import_make_all_inactive_btn", "Make all inactive"),
            actionButton("import_new_graph_btn", "New graph", class = "btn-default")
          )
        ),
        column(
          6,
          class = "import-summary-col",
          uiOutput("validation_summary_ui")
        )
      ),
      fluidRow(
        column(12, DT::dataTableOutput("validation_table"))
      ),
      div(
        style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;",
        textInput("validation_row_action", label = NULL, value = "")
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
          display: flex;
          align-items: center;
          gap: 0.35rem;
          font-size: 0.85rem;
          font-weight: 600;
          margin: 0 0 8px;
          min-width: 0;
        }
        .gallery-card-title .gallery-card-name {
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
          min-width: 0;
          flex: 1 1 auto;
        }
        .gallery-card-plot {
          width: 100%;
          min-height: 260px;
          overflow: hidden;
          border-radius: 4px;
          background: #f8f9fa;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        .gallery-card-plot .shiny-image-output {
          display: flex;
          align-items: center;
          justify-content: center;
          max-width: 100%;
          max-height: 260px;
          height: 260px;
          width: 100%;
          margin: 0;
        }
        .gallery-card-plot img,
        .gallery-card-plot .shiny-image-output img {
          max-height: 260px;
          width: auto;
          max-width: 100%;
          height: auto;
          object-fit: contain;
          display: block;
          margin: 0;
        }
        .gallery-build-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 0.75rem 1rem;
          margin: 0 0 0.75rem;
        }
        .gallery-build-row-main {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 0.75rem 1rem;
          flex: 1 1 auto;
          min-width: 0;
        }
        .gallery-view-toggle {
          margin-left: auto;
          flex: 0 0 auto;
        }
        .gallery-view-toggle .control-label {
          display: none;
        }
        .gallery-view-toggle .shiny-options-group {
          margin: 0;
        }
        .gallery-view-toggle .radio {
          margin: 0 0 0 0.75rem;
        }
        .gallery-view-toggle .radio label {
          font-size: 0.875rem;
          font-weight: 500;
        }
        .gallery-layout-fullsize .gallery-grid > [class*='col-'] {
          flex: 0 0 100%;
          max-width: 100%;
          width: 100%;
        }
        .gallery-layout-fullsize .gallery-card {
          height: auto;
        }
        .gallery-layout-fullsize .gallery-card-plot {
          min-height: 280px;
        }
        .gallery-layout-fullsize .gallery-card-plot .shiny-image-output {
          max-height: min(72vh, 920px);
          height: auto !important;
        }
        .gallery-layout-fullsize .gallery-card-plot img,
        .gallery-layout-fullsize .gallery-card-plot .shiny-image-output img {
          max-height: min(72vh, 920px);
          width: auto;
          max-width: 100%;
        }
        .gallery-build-row .shiny-text-output {
          margin: 0;
          color: #6c757d;
          font-size: 0.9rem;
          line-height: 1.4;
        }
        .gallery-card-error { margin-bottom: 18px; }
        .gallery-card-actions { margin-top: 6px; display: flex; flex-wrap: wrap; gap: 4px; }
        .gallery-card-actions .btn { font-size: 0.75rem; padding: 2px 8px; }
        .gallery-status-built { background: #198754; }
        .gallery-status-err { background: #dc3545; }
        .gallery-status-warning { background: #ffc107; color: #212529; }
        .gallery-status-not-built { background: #6c757d; }
        .gallery-status-inactive { background: #adb5bd; color: #212529; }
        .gallery-card-placeholder {
          min-height: 4rem;
          padding: 0.5rem 0.25rem;
          font-size: 0.8rem;
          color: #6c757d;
        }
        .gallery-inactive-section {
          margin-top: 1.5rem;
          padding-top: 1rem;
          border-top: 2px solid #dee2e6;
        }
        .gallery-inactive-heading {
          font-size: 1rem;
          font-weight: 600;
          margin: 0 0 0.75rem;
          color: #495057;
        }
        .gallery-card-inactive {
          background: #f8f9fa;
          border-color: #dee2e6;
        }
        #validation_table tbody tr { cursor: pointer; }
      ")),
      div(
        style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;",
        textInput("gallery_action", label = NULL, value = "")
      ),
      div(
        style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;",
        downloadButton("gallery_single_download", "Download graph")
      ),
      tags$div(
        class = "gallery-build-row",
        tags$div(
          class = "gallery-build-row-main",
          actionButton("build_valid_btn", "Build active and valid", class = "btn-primary"),
          actionButton("gallery_make_all_active_btn", "Make all active"),
          actionButton("gallery_make_all_inactive_btn", "Make all inactive"),
          actionButton("gallery_new_graph_btn", "New graph", class = "btn-default"),
          textOutput("gallery_build_status")
        ),
        tags$div(
          class = "gallery-view-toggle",
          radioButtons(
            "gallery_view_mode",
            label = NULL,
            choices = c("Preview" = "preview", "Full size" = "fullsize"),
            selected = "preview",
            inline = TRUE
          )
        )
      ),
      uiOutput("gallery_ui")
    )
  ),

  bslib::nav_panel(
    title = "Graph editor",
    value = "tab_editor",
    bslib::layout_sidebar(
      class = "editor-layout",
      fillable = TRUE,
      sidebar = bslib::sidebar(
        class = "editor-sidebar",
        width = 460,
        padding = "0.75rem",
        div(
          class = "editor-mode-toolbar",
          actionButton("ed_new_graph", "New graph", class = "btn-default"),
          actionButton("ed_save_row", "Save to graphplan", class = "btn-primary"),
          uiOutput("editor_mode_ui")
        ),
        bslib::accordion(
          id = "editor_params_accordion",
          open = c("Basic", "Indicators", "Peers", "Style"),
          multiple = TRUE,
          bslib::accordion_panel(
            "Basic",
            selectizeInput(
              "ed_graph_type",
              label = editor_input_label("Graph type", "ed_graph_type"),
              choices = graph_types,
              selected = editor_default_graph_type,
              options = list(placeholder = "Select graph type")
            ),
            bslib::layout_columns(
              col_widths = c(4, 8),
              selectizeInput(
                "ed_data_frequency", "Data freq",
                choices = c(" ", "y", "q", "m", "d")
              ),
              textInput(
                "ed_time_fix",
                label = editor_input_label("Time fix", "ed_time_fix"),
                value = ""
              )
            )
          ),
          bslib::accordion_panel(
            "Indicators",
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
            )
          ),
          bslib::accordion_panel(
            "Peers",
            bslib::layout_columns(
              col_widths = c(5, 7),
              selectizeInput(
                "ed_peers",
                label = editor_input_label("Peer group", "ed_peers"),
                choices = peers_choice,
                selected = editor_default_peers
              ),
              textInput(
                "ed_peers_formula",
                label = editor_input_label("Formula", "ed_peers_formula"),
                value = ""
              )
            ),
            selectizeInput(
              "ed_peers_custom",
              label = editor_input_label("Custom peers (ISO2)", "ed_peers_custom"),
              choices = country_choices,
              multiple = TRUE,
              options = list(placeholder = "Custom peer countries")
            ),
            checkboxInput("ed_all", "Show all countries", FALSE)
          ),
          bslib::accordion_panel(
            "Style",
            bslib::layout_columns(
              col_widths = c(6, 6),
              textInput(
                "ed_x_min",
                label = editor_input_label("X min", "ed_x_min"),
                value = ""
              ),
              textInput(
                "ed_x_max",
                label = editor_input_label("X max", "ed_x_max"),
                value = ""
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              textInput("ed_y_min", "Y min", ""),
              textInput("ed_y_max", "Y max", "")
            ),
            bslib::layout_columns(
              col_widths = c(4, 8),
              selectInput(
                "ed_trend_type",
                label = editor_input_label("Trend", "ed_trend_type"),
                choices = trend_types_ui,
                selected = ""
              ),
              selectInput(
                "ed_theme",
                "Style preset",
                choices = theme_types,
                selected = editor_default_theme
              )
            ),
            bslib::layout_columns(
              col_widths = c(8, 4),
              selectizeInput(
                "ed_sec_y_axis_ind",
                label = editor_input_label("2nd Y-axis", "ed_sec_y_axis_ind"),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Optional", maxOptions = 200)
              ),
              numericInput(
                "ed_sec_y_axis_coeff",
                label = editor_input_label("Axis mult", "ed_sec_y_axis_coeff"),
                value = NA,
                width = "100%"
              )
            ),
            bslib::layout_columns(
              col_widths = c(4, 3, 5),
              checkboxInput("ed_x_log", "X log", FALSE),
              checkboxInput("ed_y_log", "Y log", FALSE),
              checkboxInput(
                "ed_swap_axis",
                label = editor_input_label("Swap axis", "ed_swap_axis"),
                value = FALSE
              )
            ),
            bslib::layout_columns(
              col_widths = c(4, 3, 5),
              checkboxInput(
                "ed_recession",
                label = editor_input_label("Recession", "ed_recession"),
                value = FALSE
              ),
              checkboxInput(
                "ed_index",
                label = editor_input_label("Index", "ed_index"),
                value = FALSE
              ),
              checkboxInput(
                "ed_long_legend",
                label = editor_input_label("Long legend", "ed_long_legend"),
                value = FALSE
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              checkboxInput(
                "ed_short_names",
                label = editor_input_label("Short indicator names", "ed_short_names"),
                value = FALSE
              ),
              checkboxInput(
                "ed_vert_lab",
                label = editor_input_label("Vertical X labels", "ed_vert_lab"),
                value = FALSE
              )
            )
          )
        )
      ),
      div(
        class = "editor-main",
        bslib::card(
          class = "editor-preview-card",
          full_screen = TRUE,
          bslib::card_body(
            div(
              class = "editor-toolbar-row",
              div(
                class = "editor-toolbar-actions",
                actionButton("ed_plot_btn", "Update plot", class = "btn-primary"),
                actionButton("ed_import_row_btn", "Import row"),
                actionButton("ed_export_row_btn", "Export row")
              ),
              div(
                class = "editor-toolbar-tsv",
                textInput(
                  "ed_graph_plan_tsv",
                  label = NULL,
                  value = "",
                  placeholder = "Graph plan row"
                )
              ),
              div(
                class = "editor-toolbar-country",
                div(
                  class = "editor-country-context",
                  textOutput("editor_country_context")
                )
              )
            ),
            uiOutput("ed_graph_plot_ui")
          )
        ),
        bslib::card(
          class = "editor-downloads-card",
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              downloadButton("ed_download_png", "Download png"),
              downloadButton("ed_download_jpeg", "Download jpeg"),
              downloadButton("ed_download_data", "Download data")
            )
          )
        ),
        bslib::card(
          class = "editor-metadata-card",
          bslib::card_header("Output metadata"),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(5, 2, 3, 2),
              textAreaInput("ed_graph_title", "Graph title", "Graph Title", rows = 2),
              selectizeInput(
                "ed_graph_group",
                label = editor_input_label("Graph group", "ed_graph_group"),
                choices = graph_group_choices
              ),
              textInput(
                "ed_graph_name_suffix",
                label = editor_input_label("File name suffix", "ed_graph_name_suffix"),
                value = "goodgraph"
              ),
              selectInput(
                "ed_orientation",
                label = editor_input_label("Orientation", "ed_orientation"),
                choices = orient_types,
                selected = editor_default_orientation
              )
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              checkboxInput("ed_show_title", "Show title", TRUE),
              checkboxInput(
                "ed_active",
                label = editor_input_label("Active row", "ed_active"),
                value = TRUE
              )
            )
          )
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
          downloadButton("download_graphplan_xlsx", "Download graphplan (xlsx)", class = "btn-primary"),
          downloadButton("download_recipes_tsv", "Download recipes (TSV)"),
          checkboxInput("export_recipes_active_only", "Recipes: active rows only", FALSE)
        )
      ),
      fluidRow(
        column(
          12,
          class = "import-summary-col",
          style = "margin-top: 1.25rem;",
          uiOutput("export_summary_ui")
        )
      )
    )
  )
)

# --------------------------- Server -----------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    graphplan       = NULL,
    graphplan_info  = NULL,
    graphplan_title_row = NULL,
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
    gallery_build_progress = NULL,
    dirty           = FALSE,
    plan_validation_revision = 0L,
    editor_validation_cache_key = NULL,
    editor_touch_row_ids = integer(0),
    gallery_status_country_label = NULL,
    editor_context_country_label = NULL
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
    next_iso3c <- country_iso3c_from_choice_if_valid(input$country_choice)
    if (is.null(next_iso3c)) {
      return(invisible(NULL))
    }
    rv$country_iso3c <- next_iso3c
    bump_plan_validation_revision()
  }, ignoreNULL = FALSE)

  note_gallery_built_country <- function(country_iso3c) {
    lab <- country_label_from_iso3c(country_iso3c)
    if (!is.null(lab) && !is.na(lab) && nzchar(lab)) {
      rv$gallery_status_country_label <- lab
    }
    invisible(lab)
  }

  note_editor_plot_country <- function(country_iso3c) {
    lab <- country_label_from_iso3c(country_iso3c)
    if (!is.null(lab) && !is.na(lab) && nzchar(lab)) {
      rv$editor_context_country_label <- lab
    }
    invisible(lab)
  }

  output$editor_country_context <- renderText({
    lab <- rv$editor_context_country_label
    if (is.null(lab) || is.na(lab) || !nzchar(lab)) "—" else lab
  })

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
    withProgress(message = "Loading graphplan...", value = 0, {
      incProgress(0.12, detail = "Reading workbook")
      imported <- read_graphplan_file(input$graphplan_file$datapath, dict = FD$dict)
      incProgress(0.72, detail = "Updating session state")
      rv$graphplan <- imported$plan
      rv$graphplan_info <- imported$info
      rv$graphplan_title_row <- imported$title_row
      rv$graphplan_baseline <- graphplan_baseline_capture(imported$plan)
      rv$validation <- NULL
      rv$built <- list()
      rv$gallery_status_country_label <- NULL
      reset_gallery_thumb_cache()
      rv$dirty <- TRUE
      bump_plan_validation_revision()
      clear_editor_validation_cache()
      rv$editor_touch_row_ids <- integer(0)
      incProgress(1, detail = "Done")
    })
    showNotification("Graphplan loaded.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$validate_btn, {
    selected_iso3c <- country_iso3c_from_choice_if_valid(input$country_choice)
    shiny::validate(
      shiny::need(!is.null(rv$graphplan) && nrow(rv$graphplan) > 0, "Load or create a graphplan first."),
      shiny::need(!is.null(selected_iso3c), "Select a valid country.")
    )
    iso3 <- selected_iso3c
    rv$country_iso3c <- iso3
    withProgress(message = "Validating graphplan...", value = 0, {
      incProgress(0.12, detail = "Running row checks")
      rv$validation <- profile_step(
        "import.validate_graphplan_for_app",
        validate_graphplan_for_app(
          graphplan     = rv$graphplan,
          FD            = FD,
          country_iso3c = iso3,
          peers_fname   = peers_fname
        )
      )
      incProgress(0.78, detail = "Updating plan and gallery state")
      rv$graphplan <- rv$validation$plan
      rv$built <- prune_built_list_for_validation(rv$built, rv$validation)
      bump_plan_validation_revision()
      clear_editor_validation_cache()
      incProgress(1, detail = "Complete")
    })
    s <- rv$validation$summary
    rs <- rv$validation$row_status
    n_valid <- sum(rs$check_status == "valid", na.rm = TRUE)
    showModal(modalDialog(
      title = "Validation complete",
      tags$p(glue(
        "Active: {s$n_active[[1]]}, inactive: {s$n_inactive[[1]]}, ",
        "errors: {s$n_errors[[1]]}, buildable (valid rows): {n_valid}"
      )),
      footer = tagList(
        modalButton("Fix issues first"),
        actionButton("validate_build_continue", "Build active and valid", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$validate_build_continue, {
    removeModal()
    shinyjs::click("build_valid_btn")
  })

  unlink_built_gallery_thumbs <- function(built_list) {
    for (item in as.list(built_list %||% list())) {
      path <- item$thumb_path
      if (!is.null(path) && nzchar(path) && file.exists(path)) {
        unlink(path, force = TRUE)
      }
    }
    invisible(NULL)
  }

  set_graphplan_all_active_state <- function(make_active) {
    shiny::validate(
      shiny::need(!is.null(rv$graphplan) && nrow(rv$graphplan) > 0L,
                  "Load a graphplan first.")
    )
    n <- nrow(rv$graphplan)
    if (isTRUE(make_active)) {
      rv$graphplan <- graphplan_activate_all(rv$graphplan)
      msg <- glue("All {n} rows set active.")
    } else {
      unlink_built_gallery_thumbs(rv$built)
      rv$graphplan <- graphplan_deactivate_all(rv$graphplan)
      rv$built <- list()
      msg <- glue("All {n} rows set inactive.")
    }
    rv$dirty <- TRUE
    if (!is.null(rv$country_iso3c)) {
      rv$validation <- validate_graphplan_for_app(
        rv$graphplan, FD, rv$country_iso3c, peers_fname
      )
      rv$graphplan <- rv$validation$plan
      rv$built <- prune_built_list_for_validation(rv$built, rv$validation)
      bump_plan_validation_revision()
      clear_editor_validation_cache()
    } else {
      showNotification(
        "Select a country and Validate on Import to refresh the status table.",
        type = "warning"
      )
    }
    showNotification(msg, type = "message")
  }

  observeEvent(input$import_make_all_active_btn, {
    set_graphplan_all_active_state(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$import_make_all_inactive_btn, {
    set_graphplan_all_active_state(FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$gallery_make_all_active_btn, {
    set_graphplan_all_active_state(TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$gallery_make_all_inactive_btn, {
    set_graphplan_all_active_state(FALSE)
  }, ignoreInit = TRUE)

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
    rs <- rv$validation$row_status
    n_valid <- sum(rs$check_status == "valid", na.rm = TRUE)
    tags$div(
      class = "import-summary-cards import-summary-cards--two-rows",
      tags$div(
        class = "validation-summary-row",
        validation_metric_card("Active", s$n_active[[1]], "validation-metric-active"),
        validation_metric_card("Inactive", s$n_inactive[[1]], "validation-metric-inactive")
      ),
      tags$div(
        class = "validation-summary-row",
        validation_metric_card("Errors", s$n_errors[[1]], "validation-metric-errors"),
        validation_metric_card("Buildable", n_valid, "validation-metric-buildable")
      )
    )
  })

  output$validation_table <- DT::renderDataTable({
    req(rv$validation)
    req(rv$graphplan)
    rs <- rv$validation$row_status
    rs$Built <- vapply(
      rs$row_id,
      function(rid) graphplan_row_built_in_gallery(rv$built, rid),
      logical(1)
    )
    rs$Edited <- vapply(
      rs$row_id,
      function(rid) {
        graphplan_row_edited_hybrid(
          rid,
          rv$graphplan,
          rv$graphplan_baseline,
          rv$editor_touch_row_ids
        )
      },
      logical(1)
    )
    col_order <- c(
      "row_id", "graph_name", "active", "check_status", "can_build",
      "Built", "limits", "peers", "Edited", "messages"
    )
    rs <- rs[, intersect(col_order, names(rs)), drop = FALSE]
    display_names <- c(
      row_id = "Row",
      graph_name = "Graph",
      active = "Active",
      check_status = "Status",
      can_build = "Planned to build",
      Built = "Built",
      limits = "Limits",
      peers = "Peers",
      Edited = "Edited",
      messages = "Details"
    )
    for (nm in names(display_names)) {
      if (nm %in% names(rs)) {
        names(rs)[names(rs) == nm] <- display_names[[nm]]
      }
    }
    idx_active <- match("Active", names(rs)) - 1L
    idx_status <- match("Status", names(rs)) - 1L
    idx_details <- match("Details", names(rs)) - 1L
    if (anyNA(c(idx_active, idx_status))) {
      idx_active <- NA_integer_
      idx_status <- NA_integer_
    }
    if (is.na(idx_details)) {
      idx_details <- NA_integer_
    }
    col_defs <- list(list(className = "dt-center", targets = 0L))
    if (!is.na(idx_status)) {
      col_defs <- c(col_defs, list(list(className = "dt-col-status", targets = idx_status)))
    }
    if (!is.na(idx_details)) {
      col_defs <- c(col_defs, list(list(className = "dt-col-details", targets = idx_details)))
    }
    row_cb <- if (!is.na(idx_active) && !is.na(idx_status)) {
      DT::JS(sprintf(
        "function(row, data) {
          var av = data[%d], st = data[%d];
          if (av === 1 || av === '1' || av === true) $(row).addClass('import-validation-row-active');
          if (st === 'error') $(row).addClass('import-validation-row-error');
        }",
        idx_active,
        idx_status
      ))
    } else {
      NULL
    }
    dt_opts <- list(
      paging = FALSE,
      scrollX = TRUE,
      dom = "t",
      columnDefs = col_defs
    )
    if (!is.null(row_cb)) {
      dt_opts$rowCallback <- row_cb
    }
    DT::datatable(
      rs,
      options = dt_opts,
      selection = "none",
      rownames = FALSE,
      callback = DT::JS(
        "table.on('click', 'tbody tr', function() {",
        "  var data = table.row(this).data();",
        "  if (!data || data.length < 1) return;",
        "  var rowId = parseInt(data[0], 10);",
        "  if (isNaN(rowId)) return;",
        "  Shiny.setInputValue('validation_row_action',",
        "    JSON.stringify({action: 'edit', row_id: rowId, t: Date.now()}),",
        "    {priority: 'event'});",
        "});"
      )
    ) |>
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          c("valid", "warning", "error", "inactive"),
          c("#d4edda", "#fff3cd", "#f8d7da", "#e2e3e5")
        )
      )
  })

  parse_validation_row_action <- function(raw) {
    if (is.null(raw)) {
      return(NULL)
    }
    raw_chr <- trimws(as.character(raw))
    if (!nzchar(raw_chr)) {
      return(NULL)
    }
    if (is.list(raw) && !is.null(raw$action)) {
      act <- raw
    } else {
      act <- tryCatch(
        jsonlite::fromJSON(raw_chr, simplifyVector = FALSE),
        error = function(e) NULL
      )
    }
    if (is.null(act)) {
      return(NULL)
    }
    action <- as.character(act$action %||% act[["action"]] %||% "")[1]
    row_id <- suppressWarnings(as.integer(act$row_id %||% act[["row_id"]]))
    if (!nzchar(action) || is.na(row_id)) {
      return(NULL)
    }
    list(action = action, row_id = row_id)
  }

  observeEvent(input$validation_row_action, {
    act <- parse_validation_row_action(input$validation_row_action)
    if (is.null(act) || !identical(act$action, "edit")) {
      return()
    }
    req(rv$validation)
    rs <- rv$validation$row_status
    row_match <- rs[rs$row_id == act$row_id, , drop = FALSE]
    if (nrow(row_match) == 0L) {
      showNotification("Validation row not found.", type = "error")
      return()
    }
    gname <- as.character(row_match$graph_name[[1]])
    manifest <- build_gallery_manifest(rv$validation, rv$built)
    card <- gallery_manifest_card(manifest, gname)
    built_item <- card$built_item %||% NULL
    open_graph_in_editor(act$row_id, graph_name = gname, built_item = built_item)
  }, ignoreInit = TRUE)

  gallery_manifest <- reactive({
    build_gallery_manifest(rv$validation, rv$built)
  })

  gallery_output_id <- function(nm) {
    paste0("gallery_plot_", gsub("[^A-Za-z0-9]", "_", nm))
  }

  gallery_build_active <- reactiveVal(FALSE)

  cancel_gallery_build_job <- function() {
    job <- session$userData$gallery_build_job
    if (!is.null(job)) {
      job$cancelled <- TRUE
      session$userData$gallery_build_job <- job
    }
    gallery_build_active(FALSE)
    rv$gallery_build_progress <- NULL
  }

  finish_gallery_build_job <- function(job) {
    if (!is.null(job$progress)) {
      try(job$progress$close(), silent = TRUE)
    }
    rv$built <- job$built
    note_gallery_built_country(job$country_iso3c)
    session$userData$gallery_build_job <- NULL
    gallery_build_active(FALSE)
    rv$gallery_build_progress <- NULL
    if (plotter_profile_enabled()) {
      total_sec <- (proc.time() - job$t0)[["elapsed"]]
      message(sprintf(
        "[profile] gallery.build_valid_batch (%d rows): %.2fs (defer_ui=%s)",
        job$n,
        total_sec,
        isTRUE(job$defer_ui)
      ))
      if (!is.null(job$profile_times) && length(job$profile_times) > 0L) {
        pt <- dplyr::bind_rows(job$profile_times)
        build_sum <- sum(pt$seconds_build, na.rm = TRUE)
        assign_sum <- sum(pt$seconds_rv_assign, na.rm = TRUE)
        message(sprintf(
          "[profile] gallery.per_graph_build_sum: %.2fs; rv_built_assign_sum: %.2fs; other: %.2fs",
          build_sum,
          assign_sum,
          max(0, total_sec - build_sum - assign_sum)
        ))
      }
    }
    showNotification(
      paste("Built", length(job$built), "graph(s)."),
      type = "message"
    )
  }

  process_gallery_build_step <- function() {
    job <- session$userData$gallery_build_job
    if (is.null(job) || isTRUE(job$cancelled)) {
      if (!is.null(job)) {
        try(job$progress$close(), silent = TRUE)
      }
      session$userData$gallery_build_job <- NULL
      gallery_build_active(FALSE)
      rv$gallery_build_progress <- NULL
      return(invisible(FALSE))
    }

    job$i <- job$i + 1L
    idx <- job$i

    if (idx > job$n) {
      finish_gallery_build_job(job)
      return(invisible(FALSE))
    }

    rid <- job$can_build_ids[[idx]]
    gname <- job$row_meta$graph_name[[idx]] %||% paste0("row_", rid)
    rv$gallery_build_progress <- list(
      i = idx,
      n = job$n,
      graph_name = gname,
      row_id = rid
    )
    job$progress$set(
      idx - 1L,
      message = "Building graphs...",
      detail = glue::glue("Graph {idx}/{job$n}: row {rid} — {gname}")
    )

    row <- job$graphplan[rid, , drop = FALSE]
    t_build <- proc.time()
    item <- tryCatch(
      build_graph_row(
        graphplan_row = row,
        FD = FD,
        country_iso3c = job$country_iso3c,
        peers_fname = peers_fname
      ),
      error = function(e) {
        showNotification(
          glue::glue("Build failed at row {rid}: {conditionMessage(e)}"),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
    if (is.null(item)) {
      job$cancelled <- TRUE
      session$userData$gallery_build_job <- job
      cancel_gallery_build_job()
      return(invisible(FALSE))
    }

    sec_build <- (proc.time() - t_build)[["elapsed"]]
    key <- item$graph_name %||% paste0("row_", rid)
    built_item <- c(
      item,
      list(row_id = rid, status = if (isTRUE(item$ok)) "ok" else "error")
    )
    if (isTRUE(item$ok)) {
      built_item <- tryCatch(
        enrich_gallery_built_item(built_item, job$thumb_dir),
        error = function(e) {
          showNotification(
            glue::glue("Thumbnail failed for '{key}': {conditionMessage(e)}"),
            type = "warning"
          )
          built_item
        }
      )
    }
    job$built[[key]] <- built_item
    sec_assign <- 0
    if (!isTRUE(job$defer_ui)) {
      t_assign <- proc.time()
      rv$built <- job$built
      sec_assign <- (proc.time() - t_assign)[["elapsed"]]
    }
    if (!is.null(job$profile_times)) {
      job$profile_times[[idx]] <- list(
        graph_index = idx,
        row_id = rid,
        graph_name = key,
        seconds_build = round(sec_build, 4),
        seconds_rv_assign = round(sec_assign, 4)
      )
    }
    session$userData$gallery_build_job <- job

    job$progress$set(
      idx,
      message = "Building graphs...",
      detail = glue::glue("Done {idx}/{job$n}: {gname}")
    )
    invisible(TRUE)
  }

  observe({
    if (!gallery_build_active()) {
      return()
    }
    invalidateLater(50, session)
    process_gallery_build_step()
  })

  gallery_thumb_cache_dir <- function() {
    cache <- session$userData$gallery_thumb_dir
    if (!is.null(cache) && nzchar(cache)) {
      return(cache)
    }
    cache <- gallery_session_thumb_dir(session)
    session$userData$gallery_thumb_dir <- cache
    cache
  }

  reset_gallery_thumb_cache <- function() {
    cleanup_gallery_thumb_dir(session$userData$gallery_thumb_dir)
    session$userData$gallery_thumb_dir <- gallery_session_thumb_dir(session)
    dir.create(session$userData$gallery_thumb_dir, recursive = TRUE, showWarnings = FALSE)
    invisible(session$userData$gallery_thumb_dir)
  }

  start_incremental_gallery_build <- function(can_build_ids,
                                              row_meta,
                                              build_graphplan,
                                              build_country_iso3c) {
    cancel_gallery_build_job()
    reset_gallery_thumb_cache()
    thumb_dir <- gallery_thumb_cache_dir()

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
      defer_ui = gallery_defer_ui_enabled(),
      profile_times = if (plotter_profile_enabled()) list() else NULL,
      thumb_dir = thumb_dir,
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
    rv$gallery_build_progress <- list(i = 0L, n = n_build, graph_name = NA_character_, row_id = NA_integer_)
    gallery_build_active(TRUE)
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

      built_for_preview <- NULL
      if (!is.null(edit_built_item) && isTRUE(edit_built_item$ok) && !is.null(edit_built_item$graph)) {
        built_for_preview <- edit_built_item
      } else if (!is.null(edit_country_iso3c)) {
        built_for_preview <- build_graph_row(row, FD, edit_country_iso3c, peers_fname)
        if (!isTRUE(built_for_preview$ok)) {
          rv$editor_preview <- NULL
          showNotification(
            built_for_preview$error %||% "Could not build graph in editor.",
            type = "warning",
            duration = NULL
          )
          built_for_preview <- NULL
        }
      } else {
        rv$editor_preview <- NULL
        showNotification(
          "Select country on Import tab to preview the graph.",
          type = "warning"
        )
      }

      if (!is.null(built_for_preview)) {
        preview_item <- prepare_editor_preview_item(built_for_preview, session)
        if (is.null(preview_item)) {
          rv$editor_preview <- NULL
          showNotification(
            built_for_preview$preview_error %||% "Could not render editor preview.",
            type = "warning"
          )
        } else {
          rv$editor_preview <- preview_item
        }
      }

      showNotification(
        glue("Editing '{edit_graph_name %||% st$graph_name_suffix}' (row {edit_row_id})."),
        type = "message"
      )
    })

    invisible(TRUE)
  }

  gallery_card_ui <- function(card) {
    nm <- card$graph_name
    badge_info <- gallery_status_badge(card$display_status)
    status_badge <- tags$span(
      class = paste("badge", badge_info$class),
      badge_info$label
    )
    show_import_error_badge <- identical(as.character(card$check_status)[1], "error") &&
      !identical(card$display_status, "validation_error")
    import_error_badge <- if (show_import_error_badge) {
      tags$span(class = "badge gallery-status-import-error", "Error")
    } else {
      NULL
    }
    actions <- tags$div(
      class = "gallery-card-actions",
      tags$a(
        class = "btn btn-default btn-sm", href = "#",
        onclick = gallery_action_js("edit", nm),
        "Edit"
      ),
      if (identical(card$display_status, "inactive")) {
        tags$a(
          class = "btn btn-success btn-sm", href = "#",
          onclick = gallery_action_js("activate", nm),
          "Activate"
        )
      } else {
        tagList(
          tags$a(
            class = "btn btn-warning btn-sm", href = "#",
            onclick = gallery_action_js("deactivate", nm),
            "Deactivate"
          ),
          if (identical(card$display_status, "built_ok")) {
            tags$a(
              class = "btn btn-info btn-sm", href = "#",
              onclick = gallery_action_js("download", nm),
              "Download"
            )
          }
        )
      }
    )
    card_class <- "gallery-card"
    if (!identical(card$display_status, "built_ok")) {
      card_class <- paste(card_class, "gallery-card-error")
    }
    if (identical(card$display_status, "inactive")) {
      card_class <- paste(card_class, "gallery-card-inactive")
    }
    title_line <- tags$div(
      class = "gallery-card-title",
      tags$span(class = "gallery-card-name", nm, title = nm),
      tags$div(class = "gallery-card-badges", status_badge, import_error_badge)
    )
    body <- if (identical(card$display_status, "built_ok")) {
      tags$div(
        class = "gallery-card-plot",
        imageOutput(
          gallery_output_id(nm),
          height = paste0(gallery_thumb_height_px, "px")
        )
      )
    } else {
      msg <- switch(
        card$display_status,
        build_failed = card$built_item$error %||% "Build failed",
        validation_error = card$messages %||% "Validation error",
        warning = card$messages %||% "Warning",
        not_built = "Not built yet. Use \"Build active and valid\" to render.",
        inactive = glue::glue("Row {card$row_id} — inactive (not built)"),
        card$messages %||% ""
      )
      tags$div(
        class = "gallery-card-placeholder",
        if (identical(card$display_status, "inactive") ||
            identical(card$display_status, "not_built")) {
          tags$p(class = "text-muted small", msg)
        } else {
          tags$p(class = "text-danger small", msg)
        }
      )
    }
    column(
      width = 4,
      tags$div(class = card_class, title_line, body, actions)
    )
  }

  gallery_layout_class <- function() {
    if (identical(input$gallery_view_mode %||% "preview", "fullsize")) {
      "gallery-layout-fullsize"
    } else {
      "gallery-layout-preview"
    }
  }

  observeEvent(input$gallery_view_mode, {
    layout_cls <- gallery_layout_class()
    shinyjs::runjs(glue::glue(
      "var el = document.getElementById('gallery_layout_root');",
      "if (el) {{",
      "  el.classList.remove('gallery-layout-preview', 'gallery-layout-fullsize');",
      "  el.classList.add('{layout_cls}');",
      "}}"
    ))
  }, ignoreInit = TRUE)

  gallery_manifest_grid <- function(cards_named) {
    nms <- names(cards_named)
    if (length(nms) == 0L) {
      return(NULL)
    }
    row_groups <- split(nms, ceiling(seq_along(nms) / 3L))
    do.call(
      tagList,
      lapply(row_groups, function(group) {
        fluidRow(
          class = "gallery-grid",
          lapply(group, function(nm) {
            gallery_card_ui(cards_named[[nm]])
          })
        )
      })
    )
  }

  output$gallery_build_status <- renderText({
    prog <- rv$gallery_build_progress
    if (!is.null(prog) && isTRUE(gallery_build_active())) {
      if (is.null(prog$i) || prog$i < 1L) {
        return(glue::glue("Preparing to build {prog$n} graph(s)…"))
      }
      return(glue::glue("Building graph {prog$i} of {prog$n}…"))
    }
    if (is.null(rv$validation)) {
      return("")
    }
    manifest <- gallery_manifest()
    n_built <- sum(vapply(
      manifest$active,
      function(c) identical(c$display_status, "built_ok"),
      logical(1)
    ))
    n_inactive <- length(manifest$inactive)
    country_lab <- rv$gallery_status_country_label
    country_suffix <- if (!is.null(country_lab) && !is.na(country_lab) && nzchar(country_lab)) {
      glue::glue(" ({country_lab})")
    } else {
      ""
    }
    parts <- character()
    if (n_built > 0L) {
      parts <- c(parts, glue::glue("{n_built} built{country_suffix}"))
    }
    if (n_inactive > 0L) {
      parts <- c(parts, glue::glue("{n_inactive} inactive"))
    }
    if (length(parts) == 0L) {
      return("")
    }
    paste(parts, collapse = ", ")
  })

  output$gallery_ui <- renderUI({
    prog <- rv$gallery_build_progress
    if (!is.null(prog) && isTRUE(gallery_build_active())) {
      detail <- if (is.null(prog$i) || prog$i < 1L) {
        glue::glue("Preparing {prog$n} graph(s)…")
      } else if (!is.null(prog$graph_name) && !is.na(prog$graph_name)) {
        glue::glue("Graph {prog$i} of {prog$n}: {prog$graph_name}")
      } else {
        glue::glue("Graph {prog$i} of {prog$n}")
      }
      return(tagList(
        tags$div(
          class = "alert alert-info",
          tags$strong("Building gallery…"),
          tags$p(detail),
          tags$p(
            class = "text-muted small",
            "Thumbnails appear when the batch finishes."
          )
        )
      ))
    }
    if (is.null(rv$validation)) {
      return(p("Run validation on the Import tab to see the graph plan in the gallery."))
    }
    manifest <- gallery_manifest()
    active_grid <- gallery_manifest_grid(manifest$active)
    inactive_grid <- gallery_manifest_grid(manifest$inactive)
    if (is.null(active_grid) && is.null(inactive_grid)) {
      return(p("No rows in graphplan."))
    }
    inactive_section <- if (is.null(inactive_grid)) {
      NULL
    } else {
      tags$div(
        class = "gallery-inactive-section",
        tags$h4(
          class = "gallery-inactive-heading",
          glue::glue("Inactive ({length(manifest$inactive)})")
        ),
        inactive_grid
      )
    }
    layout_cls <- isolate(gallery_layout_class())
    tags$div(
      id = "gallery_layout_root",
      class = paste("gallery-layout-root", layout_cls),
      active_grid,
      inactive_section
    )
  })

  observe({
    manifest <- gallery_manifest()
    sig_store <- session$userData$gallery_plot_thumb_sig
    if (is.null(sig_store) || !is.list(sig_store)) {
      sig_store <- list()
    }
    ok_names <- character()
    for (nm in names(manifest$active)) {
      if (identical(manifest$active[[nm]]$display_status, "built_ok")) {
        ok_names <- c(ok_names, nm)
      }
    }
    if (length(sig_store) > 0L) {
      sig_store <- sig_store[names(sig_store) %in% ok_names]
    }
    for (nm in names(manifest$active)) {
      card <- manifest$active[[nm]]
      if (!identical(card$display_status, "built_ok")) {
        next
      }
      item <- card$built_item
      path <- item$thumb_path %||% ""
      sig <- paste(
        path,
        as.integer(item$row_id %||% 0L),
        isTRUE(item$ok),
        sep = "\x1e"
      )
      prev_sig <- sig_store[[nm]] %||% NA_character_
      if (identical(prev_sig, sig)) {
        next
      }
      sig_store[[nm]] <- sig
      local({
        plot_name <- nm
        oid <- gallery_output_id(plot_name)
        output[[oid]] <- renderImage(
          {
            m <- gallery_manifest()
            c <- m$active[[plot_name]]
            req(!is.null(c), identical(c$display_status, "built_ok"))
            item <- c$built_item
            req(!is.null(item), isTRUE(item$ok))
            path <- item$thumb_path
            req(!is.null(path), nzchar(path), file.exists(path))
            dims <- gallery_thumb_dims(item$graph_params)
            list(
              src = normalizePath(path, winslash = "/", mustWork = TRUE),
              contentType = "image/png",
              width = dims$width,
              height = dims$height,
              alt = plot_name
            )
          },
          deleteFile = FALSE
        )
      })
    }
    session$userData$gallery_plot_thumb_sig <- sig_store
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
    req(rv$validation)
    manifest <- gallery_manifest()
    card <- gallery_manifest_card(manifest, nm)
    if (is.null(card)) {
      showNotification(glue("Graph '{nm}' not found in gallery."), type = "error")
      return()
    }
    item <- card$built_item

    if (identical(action, "edit")) {
      open_graph_in_editor(
        card$row_id,
        graph_name = nm,
        built_item = if (identical(card$display_status, "built_ok")) item else NULL
      )
      return()
    }

    if (identical(action, "activate")) {
      if (is.null(rv$graphplan)) {
        showNotification("Cannot activate (no graphplan).", type = "error")
        return()
      }
      rv$graphplan <- activate_graphplan_row(rv$graphplan, card$row_id)
      rv$dirty <- TRUE
      if (!is.null(rv$country_iso3c)) {
        rv$validation <- validate_graphplan_for_app(
          rv$graphplan, FD, rv$country_iso3c, peers_fname
        )
        rv$graphplan <- rv$validation$plan
        rv$built <- prune_built_list_for_validation(rv$built, rv$validation)
        rid <- card$row_id
        rs_one <- rv$validation$row_status[
          rv$validation$row_status$row_id == rid,
          ,
          drop = FALSE
        ]
        if (nrow(rs_one) == 1L && isTRUE(rs_one$can_build[[1]])) {
          cs <- as.character(rs_one$check_status[[1]])[1]
          if (!is.na(cs) && cs %in% c("valid", "warning")) {
            fres <- refresh_gallery_built_for_row(
              rv$built,
              rid,
              rv$graphplan,
              FD,
              rv$country_iso3c,
              peers_fname,
              gallery_thumb_cache_dir()
            )
            rv$built <- fres$built_list
            note_gallery_built_country(rv$country_iso3c)
          }
        }
        bump_plan_validation_revision()
        clear_editor_validation_cache()
      }
      showNotification(
        glue("Activated '{nm}'. Review validation on Import; use Edit to open the row in the editor."),
        type = "message"
      )
      return()
    }

    if (identical(action, "deactivate")) {
      if (is.null(rv$graphplan)) {
        showNotification("Cannot deactivate (no graphplan).", type = "error")
        return()
      }
      rid <- card$row_id
      if (!is.null(item) && !is.null(item$thumb_path) &&
          nzchar(item$thumb_path) && file.exists(item$thumb_path)) {
        unlink(item$thumb_path, force = TRUE)
      }
      rv$graphplan <- soft_delete_row(rv$graphplan, rid)
      rv$built <- remove_built_list_row(rv$built, rid)
      rv$dirty <- TRUE
      if (!is.null(rv$country_iso3c)) {
        rv$validation <- profile_step(
          "gallery.deactivate.validate_graphplan_for_app",
          validate_graphplan_for_app(
            rv$graphplan, FD, rv$country_iso3c, peers_fname
          )
        )
        rv$graphplan <- rv$validation$plan
        rv$built <- prune_built_list_for_validation(rv$built, rv$validation)
        bump_plan_validation_revision()
        clear_editor_validation_cache()
      }
      showNotification(glue("Deactivated '{nm}'."), type = "message")
      return()
    }

    if (identical(action, "download")) {
      if (!identical(card$display_status, "built_ok") || is.null(item) || !isTRUE(item$ok)) {
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
      write_export_png(item$graph, item$graph_params, file)
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

  sync_ed_peers_custom_from_peer_mode <- function(peers = input$ed_peers,
                                                  peers_formula = input$ed_peers_formula,
                                                  graph_type = input$ed_graph_type) {
    # `apply_editor_state` can run inside `session$onFlushed` (e.g. Edit from Gallery);
    # reactive reads must use isolate there — observers are fine too.
    country_iso3c <- shiny::isolate({
      country_iso3c_from_choice_if_valid(input$country_choice) %||% rv$country_iso3c
    })
    if (!is_valid_iso3c_scalar(country_iso3c)) {
      return(invisible(NULL))
    }
    expanded <- expand_editor_peer_selection_to_iso2c(
      country_iso3c = country_iso3c,
      peers = peers,
      peers_formula = peers_formula,
      graph_type = graph_type,
      peers_fname = peers_fname,
      data = FD
    )
    if (is.null(expanded)) {
      return(invisible(NULL))
    }
    valid <- intersect(expanded, unname(country_choices))
    updateSelectizeInput(
      session,
      "ed_peers_custom",
      choices = country_choices,
      selected = valid
    )
    invisible(NULL)
  }

  apply_editor_state <- function(session, state, indicator_choices, country_choices) {
    updateTextInput(session, "ed_graph_name_suffix", value = state$graph_name_suffix %||% "")
    updateTextAreaInput(session, "ed_graph_title", value = state$graph_title %||% "")
    updateSelectizeInput(session, "ed_graph_type", selected = state$graph_type)
    updateSelectizeInput(session, "ed_graph_group", selected = state$graph_group_short)
    updateSelectizeInput(session, "ed_data_frequency", selected = state$data_frequency)
    ic <- editor_indicator_choices(
      indicator_catalog,
      frequency = state$data_frequency %||% " ",
      ind_group = ""
    )
    sel_inds <- state$indicators %||% character()
    ind_choices <- ensure_indicator_choices(ic$choices, sel_inds, indicator_catalog)
    updateSelectizeInput(
      session, "ed_indicators",
      choices = ind_choices,
      selected = sel_inds
    )
    updateTextInput(session, "ed_time_fix", value = state$time_fix %||% "")
    updateSelectizeInput(session, "ed_peers", selected = state$peers)
    updateTextInput(session, "ed_peers_formula", value = state$peers_formula %||% "")
    updateCheckboxInput(session, "ed_all", value = state$all)
    updateTextInput(session, "ed_x_min", value = state$x_min %||% "")
    updateTextInput(session, "ed_x_max", value = state$x_max %||% "")
    updateTextInput(session, "ed_y_min", value = state$y_min %||% "")
    updateTextInput(session, "ed_y_max", value = state$y_max %||% "")
    updateSelectInput(session, "ed_trend_type", selected = state$trend_type %||% "")
    updateSelectInput(session, "ed_theme", selected = state$theme %||% "ipsum")
    sec_choices <- sec_y_choices_from_indicators(indicator_catalog, sel_inds)
    sec_choices <- ensure_indicator_choices(sec_choices, state$sec_y_ind %||% character(), indicator_catalog)
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
    if (identical(state$peers, "custom")) {
      updateSelectizeInput(
        session, "ed_peers_custom",
        selected = state$peers_custom %||% character()
      )
    } else {
      sync_ed_peers_custom_from_peer_mode(
        peers = state$peers,
        peers_formula = state$peers_formula,
        graph_type = state$graph_type
      )
    }
  }

  output$editor_mode_ui <- renderUI({
    label <- if (rv$editor_mode == "edit" && !is.null(rv$selected_row_id)) {
      glue("Job: edit row {rv$selected_row_id}")
    } else {
      "Job: new graph"
    }
    tags$span(class = "editor-mode-badge", label)
  })

  update_editor_indicators <- function(freq = NULL,
                                       ind_group = NULL,
                                       keep_selected = TRUE,
                                       selected_indicators = NULL) {
    if (is.null(freq)) freq <- input$ed_data_frequency
    if (is.null(ind_group)) ind_group <- input$ed_ind_group
    ic <- editor_indicator_choices(indicator_catalog, frequency = freq, ind_group = ind_group)
    selected <- if (!is.null(selected_indicators)) {
      selected_indicators
    } else if (!is.null(ic$selected)) {
      ic$selected
    } else if (isTRUE(keep_selected)) {
      input$ed_indicators
    } else {
      character()
    }
    ind_choices <- ensure_indicator_choices(ic$choices, selected, indicator_catalog)
    updateSelectizeInput(
      session, "ed_indicators",
      choices = ind_choices,
      selected = selected
    )
  }

  observeEvent(input$ed_peers, sync_ed_peers_custom_from_peer_mode(), ignoreNULL = FALSE)
  observeEvent(input$ed_graph_type, sync_ed_peers_custom_from_peer_mode(), ignoreNULL = FALSE)
  observeEvent(input$country_choice, {
    if (is.null(country_iso3c_from_choice_if_valid(input$country_choice))) {
      return(invisible(NULL))
    }
    sync_ed_peers_custom_from_peer_mode()
  }, ignoreNULL = FALSE)
  observeEvent(input$ed_peers_formula, sync_ed_peers_custom_from_peer_mode(), ignoreNULL = FALSE)

  observeEvent(
    list(input$ed_data_frequency, input$ed_ind_group),
    update_editor_indicators(),
    ignoreInit = TRUE
  )

  observeEvent(input$ed_indicators, {
    sec_choices <- sec_y_choices_from_indicators(indicator_catalog, input$ed_indicators)
    sec_selected <- intersect(
      input$ed_sec_y_axis_ind %||% character(),
      unname(sec_choices)
    )
    sec_choices <- ensure_indicator_choices(sec_choices, sec_selected, indicator_catalog)
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

  reset_editor_for_new_graph <- function() {
    rv$editor_preview <- NULL
    preview_path <- editor_preview_path(
      session$userData$editor_preview_dir %||% editor_session_preview_dir(session)
    )
    if (file.exists(preview_path)) {
      unlink(preview_path, force = TRUE)
    }
    clear_editor_validation_cache()
    rv$selected_row_id <- NULL
    rv$editor_mode <- "new"
    st <- graphplan_row_to_editor_state(editor_new_graph_seed_row())
    apply_editor_state(session, st, indicator_choices, country_choices)
    updateTextInput(session, "ed_graph_plan_tsv", value = "")
    updateSelectInput(session, "ed_ind_group", selected = "")
    update_editor_indicators(
      freq = st$data_frequency,
      ind_group = "",
      selected_indicators = st$indicators
    )
    sync_ed_peers_custom_from_peer_mode(
      peers = st$peers,
      peers_formula = st$peers_formula,
      graph_type = st$graph_type
    )
  }

  observeEvent(input$ed_new_graph, reset_editor_for_new_graph())

  observeEvent(input$import_new_graph_btn, {
    navigate_to_editor_tab()
    run_after_tab_switch(reset_editor_for_new_graph())
  })

  observeEvent(input$gallery_new_graph_btn, {
    navigate_to_editor_tab()
    run_after_tab_switch(reset_editor_for_new_graph())
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
    preview_item <- prepare_editor_preview_item(built, session)
    if (is.null(preview_item)) {
      rv$editor_preview <- NULL
      showNotification(
        built$preview_error %||% "Preview PNG failed.",
        type = "warning"
      )
      return()
    }
    rv$editor_preview <- preview_item
    note_editor_plot_country(rv$country_iso3c)
  })

  output$ed_graph_plot_ui <- renderUI({
    preview <- rv$editor_preview
    if (is.null(preview) || !isTRUE(preview$ok)) {
      return(tags$div(class = "editor-preview-plot editor-preview-plot--empty"))
    }
    path <- preview$preview_path
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      return(tags$div(class = "editor-preview-plot editor-preview-plot--empty"))
    }
    dims <- editor_preview_display_dims(preview$graph_params)
    tags$div(
      class = "editor-preview-plot",
      imageOutput(
        "ed_graph_plot",
        height = paste0(dims$height, "px"),
        width = paste0(dims$width, "px")
      )
    )
  })

  output$ed_graph_plot <- renderImage(
    {
      req(rv$editor_preview$ok)
      path <- rv$editor_preview$preview_path
      req(!is.null(path), nzchar(path), file.exists(path))
      dims <- editor_preview_display_dims(rv$editor_preview$graph_params)
      list(
        src = normalizePath(path, winslash = "/", mustWork = TRUE),
        contentType = "image/png",
        width = dims$width,
        height = dims$height,
        alt = rv$editor_preview$graph_params$graph_name %||% "preview"
      )
    },
    deleteFile = FALSE
  )

  output$ed_download_png <- downloadHandler(
    filename = function() {
      g <- rv$editor_preview$graph_params$graph_name %||% "graph"
      paste0(g, ".png")
    },
    content = function(file) {
      preview_path <- rv$editor_preview$preview_path
      if (!is.null(preview_path) && nzchar(preview_path) && file.exists(preview_path)) {
        file.copy(preview_path, file, overwrite = TRUE)
      } else {
        write_export_png(
          rv$editor_preview$graph,
          rv$editor_preview$graph_params,
          file
        )
      }
    }
  )

  output$ed_download_jpeg <- downloadHandler(
    filename = function() {
      g <- rv$editor_preview$graph_params$graph_name %||% "graph"
      paste0(g, ".jpeg")
    },
    content = function(file) {
      req(rv$editor_preview$ok, rv$editor_preview$graph, rv$editor_preview$graph_params)
      write_export_image(
        rv$editor_preview$graph,
        rv$editor_preview$graph_params,
        file,
        device = "jpeg"
      )
    }
  )

  output$ed_download_data <- downloadHandler(
    filename = function() {
      paste0(rv$editor_preview$graph_params$graph_name %||% "graph", "_data.xlsx")
    },
    content = function(file) {
      shiny::validate(
        shiny::need(isTRUE(rv$editor_preview$ok), "Build the plot before downloading data."),
        shiny::need(!is.null(rv$editor_preview$data), "No graph data available.")
      )
      export_graph_data_workbook(
        item = rv$editor_preview,
        path = file,
        country_iso3c = rv$country_iso3c,
        graphplan_row = editor_inputs_to_graphplan_row(
          collect_editor_state(),
          dict = FD$dict
        ),
        dict = FD$dict,
        peers_iso2c = rv$editor_preview$peers_iso2c,
        country_iso2c = rv$editor_preview$country_iso2c,
        country_label = country_label_from_iso3c(rv$country_iso3c),
        fd = FD
      )
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
    row_is_inactive <- !isTRUE(active_flag_vec(row)[1])
    if (!row_is_inactive && !is.null(row_val) && !isTRUE(row_val$can_build)) {
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
              peers_fname   = peers_fname,
              thumb_cache_dir = gallery_thumb_cache_dir()
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
        rv$built <- prune_built_list_for_validation(
          gallery_refresh$built_list,
          save_result$validation
        )
        if (!is.null(gallery_refresh$editor_preview)) {
          rv$editor_preview <- prepare_editor_preview_item(
            gallery_refresh$editor_preview,
            session
          )
        } else {
          rv$editor_preview <- NULL
        }
        rv$plan_validation_revision <- save_next_revision
        rv$editor_validation_cache_key <- NULL
        rv$editor_row_validation <- NULL
        incProgress(1)
      })
      sid <- as.integer(save_result$saved_row_id)
      prev_touch <- shiny::isolate(rv$editor_touch_row_ids %||% integer(0))
      if (identical(save_editor_mode, "edit") && !is.null(save_selected_row_id)) {
        br <- save_graphplan[save_selected_row_id, , drop = FALSE]
        if (isTRUE(meaningful_editor_save_for_edited_flag(br, save_row))) {
          rv$editor_touch_row_ids <- sort(unique(c(prev_touch, sid)))
        } else {
          rv$editor_touch_row_ids <- setdiff(prev_touch, sid)
        }
      } else {
        rv$editor_touch_row_ids <- sort(unique(c(prev_touch, sid)))
      }
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
      st <- graphplan_row_to_editor_state(editor_new_graph_seed_row())
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

  export_button_ids <- c(
    "download_graphs_zip",
    "download_graph_data_xlsx",
    "download_graphplan_xlsx",
    "download_recipes_tsv"
  )
  export_in_progress <- reactiveVal(FALSE)

  set_export_buttons_disabled <- function(disabled) {
    for (id in export_button_ids) {
      if (disabled) {
        shinyjs::disable(id)
      } else {
        shinyjs::enable(id)
      }
    }
  }

  with_export_feedback <- function(label, expr) {
    if (isTRUE(export_in_progress())) {
      shiny::validate(shiny::need(FALSE, "Another export is in progress."))
    }
    export_in_progress(TRUE)
    set_export_buttons_disabled(TRUE)
    notice_id <- showNotification(
      paste0("Preparing ", label, "..."),
      duration = NULL,
      closeButton = FALSE
    )
    on.exit({
      export_in_progress(FALSE)
      set_export_buttons_disabled(FALSE)
      removeNotification(notice_id)
    }, add = TRUE)
    tryCatch(
      {
        result <- force(expr)
        showNotification(paste0(label, " ready."), type = "message")
        result
      },
      error = function(e) {
        showNotification(
          paste0(label, " failed: ", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        stop(e)
      }
    )
  }

  output$export_summary_ui <- renderUI({
    country_lab <- country_label_from_iso3c(
      rv$country_iso3c %||% country_iso3c_from_id(input$country_choice %||% "")
    )
    s <- compute_export_summary_ui_data(
      graphplan = rv$graphplan,
      validation = rv$validation,
      built = rv$built,
      editor_touch_row_ids = rv$editor_touch_row_ids,
      graphplan_baseline = rv$graphplan_baseline,
      country_label = country_lab
    )
    tags$div(
      class = "import-summary-cards",
      validation_metric_card("Planned to build", s$planned, "validation-metric-buildable"),
      validation_metric_card("Edited", s$edited, "validation-metric-total"),
      validation_metric_card("Built", s$built, "validation-metric-active"),
      tags$div(
        class = "validation-metric-card validation-metric-total",
        tags$div(class = "validation-metric-value", s$country),
        tags$div(class = "validation-metric-label", "Country")
      )
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

  output$download_graphs_zip <- downloadHandler(
    filename = function() paste0("graphs_", Sys.Date(), ".zip"),
    content = function(file) {
      with_export_feedback("Graph zip export", {
        scoped <- export_built_for_download()
        shiny::validate(shiny::need(length(scoped) > 0, "No graphs match the export scope."))
        export_built_graphs_zip(
          built    = scoped,
          zip_path = file,
          device   = input$export_device
        )
      })
    }
  )

  output$download_graph_data_xlsx <- downloadHandler(
    filename = function() paste0("graph_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      with_export_feedback("Graph data export", {
        scoped <- export_built_for_download()
        shiny::validate(shiny::need(length(scoped) > 0, "No graphs match the export scope."))
        export_graph_data_xlsx(
          scoped,
          path = file,
          country_iso3c = rv$country_iso3c
        )
        shiny::validate(shiny::need(
          file.exists(file) && file.info(file)$size > 0,
          "No graph data available for export."
        ))
      })
    }
  )

  output$download_graphplan_xlsx <- downloadHandler(
    filename = function() "2_graphlib.xlsx",
    content = function(file) {
      with_export_feedback("Graphplan export", {
        shiny::validate(shiny::need(!is.null(rv$graphplan), "No graphplan to export."))
        info <- default_graphplan_info()
        title_row <- graphplan_export_title_row(
          country_iso3c = rv$country_iso3c,
          country_label = country_label_from_iso3c(rv$country_iso3c),
          base = rv$graphplan_title_row,
          fd = FD
        )
        export_graphplan_xlsx(
          plan = rv$graphplan,
          path = file,
          info = info,
          title_row = title_row
        )
      })
    }
  )

  output$download_recipes_tsv <- downloadHandler(
    filename = function() paste0("graphplan_recipes_", Sys.Date(), ".tsv"),
    content = function(file) {
      with_export_feedback("Recipes export", {
        shiny::validate(shiny::need(!is.null(rv$graphplan), "No graphplan to export."))
        text <- export_graphplan_recipes_text(
          plan = rv$graphplan,
          include_inactive = !isTRUE(input$export_recipes_active_only)
        )
        shiny::validate(shiny::need(nzchar(text), "No rows to export as recipes."))
        writeLines(text, con = file, useBytes = TRUE)
      })
    }
  )
}

shinyApp(ui = ui, server = server)
