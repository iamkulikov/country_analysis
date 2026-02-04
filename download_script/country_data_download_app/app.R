library(shiny)
library(bslib)
library(here)
library(dplyr)
library(purrr)
library(glue)
library(rlang)
library(DT)
library(openxlsx)

here::i_am("app.R")
source(here("service.R"))

# app_dir <- normalizePath(getwd())
# source(file.path(app_dir, "service.R"))

# ------------------------- LOAD DB -----------------------------------------

data_fname   <- here("Filled_DB.rds")
data_d_fname <- here("Filled_DB_d.rds")

# data_fname <- file.path(app_dir, "Filled_DB.rds")
# data_d_fname <- file.path(app_dir, "Filled_DB_d.rds")
sheet_keys   <- c(y = "y", q = "q", m = "m")

FD <- importData(
  yqm_file   = data_fname,
  d_file     = data_d_fname,
  sheet_keys = sheet_keys,
  format     = "auto",
  add_time   = TRUE
)

param_fname <- here("0_database_params.xlsx")
# param_fname <- file.path(app_dir, "0_database_params.xlsx")
update_mode <- 0L

import_params <- readImportParams(param_fname = param_fname, update_mode = update_mode)
impplan       <- import_params$impplan
fillplan      <- readFillParams(param_fname, sheet = "fill")

formula_words <- c(
  "lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first",
  "min", "pmin", "max", "pmax", "sum", "coalesce", "share", "exp", "fromto",
  "year", "na_if", "cummax", "cummin", "cumsum", "ceiling", "letterize",
  "if_else", "ifelse", "sqrt", "log", "abs", "rollapply"
)

dep_graph_static <- build_dependency_graph(
  impplan       = impplan,
  fillplan      = fillplan,
  formula_words = formula_words,
  only_active   = TRUE
)

countries_tbl <- FD$extdata_y |>
  dplyr::distinct(country, country_id) |>
  dplyr::arrange(country)

country_choices_single <- rlang::set_names(countries_tbl$country_id, countries_tbl$country)
country_choices_multi  <- country_choices_single

indicator_catalog <- build_indicator_catalog_from_dict(FD$dict)

# --------------------------- UI --------------------------------------------

ui <- navbarPage(
  title = "Data downloader",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  # -------- Tab 1: Country file ----------
  tabPanel(
    title = "Country file",
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "country_choice",
          "Choose a country",
          choices  = country_choices_single,
          selected = "RU",
          options  = list(placeholder = "Please select an option below")
        ),
        
        selectInput(
          "file_structure",
          "Choose download format:",
          choices = c("Model", "All data (vertical)", "All data (horizontal)")
        ),
        
        downloadButton("download_country", "Download")
      ),
      mainPanel()
    )
  ),
  
  # -------- Tab 2: Custom ----------
  tabPanel(
    title = "Custom",
    fluidRow(
      column(
        width = 12,
        wellPanel(
          fluidRow(
            column(
              width = 5,
              selectizeInput(
                inputId  = "custom_indicators",
                label    = "Indicators",
                choices  = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Начните вводить код/название индикатора…",
                  maxOptions  = 5000
                )
              )
            ),
            column(
              width = 4,
              selectizeInput(
                inputId  = "custom_countries",
                label    = "Country",
                choices  = country_choices_multi,
                multiple = TRUE,
                options  = list(
                  placeholder = "Выберите страны…"
                )
              )
            ),
            column(
              width = 3,
              fluidRow(
                column(
                  width = 6,
                  textInput("year_from", "Years from", value = "", placeholder = "e.g. 2000")
                ),
                column(
                  width = 6,
                  textInput("year_to", "Years to", value = "", placeholder = "e.g. 2023")
                )
              ),
              downloadButton("download_custom", "Download")
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        DT::DTOutput("custom_table")
      ),
      
      fluidRow(
        column(
          width = 12,
          shiny::uiOutput("dep_graph_ui")
        )
      )
    )
  )
)

# --------------------------- SERVER ----------------------------------------

server <- function(input, output, session) {
  
  # --- заполняем indicator selectize server-side ---
  observe({
    choices <- rlang::set_names(indicator_catalog$node_id, indicator_catalog$label)
    updateSelectizeInput(
      session,
      inputId  = "custom_indicators",
      choices  = choices,
      server   = TRUE
    )
  })
  
  parse_year <- function(x) {
    x <- stringr::str_trim(x %||% "")
    if (x == "") return(NULL)
    y <- suppressWarnings(as.integer(x))
    if (is.na(y)) return(NULL)
    y
  }
  
  custom_year_from <- reactive(parse_year(input$year_from))
  custom_year_to   <- reactive(parse_year(input$year_to))
  
  # ----------------------- TAB 1: Country file -----------------------------
  
  chosen_country_name <- reactive({
    req(input$country_choice)
    countries_tbl |>
      dplyr::filter(.data$country_id == input$country_choice) |>
      dplyr::pull(.data$country) |>
      dplyr::first()
  })
  
  data_subset <- reactive({
    req(input$country_choice)
    subsetCountry(country_id = input$country_choice, datalist = FD)
  })
  
  data_to_download_country <- reactive({
    req(input$country_choice, input$file_structure)
    ds <- data_subset()
    
    out <- switch(
      input$file_structure,
      "Model" = generateModelSheet(yearly_data = ds[["y"]], dict = ds[["dict"]]),
      "All data (vertical)"   = ds,
      "All data (horizontal)" = transposeDatalist(ds)
    )
    
    if (inherits(out, c("data.frame", "tbl"))) out <- list(data = out)
    purrr::keep(out, \(x) inherits(x, c("data.frame", "tbl")))
  })
  
  download_filename_country <- reactive({
    req(input$file_structure)
    switch(
      input$file_structure,
      "Model"                 = "model",
      "All data (vertical)"   = "filled_v",
      "All data (horizontal)" = "filled_h"
    )
  })
  
  output$download_country <- downloadHandler(
    filename = function() {
      req(input$country_choice)
      glue("{chosen_country_name()}_data_{download_filename_country()}.xlsx")
    },
    content = function(file) {
      dat <- data_to_download_country()
      shiny::validate(shiny::need(length(dat) > 0, "Нет данных для выгрузки."))
      
      # форматирование можно оставить как у вас (или упростить)
      write_xlsx_formatted(sheets = dat, path = file)
    }
  )
  
  # ----------------------- TAB 2: Custom -----------------------------------
  
  custom_summary <- reactive({
    req(input$custom_indicators)
    
    build_custom_summary_table(
      fd                = FD,
      dict              = FD$dict,
      selected_node_ids = input$custom_indicators,
      country_ids       = input$custom_countries,
      year_from         = custom_year_from(),
      year_to           = custom_year_to()
    )
  })
  
  output$custom_table <- DT::renderDT(
    {
      custom_summary()
    },
    selection = "single",
    options = list(
      pageLength = 10,
      autoWidth  = TRUE
    )
  )
  
  selected_dep_node <- reactiveVal(NULL)
  
  observeEvent(input$custom_table_rows_selected, {
    sel <- input$custom_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return()
    
    tbl <- custom_summary()
    if (nrow(tbl) == 0) return()
    
    clicked <- tbl |>
      dplyr::slice(sel) |>
      dplyr::transmute(
        node_id = glue::glue("{indicator_code}@{source_frequency}")
      ) |>
      dplyr::pull(.data$node_id) |>
      dplyr::first()
    
    current <- selected_dep_node()
    
    # toggle: повторный клик по тому же -> скрыть
    if (!is.null(current) && identical(current, clicked)) {
      selected_dep_node(NULL)
    } else {
      selected_dep_node(clicked)
    }
  })
  
  output$dep_graph_ui <- renderUI({
    node <- selected_dep_node()
    if (is.null(node)) return(NULL)
    
    tagList(
      tags$hr(),
      tags$h4("Upstream dependencies"),
      ggiraph::girafeOutput("dep_graph", width = "100%", height = "650px")
    )
  })
  
  output$dep_graph <- ggiraph::renderGirafe({
    node <- selected_dep_node()
    req(node)
    
    parts <- split_node_id(node)
    indicator_code <- parts$indicator_code
    frequency      <- parts$frequency
    
    plot_indicator_graph_interactive(
      dep_graph      = dep_graph_static,
      indicator_code = indicator_code,
      frequency      = frequency,
      direction      = "upstream"
    )
  })
  
  output$download_custom <- downloadHandler(
    filename = function() {
      glue("custom_extract_{format(Sys.Date(), '%Y-%m-%d')}.xlsx")
    },
    content = function(file) {
      req(input$custom_indicators)
      
      wide_tbl <- build_custom_download_wide(
        fd                = FD,
        selected_node_ids = input$custom_indicators,
        country_ids       = input$custom_countries,
        year_from         = custom_year_from(),
        year_to           = custom_year_to()
      )
      
      shiny::validate(shiny::need(nrow(wide_tbl) > 0, "Нет данных по выбранным фильтрам."))
      
      write_xlsx_formatted(
        sheets = list(data = wide_tbl),
        path   = file,
        freeze_by_sheet = list(data = list(cell = "C2")),
        col_widths_by_sheet = list(data = c(10, 25))
      )
    }
  )
}

shinyApp(ui = ui, server = server)