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
  title = "Country data downloader",
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
          choices = c("Model", "All data (vertical)")
        ),
        
        downloadButton("download_country", "Download")
      ),
      mainPanel()
    )
  ),
  
  # -------- Tab 2: Custom ----------
  tabPanel(
    title = "Custom query",
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
                  placeholder = "Start entering code/name of the indicator…",
                  maxOptions  = 5000
                )
              )
            ),
            column(
              width = 3,
              selectizeInput(
                inputId  = "custom_countries",
                label    = "Country",
                choices  = country_choices_multi,
                multiple = TRUE,
                options  = list(
                  placeholder = "Choose countries…"
                )
              )
            ),
            column(
              width = 4,
              
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
              
              div(
                style = "display: flex; align-items: center; gap: 12px;",
                
                radioButtons(
                  inputId = "custom_time_layout",
                  label   = NULL,
                  choices = c(
                    "Time in columns" = "columns",
                    "Time in rows"    = "rows"
                  ),
                  selected = "columns",
                  inline   = TRUE
                ),
                
                downloadButton("download_custom", "Download")
              )
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
      "All data (vertical)"   = ds
    )
    
    if (inherits(out, c("data.frame", "tbl"))) out <- list(data = out)
    purrr::keep(out, \(x) inherits(x, c("data.frame", "tbl")))
  })
  
  download_filename_country <- reactive({
    req(input$file_structure)
    switch(
      input$file_structure,
      "Model"                 = "model",
      "All data (vertical)"   = "filled_v"
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
      
      tryCatch(
        {
          # --- форматирование как в исходном country_data_download_app ---
          fmt <- switch(
            input$file_structure,
            
            "Model" = list(
              freeze = list(
                y = list(cell = "E2")
              ),
              widths = list(
                y = c(30, 15, 12, 12)
              )
            ),
            
            "All data (vertical)" = list(
              freeze = list(
                y    = list(cell = "B2"),
                q    = list(cell = "C2"),
                m    = list(cell = "D2"),
                d    = list(cell = "B2"),
                dict = list(row = 2, col = 1)
              ),
              widths = list(
                d    = c(22),
                dict = c(41, 14, 20, 7, 22, 3, 3, 10, 10, 10, 10)
              )
            ),
            
            list(freeze = list(), widths = list())
          )
          
          # применяем только к реально существующим листам
          fmt$freeze <- fmt$freeze[names(dat)]
          fmt$widths <- fmt$widths[names(dat)]
          
          write_xlsx_formatted(
            sheets              = dat,
            path                = file,
            freeze_by_sheet     = fmt$freeze,
            col_widths_by_sheet = fmt$widths
          )
        },
        error = function(e) {
          message("Download error: ", conditionMessage(e))
          shiny::showNotification(
            paste("Ошибка при формировании XLSX:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
          stop(e)
        }
      )
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
      layout_tag <- if (identical(input$custom_time_layout, "rows")) "time_in_rows" else "time_in_columns"
      glue("custom_extract_{layout_tag}_{format(Sys.Date(), '%Y-%m-%d')}.xlsx")
    },
    content = function(file) {
      req(input$custom_indicators)
      
      sheets <- if (identical(input$custom_time_layout, "rows")) {
        build_custom_download_vertical(
          fd                = FD,
          selected_node_ids = input$custom_indicators,
          country_ids       = input$custom_countries,
          year_from         = custom_year_from(),
          year_to           = custom_year_to()
        )
      } else {
        build_custom_download_time_in_columns(
          fd                = FD,
          selected_node_ids = input$custom_indicators,
          country_ids       = input$custom_countries,
          year_from         = custom_year_from(),
          year_to           = custom_year_to()
        )
      }
      
      shiny::validate(shiny::need(length(sheets) > 0, "Нет данных по выбранным фильтрам."))
      
      # ---- форматирование ----
      # 1) Time in rows: как vertical на первой вкладке, но +2 столбца (country/country_id)
      # 2) Time in columns: фиксируем 3 первых колонки (country/country_id/indicator_code) => freeze D2
      fmt <- if (identical(input$custom_time_layout, "rows")) {
        list(
          freeze = list(
            y    = list(cell = "D2"), # было B2 -> стало D2
            q    = list(cell = "E2"), # было C2 -> стало E2
            m    = list(cell = "F2"), # было D2 -> стало F2
            d    = list(cell = "D2"), # было B2 -> стало D2
            dict = list(row = 2, col = 1)
          ),
          widths = list(
            d    = c(22, 10, 22),
            dict = c(41, 14, 20, 7, 22, 3, 3, 10, 10, 10, 10)
          )
        )
      } else {
        list(
          freeze = list(
            y    = list(cell = "D2"),
            q    = list(cell = "D2"),
            m    = list(cell = "D2"),
            d    = list(cell = "D2"),
            dict = list(row = 2, col = 1)
          ),
          widths = list(
            y = c(22, 10, 22),
            q = c(22, 10, 22),
            m = c(22, 10, 22),
            d = c(22, 10, 22),
            dict = c(41, 14, 20, 7, 22, 3, 3, 10, 10, 10, 10)
          )
        )
      }
      
      fmt$freeze <- fmt$freeze[names(sheets)]
      fmt$widths <- fmt$widths[names(sheets)]
      
      write_xlsx_formatted(
        sheets              = sheets,
        path                = file,
        freeze_by_sheet     = fmt$freeze,
        col_widths_by_sheet = fmt$widths
      )
    }
  )
}

shinyApp(ui = ui, server = server)