# An app to download updated data for a particular country

library(shiny)
library(here)
library(dplyr)
library(purrr)
library(glue)
library(rlang)
library(DT)

here::i_am("app.R")
# here::i_am("download_script/country_data_download_app/app.R")
# app_root <- here::here("download_script/country_data_download_app")

source(here("service.R"))
# source(file.path(app_root, "service.R"))

data_fname <- here("Filled_DB.rds")
data_d_fname <- here("Filled_DB_d.rds")
# data_fname   <- file.path(app_root, "Filled_DB.rds")
# data_d_fname <- file.path(app_root, "Filled_DB_d.rds")
sheet_keys <- c(y = "y", q = "q", m = "m")

# Import data
FD <- importData(yqm_file = data_fname, d_file = data_d_fname, sheet_keys = sheet_keys, format = "auto", add_time = T)

countries_tbl <- FD$extdata_y |>
  dplyr::distinct(country, country_id) |>
  dplyr::arrange(country)

country_choices <- rlang::set_names(countries_tbl$country_id, countries_tbl$country)

#countries <- c("Russia", "France", "Saudi Arabia")

ui <-   fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("Choose a country to download data"),
    
    sidebarLayout(
      
      sidebarPanel(
        
        # Input: Choose country
        selectizeInput(
          "country_choice",
          "Choose a country",
          choices  = country_choices,
          selected = "RU",
          options = list(
            placeholder = "Please select an option below"
          )
        ),
        
        # Input: Choose file structure
        selectInput("file_structure", "Choose download format:",
                    choices = c("Model", "All data (vertical)", "All data (horizontal)")),
        
        # Button
        downloadButton("downloadData", "Download")
        
      ),
      
      mainPanel(
        
        textOutput("chosen_country"),
        DT::DTOutput("table")
        
     )
  )
)


server <- function(input, output, session) {
  
  # Calculating a data subset for a chosen country
  data_subset <- reactive({
    req(input$country_choice)
    subsetCountry(country_id = input$country_choice, datalist = FD)
  })
  
  dict_to_show <- reactive({
    req(input$country_choice)
    data_subset()[["dict"]]
  })
  
  data_to_download <- reactive({
    req(input$country_choice, input$file_structure)
    
    ds <- data_subset()
    
    out <- ds
    
    out <- switch(
      input$file_structure,
      "Model" = generateModelSheet(
        yearly_data = ds[["y"]],
        dict        = ds[["dict"]]
      ),
      "All data (vertical)"   = ds,
      "All data (horizontal)" = transposeDatalist(ds)
    )
    
    # write_xlsx ждёт list of sheets (list of data.frames)
    if (inherits(out, c("data.frame", "tbl"))) {
      out <- list(data = out)
    }
    
    purrr::keep(out, \(x) inherits(x, c("data.frame", "tbl")))
  })
  
  download_filename <- reactive({ if (is.null(input$country_choice)) {return(NULL)} else {
    switch(input$file_structure,
           "Model" = "model",
           "All data (vertical)" = "filled_v",
           "All data (horizontal)" = "filled_h"
    )
  }
  })
  
  # Showing the name of a chosen country
  output$chosen_country <- renderText({
    
    if (is.null(input$country_choice)) {
      return("Choose a country to download data")
    } else {
      return(glue("{chosen_country_name()} data found:"))
    }
    
  })
  
  chosen_country_name <- reactive({
    req(input$country_choice)
    
    countries_tbl |>
      dplyr::filter(.data$country_id == input$country_choice) |>
      dplyr::pull(.data$country) |>
      dplyr::first()
  })
  
  # Printing a table with a dict for a chosen country
  output$table <- DT::renderDT(
    {
      req(input$country_choice)
      dict_to_show()
    },
    escape = FALSE,
    options = list(pageLength = 7, autoWidth = TRUE)
  )
  
  # Preparing datalists to download
  
  # Downloadable xlsx of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      req(input$country_choice)
      glue("{chosen_country_name()}_data_{download_filename()}.xlsx")
    },
    content = function(file) {
      dat <- data_to_download()
      
      shiny::validate(
        shiny::need(length(dat) > 0, "Нет данных для выгрузки.")
      )
      
      tryCatch(
        {
          # Настройки форматирования зависят от типа выгрузки
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
                dict = list(row = 2, col = 1) # "первая строчка": активная строка 2, столбец 1
              ),
              widths = list(
                d    = c(22),
                dict = c(41, 14, 20, 7, 22, 3, 3, 10, 10, 10, 10)
              )
            ),
            
            "All data (horizontal)" = list(
              freeze = list(),
              widths = list()
            ),
            
            list(freeze = list(), widths = list())
          )
          
          # Применяем только к реально существующим листам
          fmt$freeze <- fmt$freeze[names(dat)]
          fmt$widths <- fmt$widths[names(dat)]
          
          write_xlsx_formatted(
            sheets = dat,
            path = file,
            freeze_by_sheet = fmt$freeze,
            col_widths_by_sheet = fmt$widths
          )
        },
        error = function(e) {
          # Ошибка будет и в консоли, и всплывашкой в UI
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
    
}

# Run the application 
shinyApp(ui = ui, server = server)
