# An app to plot the country graphs

library(shiny)
library(here)
here::i_am("app.R")

# Import all the necessary assets except data
source(here("plot_service.R"))
peers_fname <- here("1_peers_params.xlsx")

# Import data
FD <- importData(data_y_fname = "extdata_y.csv", data_q_fname = "extdata_q.csv", data_m_fname = "extdata_m.csv", 
                 data_d_fname = "extdata_d.csv", dict_fname = "dict.csv", path = here())

# Menu lists
countries <- FD$extdata_y$country %>% unique()
indicators <- list("", "GDP" = "gdp_g", "CPI" = "cpi_yoy", "Death rate" = "dead", "R&D, % GDP" = "rnd", "WGI index" = "wgi1")
indicator_groups <- c("GDP decomposition", "GDP growth decomposition", "BOP", "BOP detailed", "IIP", "IIP detailed", 
                      "Budget revenue","Budget expenditure", "Budget balance", "Population drivers")
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")
peers <- c("none", "default", "custom", "neighbours", "formula", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "EM", "DM", "ACRA")
graph_groups <- c("macro", "budget", "external", "institutional", "demogr", "covid", "other")
graph_filetypes <- c("jpeg", "png")

# Interface

ui <-   fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Graph parameters"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      ## Basic Input
      
      selectizeInput(
        'graph_type', 'Graph type', choices = graph_types,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue("bar_country_comparison"); }')
        )),
      
      fluidRow(column(8,
              selectizeInput(
                'country_choice', 'Country', choices = countries,
                options = list(
                    placeholder = 'Select an option below',
                    onInitialize = I('function() { this.setValue("Russian Federation"); }')
                    ))
      ),      
      
      column(4,     
      selectizeInput(
        'data_frequency', 'Data freq', choices = c("y", "q", "m", "d"),
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue("y"); }')
        )
      ))),
      
      ## Indicator input and comparison
      
      selectizeInput('indicators', 'Indicators', choices = indicators, multiple = T,
              options = list(
                 placeholder = 'Please select an option below',
                 onInitialize = I('function() { this.setValue("gdp_g"); }')
                        )),
      selectInput("ind_group", "Indicator group", choices = indicator_groups, NULL),
      textInput("time_fix1", "Time fix", "none"),

      
      h3("Peers"),
      fluidRow(column(9,selectizeInput("peers", "Peer group", choices = peers)),
               column(3,radioButtons("all", "All", choices = " ", selected = character(0), inline = T))
      ),
      
      selectizeInput(
        'peers_custom', 'Custom list', choices = countries, multiple = T,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue("Chile"); }')
        )),
      
      textInput("peers_formula", "Formula", NULL),
      
      
      ## Style
      
      h3("Style"),
      fluidRow(
               column(4,textInput("x_min", "X min", 8)),
               column(4,textInput("x_max", "X max", 8)),
               column(4,radioButtons("x_log", "X log", choices = " ", selected = character(0), inline = T))
                      ),
      
      fluidRow(
               column(4,textInput("y_min", "Y min", 8)),
               column(4,textInput("y_max", "Y max", 8)),
               column(4,radioButtons("y_log", "Y log", choices = " ", selected = character(0), inline = T))
      ),
      
      fluidRow(
        column(4,selectizeInput("sec_y_axis_ind", "2nd Y-axis", choices = indicators)),
        column(4,textInput("sec_y_axis_coeff", "Axis mult", 10)),
        column(4,radioButtons("swap_axis", "Swap axis", choices = " ", selected = character(0), inline = T))
      ),

      fluidRow(
        column(4,radioButtons("trend_type", "Trend", choices = " ", selected = character(0), inline = T)),
        column(4,radioButtons("recession", "Recession", choices = " ", selected = character(0), inline = T)),
        column(4,radioButtons("index", "Index", choices = " ", selected = character(0), inline = T))
      ),
      
      selectInput("style", "Style preset", "ACRA"),
      
      ## Output
      
      h3("Output"),
      
      fluidRow(
        column(6, selectizeInput(
          'graph_group', 'Graph group', choices = graph_groups,
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue("macro"); }')
          )
        )),
        column(6, selectInput("orientation", "Orientation", choices = c("horizontal", "vertical")))
      ),
      
      fluidRow(
        column(8,textAreaInput("graph_title", "Graph Title", "Graph Title")),
        column(4,radioButtons("show_title", "Show Title", choices = " ", selected = character(0), inline = T)),
      ),
      
      fluidRow(
        column(8,textInput("graph_name", "File name", "goodgraph")),
        column(4,selectInput("filetype", "File type", choices = graph_filetypes)),
      ),
      
      # Button
      downloadButton("dData", "Plot")
      
    ),
    
    mainPanel(
      
      textOutput("chosen_country"),
      dataTableOutput("table")
      
    )
  )
)

# Calculations

server <- function(input, output, session) {
  
  # Import peers for a chosen country
  country_info <- reactive({getPeersCodes(country_name = country_name, peers_fname = peers_fname)})
  
  # Calculating a data subset for a chosen country
  data_subset <- reactive({ if (is.null(input$country_choice)) {return(NULL)} else {
    subsetCountry(country = input$country_choice, datalist = FD)}
  })
  
  dict_to_show <- reactive({ if (is.null(input$country_choice)) {return(NULL)} else {data_subset() %>% '[['("dict")} })
  
  data_to_download <- reactive({ if (is.null(input$country_choice)) {return(NULL)} else {
    switch(input$file_structure,
           "Model" = data_subset() %>% '[['("y") %>% generateModelSheet(dict_to_show()),
           "All data (vertical)" = data_subset(),
           "All data (horizontal)" = transposeDatalist(data_subset()) 
    )
  }
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
      return(glue("{input$country_choice} data found:"))
    }
    
  })
  
  # Printing a table with a dict for a chosen country
  output$table <- renderDataTable({
    
    if (is.null(input$country_choice)) {
      return(NULL)
    } else {
      dict_to_show()
    }
    
  }, escape = FALSE, options = list(pageLength = 7, autoWidth = TRUE))
  
  # Preparing datalists to download
  
  # Downloadable xlsx of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() { glue("{input$country_choice}_data_{download_filename()}.xlsx") },
    content = function(file) { write_xlsx(data_to_download(), file, col_names = T, format_headers = T) }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
