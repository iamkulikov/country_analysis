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
indicators <- c("gdp_g", "cpi_yoy")
indicator_groups <- c("GDP decomposition", "GDP growth decomposition", "BOP", "BOP detailed", "IIP", "IIP detailed", 
                      "Budget revenue","Budget expenditure", "Budget balance", "Population drivers")
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")
peers <- c("none", "default", "neighbours", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "EM", "DM", "ACRA")
graph_filetypes <- c("jpeg", "png")

# Interface

ui <-   fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Graph parameters"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      ## Basic Input
      
      selectizeInput(
        'graph_type_choice', 'Graph type', choices = graph_types,
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
        'freq_choice', 'Data freq', choices = c("y", "q", "m", "d"),
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue("y"); }')
        )
      ))),
      
      ## Indicator input and comparison
      
      h3("Indicators"),
      fluidRow(column(8,
                      selectizeInput(
                        'ind1', 'Indicator 1', choices = indicators,
                          options = list(
                            placeholder = 'Please select an option below',
                            onInitialize = I('function() { this.setValue("gdp_g"); }')
                        ))
                      ),
               column(4, textInput("time_fix1", "Time fix", "none"))
      ),
      
      fluidRow(column(8,
                      selectizeInput(
                        'ind2', 'Indicator 2', choices = indicators,
                        options = list(
                          placeholder = 'Please select an option below',
                          onInitialize = I('function() { this.setValue("NA"); }')
                        ))
                      ),
              column(4, textInput("time_fix2", "Time fix", "none"))
      ),
      
      selectInput("ind_group", "Indicator group", choices = indicator_groups, NULL),
      
      h3("Peers"),
      fluidRow(column(6,selectizeInput("peers", "Peer group", choices = peers)),
               column(6,textInput("all", "All", 8))
      ),
      
      textInput("peers_list", "Custom", "RU, CN, BR"),
      
      ## Style
      
      h3("Style"),
      fluidRow(
               column(4,textInput("x_min", "X min", 8)),
               column(4,textInput("x_max", "X max", 8)),
               column(4,textInput("x_log", "X log", 8))
                      ),
      
      fluidRow(
               column(4,textInput("y_min", "Y min", 8)),
               column(4,textInput("y_max", "Y max", 8)),
               column(4,textInput("y_log", "Y log", 8))
      ),
      
      textInput("swap_axis", "Swap axis", "NA"),

      fluidRow(
        column(4,textInput("trend_type", "Trend", "NA")),
        column(4,textInput("recession", "Recession", "No")),
        column(4,selectInput("style", "Style preset", "ACRA"))
      ),
      
      ## Output
      
      h3("Output"),
      selectizeInput(
        'indicators_choice', 'Choose the graph type', choices = indicators,
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue("gdp_growth"); }')
        )
      ),
      
      selectInput("orientation", "Choose orientation",
                  choices = c("horizontal", "vertical")),
      
      fluidRow(
        column(8,textAreaInput("graph_title", "Graph Title", "Graph Title")),
        column(4,textInput("show_title", "Show", "No")),
      ),
      
      fluidRow(
        column(8,textInput("filename", "File name", "goodgraph")),
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
