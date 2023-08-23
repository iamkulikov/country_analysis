# An app to plot the country graphs

library(shiny)
library(here)
library(shinyWidgets)
here::i_am("app.R")

# Import all the necessary assets except data
source(here("plot_service.R"))
peers_fname <- here("1_peers_params.xlsx")

# Import data
FD <- importData(data_y_fname = "extdata_y.csv", data_q_fname = "extdata_q.csv", data_m_fname = "extdata_m.csv", 
                 data_d_fname = "extdata_d.csv", dict_fname = "dict.csv", path = here())

# Menu lists
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")
countries <- FD$extdata_y$country %>% unique()
indicators <- list("", "GDP" = "gdp_g", "CPI" = "cpi_yoy", "Death rate" = "dead", "R&D, % GDP" = "rnd", "WGI index" = "wgi1")
indicator_groups <- c("", "GDP decomposition", "GDP growth decomposition", "BOP", "BOP detailed", "IIP", "IIP detailed", 
                      "Budget revenue","Budget expenditure", "Budget balance", "Population drivers")
peers <- c("none","default", "custom", "neighbours", "formula", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "EM", "DM", "ACRA")
trend_types <- c("lm", "loess")
graph_themes <- c("ACRA", "ipsum", "economist", "minimal")
graph_groups <- c("macro", "budget", "external", "institutional", "demogr", "covid", "other")
graph_filetypes <- c("jpeg", "png")


# Interface

ui <-   fluidPage(

  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Graph parameters"),

  #tags$head(uiOutput("colour_helper")),
  
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
      selectInput("ind_group", "Indicator group", choices = indicator_groups, selected = ""),
      textInput("time_fix", "Time fix", ""),

      
      h3("Peers"),
      fluidRow(column(9,selectizeInput("peers", "Peer group", choices = peers,
                            options = list(
                                    placeholder = 'Please select an option below',
                                    onInitialize = I('function() { this.setValue("default"); }')
                                          )
                                       )),
               column(3,checkboxInput("all", "All"))
      ),
      
      selectizeInput(
        'peers_custom', 'Custom list', choices = countries, multiple = T,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue("Chile"); }')
        )),
      
      textInput("peers_formula", "Formula", ""),
      
      
      ## Style
      
      h3("Style"),
      fluidRow(
               column(4,textInput("x_min", "X min")),
               column(4,textInput("x_max", "X max")),
               column(4,checkboxInput("x_log", "X log"))
                      ),
      
      fluidRow(
               column(4,textInput("y_min", "Y min")),
               column(4,textInput("y_max", "Y max")),
               column(4,checkboxInput("y_log", "Y log"))
      ),
      
      fluidRow(
        column(4,selectizeInput("sec_y_axis_ind", "2nd Y-axis", choices = indicators, 
                        options = list(
                          placeholder = 'Select an option below',
                          onInitialize = I('function() { this.setValue("Chile"); }')
                                        )
                                )),
        column(4,textInput("sec_y_axis_coeff", "Axis mult", 10)),
        column(4,checkboxInput("swap_axis", "Swap axis"))
      ),

      fluidRow(
        column(4,checkboxInput("trend_type", "Trend")),
        column(4,checkboxInput("recession", "Recession")),
        column(4,checkboxInput("index", "Index"))
      ),
      
      selectInput("theme", "Style preset", graph_themes),
      
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
        column(4,checkboxInput("show_title", "Show Title")),
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
      #plotOutput("graph"),
      tableOutput("table"),
      textOutput("check")
      
    )
  )
)

# Calculations

server <- function(input, output, session) {

  
  # Listening to the user
  graphrow <- reactive({
                  data.frame(
                            graph_name = input$graph_name,
                            graph_title = input$graph_title,
                            graph_type = input$graph_type,
                            graph_group = input$graph_group,
                            data_frequency = input$data_frequency,
                            indicators = paste(input$indicators, collapse = ", "),
                            time_fix = input$time_fix,
                            peers = input$peers, # прописать конструктор
                            all = 1*input$all,
                            x_log = 1*input$x_log,
                            y_log = 1*input$y_log,
                            x_min = input$x_min,
                            x_max = input$x_max,
                            y_min = input$y_min,
                            y_max = input$y_max,
                            trend_type = input$trend_type,
                            index = 1*input$index,
                            #sec_y_axis = 1*input$sec_y_axis_ind,  прописать конструктор
                            theme = input$theme,
                            orientation = input$orientation,
                            show_title = 1*input$show_title,
                            active = 1
                            )
                      })

  # Table to export to excel plan
  output$table <- renderTable({ graphrow() })
  output$check <- renderText({input$x_min == ""})
  
  # Showing the name of a chosen country
  output$chosen_country <- renderText({
    
    if (is.null(input$country_choice)) {
      return("Choose a country to download data")
    } else {
      return(glue("{input$country_choice} data found:"))
    }
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
