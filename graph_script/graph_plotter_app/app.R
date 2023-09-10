# An app to plot the country graphs

library(shiny)
library(here)
library(shinyWidgets)
here::i_am("app.R")

# Import all the necessary assets except data
source(here("plot_service.R"))
peers_fname <- here("1_peers_params.xlsx")

# Import data
D <- importData(data_y_fname = "extdata_y.csv", data_q_fname = "extdata_q.csv", data_m_fname = "extdata_m.csv", 
                 data_d_fname = "extdata_d.csv", dict_fname = "dict.csv", path = here())

# Menu lists
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")

countries_peers <- as.list(D$extdata_y$country_id %>% unique())
countries <- D$extdata_y$country %>% unique()
names(countries_peers) <- countries

indicators_all <- D$dict %>% select(indicator, indicator_code, source_frequency)
indicators_start <- as.list(indicators_all %>% pull(indicator_code))
names(indicators_start) <- indicators_all %>% pull(indicator)

indicator_groups <- c("", "GDP decomposition", "GDP growth decomposition", "BOP", "BOP detailed", "IIP", "IIP detailed", 
                      "Budget revenue","Budget expenditure", "Budget balance", "Population drivers")
peers <- c("default", "neighbours", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "EM", "DM", "ACRA")
peers_choice <- c("none", "default", "custom", "neighbours", "formula", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS", "EM", "DM", "ACRA")
trend_types <- c("", "lm", "loess")
graph_themes <- c("ACRA", "ipsum", "economist", "minimal")
graph_groups <- c("macro", "budget", "external", "institutional", "demogr", "covid", "other")
graph_filetypes <- c("jpeg", "png")

# Parameters
horizontal_size <- c(1800, 900)
vertical_size <- c(900, 900)
verbose <- F

# Interface

ui <-   fluidPage(

  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Graph country data"),
  
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
        'data_frequency', 'Data freq', choices = c(" ", "y", "q", "m", "d"),
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(" "); }')
        )
      ))),
      
      ## Indicator input and comparison
      
      selectizeInput('indicators', 'Indicators', choices = indicators_start, multiple = T,
                options = list(
                   placeholder = 'Please select one or more options below',
                   onInitialize = I('function() { this.setValue("gdp_g"); }')
                          )),
      selectInput("ind_group", "Indicator group", choices = indicator_groups, selected = ""),
      textInput("time_fix", "Time fix", ""),
      
      
      h3("Peers"),
      fluidRow(column(6,selectizeInput("peers", "Peer group", choices = peers_choice,
                            options = list(
                                    placeholder = 'Select',
                                    onInitialize = I('function() { this.setValue("default"); }')
                                          )
                                       )),
               column(6,textInput("peers_formula", "Formula", "")),
      ),
      
      selectizeInput(
        'peers_custom', 'Custom list', choices = countries_peers, multiple = T,
        options = list(
          placeholder = 'Select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )),
      
      checkboxInput("all", "Show all countries"),
      
      
      ## Style
      
      h3("Style"),
      fluidRow(
               column(3,textInput("x_min", "X min")),
               column(3,textInput("x_max", "X max")),
               column(3,numericInput("y_min", "Y min", "")),
               column(3,numericInput("y_max", "Y max", ""))             
                      ),
      
      fluidRow(
                column(4,checkboxInput("x_log", "X log")),
                column(3,checkboxInput("y_log", "Y log")),
                column(5,checkboxInput("swap_axis", "Swap axis"))
      ),
      

      fluidRow(
        column(4,checkboxInput("recession", "Recession")),
        column(3,checkboxInput("index", "Index")),
        column(5,checkboxInput("long_legend", "Long legend"))
      ),
      
      fluidRow(
        column(4,selectInput("trend_type", "Trend", choices = trend_types, selected = "")),
        column(8,selectInput("theme", "Style preset", graph_themes, selected = "ipsum"))
      ),
      
      fluidRow(
        column(8,selectizeInput("sec_y_axis_ind", "2nd Y-axis", choices = indicators_start, multiple = T,
                                options = list(
                                  placeholder = 'Select an option below',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
        )),
        column(4,numericInput("sec_y_axis_coeff", "Axis mult", "")),
        
      ),
      
    ),
    
    mainPanel(
      
      actionButton("plot_button", "Update Plot"),
      plotOutput("graph"),
      
      ## Output panel
      
      fluidRow(
        column(3, downloadButton("downloadJpeg", "Download jpeg")),
        column(3, downloadButton("downloadPng", "Download png")),
        column(3, downloadButton("downloadData", "Download data")),
        column(3, actionButton("copyPlan", "Copy plan"))
        ),
      
      br(),
        
      fluidRow(
        column(5,textAreaInput("graph_title", "Graph Title", "Graph Title")),
        column(2, selectizeInput(
          'graph_group', 'Graph group', choices = graph_groups,
          options = list(
            placeholder = '',
            onInitialize = I('function() { this.setValue("macro"); }')
          )
        )),
        column(3,textInput("graph_name", "File name", "goodgraph")),
        column(2, selectInput("orientation", "Orientation", choices = c("horizontal", "vertical")))
      ),
        
      checkboxInput("show_title", "Show Title"),

      
      tableOutput("table"),
      textOutput("check"),
      textOutput("check1"),
      textOutput("check2"),
      textOutput("check3")
      
    )
  )
)

# Calculations

server <- function(input, output, session) {

  
  ## Listening to the user
  graphplan <- reactive({
                  data.frame(
                            graph_name = input$graph_name,
                            graph_title = input$graph_title,
                            graph_type = input$graph_type,
                            graph_group = input$graph_group,
                            data_frequency = input$data_frequency,
                            indicators = indicators(), # external constructor
                            time_fix = input$time_fix,
                            peers = peers_string(), # external constructor
                            all = 1*input$all,
                            x_log = 1*input$x_log,
                            y_log = 1*input$y_log,
                            x_min = input$x_min,
                            x_max = input$x_max,
                            y_min = input$y_min,
                            y_max = input$y_max,
                            trend_type = input$trend_type,
                            index = 1*input$index,
                            sec_y_axis = sec_y_string(),  # external constructor
                            theme = input$theme,
                            orientation = input$orientation,
                            show_title = 1*input$show_title,
                            active = 1,
                            source_name = sources()
                            ) %>% mutate(across(everything(), ~replace(., . ==  "" , NA)))
                      })
  
  ## Constructing peers string from inputs
  
  peers_string <- reactive({
    ifelse(
      input$peers == "none", 
      0,
      ifelse(
        input$peers %in% peers,
        input$peers,
        ifelse(
          input$peers == "custom",
          paste0("custom: ", paste(input$peers_custom, collapse = ", ")),
          ifelse(
            input$peers == "formula",
            input$peers_formula,
            0
                )
              )
            )
          )
  })

  ## Constructing second axis string from inputs
  
  sec_y_string_temp <- reactive({ ifelse(all(input$sec_y_axis_ind != ""), paste(input$sec_y_axis_ind, collapse = ", "), NA) })
  sec_y_string <- reactive({ ifelse(!is.na(input$sec_y_axis_coeff), 
                                    paste(c(sec_y_string_temp(), input$sec_y_axis_coeff), collapse = ", "), 
                                    sec_y_string_temp()) })
  
  ## Updating second axis indicator list based on the chosen indicators
  
  observeEvent( input$indicators,
                {   
                  if (all(input$indicators != " ")) {
                    indicators_selected_temp <- indicators_all %>% filter(indicator_code %in% input$indicators) 
                    indicators_selected <- indicators_selected_temp %>% pull(indicator_code) %>% as.list
                    names(indicators_selected) <- indicators_selected_temp %>% pull(indicator)
                    updateSelectizeInput(session, "sec_y_axis_ind", label = "2nd Y-axis", 
                                         choices = indicators_selected, selected = "")
                  }
                })
  
  ## Updating indicators list based on the chosen frequency
  
  observeEvent( input$data_frequency,
                {   
                  if (input$data_frequency != " ") {
                  indicators_temp <- indicators_all %>% filter(source_frequency == input$data_frequency)} else {
                  indicators_temp <-indicators_all}
                  indicators <- as.list( indicators_temp %>% pull(indicator_code) )
                  names(indicators) <- indicators_temp %>% pull(indicator)
                  updateSelectizeInput(session, "indicators", label = "Indicators", choices = indicators, selected = "") 
                  })

  
  ## Updating source string based on updated indicators 
  
  generateSources <- function(indicators, dict) {
    x <- unlist(str_extract_all( string = indicators, pattern = paste(na.omit(dict$indicator_code), collapse = "|") ))
    x <- plyr::mapvalues(x, from = dict$indicator_code, to = dict$source_name, warn_missing = F)
    x <- unlist(strsplit(x, ", "))
    x <- x[x!="расчеты АКРА"]
    x <- c(unique(x), "расчеты АКРА")
    x <- toString(x)
    return(x)
  }
  
  indicators <- reactive({ paste(input$indicators, collapse = ", ") })
  sources <-  reactive({ generateSources(indicators = indicators(), dict = D$dict)  })
  
  ## Producing graph (copied from do_plot)
  
  observeEvent(input$plot_button, {
  
        ##### Update graphplan     
    
        ##### Update country groups
        country_info <- getPeersCodes(country_name = input$country_choice, peers_fname = peers_fname)
    
        ##### Check integrity of the plans
        graphplan_aug <- graphplan() %>% filter(active == 1) %>%
          checkGraphTypes(graph_types = graph_types) %>% 
          checkUnique %>%
          checkPeers(peer_groups = country_info$regions) %>% # после того как напишу check проверить
          checkAvailability(dict = D$dict) %>%
          mutate(checks = check_types*check_unique*check_peers*check_availability)
        error_report <- graphplan_aug %>% filter(checks == 0)
      
        
        if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
          
            
            ### Parsing single graph parameters  
            graph_params <- parseGraphPlan(graphrow = graphplan(), dict = D$dict, horizontal_size = horizontal_size, vertical_size = vertical_size)
            
            ### Fixing peers
            peers_iso2c <- fixPeers(country_info = country_info, peers = graph_params$peers, data = D)
            updateSelectizeInput(session, "peers_custom", label = "Custom list", choices = countries_peers, selected = peers_iso2c)
            
            ### Filtering data to include only needed for the graph
            data_temp <- subsetData(data = D, graph_params = graph_params, country = country_info$country_iso2c, peers = peers_iso2c)
            
            ### Choosing the needed function based on the graph type 
            func_name <- funcNameTransform(graph_type = graph_params$graph_type)
            
            ### Producing the graph
            eval(parse(text= paste0( 
              "output$graph <- renderPlot(", func_name, "(data = data_temp, graph_params = graph_params, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c, verbose = verbose))"
            ) ))
            
          
        } else {print("Errors found"); print(error_report)}
        

        
    })

  # Table to export to excel plan
  output$table <- renderTable({ graphplan() })
  #output$table <- renderTable({ indicators_temp() })
  
  # Checks
  output$check <- renderText({ sec_y_string_temp() })
  output$check1 <- renderText({ input$sec_y_axis_coeff != "" })
  output$check2 <- renderText({ paste0("all non-na" , input$data_frequency != "" ) })
  output$check3 <- renderText({ paste0("non-na" ,!is.na(input$sec_y_axis_ind)) })
  
  # Downloadable files
  # output$downloadData <- downloadHandler(
  #   filename = function() { glue("{input$country_choice}_data_{download_filename()}.xlsx") },
  #   content = function(file) { write_xlsx(data_to_download(), file, col_names = T, format_headers = T) }
  # )
  
  # Showing the name of a chosen country
  output$chosen_country <- renderText({
    
    if (is.null(input$country_choice)) {
      return("Choose a country to download data")
    } else {
      return(glue("{input$country_choice} data found:"))
    }
    
  })

  # Saving file
  # filename <- paste(graph_params$graph_name, file_output, sep=".")
  # ggsave(path = here(country_name, "Auto_report"), filename = filename,  plot = theplot, device = file_output,
  #        width = graph_params$width, height = graph_params$height, units = "px", dpi = 150)
  #theplot
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
