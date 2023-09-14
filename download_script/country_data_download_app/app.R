# An app to download updated data for a particular country

library(shiny)
library(here)
here::i_am("app.R")

source(here("service.R"))

# Import data
FD <- importData(data_y_fname = "extdata_y.csv", data_q_fname = "extdata_q.csv", data_m_fname = "extdata_m.csv", data_d_fname = "extdata_d.csv",
                 dict_fname = "dict.csv", path = here())
countries <- FD$extdata_y$country %>% unique()
#countries <- c("Russia", "France", "Saudi Arabia")

ui <-   fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("Choose a country to download data"),
    
    sidebarLayout(
      
      sidebarPanel(
        
        # Input: Choose country
        selectizeInput(
          'country_choice', 'Choose a country', choices = countries,
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue("Russian Federation"); }')
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
        dataTableOutput("table")
        
     )
  )
)


server <- function(input, output, session) {
  
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
