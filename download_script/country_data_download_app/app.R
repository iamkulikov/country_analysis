# Descriprtion

library(shiny)
library(here)
here::i_am("app.R")

source(here("service.R"))
data_fname <- "Filled_DB.xlsx"
data_d_fname <- "Filled_d_DB.xlsx"

# Import data
FD <- importData(data_fname = here(data_fname), data_d_fname = here(data_d_fname))
countries <- FD$extdata_y$country %>% unique()
#countries <- c("Russia", "France", "Saudi Arabia")

ui <-   fluidPage(
    
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("Choose a country to download data"),
    
    sidebarLayout(
      
      sidebarPanel(
        
        selectizeInput(
          'country_choice', 'Choose a country', choices = countries,
          options = list(
            placeholder = 'Please select an option below',
            onInitialize = I('function() { this.setValue(""); }')
          )
        )
        
      ),
      
      mainPanel(
        
        textOutput("chosen_country"),
        dataTableOutput("table")
        
     )
  )
)


server <- function(input, output, session) {
  
  # Calculating a data subset for a chosen country
  #data_subset <- reactive(data.frame(a = c(1,2), dict = c(input$country_choice,8)))
  data_subset <- reactive(subsetCountry(country = input$country_choice, datalist = FD))
  dict_to_show <- reactive( data_subset() %>% '[['("dict") )
  data_to_download <- reactive( data_subset() %>% '[['("y") )
    
  # Showing the name of a chosen country
  output$chosen_country <- renderText({
    
    if (is.null(input$country_choice)) {
      return("Choose a country to download data")
    } else {
      return(glue::glue("{input$country_choice} data found:"))
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
