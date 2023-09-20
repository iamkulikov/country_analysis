######## Call the sequence of plotting functions
library(here)

##### Where is the plotting schedule saved? What are the data files?
here::i_am("_country_analysis_scripts/graph_script/do_plot.R")

##### Parameters and source names
country_name <- "Armenia"
file_output <- "jpeg"
horizontal_size <- c(1800, 900)
vertical_size <- c(900, 900)
data_fname <- here("_DB", "Filled_DB.xlsx")
data_d_fname <- here("_DB", "Filled_d_DB.xlsx")
peers_fname <- here("_DB", "1_peers_params.xlsx")
plotparam_fname <- here(country_name, "Auto_report", "2_graphlib.xlsx")
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "structure_dynamic", "structure_country_comparison",
                 "structure_country_comparison_norm", "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", 
                 "bar_year_comparison", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic")
verbose <- T

##### Import function definitions
source(here("_country_analysis_scripts","download_script","import.R"))
source(here("_country_analysis_scripts","download_script","fill.R"))
source(here("_country_analysis_scripts","graph_script","plot.R"))

###### Import data from the local database
D <- importFilledData(data_fname = data_fname, data_d_fname = data_d_fname)

###### Determining country focus and peers
country_info <- getPeersCodes(country_name = country_name, peers_fname = peers_fname)

##### Importing plotting schedule and generating sources
graphplan <- getPlotSchedule(plotparam_fname = plotparam_fname, dict = D$dict)

##### Check integrity of the plans
graphplan <- graphplan %>% filter(active == 1) %>%
            checkGraphTypes(graph_types = graph_types) %>% 
            checkUnique %>%
            checkPeers(peer_groups = country_info$regions) %>%
            checkAvailability(dict = D$dict) %>% 
            mutate(checks = check_types*check_unique*check_peers*check_availability)
error_report <- graphplan %>% filter(checks == 0)


if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
  
  print("Checks passed")

    ##### Plotting cycle for each row of the plan
    for(i in seq_along(graphplan$graph_name)) {    
  
      ### Parsing single graph parameters  
      graph_params <- parseGraphPlan(graphrow = graphplan[i,], dict = D$dict, horizontal_size = horizontal_size, vertical_size = vertical_size)
      
      ### Fixing peers
      peers_iso2c <- fixPeers(country_info = country_info, peers = graph_params$peers, data = D)
      
      ### Filtering data to include only needed for the graph
      data_temp <- subsetData(data = D, graph_params = graph_params, country = country_info$country_iso2c, peers = peers_iso2c)
      
      ### Choosing the needed function based on the graph type 
      func_name <- funcNameTransform(graph_type = graph_params$graph_type)
      
      ### Producing the graph
      eval(parse(text= paste0( 
      "theplot <- ", func_name, "(data = data_temp, graph_params = graph_params, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c, verbose = verbose)"
        ) ))
      
      ### Saving file
      filename <- paste(graph_params$graph_name, file_output, sep=".")
      ggsave(path = here(country_name, "Auto_report"), filename = filename,  plot = theplot$graph, device = file_output,
             width = graph_params$width, height = graph_params$height, units = "px", dpi = 150)
      #theplot
      
    }
  
} else {print("Errors found"); print(error_report)}