######## Call the sequence of plotting functions
library(here)

##### Where is the plotting schedule saved? What are the data files?
here::i_am("_country_analysis_scripts/graph_script/do_plot.R")

##### Parameters and source names
country_name <- "China"
file_output <- "jpg"
horizontal_size <- c(1800, 900)
vertical_size <- c(900, 900)
data_fname <- here("_DB", "Filled_DB.xlsx")
data_d_fname <- here("_DB", "Filled_d_DB.xlsx")
peers_fname <- here("_DB", "1_peers_params.xlsx")
plotparam_fname <- here(country_name, "Auto_report", "2_graphlib.xlsx")

##### Import function definitions
source(here("_country_analysis_scripts","download_script","import.R"))
source(here("_country_analysis_scripts","download_script","fill.R"))
source(here("_country_analysis_scripts","graph_script","plot.R"))

###### Import data from local database
D <- importFilledData(data_fname = data_fname, data_d_fname = data_d_fname)

###### Determining country focus and peers
country_info <- getPeersCodes(country_name = country_name, peers_fname = peers_fname)

##### Import plotting schedule
graphdata <- getPlotSchedule(plotparam_fname = plotparam_fname)

##### Generating sources
graphdata <- generatePlotSources(graphdata = graphdata, dict = dict)

##### Plotting cycle
  
  ### Parse graph parameters  

  ### Filter data

  ### Use needed graph type 

  ### Produce graph


##### Saving
