######## Call the sequence of plotting functions
library(here)

##### Where is the plotting schedule saved? What are the data files?
here::i_am("_country_analysis_scripts/graph_script/do_plot.R")

##### Parameters and source names
country_name <- "China"
file_output <- "jpg"
horizontal_size <- c()
vertical_size <- c()
data_fname <- "Filled_DB.xlsx"
peers_fname <- "1_peers_params.xlsx"
plotparam_fname <- "2_graphlib.xlsx"

##### Import function definitions
source(here("_country_analysis_scripts","download_script","import.R"))
source(here("_country_analysis_scripts","download_script","fill.R"))
source(here("_country_analysis_scripts","graph_script","plot.R"))

###### Import data from local database
D <- importFilledData(data_fname = here("_DB", data_fname))

###### Determining country focus and peers
country_iso2c <- unique(extdata_y$country_id[extdata_y$country == country_name])


##### Import plotting schedule

##### Generating sources

##### Plotting cycle
  
  ### Parse graph parameters  

  ### Filter data

  ### Use needed graph type 

  ### Produce graph



