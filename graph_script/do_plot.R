######## Call the sequence of plotting functions

library_names <- c("dplyr","reshape2","ggplot2","ggthemes","countrycode","readxl","tidyr","data.table","writexl","unikn",
                   "ggtext","svglite","stringr","directlabels","fanplot", "forcats", "rlang", "here",
                   "ggfan",  # проверять, не вернулся ли на CRAN
                   #"hrbrthemes",
                   "glue","readr", "showtext")

#devtools::install_github("jasonhilton/ggfan")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

##### Where is the plotting schedule saved? What are the data files?
here::i_am("graph_script/do_plot.R")

##### Import function definitions
source(here("download_script","import.R"))
source(here("download_script","fill.R"))
source(here("graph_script","check_graphplan.R"))
source(here("graph_script","prepare_elements.R"))
source(here("graph_script","plot_types.R"))
source(here("graph_script","plot_themes.R"))

##### Parameters and source names
country_code3 <- "BRA"
country_code2 <- countrycode::countrycode(country_code3, "iso3c", "iso2c", warn = FALSE)
file_output <- "jpeg"
horizontal_size <- c(1800, 900)
vertical_size <- c(850, 850)
data_fname <- here("assets", "_DB", "Filled_DB.rds")
data_d_fname <- here("assets", "_DB", "Filled_DB_d.rds")
peers_fname <- here("assets", "_DB", "1_peers_params.xlsx")
graphplan_columns <- c("graph_name", "graph_title", "graph_type", "graph_group", "data_frequency", "indicators", "time_fix", "peers", "all", "x_log",
                       "y_log", "x_min", "x_max", "y_min", "y_max", "trend_type", "index", "recession", "sec_y_axis", "swap_axis", "long_legend", "vert_lab",
                       "short_names", "theme", "orientation", "show_title", "active")
graph_types <- c("scatter_dynamic", "scatter_country_comparison", "scatter_before_after", 
                 "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm", "bar_year_comparison", 
                 "structure_dynamic", "structure_country_comparison", "structure_country_comparison_norm",
                 "lines_indicator_comparison", "lines_country_comparison", 
                 "density_fix", "distribution_dynamic", "distribution_year_comparison", "distribution_indicator_comparison",
                 "triangle")
trend_types <- c("lm" ,"loess")
orient_types <- c("horizontal", "vertical")
theme_types <- c("ipsum", "ACRA_light", "ACRA_clean", "grey", "economist", "minimal")
sheet_keys <- c(y = "y", q = "q", m = "m")
verbose <- T

###### Import data from the local database
# D <- importFilledData(data_fname = data_fname, data_d_fname = data_d_fname)
D <- importData(yqm_file = data_fname, d_file = data_d_fname, sheet_keys = sheet_keys, format = "auto", add_time = T)
country_name <- D$extdata_y |> select(country_id, country) |> unique() |> filter(country_id == country_code2) |> pull(country)
plotparam_fname <- here("assets", country_name, "Auto_report", "2_graphlib.xlsx")

###### Determining country focus and peers
country_info <- getPeersCodes(country_iso3c = country_code3, peers_fname = peers_fname)

##### Importing plotting schedule and generating sources
source(here("graph_script","plot_types.R"))
graphplan <- getPlotSchedule(plotparam_fname = plotparam_fname, dict = D$dict)

##### Check integrity of the plans
if (checkColumns(graphplan = graphplan, graphplan_columns = graphplan_columns) == 1 & checkEmpty(graphplan = graphplan) == 1) {
  graphplan <- graphplan |> filter(active == 1) |>
              checkGraphTypes(graph_types = graph_types) |> checkFreq() |>
              checkUnique() |> checkAvailability(dict = D$dict) |> 
              checkPeers(peer_groups = country_info, dict = D$dict) |> checkTimes(warn_invalid = TRUE) |>
              checkBinaryParams() |> checkNumericParams() |> checkTrend(trend_types = trend_types) |> 
              checkTheme(theme_types = theme_types) |> checkOrientation(orient_types = orient_types) |> 
              mutate(checks = check_types*check_freq*check_unique*check_availability*check_peers*check_times*check_binary*
                       check_num*check_trend*check_theme*check_orient)
  error_report <- graphplan |> filter(checks == 0) |> select(graph_name, indicators, data_frequency, starts_with("check_"))
} else {error_report <- data.frame("Check number of columns and rows")}

##### Plotting sequence
if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
  
  print("Checks passed")

    ##### Plotting cycle for each row of the plan
    for(i in seq_along(graphplan$graph_name)) {    
      
      ### Parsing single graph parameters (and filling with defaults)
      graph_params <- parseGraphPlan(graphrow = graphplan[i,], dict = D$dict, horizontal_size = horizontal_size, vertical_size = vertical_size)

      ### Fixing peers
      peers_iso2c <- fixPeers(country_info = country_info, params = graph_params, data = D)

      ### Fill empty graph parameters with defaults and calculate dependent labels
      graph_params <- fillGraphPlan(parsedrow = graph_params, data = D, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c)
      
      ### Filtering data to include only needed for the graph
      data_temp <- subsetData(data = D, graph_params = graph_params, country_code = country_info$country_iso2c, peers_code = peers_iso2c)
      
      ### Choosing the needed function based on the planned graph type 
      func_name <- funcNameTransform(graph_type = graph_params$graph_type)
      
      ### Producing the graph
      eval(parse(text= paste0( 
      "theplot <- ", func_name, "(data = data_temp, graph_params = graph_params, country_iso2c = country_info$country_iso2c, peers_iso2c = peers_iso2c, verbose = verbose)"
        ) ))
      # theplot$data
      
      ### Saving graph file
      filename <- paste(graph_params$graph_name, file_output, sep=".")
      ggsave(path = here("assets", country_name, "Auto_report"), filename = filename,  plot = theplot$graph, device = file_output,
            width = graph_params$width, height = graph_params$height, units = "px", dpi = 150)
      #theplot
      
    }
  
} else {explainErrors(error_report = error_report)}