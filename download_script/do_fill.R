######## Call the sequence of database filling functions
library(here)

##### Choose filling model

##### Where is the filling schedule saved? What are the data files?
here::i_am("download_script/do_fill.R")
test <- 0

countries <- c("Armenia", "Belarus", "Brazil", "Bulgaria", "Greece", "China", "India", "Kazakhstan", "Kyrgyz Republic", "Mongolia",
               "Romania", "Russian Federation", "Slovak Republic", "South Africa", "Switzerland", "Turkiye", "Ukraine",
                "Uzbekistan", "United Arab Emirates", "Ethiopia")
# "Iran, Islamic Rep." - что делать с этой точкой на конце? так папка называться не может, но название 
# в базе именно такое (дропнуть изначально при кодировке базы все точки на концах?)
formula_words <- c("lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first", "min", "pmin", "max", "pmax", 
                   "sum", "coalesce", "share", "exp", "fromto", "year", "na_if", "cummax", "cummin", "cumsum", "ceiling",
                   "letterize")
sheet_keys <- c(y = "y", q = "q", m = "m")

if (test == 0) {param_fname <- "0_database_params.xlsx"; data_fname <- "Imported_DB.rds";
data_d_fname <- "Imported_DB_d.rds"; filled_fname <- "Filled_DB.xlsx"; filled_d_fname <- "Filled_d_DB.xlsx"} else {
  param_fname <- "0_database_params_test.xlsx"; data_fname <- "Temp.xlsx"; data_d_fname <- "Temp_d.xlsx";
  filled_fname <- "Temp_filled.xlsx"; filled_d_fname <- "Temp_filled_d.xlsx"}

app_dirs <- c(here::here("download_script", "country_data_download_app"), here::here("graph_script", "graph_plotter_app"))

##### Import function definitions
source(here("download_script","import.R"))
source(here("download_script","fill.R"))

##### Import parameters and schedules
imp_params <- readImportParams(param_fname = here("assets", "_DB", param_fname), update_mode = 0)
for(i in seq_along(imp_params)) { assign(names(imp_params)[i], imp_params[[i]]) }
fillplan <- readFillParams(param_fname = here("assets", "_DB", param_fname))

##### Check integrity of the plans
fillplan <- checkNames(fillplan = fillplan, formula_words = formula_words)
fillplan <- checkUnique(fillplan = fillplan)
fillplan <- checkAvailability(fillplan = fillplan, impplan = impplan) |> mutate(checks = check_names*check_unique*check_availability)
error_report <- fillplan |> filter(checks == 0)
#error_report$formula

if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
  
      print("Checks passed")
      
      ##### The filling sequence
      D <- importData(yqm_file = here("assets", "_DB", data_fname), d_file = here("assets", "_DB", data_d_fname), sheet_keys = sheet_keys, format = "auto")
      data_dim <- captureDimensions(extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      print("Filling started")
      D <- createDateColumns(extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      FD <- fill(fillplan = fillplan, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      remove(D)  
      FD <- dropDateColumns(extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d)
      
      ##### Check whether data container was broken in the process
      if (all(captureDimensions(extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d) == data_dim)) {
        print("Dimensions were preserved")} else {
          print("Dimensions have changed. Something was not ok during the calculation.")}
      
      ##### Export data
      
      ### All countries
      saveplan <- generateSaveplan(impplan = impplan, fillplan = fillplan)
      FD <- preExport(saveplan = saveplan, extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d)
      # writeDatafiles(data_fname = here("assets", "_DB", filled_fname), data_d_fname = here("assets", "_DB", filled_d_fname), extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d, dict = FD$dict, dict_d = FD$dict_d)
      paths <- writeDatafiles(y = FD$extdata_y, q = FD$extdata_q, m = FD$extdata_m, d = FD$extdata_d, dict = FD$dict, dict_d = FD$dict_d, 
                     dir = here("assets", "_DB"), stem = "Filled_DB")
      replicateSavedFiles(src_paths   = unname(paths[c("rds_yqm", "rds_d")]), target_dirs = app_dirs, method = "copy")
      # writeDatafilesCsv(datalist = FD, path = here("download_script", "country_data_download_app"))
      # writeDatafilesCsv(datalist = FD, path = here("graph_script", "graph_plotter_app"))
      
      ### Single countries
      writeCountryFile(countries = countries, datalist = FD)
      writeCountryModelFile(countries = countries, extdata_y = FD$extdata_y, saveplan = saveplan)
  
  } else {print("Errors found"); print(error_report)}
