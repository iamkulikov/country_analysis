######## Call the sequence of database filling functions
library(here)

##### Choose filling mode

##### Where is the fillinf schedule saved? What are the data files?
here::i_am("_country_analysis_scripts/download_script/do_fill.R")
test <- 0

countries <- c("Armenia", "Brazil", "Bulgaria", "Greece", "China", "India", "Kyrgyz Republic", "Romania",
               "Russian Federation", "Slovak Republic", "South Africa", "Switzerland", "Ukraine")
formula_words <- c("lag", "rollsum", "rollavg", "rollvol", "mean", "last", "first", "min", "max", "sum", "coalesce", "share", "exp")

if (test == 0) {param_fname <- "0_database_params.xlsx"; data_fname <- "Imported_DB.xlsx";
data_d_fname <- "Imported_d_DB.xlsx"; filled_fname <- "Filled_DB.xlsx"; filled_d_fname <- "Filled_d_DB.xlsx"} else {
  param_fname <- "0_database_params_test.xlsx"; data_fname <- "Temp.xlsx"; data_d_fname <- "Temp_d.xlsx";
  filled_fname <- "Temp_filled.xlsx"; filled_d_fname <- "Temp_filled_d.xlsx"}

##### Import function definitions
source(here("_country_analysis_scripts","download_script","import.R"))
source(here("_country_analysis_scripts","download_script","fill.R"))

##### Import parameters and schedules
imp_params <- readImportParams(param_fname = here("_DB", param_fname), update_mode = 0)
for(i in seq_along(imp_params)) { assign(names(imp_params)[i], imp_params[[i]]) }
fillplan <- readFillParams(param_fname = here("_DB", param_fname))

##### Check integrity of the plans
fillplan <- checkNames(fillplan = fillplan, formula_words = formula_words)
fillplan <- checkUnique(fillplan = fillplan)
fillplan <- checkAvailability(fillplan = fillplan, impplan = impplan) %>% mutate(checks = check_names*check_unique*check_availability)
error_report <- fillplan %>% filter(checks == 0)
#error_report$formula

if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
  
      print("Checks passed")
      
      ##### The filling sequence
      D <- importOldData(data_fname = here("_DB", data_fname), data_d_fname = here("_DB", data_d_fname))
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
      writeDatafiles(data_fname = here("_DB", filled_fname), data_d_fname = here("_DB", filled_d_fname), extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d, dict = FD$dict, dict_d = FD$dict_d)
      
      ### Single countries
      writeCountryFile(countries = countries, datalist = FD)
      writeCountryModelFile(countries = countries, extdata_y = FD$extdata_y, saveplan = saveplan)
  
  } else {print("Errors found"); print(error_report)}
