######## Call the sequence of import functions
library(here)

##### Choose import mode
test <- 0
update_mode <- 1    # 0 if all the data and containers should be new, 1 if only update
n_attempts <- 15    # how many times should we ping API's for needed data
formula_words <- c("lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first", "min", "pmin", "max", "pmax", "sum", "coalesce", "share", "exp",
                   "fromto", "year")

##### Where is import schedule saved? What are the data files?
here::i_am("_country_analysis_scripts/download_script/do_import.R")

if (test == 0) {param_fname <- "0_database_params.xlsx"; data_fname <- "Imported_DB.xlsx";
data_d_fname <- "Imported_d_DB.xlsx"; filled_fname <- "Filled_DB.xlsx"; filled_d_fname <- "Filled_d_DB.xlsx"} else {
  param_fname <- "0_database_params_test.xlsx"; data_fname <- "Temp.xlsx"; data_d_fname <- "Temp_d.xlsx";
  filled_fname <- "Temp_filled.xlsx"; filled_d_fname <- "Temp_filled_d.xlsx"}

##### Import function definitions
source(here("_country_analysis_scripts","download_script","import.R"))
source(here("_country_analysis_scripts","download_script","fill.R"))

##### Import parameters and schedules
imp_params <- readImportParams(param_fname = here("_DB", param_fname), update_mode = update_mode)
for(i in seq_along(imp_params)) { assign(names(imp_params)[i], imp_params[[i]]) }
impplan_full <- readImportParams(param_fname = here("_DB", param_fname), update_mode = 0)$impplan
  
##### Check integrity of the plans
impplan_full <- checkNames(fillplan = impplan_full, formula_words = formula_words)
impplan_full <- checkUnique(fillplan = impplan_full)
impplan_full <- checkFileExistence(impplan = impplan_full, extdata_folder = here("_DB", "_extsources")) %>% 
    mutate(checks = check_names*check_unique*check_exist)
error_report <- impplan_full %>% filter(checks == 0)

if (is.null(dim(error_report)[1]) | is.na(dim(error_report)[1]) | (dim(error_report)[1] == 0)) {
      
    print("Checks passed")
    
    ##### New import sequence starts
    if (update_mode == 0) { 
        D <- generateDataContainers(from = year_first, to = year_final) 
    } else {
        D <- importOldData(here("_DB", data_fname), here("_DB", data_d_fname))
    }
    
    data_dim <- captureDimensions(extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
  
    impplan_temp <- impplan
    
    ##### Main import cycle
    for (t in 1:n_attempts) {
      
      print(glue("Downloading attempt {t} of {n_attempts}"))
      if ( dim(impplan_temp)[1] == 0 ) {print("Nothing left to import"); break}
      D <- dropDataToUpdate(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      D <- tryImport(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      impplan_temp <- updateImportPlan(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
      
    }
  
    ##### Check whether data container was broken in the process
    if (all(captureDimensions(extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d) == data_dim)) {
      print("Dimensions were preserved")} else {
        print("Dimensions have changed. Something was not ok during the data import.")}
    
    ##### Export data
    D <- preExport(saveplan = saveplan, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
    writeDatafiles(data_fname = here("_DB", data_fname), data_d_fname = here("_DB", data_d_fname), extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d, dict = D$dict, dict_d = D$dict_d)

    
}  else {print("Errors found"); print(error_report)}