######## Call the sequence of import functions

##### Choose import mode
update_mode <- 1        # 0 if all the data and containers should be new, 1 if only update
n_attempts <- 5   # how many times should we ping API's for needed data

##### Where is import schedule saved? What are the data files?
#setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
setwd("C:/Projects/country_analysis/_DB")
param_fname <- "0_database_params.xlsx"
data_fname <- "Imported_DB.xlsx"
data_d_fname <- "Imported_d_DB.xlsx"
#data_fname <- "Temp.xlsx"
#data_d_fname <- "Temp_d.xlsx"

##### Import function definitions
source("../_country_analysis_scripts/download_script/import.R")

##### New import sequence starts

  imp_params <- readImportParams(param_fname = param_fname, update_mode = update_mode)
  for(i in seq_along(imp_params)) { assign(names(imp_params)[i], imp_params[[i]]) }
  
  if (update_mode == 0) { 
      D <- generateDataContainers(from = year_first, to = year_final) 
  } else {
      D <- importOldData(data_fname, data_d_fname)
  }
  
  #for(i in seq_along(data_containers)) { assign(names(data_containers)[i], data_containers[[i]]) }
  impplan_temp <- impplan
  
  for (t in 1:n_attempts) {
    
    print(glue("Downloading attempt {t} of {n_attempts}"))
    if ( dim(impplan_temp)[1] == 0 ) {print("Nothing left to import"); break}
    D <- dropDataToUpdate(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
    D <- tryImport(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
    impplan_temp <- updateImportPlan(impplan = impplan_temp, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
    
  }

  D <- preExport(saveplan = saveplan, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
  writeDatafiles(data_fname = data_fname, data_d_fname = data_d_fname, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d, dict = D$dict, dict_d = D$dict_d)
  