######## Call the sequence of import functions

##### Choose import mode
update_mode <- 1        # 0 if all the data and containers should be new, 1 if only update
n_attempts <- 10   # how many times should we ping API's for needed data

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

  readImportParams(param_fname = param_fname, update_mode = update_mode)
  #print(impplan)
  if (update_mode == 0) { generateDataContainers(from = year_first, to = year_final) } else {
    importOldData(data_fname, data_d_fname)
  }
  impplan_temp <- impplan
  
  for (t in 1:n_attempts) {
    
    print(glue("Downloading attempt {t} of {n_attempts}"))
    if ( dim(impplan_temp)[1] == 0 ) {print("Nothing left to import"); break}
    dropDataToUpdate(impplan = impplan_temp, extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
    tryImport(impplan = impplan_temp, extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
    impplan_temp <- updateImportPlan(impplan = impplan_temp, extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
    
  }
  
  preExport(saveplan = saveplan, extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
  #print(head(extdata_y))
  writeDatafiles(data_fname = data_fname, data_d_fname = data_d_fname, extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict, dict_d = dict_d)
  