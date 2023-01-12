######## Call the sequence of database filling functions

##### Choose filling mode

##### Where is the fillinf schedule saved? What are the data files?
#setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
setwd("C:/Projects/country_analysis/_DB")
param_fname <- "0_database_params_test.xlsx"
#data_fname <- "Imported_DB.xlsx"
#data_d_fname <- "Imported_d_DB.xlsx"
data_fname <- "Temp.xlsx"
data_d_fname <- "Temp_d.xlsx"
#filled_fname <- "Filled_DB.xlsx"
#filled_d_fname <- "Filled_d_DB.xlsx"
filled_fname <- "Temp.xlsx"
filled_d_fname <- "Temp_d.xlsx"
countries <- c("Armenia", "Brazil", "Bulgaria", "Greece", "China", "India", "Kyrgyz Republic", "Romania",
               "Russian Federation", "Slovak Republic", "South Africa", "Switzerland", "Ukraine")

##### Import function definitions
source("../_country_analysis_scripts/download_script/import.R")
source("../_country_analysis_scripts/download_script/fill.R")

##### Import parameters and schedules
impp_params <- readImportParams(param_fname = param_fname, update_mode = update_mode)
for(i in seq_along(imp_params)) { assign(names(imp_params)[i], imp_params[[i]]) }
fillplan <- readFillParams(param_fname = param_fname, update_mode = update_mode)
for(i in seq_along(fill_params)) { assign(names(fill_params)[i], fill_params[[i]]) }

##### Check integrity of the plans
checklist[1] <- checkNames(filldata)
checklist[2] <- checkDefinitions(filldata)
checklist[3] <- checkExistence(filldata, impdata)

##### New filling sequence starts
D <- importOldData(data_fname, data_d_fname)
captureDimensions(D)
print("Filling started")
D <- createDateColumns(D)

for (i in 1:dim(filldata)[1]) {
  
  oldfreq <- filldata$old_frequency[i]
  oldfreq_long <- case_when(oldfreq == "y" ~ "year", oldfreq == "q" ~ "quarter", oldfreq == "m" ~ "month", oldfreq == "d" ~ "date")
  newfreq <- filldata$new_frequency[i]
  newfreq_long <- case_when(newfreq == "y" ~ "year", newfreq == "q" ~ "quarter", newfreq == "m" ~ "month", newfreq == "d" ~ "date")
  active <- filldata$active1[i]
  oldcode <- filldata$old_indicator_code[i]
  newcode <- filldata$new_indicator_code[i]
  formula <- filldata$formula[i]
  
  FD <- fill(D)
  
  }

FD <- dropDate(FD)
checkDimensions(FD)

##### Export data

### All countries
D <- preExport(saveplan = saveplan, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
writeDatafiles(data_fname = data_fname, data_d_fname = data_d_fname, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d, dict = D$dict, dict_d = D$dict_d)

### Single countries
writeCountryFile(countries, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)
writeCountryModelFile(countries, extdata_y = D$extdata_y, extdata_q = D$extdata_q, extdata_m = D$extdata_m, extdata_d = D$extdata_d)


