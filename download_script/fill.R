#   All the functions to make calculations on imported data

##  Load packages

library("dplyr")
library("reshape2")
library("countrycode")
library("readxl")
library("tidyr")
library("data.table")
library("writexl")
library("stringr")
library("purrr")
library("gsubfn")
library("tidyquant")
library("timetk")
library("glue")
library("lubridate")

##### Import data

setwd("C:/Projects/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/download_data")

#data_fname <- "Imported_DB.xlsx"
#data_d_fname <- "Imported_d_DB.xlsx"
data_fname <- "Temp.xlsx"
data_d_fname <- "Temp_d.xlsx"

for (i in c("y", "q", "m")) {
  
  eval(parse(text = glue("a <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
  eval(parse(text = glue("extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                          col_types = c('text', 'text', rep('numeric', a-2)))") ))
  
}

a <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                        col_types = c("text", "text", "date", rep("numeric", a-3)))


##### Generate date columns in data (first day in each period is chosen)

extdata_y <- extdata_y %>% mutate(date = make_date(year = year, month = 12, day = 1))
extdata_q <- extdata_q %>% mutate(date = make_date(year = year, month = 3*(quarter-1)+1, day = 1))
extdata_m <- extdata_m %>% mutate(date = make_date(year = year, month = month, day = 1))


##### Import and filling schedule

#param_fname <- "0_database_params.xlsx"
param_fname <- "0_database_params_test.xlsx"
impdata <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
filldata <- read_excel(param_fname, sheet = "fill", col_names = T, skip=1)
filldata <- filldata %>% filter(active1==1)


##### Filling cycle

for (i in 1:dim(filldata)[1]) {
  
  #i = 4
  oldfreq <- filldata$old_frequency[i]
  oldfreq_long <- case_when(oldfreq == "y" ~ "year", oldfreq == "q" ~ "quarter", oldfreq == "m" ~ "month", oldfreq == "d" ~ "date")
  newfreq <- filldata$new_frequency[i]
  newfreq_long <- case_when(newfreq == "y" ~ "year", newfreq == "q" ~ "quarter", newfreq == "m" ~ "month", newfreq == "d" ~ "date")
  active <- filldata$active1[i]
  oldcode <- filldata$old_indicator_code[i]
  newcode <- filldata$new_indicator_code[i]
  formula <- filldata$formula[i]
  
  
  #### Calculating new variables of the same frequency
  
  if (oldfreq == newfreq & active == 1 & str_detect(formula, "roll") == F & formula != "share") {
    
    a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by(country) %>% mutate({newcode} = {formula}) %>% ungroup()")
    eval(parse(text = a)); print(a)
    
  } else {}
  
  
  #### Calculating new rolling variables of the same frequency 
  
  if (oldfreq == newfreq & active == 1 & str_detect(formula, "roll") == T ) {
    
    # parse formula
    type <- reduce2(c('vol', 'avg'), c('sd', 'mean'), .init = substr(formula,5,7), str_replace)
    windowlen <- as.numeric( substr(formula, str_locate(formula, ", ")[1,2]+1, str_locate(formula, '[)]')[1,1]-1 ) )
    oldcode <- substr(formula, str_locate(formula, "[(]")[1,2]+1, str_locate(formula, '[,]')[1,1]-1 )
    
    a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by(country) %>% \\
                    mutate({newcode} = rollapply({oldcode}, windowlen, {type}, align='right', fill=NA)) %>% ungroup()")
    eval(parse(text = a)); print(a)
    
  } else {}
  
  
  #### Aggregating the same variables to lower frequency d > m > q > y (d<m but frequency is vice versa)
  
  if ( oldfreq < newfreq & active == 1 & (formula=="last"|formula=="first"|formula=="mean"|formula =="max"|formula =="min"|formula=="sum") ) {
    
    a <- glue("aggreg <- extdata_{oldfreq} %>% select(date, country_id, {oldcode}) %>% group_by(country_id) %>% \\
      summarise_by_time(.by = '{newfreq_long}', .date_var = date, {newcode} = {formula}({oldcode}), .type = 'floor') %>% ungroup()")
    eval(parse(text = a)); print(a)
    
    eval(parse(text=glue("extdata_{newfreq} <- extdata_{newfreq} %>% \\
          left_join(aggreg, by = c('country_id'='country_id', 'date'='date'))") ))
    
  } else {}
  
  
  #### Calculating shares 
  #i=68
  if (oldfreq == newfreq & active == 1 & filldata$formula[i] =="share" ) {
    
    a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by({oldfreq_long}) %>% \\
            mutate({newcode} = {oldcode}*100/sum({oldcode}, na.rm=T)) %>% ungroup()")
    eval(parse(text = a)); print(a)
    
  } else {}
  
  # check dimensions and formulas
  if (i %% 10 == 0) {gc(verbose = T)}
  print(paste(i, dim(extdata_m)[1], object.size(extdata_m)/10^6, dim(extdata_q)[1], object.size(extdata_q)/10^6, 
              dim(extdata_y)[1], object.size(extdata_y)/10^6, sep=" "))
  
}


##### Drop date

for (i in c("y", "q", "m")) { eval(parse(text = glue("extdata_{i} <- extdata_{i} %>% select(-c(date))") )) }


##### Export filled data

#### Export schedule

impdatatodict <- impdata %>% filter(active==1) %>% select(indicator, theme, indicator_code, source_frequency, keep, source_name)
filldatatodict <- filldata %>% filter(active1==1) %>% select(new_indicator, theme, new_indicator_code, new_frequency, keep, formula)
dict <- unique(rbind(impdatatodict, setnames(filldatatodict, names(impdatatodict))))

# Calculates sources for filled data
for (i in 1:dim(filldatatodict)[1]) {
  if (!is.na(filldata$old_indicator_code[i])) {filldatatodict$source_name[i] = filldata$old_indicator_code[i]}
  a <- unlist(str_extract_all( string = filldatatodict$source_name[i], pattern = paste(dict$indicator_code[1:(dim(impdatatodict)[1]+i-1)], collapse = "|") ))
  a <- plyr::mapvalues(a, from = dict$indicator_code, to = dict$source_name, warn_missing = F)
  a <- unlist(strsplit(a, ", "))
  a <- a[a!="расчеты АКРА"]
  a <- c(unique(a), "расчеты АКРА")
  dict$source_name[dim(impdatatodict)[1]+i] = toString(a)
}  

# Collecting lists of what we planned to download
dict_y <- dict %>% filter(source_frequency=="y", keep==1) %>% arrange(theme, indicator_code)
dict_q <- dict %>% filter(source_frequency=="q", keep==1) %>% arrange(theme, indicator_code)
dict_m <- dict %>% filter(source_frequency=="m", keep==1) %>% arrange(theme, indicator_code)
dict <- dict %>% filter(keep==1) %>% select(-keep) %>% arrange(theme, indicator_code)

# Filtering databases to contain only planned output
extdata_y <- extdata_y %>% select(country, country_id, year, any_of(dict_y$indicator_code))
extdata_q <- extdata_q %>% select(country, country_id, year, quarter, any_of(dict_q$indicator_code))
extdata_m <- extdata_m %>% select(country, country_id, year, quarter, month, any_of(dict_m$indicator_code))

# Checking dict if the data was successfully downloaded and filled
downloaded <- data.frame(indicator_code = c(names(extdata_y), names(extdata_q), names(extdata_m)))
downloaded$source_frequency <- c(rep("y", length(names(extdata_y))), rep("q", length(names(extdata_q))), rep("m", length(names(extdata_m))))
downloaded <- downloaded %>% mutate(success = "+")
dict <- dict %>% left_join(downloaded, by = c("indicator_code", "source_frequency"))


#### Export data on all countries to the yearly/quarterly/monthly database

data_export <- list(extdata_y, extdata_q, extdata_m, dict)
names(data_export) <- c("y", "q", "m", "dict")
write_xlsx(data_export, path = "Filled_DB.xlsx", col_names = TRUE, format_headers = TRUE)

data_export <- list(extdata_d, dict_d)
names(data_export) <- c("d", "dict_d")
write_xlsx(data_export, path = "Filled_DB_d.xlsx", col_names = TRUE, format_headers = TRUE)

#### Export data on certain countries in model format

setwd('..')
for (countryname_export in c("Armenia", "Brazil", "Bulgaria", "China", "India", "Kyrgyz Republic", "Russian Federation", "Slovak Republic", "Ukraine")) {
  
  ### Export all data on a country to the yearly database (add q and m?)
  
  #countryname_export = "Russian Federation"
  
  extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) -> t_data_export
  extdata_y %>% filter(country==countryname_export) %>% select("year") -> years
  
  data_export <- data.frame(t(t_data_export))
  names(data_export) <- unlist(years)
  data_export <- cbind("indicator_code" = names(t_data_export), data_export)
  data_export <- data_export %>% left_join(dict_y, "indicator_code"="indicator_code") %>% 
    select(indicator, indicator_code, theme, source_name, everything()) %>% select(-c(keep, source_frequency))
  
  data_export <- list(data_export)
  names(data_export) <- c("y")
  write_xlsx(data_export, path = paste(countryname_export, "/Data/", countryname_export, "_data_filled.xlsx", sep=""), 
             col_names = T, format_headers = T)
  
  ### Export model data on a country to the yearly database
  
  #countryname_export = "Russian Federation"
  # убрал educ и hci пока что из select-а
  extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) %>%
    select(gdp_pc_usd, gdp_pc_ppp, gdp_growth, gdp_usd, gdp, cpi_av, deflator, rnd, gcfc_gdp, open, gg_debt, gg_rev, gg_debttorev,
           gg_exp_int, gg_bal, ca_usd, imp_gs_usd, intres_usd, intrestoimp, exp_div, neer_av, usdlc_eop, usdlc_av, wgi_va_est, wgi_ps_est,
           wgi1, wgi_cc_est, wgi_rl_est, wgi_rq_est, wgi_ge_est, wgi2, amr_male, amr_female, amr, life_exp)-> t_data_export
  extdata_y %>% filter(country==countryname_export) %>% select("year") -> years
  
  data_export <- data.frame(t(t_data_export))
  names(data_export) <- unlist(years)
  data_export <- cbind("indicator_code" = names(t_data_export), data_export)
  data_export <- data_export %>% left_join(dict_y, "indicator_code"="indicator_code") %>% 
    select(indicator, indicator_code, theme, source_name, everything()) %>% select(-c(keep, source_frequency))
  
  data_export <- list(data_export)
  names(data_export) <- c("y")
  write_xlsx(data_export, path = paste(countryname_export, "/Data/", countryname_export, "_data_model.xlsx", sep=""), 
             col_names = T, format_headers = T)
  
}

#########################################