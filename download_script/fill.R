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

##### Function to set filling schedule

readFillParams <- function(param_fname) {
  
  fillplan <- read_excel(param_fname, sheet = "fill", col_names = T, skip=1) %>% filter(active1==1)
  return(fillplan)
  
}

##### Function to generate date columns in data (first day in each period is chosen)

createDateColumns <- function(extdata_y, extdata_q, extdata_m, extdata_d) { 

    extdata_y <- extdata_y %>% mutate(date = make_date(year = year, month = 1, day = 1))
    extdata_q <- extdata_q %>% mutate(date = make_date(year = year, month = 3*(quarter-1)+1, day = 1))
    extdata_m <- extdata_m %>% mutate(date = make_date(year = year, month = month, day = 1))
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
    
}

##### Function to check variable names correctness

checkNames <- function(fillplan) {
    
    # add checks for commands
    fillplan <- fillplan %>% mutate(check_names = 1)
    for (i in c('roll', 'share', 'last', 'max', 'min', 'sum')) {
      eval(parse(text = glue::glue("fillplan <- fillplan %>% mutate(check_names = check_names * str_detect(new_indicator_code, '{i}', negate = TRUE))") ))
    }
    
    # add checks for special symbols
    return(fillplan)
    
}

##### Function to check uniqueness of variable definitions

checkUnique <- function(fillplan) { 
  
  counted <- fillplan %>% count(new_indicator_code, new_frequency) %>% 
                select(new_indicator_code, new_frequency, n) %>% rename("check_unique" = "n") %>%
                mutate(check_unique = (check_unique == 1)*1)
  fillplan <- fillplan %>% left_join(counted, by = c("new_indicator_code" = "new_indicator_code", "new_frequency" = "new_frequency"))
  return(fillplan)
  
}

##### Function to check if the calculaion is possible in terms of data availability

checkAvailability <- function(fillplan, impplan) { 
  
  fillplan <- fillplan %>% mutate(check_availability = 1)
  return(fillplan)
  
}

##### Function to save length of data containers (time x countries)

captureDimensions <- function(extdata_y, extdata_q, extdata_m, extdata_d) {
  
  c(dim(extdata_y)[1], dim(extdata_q)[1], dim(extdata_m)[1], dim(extdata_d)[1])
  
}


##### Function for the filling cycle

fill <- function(fillplan, extdata_y, extdata_q, extdata_m, extdata_d) {

    for (i in 1:dim(fillplan)[1]) {
      
      #i = 4
      oldfreq <- fillplan$old_frequency[i]
      oldfreq_long <- case_when(oldfreq == "y" ~ "year", oldfreq == "q" ~ "quarter", oldfreq == "m" ~ "month", oldfreq == "d" ~ "date")
      newfreq <- fillplan$new_frequency[i]
      newfreq_long <- case_when(newfreq == "y" ~ "year", newfreq == "q" ~ "quarter", newfreq == "m" ~ "month", newfreq == "d" ~ "date")
      active <- fillplan$active1[i]
      oldcode <- fillplan$old_indicator_code[i]
      newcode <- fillplan$new_indicator_code[i]
      formula <- fillplan$formula[i]
      
      try({
      
      #### Calculating new variables of the same frequency
      
      if (oldfreq == newfreq & active == 1 & str_detect(formula, "roll") == F & formula != "share") {
        
        a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by(country) %>% mutate({newcode} = {formula}) %>% ungroup()")
        eval(parse(text = a)); print(a)
        
      } else {};
      
      
      #### Calculating new rolling variables of the same frequency 
      
      if (oldfreq == newfreq & active == 1 & str_detect(formula, "roll") == T ) {
        
        # parse formula
        type <- reduce2(c('vol', 'avg'), c('sd', 'mean'), .init = substr(formula,5,7), str_replace)
        windowlen <- as.numeric( substr(formula, str_locate(formula, ", ")[1,2]+1, str_locate(formula, '[)]')[1,1]-1 ) )
        oldcode <- substr(formula, str_locate(formula, "[(]")[1,2]+1, str_locate(formula, '[,]')[1,1]-1 )
        
        a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by(country) %>% \\
                        mutate({newcode} = rollapply({oldcode}, windowlen, {type}, align='right', fill=NA)) %>% ungroup()")
        eval(parse(text = a)); print(a)
        
      } else {};
      
      
      #### Aggregating the same variables to lower frequency d > m > q > y (d<m but frequency is vice versa)
      
      if ( oldfreq < newfreq & active == 1 & (formula=="last"|formula=="first"|formula=="mean"|formula=="max"|formula=="min"|formula=="sum") ) {
        
        a <- glue("aggreg <- extdata_{oldfreq} %>% select(date, country_id, {oldcode}) %>% group_by(country_id) %>% \\
          filter(!is.na({oldcode})) %>% \\
          summarise_by_time(.by = '{newfreq_long}', .date_var = date, {newcode} = {formula}({oldcode}, na.rm = TRUE), .type = 'floor') %>% ungroup()")
        eval(parse(text = a)); print(a)
        
        eval(parse(text=glue("extdata_{newfreq} <- extdata_{newfreq} %>% \\
              left_join(aggreg, by = c('country_id'='country_id', 'date'='date'))") ))
        
      } else {};
      
      
      #### Calculating shares 
      #i=68
      if (oldfreq == newfreq & active == 1 & fillplan$formula[i] =="share" ) {
        
        a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by({oldfreq_long}) %>% \\
                mutate({newcode} = {oldcode}*100/sum({oldcode}, na.rm=T)) %>% ungroup()")
        eval(parse(text = a)); print(a)
        
      } else {};
      
      # check dimensions and formulas
      if (i %% 10 == 0) {gc(verbose = T)}
      print(paste(i, dim(extdata_m)[1], object.size(extdata_m)/10^6, dim(extdata_q)[1], object.size(extdata_q)/10^6, 
                  dim(extdata_y)[1], object.size(extdata_y)/10^6, sep=" "))
      
      })
      
    }

    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
  
}

##### Function to drop the date

dropDateColumns <- function(extdata_y, extdata_q, extdata_m, extdata_d) {
  
  for (i in c("y", "q", "m")) { eval(parse(text = glue("extdata_{i} <- extdata_{i} %>% select(-c(date))") )) }
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
  
}


##### Function to create an export schedule

generateSaveplan <- function (impplan, fillplan) {
    
    print("Generating a saving plan")  
    impplantodict <- impplan %>% filter(active==1) %>% select(indicator, theme, indicator_code, source_frequency, keep, source_name)
    fillplantodict <- fillplan %>% filter(active1==1) %>% select(new_indicator, theme, new_indicator_code, new_frequency, keep, formula)
    saveplan <- unique(rbind(impplantodict, setnames(fillplantodict, names(impplantodict))))
    
    # Calculates sources for filled data
    for (i in 1:dim(fillplantodict)[1]) {
      if (!is.na(fillplan$old_indicator_code[i])) {fillplantodict$source_name[i] = fillplan$old_indicator_code[i]}
      a <- unlist(str_extract_all( string = fillplantodict$source_name[i], pattern = paste(saveplan$indicator_code[1:(dim(impplantodict)[1]+i-1)], collapse = "|") ))
      a <- plyr::mapvalues(a, from = saveplan$indicator_code, to = saveplan$source_name, warn_missing = F)
      a <- unlist(strsplit(a, ", "))
      a <- a[a!="расчеты АКРА"]
      a <- c(unique(a), "расчеты АКРА")
      saveplan$source_name[dim(impplantodict)[1]+i] = toString(a)
    }
    
    saveplan <- saveplan %>% filter(keep == 1)
    return(saveplan)
}


##### Function to export data on certain countries (add other frequencies)

writeCountryFile <- function(countries, extdata_y, extdata_q, extdata_m, extdata_d) {

      #setwd('..')
      print("Writing country files")  
      for (countryname_export in countries) {
        
        extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) -> t_data_export
        extdata_y %>% filter(country==countryname_export) %>% select("year") -> years
        
        data_export <- data.frame(t(t_data_export))
        names(data_export) <- unlist(years)
        data_export <- cbind("indicator_code" = names(t_data_export), data_export)
        data_export <- data_export %>% left_join(dict_y, "indicator_code"="indicator_code") %>% 
          select(indicator, indicator_code, theme, source_name, everything()) %>% select(-c(keep, source_frequency))
        
        data_export <- list(data_export)
        names(data_export) <- c("y")
        write_xlsx(data_export, path = paste0(countryname_export, "/Data/", countryname_export, "_data_filled.xlsx"), 
                   col_names = T, format_headers = T)
        
      }
  
}

##### Function to export data on certain countries in model format (only yearly)

writeCountryModelFile <- function(countries, extdata_y, extdata_q, extdata_m, extdata_d) {
  
  #setwd('..')
  print("Writing country model files") 
  for (countryname_export in countries) {
    
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
  
}