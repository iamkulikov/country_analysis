#   All the functions to make calculations on imported data

##### Load libraries
library_names <- c("dplyr","reshape2","countrycode","readxl","tidyr","data.table","writexl","stringr","purrr",
                   "gsubfn","tidyquant","timetk","glue","lubridate","here")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

here::i_am("_country_analysis_scripts/download_script/fill.R")

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

checkNames <- function(fillplan, formula_words) {
    
    # add checks for commands
    fillplan <- fillplan %>% mutate(check_names = 1)
    for (i in formula_words) {
      if ('new_indicator_code' %in% names(fillplan)) {
      eval(parse(text = glue("fillplan <- fillplan %>% mutate(check_names = check_names * str_detect(new_indicator_code, '{i}', negate = TRUE))") )) } else{
      eval(parse(text = glue("fillplan <- fillplan %>% mutate(check_names = check_names * str_detect(indicator_code, '{i}', negate = TRUE))") )) 
      }
    }
    
    # add checks for special symbols
    return(fillplan)
    
}

##### Function to check uniqueness of variable definitions

checkUnique <- function(fillplan) { 
  
  if ('new_indicator_code' %in% names(fillplan)) {
    counted <- fillplan %>% count(new_indicator_code, new_frequency) %>% 
                select(new_indicator_code, new_frequency, n) %>% rename("check_unique" = "n") %>%
                mutate(check_unique = (check_unique == 1)*1)
    fillplan <- fillplan %>% left_join(counted, by = c("new_indicator_code" = "new_indicator_code", "new_frequency" = "new_frequency"))
  } else{
    counted <- fillplan %>% count(indicator_code, source_frequency) %>% 
                select(indicator_code, source_frequency, n) %>% rename("check_unique" = "n") %>%
                mutate(check_unique = (check_unique == 1)*1)
    fillplan <- fillplan %>% left_join(counted, by = c("indicator_code" = "indicator_code", "source_frequency" = "source_frequency"))
  }
  
  return(fillplan)
  
}

##### Function to check if the calculation is possible in terms of data availability

checkAvailability <- function(fillplan, impplan) { 
  
  fillplan <- fillplan %>% mutate(check_availability = 0)
  for (i in c('d','q','m','y')) {
    eval(parse(text = glue("available_{i} <- impplan %>% filter(source_frequency == '{i}') %>% pull(indicator_code)") ))
  }
  for (i in seq_along(fillplan$new_indicator)) {
    
    oldfreq <- fillplan$old_frequency[i]
    newfreq <- fillplan$new_frequency[i]
    eval(parse(text = glue("available_now <- available_{oldfreq}") ))
    needed <- unique(strsplit(fillplan$formula[i],"\\/|\\(|\\)|\\+|\\-|\\*|\\=|\\^|\\,|\\s+|>|<|==|!=")[[1]])
    to_drop <- na.omit(c(formula_words, as.numeric(needed), "", needed[nchar(needed) <= 2]))
    needed <- needed[!(needed %in% to_drop)]
    if (all(needed %in% available_now)) {fillplan$check_availability[i] <- 1} else {
        print(needed[!(needed %in% available_now)])
      }
    eval(parse(text = glue("available_{newfreq} <- c(available_{newfreq}, fillplan$new_indicator_code[i])") ))
    
  }
  
  return(fillplan)
  
}


##### Function to save length of data containers (time x countries)

captureDimensions <- function(extdata_y, extdata_q, extdata_m, extdata_d) {
  
  c(dim(extdata_y)[1], dim(extdata_q)[1], dim(extdata_m)[1], dim(extdata_d)[1])
  
}


##### Function for the filling cycle

fill <- function(fillplan, extdata_y, extdata_q, extdata_m, extdata_d) {

    for (i in 1:dim(fillplan)[1]) {
      
      #i = 435
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
      
      if (oldfreq == newfreq & active == 1 & str_detect(formula, "roll") == F & str_detect(formula, "fromto") == F & formula != "share") {
        
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
      
      if (oldfreq == newfreq & active == 1 & fillplan$formula[i] =="share" ) {
        
        a <- glue("extdata_{oldfreq} <- extdata_{oldfreq} %>% group_by({oldfreq_long}) %>% \\
                mutate({newcode} = {oldcode}*100/sum({oldcode}, na.rm=T)) %>% ungroup()")
        eval(parse(text = a)); print(a)
        
      } else {};
      
      
      #### Filling indicator values for selected countries with values from a particular country
      
      if (oldfreq == newfreq & active == 1 & str_detect(formula, "fromto") == T ) {
        
        country_from <- substr(formula, str_locate(formula, '[(]')[1,1]+1, str_locate(formula, ', ')[1,1]-1 )
        if (is.na(country_from)) {
          country_from <- substr(formula, str_locate(formula, '[(]')[1,1]+1, str_locate(formula, '[)]')[1,1]-1 )
        }
        countries_to <- substr(formula, str_locate(formula, "c[(]")[1,1]+2, str_locate(formula, '[)]')[1,1]-1 ) %>% 
            str_split(", ") %>% '[['(1)
        if (all(is.na(countries_to))) {countries_to <- extdata_y %>% pull(country_id) %>% unique()}
 
        #FD <- createDateColumns(extdata_y = FD$extdata_y, extdata_q = FD$extdata_q, extdata_m = FD$extdata_m, extdata_d = FD$extdata_d)
               
        a <- glue("from <- extdata_{oldfreq} %>% select(date, country_id, {oldcode}) %>% \\
                    filter(country_id == '{country_from}') %>% select(-c(country_id)) ") 
        eval(parse(text = a)); print(a)
        from <- eval(parse(text = glue("rename(from,'{newcode}'='{oldcode}')") ))
        from <- from %>% slice(rep(1:n(), each = length(countries_to))) %>% mutate(country_id = NA)
        from$country_id <- rep(countries_to, dim(from)[1]/length(countries_to))
        
        a <- glue("extdata_{newfreq} <- extdata_{newfreq} %>% left_join(from, join_by('date', 'country_id'))")
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


##### Function to export data on certain countries

writeCountryFile <- function(countries, datalist) {

      print("Writing country files")
      datalist$dict <- rbind(datalist$dict, datalist$dict_d) %>% select(-c(keep, success, n_countries, n_points))
      datalist$dict_d <- NULL
  
      for (countryname_export in countries) {
        
        datalist$extdata_d <- datalist$extdata_d %>% mutate(year = year(date)) %>% select(year, date, everything())
        datalist_country <- datalist
        
        for (i in c("y", "q", "m", "d")) {
          eval(parse(text = glue("datalist_country$extdata_{i} <- datalist_country$extdata_{i} %>% 
                                filter(country == '{countryname_export}') %>%
                                select(-c(country, country_id))") ))
        }
        
        for (i in seq_along(datalist_country$dict$indicator)) {
            a <- eval(parse(text = glue("datalist_country$extdata_{datalist$dict$source_frequency[i]} %>% 
                                          select(year, {datalist_country$dict$indicator_code[i]}) %>% 
                                          filter(!is.na({datalist_country$dict$indicator_code[i]}))") ))
            datalist_country$dict$start_year[i] <- a %>% pull(year) %>% unique() %>% min(na.rm = TRUE)
            datalist_country$dict$end_year[i] <- a %>% pull(year) %>% unique() %>% max(na.rm = TRUE)
        }
        
        datalist_country$dict <- datalist_country$dict %>% filter(is.finite(start_year), is.finite(end_year))
        
        for (i in c("y", "q", "m", "d")) {
          eval(parse(text = glue("year_min_{i} <- datalist_country$dict %>% filter(source_frequency == '{i}') %>%
                                 select(start_year) %>% unique() %>% min(na.rm = TRUE) ") ))
          eval(parse(text = glue("year_max_{i} <- datalist_country$dict %>% filter(source_frequency == '{i}') %>%
                                 select(end_year) %>% unique() %>% max(na.rm = TRUE) ") ))
          eval(parse(text = glue("datalist_country$extdata_{i} <- datalist_country$extdata_{i} %>% discard(~all(is.na(.))) %>%
                                 filter(year >= year_min_{i}, year <= year_max_{i})") ))
        } 
        
        names(datalist_country) <- c("y", "q", "m", "d", "dict")
        write_xlsx(datalist_country, path = here(countryname_export, "Data", glue("{countryname_export}_data_filled.xlsx")), 
                   col_names = T, format_headers = T)
        
      }
}

##### Function to export data on certain countries in model format (only yearly)

writeCountryModelFile <- function(countries, extdata_y, saveplan) {
  
  #setwd('..')
  print("Writing country model files") 
  for (countryname_export in countries) {
    
    # убрал gg_debttorev и gg_inttorev пока что из select-а
    extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) %>%
      select(any_of(c('gdp_pc_usd_wb', 'gdp_pc_ppp_wb', 'gdp_growth', 'gdp_usd', 'gdp', 
                    'gdp_growth_world_weo', 'gdp_growth_dm_weo', 'gdp_growth_em_weo',
                    'cpi_av', 'deflator', 'rnd', 'gcfc', 'gcfc_gdp', 'open',
                    'gg_debt_weo', 'gg_rev_weo', 'gg_debttorev', 'gg_exns_int', 'gg_inttorev', 'gg_debt_conc_usd',
                    'extdebt_conc_gdp', 'gg_bal_weo', 'gg_bal_gdp_weo', 'extdebt_gg_usd', 'extdebt_gg_gdp', 
                    'gg_debt_gdp_weo', 'gg_debt_fc_role_fsdb', 'gg_debt_held_global_usd', 'gg_debt_held_global_role',
                    'gg_debt_maturity', 'dpension2030', 
                    'ca_usd', 'ca_gdp', 'imp_gs_usd', 'ex_gs_usd', 'intres_usd', 'intrestoimp', 'niip_ex_ggcb_usd', 'niip_ex_ggcb_gdp', 
                    'ex_div', 'neer_av', 'usdlc_eop', 'usdlc_av', 'remit_usd_wb', 'remit_gdp_wb', 'extdebt_usd', 'intrestoextdebt',
                    'wgi_va_est', 'wgi_ps_est', 'wgi1', 'wgi_cc_est', 'wgi_rl_est', 'wgi_rq_est', 'wgi_ge_est', 'wgi2',
                    'educ', 'amr_male', 'amr_female', 'amr', 'life_length', 'hci'))) -> t_data_export
     
    extdata_y %>% filter(country==countryname_export) %>% select("year") -> years
    dict_y <- saveplan %>% filter(source_frequency == "y")
    
    data_export <- data.frame(t(t_data_export))
    names(data_export) <- unlist(years)
    data_export <- cbind("indicator_code" = names(t_data_export), data_export)
    data_export <- data_export %>% left_join(dict_y, by=c("indicator_code"="indicator_code")) %>% 
      select(indicator, indicator_code, theme, source_name, everything()) %>% select(-c(keep, source_frequency))
    
    data_export <- list(data_export)
    names(data_export) <- c("y")
    write_xlsx(data_export, path = here(countryname_export, "Data", glue("{countryname_export}_data_model.xlsx")), 
               col_names = T, format_headers = T)
    
  }
  
}