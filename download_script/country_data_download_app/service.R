library("dplyr")
library("readxl")
library("readr")
library("tidyr")
library("writexl")
library("glue")
library("here")
library("lubridate")
library("purrr")

## Function to import data

importData <- function(data_fname, data_d_fname) {
  
  for (i in c("y", "q", "m")) {
    eval(parse(text = glue("ncols <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
    eval(parse(text = glue("extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                            col_types = c('text', 'text', rep('numeric', ncols-2)))") ))
  }
  dict <- read_excel(data_fname, sheet = 'dict', col_names = T, skip=0)
  
  ncols <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
  extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                          col_types = c("text", "text", "date", rep("numeric", ncols-3)))
  dict_d <- read_excel(data_d_fname, sheet = 'dict_d', col_names = T, skip=0)
  dict <- rbind(dict, dict_d)
  
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict))
  
}


## Function to subset data

subsetCountry <- function(countryname_export, datalist) {
    
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
    return(datalist_country)
    #write_xlsx(datalist_country, path = here(countryname_export, "Data", glue("{countryname_export}_data_filled.xlsx")), 
    #           col_names = T, format_headers = T)
  
  
}