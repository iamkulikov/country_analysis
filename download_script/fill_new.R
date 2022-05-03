library("dplyr")
library("reshape2")
library("ggplot2")
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

##### Import data

setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/download_data")
#setwd("C:/Projects/R_utilities/download_data")

data_fname <- "Imported_DB.xlsx"
data_d_fname <- "Imported_d_DB.xlsx"

a <- length(read_excel(data_fname, sheet = "y", col_names = T, skip=0, n_max = 0))
extdata_y <- read_excel(data_fname, sheet = "y", col_names = T, skip=0,
                        col_types = c("text", "text", rep("numeric", a-2)))
a <- length(read_excel(data_fname, sheet = "q", col_names = T, skip=0, n_max = 0))
extdata_q <- read_excel(data_fname, sheet = "q", col_names = T, skip=0,
                        col_types = c("text", "text", rep("numeric", a-2)))
a <- length(read_excel(data_fname, sheet = "m", col_names = T, skip=0, n_max = 0))
extdata_m <- read_excel(data_fname, sheet = "m", col_names = T, skip=0,
                        col_types = c("text", "text", rep("numeric", a-2)))
a <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                        col_types = c("text", "text", "date", rep("numeric", a-3)))

##### Import and filling schedule

param_fname <- "0_database_params.xlsx"
impdata <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
filldata <- read_excel(param_fname, sheet = "fill", col_names = T, skip=1)
filldata <- filldata %>% filter(active1==1)


##### Filling cycle

for (i in 1:dim(filldata)[1]) {
  #for (i in 1:2) {
  
  ##### Calculating new variables of the same frequency
  
  if (filldata$old_frequency[i]==filldata$new_frequency[i] & filldata$active1[i] == 1 & str_detect(filldata$formula[i], "roll") == F & filldata$formula[i] != "share") {
    
    eval(parse(text=paste("extdata_", filldata$old_frequency[i]," <- extdata_",filldata$old_frequency[i] ,
                          " %>% group_by(country) %>% mutate(",filldata$new_indicator_code[i]," = ",filldata$formula[i],") %>% ungroup()",
                          sep="") ))
    print(paste("extdata_", filldata$old_frequency[i]," <- extdata_",filldata$old_frequency[i] ,
                " %>% mutate(",filldata$new_indicator_code[i]," = ",filldata$formula[i],") %>% ungroup()",
                sep=""))
    
  } else {}
  
  ##### Calculating new rolling variables of the same frequency 
  
  if (filldata$old_frequency[i]==filldata$new_frequency[i] & filldata$active1[i] == 1 & str_detect(filldata$formula[i], "roll") == T ) {
    
    type <- substr(filldata$formula[i],5,7)
    type <- reduce2(c('vol', 'avg'), c('sd', 'mean'), .init = type, str_replace)
    windowlen <- as.numeric(substr(filldata$formula[i], str_locate(filldata$formula[i], ", ")[1,2]+1, str_locate(filldata$formula[i], '[)]')[1,1]-1 ))
    ind_code <- substr(filldata$formula[i], str_locate(filldata$formula[i], "[(]")[1,2]+1, str_locate(filldata$formula[i], '[,]')[1,1]-1 )
    
    eval(parse(text=paste("extdata_", filldata$old_frequency[i], " <- extdata_", filldata$old_frequency[i],
                          " %>% group_by(country) %>% mutate(",filldata$new_indicator_code[i]," = rollapply(",ind_code,", ",
                          windowlen, ", ", type, ", align='right', fill=NA)) %>% ungroup()", sep="") ))     
    
  } else {}
  
  
  ##### Aggregating the same variables to lower frequency, m or q -> q or y
  
  ## Picking last observation from m i=38
  if ( (filldata$old_frequency[i]=="m" & filldata$new_frequency[i]!="m") &
       filldata$active1[i] == 1 & filldata$formula[i]=="last") {
    
    if (filldata$new_frequency[i]=="y") {a <- "month==12"; b <- ""; c <- ""; j <- 3} else
    {a <- "month %in% c(3,6,9,12)"; b <- "quarter, "; c <- ", 'quarter' = 'quarter'"; j <- 4}
    
    eval(parse(text=paste("aggreg <- extdata_",filldata$old_frequency[i],
                          " %>% filter(", a,") %>% select(country_id, year, ", b, filldata$old_indicator_code[i],")",
                          sep="") ))
    
    names(aggreg)[j] <- filldata$new_indicator_code[i]
    
    eval(parse(text=paste("extdata_", filldata$new_frequency[i], " <- extdata_",filldata$new_frequency[i],
                          " %>% left_join(aggreg, by = c('country_id'='country_id', 'year'='year'", c,"))",
                          sep="") ))
    
  } else {}
  
  ## Picking last observation from q
  if ( (filldata$old_frequency[i]=="q" & filldata$new_frequency[i]=="y") &
       filldata$active1[i] == 1 & filldata$formula[i]=="last") {
    
    eval(parse(text=paste("aggreg <- extdata_q %>% filter(quarter==4) %>% select(country_id, year, ",
                          filldata$old_indicator_code[i],")", sep="") ))
    
    names(aggreg)[3] <- filldata$new_indicator_code[i]
    
    extdata_y <- extdata_y %>% left_join(aggreg, by = c('country_id'='country_id', 'year'='year'))
    
  } else {}
  
  ## Calculating means from m   
  if ( (filldata$old_frequency[i]=="m" & filldata$new_frequency[i]!="m") &
       filldata$active1[i] == 1 & filldata$formula[i] %in% c("mean","sum") ) {
    
    if (filldata$new_frequency[i]=="y") {b <- ""; c <- ""; j <- 3; k <-12} else
    {b <- ", quarter"; c <- ", 'quarter' = 'quarter'"; j <- 4; k <- 3}
    
    eval(parse(text=paste("aggreg <- extdata_",filldata$old_frequency[i],
                          " %>% select(country_id, year, quarter, ",filldata$old_indicator_code[i],
                          ") %>% group_by(country_id, year", b,") %>% summarize(mean = mean(",
                          filldata$old_indicator_code[i],")) %>% ungroup()", 
                          sep="") ))
    
    names(aggreg)[j] <- filldata$new_indicator_code[i]
    if (filldata$formula[i] == "sum") {aggreg[j] <- aggreg[j]*k} else {}
    
    eval(parse(text=paste("extdata_", filldata$new_frequency[i], " <- extdata_",filldata$new_frequency[i],
                          " %>% left_join(aggreg, by = c('country_id'='country_id', 'year'='year'", c,"))",
                          sep="") ))
    
  } else {}
  
  ## Calculating means from q  
  if ( (filldata$old_frequency[i]=="q" & filldata$new_frequency[i]=="y") &
       filldata$active1[i] == 1 & filldata$formula[i] %in% c("mean","sum") ) {
    
    eval(parse(text=paste("aggreg <- extdata_q %>% select(country_id, year, quarter, ",filldata$old_indicator_code[i],
                          ") %>% group_by(country_id, year) %>% summarize(mean = mean(",filldata$old_indicator_code[i],")) %>% ungroup()", 
                          sep="") ))
    
    names(aggreg)[3] <- filldata$new_indicator_code[i]
    if (filldata$formula[i] == "sum") {aggreg[j] <- aggreg[j]*4} else {}
    
    extdata_y <- extdata_y %>% left_join(aggreg, by = c('country_id'='country_id', 'year'='year'))
    
  } else {}
  
  
  ##### Calculating shares 
  #i=68
  if (filldata$old_frequency[i]==filldata$new_frequency[i] & filldata$active1[i] == 1 & filldata$formula[i] =="share" ) {
    
    if (filldata$new_frequency[i]=="y") {
      eval(parse(text = paste("extdata_y <- extdata_y %>% group_by(year) %>% mutate(", filldata$new_indicator_code[i],
                              " = ", filldata$old_indicator_code[i], "*100/sum(", filldata$old_indicator_code[i],
                              ", na.rm=T)) %>% ungroup()", sep="") ))
    }
    
  } else {}
  
  if (i %% 10 == 0) {gc(verbose = T)}
  print(paste(i, dim(extdata_m)[1], object.size(extdata_m)/10^6, dim(extdata_q)[1], object.size(extdata_q)/10^6, 
              dim(extdata_y)[1], object.size(extdata_y)/10^6, sep=" "))
  
}

#str(extdata_y)
#str(extdata_q)


###### Export filled data

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


#### Export data on certain countries in model format

setwd('..')
for (countryname_export in c("Armenia", "Brazil", "Bulgaria", "China", "India", "Kyrgyz Republic", "Russian Federation", "Slovak Republic", "Ukraine")) {
  
  ###### Export all data on a country to the yearly database (add q and m?)
  
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
  
  ###### Export model data on a country to the yearly database
  
  #countryname_export = "Russian Federation"
  
  extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) %>%
    select(gdp_pc_usd, gdp_pc_ppp, gdp_growth, gdp_usd, gdp, cpi_av, deflator, rnd, gcfc_gdp, open, gg_debt, gg_rev, gg_debttorev,
           gg_exp_int, gg_bal, ca_usd, imp_gs_usd, intres_usd, intrestoimp, exp_div, neer_av, usdlc_eop, usdlc_av, wgi_va_est, wgi_ps_est,
           wgi1, wgi_cc_est, wgi_rl_est, wgi_rq_est, wgi_ge_est, wgi2, educ, amr_male, amr_female, amr, life_exp, hci)-> t_data_export
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