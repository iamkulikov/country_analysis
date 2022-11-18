library("dplyr")
library("reshape2")
library("WDI")
#library("wbstats")
library("countrycode")
library("readxl")
library("tidyr")
library("data.table")
library("writexl")
library("stringr")
#library("imfr")
library("gsubfn")
library("jsonlite")
library("Rilostat")
library("glue")
library("httr")
library("jsonlite") 
library("rlist")

###### Import custom tools
source("../_country_analysis_scripts/download_script/imf_tool.R")

##### Function to set import/update schedule

readImportParams <- function (param_fname, update_mode) {
  
  impplan <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
  parameters <- read_excel(param_fname, sheet = "scope", col_names = T, skip=1, n_max=1)
  .GlobalEnv$year_first <- parameters$start
  .GlobalEnv$year_final <- parameters$end
  .GlobalEnv$saveplan <- impplan %>% filter(active == 1)
  if (update_mode == 1) {impplan <- impplan %>% filter(update == 1)}
  .GlobalEnv$impplan <- impplan %>% filter(active == 1)
  
}

###### Function to generate data containers

generateDataContainers <- function(from, to) {
  
  years <- c(from:to)
  months <- c(1:12)
  days <- seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by="days")
  
  countries <- WDI(indicator = "NY.GDP.MKTP.CD", start = from, end = to, extra=F) %>%
    select("country", "iso2c") %>% rename("country_id" = "iso2c")
  countries <- unique(countries[,c('country','country_id')])
  countries <- subset(countries, as.numeric(rownames(countries)) > 1650)
  
  .GlobalEnv$extdata_y <- expand.grid(paste(countries$country,countries$country_id,sep="."),years) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4)) %>%
    rename("year"="Var2") %>% select(country,country_id,year)
  
  .GlobalEnv$extdata_m <- expand.grid(paste(countries$country,countries$country_id,sep="."),years,months) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4), quarter = (Var3-1)%/%3+1 ) %>%
    rename("year"="Var2", "month"="Var3") %>% select(country,country_id,year,quarter,month) %>%
    arrange(country, year, month)
  
  .GlobalEnv$extdata_q <- extdata_m %>% filter(month %in% c(3,6,9,12)) %>% select(country,country_id,year,quarter)
  
  .GlobalEnv$extdata_d <- expand.grid(paste(countries$country,countries$country_id,sep="."),days) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4)) %>%
    rename("date"="Var2") %>% select(country,country_id,date)
  
  #test <- extdata_y %>% filter(country=="Russian Federation", year>2013)
  
}

##### Function to import previously downloaded data
importOldData <- function(data_fname, data_d_fname) {
  
    #data_fname <- "Imported_DB.xlsx"
    #data_d_fname <- "Imported_d_DB.xlsx"
    
    for (i in c("y", "q", "m")) {
      eval(parse(text = glue("ncols <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
      eval(parse(text = glue(".GlobalEnv$extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                            col_types = c('text', 'text', rep('numeric', ncols-2)))") ))
    }
    
    ncols <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
    .GlobalEnv$extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                            col_types = c("text", "text", "date", rep("numeric", ncols-3)))
  }


###### Function to update import schedule after each cycle of import attempts

updateImportPlan <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {
  
  extdata_d <- extdata_d %>% select(-date)
  impplan_frequencies <- impplan %>% select(source_frequency) %>% distinct() %>% unlist()
  empty_codes <- rbind(c('code', 'a'), c('code', 'a'))
  
  # include all empty columns from extdata
  for (i in impplan_frequencies) {
      eval(parse(text = glue(" extdata_{i}_temp <- extdata_{i} %>% mutate(code = '') %>% select_if(~(all(is.na(.)) | all(. == ''))) ") ))
      eval(parse(text = glue( "empty_codes <- rbind(empty_codes, cbind(names(extdata_{i}_temp), '{i}')) ") ))
  }
  
  empty_codes <- as_tibble(empty_codes)
  names(empty_codes) <- c("indicator_code","source_frequency")
  empty_output <- impplan %>% inner_join(empty_codes, by=c("indicator_code", "source_frequency")) 
  
  # include all non-existent columns
  existing_codes <- rbind(cbind(names(extdata_d), "d"), cbind(names(extdata_m), "m"), cbind(names(extdata_q), "q"), cbind(names(extdata_y), "y"))
  existing_codes <- as_tibble(existing_codes)
  names(existing_codes) <- c("indicator_code","source_frequency")
  nonexistent_output <- impplan %>% anti_join(existing_codes, by=c("indicator_code", "source_frequency"))
  
  output <- distinct(rbind(empty_output, nonexistent_output))
  print(output)
  return(output)

}

##### Function to drop data, which needs to be updated

dropDataToUpdate <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {

    for (i in seq_along(impplan$indicator)) { 
      
      eval(parse( text = paste0("if ('", impplan$indicator_code[i], "' %in% names(extdata_", impplan$source_frequency[i],")) { .GlobalEnv$extdata_",
                                impplan$source_frequency[i], " <- extdata_",
                                impplan$source_frequency[i], " %>% select(-c(", impplan$indicator_code[i],")) }") ))
    }
  
}


###### Main import function for APIs and local files 

tryImport <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {
      
    ##### Import WDI
    
    wdi_impplan <- impplan %>% filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="y")
    wdi_names <- wdi_impplan$indicator_code
    wdi_codes <- wdi_impplan$retrieve_code
    
    if (length(wdi_codes)>0) {
    
      print("WDI")
      wdi_data <- WDI(indicator = wdi_codes, start = year_first, end = year_final, extra=F) %>% select(-c(country)) %>%
        rename_at(vars(any_of(wdi_codes)), ~wdi_names)
      
      extdata_y <- extdata_y %>% left_join(wdi_data, by = c("country_id"="iso2c", "year"="year"), suffix=c("","_old"))
    
      }
    
    ##### Import WGI
    
    wgi_impplan <- impplan %>% filter(active==1, database_name=="WGI", retrieve_type=="file", source_frequency=="y")
    wgi_names <- wgi_impplan$indicator_code
    wgi_fname <- unique(wgi_impplan$file_name)
    wgi_sheets <- wgi_impplan$sheet_name
    wgi_type <- wgi_impplan$retrieve_code
    wgi_data <- NULL
    
    if (length(wgi_names)>0) {
        
      print("WGI")
      for (i in seq_along(wgi_names)) {
          
          wgi_data <- read_excel(wgi_fname, sheet = wgi_sheets[i], col_names = F, na = "#N/A", skip=15)
          wgi_header <- read_excel(wgi_fname, sheet = wgi_sheets[i], col_names = F, na = "#N/A", skip=13, n_max=2)
          wgi_data <- wgi_data[ , wgi_header[2,] %in% c("Country/Territory","Code",wgi_type[i])]
          names(wgi_data) <- c("country","country_id", wgi_header[1, as.vector(wgi_header[2,] == wgi_type[i]) ])
          
          wgi_data %>% select(starts_with(c("country","1","2"))) %>%
            mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                            custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                            'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))) %>%
            select(-c("country")) -> wgi_data
          
          reshape2::melt(wgi_data, id.vars = "country_id", variable.name = "year", value.name = wgi_names[i]) %>% 
            mutate(year =  as.numeric(as.character(year))) -> wgi_data
          
          extdata_y %>% left_join(wgi_data, by = c("country_id" = "country_id", "year"="year"), 
                                  suffix = c("", "_wgi")) -> extdata_y
          
        }
    
    }
    
    ##### Import UN education index
    
    un_impplan <- impplan %>% filter(active==1, database_name=="HDR", retrieve_type=="API", source_frequency=="y")
    un_names <- un_impplan$indicator_code
    un_code <- paste("http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=", un_impplan$retrieve_code, sep="")
    
    #if (length(un_names)>0) {
    
    #print("UN")
    #un_dat <- try(flatten(data.frame(jsonlite::fromJSON(un_code)$indicator_value)))
    #un_data <- unlist(data.frame(t(un_dat)))  !!! здесь ругается
    #un_descr <- names(un_dat)
    ##un_data <- as.numeric(un_data)
    #un_data <- data.frame(cbind(un_descr, un_data)) %>% mutate(un_data = as.numeric(un_data))
    
    #un_data <- un_data %>% mutate(country_id = str_sub(un_descr, 1,3), year = as.numeric(str_sub(un_descr, -4,-1)),
    #        country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c')) %>%
    #  select(-c(un_descr)) %>% select(country_id, year, un_data)
    #names(un_data)[3] <- un_names
    
    #extdata_y %>% left_join(un_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old")) -> extdata_y
    
    #}
    
    ##### Import UNCTAD export diversification index
    
    unctad_impplan <- impplan %>% filter(active==1, source_name=="UNCTAD", retrieve_type=="file", source_frequency=="y")
    unctad_names <- unctad_impplan$indicator_code
    unctad_fname <- unctad_impplan$file_name
    
    if (length(unctad_names)>0) {
      
      print("UNCTAD")
      unctad_data <- read.csv(unctad_fname, header = TRUE, sep = ",", quote = "\"",
                         dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"),
                         colClasses=c("Economy"="numeric"), skip=0)
      
      unctad_data %>% filter (Flow.Label=="Exports") %>%
        select(Year, Economy, Economy.Label, Diversification.Index) %>%
        mutate(country_id = countrycode(Economy, origin = 'un', destination = 'iso2c')) %>%
        select(-c(Economy.Label, Economy)) %>% rename("year"="Year") -> unctad_data
      
      unctad_data <- eval(parse(text = glue("rename(unctad_data,'{unctad_names}'='Diversification.Index')") ))
      
      extdata_y %>% left_join(unctad_data, by = c("country_id" = "country_id", "year"="year"),
                              suffix=c("","_old")) -> extdata_y
    
    }
    
    ##### Import yearly IMF data
    
    imfy_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                       source_frequency=="y")
    imfy_dbnames <- imfy_impplan$database_name
    imfy_names <- imfy_impplan$indicator_code
    imfy_codes <- imfy_impplan$retrieve_code
    
    if (length(imfy_codes)>0) {
      
      print("IMF-Y")
      imf_data_in <- NULL
      for (i in seq_along(imfy_codes)) {
        
        print(imfy_codes[i])
        new_data <- imfTool(database = imfy_dbnames[i], code = imfy_codes[i], freq = 'A',
                             start = year_first , end = year_final)
        
        if (i==1) {imf_data_in <- new_data} else {
          imf_data_in <- imf_data_in %>% full_join(new_data, by=c('iso2c'='iso2c', 'time'='time'), suffix=c("","_old"))}
      
        }
      
      imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfy_codes)), ~imfy_names)  %>% 
        mutate(year = as.numeric(substr(time, 1, 4))) %>% select(-c("time"))
          
      extdata_y %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year"),
                              suffix=c("","_old")) -> extdata_y
     
    }
    
    ##### Import quarterly IMF data
    
    imfq_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                       source_frequency=="q")
    imfq_dbnames <- imfq_impplan$database_name
    imfq_names <- imfq_impplan$indicator_code
    imfq_codes <- imfq_impplan$retrieve_code
    
    if (length(imfq_codes)>0) {
    
      print("IMF-Q")
      imf_data_in <- NULL
      for (i in seq_along(imfq_codes)) {
        
        print(imfq_codes[i])
        new_data <- imfTool(database = imfq_dbnames[i], code = imfq_codes[i], freq = 'Q',
                             start = ifelse(imfq_dbnames[i]=='BOP', 2011, year_first), end = year_final)
        
        if (i==1) {imf_data_in <- new_data} else {imf_data_in <- imf_data_in %>% 
                        full_join(new_data, by=c('iso2c'='iso2c', 'time'='time'), suffix=c("","_old"))}
      }
      
      imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfq_codes)), ~imfq_names) %>% 
        mutate(year=as.numeric(substr(time,1,4)), quarter=as.numeric(substr(time,7,7))) %>% select(-c(time)) 
      
      extdata_q %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year", "quarter"="quarter"),
                              suffix=c("","_old")) -> extdata_q
    
    }  
    
    ##### Import monthly IMF data
    
    imfm_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                       source_frequency=="m")
    imfm_dbnames <- imfm_impplan$database_name
    imfm_names <- imfm_impplan$indicator_code
    imfm_codes <- imfm_impplan$retrieve_code
    
    if (length(imfm_codes)>0) {
      
      print("IMF-M")
      imf_data_in <- NULL
      for (i in seq_along(imfm_codes)) {
        
        print(imfm_codes[i])
        new_data <- imfTool(database = imfm_dbnames[i], code = imfm_codes[i], freq = 'M',
                        start = ifelse(imfm_dbnames[i]=='BOP', 2011, year_first) , end = year_final)
        
        if (i==1) {imf_data_in <- new_data} else {imf_data_in <- imf_data_in %>% 
                  full_join(new_data, by=c('iso2c'='iso2c', 'time'='time'), suffix=c("","_old"))}
      }
      
      imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfm_codes)), ~imfm_names) %>% 
        mutate(year=as.numeric(substr(time,1,4)), month=as.numeric(substr(time,6,7))) %>% select(-c(time)) 
      
      extdata_m %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year", "month"="month"),
                              suffix=c("","_old")) -> extdata_m
     
    }
    
    
    ##### Import IDS external debt statistics
    
    ids_impplan <- impplan %>% filter(active==1, database_name=="IDS", retrieve_type=="file", source_frequency=="y")
    ids_names <- ids_impplan$indicator_code
    ids_fname <- unique(ids_impplan$file_name)
    ids_sheets <- unique(ids_impplan$sheet_name)
    ids_codes <- ids_impplan$retrieve_code
    
    if (length(ids_codes)>0) {
      
      print("IDS")
      ids_data <- read_excel(ids_fname, sheet = ids_sheets, col_names = T, na = "#N/A")
      ids_data <- ids_data %>% rename('country'='Country Name', 'country_id'='Country Code', 'variable_code'='Series Code', 
                            'variable_name'='Series Name', 'counterpart_name'='Counterpart-Area Name', 'counterpart_code'='Counterpart-Area Code') %>% 
                            filter(counterpart_name=="World") %>% select(-c('counterpart_code', 'counterpart_name', 'country', 'variable_name')) %>%
                            filter(variable_code %in% ids_codes)
      
      ids_data <- reshape2::melt(ids_data, id.vars = c("country_id", "variable_code"), variable.name = "year", value.name = "value") %>% 
          mutate(year = as.numeric(as.character(year)))
      
      ids_data <- ids_data %>% left_join(ids_impplan %>% select(indicator_code, retrieve_code), by=c('variable_code'='retrieve_code'), suffix=c("","_old")) %>%
                      select(-c(variable_code))
      
      ids_data <- ids_data %>% pivot_wider(names_from = indicator_code, values_from = value) %>%
        mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                         'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')))
        
      extdata_y <- extdata_y %>% left_join(ids_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      
    }
    
    ##### Import daily BIS data on policy rates
    
    bis_impplan <- impplan %>% filter(active==1, source_name=="BIS", retrieve_type=="file", database_name=="Policy rates (daily, vertical time axis)")
    bis_names <- bis_impplan$indicator_code
    bis_fname <- bis_impplan$file_name
    #bis_names <- "policy_rate_eop"
    #bis_fname <- "./_extsources/WS_CBPOL_D_csv_row.csv"
    EZ_countries <- read_excel("1_peers_params.xlsx", sheet = "groups", col_names = F, skip=2, n_max=7)[c(2,7),-c(1:3)]
    EZ_countries <- data.frame(t(EZ_countries)) %>% filter(X1 == 1) %>% select(X2) %>% unlist()
    EZ_countries <- countrycode(EZ_countries, origin = 'iso3c', destination = 'iso2c', 
                                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                 'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))
    
    if (length(bis_names)>0) {
      
      print("BIS-pol-d")
      bis_data <- read.csv(bis_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=8)

      
      for (i in EZ_countries) {eval(parse(text = glue("bis_data <- bis_data %>% mutate(D.{i} = D.XM)")))}
      
      bis_data <- bis_data %>% pivot_longer(!Time.Period, names_to = "country_id", values_to = "value")
      bis_data <- bis_data %>% mutate(date = as.Date(Time.Period), country_id = substr(country_id,3,4), value=as.numeric(value)) %>%
        select(country_id, date, value)
      
      bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names}'='value')") ))
      
      extdata_d %>% left_join(bis_data, by = c("country_id" = "country_id", "date"="date"),
                              suffix=c("","_old")) -> extdata_d
    }  
    
    ##### Import daily BIS data on exchange rates
    
    bis_impplan <- impplan %>% filter(active==1, source_name=="BIS", retrieve_type=="file", database_name=="US dollar exchange rates (daily, vertical time axis)")
    bis_names <- bis_impplan$indicator_code
    bis_fname <- bis_impplan$file_name
    #bis_names <- "usdlc_eop"
    #bis_fname <- "./_extsources/WS_XRU_D_csv_row.csv"

    
    if (length(bis_names)>0) {
      
      print("BIS-FX-d")
      bis_data <- read.csv(bis_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=8)

      bis_data <- bis_data %>% pivot_longer(!Time.Period, names_to = "country_id", values_to = "value")
      bis_data <- bis_data %>% mutate(date = as.Date(Time.Period), country_id = substr(country_id,3,4), value=as.numeric(value)) %>%
        select(country_id, date, value)
      
      bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names}'='value')") ))
      
      extdata_d %>% left_join(bis_data, by = c("country_id" = "country_id", "date"="date"),
                              suffix=c("","_old")) -> extdata_d
    }   
    
    ##### Import monthly BIS data on policy rates
    
    bis_impplan <- impplan %>% filter(active==1, source_name=="BIS", retrieve_type=="file", database_name=="Policy rates (monthly)")
    bis_names <- bis_impplan$indicator_code
    bis_fname <- bis_impplan$file_name
    
    if (length(bis_names)>0) {
      
      print("BIS-pol-m")
      bis_data <- read.csv(bis_fname, header = TRUE, sep = ",", quote = "\"",
                              dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0)
  
      bis_data <- bis_data %>% mutate(country_id = REF_AREA) %>% select(-c("REF_AREA"))
      bis_data <- bis_data[,-c(1:13)]
      bis_data <- bis_data %>% pivot_longer(!country_id, names_to = "time", values_to = "value")
      bis_data <- bis_data %>% mutate(year = as.numeric(substr(time,2,5)), month = as.numeric(substr(time,7,8)), value=as.numeric(value)) %>%
        select(country_id, year, month, value)
      
      bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names}'='value')") ))
      
      extdata_m %>% left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"),
                              suffix=c("","_old")) -> extdata_m
    }  
    
    ##### Import IMF Fiscal monitor structural indicators
    
    fm_impplan <- impplan %>% filter(active==1, database_name=="FM", retrieve_type=="file", source_frequency=="y")
    fm_names <- fm_impplan$indicator_code
    fm_fname <- fm_impplan$file_name
    fm_sheets <- fm_impplan$sheet_name
    
    if (length(fm_names)>0) {
      
      print("IMF-FM")
      for (i in seq_along(fm_names)) {
      
        #i=3
        fm_data <- read_excel(fm_fname[i], sheet = fm_sheets[i], col_names = T, na = "#N/A", col_types='text')
        
        fm_data <- fm_data %>% pivot_longer(cols = !contains('country'), names_to = 'year', values_to = ) %>%
          mutate(country_id = countrycode(country_code, origin = 'iso3c', destination = 'iso2c', 
              custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')),
              year = as.numeric(as.character(year)), value = as.numeric(value) ) %>% select(country_id, year, value)
        
        fm_data <- eval(parse(text = glue("rename(fm_data,'{fm_names[i]}'='value')") ))
          
        extdata_y <- extdata_y %>% left_join(fm_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      
        }
    
    }
    
    ##### Import ILOstat data
    
    # search
    # toc <- get_ilostat_toc(search = 'informal')
    # toc2 <- toc %>% select(-c(indicator, freq.label, collection.label, subject)) %>% filter(freq == "A")
    # toc2$indicator.label
    
    ilo_impplan <- impplan %>% filter(active==1, source_name=="ILO", retrieve_type=="API")
    ilo_names <- ilo_impplan$indicator_code
    ilo_codes <- ilo_impplan$retrieve_code
    ilo_freq <- ilo_impplan$source_frequency
    
    if (length(ilo_codes)>0) {
      
      print("ILOstat")
      for (i in seq_along(ilo_names)) {
        
        #i=1
        ilo_data <- get_ilostat(id = ilo_codes[i], segment = 'indicator')
        
        if (all(str_detect(ilo_codes[i], c("SEX", "AGE")))) {ilo_data <- ilo_data %>% filter(sex == "SEX_T",  classif1 == "AGE_YTHADULT_YGE15")}
        if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("TRUE", "FALSE"))) {ilo_data <- ilo_data %>% filter(sex == "SEX_T")}
        if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("FALSE", "TRUE"))) {ilo_data <- ilo_data %>% filter(classif1 == "AGE_YTHADULT_YGE15")}
        
        ilo_data <- ilo_data %>% 
              mutate(ref_area = countrycode(ref_area, origin = 'iso3c', destination = 'iso2c', 
                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')))
        
        if (ilo_freq[i] == "y") {ilo_data <- ilo_data %>% mutate(year = as.numeric(time)) %>% select(ref_area, year, obs_value)}
        if (ilo_freq[i] == "q") {ilo_data <- ilo_data %>% mutate(year = as.numeric(substr(time, 1, 4)), quarter = as.numeric(substr(time, 6, 6))) %>% select(ref_area, year, quarter, obs_value)}
        if (ilo_freq[i] == "m") {ilo_data <- ilo_data %>% mutate(year = as.numeric(substr(time, 1, 4)), month = as.numeric(substr(time, 6, 7))) %>% select(ref_area, year, month, obs_value)}
        
        ilo_data <- eval(parse(text = glue("rename(ilo_data,'{ilo_names[i]}'='obs_value')") ))
        
        if (ilo_freq[i] == "y") {extdata_y <- extdata_y %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year"), suffix=c("","_old"))}
        if (ilo_freq[i] == "q") {extdata_q <- extdata_q %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "quarter" = "quarter"), suffix=c("","_old"))}
        if (ilo_freq[i] == "m") {extdata_m <- extdata_m %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "month" = "month"), suffix=c("","_old"))}
      
        }
    
    }
    
    ##### Import daily data on COVID
    
    covid_impplan <- impplan %>% filter(active==1, source_name=="Ourworldindata", retrieve_type=="file", database_name== "COVID tracker")
    covid_names <- covid_impplan$indicator_code
    covid_codes <- covid_impplan$retrieve_code
    covid_fname <- covid_impplan$file_name[1]
    
    if (length(covid_codes)>0) {
      
      print("OWID")
      covid_data <- read.csv(covid_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0) %>%
                  mutate(
                    date = as.Date(date),
                    country_id = countrycode(iso_code, origin = 'iso3c', destination = 'iso2c', 
                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))
                  )
      
      eval(parse(text = glue("covid_data <- covid_data %>% select(date, country_id, {paste(covid_codes, collapse=', ')}) %>% \\
                             rename_at(vars(any_of(covid_codes)), ~covid_names) ") ))
      extdata_d <- extdata_d %>% left_join(covid_data, by=c("country_id", "date"), suffix=c("","_old"))
  
    }
        
    ##### Return imported
    .GlobalEnv$extdata_y <- extdata_y
    .GlobalEnv$extdata_q <- extdata_q
    .GlobalEnv$extdata_m <- extdata_m
    .GlobalEnv$extdata_d <- extdata_d
    
}


##### Functions to export data from memory

#### Function to generate dict and keep only planned imports

preExport <- function(saveplan, extdata_y, extdata_q, extdata_m, extdata_d) {

    dict <- saveplan %>% filter(active==1) %>% select(indicator, theme, indicator_code, source_frequency, source_name)
    
    # Collecting lists of what we planned to download
    dict <- dict %>% arrange(theme, indicator_code)
    for (i in c("y", "q", "m", "d")) {
      eval(parse(text = glue("dict_{i} <- dict %>% filter(source_frequency=='{i}')") ))
    }
    print(dict)
    
    # Filtering databases to contain only planned output
    .GlobalEnv$extdata_y <- extdata_y %>% select(country, country_id, year, any_of(dict_y$indicator_code))
    .GlobalEnv$extdata_q <- extdata_q %>% select(country, country_id, year, quarter, any_of(dict_q$indicator_code))
    .GlobalEnv$extdata_m <- extdata_m %>% select(country, country_id, year, quarter, month, any_of(dict_m$indicator_code))
    .GlobalEnv$extdata_d <- extdata_d %>% select(country, country_id, date, any_of(dict_d$indicator_code))
    
    # Checking dict if the data was successfully downloaded
    downloaded <- data.frame(indicator_code = c(names(extdata_y), names(extdata_q), names(extdata_m), names(extdata_d)))
    downloaded$source_frequency <- c(rep("y", length(names(extdata_y))), rep("q", length(names(extdata_q))), rep("m", length(names(extdata_m))), rep("d", length(names(extdata_d))))
    downloaded <- downloaded %>% mutate(success = "+")
    dict <- dict %>% left_join(downloaded, by = c("indicator_code", "source_frequency"))
    .GlobalEnv$dict_d <- dict %>% filter(source_frequency == "d")
    .GlobalEnv$dict <- dict %>% filter(source_frequency != "d")

}

#### Export data on all countries to the yearly/quarterly/monthly database

writeDatafiles <- function(data_fname, data_d_fname, extdata_y, extdata_q, extdata_m, extdata_d, dict, dict_d) {

    data_export <- list(extdata_y, extdata_q, extdata_m, dict)
    names(data_export) <- c("y", "q", "m", "dict")
    write_xlsx(data_export, path = data_fname, col_names = TRUE, format_headers = TRUE)
    
    data_export_d <- list(extdata_d, dict_d)
    names(data_export_d) <- c("d", "dict_d")
    write_xlsx(data_export_d, path = data_d_fname, col_names = TRUE, format_headers = TRUE)

}

###### Export data on a specific country to the yearly database

#countryname_export = "Russian Federation"

#extdata_y %>% filter(country==countryname_export) %>% select(-c("country","country_id","year")) -> t_data_export
#extdata_y %>% filter(country==countryname_export) %>% select("year") -> years

#data_export <- data.frame(t(t_data_export))
#names(data_export) <- unlist(years)
#data_export <- cbind("indicator_code" = names(t_data_export), data_export)
#data_export <- data_export %>% left_join(dict, "indicator_code"="indicator_code") %>% select(indicator, everything())

#data_export <- list(data_export)
#names(data_export) <- c("y")
#write_xlsx(data_export, path = paste(countryname_export, "/", countryname_export, "_data.xlsx", sep=""), 
#           col_names = T, format_headers = T)
