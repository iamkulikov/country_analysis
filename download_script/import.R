#   All the functions to import data

##### Load libraries
library_names <- c("dplyr","reshape2","WDI","countrycode","readxl","readr","tidyr","data.table","writexl","stringr",
                   "gsubfn","jsonlite","Rilostat","glue","httr","rlist","here")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

##### Import custom tools
here::i_am("_country_analysis_scripts/download_script/import.R")
source(here("_country_analysis_scripts","download_script","imf_tool.R"))

##### Set parameters
d_container_start <- "2019-01-01"
d_container_end <- "2025-12-31"  # it's easier to expand container manually without using update=0 mode

##### Function to set import/update schedule

readImportParams <- function (param_fname, update_mode) {
  
  impplan <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
  parameters <- read_excel(param_fname, sheet = "scope", col_names = T, skip=1, n_max=1)
  year_first <- parameters$start
  year_final <- parameters$end
  saveplan <- impplan %>% filter(active == 1)
  if (update_mode == 1) {impplan <- impplan %>% filter(update == 1)}
  impplan <- impplan %>% filter(active == 1)
  return(list(year_first = year_first, year_final = year_final, saveplan = saveplan, impplan = impplan))
  
}

##### Function to generate data containers

generateDataContainers <- function(from, to) {
  
  years <- c(from:to)
  months <- c(1:12)
  days <- seq(as.Date(d_container_start), as.Date(d_container_end), by="days")
  
  countries <- WDI(indicator = "NY.GDP.MKTP.CD", start = from, end = to, extra=F) %>%
    select("country", "iso2c") %>% rename("country_id" = "iso2c")
  countries <- unique(countries[,c('country','country_id')])
  countries <- subset(countries, as.numeric(rownames(countries)) > 1650)
  
  extdata_y <- expand.grid(paste(countries$country,countries$country_id,sep="."),years) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4)) %>%
    rename("year"="Var2") %>% select(country,country_id,year)
  
  extdata_m <- expand.grid(paste(countries$country,countries$country_id,sep="."),years,months) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4), quarter = (Var3-1)%/%3+1 ) %>%
    rename("year"="Var2", "month"="Var3") %>% select(country,country_id,year,quarter,month) %>%
    arrange(country, year, month)
  
  extdata_q <- extdata_m %>% filter(month %in% c(3,6,9,12)) %>% select(country,country_id,year,quarter)
  
  extdata_d <- expand.grid(paste(countries$country,countries$country_id,sep="."),days) %>%
    mutate(country_id = str_sub(Var1, - 2, - 1) , country = str_sub(Var1, 1, -4)) %>%
    rename("date"="Var2") %>% select(country,country_id,date)

  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))

}

##### Function to import previously downloaded data

importOldData <- function(data_fname, data_d_fname) {
  
    #data_fname <- "Imported_DB.xlsx"
    #data_d_fname <- "Imported_d_DB.xlsx"
    
    for (i in c("y", "q", "m")) {
      eval(parse(text = glue("ncols <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
      eval(parse(text = glue("extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                            col_types = c('text', 'text', rep('numeric', ncols-2)))") ))
    }
    
    ncols <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
    extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                            col_types = c("text", "text", "date", rep("numeric", ncols-3)))
    
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
    
  }


##### Function to update import schedule after each cycle of import attempts

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
      
      eval(parse( text = paste0("if ('", impplan$indicator_code[i], "' %in% names(extdata_", impplan$source_frequency[i],")) {extdata_",
                                impplan$source_frequency[i], " <- extdata_",
                                impplan$source_frequency[i], " %>% select(-c(", impplan$indicator_code[i],")) }") ))
    
    }
  
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
  
}

##### Function to check if all the necessary files for import exist

checkFileExistence <- function(impplan, extdata_folder) {
  
  impplan_temp <- impplan %>% filter(retrieve_type == "file") %>% 
      mutate(check_exist = 1*file.exists(here(extdata_folder, file_name))) %>%
      select(indicator_code, source_frequency, check_exist)
  
  impplan <- impplan %>% left_join(impplan_temp, by = c("indicator_code" = "indicator_code", "source_frequency" = "source_frequency"))
  return(impplan)
  
}

##### Main import function for APIs and local files 

tryImport <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {
      
    ##### Import WDI yearly
    try({
      
      wdiy_impplan <- impplan %>% filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="y")
      wdiy_names <- wdiy_impplan$indicator_code
      wdiy_codes <- wdiy_impplan$retrieve_code
      
      if (length(wdiy_names)>0 & all(!is.na(wdiy_names))) {
      
        print("WDI-y")
        #### !!!! разобраться с объемом пакета, в следующей строчке - костыль, снижающий его искуственно, 
        #### !!!!  не забывать двигать год !!!!
        for (i in seq_along(wdiy_names)) {
          wdiy_data <- WDI(indicator = wdiy_codes[i], start = max(year_first, 1996), end = min(year_final, 2024), extra=F) %>% select(-c(country)) %>%
            rename_at(vars(any_of(wdiy_codes[i])), ~wdiy_names[i])
          
          extdata_y <- extdata_y %>% left_join(wdiy_data, by = c("country_id"="iso2c", "year"="year"), suffix=c("","_old"))
          print(wdiy_names[i])
        }
        print("+")
        }
    
    })
  
    ##### Import WDI quarterly
    try({
      
      wdiq_impplan <- impplan %>% filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="q")
      wdiq_names <- wdiq_impplan$indicator_code
      wdiq_codes <- wdiq_impplan$retrieve_code
      
      if (length(wdiq_names)>0 & all(!is.na(wdiq_names))) {
        
        print("WDI-q")
        #### !!!! разобраться с объемом пакета, в следующей строчке - костыль, снижающий его искуственно, 
        #### !!!!  не забывать двигать год !!!!
        for (i in seq_along(wdiq_names)) {
          
          wdiq_data <- WDI(indicator = wdiq_codes[i], start = max(year_first, 2007), end = min(year_final, 2024), extra=F) %>% select(-c(country, iso3c)) %>%
            mutate(quarter = as.numeric(substr(year, 6, 6)), year = as.numeric(substr(year, 1, 4))) %>%
            mutate(iso2c = countrycode(iso2c, origin = 'iso3c', destination = 'iso2c', 
                                       custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                        'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) %>%
            rename_at(vars(any_of(wdiq_codes[i])), ~wdiq_names[i])
          
          extdata_q <- extdata_q %>% left_join(wdiq_data, by = c("country_id"="iso2c", "year"="year", "quarter"="quarter"), suffix=c("","_old"))
          print(wdiq_names[i])
        }
        print("+")
      }
      
    })
  
    ##### Import WGI
    try({
      
      wgi_impplan <- impplan %>% filter(active==1, database_name=="WGI", retrieve_type=="file", source_frequency=="y")
      wgi_names <- wgi_impplan$indicator_code
      wgi_fname <- here("_DB", "_extsources", wgi_impplan$file_name[1])
      wgi_sheets <- wgi_impplan$sheet_name
      wgi_type <- wgi_impplan$retrieve_code
      wgi_data <- NULL
      
      if (length(wgi_names)>0 & all(!is.na(wdiy_names))) {
          
        print("WGI")
        for (i in seq_along(wgi_names)) {
            
            suppressMessages({
            wgi_data <- read_excel(wgi_fname, sheet = wgi_sheets[i], col_names = F, na = "#N/A", skip=15)
            wgi_header <- read_excel(wgi_fname, sheet = wgi_sheets[i], col_names = F, na = "#N/A", skip=13, n_max=2)
            })
            wgi_data <- wgi_data[ , wgi_header[2,] %in% c("Country/Territory","Code",wgi_type[i])]
            names(wgi_data) <- c("country","country_id", wgi_header[1, as.vector(wgi_header[2,] == wgi_type[i]) ])
            
            wgi_data %>% select(starts_with(c("country","1","2"))) %>%
              mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                        'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) %>%
              select(-c("country")) -> wgi_data
            
            reshape2::melt(wgi_data, id.vars = "country_id", variable.name = "year", value.name = wgi_names[i]) %>% 
              mutate(year =  as.numeric(as.character(year))) -> wgi_data
            
            extdata_y %>% left_join(wgi_data, by = c("country_id" = "country_id", "year"="year"), 
                                    suffix = c("", "_wgi")) -> extdata_y
            print("+")
            
          }
      
      }
    
    })
    
    ##### Import UNCTAD export diversification index
    try({
      
      unctad_impplan <- impplan %>% filter(active==1, source_name=="UNCTAD", retrieve_type=="file", source_frequency=="y")
      unctad_names <- unctad_impplan$indicator_code
      unctad_fname <- here("_DB", "_extsources", unctad_impplan$file_name[1])
    
      if (length(unctad_names)>0 & all(!is.na(unctad_names))) {
        
        print("UNCTAD")
        unctad_data <- read.csv(unctad_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"),
                           colClasses=c("Economy"="character"), skip=0)

        unctad_data %>% filter (Flow.Label=="Exports") %>%
          select(Year, Economy, Economy.Label, Diversification.Index) %>% mutate(Economy = recode(Economy, '757' = '756')) %>%  # CH+LI -> CH
          mutate(country_id = countrycode(as.numeric(Economy), origin = 'un', destination = 'iso2c')) %>%
          select(-c(Economy.Label, Economy)) %>% rename("year"="Year") -> unctad_data
        
        unctad_data <- eval(parse(text = glue("rename(unctad_data,'{unctad_names}'='Diversification.Index')") ))
        
        extdata_y %>% left_join(unctad_data, by = c("country_id" = "country_id", "year"="year"),
                                suffix=c("","_old")) -> extdata_y
        print("+")
        
      }
    
    })
  
    ##### Import yearly IMF data
    try({
      
      imfy_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="y")
      imfy_dbnames <- imfy_impplan$database_name
      imfy_names <- imfy_impplan$indicator_code
      imfy_codes <- imfy_impplan$retrieve_code
      
      if (length(imfy_names)>0 & all(!is.na(imfy_names))) {
        
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
        print("+")
        
      }
    
    })
      
    ##### Import quarterly IMF data
    try({
      
      imfq_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="q")
      imfq_dbnames <- imfq_impplan$database_name
      imfq_names <- imfq_impplan$indicator_code
      imfq_codes <- imfq_impplan$retrieve_code
      
      if (length(imfq_names)>0 & all(!is.na(imfq_names))) {
      
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
        print("+")
        
      }  
    
    })
    
    ##### Import monthly IMF data
    try({
      
      imfm_impplan <- impplan %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="m")
      imfm_dbnames <- imfm_impplan$database_name
      imfm_names <- imfm_impplan$indicator_code
      imfm_codes <- imfm_impplan$retrieve_code
      
      if (length(imfm_names)>0 & all(!is.na(imfm_names))) {
        
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
        print("+")
        
      }
    
    })
      
    ##### Import IDS external debt statistics
    try({
      
      ids_impplan <- impplan %>% filter(active==1, database_name=="IDS", retrieve_type=="file", source_frequency=="y")
      ids_names <- ids_impplan$indicator_code
      ids_fname <- here("_DB", "_extsources", ids_impplan$file_name[1])
      ids_sheets <- unique(ids_impplan$sheet_name)
      ids_codes <- ids_impplan$retrieve_code
      
      if (length(ids_names)>0 & all(!is.na(ids_names))) {
        
        print("IDS")
        ids_data <- read_excel(ids_fname, sheet = ids_sheets, col_names = T, na = "#N/A")
        ids_data <- ids_data %>% rename('country'='Country Name', 'country_id'='Country Code', 'variable_code'='Series Code', 
                              'variable_name'='Series Name', 'counterpart_name'='Counterpart-Area Name', 'counterpart_code'='Counterpart-Area Code') %>% 
                              filter(counterpart_name=="World") %>% select(-c('counterpart_code', 'counterpart_name', 'country', 'variable_name')) %>%
                              filter(variable_code %in% ids_codes)
        
        ids_data <- reshape2::melt(ids_data, id.vars = c("country_id", "variable_code"), variable.name = "year", value.name = "value") %>% 
            mutate(year = as.numeric(as.character(year))) %>%
            left_join(ids_impplan %>% select(indicator_code, retrieve_code), by=c('variable_code'='retrieve_code'), suffix=c("","_old")) %>%
            select(-c(variable_code)) %>% pivot_wider(names_from = "indicator_code", values_from = "value") %>%
            mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                          custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                          'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F))
          
        extdata_y <- extdata_y %>% left_join(ids_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
        print("+")
        
      }
    
    })  
      
    ##### Import daily and monthly BIS data on policy rates and exchange rates
    try({
      
      bis_impplan <- impplan %>% filter(active==1, source_name=="BIS", retrieve_type=="file", 
                                        database_name %in% c("Policy rates (flat)", "US dollar exchange rates (flat)"))
      bis_names <- bis_impplan$indicator_code
      bis_fname <- here("_DB", "_extsources", bis_impplan$file_name)
      bis_freq <- toupper(bis_impplan$source_frequency)
      bis_codes <- bis_impplan$retrieve_code

      suppressMessages({
        EZ_countries <- read_excel(here("_DB","1_peers_params.xlsx"), sheet = "groups", col_names = F, skip=2, n_max=11)[c(2,11),-c(1:3)]
      })
      EZ_countries <- data.frame(t(EZ_countries)) %>% filter(X1 == 1) %>% select(X2) %>% unlist()
      EZ_countries <- countrycode(EZ_countries, origin = 'iso3c', destination = 'iso2c', 
                                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                          'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)
      
      if (length(bis_names)>0 & all(!is.na(bis_names))) {
        
        print("BIS policy and exchange rates")
        for (i in seq_along(bis_names)) {  

            bis_data <- read.csv(bis_fname[i], header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings=c("..","","NA"), skip=0) %>%
              select(any_of(c("REF_AREA.Reference.area", "TIME_PERIOD.Time.period.or.range", "OBS_VALUE.Observation.Value", "FREQ.Frequency", "COLLECTION.Collection"))) %>%
              rename(ref_area = REF_AREA.Reference.area, time_period = TIME_PERIOD.Time.period.or.range, value = OBS_VALUE.Observation.Value, freq = FREQ.Frequency) %>%
              mutate(country_id = substr(ref_area,1,2), freq = substr(freq,1,1), value = as.numeric(value)) 

            if (!is.na(bis_codes[i])) {bis_data <- bis_data %>% rename(aveop = COLLECTION.Collection) %>% mutate(aveop = substr(aveop,1,1)) %>% 
              filter(aveop == bis_codes[i]) }
            bis_data <- bis_data %>% filter(freq == bis_freq[i]) %>% select(time_period, country_id, value)

            
            # managing EZ countries (для этих стран нужно закоалесить имеющийся ряд и XM) 
            EZ_rate <- bis_data %>% filter(country_id == "XM") %>% select(time_period, value)
            EZ_full <- bis_data %>% filter(country_id == "US") %>% select(time_period) %>% 
              left_join(EZ_rate, by=c("time_period"="time_period")) %>% rename("new_value" = "value")
            
            for (j in EZ_countries) {
              
              EZ_country_old <- bis_data %>% filter(country_id == j) %>% select(time_period, value) %>% 
                rename("old_value" = "value")
              if (dim(EZ_country_old)[1] == 0) {EZ_country_old <- EZ_full %>% mutate(old_value = NA) %>% 
                select(time_period, old_value) }
              EZ_country_new <- EZ_full %>% left_join(EZ_country_old, by=c("time_period"="time_period"))
              EZ_country_new <- EZ_country_new %>% mutate(value = coalesce(old_value, new_value), country_id = j) %>% 
                select(time_period, country_id, value)

              bis_data <- bis_data %>% filter(country_id != j)
              bis_data <- bind_rows(bis_data, EZ_country_new)
            }
                         
            if (bis_freq[i] == "D") {
              bis_data <- bis_data %>% mutate(date = as.Date(time_period, "%Y-%m-%d")) %>% select(country_id, date, value)
              bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
              extdata_d <- extdata_d %>% left_join(bis_data, by = c("country_id" = "country_id", "date"="date"), suffix=c("","_old")) 
              }
          
            if (bis_freq[i] == "M") {
              bis_data <- bis_data %>% mutate(year = as.numeric(substr(time_period, 1, 4)), month = as.numeric(substr(time_period, 6, 7))) %>%
                  select(country_id, year, month, value)
              bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
              extdata_m <- extdata_m %>% left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
              }

          print(bis_names[i])
        
        }
      }  
    
    })  
    
    ##### Import monthly BIS data on effective exchange rates
    try({
      
      bis_impplan <- impplan %>% filter(active==1, source_name=="BIS", retrieve_type=="file", database_name == "Effective exchange rate indices (monthly)")
      bis_names <- bis_impplan$indicator_code
      bis_fname <- here("_DB", "_extsources", bis_impplan$file_name)
      bis_freq <- toupper(bis_impplan$source_frequency)
      bis_codes <- bis_impplan$retrieve_code
      
      if (length(bis_names)>0 & all(!is.na(bis_names))) {
        
        print("BIS effective exchange rates")
        for (i in seq_along(bis_names)) {  
          
          bis_data <- read.csv(bis_fname[i], header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
                               na.strings=c("..","","NA"), skip=0) %>%
            select(all_of(c("REF_AREA.Reference.area", "TIME_PERIOD.Time.period.or.range", "OBS_VALUE.Observation.Value", "FREQ.Frequency", 
                            "EER_TYPE.Type", "EER_BASKET.Basket"))) %>%
            rename(ref_area = REF_AREA.Reference.area, time_period = TIME_PERIOD.Time.period.or.range, value = OBS_VALUE.Observation.Value, 
                   freq = FREQ.Frequency, type = EER_TYPE.Type, basket = EER_BASKET.Basket) %>%
            mutate(country_id = substr(ref_area,1,2), freq = substr(freq,1,1), type = substr(type,1,1), basket = substr(basket,1,1), value = as.numeric(value)) %>%
            filter(freq == bis_freq[i], type == bis_codes[i], basket == "B") %>% select(time_period, country_id, value)
          
            bis_data <- bis_data %>% mutate(year = as.numeric(substr(time_period, 1, 4)), month = as.numeric(substr(time_period, 6, 7))) %>%
              select(country_id, year, month, value)
            bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
            extdata_m <- extdata_m %>% left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
          
          print(bis_names[i])
          
        }
      }  
      
    })    
    
    ##### Import IMF Fiscal monitor structural indicators
    try({
      
      fm_impplan <- impplan %>% filter(active==1, database_name=="FM", retrieve_type=="file", source_frequency=="y")
      fm_names <- fm_impplan$indicator_code
      fm_fname <- here("_DB", "_extsources", fm_impplan$file_name[1])
      fm_sheets <- fm_impplan$sheet_name
      
      if (length(fm_names)>0 & all(!is.na(fm_names))) {
        
        print("IMF-FM")
        for (i in seq_along(fm_names)) {
        
          #i=3
          fm_data <- read_excel(fm_fname[i], sheet = fm_sheets[i], col_names = T, na = "#N/A", col_types='text')
          
          fm_data <- fm_data %>% pivot_longer(cols = !contains('country'), names_to = 'year', values_to = 'value') %>%
            mutate(country_id = countrycode(country_code, origin = 'iso3c', destination = 'iso2c',  warn = F,
                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')),
                year = as.numeric(as.character(year)), value = as.numeric(value) ) %>% select(country_id, year, value)
          
          fm_data <- eval(parse(text = glue("rename(fm_data,'{fm_names[i]}'='value')") ))
            
          extdata_y <- extdata_y %>% left_join(fm_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
          }
      
      }
    
    })
      
    ##### Import ILOstat data
    try({
        
      # search
      # toc <- get_ilostat_toc(search = 'informal')
      # toc2 <- toc %>% select(-c(indicator, freq.label, collection.label, subject)) %>% filter(freq == "A")
      # toc2$indicator.label
      
      ilo_impplan <- impplan %>% filter(active==1, source_name=="ILO", retrieve_type=="API")
      ilo_names <- ilo_impplan$indicator_code
      ilo_codes <- ilo_impplan$retrieve_code
      ilo_freq <- ilo_impplan$source_frequency
      
      if (length(ilo_names)>0 & all(!is.na(ilo_names))) {
        
        print("ILOstat")
        for (i in seq_along(ilo_names)) {
          
          #i=1
          ilo_data <- get_ilostat(id = ilo_codes[i], segment = 'indicator')
          
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")))) {ilo_data <- ilo_data %>% filter(sex == "SEX_T",  classif1 == "AGE_YTHADULT_YGE15")}
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("TRUE", "FALSE"))) {ilo_data <- ilo_data %>% filter(sex == "SEX_T")}
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("FALSE", "TRUE"))) {ilo_data <- ilo_data %>% filter(classif1 == "AGE_YTHADULT_YGE15")}
          
          ilo_data <- ilo_data %>% 
                mutate(ref_area = countrycode(ref_area, origin = 'iso3c', destination = 'iso2c', warn = F,
                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')))
          
          if (ilo_freq[i] == "y") {ilo_data <- ilo_data %>% mutate(year = as.numeric(time)) %>% select(ref_area, year, obs_value)}
          if (ilo_freq[i] == "q") {ilo_data <- ilo_data %>% mutate(year = as.numeric(substr(time, 1, 4)), quarter = as.numeric(substr(time, 6, 6))) %>% select(ref_area, year, quarter, obs_value)}
          if (ilo_freq[i] == "m") {ilo_data <- ilo_data %>% mutate(year = as.numeric(substr(time, 1, 4)), month = as.numeric(substr(time, 6, 7))) %>% select(ref_area, year, month, obs_value)}
          
          ilo_data <- eval(parse(text = glue("rename(ilo_data,'{ilo_names[i]}'='obs_value')") ))
          
          if (ilo_freq[i] == "y") {extdata_y <- extdata_y %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year"), suffix=c("","_old"))}
          if (ilo_freq[i] == "q") {extdata_q <- extdata_q %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "quarter" = "quarter"), suffix=c("","_old"))}
          if (ilo_freq[i] == "m") {extdata_m <- extdata_m %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "month" = "month"), suffix=c("","_old"))}
          print("+")
          
          }
      
      }
    
    })
      
    ##### Import daily data on COVID
    try({  
    
      covid_impplan <- impplan %>% filter(active==1, source_name=="Ourworldindata", retrieve_type=="file", database_name== "COVID tracker")
      covid_names <- covid_impplan$indicator_code
      covid_codes <- covid_impplan$retrieve_code
      covid_fname <- here("_DB", "_extsources", covid_impplan$file_name[1])
      
      if (length(covid_names)>0 & all(!is.na(covid_names))) {
        
        print("OWID")
        covid_data <- read.csv(covid_fname, header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0) %>%
                    mutate(
                      date = as.Date(date),
                      country_id = countrycode(iso_code, origin = 'iso3c', destination = 'iso2c', warn = F,
                          custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))
                    )
        
        eval(parse(text = glue("covid_data <- covid_data %>% select(date, country_id, {paste(covid_codes, collapse=', ')}) %>% \\
                               rename_at(vars(any_of(covid_codes)), ~covid_names) ") ))
        extdata_d <- extdata_d %>% left_join(covid_data, by=c("country_id", "date"), suffix=c("","_old"))
        print("+")
        
      }
    
    })
      
    ##### Import UN HDR data
    try({
        
      unhdr_impplan <- impplan %>% filter(active==1, source_name=="UN", retrieve_type=="file", database_name=="HDR")
      unhdr_names <- unhdr_impplan$indicator_code
      unhdr_codes <- unhdr_impplan$retrieve_code
      unhdr_fname <- here("_DB", "_extsources", unhdr_impplan$file_name)
      
      if (length(unhdr_names)>0 & all(!is.na(unhdr_names))) {
        
        print("UN-HDR")
        
        for (i in 1:length(unhdr_names)) {
          
          unhdr_data <- read.csv(unhdr_fname[i], header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
          eval(parse(text = glue("unhdr_data <- unhdr_data %>% select(iso3, starts_with('{unhdr_codes[i]}_1'), starts_with('{unhdr_codes[i]}_2'))") ))
            
          unhdr_data <- unhdr_data %>% pivot_longer(!iso3, names_to = "year", values_to = "value") %>% 
            mutate(year = as.numeric(str_sub(year, -4, -1)), value = as.numeric(value)) %>%
            mutate(iso3 = countrycode(iso3, origin = 'iso3c', destination = 'iso2c', warn = F,
                                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')) )
          
          eval(parse(text = glue("unhdr_data <- unhdr_data %>% rename('{unhdr_names[i]}' = 'value', 'country_id' = 'iso3')") ))
          
          extdata_y %>% left_join(unhdr_data, by = c("country_id" = "country_id", "year" = "year"),
                                  suffix=c("","_old")) -> extdata_y
          print("+")
          
        }
      }
    
    })  
      
    ##### Import Chinn-Ito financial system classification
    try({
        
      ci_impplan <- impplan %>% filter(active==1, database_name=="Chinn-Ito", retrieve_type=="file", source_frequency=="y")
      ci_names <- ci_impplan$indicator_code
      ci_codes <- ci_impplan$retrieve_code
      ci_fname <- here("_DB", "_extsources", ci_impplan$file_name[1])
      ci_sheets <- ci_impplan$sheet_name
      
      if (length(ci_names)>0 & all(!is.na(ci_names))) {
        
        print("Chinn-Ito")
        for (i in seq_along(ci_names)) {
          
          #i=1
          ci_data <- read_excel(ci_fname[i], sheet = ci_sheets[i], col_names = T, na = "#N/A", col_types='text')
          
          ci_data <- eval(parse(text = glue("ci_data %>% select('IMF-World Bank Country Code', year,'{ci_codes[i]}')") ))
          ci_data <- eval(parse(text = glue("rename(ci_data,'country_id'='IMF-World Bank Country Code', 'value'='{ci_codes[i]}')") ))
          
          ci_data <- ci_data %>% mutate(country_id = countrycode(country_id, origin = 'imf', destination = 'iso2c'),
                   year = as.numeric(year), value = as.numeric(value) ) %>% select(country_id, year, value)
          
          ci_data <- eval(parse(text = glue("rename(ci_data,'{ci_names[i]}'='value')") ))
          
          extdata_y <- extdata_y %>% left_join(ci_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
        }
        
      }
      
    })  
    
    ##### Import WEO outlook
    try({
      
      weo_impplan <- impplan %>% filter(active==1, database_name=="WEO", retrieve_type=="file", source_frequency=="y")
      weo_names <- weo_impplan$indicator_code
      weo_codes <- weo_impplan$retrieve_code
      weo_fname <- here("_DB", "_extsources", weo_impplan$file_name[1])
      weo_sheets <- weo_impplan$sheet_name
      
      if (length(weo_names)>0 & all(!is.na(weo_names))) {
        
        print("IMF-WEO")
        for (i in seq_along(weo_names)) {
          
          #weo_fname = "./_extsources/WEO.xls"
          #i=1
          #weo_data <- read_tsv(weo_fname, na = c("", "NA", "n/a"), col_types = "c", show_col_types = FALSE)
          weo_data <- read_excel(weo_fname, sheet = weo_sheets[i], na = c("", "NA", "n/a"))
          weo_data <- weo_data %>% rename('country_id' = 'ISO', 'code' = 'WEO Subject Code') %>%
            mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = gsub, pattern = ",", replacement = "") %>%
            mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = as.numeric)
          weo_data <- eval(parse(text = glue("weo_data %>% filter(code == '{weo_codes[i]}')") ))
          
          weo_data <- weo_data %>% select(country_id, starts_with('19'), starts_with('20')) %>%
                mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))) %>%
                pivot_longer(!country_id, names_to = "year", values_to = "value") %>%
                mutate(year = as.numeric(year), value = as.numeric(value)) %>%
                mutate(value = value + 0.000006) # needed to prevent excel from treating as dates

          weo_data <- eval(parse(text = glue("rename(weo_data,'{weo_names[i]}'='value')") ))
          
          extdata_y <- extdata_y %>% left_join(weo_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
        }
        
      }
    
    })

  ##### Import WEO outlook for aggregates
  try({
    
    weo_impplan <- impplan %>% filter(active==1, database_name=="WEO_aggr", retrieve_type=="file", source_frequency=="y")
    weo_names <- weo_impplan$indicator_code
    weo_codes <- weo_impplan$retrieve_code
    weo_fname <- here("_DB", "_extsources", weo_impplan$file_name[1])
    weo_sheets <- weo_impplan$sheet_name
    
    if (length(weo_names)>0 & all(!is.na(weo_names))) {
      
      print("IMF-WEO-aggr")
      for (i in seq_along(weo_names)) {
        
        #i=1
        #weo_data <- read_tsv(weo_fname, na = c("", "NA", "n/a"), col_types = "c", show_col_types = F)
        weo_data <- read_excel(weo_fname, sheet = weo_sheets[i], na = c("", "NA", "n/a"))        
        weo_data <- weo_data %>% rename('code' = 'Country Group Name', 'indicator' = 'WEO Subject Code', 
                        'note' = 'Country/Series-specific Notes') %>%
          mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = gsub, pattern = ",", replacement = "") %>%
          mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = as.numeric)
        weo_data <- eval(parse(text = glue("weo_data %>% filter(code == '{weo_codes[i]}', indicator == 'NGDP_RPCH')") ))
        
        weo_data <- weo_data %>% select(starts_with('19'), starts_with('20')) %>% 
          mutate(country_id = "1W") %>% pivot_longer(!country_id, names_to = "year", values_to = "value") %>%
          mutate(year = as.numeric(year), value = as.numeric(value))
        
        weo_data <- eval(parse(text = glue("rename(weo_data,'{weo_names[i]}'='value')") ))
        
        extdata_y <- extdata_y %>% left_join(weo_data, by = c("country_id" = "country_id", "year" = "year"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  })  
  
  ##### Import UNPD aggregated data
  try({
    
    pp_impplan <- impplan %>% filter(active==1, database_name=="WPP_aggr", retrieve_type=="file", source_frequency=="y")
    pp_names <- pp_impplan$indicator_code
    pp_codes <- pp_impplan$retrieve_code
    pp_fname <- here("_DB", "_extsources", pp_impplan$file_name[1])
    
    if (length(pp_names)>0 & all(!is.na(pp_names))) {
      
      print("UNPD aggregates")
      for (i in seq_along(pp_names)) {
        
        #i=1
        pp_data <- read.csv(pp_fname, header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
        
        pp_data <- eval(parse(text = glue("pp_data %>% select('ISO2_code', 'Time', 'Variant', '{pp_codes[i]}')") ))
        pp_data <- eval(parse(text = glue("rename(pp_data,'country_id'='ISO2_code', 'year' = 'Time', 'value'='{pp_codes[i]}')") ))
        
        pp_data <- pp_data %>% filter(Variant == "Medium", !is.na(country_id)) %>% mutate(year = as.numeric(year), value = as.numeric(value) ) %>% 
              select(country_id, year, value)
        
        pp_data <- eval(parse(text = glue("rename(pp_data,'{pp_names[i]}'='value')") ))
        
        extdata_y <- extdata_y %>% left_join(pp_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  }) 
  
  ##### Import UNPD 5-year groups data
  try({
    
    pp_impplan <- impplan %>% filter(active==1, database_name=="WPP_5yr", retrieve_type=="file", source_frequency=="y")
    pp_names <- pp_impplan$indicator_code
    pp_codes <- pp_impplan$retrieve_code
    pp_dict <- data.frame(pp_codes, pp_names)
    pp_fname <- here("_DB", "_extsources", pp_impplan$file_name[1])
    
    if (length(pp_names)>0 & all(!is.na(pp_names))) {
      
      print("UNPD 5-year groups")
        
      pp_data <- read.csv(pp_fname, header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
        
      pp_data <- pp_data %>% select('ISO2_code', 'Time', 'Variant', 'AgeGrp', 'PopTotal') %>% filter(Variant == "Medium") %>%
                rename('country_id' = 'ISO2_code', 'year' = 'Time', 'age' = 'AgeGrp', 'value' = 'PopTotal') %>% 
                mutate(year = as.numeric(year), value = as.numeric(value)) %>% 
                filter(age %in% pp_codes, Variant == "Medium", !is.na(country_id)) %>% select(-c(Variant)) 
      
      pp_data <- pp_data %>% left_join(pp_dict, by = c('age' = 'pp_codes')) %>% select(-c(age)) %>%
          pivot_wider(names_from = pp_names, values_from = value)

      extdata_y <- extdata_y %>% left_join(pp_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
    }
    
  })
  
  ##### Import data on debt held in reserves
  try({
    
    hres_impplan <- impplan %>% filter(active==1, database_name=="BondsInReserves", retrieve_type=="file", source_frequency=="y")
    hres_names <- hres_impplan$indicator_code[1]
    hres_fname <- here("_DB", "_extsources", hres_impplan$file_name[1])
    hres_sheets <- hres_impplan$sheet_name[1]
    
    if (length(hres_names)>0 & all(!is.na(hres_names))) {
      
      print("BondsInReserves")
      
      hres_data <- read_excel(hres_fname, sheet = hres_sheets, skip = 3, col_names = T, na = "C")
      names(hres_data)[1] <- "country_id"
      hres_data <- hres_data %>% select(country_id, contains("DEC")) %>% 
        pivot_longer(!country_id, names_to = "year", values_to = "value") %>%
        mutate(country_id = countrycode(country_id, origin = 'country.name', destination = 'iso2c', custom_match = c('World' = '1W'), warn = F)) %>% 
        mutate(year = as.numeric(str_sub(year, 6, 9)))
      
      hres_data <- eval(parse(text = glue("rename(hres_data,'{hres_names[1]}'='value')") ))
      
      extdata_y <- extdata_y %>% left_join(hres_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
    }
    
  })
    
  ##### Import data from Fiscal Space Database
  
  try({
    
    fsdb_impplan <- impplan %>% filter(active==1, database_name=="FSDB", retrieve_type=="file", source_frequency=="y")
    fsdb_names <- fsdb_impplan$indicator_code
    fsdb_sheets <- fsdb_impplan$sheet_name
    
    
    if (length(fsdb_names)>0 & all(!is.na(fsdb_names))) {
      
      print("FSDB")
      fsdb_fname <- here("_DB", "_extsources", fsdb_impplan$file_name[1])
      
      for(i in seq_along(fsdb_names)) {
      
      fsdb_data <- read_excel(fsdb_fname, sheet = fsdb_sheets[i], col_names = T, na = "")
      names(fsdb_data)[1] <- "country_id"
      fsdb_data <- fsdb_data %>% select(country_id, starts_with('19'), starts_with('20')) %>% 
        pivot_longer(!country_id, names_to = "year", values_to = "value") %>%
        mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', warn = F)) %>% 
        mutate(year = as.numeric(year), value = as.numeric(value))
      
      fsdb_data <- eval(parse(text = glue("rename(fsdb_data,'{fsdb_names[i]}'='value')") ))
      
      extdata_y <- extdata_y %>% left_join(fsdb_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
      }
      
    }
    
  })
  
  ##### Import data from the Integrated Macroprudential Policy (iMaPP) Database
  
  try({
    
    imapp_impplan <- impplan %>% filter(active==1, database_name=="iMaPP", retrieve_type=="file", source_frequency=="m")
    imapp_names <- imapp_impplan$indicator_code
    imapp_sheets <- unique(imapp_impplan$sheet_name)
    
    
    if (length(imapp_names)>0 & all(!is.na(imapp_names))) {
      
      print("iMaPP")
      imapp_fname <- here("_DB", "_extsources", imapp_impplan$file_name[1])
      
      for(i in seq_along(imapp_sheets)) {
        
        old_codes <- imapp_impplan %>% filter(sheet_name == imapp_sheets[i]) %>% pull(retrieve_code)
        new_codes <- imapp_impplan %>% filter(sheet_name == imapp_sheets[i]) %>% pull(indicator_code)
        imapp_data <- read_excel(imapp_fname, sheet = imapp_sheets[i], col_names = T, na = "") %>%
          rename_at(vars(c("iso2", "Year", "Month")), ~c("country_id", "year", "month")) %>%
          rename_at(vars(old_codes), ~new_codes) %>% select("country_id", "year", "month", all_of(new_codes)) %>%
          mutate(year = as.numeric(year), month = as.numeric(month))
        
        extdata_m <- extdata_m %>% left_join(imapp_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  })  

  ##### Import data on the financial systems structure (from Financial Stability Board)
  
  try({
    
    fsb_impplan_temp <- impplan %>% filter(active==1, source_name=="FSB", retrieve_type=="file")
    fsb_parts <- unique(fsb_impplan_temp$database_name)
    
    for (i in fsb_parts) {
      
      fsb_impplan <- fsb_impplan_temp %>% filter(database_name==i)
      #fsb_impplan <- fsb_impplan_temp %>% filter(database_name==fsb_parts[2])
      fsb_names <- fsb_impplan$indicator_code
      fsb_sheet <- unique(fsb_impplan$sheet_name)[1]
      
      if (length(fsb_names)>0 & all(!is.na(fsb_names))) {
        
        print("FSB")
        fsb_fname <- here("_DB", "_extsources", fsb_impplan$file_name[1])
        old_codes <- fsb_impplan %>% filter(sheet_name == fsb_sheet) %>% pull(retrieve_code)
        new_codes <- fsb_impplan %>% filter(sheet_name == fsb_sheet) %>% pull(indicator_code)
        
        fsb_data <- read_excel(fsb_fname, sheet = fsb_sheet, col_names = T, na = "") %>%
            rename_at(vars(c("Jurisdiction code", "Year", "Entity/Economic function", "Value, in USD trillions")),
                      ~c("country_id", "year", "code", "value")) %>% mutate(value = as.numeric(value))  %>%
          filter(Topic == i) %>% select("country_id", "year", "code", "value") %>% 
          pivot_wider(id_cols = c("country_id", "year"), names_from = "code", values_from = "value") %>%
            rename_at(vars(old_codes), ~new_codes) %>%
            mutate(year = as.numeric(year))
          
          extdata_y <- extdata_y %>% left_join(fsb_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
      
      }  
    }
    
  })   
 
    ##### Return imported
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
    print("+++")
    
}


# Functions to export data from memory

##### Function to generate dict and keep only planned imports

preExport <- function(saveplan, extdata_y, extdata_q, extdata_m, extdata_d) {

    print("Generating the dictionary, finalizing data")
    dict <- saveplan %>% select(indicator, theme, indicator_code, source_frequency, source_name, keep) %>% arrange(theme, indicator_code)
    
    ### Collecting lists of what we planned to download
    for (i in c("y", "q", "m", "d")) {
      eval(parse(text = glue("dict_{i} <- dict %>% filter(source_frequency=='{i}')") ))
    }
    print(dict)
    
    ### Filtering databases to contain only planned output
    extdata_y <- extdata_y %>% select(country, country_id, year, any_of(dict_y$indicator_code))
    extdata_q <- extdata_q %>% select(country, country_id, year, quarter, any_of(dict_q$indicator_code))
    extdata_m <- extdata_m %>% select(country, country_id, year, quarter, month, any_of(dict_m$indicator_code))
    extdata_d <- extdata_d %>% select(country, country_id, date, any_of(dict_d$indicator_code))
    
    ### Checking dict if the data was successfully downloaded
    downloaded <- data.frame(indicator_code = c(names(extdata_y), names(extdata_q), names(extdata_m), names(extdata_d)))
    downloaded$source_frequency <- c(rep("y", length(names(extdata_y))), rep("q", length(names(extdata_q))), rep("m", length(names(extdata_m))), rep("d", length(names(extdata_d))))
    downloaded <- downloaded %>% mutate(success = "+")
    dict <- dict %>% left_join(downloaded, by = c("indicator_code", "source_frequency")) %>%
      mutate(n_countries = 0, start_year = 0, end_year = 0, n_points = 0)
    extdata_d <- extdata_d %>% mutate(year = year(date))
    
    ### Calculating availability of data: non-empty countries and years
    for (i in seq_along(dict$indicator)) {
      if (!is.na(dict$success[i])) {
        a <- eval(parse(text = glue("extdata_{dict$source_frequency[i]} %>% 
          select(country_id, year, {dict$indicator_code[i]}) %>% 
          filter(!is.na({dict$indicator_code[i]}))") ))
        dict$n_countries[i] <- a %>% select(country_id) %>% unique() %>% dim() %>% '['(1)
        dict$start_year[i] <- a %>% select(year) %>% unique() %>% min(na.rm = TRUE) %>% 'if'(is.infinite(.), NA, .)
        dict$end_year[i] <- a %>% select(year) %>% unique() %>% max(na.rm = TRUE) %>% 'if'(is.infinite(.), NA, .)
        dict$n_points[i] <- a %>% select(country_id) %>% dim() %>% '['(1)
      }
    }
    extdata_d <- extdata_d %>% select(-c("year"))
    
    ### Define final variants of dict
    dict_d <- dict %>% filter(source_frequency == "d")
    dict <- dict %>% filter(source_frequency != "d")
    
    ### Return everything
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict, dict_d = dict_d))

}

##### Export data on all countries to the yearly/quarterly/monthly database

writeDatafiles <- function(data_fname, data_d_fname, extdata_y, extdata_q, extdata_m, extdata_d, dict, dict_d) {

    data_export <- list(extdata_y, extdata_q, extdata_m, dict)
    names(data_export) <- c("y", "q", "m", "dict")
    write_xlsx(data_export, path = data_fname, col_names = TRUE, format_headers = TRUE)
    
    data_export_d <- list(extdata_d, dict_d)
    names(data_export_d) <- c("d", "dict_d")
    write_xlsx(data_export_d, path = data_d_fname, col_names = TRUE, format_headers = TRUE)

}

##### Export data on all countries to 4 csv files: yearly/quarterly/monthly database and dict

writeDatafilesCsv <- function(datalist, path) {
  
  print("Writing csv files")
  datalist$dict <- rbind(datalist$dict, datalist$dict_d) %>% select(-c(keep, success, n_countries, n_points))
  datalist$dict_d <- NULL
  
  for (i in 1:length(datalist)) {
    write_excel_csv2(datalist[[i]], file = here(path, glue("{names(datalist)[i]}.csv")), col_names = TRUE, na="")
  }
  
}

## Export data on a specific country to the yearly database

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
