#install.packages("tidyverse") # коллекция пакетов от Hadley Wickham
#install.packages("WDI")
#install.packages("ggthemes")
#install.packages("countrycode")
#install.packages("readxl")
#install.packages('IMFData')
#install.packages("writexl")
#install.packages("writexl",lib = "C:/Users/iamku/OneDrive/Documents/R/win-library/4.1")
#install.packages("rJava")
#install.packages("imfr")
#install.packages("gsubfn")
#install.packages("wbstats")
#install.packages("Rilostat")

library("dplyr")
library("reshape2")
library("WDI")
library("wbstats")
library("countrycode")
library("readxl")
library("tidyr")
library("data.table")
library("writexl")
library("stringr")
library("imfr")
library("gsubfn")
library("jsonlite")
library("Rilostat")

##### Mode import (default) or update
update = 1


##### Where is import schedule saved?
setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/download_data")
#setwd("C:/Projects/R_utilities/download_data")
#extdata_y <- read_excel("extdata.xlsx", sheet = "y", col_names = T, skip=0,
#        col_types = c("text", "text", rep("numeric", 29)))


##### Import previously downloaded data
if (update == 1) {
  data_fname <- "Imported_DB.xlsx"
  
  a <- length(read_excel(data_fname, sheet = "y", col_names = T, skip=0, n_max = 0))
  extdata_y <- read_excel(data_fname, sheet = "y", col_names = T, skip=0,
                          col_types = c("text", "text", rep("numeric", a-2)))
  a <- length(read_excel(data_fname, sheet = "q", col_names = T, skip=0, n_max = 0))
  extdata_q <- read_excel(data_fname, sheet = "q", col_names = T, skip=0,
                          col_types = c("text", "text", rep("numeric", a-2)))
  a <- length(read_excel(data_fname, sheet = "m", col_names = T, skip=0, n_max = 0))
  extdata_m <- read_excel(data_fname, sheet = "m", col_names = T, skip=0,
                          col_types = c("text", "text", rep("numeric", a-2)))
}

##### Set import/update schedule
param_fname <- "0_database_params.xlsx"
impdata <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
parameters <- read_excel(param_fname, sheet = "scope", col_names = T, skip=1, n_max=1)
year_first <- parameters$start
year_final <- parameters$end
years <-c(year_first:year_final)
months <- c(1:12)
days <- seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by="days")

#filldata <- read_excel(param_fname, sheet = "fill", col_names = T, skip=1)
#dict <- unique(rbind(impdata[,1:2], setnames(filldata[filldata$new_frequency=="y",1:2], names(impdata[,1:2]))))
if (update == 1) { impdata <- impdata %>% filter(update == 1) }
 
###### Generate/update data containers

if (update == 0) {

  countries <- WDI(indicator = "NY.GDP.MKTP.CD", start = year_first, end = year_final, extra=F) %>%
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
  
  #test <- extdata_y %>% filter(country=="Russian Federation", year>2013)

}

if (update == 1) {
  for (i in seq_along(impdata$indicator)) { 
    
    eval(parse( text = paste0("if ('", impdata$indicator_code[i], "' %in% names(extdata_", impdata$source_frequency[i],")) { extdata_",
                              impdata$source_frequency[i], " <- extdata_",
                              impdata$source_frequency[i], " %>% select(-c(", impdata$indicator_code[i],")) }") )) 
    }
}
  
##### Import WDI

wdi_impdata <- impdata %>% filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="y")
wdi_names <- wdi_impdata$indicator_code
wdi_codes <- wdi_impdata$retrieve_code

wdi_data <- WDI(indicator = wdi_codes, start = year_first, end = year_final, extra=F) %>% select(-c(country)) %>%
  rename_at(vars(any_of(wdi_codes)), ~wdi_names)

extdata_y <- extdata_y %>% left_join(wdi_data, by = c("country_id"="iso2c", "year"="year"), suffix=c("","_old"))


##### Import WGI

wgi_impdata <- impdata %>% filter(active==1, database_name=="WGI", retrieve_type=="file", source_frequency=="y")
wgi_names <- wgi_impdata$indicator_code
wgi_fname <- unique(wgi_impdata$file_name)
wgi_sheets <- wgi_impdata$sheet_name
wgi_type <- wgi_impdata$retrieve_code
wgi_data <- NULL

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
    
    melt(wgi_data, id.vars = "country_id", variable.name = "year", value.name = wgi_names[i]) %>% 
      mutate(year =  as.numeric(as.character(year))) -> wgi_data
    
    extdata_y %>% left_join(wgi_data, by = c("country_id" = "country_id", "year"="year"), 
                            suffix = c("", "_wgi")) -> extdata_y
    
  }


##### Import UN education index

un_impdata <- impdata %>% filter(active==1, database_name=="HDR", retrieve_type=="API", source_frequency=="y")
un_names <- un_impdata$indicator_code
un_code <- paste("http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=", un_impdata$retrieve_code, sep="")

un_dat <- flatten(data.frame(jsonlite::fromJSON(un_code)))
un_data <- unlist(data.frame(t(un_dat)))
un_descr <- names(un_data)
un_data <- as.numeric(un_data)
un_data <- data.frame(cbind(un_descr, un_data)) %>% mutate(un_data = as.numeric(un_data))
names(un_data)[2] <- un_names

un_data <- un_data %>% mutate(country_id = str_sub(un_descr, 17,19), year = as.numeric(str_sub(un_descr, -4,-1)),
        country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c')) %>%
  select(-c(un_descr)) %>% select(country_id, year, educ)

extdata_y %>% left_join(un_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old")) -> extdata_y


##### Import UNCTAD export diversification index

unctad_impdata <- impdata %>% filter(active==1, source_name=="UNCTAD", retrieve_type=="file", source_frequency=="y")
unctad_names <- unctad_impdata$indicator_code
unctad_fname <- unctad_impdata$file_name

unctad_data <- read.csv(unctad_fname, header = TRUE, sep = ",", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"),
                   colClasses=c("Economy"="numeric"), skip=0)

unctad_data %>% filter (Flow.Label=="Exports") %>%
  select(Year, Economy, Economy.Label, Diversification.Index) %>%
  mutate(country_id = countrycode(Economy, origin = 'un', destination = 'iso2c')) %>%
  select(-c(Economy.Label, Economy)) %>% rename("year"="Year") -> unctad_data

unctad_data <- eval(parse(text=paste("rename(unctad_data,'",unctad_names,"'='Diversification.Index')", sep="")) )

extdata_y %>% left_join(unctad_data, by = c("country_id" = "country_id", "year"="year"),
                        suffix=c("","_old")) -> extdata_y


##### Import yearly IMF data

imfy_impdata <- impdata %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                   source_frequency=="y")
imfy_dbnames <- imfy_impdata$database_name
imfy_names <- imfy_impdata$indicator_code
imfy_codes <- imfy_impdata$retrieve_code

imf_data_in <- NULL
for (i in seq_along(imfy_codes)) {
  new_data <- imf_data(database_id = imfy_dbnames[i],indicator = imfy_codes[i], freq = 'A', country = 'all',
                       start = year_first , end = year_final) %>% filter(!is.na(year))
  if (i==1) {imf_data_in <- new_data} else {imf_data_in <- imf_data_in %>% full_join(new_data, by=c('iso2c'='iso2c', 'year'='year'), suffix=c("","_old"))}
}

imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfy_codes)), ~imfy_names) %>% mutate(year=as.numeric(year))

extdata_y %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year"),
                        suffix=c("","_old")) -> extdata_y

#database list: imf_databases <- imf_ids() 
#database dimensions: DOT_codelist<-imf_codelist(database_id = "BOP")
#dimension options: DOT_FREQ_codes<-imf_codes(codelist = "CL_AREA_BOP")
#indicator codes: IFS_INDICATOR_codes<-imf_codes(codelist = "CL_INDICATOR_IFS")
#main table from countrycode package: country_set <- codelist
#head(extdata_y)


##### Import quarterly IMF data

imfq_impdata <- impdata %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                   source_frequency=="q")
imfq_dbnames <- imfq_impdata$database_name
imfq_names <- imfq_impdata$indicator_code
imfq_codes <- imfq_impdata$retrieve_code

imf_data_in <- NULL
for (i in seq_along(imfq_codes)) {
  new_data <- imf_data(database_id = imfq_dbnames[i],indicator = imfq_codes[i], freq = 'Q', country = 'all',
                       start = ifelse(imfq_dbnames[i]=='BOP', 2011, year_first) , end = year_final) %>% filter(!is.na(year_quarter))
  if (i==1) {imf_data_in <- new_data} else {imf_data_in <- imf_data_in %>% 
                  full_join(new_data, by=c('iso2c'='iso2c', 'year_quarter'='year_quarter'), suffix=c("","_old"))}
}

imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfq_codes)), ~imfq_names) %>% 
  mutate(year=as.numeric(substring(year_quarter,1,4)), quarter=as.numeric(substring(year_quarter,7,7))) %>% select(-c(year_quarter)) 

extdata_q %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year", "quarter"="quarter"),
                        suffix=c("","_old")) -> extdata_q

#test <- extdata_q %>% filter(country == "Russian Federation", year>2000) %>% select(year, country_id, exp_g_usd, exp_s_usd)
#str(imf_data)
#str(extdata_q)


##### Import monthly IMF data

imfm_impdata <- impdata %>% filter(active==1, source_name=="IMF", retrieve_type=="API",
                                   source_frequency=="m")
imfm_dbnames <- imfm_impdata$database_name
imfm_names <- imfm_impdata$indicator_code
imfm_codes <- imfm_impdata$retrieve_code


imf_data_in <- NULL
for (i in seq_along(imfm_codes)) {
  new_data <- imf_data(database_id = imfm_dbnames[i],indicator = imfm_codes[i], freq = 'M', country = 'all',
                       start = ifelse(imfm_dbnames[i]=='BOP', 2011, year_first) , end = year_final) %>% filter(!is.na(year_month))
  if (i==1) {imf_data_in <- new_data} else {imf_data_in <- imf_data_in %>% 
            full_join(new_data, by=c('iso2c'='iso2c', 'year_month'='year_month'), suffix=c("","_old"))}
}

imf_data_in <- imf_data_in %>% rename(country_id = 'iso2c') %>% rename_at(vars(any_of(imfm_codes)), ~imfm_names) %>% 
  mutate(year=as.numeric(substring(year_month,1,4)), month=as.numeric(substring(year_month,6,7))) %>% select(-c(year_month)) 

extdata_m %>% left_join(imf_data_in, by = c("country_id" = "country_id", "year"="year", "month"="month"),
                        suffix=c("","_old")) -> extdata_m

#test <- extdata_m %>% filter(country == "Brazil", year>2013) %>% select(year, month, cpi_ind_av, lcusd_av)
#str(imf_data)
#str(extdata_m)


##### Import IDS external debt statistics

ids_impdata <- impdata %>% filter(active==1, database_name=="IDS", retrieve_type=="file", source_frequency=="y")
ids_names <- ids_impdata$indicator_code
ids_fname <- unique(ids_impdata$file_name)
ids_sheets <- unique(ids_impdata$sheet_name)
ids_codes <- ids_impdata$retrieve_code
  
ids_data <- read_excel(ids_fname, sheet = ids_sheets, col_names = T, na = "#N/A")
ids_data <- ids_data %>% rename('country'='Country Name', 'country_id'='Country Code', 'variable_code'='Series Code', 
                      'variable_name'='Series Name', 'counterpart_name'='Counterpart-Area Name', 'counterpart_code'='Counterpart-Area Code') %>% 
                      filter(counterpart_name=="World") %>% select(-c('counterpart_code', 'counterpart_name', 'country', 'variable_name')) %>%
                      filter(variable_code %in% ids_codes)

ids_data <- reshape2::melt(ids_data, id.vars = c("country_id", "variable_code"), variable.name = "year", value.name = "value") %>% 
    mutate(year = as.numeric(as.character(year)))

ids_data <- ids_data %>% left_join(ids_impdata %>% select(indicator_code, retrieve_code), by=c('variable_code'='retrieve_code'), suffix=c("","_old")) %>%
                select(-c(variable_code))

ids_data <- ids_data %>% pivot_wider(names_from = indicator_code, values_from = value) %>%
  mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                   'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')))
  
extdata_y <- extdata_y %>% left_join(ids_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))


##### Import monthly BIS data on policy rates

bis_impdata <- impdata %>% filter(active==1, source_name=="BIS", retrieve_type=="file", database_name=="Policy rates (monthly)")
bis_names <- bis_impdata$indicator_code
bis_fname <- bis_impdata$file_name

bis_data <- read.csv(bis_fname, header = TRUE, sep = ",", quote = "\"",
                        dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0)

bis_data <- bis_data[,-c(1:8)]
bis_data <- bis_data %>% mutate(country_id = substr(Time.Period,3,4)) %>% select(-c("Time.Period"))
bis_data <- bis_data %>% pivot_longer(!country_id, names_to = "time", values_to = "value")
bis_data <- bis_data %>% mutate(year = as.numeric(substr(time,2,5)), month = as.numeric(substr(time,7,8)), value=as.numeric(value)) %>%
  select(country_id, year, month, value)

bis_data <- eval(parse(text=paste("rename(bis_data,'",bis_names,"'='value')", sep="")) )

extdata_m %>% left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"),
                        suffix=c("","_old")) -> extdata_m


##### Import IMF Fiscal monitor structural indicators

fm_impdata <- impdata %>% filter(active==1, database_name=="FM", retrieve_type=="file", source_frequency=="y")
fm_names <- fm_impdata$indicator_code
fm_fname <- fm_impdata$file_name
fm_sheets <- fm_impdata$sheet_name

for (i in seq_along(fm_names)) {

  #i=3
  fm_data <- read_excel(fm_fname[i], sheet = fm_sheets[i], col_names = T, na = "#N/A", col_types='text')
  
  fm_data <- fm_data %>% pivot_longer(cols = !contains('country'), names_to = 'year', values_to = ) %>%
    mutate(country_id = countrycode(country_code, origin = 'iso3c', destination = 'iso2c', 
        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')),
        year = as.numeric(as.character(year)), value = as.numeric(value) ) %>% select(country_id, year, value)
  
  fm_data <- eval(parse(text=paste0("rename(fm_data,'",fm_names[i],"'='value')")) )
    
  extdata_y <- extdata_y %>% left_join(fm_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))

  }


##### Import ILOstat data

# search
# toc <- get_ilostat_toc(search = 'informal')
# toc2 <- toc %>% select(-c(indicator, freq.label, collection.label, subject)) %>% filter(freq == "A")
# toc2$indicator.label

ilo_impdata <- impdata %>% filter(active==1, source_name=="ILO", retrieve_type=="API")
ilo_names <- ilo_impdata$indicator_code
ilo_codes <- ilo_impdata$retrieve_code
ilo_freq <- ilo_impdata$source_frequency

for (i in seq_along(ilo_names)) {
  
  #i=5
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
  
  ilo_data <- eval(parse(text=paste0("rename(ilo_data,'",ilo_names[i],"'='obs_value')")) )
  
  if (ilo_freq[i] == "y") {extdata_y <- extdata_y %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year"), suffix=c("","_old"))}
  if (ilo_freq[i] == "q") {extdata_q <- extdata_q %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "quarter" = "quarter"), suffix=c("","_old"))}
  if (ilo_freq[i] == "m") {extdata_m <- extdata_m %>% left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "month" = "month"), suffix=c("","_old"))}

  }


##### Import daily data on COVID

covid_impdata <- impdata %>% filter(active==1, source_name=="Ourworldindata", retrieve_type=="file", database_name== "COVID tracker")
covid_names <- covid_impdata$indicator_code
covid_codes <- covid_impdata$retrieve_code
covid_fname <- covid_impdata$file_name[1]

covid_data <- read.csv(covid_fname, header = TRUE, sep = ",", quote = "\"",
                     dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0) %>%
            mutate(
              date = as.Date(date),
              country_id = countrycode(iso_code, origin = 'iso3c', destination = 'iso2c', 
                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))
            )

eval(parse(text=paste0( "covid_data <- covid_data %>% select(date, country_id, ", paste(covid_codes, collapse=", "), ")" )))
extdata_d <- extdata_d %>% left_join(covid_data, by=c("country_id", "date"))


###### Export data on all countries to the yearly/monthly database

data_export <- list(extdata_y, extdata_q, extdata_m)
names(data_export) <- c("y", "q", "m")
write_xlsx(data_export, path = "Imported_DB.xlsx", col_names = TRUE, format_headers = TRUE)

data_export_d <- list(extdata_d)
names(data_export_d) <- c("d")
write_xlsx(data_export_d, path = "Imported_d_DB.xlsx", col_names = TRUE, format_headers = TRUE)


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
