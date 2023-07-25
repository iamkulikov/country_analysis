###### Load libraries
library_names <- c("dplyr","reshape2","ggplot2","ggthemes","countrycode","readxl","tidyr","data.table","writexl","unikn",
                   "ggtext","svglite","stringr","directlabels","fanplot","ggfan","hrbrthemes")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

###### Define custom color palettes

## ACRA palette
ACRA <- newpal( col = c(rgb(147, 202, 116, maxColorValue = 255),rgb(153, 38, 115, maxColorValue = 255),
                        rgb(238, 108, 64, maxColorValue = 255),rgb(155, 155, 155, maxColorValue = 255),
                        rgb(238, 162, 53, maxColorValue = 255),rgb(55, 165, 188, maxColorValue = 255),
                        rgb(69, 159, 122, maxColorValue = 255),rgb(115, 144, 159, maxColorValue = 255),
                        rgb(115, 83, 116, maxColorValue = 255),rgb(60, 100, 162, maxColorValue = 255),
                        rgb(63, 133, 165, maxColorValue = 255),rgb(220, 73, 66, maxColorValue = 255) ),
                names = c("green", "dark", "red", "grey", "sec1", "sec2", "sec3",
                          "sec4", "sec5", "sec6", "sec7", "sec8" )
)

#seecol(ACRA, 
#       col_brd = "white", lwd_brd = 4, 
#       title = "Colours of ACRA", 
#       mar_note = "For fuck's sake")

#using +scale_color_manual(values = ACRA)


####### Function to import filled data

importFilledData <- function(data_fname, data_d_fname) {
  
  for (i in c("y", "q", "m")) {
    eval(parse(text = glue("ncols <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
    eval(parse(text = glue("extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                            col_types = c('text', 'text', rep('numeric', ncols-2)))") ))
  }
  
  ncols <- length(read_excel(data_d_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
  extdata_d <- read_excel(data_d_fname, sheet = "d", col_names = T, skip=0,
                          col_types = c("text", "text", "date", rep("numeric", ncols-3)))
  
  extdata_y <- extdata_y %>% mutate(time=year-1987)
  extdata_q <- extdata_q %>% mutate(time=(year-1987)*4+quarter)
  extdata_m <- extdata_m %>% mutate(time=(year-1987)*12+month)
  #extdata_d <- extdata_d %>% mutate(time=year-1987)
  
  ncols <- length(read_excel(data_fname, sheet = "dict", col_names = T, skip=0, n_max = 0))
  dict <- read_excel(data_fname, sheet = "dict", col_names = T, skip=0,
                     col_types = rep("text", ncols))
  dict_d <- read_excel(data_d_fname, sheet = "dict_d", col_names = T, skip=0,
                     col_types = rep("text", ncols))
  dict <- rbind(dict, dict_d)
  
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict))
  
}


####### Function to get country's codes and the codes of its peers

getPeersCodes <- function(country_name, peers_fname) {
  
  groupsdata <- t(read_excel(peers_fname, sheet = "groups", col_names = T, n_max = 11))
  labels <- c("region", groupsdata[3,-1])
  groupsdata <- data.frame(groupsdata[-c(1:3),])
  colnames(groupsdata) <- labels
  country_iso3c <- groupsdata %>% filter(rownames(groupsdata) == country_name) %>% pull(country_code)
  country_iso2c <- countrycode(country_iso3c, origin = 'iso3c', destination = 'iso2c')
  groupsdata <- groupsdata %>% select(-c(region)) %>% 
      mutate(country_iso2c = countrycode(country_code, origin = 'iso3c', destination = 'iso2c')) %>%
      rename('country_iso3c' = 'country_code') %>% as_tibble
  
  peersdata <- read_excel(peers_fname, sheet = "groups", col_names = T, skip=11)
  peers_default_iso3c <- names(peersdata)[peersdata[peersdata$country_code == country_iso3c, ] == 1]
  peers_default_iso2c <- countrycode(peers_default_iso3c, origin = 'iso3c', destination = 'iso2c')
  peers_neighbours_iso3c <- peersdata %>% select(region, country_code) %>% filter(region==peersdata$region[peersdata$country_code==country_iso3c]) %>%
    pull(country_code)
  peers_neighbours_iso2c <- countrycode(peers_neighbours_iso3c, origin = 'iso3c', destination = 'iso2c')
  
  return(list(country_iso2c = country_iso2c, country_iso3c = country_iso3c, peers_default_iso2c = peers_default_iso2c, 
              peers_default_iso3c = peers_default_iso3c, peers_neighbours_iso2c = peers_neighbours_iso2c, 
              peers_neighbours_iso3c = peers_neighbours_iso3c, regions = groupsdata))
}


####### Function to get plot schedule

getPlotSchedule <- function(plotparam_fname, dict) {
  
  graphplan <- read_excel(plotparam_fname, sheet = "library", col_names = T, skip=1) %>% mutate(source_name=NA)
  
  for (i in 1:dim(graphplan)[1]) {
    a <- unlist(str_extract_all( string = graphplan$indicators[i], pattern = paste(na.omit(dict$indicator_code), collapse = "|") ))
    a <- plyr::mapvalues(a, from = dict$indicator_code, to = dict$source_name, warn_missing = F)
    a <- unlist(strsplit(a, ", "))
    a <- a[a!="расчеты АКРА"]
    a <- c(unique(a), "расчеты АКРА")
    graphplan$source_name[i] = toString(a)
  }  
  
  return(graphplan)
  
}

####### Functions to check the plot schedule
checkGraphTypes <- function(graphplan, graph_types) {
  
  graphplan <- graphplan %>% mutate(check_types = 1)
  for (i in 1:dim(graphplan)[1]) {
    
    if(graphplan$graph_type[i] %in% graph_types) {} else {graphplan$check_types[i] = 0}
        
  }
  
  return(graphplan)
  
}


checkUnique <- function(graphplan) {
  
  graphplan <- graphplan %>% mutate(check_unique = 1)
  
}


checkAvailability <- function(graphplan, dict) {
  
  graphplan <- graphplan %>% mutate(check_availability = 0)
  for (i in 1:dim(graphplan)[1]) {
    
    needed <- graphplan$indicators[i] %>% str_split(", ") %>% '[['(1)
    available <- D$dict %>% filter(source_frequency == graphplan$data_frequency[i]) %>% pull(indicator_code)
    if(all(needed %in% available)) {graphplan$check_availability[i] = 1}
    
  }
  
  return(graphplan)
  
}

####### Function to generate plot sources


