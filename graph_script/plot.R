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


####### Function to get plot schedule and generate sources

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


checkPeers <- function(graphplan, peer_groups) {
  
  graphplan <- graphplan %>% mutate(check_peers = 1)
  
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

####### Function to generate all graph parameters

parseGraphPlan <- function(graphrow, dict, horizontal_size, vertical_size) {
  
        ###### Fix graph parameters
        for (j in seq_along(graphrow)) { eval(parse(text = paste0(names(graphrow)[j], " <- graphrow$", names(graphrow)[j]) )) }
        #    show_title <- 0   
        
        ## indicators and limits fixation
        indicators <- unlist(strsplit(indicators, ", "))
        x_ind <- indicators[1]
        y_ind <- indicators[2]
        
        x_min <- as.numeric(unlist(strsplit(x_min, "q|m|d" )))
        x_max <- as.numeric(unlist(strsplit(x_max, "q|m|d" )))
        
        if (graph_type %in% c("bar_dynamic", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic",
                              "structure_dynamic", "structure_dynamic_norm")) {
          if (data_frequency=="y") { time_start <- x_min[1]-1987 ; time_end <- x_max[1]-1987; 
          labfreq <- 1; timetony_start <- 0; timetony_end <- 1}
          if (data_frequency=="q") { time_start <- (x_min[1]-1987)*4+x_min[2]; time_end <- (x_max[1]-1987)*4+x_max[2];
          labfreq <- 4; timetony_start <- (5-x_min[2])%%4; timetony_end <- x_min[2] }
          if (data_frequency=="m") { time_start <- (x_min[1]-1987)*12+x_min[2] ; time_end <- (x_max[1]-1987)*12+x_max[2];
          labfreq <- 12; timetony_start <- (13-x_min[2])%%12; timetony_end <- x_min[2] }
          #if (data_frequency=="d") { time_start <- (x_min[1]-1987)*365+x_min[2] ; time_end <- (x_max[1]-1987)*365+x_max[2];
          #labfreq <- 30; timetony_start <- (13-x_min[2])%%12; timetony_end <- x_min[2] }   
        } else {time_start <- NA; time_end <- NA; timetony_start <- NA; timetony_end <- NA}
        
        y_min <- as.numeric(y_min)
        y_max <- as.numeric(y_max)
        if (is.na(sec_y_axis) == F) {
          indicators_sec <- unlist(strsplit(sec_y_axis, ", "))
          coeff <- as.numeric(tail(indicators_sec, 1))
          indicators_sec <- head(indicators_sec, -1)
        } else {indicators_sec <- NA}
        
        time_fix_label <- time_fix
        time_fix <- unlist(strsplit(time_fix, ", "))
        if (graph_type =="bar_year_comparison") { time_fix <- as.numeric(time_fix)} else 
        {time_fix <- as.numeric(unlist(strsplit(time_fix, "q|m|d" )))}
        if (graph_type %in% c("scatter_country_comparison", "bar_country_comparison", "structure_country_comparison",
                  "structure_country_comparison_norm")) {
          if (data_frequency=="y") { time_fix <- time_fix[1]-1987 }
          if (data_frequency=="q") { time_fix <- (time_fix[1]-1987)*4+time_fix[2] }
          if (data_frequency=="m") { time_fix <- (time_fix[1]-1987)*12+time_fix[2] }   
        }
        
        ## logs calculation 
        if (x_log==1) { x_lab <- paste0("log(", dict[dict$indicator_code==x_ind & dict$source_frequency==data_frequency,1], "), ", time_fix_label); 
        x_ind <- paste0("log(", x_ind, ")") } else {
          x_lab <- paste0(dict[dict$indicator_code==x_ind & dict$source_frequency==data_frequency,1], ", ", time_fix_label) }
        if (y_log==1) { y_lab <- paste0("log(", dict[dict$indicator_code==y_ind & dict$source_frequency==data_frequency,1], "), ", time_fix_label);
        y_ind <- paste0("log(", y_ind, ")") } else {
          y_lab <- paste0(dict[dict$indicator_code==y_ind & dict$source_frequency==data_frequency,1], ", ", time_fix_label) }
        
        ## theme and labs calculation
        if (show_title == 1) { title <- graph_title } else {title <- ""} 
        caption <- paste("Источники:", source_name)
        theme <- paste("theme_", theme, "()", sep="")
        if (orientation=="vertical") {width = vertical_size[1]; height = vertical_size[2]} else {
                width = horizontal_size[1]; height = horizontal_size[2]}
  
        return(list(indicators = indicators, graph_type = graph_type, x_ind = x_ind, y_ind = y_ind, x_min = x_min, y_min = y_min, 
               data_frequency = data_frequency, time_start = time_start, time_end = time_end, timetony_start = timetony_start, 
               timetony_end = timetony_end, time_fix = time_fix, time_fix_label = time_fix_label, indicators_sec = indicators_sec, 
               peers = peers, x_lab = x_lab, y_lab = y_lab, caption = caption, title = title, theme = theme, 
               width = width, height = height))
        
}


####### Function to fix peers for the particular graph

fixPeers <- function(country_info, peers, data) {
  
  if (peers != 0) {
    
    peers_vec <- unlist(strsplit(peers, ": "))
    peers_type <- peers_vec[1]
    peers_vec <- unlist(strsplit(peers_vec[2], ", "))
    
    if (peers_type=="custom") {peers_iso2c <- peers_vec}
    if (peers_type == "default") {peers_iso2c <- country_info$peers_default_iso2c}
    if (peers_type == "neighbours") {peers_iso2c <- country_info$peers_neighbours_iso2c}
    eval(parse(text = paste( "if (peers_type %in% names(country_info$regions)) {peers_iso2c <- country_info$regions %>% filter(", peers_type, " == 1) %>% pull(country_iso2c)}" , sep="")  )) 
    
    if (peers_type=="similar"|peers_type=="top"|peers_type=="low") {peers_ind <- peers_vec[1]; peers_param <- as.numeric(peers_vec[2]); peers_year <- as.numeric(peers_vec[3])}
    if (peers_type=="similar") {
      eval(parse(text = paste( "peers_central <- data$extdata_y %>% filter(year == ", peers_year, ", country_id == country_iso2c) %>% pull(", peers_ind, ")" , sep="")  )) 
      eval(parse(text=paste("peers_iso2c <- data$extdata_y %>% filter(year == peers_year, ", peers_ind,  " > peers_central*(1-peers_param), ",
                            peers_ind, " < peers_central*(1+peers_param)) %>% pull(country_id)" , sep="") ))
    }
    
    if (peers_type=="top") {
      eval(parse(text = paste("peers_iso2c <- data$extdata_y %>% filter(year == peers_year, !is.na(", peers_ind,
                              ")) %>% arrange(desc(", peers_ind, ")) %>% slice(1: peers_param) %>% pull(country_id)" , sep="")  )) 
    }
    
    if (peers_type=="low") {
      eval(parse(text = paste("peers_iso2c <- data$extdata_y %>% filter(year == peers_year, !is.na(", peers_ind,
                              ")) %>% arrange(", peers_ind, ") %>% slice(1:peers_param) %>% pull(country_id)" , sep="")  )) 
    }
    
    peers_iso2c <- peers_iso2c[peers_iso2c!=country_info$country_iso2c]
    
  }
  
  return(peers_iso2c)
  
}
