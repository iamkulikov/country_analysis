###### Load libraries and fonts
library_names <- c("dplyr","reshape2","ggplot2","ggthemes","countrycode","readxl","tidyr","data.table","writexl","unikn",
                   "ggtext","svglite","stringr","directlabels","fanplot",
                   #"ggfan",
                   "hrbrthemes","glue","readr", "showtext")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

font_add_google("Nunito Sans", regular.wt = 400, bold.wt = 700)
showtext_opts(dpi = 150)
showtext_auto()

###### Define custom color palettes and modifying themes

## ACRA palette
ACRA <- newpal( col = c(rgb(147, 202, 116, maxColorValue = 255),rgb(153, 38, 115, maxColorValue = 255),
                        rgb(238, 108, 64, maxColorValue = 255),rgb(155, 155, 155, maxColorValue = 255),
                        rgb(238, 162, 53, maxColorValue = 255),rgb(55, 165, 188, maxColorValue = 255),
                        rgb(69, 159, 122, maxColorValue = 255),rgb(115, 144, 159, maxColorValue = 255),
                        rgb(115, 83, 116, maxColorValue = 255),rgb(60, 100, 162, maxColorValue = 255),
                        rgb(63, 133, 165, maxColorValue = 255),rgb(220, 73, 66, maxColorValue = 255),
                        rgb(225, 225, 25, maxColorValue = 255),rgb(145, 30, 180, maxColorValue = 255),
                        rgb(230, 25, 75, maxColorValue = 255),rgb(70, 240, 240, maxColorValue = 255),
                        rgb(240, 50, 230, maxColorValue = 255), rgb(0, 0, 0, maxColorValue = 255),
                        rgb(139, 69, 19, maxColorValue = 255), rgb(255, 0, 0, maxColorValue = 255),
                        rgb(240, 179, 35, maxColorValue = 255)),
                names = c("green", "dark", "red", "grey", "sec1", "sec2", "sec3",
                          "sec4", "sec5", "sec6", "sec7", "sec8", "add1", "add2", 
                          "add3", "add4", "add5", "black", "brown", "reddest", "orange")
)

ipsum_theme <- function(base_size = 12, base_family = "Nunito Sans") {
  theme_ipsum() + theme( axis.line.x = element_line(color = "black", size = 3) )
}

# seecol(ACRA,
#       col_brd = "white", lwd_brd = 4,
#       title = "Colours of ACRA",
#       mar_note = "For fuck's sake")

#using +scale_color_manual(values = ACRA)


####### Function to import filled data

importData <- function(data_y_fname, data_q_fname, data_m_fname, data_d_fname, dict_fname, path) {
  
  for (i in c("y", "q", "m", "d")) {
    eval(parse(text = glue("ncols <- length(read_csv2(here(path, data_{i}_fname), col_names = T, skip=0, n_max = 0, na = ''))") ))
    if (i == "d") {types <- paste0('cc?', strrep('d', ncols-3))} else {
      types <- paste0('cc', strrep('d', ncols-2)) }
    eval(parse(text = glue("extdata_{i} <- read_csv2(here(path, data_{i}_fname), col_names = T, col_types = types, na = '' )") ))
  }
  
  dict <- read_csv2(here(path, dict_fname), col_names = T, skip=0, na = '')
  extdata_d <- extdata_d %>% mutate(date = as.Date(date))
  
  
  extdata_y <- extdata_y %>% mutate(time=year-1987)
  extdata_q <- extdata_q %>% mutate(time=(year-1987)*4+quarter)
  extdata_m <- extdata_m %>% mutate(time=(year-1987)*12+month)
  #extdata_d <- extdata_d %>% mutate(time=year-1987)
  
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict))
  
}


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
  
  n_groups = 12
  
  groupsdata <- t(read_excel(peers_fname, sheet = "groups", col_names = T, n_max = n_groups))
  labels <- c("region", groupsdata[3,-1])
  groupsdata <- data.frame(groupsdata[-c(1:3),])
  colnames(groupsdata) <- labels
  country_iso3c <- groupsdata %>% filter(rownames(groupsdata) == country_name) %>% pull(country_code)
  country_iso2c <- countrycode(country_iso3c, origin = 'iso3c', destination = 'iso2c')
  groupsdata <- groupsdata %>% select(-c(region)) %>% 
    mutate(country_iso2c = countrycode(country_code, origin = 'iso3c', destination = 'iso2c')) %>%
    rename('country_iso3c' = 'country_code') %>% as_tibble
  
  peersdata <- read_excel(peers_fname, sheet = "groups", col_names = T, skip=n_groups)
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
  graphplan <- graphplan %>% mutate(check_types = 1*(graph_type %in% graph_types))
  return(graphplan)
}

checkFreq <- function(graphplan) {
  graphplan <- graphplan %>% mutate(check_freq = 1*(data_frequency %in% c("y", "q", "m", "d") | is.na(data_frequency)))
  return(graphplan)
}

checkUnique <- function(graphplan) {
  graphplan <- graphplan %>% mutate(check_unique = 1)
  n_graphs <- graphplan %>% count(graph_name)
  graphplan <- graphplan %>% left_join(n_graphs, by=c("graph_name"="graph_name")) %>% 
    mutate(check_unique = (n > 1)*0 + (n==1)*1) %>% select(-c(n))
  return(graphplan)
}

checkAvailability <- function(graphplan, dict) {
  graphplan <- graphplan %>% mutate(check_availability = 0)
  for (i in 1:dim(graphplan)[1]) {
    needed <- graphplan$indicators[i] %>% str_split(", ") %>% '[['(1)
    available <- dict %>% filter(source_frequency == graphplan$data_frequency[i]) %>% pull(indicator_code)
    if(all(needed %in% available)) {graphplan$check_availability[i] = 1}
  }
  return(graphplan)
}

checkPeers <- function(graphplan, peer_groups, dict) {
  graphplan <- graphplan %>% mutate(check_peers = ifelse(is.na(peers) |
                    peers %in% head(names(peer_groups), -2) |
                    peers %in% c("0", "default", "neighbours") |
                    peers %in% c("0", "default", "neighbours")
                                      ,1,0))
  
  for (i in seq_along(graphplan$graph_name)) {
    available <- dict %>% filter(source_frequency == graphplan$data_frequency[i]) %>% pull(indicator_code)
    if (str_count(graphplan$peers[i], ":") == 1 & str_count(graphplan$peers[i], ",") >= 2 ) {
      peers_type <- unlist(strsplit(graphplan$peers[i], ": |:"))
      peers_def <- unlist(strsplit(peers_type[2], ", |,"))
      if ( (peers_type[1] == "custom" & all(peers_def %in% peer_groups$country_iso2c)) |
        (peers_type[1] == "similar" & peers_def[1] %in% available & is_numeric(peers_def[2]) & peers_def[2] <=1 &
           peers_def[2] > 0 & is_numeric(peers_def[3]) & peers_def[3] >= 1980 & peers_def[3] < 2100 ) |
        (peers_type[1] %in% c("low", "top") & peers_def[1] %in% available & is_numeric(peers_def[2]) &
           peers_def[2] > 0 & is_numeric(peers_def[3]) & peers_def[3] >= 1980 & peers_def[3] < 2100 ) ) {
        graphplan$check_peers[i] <- 1
      }
    }
  }
  
  return(graphplan)
}

checkTimes <- function(graphplan) {
  graphplan <- graphplan %>% mutate(check_times = 0)
  for (i in 1:dim(graphplan)[1]) {
    if (graphplan$graph_type[i] %in% c("structure_dynamic", "bar_dynamic", "lines_country_comparison", "lines_indicator_comparison", 
                                       "distribution_dynamic")) {
      if (isTime(graphplan$x_min[i]) & isTime(graphplan$x_max[i])) graphplan$check_times[i] <- 1
    }
    
    if (graphplan$graph_type[i] %in% c("scatter_country_comparison", "structure_country_comparison", "structure_country_comparison_norm", 
                                       "bar_country_comparison", "bar_country_comparison_norm","bar_year_comparison",
                                       "bar_year_comparison", "scatter_dynamic")) {
      if ((is.na(graphplan$x_min[i]) | is_numeric(graphplan$x_min[i])) & (is.na(graphplan$x_max[i]) | is_numeric(graphplan$x_max[i])) & isTime(graphplan$time_fix[i])) graphplan$check_times[i] <- 1
    }
  }
  return(graphplan)
}

is_numeric <- function(text_number) {suppressWarnings(!is.na(as.numeric(text_number)))}

isTime <- function(text_date) {
  if (is.na(text_date)) return(T)
  text_split <- unlist(strsplit(text_date, ", |,"))
  check <- rep(0, length(text_split))
  for (i in seq_along(text_split)) {
    if (grepl(pattern = "[\\.]", x = text_split[i])) {check[i] <- !is.na(as.Date(text_split[i], "%d.%m.%Y")) } else {
      text_split2 <- unlist(strsplit(text_split[i], "q|m"))
      text_split2 <- text_split2[order(-nchar(text_split2))]
      text_extract <- str_remove_all(text_split[i], "[:digit:]")
      check[i] <- (str_count(text_extract, "q|m|Q|M") <= 1)&(!str_detect(text_extract, "[^mqMQ]"))&(length(text_split2)<=2)&
        (str_detect(text_split2[[1]][1], "^(19|20)\\d{2}$"))
    }
  }
  return(all(check==1))
}

checkBinaryParams <- function(graphplan) {
  for (i in c("all",	"x_log",	"y_log", "index",	"recession", "swap_axis",	"long_legend",	"vert_lab",	"short_names", "show_title",	"active")) {
      eval(parse(text = glue("graphplan <- graphplan %>% mutate(bin_{i} <- 1*({i} == 0 | {i} == 1 | is.na({i})))") ))
    }
  graphplan <- graphplan %>% rowwise() %>% mutate(check_binary = prod(c_across(starts_with("bin_")))) %>% select(-starts_with("bin_"))
  return(graphplan)
}

checkNumericParams <- function(graphplan) {
  graphplan <- graphplan %>% mutate(check_num = 1*(!is.na(as.numeric(y_min)) | is.na(y_min))*(!is.na(as.numeric(y_max)) | is.na(y_max)) )
  return(graphplan)
}

checkTrend <- function(graphplan, trend_types) {
  graphplan <- graphplan %>% mutate(check_trend = 1*(trend_type %in% trend_types | is.na(trend_type)))
  return(graphplan)
}

checkTheme <- function(graphplan, theme_types) {
  graphplan <- graphplan %>% mutate(check_theme = 1*(theme %in% theme_types | is.na(theme)))
  return(graphplan)
}

checkOrientation <- function(graphplan, orient_types) {
  graphplan <- graphplan %>% mutate(check_orient = 1*(orientation %in% orient_types | is.na(orientation)))
  return(graphplan)
}


####### Function to generate text from the error report table

explainErrors <- function(error_report) {
  if (dim(error_report)[1] == 0) {print("No errors")} else {
    
    print("The plan cannot be executed. Please check following issues.")
    counter <- 0
    
    if (dim({error_report %>% filter(check_types == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown graph types in:   {error_report %>% filter(check_types == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
    
    if (dim({error_report %>% filter(check_freq == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown data frequency in:   {error_report %>% filter(check_freq == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
    
    if (dim({error_report %>% filter(check_unique == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Duplicating graph names:   {error_report %>% filter(check_unique == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
    
    if (dim({error_report %>% filter(check_availability == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Some indicators are not available in the database. Check their codenames in:   {error_report %>% filter(check_availability == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
 
    if (dim({error_report %>% filter(check_peers == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown peers in:   {error_report %>% filter(check_peers == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
    
    if (dim({error_report %>% filter(check_times == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown bounds or time formats. Check x_min, x_max or time_fix in:   {error_report %>% filter(check_times == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }
    
    if (dim({error_report %>% filter(check_binary == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Non-binary parameters. Check all, x_log, y_log, index, recession, swap_axis, long_legend, vert_lab, short_names, show_title and active in:   {error_report %>% filter(check_binary == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }   
    
    if (dim({error_report %>% filter(check_num == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Non-numeric parameters. Check y_min and y_max in:   {error_report %>% filter(check_num == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }   
    
    if (dim({error_report %>% filter(check_trend == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown trend types in:   {error_report %>% filter(check_trend == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }   
    
    if (dim({error_report %>% filter(check_theme == 0)})[1] > 0) {
      print(glue("Unknown theme types in:   {error_report %>% filter(check_theme == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }   
    
    if (dim({error_report %>% filter(check_orient == 0)})[1] > 0) {
      print(glue("{counter <- counter + 1; counter}. Unknown orientation in:   {error_report %>% filter(check_orient == 0) %>% pull(graph_name) %>% paste(., collapse = ', ')}")) }   

  }
}

####### Function to generate data-independent graph parameters

parseGraphPlan <- function(graphrow, dict, horizontal_size, vertical_size) {
  
  ###### Fix graph parameters
  for (j in seq_along(graphrow)) { eval(parse(text = paste0(names(graphrow)[j], " <- graphrow$", names(graphrow)[j]) )) }
  #    show_title <- 0   
  
  ## fill NAs with defaults if needed
  if (is.na(graph_group)) {graph_group <- "other"}
  if (is.na(data_frequency)) {data_frequency <- "y"}
  if (is.na(peers)) {peers <- "default"}
  if (is.na(theme)) {theme <- "ipsum"}
  if (is.na(orientation)) {orientation <- "horizontal"}
  for (i in c("all",	"x_log",	"y_log", "index",	"recession", "swap_axis",	"long_legend",	"vert_lab",	"short_names", "show_title",	"active")) {
    eval(parse(text = glue("if (is.na({i})) {i}<-0") ))
  }
  
  ## convert dates so that they correspond to the graph's intended frequency
  if (graph_type %in% c("scatter_country_comparison", "structure_country_comparison", "structure_country_comparison_norm", 
                        "bar_country_comparison", "bar_country_comparison_norm","bar_year_comparison",
                        "bar_year_comparison", "scatter_dynamic") & !is.na(time_fix)) {time_fix <- prepareDates(time_fix, freq = data_frequency, end = 1)}
  if (graph_type %in% c("structure_dynamic", "bar_dynamic", "lines_country_comparison", "lines_indicator_comparison", 
                        "distribution_dynamic")) {
        if (!is.na(x_min)) x_min <- prepareDates(x_min, freq = data_frequency, end = 0)
        if (!is.na(x_max)) x_max <- prepareDates(x_max, freq = data_frequency, end = 1)
  }
  
  ## fix indicators
  indicators <- unlist(strsplit(indicators, ", "))
  x_ind <- indicators[1]
  y_ind <- indicators[2]
 
  ## fix y limits
  y_min <- as.numeric(y_min)
  y_max <- as.numeric(y_max)
  if (is.na(sec_y_axis) == F) {
    indicators_sec <- unlist(strsplit(sec_y_axis, ", "))
    coeff <- as.numeric(tail(indicators_sec, 1))
    indicators_sec <- head(indicators_sec, -1)
  } else {indicators_sec <- NA; coeff <- NA}
  
  ## theme and labs calculation
  if (show_title == 1) { title <- graph_title } else {title <- ""} 
  caption <- paste("Источники:", source_name)
  theme <- paste("theme_", theme, "()", sep="")
  if (orientation=="vertical") {width = vertical_size[1]; height = vertical_size[2]} else {
    width = horizontal_size[1]; height = horizontal_size[2]}
  
  return(list(indicators = indicators, graph_type = graph_type, x_ind = x_ind, y_ind = y_ind, x_min = x_min, 
              y_min = y_min, x_max = x_max, y_max = y_max, data_frequency = data_frequency,
              trend_type = trend_type, graph_name = graph_name, time_fix = time_fix, indicators_sec = indicators_sec, 
              peers = peers, caption = caption, title = title, theme = theme, all = all, x_log = x_log, y_log = y_log,
              width = width, height = height, sec_y_axis = sec_y_axis, coeff = coeff, index = index))
  
}

####### Functions to recode dates based on the graph intended frequency

numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}


prepareDates <- function(datetext, freq, end) {
  
  dates <- unlist(strsplit(datetext, ", "))
  converted_date <- rep(0, length(dates))
  oldfreq <- rep("a", length(dates))
  
  for (i in seq_along(dates)) {
    
    datevec <- unlist(strsplit(dates[i], "d|m|q|y|\\."))
    datevec_sorted <- datevec[order(-nchar(datevec))]
    oldfreq[i] <- gsub(pattern = "[^a-zA-Z]", replacement = "", x = dates[i])
    if (oldfreq[i] == "") {oldfreq[i] <- "y"}
    if (grepl(pattern = "[\\.]", x = dates[i])) {oldfreq[i] <- "d"}
    #print(oldfreq[i] == "")
    if (oldfreq[i] == freq) {converted_date[i] <- ifelse(oldfreq[i] != "d", paste0(datevec_sorted, collapse = freq), dates[i])}
    
    if (oldfreq[i] != freq) {
        if (freq == "y") {converted_date[i] <- datevec_sorted[1]}
        if (oldfreq[i] == "y") {
          if (freq == "q") converted_date[i] <- glue("{datevec_sorted[1]}{freq}{ifelse(end==0,1,4)}")
          if (freq == "m") converted_date[i] <- glue("{datevec_sorted[1]}{freq}{ifelse(end==0,1,12)}")
          if (freq == "d") converted_date[i] <- glue("{ifelse(end==0,'01','31')}.{ifelse(end==0,'01','12')}.{datevec_sorted[1]}")
          }
        if (freq < oldfreq[i] & oldfreq[i] != "y" & freq != "y") {
          if (oldfreq[i] == "q") month_calc <- (as.numeric(datevec_sorted[2])-1)*3+ifelse(end == 0, 1, 3) 
          if (freq == "m") { converted_date[i] <- glue("{datevec_sorted[1]}{freq}{month_calc}") } else {
            if (oldfreq[i] == "q") {
              max_day <- numberOfDays(as.Date(glue("01.{month_calc}.{datevec_sorted[1]}"), "%d.%m.%Y"))
              converted_date[i] <- glue("{ifelse(end==0,'01',max_day)}.{month_calc}.{datevec_sorted[1]}")
              }
            if (oldfreq[i] == "m") {
              max_day <- numberOfDays(as.Date(glue("01.{datevec_sorted[2]}.{datevec_sorted[1]}"), "%d.%m.%Y"))
              converted_date[i] <- glue("{ifelse(end==0,'01',max_day)}.{datevec_sorted[2]}.{datevec_sorted[1]}")
              }
          }
          
        }
        if (freq > oldfreq[i] & oldfreq[i] != "y" & freq != "y") {
          if (freq == "q") converted_date[i] <- 
              glue("{datevec_sorted[1]}{freq}{(as.numeric(ifelse(oldfreq[i] == 'm', datevec_sorted[2], datevec_sorted[3]))-1)%/%3+1}")
          if (freq == "m") converted_date[i] <- glue("{datevec_sorted[1]}{freq}{datevec_sorted[3]}")
        }
    }
      
  }
  
  return(paste0(converted_date, collapse = ", "))
  
}

#prepareDates(datetext = "2011", freq = "d", end = 1)


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
      eval(parse(text = paste( "peers_central <- data$extdata_y %>% filter(year == ", peers_year, ", country_id == country_info$country_iso2c) %>% pull(", peers_ind, ")" , sep="")  )) 
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
    return(peers_iso2c)
    
  }
  
}

####### Function to fill data-dependent graph parameters

fillGraphPlan <- function(parsedrow, data, country_code, peers_code){
  
  for (j in seq_along(parsedrow)) { eval(parse(text = paste0(names(parsedrow)[j], " <- parsedrow$", names(parsedrow)[j]) )) }
  
  ## subset data
  eval(parse(text = paste("data_temp <- data$extdata_", data_frequency, sep="") ))
  data_temp <- data_temp %>% select(any_of(c("year", "quarter", "month", "country", "country_id")), all_of(indicators))
  dict <- data$dict
  countries_needed <- c(country_code, peers_code)
  
  ## fix time limits for dynamic graphs (needs to leave unchanged simple x limits in case of non-dynamic)
  
  if (graph_type %in% c("bar_dynamic", "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic",
                        "structure_dynamic", "structure_dynamic_norm", "scatter_dynamic", "bar_year_comparison", 
                        "distribution_year_comparison")) {
  
        if (!is.na(x_min)) {
          x_min <- unlist(strsplit(as.character(x_min), "q|m|d" ))
          x_min <- as.numeric(x_min[order(-nchar(x_min))])
        } else {
          if (graph_type %in% c("bar_dynamic", "lines_indicator_comparison", "structure_dynamic", "structure_dynamic_norm", "scatter_dynamic", 
                                "bar_year_comparison", "distribution_year_comparison")) {           # at least one indicator (all - for structure)
            data_min <- data_temp %>% filter(country_id == country_code) %>% 
              filter(rowSums(!is.na(.[indicators])) >= ifelse(graph_type %in% c("structure_dynamic",
                                      "scatter_dynamic", "bar_year_comparison", "distribution_year_comparison"), length(indicators), 1)) %>% slice(1) }
          
          if (graph_type %in% c("lines_country_comparison", "distribution_dynamic")) {                  # at least one country (20 - for distrib)
            data_min <- data_temp %>% select(any_of(c("year", "quarter", "month", "country_id")), all_of(indicators[1])) %>% 
                filter(country_id %in% countries_needed) %>% 
                pivot_wider(names_from = country_id, values_from = all_of(indicators[1])) %>% 
                filter(rowSums(!is.na(.[countries_needed])) >= ifelse(graph_type == "lines_country_comparison", 1, 20)) %>% 
                slice(1) }
          
          if (dim(data_min)[1]==0) {x_min <- c(2030, 1); trend_type <- NA} else { 
            if (data_frequency=="y") {x_min <- data_min %>% pull(year) }
            if (data_frequency=="q") {x_min <- data_min %>% select(year, quarter) %>% unlist() }
            if (data_frequency=="m") {x_min <- data_min %>% select(year, month) %>% unlist()}
            #if (data_frequency=="d") {x_min <- data_min %>% select(year, day) %>% unlist()}
          }
          
        }
        
        if (!is.na(x_max)) {
          x_max <- unlist(strsplit(as.character(x_max), "q|m|d" ))
          x_max <- as.numeric(x_max[order(-nchar(x_max))])
        } else {
          if (graph_type %in% c("bar_dynamic", "lines_indicator_comparison", "structure_dynamic", "structure_dynamic_norm", "scatter_dynamic", 
                                "bar_year_comparison", "distribution_year_comparison")) {         # at least one indicator (all - for structure)
            data_max <- data_temp %>% filter(country_id == country_code) %>% 
              filter(rowSums(!is.na(.[indicators])) >= ifelse(graph_type %in% c("structure_dynamic",
                                  "scatter_dynamic", "bar_year_comparison", "distribution_year_comparison"), length(indicators), 1)) %>% slice(n()) }
          
          if (graph_type %in% c("lines_country_comparison", "distribution_dynamic")) {            # at least one country (20 - for distrib)
            data_max <- data_temp %>% select(any_of(c("year", "quarter", "month", "country_id")), all_of(indicators[1])) %>% 
              filter(country_id %in% countries_needed) %>% 
              pivot_wider(names_from = country_id, values_from = all_of(indicators[1])) %>% 
              filter(rowSums(!is.na(.[countries_needed])) >= ifelse(graph_type == "lines_country_comparison", 1, 20)) %>% 
              slice(n()) }
      
      if (dim(data_max)[1]==0) {x_max <- c(2030, 4); trend_type <- NA} else { 
        if (data_frequency=="y") {x_max <- data_max %>% pull(year) }
        if (data_frequency=="q") {x_max <- data_max %>% select(year, quarter) %>% unlist() }
        if (data_frequency=="m") {x_max <- data_max %>% select(year, month) %>% unlist()}
        #if (data_frequency=="d") {x_max <- data_max %>% select(year, day) %>% unlist()}
      }
  
    }
    
    if (data_frequency=="y") { time_start <- x_min[1]-1987 ; time_end <- x_max[1]-1987; 
    labfreq <- 1; timetony_start <- 0; timetony_end <- 1}
    if (data_frequency=="q") { time_start <- (x_min[1]-1987)*4+x_min[2]; time_end <- (x_max[1]-1987)*4+x_max[2];
    labfreq <- 4; timetony_start <- (5-x_min[2])%%4; timetony_end <- x_max[2] }
    if (data_frequency=="m") { time_start <- (x_min[1]-1987)*12+x_min[2] ; time_end <- (x_max[1]-1987)*12+x_max[2];
    labfreq <- 12; timetony_start <- (13-x_min[2])%%12; timetony_end <- x_max[2] }
    #if (data_frequency=="d") { time_start <- (x_min[1]-1987)*365+x_min[2] ; time_end <- (x_max[1]-1987)*365+x_max[2];
    #labfreq <- 30; timetony_start <- (13-x_min[2])%%12; timetony_end <- x_min[2] }
    
  } else {time_start <- NA; time_end <- NA; timetony_start <- NA; timetony_end <- NA; labfreq <- NA}
  
  
  ## fix the time point for cross-section graphs
  
  if (graph_type %in% c("bar_year_comparison", "distribution_year_comparison", "scatter_country_comparison", "bar_country_comparison", 
                        "structure_country_comparison", "structure_country_comparison_norm", "triangle")) {
        
        time_fix_label <- time_fix
        
        if (!is.na(time_fix)) { 
          
          if  (graph_type %in% c("bar_year_comparison", "distribution_year_comparison")) {
            time_fix <- unlist(strsplit(as.character(time_fix), ", " ))
            }
          
          if (graph_type %in% c("scatter_country_comparison", "bar_country_comparison", "structure_country_comparison",
                                "structure_country_comparison_norm", "triangle")) {
            time_fix <- unlist(strsplit(as.character(time_fix), "q|m|d" ))
            time_fix <- as.numeric(time_fix[order(-nchar(time_fix))])
          }
        
        } else {
          
          if  (graph_type %in% c("bar_year_comparison", "distribution_year_comparison")) {
                        time_fix <- c(x_min, x_max)
          }
          
          if (graph_type %in% c("scatter_country_comparison", "bar_country_comparison", "structure_country_comparison",
                                "structure_country_comparison_norm", "triangle")) {           # all indicators for the main country
            
              data_fix <- data_temp %>% select(any_of(c("year", "quarter", "month", "country_id")), all_of(indicators)) %>% 
                filter(country_id %in% country_code) %>% 
                filter(rowSums(!is.na(.[indicators])) == length(indicators) ) %>% 
                slice(n())
              
              if (dim(data_fix)[1]==0) {time_fix <- c(2030, 4); trend_type <- NA} else {
                
                if  (!(graph_type %in% c("bar_year_comparison", "distribution_year_comparison"))) {
                  if (data_frequency=="y") {time_fix <- data_fix %>% pull(year) }
                  if (data_frequency=="q") {time_fix <- data_fix %>% select(year, quarter) %>% unlist() }
                  if (data_frequency=="m") {time_fix <- data_fix %>% select(year, month) %>% unlist()}
                  #if (data_frequency=="d") {time_fix<- data_fix %>% select(year, day) %>% unlist()} 
                  }
              }
          }
        }
        
        if  (!(graph_type %in% c("bar_year_comparison", "distribution_year_comparison"))) {
          if (data_frequency=="y") { time_fix <- time_fix[1]-1987 }
          if (data_frequency=="q") { time_fix <- (time_fix[1]-1987)*4+time_fix[2] }
          if (data_frequency=="m") { time_fix <- (time_fix[1]-1987)*12+time_fix[2] }
        }
          
  } else {time_fix_label <- NA; time_fix <- NA}
  
  ## logs calculation 
  if (x_log==1) { x_lab <- paste0("log(", dict[dict$indicator_code==x_ind & dict$source_frequency==data_frequency,1], "), ", time_fix_label); 
  x_ind <- paste0("log(", x_ind, ")") } else {
    if (!is.na(time_fix_label)) {
      x_lab <- paste0(dict[dict$indicator_code==x_ind & dict$source_frequency==data_frequency,1], ", ", time_fix_label)
    } else {x_lab <- dict[dict$indicator_code==x_ind & dict$source_frequency==data_frequency,1]}
  }
  if (y_log==1) { y_lab <- paste0("log(", dict[dict$indicator_code==y_ind & dict$source_frequency==data_frequency,1], "), ", time_fix_label);
  y_ind <- paste0("log(", y_ind, ")") } else {
    if (!is.na(time_fix_label)) {
      y_lab <- paste0(dict[dict$indicator_code==y_ind & dict$source_frequency==data_frequency,1], ", ", time_fix_label) 
    } else {y_lab <- dict[dict$indicator_code==y_ind & dict$source_frequency==data_frequency,1] }
  }
  
  return(list(indicators = indicators, graph_type = graph_type, x_ind = x_ind, y_ind = y_ind, x_min = x_min, y_min = y_min, 
              x_max = x_max, y_max = y_max, data_frequency = data_frequency, time_start = time_start, labfreq = labfreq,
              time_end = time_end, timetony_start = timetony_start, trend_type = trend_type, graph_name = graph_name,
              timetony_end = timetony_end, time_fix = time_fix, time_fix_label = time_fix_label, indicators_sec = indicators_sec, 
              peers = peers, x_lab = x_lab, y_lab = y_lab, caption = caption, title = title, theme = theme, all = all,
              width = width, height = height, sec_y_axis = sec_y_axis, coeff = coeff, index = index))
  
}


####### Function to subset data for the particular graph

subsetData <- function(data, graph_params, country_code, peers_code) {
  
  eval(parse(text= paste("data_temp <- data$extdata_", graph_params$data_frequency, sep="") ))
  return(list(extdata = data_temp, dict = data$dict))
  
}


####### Function to choose the needed function based on the graph type (simply transforming the name) 

funcNameTransform <- function(graph_type) {
  
  # Split the input string by underscores
  words <- strsplit(graph_type, "_", fixed = TRUE)[[1]]
  
  # Capitalize the letters after underscore
  words <- sapply(words, function(word) {
    if (nchar(word) > 1) {
      paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
    } else {
      word
    }
  })
  
  # Combine the words back into a single string without underscores
  output_string <- paste(words, collapse = "")
  output_string <- paste0(tolower(substr(output_string, 1, 1)), substr(output_string, 2, nchar(output_string)))
  return(output_string)
  
}


###### Scatter plot

scatterCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {

  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_all <- data$extdata %>% filter(time==time_fix) %>% select("country", "country_id", "year", all_of(indicators))
  data_highlight <- data_all %>% filter(country_id==country_iso2c)
  if (peers!=0) {data_peers <- data_all %>% filter(country_id %in% peers_iso2c)} else {data_peers <- NULL}
  x_min <- as.numeric(x_min); x_max <- as.numeric(x_max); y_min <- as.numeric(y_min); y_max <- as.numeric(y_max)
  
  eval(parse(text = paste("theplot <- ggplot(data = data_all, aes(", x_ind, ",", y_ind, "))", sep="") ))
  theplot <- theplot + geom_point(alpha=ifelse(all==1, 1, 0), fill="#619cff", color=ACRA['green'])
  theplot <- theplot + scale_x_continuous(limits=c(x_min, x_max)) + scale_y_continuous(limits=c(y_min, y_max)) +
    geom_smooth(method = trend_type) + ggtitle(title) + labs(x = x_lab, y = y_lab, caption = caption)
  
  eval(parse(text = paste("theplot <- theplot + geom_point(data=data_peers, aes(", x_ind, ", ", y_ind, "), color=ACRA['sec2'], alpha=1, size=3)", sep="") ))
  eval(parse(text = paste("theplot <- theplot + geom_point(data=data_highlight, aes(", x_ind, ", ", y_ind, "), color=ACRA['dark'], alpha=1, size=3)", sep="") ))
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  theplot <- theplot + geom_text(data=data_peers, aes(label = country_id), 
                                 nudge_x = 0.03*(layer_scales(theplot)$x$range$range[2]-layer_scales(theplot)$x$range$range[1]), 
                                 nudge_y = 0.03*(layer_scales(theplot)$y$range$range[2]-layer_scales(theplot)$y$range$range[1]), 
                                 check_overlap = F, colour = ACRA['sec2']) +
    geom_text(data=data_highlight, aes(label = country_id), 
              nudge_x = 0.03*(layer_scales(theplot)$x$range$range[2]-layer_scales(theplot)$x$range$range[1]), 
              nudge_y = 0.03*(layer_scales(theplot)$y$range$range[2]-layer_scales(theplot)$y$range$range[1]), 
              check_overlap = F, colour=ACRA['dark'])
  theplot <- theplot + theme(plot.title = element_textbox_simple()) + 
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  
  return(list(graph = theplot, data = data_all)) 
  #return(data_all)
}


###### Bar country comparison - dodged, stacked or stacked and normalized

barCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  dict_temp <- data$dict %>% filter(indicator_code %in% indicators, source_frequency == data_frequency) %>% 
    arrange(factor(indicator_code, levels = indicators)) %>% select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  data_temp <- data$extdata %>% select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators))) 
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, time==time_fix, !is.na(value)) %>%
    mutate(highlight2 = as.factor(ifelse(country_id==country_iso2c,1,0))) 
  
  if (length(indicators)==1) {data_all <- data_all %>% mutate(highlight = as.factor(case_when(country_id %in% peers_iso2c ~ 1,
                                                                                              country_id %in% country_iso2c ~ 2, TRUE ~ 0)) )}
  if (is.na(sec_y_axis)==F) { data_all <- data_all %>% mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  
  if (all==1) {} else {data_all <- data_all %>% filter(country_id %in% c(peers_iso2c, country_iso2c)) }
  
  ordering_table <- data_all %>% filter(variable == indicators[1]) %>% select(country_id, value) %>% arrange(desc(value)) %>% 
    mutate(ordering = row_number()) %>% select(country_id, ordering)
  data_all <- data_all %>% left_join(ordering_table, by="country_id") %>% arrange(desc(variable), ordering)
  data_all <- data_all %>% mutate(variable = factor(variable, levels = indicators))
  
  if (length(indicators)==1) {theplot <- ggplot(data_all, aes(reorder(country_id, -value), value, fill = highlight)) } else 
  {theplot <- ggplot(data_all, aes(reorder(country_id, ordering), value, fill = variable)) }
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  
  theplot <- theplot + geom_col(aes(size=highlight2), color="black", alpha=.8,
                                position=case_when(graph_type=="bar_country_comparison" ~ "dodge", 
                                                   graph_type=="structure_country_comparison" ~ "stack",
                                                   graph_type=="structure_country_comparison_norm" ~ "fill"))
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  theplot <- theplot + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6','black','brown','sec5','sec7', 'sec8',
                                                'add1', 'add2', 'reddest', 'add4', 'add5')]), name="", labels=y_lab) +
    labs(x=NULL, y = ifelse(length(indicators)==1, y_lab, ""), caption = caption) + ggtitle(title) +
    guides(size="none") + scale_size_manual(values = c(0,1)) +
    theme(plot.title = element_textbox_simple(), legend.position="bottom") + 
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  if (all==1) {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
  }  else {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))
  }
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}

structureCountryComparison <- barCountryComparison
structureCountryComparisonNorm <- barCountryComparison


###### Bar multi-year comparison for multiple variables

barYearComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_all <- reshape2::melt(data$extdata, id.vars=c("country", "country_id", "year"), variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, year %in% time_fix, country_id == country_iso2c, !is.na(value)) %>% 
    arrange(desc(variable), desc(value))
  #x_lab <- unname(unlist(dict[dict$indicator_code %in% indicators & dict$source_frequency==data_frequency,1]))
  dict_temp <- data$dict %>% filter(indicator_code %in% indicators, source_frequency == data_frequency) %>% 
    arrange(factor(indicator_code, levels = indicators)) %>% select(indicator)
  x_lab <- unname(unlist(dict_temp))
  x_lab <- str_wrap(x_lab, width = 15)
  
  theplot <- ggplot(data_all, aes(variable, value, fill=as.factor(year))) + geom_col(alpha=.8, position ='dodge') +
    scale_y_continuous(limits=c(y_min, y_max)) + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6')]), name="Год" ) +
    scale_x_discrete(labels=x_lab) +
    ggtitle(title) + labs(caption = caption, x=NULL, y=NULL)
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  theplot <- theplot + theme(plot.title = element_textbox_simple(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}


###### Bar dynamic - dodged, stacked or stacked and normalized

barDynamic <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_temp <- data$extdata %>% select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, time >= time_start, time <= time_end, country_id == country_iso2c, !is.na(value)) 
  if (is.na(sec_y_axis)==F) { data_all <- data_all %>% mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  if (graph_type=="structure_dynamic") {data_total <- data_all %>% group_by(time) %>% summarize(total=sum(value)) %>% ungroup()}
  
  dict_temp <- data$dict %>% filter(indicator_code %in% indicators, source_frequency == data_frequency) %>% 
    arrange(factor(indicator_code, levels = indicators)) %>% select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  theplot <- ggplot(data_all, aes(time, value, fill=as.factor(variable)))
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  
  theplot <- theplot + scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                                          labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1])) 
  
  if (graph_type=="bar_dynamic") {theplot <- theplot + geom_col(alpha=.8, width=0.65, position = position_dodge(width = 0.65)) } else {
    theplot <- theplot + geom_col(alpha=.8, width=0.65, 
                                  position = case_when(graph_type=="structure_dynamic" ~ "stack", graph_type=="structure_dynamic_norm" ~ "fill"))
  }
  
  if (graph_type=="structure_dynamic") {theplot <- theplot + stat_summary(fun = sum, geom = "point",
                                                                          shape = 17, size = 2, mapping = aes(group = time), show.legend = F)}
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  
  theplot <- theplot + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6','black','brown','sec5','sec7', 'sec8',
                                                'add1', 'add2', 'reddest', 'add4', 'add5')]), name="", labels=y_lab) +
    ggtitle(title) + labs(caption = caption, x=NULL, y=NULL) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), legend.position="bottom")
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}

structureDynamic <- barDynamic
structureDynamicNorm <- barDynamic


###### Lines indicator comparison

linesIndicatorComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  dict_temp <- data$dict %>% filter(indicator_code %in% indicators, source_frequency == data_frequency) %>% 
    arrange(factor(indicator_code, levels = indicators)) %>% select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  data_temp <- data$extdata %>% select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, time >= time_start, time <= time_end, country_id == country_iso2c, !is.na(value)) 
  data_all <- data_all %>% left_join(data$dict, by=c("variable"="indicator_code")) %>% filter(source_frequency == data_frequency)
  if (is.na(sec_y_axis)==F) { data_all <- data_all %>% mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  
  theplot <- ggplot(data_all, aes(time, value)) + geom_line(aes(group=variable, color=variable), alpha=1, size=2)
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  
  theplot <- theplot + scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                                          labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1])) +
    scale_color_manual(values = as.vector(ACRA[c('dark','sec2','green','sec1','sec6')]) ) +
    ggtitle(title) + labs(caption = caption, x="Год", y=NULL) + 
    geom_dl(aes(label = indicator, colour = variable), method = list(dl.trans(x = x + 0.3), "extreme.grid", cex = 0.8, alpha=0.8))
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), plot.margin = margin(r = 15)) +
    labs(caption = caption, x=NULL, y=NULL) + guides(colour = "none") +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}


###### Lines country comparison

linesCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  y_ind <- x_ind
  y_lab <- x_lab
  
  data_temp <- data$extdata %>% select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, time >= time_start, time <= time_end)
  if (!(is.na(index)|index==0)) { data_all <- data_all %>% group_by(variable, country_id) %>% mutate(value = value/first(value)) %>% ungroup() }
  
  if (peers != 0) {data_peers <- data_all %>% filter(country_id %in% peers_iso2c)}
  data_country <- data_all %>% filter(country_id %in% country_iso2c)
  
  theplot <- ggplot(data_all, aes(time, value))
  
  if (all == 1) {theplot <- theplot + geom_line(data=data_all, aes(group=country), colour=ACRA['grey'], size = 0.7, alpha=.4)} 
  if (peers != 0) {theplot <- theplot + geom_line(data=data_peers, aes(group=country), colour=ACRA['sec2'], size = 2, alpha=.8)}
  
  theplot <- theplot + geom_line(data=data_country, aes(group=country), colour=ACRA['dark'], size = 3, alpha=.8) +
    scale_y_continuous(limits=c(y_min, y_max)) + 
    scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                       labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1]))
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  if (!(is.na(index)|index==0)) { y_lab <- paste(y_lab, ", index: ", x_min, " = 1", sep = "" ) }
  
  theplot <- theplot + ggtitle(title) + theme(plot.title = element_textbox_simple()) +
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none") +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['dark'], alpha=1)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['dark'], alpha=1)) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}


###### Distribution dynamics (fan plot)

distributionDynamic <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  y_ind <- x_ind
  y_lab <- x_lab
  
  data_temp <- data$extdata %>% select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all %>% filter(variable %in% indicators, time >= time_start, time <= time_end, !is.na(value)) %>%
    select(-c(variable))
  data_quant <- data_all %>% calc_quantiles(intervals=(1:19)/20, x_var="time", y_var="value", rename=F) %>%
    arrange(time, desc(quantile))
  data_country <- data_all %>% filter(country_id == country_iso2c) %>% select(time, value)
  data_quant <- data_quant %>% left_join(data_country, by=c("time"="time"), suffix=c("","_c"))
  
  theplot <- ggplot(data=data_quant, aes(time, value, quantile=quantile)) + geom_fan(intervals=c(seq(10,90, by=10))/100)
  theplot <- theplot + coord_cartesian(ylim=c(y_min, y_max))+scale_fill_gradient(low=ACRA['dark'], high=ACRA['green']) +
    scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                       labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1]))
  theplot <- theplot + geom_line(data=data_quant, aes(time, value_c), colour=ACRA['sec6'], size=2)
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  
  theplot <- theplot + ggtitle(title) + theme(plot.title = element_textbox_simple()) +
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none") +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (theme == "theme_ipsum") { theplot <- theplot + theme(text = element_text(family = "Nunito Sans")) }
  return(list(graph = theplot, data = data_all))
  
}
