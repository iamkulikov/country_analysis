###### Load libraries
library_names <- c("dplyr","reshape2","ggplot2","ggthemes","countrycode","readxl","tidyr","data.table","writexl","unikn",
                   "ggtext","svglite","stringr","directlabels","fanplot","ggfan","hrbrthemes","glue","readr")

for (library_name in library_names) {
  #library(library_name, character.only = TRUE)
  eval(parse(text = paste0("require(", library_name, ")") ))
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
    available <- dict %>% filter(source_frequency == graphplan$data_frequency[i]) %>% pull(indicator_code)
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
        } else {time_start <- NA; time_end <- NA; timetony_start <- NA; timetony_end <- NA; labfreq <- NA}
        
        y_min <- as.numeric(y_min)
        y_max <- as.numeric(y_max)
        if (is.na(sec_y_axis) == F) {
          indicators_sec <- unlist(strsplit(sec_y_axis, ", "))
          coeff <- as.numeric(tail(indicators_sec, 1))
          indicators_sec <- head(indicators_sec, -1)
        } else {indicators_sec <- NA; coeff <- NA}
        
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
                x_max = x_max, y_max = y_max, data_frequency = data_frequency, time_start = time_start, labfreq = labfreq,
                time_end = time_end, timetony_start = timetony_start, trend_type = trend_type, graph_name = graph_name,
                timetony_end = timetony_end, time_fix = time_fix, time_fix_label = time_fix_label, indicators_sec = indicators_sec, 
                peers = peers, x_lab = x_lab, y_lab = y_lab, caption = caption, title = title, theme = theme, all = all,
                width = width, height = height, sec_y_axis = sec_y_axis, coeff = coeff, index = index))
        
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


####### Function to subset data for the particular graph

subsetData <- function(data, graph_params, country, peers) {
  
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
  if (all==1) {alpha=1} else {alpha=0}
  
  eval(parse(text = paste("theplot <- ggplot(data = data_all, aes(", x_ind, ",", y_ind, "))", sep="") ))
  theplot <- theplot + geom_point(alpha=alpha, fill="#619cff", color=ACRA['green']) +
    scale_x_continuous(limits=c(x_min, x_max)) + scale_y_continuous(limits=c(y_min, y_max)) +
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
  theplot <- theplot + theme(plot.title = element_textbox_simple())
  
  return(theplot)
  
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
  theplot <- theplot + scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6')]), name="", labels=y_lab) +
    labs(x=NULL, y = ifelse(length(indicators)==1, y_lab, ""), caption = caption) + ggtitle(title) +
    guides(size="none") + scale_size_manual(values = c(0,1)) +
    theme(plot.title = element_textbox_simple(), legend.position="bottom")
  
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  if (all==1) {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
  }  else {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))
  }
  
  return(theplot)
  
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
  theplot <- theplot + theme(plot.title = element_textbox_simple(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  return(theplot)
  
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
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6')]), name="", labels=y_lab) +
    ggtitle(title) + labs(caption = caption, x=NULL, y=NULL)
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), legend.position="bottom")
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  return(theplot)
  
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
    geom_dl(aes(label = indicator, colour = variable), method = list(dl.trans(x = x + 0.2), "extreme.grid", cex = 0.8, alpha=0.8))
  
  eval(parse(text = paste("theplot <- theplot + ", theme, sep="") ))
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), plot.margin = margin(r = 100)) +
    labs(caption = caption, x=NULL, y=NULL) + guides(colour = "none")
  
  return(theplot)
  
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
  if (!is.na(index)) { data_all <- data_all %>% group_by(variable, country_id) %>% mutate(value = value/first(value)) %>% ungroup() }
  
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
  if (!is.na(index)) { y_lab <- paste(y_lab, ", index: ", x_min, " = 1", sep = "" ) }
  
  theplot <- theplot + ggtitle(title) + theme(plot.title = element_textbox_simple()) +
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none") +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['dark'], alpha=1)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['dark'], alpha=1))
  
  return(theplot)
  
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
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none")
  
  return(theplot)
  
}
