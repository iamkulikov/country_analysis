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


####### Import filled data

importFilledData <- function(data_fname) {
  
  for (i in c("y", "q", "m")) {
    eval(parse(text = glue("ncols <- length(read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, n_max = 0))") ))
    eval(parse(text = glue("extdata_{i} <- read_excel(data_fname, sheet = '{i}', col_names = T, skip=0, \\
                            col_types = c('text', 'text', rep('numeric', ncols-2)))") ))
  }
  
  ncols <- length(read_excel(data_fname, sheet = "d", col_names = T, skip=0, n_max = 0))
  extdata_d <- read_excel(data_fname, sheet = "d", col_names = T, skip=0,
                          col_types = c("text", "text", "date", rep("numeric", ncols-3)))
  
  extdata_y <- extdata_y %>% mutate(time=year-1987)
  extdata_q <- extdata_q %>% mutate(time=(year-1987)*4+quarter)
  extdata_m <- extdata_m %>% mutate(time=(year-1987)*12+month)
  #extdata_d <- extdata_d %>% mutate(time=year-1987)
  
  ncols <- length(read_excel(data_fname, sheet = "dict", col_names = T, skip=0, n_max = 0))
  dict <- read_excel(data_fname, sheet = "dict", col_names = T, skip=0,
                     col_types = rep("text", ncols))
  
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict))
  
}


####### Import filled data