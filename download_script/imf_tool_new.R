library(rsdmx)

flowref <- 'IMF.STA,CPI'
filter <- '...IX.M'
dataset <- as.data.frame(readSDMX(providerId = 'IMF_DATA', resource = 'data', flowRef = flowref, key = filter))