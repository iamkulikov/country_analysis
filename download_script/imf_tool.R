imfTool <- function(code, database, freq, start, end) {

  adres <- glue("http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/{database}/{freq}..{code}.?startPeriod={start}&endPeriod={end}")
  #print(adres)
  downloaded <- jsonlite::fromJSON(adres)
  obs <- downloaded$CompactData$DataSet$Series$Obs
  countries <- downloaded$CompactData$DataSet$Series$`@REF_AREA`

  if (database %in% c("PCPS","FDI","FM","GFSR","GFSCOFOG","GFSE","GFSMAB") | {database == "IFS" & freq != 'A'} ) {
    if (database %in% c("PCPS")) {df <- obs[[1]]} else {df <- obs}
    } else {
    df <- sapply(obs, as.data.table, simplify = T) }
  
  if (database =="PCPS") {a <- data.frame(df) %>% mutate(iso2c = "1W", X.OBS_VALUE = as.numeric(X.OBS_VALUE))} else {
  
    a <- NULL
    for (i in 1:length(df)) {
      #print(i)
      #print(data.frame(df[[i]]))
      #print(dim(df[[i]]))
      #dim(df[[i]])[1] != 0
      if ( is.null(dim(df[[i]])) | !{"X.OBS_VALUE" %in% names(data.frame(df[[i]]))} ) {} else {
        a <- data.frame(df[[i]]) %>% select(any_of(c("X.TIME_PERIOD", "X.OBS_VALUE"))) %>% 
            mutate(country_id = countries[i], X.OBS_VALUE = as.numeric(X.OBS_VALUE)) %>% rbind(a) 
      }
    }
  }
  
  eval(parse(text = glue("names(a) <- c('time', '{code}', 'iso2c')") ))
  return(a)
  
}

# imfTool(code = "NGDP_NSA_XDC", database = "IFS", freq = "Q", start = "2019", end = "2022")

# IFS

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/A..BGS_BP6_USD.?startPeriod=1987&endPeriod=2025"
# imfTool(code = "BGS_BP6_USD", database = "IFS", freq = "A", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/Q..NGDP_NSA_XDC.?startPeriod=1987&endPeriod=2025"
# imfTool(code = "NCP_NSA_XDC", database = "IFS", freq = "Q", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/IFS/M..PCPI_IX.?startPeriod=2011&endPeriod=2025"
# imfTool(code = "PCPI_IX", database = "IFS", freq = "M", start = "2011", end = "2022")
# imfTool(code = "ENEER_IX", database = "IFS", freq = "M", start = "2011", end = "2025")

# BOP

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/BOP/A..BGS_BP6_USD.?startPeriod=2000&endPeriod=2021"
# imfTool(code = "BGS_BP6_USD", database = "BOP", freq = "A", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/BOP/Q..BXGS_BP6_USD.?startPeriod=2020&endPeriod=2021"
# imfTool(code = "BGS_BP6_USD", database = "BOP", freq = "Q", start = "2019", end = "2022")

# FM

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/FM/A..G_XWDG_G01_GDP_PT.?startPeriod=1987&endPeriod=2022"
# imfTool(code = "G_XWDG_G01_GDP_PT", database = "FM", freq = "A", start = "1987", end = "2025")

# PCPS Primary commodity prices

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/A..PALLFNF.?startPeriod=2000&endPeriod=2022"
# imfTool(code = "PALLFNF", database = "PCPS", freq = "A", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/Q..PALLFNF.?startPeriod=2020&endPeriod=2022"
# imfTool(code = "PALLFNF", database = "PCPS", freq = "Q", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/PCPS/M..PALLFNF.?startPeriod=2021&endPeriod=2022"
# imfTool(code = "PALLFNF", database = "PCPS", freq = "M", start = "2019", end = "2022")

# FDI Financial Development Index

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/FDI/A..FD_FID_IX.?startPeriod=2000&endPeriod=2022"
# imfTool(code = "FD_FID_IX", database = "FDI", freq = "A", start = "2019", end = "2022")

# GFS

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/GFSR/A..S13.XDC_R_B1GQ.W0_S1_G111"
# imfTool(code = "S13.XDC.W0_S1_G1", database = "GFSR", freq = "A", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/GFSCOFOG/A..S13.XDC_R_B1GQ._T"
# imfTool(code = "S13.XDC_R_B1GQ._T", database = "GFSCOFOG", freq = "A", start = "2019", end = "2025")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/GFSE/A..S13.XDC_R_B1GQ.W0_S1_G21"
# imfTool(code = "S13.XDC_R_B1GQ.W0_S1_G21", database = "GFSE", freq = "A", start = "2019", end = "2022")
# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/GFSMAB/A..S13.XDC_R_B1GQ.G63_F3T4"
# imfTool(code = "S13.XDC_R_B1GQ.G63_F3T4", database = "GFSMAB", freq = "A", start = "2019", end = "2022")

#GFSR - Revenues
#GFSSSUC - Sources and Uses of Cash
#GFSCOFOG - Expenditure by Function of Government
#GFSFALCS - Financial Assets and Liabilities by Counterpart Sector
#GFSIBS - Integrated Balance Sheet
#GFSMAB - Main Aggregates and Balances
#GFSE - Expense

# adres <- "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/GFSR/0.A..S1311B.W0_S1_G11411.XDC.?startPeriod=2019&endPeriod=2022"


# Other

#COFER - Currency Composition of Official Foreign Exchange Reserves
#CPIS - Coordinated Portfolio Investment Survey
#CDIS - Coordinated Direct Investment Survey (CDIS)
#ED - Export Diversification
#FSI - Financial Soundness Indicators (FSIs)
#NAMAIN_IDC_N - System of National Accounts (SNA)

# Structure

# http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/GFSR
# http://dataservices.imf.org/REST/SDMX_JSON.svc/CodeList/CL_INDICATOR_PCPS
