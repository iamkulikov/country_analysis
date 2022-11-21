#devtools::install_github("jimhester/archive")
library("archive")

### Place to save downloaded files
#setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/download_data")
setwd("C:/Projects/country_analysis/_DB")
datafolder <- "_extsources"

### Download WGIs
url <- "http://info.worldbank.org/governance/wgi/Home/downLoadFile?fileName=wgidataset.xlsx"
dest <- paste(getwd(), datafolder,"wgidataset.xlsx", sep="/")
download.file(url, dest, mode="wb")

### Download BIS daily data on nominal exchange rates
url <- "https://www.bis.org/statistics/full_xru_d_csv_row.zip"
dest <- paste(getwd(), datafolder,"full_xru_d_csv_row.zip", sep="/")
download.file(url, dest)
unzip(zipfile = paste(getwd(), datafolder,"full_xru_d_csv_row.zip", sep="/"), 
      exdir = paste(getwd(), datafolder, sep="/"))

### Download BIS daily data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip"
dest <- paste(getwd(), datafolder,"full_cbpol_d_csv_row.zip", sep="/")
download.file(url, dest)
unzip(zipfile = paste(getwd(), datafolder,"full_cbpol_d_csv_row.zip", sep="/"), 
      exdir = paste(getwd(), datafolder, sep="/"))

### Download BIS monthly data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_m_csv.zip"
dest <- paste(getwd(), datafolder, "full_cbpol_m_csv.zip", sep="/")
download.file(url, dest)
unzip(zipfile = paste(getwd(), datafolder, "full_cbpol_m_csv.zip", sep="/"), 
      exdir = paste(getwd(), datafolder, sep="/"))

### Download BIS monthly data on effective exchange rates
url <- "https://www.bis.org/statistics/full_eer_m_csv.zip"
dest <- paste(getwd(), datafolder, "full_eer_m_csv.zip", sep="/")
download.file(url, dest)
unzip(zipfile = paste(getwd(), datafolder, "full_eer_m_csv.zip", sep="/"), 
      exdir = paste(getwd(), datafolder, sep="/"))

### Download UNCTAD diversification index
url <- "http://unctadstat.unctad.org/7zip/US_ConcentDiversIndices.csv.7z"
dest <- paste(getwd(), datafolder, "US_ConcentDiversIndices.csv.7z", sep="/")
download.file(url, dest, mode="wb")
a <- read.csv(archive_read(archive=dest, format="7zip"))
write.csv(a, file=paste(getwd(), datafolder, "US_ConcentDiversIndices.csv", sep="/") )

### Download IDS external debt statistics
url <- "https://databank.worldbank.org/data/download/IDS_Excel.zip"
dest <- paste(getwd(), datafolder, "IDS_Excel.zip", sep="/")
download.file(url, dest)
unzip(zipfile = paste(getwd(), datafolder, "IDS_Excel.zip",sep="/"), 
      exdir = paste(getwd(), datafolder, sep="/"))

### Download COVID data from Ourworldindata
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
dest <- paste(getwd(), datafolder, "owid-covid-data.csv", sep="/")
download.file(url, dest, mode="wb")

### Download Fiscal Monitor structural indicators table 
### and insert it to the FiscalMonitor.xlsx file manually!!!
### https://www.imf.org/en/Publications/FM

### Download fresh WEO database, name it WEO.xlsx and rename the sheet to y_weo manually!!!!!
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download UN HDR data (links should be updated manually!!!!!)
### https://hdr.undp.org/data-center/documentation-and-downloads
url <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
dest <- paste(getwd(), datafolder, "HDR.csv", sep="/")
download.file(url, dest)

### Download Chinn-Ito financial system classification (links should be updated manually!!!!!)
### http://web.pdx.edu/~ito/trilemma_indexes.htm
url <- "https://web.pdx.edu/~ito/trilemma_indexes_update2020.xlsx"
dest <- paste(getwd(), datafolder, "trilemma_indexes_update2020.xlsx", sep="/")
download.file(url, dest, mode="wb")

