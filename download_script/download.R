#devtools::install_github("jimhester/archive")
library("archive")

### Place to save downloaded files
#setwd("D:/Dropbox/Methods_Programs/R_utilities/country_analysis/_DB")
#setwd("D:/Dropbox/Methods_Programs/R_utilities/download_data")
datafolder <- "_extsources"
setwd(paste("C:/Projects/country_analysis/_DB", datafolder, sep="/"))

### Download WGIs
url <- "http://info.worldbank.org/governance/wgi/Home/downLoadFile?fileName=wgidataset.xlsx"
dest <- paste(getwd(),"wgidataset.xlsx", sep="/")
download.file(url, dest, mode="wb")

### Download BIS daily data on nominal exchange rates
url <- "https://www.bis.org/statistics/full_xru_d_csv_row.zip"
dest <- paste(getwd(),"full_xru_d_csv_row.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download BIS daily data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip"
dest <- paste(getwd(),"full_cbpol_d_csv_row.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download BIS monthly data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_m_csv.zip"
dest <- paste(getwd(), "full_cbpol_m_csv.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download BIS monthly data on effective exchange rates
url <- "https://www.bis.org/statistics/full_eer_m_csv.zip"
dest <- paste(getwd(), "full_eer_m_csv.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download BIS monthly data on nominal exchange rates
url <- "https://www.bis.org/statistics/full_xru_csv.zip"
dest <- paste(getwd(), "full_xru_csv.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download UNCTAD diversification index
url <- "http://unctadstat.unctad.org/7zip/US_ConcentDiversIndices.csv.7z"
dest <- paste(getwd(), "US_ConcentDiversIndices.csv.7z", sep="/")
download.file(url, dest, mode="wb")
a <- read.csv(archive_read(archive=dest, format="7zip"))
write.csv(a, file=paste(getwd(), "US_ConcentDiversIndices.csv", sep="/") )

### Download IDS external debt statistics
url <- "https://databank.worldbank.org/data/download/IDS_Excel.zip"
dest <- paste(getwd(), "IDS_Excel.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())

### Download COVID data from Ourworldindata
### or manually from here https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv
url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
dest <- paste(getwd(), "owid-covid-data.csv", sep="/")
download.file(url, dest, mode="wb")

### Download Fiscal Monitor structural indicators table 
### and insert it to the FiscalMonitor.xlsx file manually!!!
### https://www.imf.org/en/Publications/FM

### Download fresh WEO database in tab-delimited form - rename as WEO. 
### Attention to number to date conversion
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download UN HDR data (links should be updated manually!!!!!)
### https://hdr.undp.org/data-center/documentation-and-downloads
url <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
dest <- paste(getwd(), "HDR.csv", sep="/")
download.file(url, dest)

### Download Chinn-Ito financial system classification (links should be updated manually!!!!!)
### http://web.pdx.edu/~ito/trilemma_indexes.htm
url <- "https://web.pdx.edu/~ito/trilemma_indexes_update2020.xlsx"
dest <- paste(getwd(), "trilemma_indexes_update2020.xlsx", sep="/")
download.file(url, dest, mode="wb")

### Download UNPD population projections, aggregates (links should be updated manually!!!!!)
### from here https://population.un.org/wpp/Download/Standard/CSV/
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_Medium.zip"
dest <- paste(getwd(), "UNDP_aggregates.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())
file.rename(from='WPP2022_Demographic_Indicators_Medium.csv', to='UNDP_aggregates.csv')

url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip"
dest <- paste(getwd(), "UNDP_5yr.zip", sep="/")
download.file(url, dest)
unzip(zipfile = dest, exdir = getwd())
file.rename(from='WPP2022_PopulationByAge5GroupSex_Medium.csv', to='UNDP_5yr.csv')