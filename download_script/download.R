#devtools::install_github("jimhester/archive")
library("archive")
library("here")

### Place to save downloaded files
here::i_am("_country_analysis_scripts/download_script/download.R")
datafolder <- "_extsources"

### Download WGIs
url <- "http://info.worldbank.org/governance/wgi/Home/downLoadFile?fileName=wgidataset.xlsx"
dest <- here("_DB", datafolder, "wgidataset.xlsx")
download.file(url, dest, mode="wb")

### Download BIS daily data on nominal exchange rates
url <- "https://www.bis.org/statistics/full_xru_d_csv_row.zip"
dest <- here("_DB", datafolder, "full_xru_d_csv_row.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download BIS daily data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip"
dest <- here("_DB", datafolder, "full_cbpol_d_csv_row.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download BIS monthly data on policy rates
url <- "https://www.bis.org/statistics/full_cbpol_m_csv.zip"
dest <- here("_DB", datafolder, "full_cbpol_m_csv.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download BIS monthly data on effective exchange rates
url <- "https://www.bis.org/statistics/full_eer_m_csv.zip"
dest <- here("_DB", datafolder, "full_eer_m_csv.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download BIS monthly data on nominal exchange rates
url <- "https://www.bis.org/statistics/full_xru_csv.zip"
dest <- here("_DB", datafolder, "full_xru_csv.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download UNCTAD diversification index
url <- "http://unctadstat.unctad.org/7zip/US_ConcentDiversIndices.csv.7z"
dest <- here("_DB", datafolder, "US_ConcentDiversIndices.csv.7z")
download.file(url, dest, mode = "wb")
a <- read.csv(archive_read(archive = dest, format = "7zip"))
write.csv(a, file = here("_DB", datafolder, "US_ConcentDiversIndices.csv") )

### Download IDS external debt statistics
url <- "https://databank.worldbank.org/data/download/IDS_Excel.zip"
dest <- here("_DB", datafolder, "IDS_Excel.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download COVID data from Ourworldindata (don't need to update any more)
### or manually from here https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv
#url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
#dest <- here("_DB", datafolder, "owid-covid-data.csv")
#download.file(url, dest, mode="wb")

### Download Fiscal Monitor structural indicators table 
### and insert it to the FiscalMonitor.xlsx file manually!!!
### https://www.imf.org/en/Publications/FM

### Download fresh WEO database in tab-delimited form - rename as WEO. 
### Check that all rows contain smth, at least n/a
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download fresh WEO database for aggregates in tab-delimited form, choose only GDP growth - rename as WEO_aggr.
### Check that all rows contain smth, at least n/a
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download fiscal space database and check that it is called Fiscal-space-data.xlsx
### https://www.worldbank.org/en/research/brief/fiscal-space

### Download fresh IMF database on GG debt held in global reserves
### and insert it to the Reserves.xlsx file manually!!!
### https://data.imf.org/regular.aspx?key=60587813

### Download UN HDR data (links should be updated manually!!!!!)
### https://hdr.undp.org/data-center/documentation-and-downloads
url <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
dest <- here("_DB", datafolder, "HDR.csv")
download.file(url, dest)

### Download Chinn-Ito financial system classification (links should be updated manually!!!!!)
### http://web.pdx.edu/~ito/trilemma_indexes.htm
url <- "https://web.pdx.edu/~ito/trilemma_indexes_update2020.xlsx"
dest <- here("_DB", datafolder, "trilemma_indexes_update2020.xlsx")
download.file(url, dest, mode="wb")

### Download UNPD population projections, aggregates (links should be updated manually!!!!!)
### from here https://population.un.org/wpp/Download/Standard/CSV/
url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_Demographic_Indicators_Medium.zip"
dest <- here("_DB", datafolder, "UNDP_aggregates.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))
file.rename(from=here("_DB", datafolder, 'WPP2022_Demographic_Indicators_Medium.csv'), to=here("_DB", datafolder, 'UNDP_aggregates.csv'))

url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip"
dest <- here("_DB", datafolder, "UNDP_5yr.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))
file.rename(from=here("_DB", datafolder, 'WPP2022_PopulationByAge5GroupSex_Medium.csv'), to=here("_DB", datafolder, 'UNDP_5yr.csv'))

### Download macroprudential database iMaPP (link should be updated manually!!!!!)
### https://www.elibrary-areaer.imf.org/Macroprudential/Pages/iMaPPDatabase.aspx

url <- "https://www.elibrary-areaer.imf.org/Macroprudential/Documents/iMaPP_database-2023-04-11.zip"
dest <- here("_DB", datafolder, "iMaPP_database-2023-04-11.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("_DB", datafolder))
file.rename(from=here("_DB", datafolder, 'iMaPP_database-2023-4-11.xlsx'), to=here("_DB", datafolder, 'iMaPP_database.xlsx'))
