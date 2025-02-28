#devtools::install_github("jimhester/archive")
library("archive")
library("here")

### Place to save downloaded files
here::i_am("download_script/download.R")
datafolder <- "_extsources"

### Download WGIs
url <- "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_excel.zip"
dest <- here("assets", "_DB", datafolder, "wgidataset_excel.zip")
download.file(url, dest, mode="wb")
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))

### Download BIS daily and monthly data on nominal exchange rates
#url <- "https://www.bis.org/statistics/full_xru_d_csv_row.zip"
url <- "https://data.bis.org/static/bulk/WS_XRU_csv_flat.zip"
dest <- here("assets", "_DB", datafolder, "WS_XRU_csv_flat.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))

### Download BIS daily and monthly data on policy rates
#url <- "https://www.bis.org/statistics/full_cbpol_d_csv_row.zip"
url <- "https://data.bis.org/static/bulk/WS_CBPOL_csv_flat.zip"
dest <- here("assets", "_DB", datafolder, "WS_CBPOL_csv_flat.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))

### Download BIS monthly data on effective exchange rates
#url <- "https://www.bis.org/statistics/full_eer_m_csv.zip"
url <- "https://data.bis.org/static/bulk/WS_EER_csv_flat.zip"
dest <- here("assets" ,"_DB", datafolder, "WS_EER_csv_flat.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets" ,"_DB", datafolder))

### Download BIS monthly data on policy rates
# url <- "https://www.bis.org/statistics/full_cbpol_m_csv.zip"
# dest <- here("_DB", datafolder, "full_cbpol_m_csv.zip")
# download.file(url, dest)
# unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download BIS monthly data on nominal exchange rates
# url <- "https://www.bis.org/statistics/full_xru_csv.zip"
# dest <- here("_DB", datafolder, "full_xru_csv.zip")
# download.file(url, dest)
# unzip(zipfile = dest, exdir = here("_DB", datafolder))

### Download UNCTAD diversification index
url <- "https://unctadstat-api.unctad.org/bulkdownload/US.ConcentDiversIndices/US_ConcentDiversIndices"
#url <- "http://unctadstat.unctad.org/7zip/US_ConcentDiversIndices.csv.7z"
dest <- here("assets", "_DB", datafolder, "US_ConcentDiversIndices.csv.7z")
download.file(url, dest, mode = "wb")
a <- read.csv(archive_read(archive = dest, format = "7zip"))
write.csv(a, file = here("assets", "_DB", datafolder, "US_ConcentDiversIndices.csv") )

### Download IDS external debt statistics
url <- "https://databank.worldbank.org/data/download/IDS_Excel.zip"
dest <- here("assets", "_DB", datafolder, "IDS_Excel.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))

### Download COVID data from Ourworldindata (don't need to update any more)
### or manually from here https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv
#url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
#dest <- here("_DB", datafolder, "owid-covid-data.csv")
#download.file(url, dest, mode="wb")

### Download Fiscal Monitor structural indicators table 
### and insert it to the FiscalMonitor.xlsx file manually!!!
### https://www.imf.org/en/Publications/FM

### Download fresh WEO database in tab-delimited form - rename as WEO.xlsx
### Check that all rows contain smth, at least n/a (delete footnotes)
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download fresh WEO database for aggregates in tab-delimited form, choose only GDP growth - rename as WEO_aggr.xlsx
### Check that all rows contain smth, at least n/a (delete footnotes)
### https://www.imf.org/en/Publications/SPROLLs/world-economic-outlook-databases#sort=%40imfdate%20descending

### Download fiscal space database and check that it is called Fiscal-space-data.xlsx
### https://www.worldbank.org/en/research/brief/fiscal-space

### Download fresh IMF database on GG debt held in global reserves
### and insert it to the Reserves.xlsx file manually!!!
### https://data.imf.org/regular.aspx?key=60587813

### Download UN HDR data (links should be updated manually!!!!!)
### https://hdr.undp.org/data-center/documentation-and-downloads
url <- "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv"
dest <- here("assets", "_DB", datafolder, "HDR.csv")
download.file(url, dest)

### Download Chinn-Ito financial system classification (links should be updated manually!!!!!)
### http://web.pdx.edu/~ito/trilemma_indexes.htm
url <- "https://web.pdx.edu/~ito/trilemma_indexes_update2020.xlsx"
dest <- here("assets", "_DB", datafolder, "trilemma_indexes_update2020.xlsx")
download.file(url, dest, mode="wb")

### Download UNPD population projections, aggregates (links should be updated manually!!!!!)
### from here https://population.un.org/wpp/Download/Standard/CSV/
url <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz"
dest <- here("assets", "_DB", datafolder, "UNDP_aggregates.gz")
final_dest <- here("assets", "_DB", datafolder, "UNDP_aggregates.csv")
download.file(url, dest)
gzfile <- gzfile(dest, 'rt')
writeLines(readLines(gzfile), final_dest)
close(gzfile)

url <- "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"
dest <- here("assets", "_DB", datafolder, "UNDP_5yr.gz")
final_dest <- here("assets", "_DB", datafolder, "UNDP_5yr.csv")
download.file(url, dest)
gzfile <- gzfile(dest, 'rt')
writeLines(readLines(gzfile), final_dest)
close(gzfile)

### Download macroprudential database iMaPP (link should be updated manually!!!!!)
### https://www.elibrary-areaer.imf.org/Macroprudential/Pages/iMaPPDatabase.aspx

url <- "https://www.elibrary-areaer.imf.org/Macroprudential/Documents/iMaPP_database-2024-12-02.zip"
dest <- here("assets", "_DB", datafolder, "iMaPP_database-2024-12-02.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))
file.rename(from=here("assets", "_DB", datafolder, 'iMaPP_database-2024-12-2.xlsx'), to=here("assets", "_DB", datafolder, 'iMaPP_database.xlsx'))

### Download NBFI markey data from Financial Stability Board (link should be updated manually!!!!!)
### https://www.fsb.org/2023/12/global-monitoring-report-on-non-bank-financial-intermediation-2023/

url <- "https://www.fsb.org/wp-content/uploads/Monitoring-Dataset-2024.xlsx"
dest <- here("assets", "_DB", datafolder, "NBFI_FSB.xlsx")
download.file(url, dest, mode="wb")

### Download Global Macro Data manually in csv format - GMD.csv !!!
### https://www.globalmacrodata.com/data.html

