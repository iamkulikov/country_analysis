#devtools::install_github("jimhester/archive")
library("archive")
library("here")

### Place to save downloaded files
here::i_am("download_script/download.R")
datafolder <- "_extsources"

### Download WGIs
url <- "https://www.worldbank.org/content/dam/sites/govindicators/doc/wgidataset_with_sourcedata-2025.xlsx"
dest <- here("assets", "_DB", datafolder, "wgidataset.xlsx")
download.file(url, dest, mode="wb")
# unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))

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
# url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
# dest <- here("_DB", datafolder, "owid-covid-data.csv")
# download.file(url, dest, mode="wb")

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

### Download fresh IMF database on GG debt held in global reserves. Find new place in IMF for this data!!!
### and insert it to the Reserves.xlsx file manually!!!
### https://legacydata.imf.org/regular.aspx?key=60587813

### Download UN HDR data (links should be updated manually!!!!!)
### https://hdr.undp.org/data-center/documentation-and-downloads
url <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv"
dest <- here("assets", "_DB", datafolder, "HDR.csv")
download.file(url, dest)

### Download Chinn-Ito financial system classification (links should be updated manually!!!!!)
### http://web.pdx.edu/~ito/trilemma_indexes.htm
url <- "https://web.pdx.edu/~ito/trilemma_indexes_update2020.xlsx"
dest <- here("assets", "_DB", datafolder, "trilemma_indexes_update2020.xlsx")
download.file(url, dest, mode="wb")

### Download UNPD population projections, aggregates (links should be updated manually!!!!!)
### from here https://population.un.org/wpp/Download/Standard/CSV/
url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Demographic_Indicators_Medium.csv.gz"
dest <- here("assets", "_DB", datafolder, "UNDP_aggregates.gz")
final_dest <- here("assets", "_DB", datafolder, "UNDP_aggregates.csv")
download.file(url, dest)
gzfile <- gzfile(dest, 'rt')
writeLines(readLines(gzfile), final_dest)
close(gzfile)

url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"
dest <- here("assets", "_DB", datafolder, "UNDP_5yr.gz")
final_dest <- here("assets", "_DB", datafolder, "UNDP_5yr.csv")
download.file(url, dest)
gzfile <- gzfile(dest, 'rt')
writeLines(readLines(gzfile), final_dest)
close(gzfile)

### Download macroprudential database iMaPP (link should be updated manually!!!!!)
### https://www.elibrary-areaer.imf.org/Macroprudential/Pages/iMaPPDatabase.aspx

url <- "https://www.elibrary-areaer.imf.org/Macroprudential/Documents/iMaPP_database-2025-09-29.zip"
dest <- here("assets", "_DB", datafolder, "iMaPP_database-2025-09-29.zip")
download.file(url, dest)
unzip(zipfile = dest, exdir = here("assets", "_DB", datafolder))
file.rename(from=here("assets", "_DB", datafolder, 'iMaPP_database-2025-9-29.xlsx'), to=here("assets", "_DB", datafolder, 'iMaPP_database.xlsx'))

### Download NBFI market data from Financial Stability Board (link should be updated manually!!!!!)
### https://www.fsb.org/2025/12/global-monitoring-report-on-nonbank-financial-intermediation-2025/

url <- "https://www.fsb.org/uploads/2025-monitoring-dataset.xlsx"
dest <- here("assets", "_DB", datafolder, "NBFI_FSB.xlsx")
download.file(url, dest, mode="wb")

### Download Global Macro Data manually in csv format - GMD.csv !!!
### https://www.globalmacrodata.com/data.html

### Download "Total Economy Database - Output, Labor and Labor Productivity" DB manually 
### in xlsx format and rename CB_GrowthFactors.xlsx !!!
### https://data-central.conference-board.org/

### Download "Total Economy Database - Growth Accounting and Total Factor Productivity" DB manually 
### in xlsx format and rename CB_GrowthAccounting.xlsx !!!
### https://data-central.conference-board.org/

### Update cpi_target.xlsx in extsources manually!!! Source TO-FIND?

### Update defaults_DB.xlsx in extsources manually!!! Check in source manually

### Update CDS_cbonds.xlsx in extsources manually!!! Find data on cbonds.ru

### Update Neutral_rates.xlsx in extsources manually!!! Only use it for world and countries which we do not need,
### otherwise keep the same locally

### Download new WEO database using bulk download in excel - place in WEO_vintages folder. Rename accordingly.

### Download BoC–BoE Sovereign Default Database manually
### Find new link somewhere here by analogy: https://www.bankofcanada.ca/2024/07/staff-analytical-note-2024-19/
url <- "https://www.bankofcanada.ca/wp-content/uploads/2024/07/BoC-BoE-Database-2024.xlsx"
dest <- here("assets", "_DB", datafolder, "BOC-BOE.xlsx")
download.file(url, dest, mode="wb")

### Download R&R "This time is different" sovereign default database
### Use S&P_External sheet to fill explicit sheet in Default_DB manually (only starts of the periods). 
### Copy parts of ExternalDefaultDummys to fill RR sheet in Default_DB
url <- "https://carmenreinhart.com/wp-content/uploads/2020/04/165_data-3.xlsx"
dest <- here("assets", "_DB", datafolder, "RR.xlsx")
download.file(url, dest, mode="wb")

### Download Global Crises Data
url <- "https://www.hbs.edu/behavioral-finance-and-financial-stability/Documents/ChartData/MapCharts/20160923_global_crisis_data.xlsx"
dest <- here("assets", "_DB", datafolder, "GCD.xlsx")
download.file(url, dest, mode="wb")

### Download Horn/Cruces/Trebesch hidden default database
url <- "https://docs.google.com/spreadsheets/d/1Vp6PvRaAtv5Hh0OyJGGFbyj28PKAOrOx/export?format=xlsx"
dest <- here("assets", "_DB", datafolder, "HCT_hidden.xlsx")
download.file(url, dest, mode="wb", method = "libcurl")

### Download Cruces/Trebesch sovereign haircut database
url <- "https://docs.google.com/spreadsheets/d/1yUUMPAEF9uKbYV6bG3dLZ6dzDQnkJqcS/export?format=xlsx"
dest <- here("assets", "_DB", datafolder, "CT_haircut.xlsx")
download.file(url, dest, mode="wb", method = "libcurl")
