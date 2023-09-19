library("WDI")
library("dplyr")

WDIsearch('Gross Ext. Debt Pos., General Government, All maturities, All instruments, All currencies')


3786 External debt stocks, general government sector (PPG) (DOD, current US$)  DT.DOD.DEGG.CD

ED
+ 3672 Gross Ext. Debt Pos., All Sectors, All maturities, All instruments, All currencies, USD  DT.DOD.DECT.CD.DT.US
+ 3655 Gross Ext. Debt Pos., All Sectors, All maturities, All instruments, USD  DT.DOD.DECT.CD.AR.US
3647 Gross Ext. Debt Pos., All Sectors, All maturities, All instruments, end of period, USD   DT.DOD.DECT.CD.AR.EN.US

ED FC
+ 3699 Gross Ext. Debt Pos., All Sectors, All maturities, All instruments, Foreign currency, USD  DT.DOD.DECT.CD.FC.US

ED GG
+ 3714 Gross Ext. Debt Pos., General Government, All maturities, All instruments, USD   DT.DOD.DECT.CD.GG.AR.US
+ 3707 Gross Ext. Debt Pos., General Government , All maturities, All instruments, end of period, USD   DT.DOD.DECT.CD.GG.AR.EN.US

ED Intercompany
 
dat <- WDI(indicator=c('DT.DOD.DECT.CD.DT.US', 'DT.DOD.DECT.CD.AR.US', 'DT.DOD.DECT.CD.AR.EN.US', 'DT.DOD.DECT.CD.FC.US',
                      'DT.DOD.DECT.CD.GG.AR.US','DT.DOD.DECT.CD.FC.GG.TO.US'),
          country=c('RU'), start=2019, end=2023) %>% select(-c(country))
dat

dat = WDI(indicator='DT.DOD.DECT.CD.GG.AR.EN.US', country=c('RU'), start=2019, end=2023)
dat = WDI(indicator='DT.DOD.DEGG.CD', country='all', start=2019, end=2020)
