test <- imf_data(database_id = "BOP",indicator = "BIP_BP6_USD", freq = 'A', country = 'RU', start = 1987 , end = 2011)
test
test %>% filter(iso2c == "RU")