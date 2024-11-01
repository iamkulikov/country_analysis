library("dplyr")
library("readxl")
library("readr")
library("tidyr")
library("writexl")
library("glue")
library("here")
library("lubridate")
library("purrr")

## Function to import data

importData <- function(data_y_fname, data_q_fname, data_m_fname, data_d_fname, dict_fname, path) {
  
  for (i in c("y", "q", "m", "d")) {
    eval(parse(text = glue("ncols <- length(read_csv2(here(path, data_{i}_fname), col_names = T, skip=0, n_max = 0, na = ''))") ))
    if (i == "d") {types <- paste0('cc?', strrep('d', ncols-3))} else {
      types <- paste0('cc', strrep('d', ncols-2)) }
    eval(parse(text = glue("extdata_{i} <- read_csv2(here(path, data_{i}_fname), col_names = T, col_types = types, na = '' )") ))
  }

  dict <- read_csv2(here(path, dict_fname), col_names = T, skip=0, na = '')
  extdata_d <- extdata_d |> mutate(date = as.Date(date))
  
  return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d, dict = dict))
  
}

## Function to subset data

subsetCountry <- function(countryname_export, datalist) {
    
    datalist$extdata_d <- datalist$extdata_d |> mutate(year = year(date)) |> select(year, date, everything())
    datalist_country <- datalist
    
    for (i in c("y", "q", "m", "d")) {
      eval(parse(text = glue("datalist_country$extdata_{i} <- datalist_country$extdata_{i} |> 
                                filter(country == '{countryname_export}') |>
                                select(-c(country, country_id))") ))
    }
    
    for (i in seq_along(datalist_country$dict$indicator)) {
      a <- eval(parse(text = glue("datalist_country$extdata_{datalist$dict$source_frequency[i]} |> 
                                          select(year, {datalist_country$dict$indicator_code[i]}) |> 
                                          filter(!is.na({datalist_country$dict$indicator_code[i]}))") ))
      suppressWarnings({
        datalist_country$dict$start_year[i] <- a |> pull(year) |> unique() |> min(na.rm = TRUE)
        })
      suppressWarnings({
        datalist_country$dict$end_year[i] <- a |> pull(year) |> unique() |> max(na.rm = TRUE)
      })
    }
    
    datalist_country$dict <- datalist_country$dict |> filter(is.finite(start_year), is.finite(end_year))
    
    for (i in c("y", "q", "m", "d")) {
      eval(parse(text = glue("year_min_{i} <- datalist_country$dict |> filter(source_frequency == '{i}') |>
                                 select(start_year) |> unique() |> min(na.rm = TRUE) ") ))
      eval(parse(text = glue("year_max_{i} <- datalist_country$dict |> filter(source_frequency == '{i}') |>
                                 select(end_year) |> unique() |> max(na.rm = TRUE) ") ))
      eval(parse(text = glue("datalist_country$extdata_{i} <- datalist_country$extdata_{i} |> discard(~all(is.na(.))) |>
                                 filter(year >= year_min_{i}, year <= year_max_{i})") ))
    } 
    
    datalist_country$extdata_d <- datalist_country$extdata_d |> select(-year)
    names(datalist_country) <- c("y", "q", "m", "d", "dict")
    return(datalist_country)
    #write_xlsx(datalist_country, path = here(countryname_export, "Data", glue("{countryname_export}_data_filled.xlsx")), 
    #           col_names = T, format_headers = T)
  
  
}

## Function to make a model sheet
generateModelSheet <- function(yearly_data, dict) {
  
    # choosing needed variables
    yearly_data |> select(-c("year")) |>
    select(any_of(c('gdp_pc_usd_wb', 'gdp_pc_ppp_wb', 'gdp_growth', 'gdp_usd', 'gdp', 
                    'gdp_growth_world_weo', 'gdp_growth_dm_weo', 'gdp_growth_em_weo',
                    'cpi_av', 'deflator', 'rnd', 'gcfc', 'gcfc_gdp', 'open',
                    'gg_debt_weo', 'gg_rev_weo', 'gg_debttorev', 'gg_exns_int', 'gg_inttorev', 'gg_debt_conc_usd',
                    'extdebt_conc_gdp', 'gg_bal_weo', 'gg_bal_gdp_weo', 'extdebt_gg_usd', 'extdebt_gg_gdp', 
                    'gg_debt_gdp_weo', 'gg_debt_fc_role_fsdb', 'gg_debt_held_global_usd', 'gg_debt_held_global_role',
                    'gg_debt_maturity', 'dpension2030', 
                    'ca_usd', 'ca_gdp', 'imp_gs_usd', 'ex_gs_usd', 'intres_usd', 'intrestoimp', 'niip_ex_ggcb_usd', 'niip_ex_ggcb_gdp', 
                    'ex_div', 'neer_av', 'usdlc_eop', 'usdlc_av', 'remit_usd_wb', 'remit_gdp_wb', 'extdebt_usd', 'intrestoextdebt',
                    'wgi_va_est', 'wgi_ps_est', 'wgi1', 'wgi_cc_est', 'wgi_rl_est', 'wgi_rq_est', 'wgi_ge_est', 'wgi2',
                    'educ', 'amr_male', 'amr_female', 'amr', 'life_length', 'hci'))) -> t_data_export
    
    yearly_data |> select("year") -> years
    dict_y <- dict |> filter(source_frequency == "y") |> select(indicator, indicator_code, theme, source_name)
    
    data_export <- data.frame(t(t_data_export))
    names(data_export) <- unlist(years)
    data_export <- cbind("indicator_code" = names(t_data_export), data_export)
    data_export <- data_export |> left_join(dict_y, by = c("indicator_code"="indicator_code")) |> 
      select(indicator, indicator_code, theme, source_name, everything())
    
    data_export <- list(data_export)
    names(data_export) <- c("y")
    return(data_export)
  
}

## Transpose data
transposeDatalist <- function(countrydata) {
  
  countrydata$y <- countrydata$y |> pivot_longer(!year, names_to = "variable", values_to = "value") |>
        pivot_wider(names_from = year, values_from = value)
  
  countrydata$q <- countrydata$q |> pivot_longer(!c(year, quarter), names_to = "variable", values_to = "value") |> 
    mutate(time = glue("{year}-{quarter}")) |> select(-c(year, quarter)) |>
    pivot_wider(names_from = time, values_from = value)
  
  countrydata$m <- countrydata$m |> pivot_longer(!c(year, quarter, month), names_to = "variable", values_to = "value") |> 
    mutate(time = glue("{year}-{month}")) |> select(-c(year, quarter, month)) |>
    pivot_wider(names_from = time, values_from = value)
  
  countrydata$d <- countrydata$d |> pivot_longer(!date, names_to = "variable", values_to = "value") |>
    pivot_wider(names_from = date, values_from = value)
  
  return(countrydata)
  
}