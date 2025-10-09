library("dplyr")
library("readxl")
library("readr")
library("tidyr")
library("writexl")
library("glue")
library("here")
library("lubridate")
library("purrr")

##### Function to import previously downloaded data

#' @param yqm_file Путь к файлу с Y/Q/M (RDS или XLSX)
#' @param d_file   Путь к файлу с D (RDS или XLSX)
#' @param sheet_keys Имена листов/ключей для Y/Q/M; по умолчанию c(y="y", q="q", m="m")
#' @param format   "auto" | "rds" | "xlsx"; auto смотрит на расширение yqm_file
#' @param add_time Логический флаг: добавлять ли time для Y/Q/M (FALSE по умолчанию)
#'
#' @return list(extdata_y, extdata_q, extdata_m, extdata_d, dict)
#' @export

importData <- function(yqm_file, d_file, sheet_keys  = c(y = "y", q = "q", m = "m"),
                       format = c("auto", "rds", "xlsx"), add_time    = FALSE) {
  
  # --- 0. Валидация и определение формата ------------------------------------
  format <- rlang::arg_match(format)
  if (format == "auto") {
    ext <- tools::file_ext(yqm_file)
    format <- if (tolower(ext) == "rds") "rds" else "xlsx"
  }
  
  # --- Вспомогательные функции (локальный скоуп) ------------------------------
  .read_dict_from_rds_exact <- function(bundle, name, origin_label) {
    if (is.null(bundle[[name]])) {
      cli::cli_warn("В {origin_label} не найден объект словаря с именем '{name}'. Продолжаю без него.")
      return(NULL)
    }
    bundle[[name]]
  }
  
  .read_dict_from_xlsx_exact <- function(path, sheet, origin_label) {
    if (!file.exists(path)) {
      cli::cli_warn("Файл {.path {path}} ({origin_label}) не найден для чтения словаря '{sheet}'. Продолжаю без него.")
      return(NULL)
    }
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
    if (!(sheet %in% sheets)) {
      cli::cli_warn("В {origin_label} не найден лист словаря '{sheet}'. Продолжаю без него.")
      return(NULL)
    }
    ncols <- ncol(readxl::read_excel(path, sheet = sheet, n_max = 0))
    if (is.na(ncols) || ncols == 0) {
      cli::cli_warn("Лист словаря '{sheet}' в {origin_label} пуст. Продолжаю без него.")
      return(NULL)
    }
    readxl::read_excel(
      path,
      sheet     = sheet,
      col_names = TRUE,
      col_types = rep("text", ncols)
    ) |>
      tibble::as_tibble()
  }
  
  .maybe_add_time <- function(out, add_time_flag) {
    if (!isTRUE(add_time_flag)) return(out)
    out$extdata_y <- .add_time_safe_y(out$extdata_y)
    out$extdata_q <- .add_time_safe_q(out$extdata_q)
    out$extdata_m <- .add_time_safe_m(out$extdata_m)
    # extdata_d не трогаем
    out
  }
  
  .add_time_safe_y <- function(df) {
    if (is.null(df)) return(df)
    if (!all(c("year") %in% names(df))) {
      cli::cli_warn("Для Y не удалось добавить 'time': отсутствует колонка 'year'.")
      return(df)
    }
    dplyr::mutate(df, time = .data$year - 1987L)
  }
  
  .add_time_safe_q <- function(df) {
    if (is.null(df)) return(df)
    req <- c("year", "quarter")
    if (!all(req %in% names(df))) {
      cli::cli_warn("Для Q не удалось добавить 'time': нет колонок {setdiff(req, names(df))}.")
      return(df)
    }
    dplyr::mutate(df, time = (.data$year - 1987L) * 4L + .data$quarter)
  }
  
  .add_time_safe_m <- function(df) {
    if (is.null(df)) return(df)
    req <- c("year", "month")
    if (!all(req %in% names(df))) {
      cli::cli_warn("Для M не удалось добавить 'time': нет колонок {setdiff(req, names(df))}.")
      return(df)
    }
    dplyr::mutate(df, time = (.data$year - 1987L) * 12L + .data$month)
  }
  
  .bind_dicts <- function(d1, d2) {
    dicts <- purrr::compact(list(d1, d2))
    if (length(dicts) == 0) return(NULL)
    dplyr::bind_rows(dicts)
  }
  
  # --- 1. RDS path ------------------------------------------------------------
  if (format == "rds") {
    # 1.1 читаем оба бандла
    yqm_bundle <- readRDS(yqm_file)   # ожидаем list(y, q, m, dict, ...)
    d_bundle   <- readRDS(d_file)     # ожидаем list(d, dict_d, ...)
    
    # 1.2 собираем extdata_* из Y/Q/M и D (как в исходной функции)
    out <- purrr::map(
      sheet_keys,
      ~ yqm_bundle[[.x]]
    ) |>
      purrr::set_names(paste0("extdata_", names(sheet_keys)))
    
    out$extdata_d <- d_bundle[["d"]]
    
    # 1.3 словари: строго по именам "dict" и "dict_d"
    dict_yqm <- .read_dict_from_rds_exact(yqm_bundle, "dict",   origin_label = "файле Y/Q/M (RDS)")
    dict_d   <- .read_dict_from_rds_exact(d_bundle,   "dict_d", origin_label = "файле D (RDS)")
    out$dict <- .bind_dicts(dict_yqm, dict_d)
    
    # 1.4 опционально добавляем time
    out <- .maybe_add_time(out, add_time)
    
    return(out)
  }
  
  # --- 2. XLSX path -----------------------------------------------------------
  if (format == "xlsx") {
    # 2.1 читаем Y/Q/M через ваш помощник
    out <- purrr::map(
      sheet_keys,
      ~ readSeriesSheet(yqm_file, sheet = .x)
    ) |>
      purrr::set_names(paste0("extdata_", names(sheet_keys)))
    
    # 2.2 читаем D через ваш помощник (с фиксированными типами)
    out$extdata_d <- readSeriesSheet(
      d_file,
      sheet       = "d",
      fixed_types = c("text", "text", "date")
    )
    
    # 2.3 словари: строго по листам "dict" (в YQM) и "dict_d" (в D)
    dict_yqm <- .read_dict_from_xlsx_exact(yqm_file, sheet = "dict",   origin_label = "файле Y/Q/M (XLSX)")
    dict_d   <- .read_dict_from_xlsx_exact(d_file,   sheet = "dict_d", origin_label = "файле D (XLSX)")
    out$dict <- .bind_dicts(dict_yqm, dict_d)
    
    # 2.4 опционально добавляем time
    out <- .maybe_add_time(out, add_time)
    
    return(out)
  }
  
  # --- 3. Defensive fallback --------------------------------------------------
  rlang::abort("Unsupported format – choose 'rds', 'xlsx', or 'auto'.")
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