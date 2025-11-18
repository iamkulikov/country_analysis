#   All the functions to import data

## ------  Load libraries
library_names <- c("dplyr","reshape2","WDI","countrycode","readxl","readr","tidyr","data.table","writexl","stringr",
                   "gsubfn","jsonlite","Rilostat","glue","httr","here","zoo","rlang","purrr")
# ,"rlist"

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

## ------ Set working directory and import custom tools
here::i_am("download_script/import.R")
source(here("download_script","imf_tool.R"))

## ------ Set parameters
# d_container_start <- as.Date("2019-01-01")
# d_container_end <- as.Date("2026-12-31")  # it's easier to expand container manually without using update=0 mode

## ------- Function to set import/update schedule

readImportParams <- function (param_fname, update_mode) {
  
  param_fname <- here("assets", "_DB", "0_database_params.xlsx")
  impplan <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
  parameters <- read_excel(param_fname, sheet = "scope", col_names = T, skip=1, n_max=1)
  locals <- read_excel(param_fname, sheet = "scope", col_names = T, skip=6)
  local_countries <- locals |> filter(active == 1) |> pull(country)
  local_iso2 <- locals |> filter(active == 1) |> pull(country_id)
  local_fnames <- here("assets", local_countries, "Data", glue("{local_countries}_local.xlsx"))
  year_first <- parameters$start
  year_final <- parameters$end
  saveplan <- impplan |> filter(active == 1)
  if (update_mode == 1) {impplan <- impplan |> filter(update == 1)}
  impplan <- impplan |> filter(active == 1)
  return(list(year_first = year_first, year_final = year_final, saveplan = saveplan, impplan = impplan, 
              local_countries = local_countries, local_iso2 = local_iso2, local_fnames = local_fnames))
  
}

##### Function to generate data containers

generateDataContainers <- function(from, to, d_start  = NULL, d_end = NULL, verbose = TRUE) {
  
  # messaging helper ---------------------------------------------------
  say <- function(txt) if (verbose) message(txt)
  
  # coerce inputs ------------------------------------------------------
  from <- as.integer(from)
  to   <- as.integer(to)
  if (from > to) stop("`from` must be <= `to`.", call. = FALSE)
  
  if (is.null(d_start)) d_start <- as.Date(sprintf("%s-01-01", from))
  if (is.null(d_end))   d_end   <- as.Date(sprintf("%s-12-31", to))
  d_start <- as.Date(d_start)
  d_end   <- as.Date(d_end)
  if (d_start > d_end) stop("`d_start` must be <= `d_end`.", call. = FALSE)
  
  years  <- seq.int(from, to, by = 1L)
  months <- 1:12
  days   <- seq(d_start, d_end, by = "day")
  
  # countries ----------------------------------------------------------
  say("Fetching country list from WDI …")
  countries_raw <- WDI(indicator = "NY.GDP.MKTP.CD", start = 1980, end = 2025, extra = TRUE)
  
  countries <- countries_raw |>
    distinct(country, iso2c, region) |>
    filter(!is.na(iso2c), nzchar(iso2c), nchar(iso2c) == 2L, region != "Aggregates") |>
    rename(country_id = iso2c) |> select(-region) |>
    bind_rows(tibble(country = c("World","Viet Nam","Czechia","Puerto Rico (US)"), country_id = c("1W","VN","CZ","PR"))) |> 
    arrange(country) |> distinct(country_id, .keep_all = TRUE)
  
  say(sprintf("  → %d countries (aggregates removed).", nrow(countries)))
  
  # yearly container ---------------------------------------------------
  say("Building yearly container …")
  extdata_y <- crossing(countries, year = years) |> arrange(year, country)
  
  # monthly container --------------------------------------------------
  say("Building monthly container …")
  extdata_m <- crossing(countries, year = years, month = months) |>
    mutate(quarter = (month - 1L) %/% 3L + 1L) |> select(country, country_id, year, quarter, month) |>
    arrange(year, month, country)
  
  # quarterly container ------------------------------------------------
  say("Building quarterly container …")
  # derive safely from monthly (ensures quarter labels consistent)
  extdata_q <- extdata_m |> distinct(country, country_id, year, quarter) |> arrange(year, quarter, country)
  
  # daily container ----------------------------------------------------
  say("Building daily container …")
  extdata_d <- crossing(countries, date = days) |> arrange(date, country)
  
  say("Done.")
  list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
  
}

##### Helper function to import series from a single xslx sheet

readSeriesSheet <- function(path, sheet = "Sheet1", fixed_types = c("text", "text")) {
  
  ncols <- ncol(read_excel(path = path, sheet = sheet, n_max = 0))
  read_excel(path, sheet = sheet, col_types = c(fixed_types, rep("numeric", ncols - length(fixed_types))))
  
}

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
    # 2.1 читаем Y/Q/M через помощник
    out <- purrr::map(
      sheet_keys,
      ~ readSeriesSheet(yqm_file, sheet = .x)
    ) |>
      purrr::set_names(paste0("extdata_", names(sheet_keys)))
    
    # 2.2 читаем D через помощник (с фиксированными типами)
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


#' importData <- function(yqm_file, d_file, sheet_keys  = c(y = "y", q = "q", m = "m"), format = c("auto", "rds", "xlsx")) {
#'   
#'   format <- match.arg(format)
#'   if (format == "auto") {
#'     ext <- tools::file_ext(yqm_file)
#'     format <- if (tolower(ext) == "rds") "rds" else "xlsx"
#'   }
#'   
#'   # ---- 1. RDS path ----------------------------------------------------------
#'   if (format == "rds") {
#'     
#'     ## 1.1  read the bundles ---------------------------------------------------
#'     yqm_bundle <- readRDS(yqm_file)   # list(y = ..., q = ..., m = ..., dict = ...)
#'     d_bundle   <- readRDS(d_file)     # list(d = ..., dict_d = ...)
#'     
#'     ## 1.2  assemble return list ----------------------------------------------
#'     out <- purrr::map(sheet_keys, ~ yqm_bundle[[.x]]) |>
#'       purrr::set_names(paste0("extdata_", names(sheet_keys)))
#'     
#'     out$extdata_d <- d_bundle[["d"]]
#'     
#'     return(out)
#'   }
#'   
#'   # ---- 2. XLSX path ---------------------------------------------------------
#'   if (format == "xlsx") {
#'     
#'     ## 2.1  read Y/Q/M sheets --------------------------------------------------
#'     out <- purrr::map(sheet_keys,
#'                       ~ read_series_sheet(yqm_file, sheet = .x)) |>
#'       purrr::set_names(paste0("extdata_", names(sheet_keys)))
#'     
#'     ## 2.2  read daily sheet ---------------------------------------------------
#'     out$extdata_d <- read_series_sheet(
#'       d_file,
#'       sheet        = "d",
#'       fixed_types  = c("text", "text", "date")
#'     )
#'     
#'     return(out)
#'   }
#'   
#'   # ---- 3. Defensive fallback -------------------------------------------------
#'   rlang::abort("Unsupported format – choose 'rds', 'xlsx', or 'auto'.")
#' }

# importOldData <- function(data_fname, data_d_fname, sheet_keys) {
#   
#   extdata_list <- map(sheet_keys, ~ readSeriesSheet(data_fname, .x)) |> set_names(paste0("extdata_", names(sheet_keys)))
#   extdata_list$extdata_d <- readSeriesSheet(data_d_fname, sheet = "d", fixed_types  = c("text", "text", "date") )
#   
#   return(extdata_list)
# }


##### Function to update import schedule after each cycle of import attempts

updateImportPlan <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {
  
  extdata_list <- list(y = extdata_y, q = extdata_q, m = extdata_m, d = extdata_d |> select(-date))
  
  ## 1. Which frequencies are requested in the import plan?             ##
  impplan_freq <- impplan |> distinct(source_frequency) |> pull(source_frequency)
  
  ## 2. Find **totally blank columns** in the corresponding extdata (all NA or all empty strings)  ##
  is_blank <- function(x) all(is.na(x)) || (is.character(x) && all(trimws(x) == ""))
  
  empty_codes <- map_dfr(
    impplan_freq,
    function(freq) {
      df <- extdata_list[[freq]]
      if (is.null(df)) return(tibble())        # safety: sheet not supplied
      cols <- names(df)[map_lgl(df, is_blank)]
      tibble(indicator_code = cols, source_frequency = freq)
    }
  )
  
  ## 3. Match those blank columns to the import plan                    ##
  empty_output <- impplan |> inner_join(empty_codes, by = c("indicator_code", "source_frequency"))
  
  ## 4. Find **non‑existent** indicators (present in plan but missing from every extdata sheet) ##

  existing_codes <- map2_dfr(
    extdata_list,
    names(extdata_list),
    ~ tibble(indicator_code = names(.x), source_frequency = .y)
  )
  
  nonexistent_output <- impplan |> anti_join(existing_codes, by = c("indicator_code", "source_frequency"))
  
  ## 5. Combine & return ##
  bind_rows(empty_output, nonexistent_output) |> distinct()
  
}

##### Function to drop data, which needs to be updated

dropDataToUpdate <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d) {

  if (requireNamespace("cli", quietly = TRUE)) use_cli <- TRUE else use_cli <- FALSE  
  extdata_list <- list(y = extdata_y, q = extdata_q, m = extdata_m, d = extdata_d)
  
  # helper for talking 
  say <- function(freq, dropped) {
    txt <- if (length(dropped)) {sprintf("Dropped from extdata_%s: %s", freq, toString(dropped)) } else {
      sprintf("Nothing to drop from extdata_%s", freq) }
    if (use_cli) cli::cli_alert_info(txt) else message(txt, appendLF = TRUE)
  }
  message("Dropping data which is going to be updated:")
  
  # iterate over list and remove columns
  extdata_list <- imap( extdata_list,
    ~ {
      # all columns in impplan for this frequency
      wanted <- impplan |>
        filter(source_frequency == .y) |> pull(indicator_code)
      
      # columns that really exist in this data frame
      to_drop <- intersect(wanted, names(.x))
      say(.y, to_drop)
      
      # return the data frame minus those columns
      .x |> select(-any_of(to_drop))
    }
  )
  
  list(extdata_y = extdata_list$y, extdata_q = extdata_list$q, extdata_m = extdata_list$m, extdata_d = extdata_list$d)
  
}

##### Function to check if all the necessary files for import exist

checkFileExistence <- function(impplan, extdata_folder) {
  
  impplan_temp <- impplan |> filter(retrieve_type == "file") |> 
      mutate(check_exist = 1*file.exists(here(extdata_folder, file_name))) |>
      select(indicator_code, source_frequency, check_exist)
  
  impplan <- impplan |> left_join(impplan_temp, by = c("indicator_code" = "indicator_code", "source_frequency" = "source_frequency"))
  return(impplan)
  
}

##### Main import function for APIs and local files 

tryImport <- function(impplan, extdata_y, extdata_q, extdata_m, extdata_d, impparams) {
      
    ##### Import WDI yearly
    try({
      
      wdiy_impplan <- impplan |> filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="y")
      wdiy_names <- wdiy_impplan$indicator_code
      wdiy_codes <- wdiy_impplan$retrieve_code
      
      if (length(wdiy_names)>0 & all(!is.na(wdiy_names))) {
      
        print("WDI-y")
        #### !!!! разобраться с объемом пакета, в следующей строчке - костыль, снижающий его искуственно, 
        #### !!!!  не забывать двигать год !!!!
        for (i in seq_along(wdiy_names)) {
          wdiy_data <- WDI(indicator = wdiy_codes[i], start = max(year_first, 1980), end = min(year_final, 2025), extra=F) |> 
            select(-c(country)) |> rename_at(vars(any_of(wdiy_codes[i])), ~wdiy_names[i])
          
          extdata_y <- extdata_y |> left_join(wdiy_data, by = c("country_id"="iso2c", "year"="year"), suffix=c("","_old"))
          print(wdiy_names[i])
        }
        print("+")
        }
    
    })
  
    ##### Import WDI quarterly
    try({
      
      wdiq_impplan <- impplan |> filter(active==1, database_name=="WDI", retrieve_type=="API", source_frequency=="q")
      wdiq_names <- wdiq_impplan$indicator_code
      wdiq_codes <- wdiq_impplan$retrieve_code
      
      if (length(wdiq_names)>0 & all(!is.na(wdiq_names))) {
        
        print("WDI-q")
        #### !!!! разобраться с объемом пакета, в следующей строчке - костыль, снижающий его искуственно, 
        #### !!!!  не забывать двигать год !!!!
        for (i in seq_along(wdiq_names)) {
          
          wdiq_data <- WDI(indicator = wdiq_codes[i], start = max(year_first, 2007), end = min(year_final, 2025), extra=F) |> select(-c(country, iso3c)) |>
            mutate(quarter = as.numeric(substr(year, 6, 6)), year = as.numeric(substr(year, 1, 4))) |>
            mutate(iso2c = countrycode(iso2c, origin = 'iso3c', destination = 'iso2c', 
                                       custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                        'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) |>
            rename_at(vars(any_of(wdiq_codes[i])), ~wdiq_names[i])
          
          extdata_q <- extdata_q |> left_join(wdiq_data, by = c("country_id"="iso2c", "year"="year", "quarter"="quarter"), suffix=c("","_old"))
          print(wdiq_names[i])
        }
        print("+")
      }
      
    })
  
    ##### Import WGI
    try({
      
      wgi_impplan <- impplan |> filter(active==1, database_name=="WGI", retrieve_type=="file", source_frequency=="y") |>
        mutate(wgi_type = unlist(strsplit(retrieve_code, "/"))[c(T,F)],  wgi_var = unlist(strsplit(retrieve_code, "/"))[c(F,T)])
      wgi_names <- wgi_impplan$indicator_code
      wgi_fname <- here("assets", "_DB", "_extsources", wgi_impplan$file_name[1])
      wgi_sheets <- wgi_impplan$sheet_name
      wgi_type <- wgi_impplan$wgi_type
      wgi_var <- wgi_impplan$wgi_var
      
      if (length(wgi_names)>0 & all(!is.na(wgi_names))) {
         
        wgi_data_full <- read_excel(wgi_fname[1], sheet = wgi_sheets[1], col_names = T, na = "..", skip=0)
         
        print("WGI")
        for (i in seq_along(wgi_names)) {
            
            wgi_data_full |> select(code, year, indicator, !!sym(wgi_type[i])) |> filter(indicator == wgi_var[i]) |>
              mutate(country_id = countrycode(code, origin = 'iso3c', destination = 'iso2c',
                                            custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                             'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) |>
              rename(!!sym(wgi_names[i]) := !!sym(wgi_type[i])) |> select(-c("code", "indicator")) -> wgi_data
          
            extdata_y |> left_join(wgi_data, by = c("country_id" = "country_id", "year"="year"), suffix = c("", "_wgi")) -> extdata_y
            print("+")
            
          }
      
      }
    
    })
    
    ##### Import UNCTAD export diversification index
    try({
      
      unctad_impplan <- impplan |> filter(active==1, source_name=="UNCTAD", retrieve_type=="file", source_frequency=="y")
      unctad_names <- unctad_impplan$indicator_code
      unctad_fname <- here("assets", "_DB", "_extsources", unctad_impplan$file_name[1])
    
      if (length(unctad_names)>0 & all(!is.na(unctad_names))) {
        
        print("UNCTAD")
        unctad_data <- read.csv(unctad_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"),
                           colClasses=c("Economy"="character"), skip=0)

        unctad_data |> filter (Flow.Label=="Exports") |>
          select(Year, Economy, Economy.Label, Diversification.Index) |> mutate(Economy = recode(Economy, '757' = '756')) |>  # CH+LI -> CH
          mutate(country_id = countrycode(as.numeric(Economy), origin = 'un', destination = 'iso2c')) |>
          select(-c(Economy.Label, Economy)) |> rename("year"="Year") -> unctad_data
        
        unctad_data <- eval(parse(text = glue("rename(unctad_data,'{unctad_names}'='Diversification.Index')") ))
        
        extdata_y |> left_join(unctad_data, by = c("country_id" = "country_id", "year"="year"),
                                suffix=c("","_old")) -> extdata_y
        print("+")
        
      }
    
    })
  
    ##### Import yearly IMF data
    try({
      
      imfy_impplan <- impplan |> filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="y")
      imfy_dbnames <- imfy_impplan$database_name
      imfy_names <- imfy_impplan$indicator_code
      imfy_codes <- imfy_impplan$retrieve_code
      
      if (length(imfy_names)>0 & all(!is.na(imfy_names))) {
        
        message("IMF-Y")
        
        imf_data_in <- NULL
        
        for (i in seq_along(imfy_codes)) {
          message(imfy_codes[i])
          
          new_data <- imfTool(database = imfy_dbnames[i], code = imfy_codes[i], freq = "y", 
                              start = year_first, end = year_final) |>
            dplyr::select(iso2, year, value) |> dplyr::rename(!!imfy_codes[i] := value)
          
          imf_data_in <-
            if (is.null(imf_data_in)) new_data
            else dplyr::full_join(imf_data_in, new_data, by = c("iso2", "year"))
        }
        
        imf_data_in <- imf_data_in |> dplyr::rename(country_id = iso2) |>
          dplyr::rename_with(.cols = dplyr::any_of(imfy_codes), .fn = ~ imfy_names)
        
        extdata_y <- extdata_y |> dplyr::left_join(imf_data_in, by = c("country_id", "year"))
        
        message("+")
        
      }
    
    })
      
    ##### Import quarterly IMF data
    try({
      
      imfq_impplan <- impplan |> filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="q")
      imfq_dbnames <- imfq_impplan$database_name
      imfq_names <- imfq_impplan$indicator_code
      imfq_codes <- imfq_impplan$retrieve_code
      
      if (length(imfq_names)>0 & all(!is.na(imfq_names))) {
      
        message("IMF-Q")
        
        imf_data_in <- NULL
        
        for (i in seq_along(imfq_codes)) {
          
          message(imfq_codes[i])
          
          new_data <- imfTool(database = imfq_dbnames[i], code = imfq_codes[i], freq = "q",
              start = ifelse(imfq_dbnames[i] == "BOP", 2011, year_first), end = year_final) |>
            dplyr::select(iso2, year, quarter, value) |> dplyr::rename(!!imfq_codes[i] := value)
            
          imf_data_in <-
            if (is.null(imf_data_in)) new_data
            else dplyr::full_join(imf_data_in, new_data, by = c("iso2", "year", "quarter"))
        }
        
        imf_data_in <- imf_data_in |> dplyr::rename(country_id = iso2) |>
          dplyr::rename_with(.cols = dplyr::any_of(imfq_codes), .fn = ~ imfq_names)
        
        extdata_q <- extdata_q |> dplyr::left_join(imf_data_in, by = c("country_id", "year", "quarter"))
        
        message("+")
        
      }  
    
    })
    
    ##### Import monthly IMF data
    try({
      
      imfm_impplan <- impplan |> filter(active==1, source_name=="IMF", retrieve_type=="API",
                                         source_frequency=="m")
      imfm_dbnames <- imfm_impplan$database_name
      imfm_names <- imfm_impplan$indicator_code
      imfm_codes <- imfm_impplan$retrieve_code
      
      if (length(imfm_names)>0 & all(!is.na(imfm_names))) {
        
        message("IMF-M")
        
        imf_data_in <- NULL
        
        for (i in seq_along(imfm_codes)) {
          message(imfm_codes[i])
          
          new_data <- imfTool(database = imfm_dbnames[i], code = imfm_codes[i], freq = "m",
              start = ifelse(imfm_dbnames[i] == "BOP", 2011, year_first), end = year_final) |>
            dplyr::select(iso2, year, month, value) |> dplyr::rename(!!imfm_codes[i] := value)
          
          imf_data_in <-
            if (is.null(imf_data_in)) new_data
            else dplyr::full_join(imf_data_in, new_data, by = c("iso2", "year", "month"))
        }
        
        imf_data_in <- imf_data_in |> dplyr::rename(country_id = iso2) |>
          dplyr::rename_with(.cols = dplyr::any_of(imfm_codes), .fn = ~ imfm_names)
        
        extdata_m <- extdata_m |> dplyr::left_join(imf_data_in, by = c("country_id", "year", "month"))
        
        message("+")
        
      }
    
    })
      
    ##### Import IDS external debt statistics
    try({
      
      ids_impplan <- impplan |> filter(active==1, database_name=="IDS", retrieve_type=="file", source_frequency=="y")
      ids_names <- ids_impplan$indicator_code
      ids_fname <- here("assets", "_DB", "_extsources", ids_impplan$file_name[1])
      ids_sheets <- unique(ids_impplan$sheet_name)
      ids_codes <- ids_impplan$retrieve_code
      
      if (length(ids_names)>0 & all(!is.na(ids_names))) {
        
        print("IDS")
        ids_data <- read_excel(ids_fname, sheet = ids_sheets, col_names = T, na = "#N/A")
        ids_data <- ids_data |> rename('country'='Country Name', 'country_id'='Country Code', 'variable_code'='Series Code', 
                              'variable_name'='Series Name', 'counterpart_name'='Counterpart-Area Name', 'counterpart_code'='Counterpart-Area Code') |> 
                              filter(counterpart_name=="World") |> select(-c('counterpart_code', 'counterpart_name', 'country', 'variable_name')) |>
                              filter(variable_code %in% ids_codes)
        
        ids_data <- reshape2::melt(ids_data, id.vars = c("country_id", "variable_code"), variable.name = "year", value.name = "value") |> 
            mutate(year = as.numeric(as.character(year))) |>
            left_join(ids_impplan |> select(indicator_code, retrieve_code), by=c('variable_code'='retrieve_code'), suffix=c("","_old")) |>
            select(-c(variable_code)) |> pivot_wider(names_from = "indicator_code", values_from = "value") |>
            mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                                          custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                          'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F))
          
        extdata_y <- extdata_y |> left_join(ids_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
        print("+")
        
      }
    
    })  
      
    ##### Import daily and monthly BIS data on policy rates and exchange rates
    try({
      
      bis_impplan <- impplan |> filter(active==1, source_name=="BIS", retrieve_type=="file", 
                                        database_name %in% c("Policy rates (flat)", "US dollar exchange rates (flat)"))
      bis_names <- bis_impplan$indicator_code
      bis_fname <- here("assets", "_DB", "_extsources", bis_impplan$file_name)
      bis_freq <- toupper(bis_impplan$source_frequency)
      bis_codes <- bis_impplan$retrieve_code

      suppressMessages({
        EZ_countries <- read_excel(here("assets", "_DB","1_peers_params.xlsx"), sheet = "groups", col_names = F, skip=2, n_max=11)[c(2,11),-c(1:3)]
      })
      EZ_countries <- data.frame(t(EZ_countries)) |> filter(X1 == 1) |> select(X2) |> unlist()
      EZ_countries <- countrycode(EZ_countries, origin = 'iso3c', destination = 'iso2c', 
                                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                          'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)
      
      if (length(bis_names)>0 & all(!is.na(bis_names))) {
        
        print("BIS policy and exchange rates")
        for (i in seq_along(bis_names)) {  

            bis_data <- read.csv(bis_fname[i], header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", na.strings=c("..","","NA"), skip=0) |>
              select(any_of(c("REF_AREA.Reference.area", "TIME_PERIOD.Time.period.or.range", "OBS_VALUE.Observation.Value", "FREQ.Frequency", "COLLECTION.Collection"))) |>
              rename(ref_area = REF_AREA.Reference.area, time_period = TIME_PERIOD.Time.period.or.range, value = OBS_VALUE.Observation.Value, freq = FREQ.Frequency) |>
              mutate(country_id = substr(ref_area,1,2), freq = substr(freq,1,1), value = as.numeric(value)) 

            if (!is.na(bis_codes[i])) {bis_data <- bis_data |> rename(aveop = COLLECTION.Collection) |> mutate(aveop = substr(aveop,1,1)) |> 
              filter(aveop == bis_codes[i]) }
            bis_data <- bis_data |> filter(freq == bis_freq[i]) |> select(time_period, country_id, value)

            
            # managing EZ countries (для этих стран нужно закоалесить имеющийся ряд и XM) 
            EZ_rate <- bis_data |> filter(country_id == "XM") |> select(time_period, value)
            EZ_full <- bis_data |> filter(country_id == "US") |> select(time_period) |> 
              left_join(EZ_rate, by=c("time_period"="time_period")) |> rename("new_value" = "value")
            
            for (j in EZ_countries) {
              
              EZ_country_old <- bis_data |> filter(country_id == j) |> select(time_period, value) |> 
                rename("old_value" = "value")
              if (dim(EZ_country_old)[1] == 0) {EZ_country_old <- EZ_full |> mutate(old_value = NA) |> 
                select(time_period, old_value) }
              EZ_country_new <- EZ_full |> left_join(EZ_country_old, by=c("time_period"="time_period"))
              EZ_country_new <- EZ_country_new |> mutate(value = coalesce(old_value, new_value), country_id = j) |> 
                select(time_period, country_id, value)

              bis_data <- bis_data |> filter(country_id != j)
              bis_data <- bind_rows(bis_data, EZ_country_new)
            }
                         
            if (bis_freq[i] == "D") {
              bis_data <- bis_data |> mutate(date = as.Date(time_period, "%Y-%m-%d")) |> select(country_id, date, value)
              bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
              extdata_d <- extdata_d |> left_join(bis_data, by = c("country_id" = "country_id", "date"="date"), suffix=c("","_old")) 
              }
          
            if (bis_freq[i] == "M") {
              bis_data <- bis_data |> mutate(year = as.numeric(substr(time_period, 1, 4)), month = as.numeric(substr(time_period, 6, 7))) |>
                  select(country_id, year, month, value)
              bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
              extdata_m <- extdata_m |> left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
              }

          print(bis_names[i])
        
        }
      }  
    
    })  
    
    ##### Import monthly BIS data on effective exchange rates
    try({
      
      bis_impplan <- impplan |> filter(active==1, source_name=="BIS", retrieve_type=="file", database_name == "Effective exchange rate indices (monthly)")
      bis_names <- bis_impplan$indicator_code
      bis_fname <- here("assets", "_DB", "_extsources", bis_impplan$file_name)
      bis_freq <- toupper(bis_impplan$source_frequency)
      bis_codes <- bis_impplan$retrieve_code
      
      if (length(bis_names)>0 & all(!is.na(bis_names))) {
        
        print("BIS effective exchange rates")
        for (i in seq_along(bis_names)) {  
          
          bis_data <- read.csv(bis_fname[i], header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", 
                               na.strings=c("..","","NA"), skip=0) |>
            select(all_of(c("REF_AREA.Reference.area", "TIME_PERIOD.Time.period.or.range", "OBS_VALUE.Observation.Value", "FREQ.Frequency", 
                            "EER_TYPE.Type", "EER_BASKET.Basket"))) |>
            rename(ref_area = REF_AREA.Reference.area, time_period = TIME_PERIOD.Time.period.or.range, value = OBS_VALUE.Observation.Value, 
                   freq = FREQ.Frequency, type = EER_TYPE.Type, basket = EER_BASKET.Basket) |>
            mutate(country_id = substr(ref_area,1,2), freq = substr(freq,1,1), type = substr(type,1,1), basket = substr(basket,1,1), value = as.numeric(value)) |>
            filter(freq == bis_freq[i], type == bis_codes[i], basket == "B") |> select(time_period, country_id, value)
          
            bis_data <- bis_data |> mutate(year = as.numeric(substr(time_period, 1, 4)), month = as.numeric(substr(time_period, 6, 7))) |>
              select(country_id, year, month, value)
            bis_data <- eval(parse(text = glue("rename(bis_data,'{bis_names[i]}'='value')") ))
            extdata_m <- extdata_m |> left_join(bis_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
          
          print(bis_names[i])
          
        }
      }  
      
    })    
    
    ##### Import IMF Fiscal monitor structural indicators
    try({
      
      fm_impplan <- impplan |> filter(active==1, database_name=="FM", retrieve_type=="file", source_frequency=="y")
      fm_names <- fm_impplan$indicator_code
      fm_fname <- here("assets", "_DB", "_extsources", fm_impplan$file_name[1])
      fm_sheets <- fm_impplan$sheet_name
      
      if (length(fm_names)>0 & all(!is.na(fm_names))) {
        
        print("IMF-FM")
        for (i in seq_along(fm_names)) {
        
          #i=3
          fm_data <- read_excel(fm_fname[i], sheet = fm_sheets[i], col_names = T, na = "#N/A", col_types='text')
          
          fm_data <- fm_data |> pivot_longer(cols = !contains('country'), names_to = 'year', values_to = 'value') |>
            mutate(country_id = countrycode(country_code, origin = 'iso3c', destination = 'iso2c',  warn = F,
                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')),
                year = as.numeric(as.character(year)), value = as.numeric(value) ) |> select(country_id, year, value)
          
          fm_data <- eval(parse(text = glue("rename(fm_data,'{fm_names[i]}'='value')") ))
            
          extdata_y <- extdata_y |> left_join(fm_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
          }
      
      }
    
    })
      
    ##### Import ILOstat data
    try({
        
      # search
      # toc <- get_ilostat_toc(search = 'informal')
      # toc2 <- toc |> select(-c(indicator, freq.label, collection.label, subject)) |> filter(freq == "A")
      # toc2$indicator.label
      
      ilo_impplan <- impplan |> filter(active==1, source_name=="ILO", retrieve_type=="API")
      ilo_names <- ilo_impplan$indicator_code
      ilo_codes <- ilo_impplan$retrieve_code
      ilo_freq <- ilo_impplan$source_frequency
      
      if (length(ilo_names)>0 & all(!is.na(ilo_names))) {
        
        print("ILOstat")
        for (i in seq_along(ilo_names)) {
          
          #i=1
          ilo_data <- get_ilostat(id = ilo_codes[i], segment = 'indicator')
          
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")))) {ilo_data <- ilo_data |> filter(sex == "SEX_T",  classif1 == "AGE_YTHADULT_YGE15")}
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("TRUE", "FALSE"))) {ilo_data <- ilo_data |> filter(sex == "SEX_T")}
          if (all(str_detect(ilo_codes[i], c("SEX", "AGE")) == c("FALSE", "TRUE"))) {ilo_data <- ilo_data |> filter(classif1 == "AGE_YTHADULT_YGE15")}
          
          ilo_data <- ilo_data |> 
                mutate(ref_area = countrycode(ref_area, origin = 'iso3c', destination = 'iso2c', warn = F,
                  custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')))
          
          if (ilo_freq[i] == "y") {ilo_data <- ilo_data |> mutate(year = as.numeric(time)) |> select(ref_area, year, obs_value)}
          if (ilo_freq[i] == "q") {ilo_data <- ilo_data |> mutate(year = as.numeric(substr(time, 1, 4)), quarter = as.numeric(substr(time, 6, 6))) |> select(ref_area, year, quarter, obs_value)}
          if (ilo_freq[i] == "m") {ilo_data <- ilo_data |> mutate(year = as.numeric(substr(time, 1, 4)), month = as.numeric(substr(time, 6, 7))) |> select(ref_area, year, month, obs_value)}
          
          ilo_data <- eval(parse(text = glue("rename(ilo_data,'{ilo_names[i]}'='obs_value')") ))
          
          if (ilo_freq[i] == "y") {extdata_y <- extdata_y |> left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year"), suffix=c("","_old"))}
          if (ilo_freq[i] == "q") {extdata_q <- extdata_q |> left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "quarter" = "quarter"), suffix=c("","_old"))}
          if (ilo_freq[i] == "m") {extdata_m <- extdata_m |> left_join(ilo_data, by = c("country_id" = "ref_area", "year"="year", "month" = "month"), suffix=c("","_old"))}
          print("+")
          
          }
      
      }
    
    })
      
    ##### Import daily data on COVID
    try({  
    
      covid_impplan <- impplan |> filter(active==1, source_name=="Ourworldindata", retrieve_type=="file", database_name== "COVID tracker")
      covid_names <- covid_impplan$indicator_code
      covid_codes <- covid_impplan$retrieve_code
      covid_fname <- here("assets", "_DB", "_extsources", covid_impplan$file_name[1])
      
      if (length(covid_names)>0 & all(!is.na(covid_names))) {
        
        print("OWID")
        covid_data <- read.csv(covid_fname, header = TRUE, sep = ",", quote = "\"",
                             dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"), skip=0) |>
                    mutate(
                      date = as.Date(date),
                      country_id = countrycode(iso_code, origin = 'iso3c', destination = 'iso2c', warn = F,
                          custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))
                    )
        
        eval(parse(text = glue("covid_data <- covid_data |> select(date, country_id, {paste(covid_codes, collapse=', ')}) |> \\
                               rename_at(vars(any_of(covid_codes)), ~covid_names) ") ))
        extdata_d <- extdata_d |> left_join(covid_data, by=c("country_id", "date"), suffix=c("","_old"))
        print("+")
        
      }
    
    })
      
    ##### Import UN HDR data
    try({
        
      unhdr_impplan <- impplan |> filter(active==1, source_name=="UN", retrieve_type=="file", database_name=="HDR")
      unhdr_names <- unhdr_impplan$indicator_code
      unhdr_codes <- unhdr_impplan$retrieve_code
      unhdr_fname <- here("assets", "_DB", "_extsources", unhdr_impplan$file_name)
      
      if (length(unhdr_names)>0 & all(!is.na(unhdr_names))) {
        
        print("UN-HDR")
        
        for (i in 1:length(unhdr_names)) {
          
          unhdr_data <- read.csv(unhdr_fname[i], header = TRUE, sep = ",", quote = "\"",
                               dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
          eval(parse(text = glue("unhdr_data <- unhdr_data |> select(iso3, starts_with('{unhdr_codes[i]}_1'), starts_with('{unhdr_codes[i]}_2'))") ))
            
          unhdr_data <- unhdr_data |> pivot_longer(!iso3, names_to = "year", values_to = "value") |> 
            mutate(year = as.numeric(str_sub(year, -4, -1)), value = as.numeric(value)) |>
            mutate(iso3 = countrycode(iso3, origin = 'iso3c', destination = 'iso2c', warn = F,
                                custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD')) )
          
          eval(parse(text = glue("unhdr_data <- unhdr_data |> rename('{unhdr_names[i]}' = 'value', 'country_id' = 'iso3')") ))
          
          extdata_y |> left_join(unhdr_data, by = c("country_id" = "country_id", "year" = "year"),
                                  suffix=c("","_old")) -> extdata_y
          print("+")
          
        }
      }
    
    })  
      
    ##### Import Chinn-Ito financial system classification
    try({
        
      ci_impplan <- impplan |> filter(active==1, database_name=="Chinn-Ito", retrieve_type=="file", source_frequency=="y")
      ci_names <- ci_impplan$indicator_code
      ci_codes <- ci_impplan$retrieve_code
      ci_fname <- here("assets", "_DB", "_extsources", ci_impplan$file_name[1])
      ci_sheets <- ci_impplan$sheet_name
      
      if (length(ci_names)>0 & all(!is.na(ci_names))) {
        
        print("Chinn-Ito")
        for (i in seq_along(ci_names)) {
          
          #i=1
          ci_data <- read_excel(ci_fname[i], sheet = ci_sheets[i], col_names = T, na = "#N/A", col_types='text')
          
          ci_data <- eval(parse(text = glue("ci_data |> select('IMF-World Bank Country Code', year,'{ci_codes[i]}')") ))
          ci_data <- eval(parse(text = glue("rename(ci_data,'country_id'='IMF-World Bank Country Code', 'value'='{ci_codes[i]}')") ))
          
          ci_data <- ci_data |> mutate(country_id = as.numeric(country_id), country_id = countrycode(country_id, origin = 'imf', destination = 'iso2c'),
                   year = as.numeric(year), value = as.numeric(value) ) |> select(country_id, year, value)
          
          ci_data <- eval(parse(text = glue("rename(ci_data,'{ci_names[i]}'='value')") ))
          
          extdata_y <- extdata_y |> left_join(ci_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
        }
        
      }
      
    })  
    
    ##### Import WEO outlook
    try({
      
      weo_impplan <- impplan |> filter(active==1, database_name=="WEO", retrieve_type=="file", source_frequency=="y")
      weo_names <- weo_impplan$indicator_code
      weo_codes <- weo_impplan$retrieve_code
      weo_fname <- here("assets", "_DB", "_extsources", weo_impplan$file_name[1])
      weo_sheets <- weo_impplan$sheet_name
      
      if (length(weo_names)>0 & all(!is.na(weo_names))) {
        
        print("IMF-WEO")
        for (i in seq_along(weo_names)) {
          
          #weo_fname = "./_extsources/WEO.xls"
          #i=1
          #weo_data <- read_tsv(weo_fname, na = c("", "NA", "n/a"), col_types = "c", show_col_types = FALSE)
          weo_data <- read_excel(weo_fname, sheet = weo_sheets[i], na = c("", "NA", "n/a"))
          weo_data <- weo_data |> rename('country_id' = 'ISO', 'code' = 'WEO Subject Code') |>
            mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = gsub, pattern = ",", replacement = "") |>
            mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = as.numeric)
          weo_data <- eval(parse(text = glue("weo_data |> filter(code == '{weo_codes[i]}')") ))
          
          weo_data <- weo_data |> select(country_id, starts_with('19'), starts_with('20')) |>
                mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN','KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'))) |>
                pivot_longer(!country_id, names_to = "year", values_to = "value") |>
                mutate(year = as.numeric(year), value = as.numeric(value)) |>
                mutate(value = value + 0.000006) # needed to prevent excel from treating as dates

          weo_data <- eval(parse(text = glue("rename(weo_data,'{weo_names[i]}'='value')") ))
          
          extdata_y <- extdata_y |> left_join(weo_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
          
        }
        
      }
    
    })
  
  ##### Import WEO outlook for aggregates
  try({
    
    weo_impplan <- impplan |> filter(active==1, database_name=="WEO_aggr", retrieve_type=="file", source_frequency=="y")
    weo_names <- weo_impplan$indicator_code
    weo_codes <- weo_impplan$retrieve_code
    weo_fname <- here("assets", "_DB", "_extsources", weo_impplan$file_name[1])
    weo_sheets <- weo_impplan$sheet_name
    
    if (length(weo_names)>0 & all(!is.na(weo_names))) {
      
      print("IMF-WEO-aggr")
      for (i in seq_along(weo_names)) {
        
        #i=1
        #weo_data <- read_tsv(weo_fname, na = c("", "NA", "n/a"), col_types = "c", show_col_types = F)
        weo_data <- read_excel(weo_fname, sheet = weo_sheets[i], na = c("", "NA", "n/a"))        
        weo_data <- weo_data |> rename('code' = 'Country Group Name', 'indicator' = 'WEO Subject Code', 
                        'note' = 'Country/Series-specific Notes') |>
          mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = gsub, pattern = ",", replacement = "") |>
          mutate_at(.vars = vars(starts_with('19'), starts_with('20')), .funs = as.numeric)
        weo_data <- eval(parse(text = glue("weo_data |> filter(code == '{weo_codes[i]}', indicator == 'NGDP_RPCH')") ))
        
        weo_data <- weo_data |> select(starts_with('19'), starts_with('20')) |> 
          mutate(country_id = "1W") |> pivot_longer(!country_id, names_to = "year", values_to = "value") |>
          mutate(year = as.numeric(year), value = as.numeric(value))
        
        weo_data <- eval(parse(text = glue("rename(weo_data,'{weo_names[i]}'='value')") ))
        
        extdata_y <- extdata_y |> left_join(weo_data, by = c("country_id" = "country_id", "year" = "year"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  })  
  
  ##### Import UNPD aggregated data
  try({
    
    pp_impplan <- impplan |> filter(active==1, database_name=="WPP_aggr", retrieve_type=="file", source_frequency=="y")
    pp_names <- pp_impplan$indicator_code
    pp_codes <- pp_impplan$retrieve_code
    pp_fname <- here("assets", "_DB", "_extsources", pp_impplan$file_name[1])
    
    if (length(pp_names)>0 & all(!is.na(pp_names))) {
      
      print("UNPD aggregates")
      for (i in seq_along(pp_names)) {
        
        #i=1
        pp_data <- read.csv(pp_fname, header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
        
        pp_data <- eval(parse(text = glue("pp_data |> select('ISO2_code', 'Time', 'Variant', '{pp_codes[i]}')") ))
        pp_data <- eval(parse(text = glue("rename(pp_data,'country_id'='ISO2_code', 'year' = 'Time', 'value'='{pp_codes[i]}')") ))
        
        pp_data <- pp_data |> filter(Variant == "Medium", !is.na(country_id)) |> mutate(year = as.numeric(year), value = as.numeric(value) ) |> 
              select(country_id, year, value)
        
        pp_data <- eval(parse(text = glue("rename(pp_data,'{pp_names[i]}'='value')") ))
        
        extdata_y <- extdata_y |> left_join(pp_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  }) 
  
  ##### Import UNPD 5-year groups data
  try({
    
    pp_impplan <- impplan |> filter(active==1, database_name=="WPP_5yr", retrieve_type=="file", source_frequency=="y")
    pp_names <- pp_impplan$indicator_code
    pp_codes <- pp_impplan$retrieve_code
    pp_dict <- data.frame(pp_codes, pp_names)
    pp_fname <- here("assets", "_DB", "_extsources", pp_impplan$file_name[1])
    
    if (length(pp_names)>0 & all(!is.na(pp_names))) {
      
      print("UNPD 5-year groups")
        
      pp_data <- read.csv(pp_fname, header = TRUE, sep = ",", quote = "\"",
                            dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA"))
        
      pp_data <- pp_data |> select('ISO2_code', 'Time', 'Variant', 'AgeGrp', 'PopTotal') |> filter(Variant == "Medium") |>
                rename('country_id' = 'ISO2_code', 'year' = 'Time', 'age' = 'AgeGrp', 'value' = 'PopTotal') |> 
                mutate(year = as.numeric(year), value = as.numeric(value)) |> 
                filter(age %in% pp_codes, Variant == "Medium", !is.na(country_id)) |> select(-c(Variant)) 
      
      pp_data <- pp_data |> left_join(pp_dict, by = c('age' = 'pp_codes')) |> select(-c(age)) |>
          pivot_wider(names_from = pp_names, values_from = value)

      extdata_y <- extdata_y |> left_join(pp_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
    }
    
  })
  
  ##### Import data on debt held in reserves
  try({
    
    hres_impplan <- impplan |> filter(active==1, database_name=="BondsInReserves", retrieve_type=="file", source_frequency=="y")
    hres_names <- hres_impplan$indicator_code[1]
    hres_fname <- here("assets", "_DB", "_extsources", hres_impplan$file_name[1])
    hres_sheets <- hres_impplan$sheet_name[1]
    
    if (length(hres_names)>0 & all(!is.na(hres_names))) {
      
      print("BondsInReserves")
      
      hres_data <- read_excel(hres_fname, sheet = hres_sheets, skip = 3, col_names = T, na = "C")
      names(hres_data)[1] <- "country_id"
      hres_data <- hres_data |> select(country_id, contains("DEC")) |> 
        pivot_longer(!country_id, names_to = "year", values_to = "value") |>
        mutate(country_id = countrycode(country_id, origin = 'country.name', destination = 'iso2c', custom_match = c('World' = '1W'), warn = F)) |> 
        mutate(year = as.numeric(str_sub(year, 6, 9)))
      
      hres_data <- eval(parse(text = glue("rename(hres_data,'{hres_names[1]}'='value')") ))
      
      extdata_y <- extdata_y |> left_join(hres_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
    }
    
  })
    
  ##### Import data from Fiscal Space Database
  
  try({
    
    fsdb_impplan <- impplan |> filter(active==1, database_name=="FSDB", retrieve_type=="file", source_frequency=="y")
    fsdb_names <- fsdb_impplan$indicator_code
    fsdb_sheets <- fsdb_impplan$sheet_name
    
    
    if (length(fsdb_names)>0 & all(!is.na(fsdb_names))) {
      
      print("FSDB")
      fsdb_fname <- here("assets", "_DB", "_extsources", fsdb_impplan$file_name[1])
      
      for(i in seq_along(fsdb_names)) {
      
      fsdb_data <- read_excel(fsdb_fname, sheet = fsdb_sheets[i], col_names = T, na = "")
      names(fsdb_data)[1] <- "country_id"
      fsdb_data <- fsdb_data |> select(country_id, starts_with('19'), starts_with('20')) |> 
        pivot_longer(!country_id, names_to = "year", values_to = "value") |>
        mutate(country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', warn = F)) |> 
        mutate(year = as.numeric(year), value = as.numeric(value))
      
      fsdb_data <- eval(parse(text = glue("rename(fsdb_data,'{fsdb_names[i]}'='value')") ))
      
      extdata_y <- extdata_y |> left_join(fsdb_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
      }
      
    }
    
  })
  
  ##### Import data from the Integrated Macroprudential Policy (iMaPP) Database
  
  try({
    
    imapp_impplan <- impplan |> filter(active==1, database_name=="iMaPP", retrieve_type=="file", source_frequency=="m")
    imapp_names <- imapp_impplan$indicator_code
    imapp_sheets <- unique(imapp_impplan$sheet_name)
    
    
    if (length(imapp_names)>0 & all(!is.na(imapp_names))) {
      
      print("iMaPP")
      imapp_fname <- here("assets", "_DB", "_extsources", imapp_impplan$file_name[1])
      
      for(i in seq_along(imapp_sheets)) {
        
        old_codes <- imapp_impplan |> filter(sheet_name == imapp_sheets[i]) |> pull(retrieve_code)
        new_codes <- imapp_impplan |> filter(sheet_name == imapp_sheets[i]) |> pull(indicator_code)
        imapp_data <- read_excel(imapp_fname, sheet = imapp_sheets[i], col_names = T, na = "") |>
          rename_at(vars(c("iso2", "Year", "Month")), ~c("country_id", "year", "month")) |>
          rename_at(vars(old_codes), ~new_codes) |> select("country_id", "year", "month", all_of(new_codes)) |>
          mutate(year = as.numeric(year), month = as.numeric(month))
        
        extdata_m <- extdata_m |> left_join(imapp_data, by = c("country_id" = "country_id", "year"="year", "month"="month"), suffix=c("","_old"))
        print("+")
        
      }
      
    }
    
  })  

  ##### Import data on the financial systems structure (from Financial Stability Board)
  
  try({
    
    fsb_impplan_temp <- impplan |> filter(active==1, source_name=="FSB", retrieve_type=="file")
    fsb_parts <- unique(fsb_impplan_temp$database_name)
    
    for (i in fsb_parts) {
      
      fsb_impplan <- fsb_impplan_temp |> filter(database_name==i)
      #fsb_impplan <- fsb_impplan_temp |> filter(database_name==fsb_parts[2])
      fsb_names <- fsb_impplan$indicator_code
      fsb_sheet <- unique(fsb_impplan$sheet_name)[1]
      
      if (length(fsb_names)>0 & all(!is.na(fsb_names))) {
        
        print("FSB")
        fsb_fname <- here("assets", "_DB", "_extsources", fsb_impplan$file_name[1])
        old_codes <- fsb_impplan |> filter(sheet_name == fsb_sheet) |> pull(retrieve_code)
        new_codes <- fsb_impplan |> filter(sheet_name == fsb_sheet) |> pull(indicator_code)
        
        fsb_data <- read_excel(fsb_fname, sheet = fsb_sheet, col_names = T, na = "") |>
            rename_at(vars(c("Jurisdiction code", "Year", "Entity/Economic function", "Value, in USD trillions")),
                      ~c("country_id", "year", "code", "value")) |> mutate(value = as.numeric(value))  |>
          filter(Topic == i) |> select("country_id", "year", "code", "value") |> 
          pivot_wider(id_cols = c("country_id", "year"), names_from = "code", values_from = "value") |>
            rename_at(vars(old_codes), ~new_codes) |>
            mutate(year = as.numeric(year))
          
          extdata_y <- extdata_y |> left_join(fsb_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
          print("+")
      
      }  
    }
    
  })
  
  
  ##### Import data from the Global Macro Data
  
  try({
    
    gmd_impplan <- impplan |> filter(active==1, database_name=="GMD", retrieve_type=="file", source_frequency=="y")
    gmd_names <- gmd_impplan$indicator_code
    
    
    if (length(gmd_names)>0 & all(!is.na(gmd_names))) {
      
      print("GMD")
      gmd_fname <- here("assets", "_DB", "_extsources", gmd_impplan$file_name[1])
        
      old_codes <- gmd_impplan |> pull(retrieve_code)
      new_codes <- gmd_impplan |> pull(indicator_code)
      gmd_data <- read.csv(gmd_fname, header = TRUE, sep = ",", quote = "\"",
                           dec = ".", fill = TRUE, comment.char = "", na.strings=c(0,"..","","NA")) |>
        mutate(country_id = countrycode(ISO3, origin = 'iso3c', destination = 'iso2c', 
                                 custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN',
                                                  'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) |>
        rename_at(vars(old_codes), ~new_codes) |> select("country_id", "year", all_of(new_codes)) |>
        mutate(year = as.numeric(year)) 
        
      extdata_y <- extdata_y |> left_join(gmd_data, by = c("country_id" = "country_id", "year"="year"), suffix=c("","_old"))
      print("+")
      
    }
    
  })
  
  ##### Import data from the Conference Board
  
  try({
    
    for (i in c("CB_GrowthAccounting.xlsx", "CB_GrowthFactors.xlsx")) {
      
      cby_impplan <- impplan |> filter(active == 1, file_name == i, retrieve_type == "file", source_frequency == "y")
      cby_codes <- cby_impplan$retrieve_code
      cby_names <- cby_impplan$indicator_code
      
      if (length(cby_codes) > 0 && all(!is.na(cby_codes))) {
        
        print(glue("Conference Board - {i}"))
        cby_fname <- here("assets", "_DB", "_extsources", cby_impplan$file_name[1])
        codes_row <- read_excel(cby_fname, sheet = "Annual", skip  = 5, n_max = 1, col_names = FALSE) |> unlist(use.names = FALSE)
        codes_row[is.na(codes_row)] <- "year"
        cb_raw <- read_excel(cby_fname, sheet = "Annual", skip = 7, col_names  = FALSE,  .name_repair = "minimal")
        colnames(cb_raw) <- codes_row
        
        cb_long <- cb_raw |> pivot_longer(-year, names_to = "tbl_code", values_to = "value") |> filter(!is.na(value)) |>
          mutate(iso3 = str_match(tbl_code, "^[^_]+_([A-Z]{3})_")[, 2], indicator  = str_match(tbl_code, "^[^_]+_[A-Z]{3}_(.+)")[, 2]) |>
          select(year, iso3, indicator, value) |> filter(indicator %in% cby_codes) |> 
          mutate(year = as.numeric(year), country_id = countrycode(iso3, origin = 'iso3c', destination = 'iso2c', 
                        custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN', 'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS',
                                        'ZAR' = 'CD'), warn = F)) |>
          filter(!country_id %in% c("S4", "S2", "V4", "V1", "S1", "8S", "T5", "ZG", "ZF", "T6", "XT"))
        
        cb_wide <- cb_long |> pivot_wider(names_from = indicator, values_from = value)
        ren_vec <- setNames(cby_names, str_replace(cby_codes, "^[^_]+_[A-Z]{3}_", ""))
        cb_wide <- cb_wide |> rename_with(~ ren_vec[.x], .cols = names(ren_vec))
        
        extdata_y <- extdata_y |>
          left_join(cb_wide, by = c("country_id", "year"), suffix = c("", "_old"))
        
        print("+")
        
      }
    }
  })

  ##### Import data on CPI targets
  
  try({
    
      cpt_impplan <- impplan |> filter(active == 1, database_name == "CPI targets", retrieve_type == "file", source_frequency == "y")
      cpt_codes <- cpt_impplan |> pull(retrieve_code)
      new_codes <- cpt_impplan |> pull(indicator_code)
      cpt_fname <- here("assets", "_DB", "_extsources", cpt_impplan$file_name[1])
      cpt_sheet <- cpt_impplan$sheet_name[1]
      
      if (length(cpt_codes) > 0 && all(!is.na(cpt_codes))) {
        
        print("CPI targets")
        cpt_raw <- read_excel(cpt_fname, sheet = cpt_sheet, skip = 0, col_names  = T,  .name_repair = "minimal")
        cpt_raw <- cpt_raw |> mutate(year = as.numeric(year)) |> 
          mutate(across(starts_with("cpi_target"), as.numeric)) |>
          rename_with(~ new_codes, all_of(cpt_codes)) |> select("country_id", "year", all_of(new_codes))
        
        extdata_y <- extdata_y |> left_join(cpt_raw, by = c("country_id", "year"), suffix = c("", "_old"))
        
        print("+") 
      }
        
  })
  
  ##### Import data on CDS 
  
  try({
    
    cds_impplan <- impplan |> filter(active == 1, database_name == "CDS", retrieve_type == "file", source_frequency == "d")
    new_codes <- cds_impplan |> pull(indicator_code)
    cds_fname <- here("assets", "_DB", "_extsources", cds_impplan$file_name[1])
    cds_sheet <- cds_impplan$sheet_name[1]
    
    if (length(new_codes) > 0 && all(!is.na(new_codes))) {
      
      print("CDS")
      cds_raw <- read_excel(cds_fname, sheet = cds_sheet, skip = 1, col_names = T, .name_repair = "minimal")
      cds_final <- cds_raw |> mutate(date = as.Date(date)) |>
        pivot_longer(cols = -date, names_to = "country_id", values_to = new_codes) |>
        mutate(country_id = as.character(country_id), "{new_codes}" := as.numeric(.data[[new_codes]])) |>
        arrange(date, country_id) |> as_tibble()
      
      extdata_d <- extdata_d |> left_join(cds_final, by = c("country_id", "date"), suffix = c("", "_old"))
      
      print("+") 
    }
    
  })
  
  ##### Import data on neutral real rates
  
  try({
    
    nrate_impplan <- impplan |> filter(active == 1, database_name == "Neutral rates", retrieve_type == "file", source_frequency == "y")
    nrate_codes <- nrate_impplan |> pull(retrieve_code)
    new_codes <- nrate_impplan |> pull(indicator_code)
    nrate_fname <- here("assets", "_DB", "_extsources", nrate_impplan$file_name[1])
    nrate_sheet <- nrate_impplan$sheet_name[1]
    
    if (length(nrate_codes) > 0 && all(!is.na(nrate_codes))) {
      
      print("Neutral rates")
      nrate_raw <- read_excel(nrate_fname, sheet = nrate_sheet, skip = 0, col_names  = T,  .name_repair = "minimal")
      nrate_raw <- nrate_raw |> mutate(year = as.numeric(year)) |> 
        mutate(across(contains("neutral_rate"), as.numeric)) |>
        rename_with(~ new_codes, all_of(nrate_codes)) |> select("country_id", "year", all_of(new_codes))
      extdata_y <- extdata_y |> left_join(nrate_raw, by = c("country_id", "year"), suffix = c("", "_old"))
      
      print("+") 
    }
    
  })
  
  ##### Import BIS debt data
  
  try({
    
    bisd_impplan <- impplan |> filter(active == 1, source_name == "BIS", 
                          database_name %in% c("Debt securities", "DSR", "Credit-to-GDP"), retrieve_type == "API")
    bisd_codes <- bisd_impplan |> pull(retrieve_code) 
    new_codes <- bisd_impplan |> pull(indicator_code)
    
    if (length(bisd_codes) > 0 && all(!is.na(bisd_codes))) {
      
      print("BIS debt securities")
      
      for (i in 1:length(bisd_codes)) {
        bisd_raw <- read.csv(bisd_codes[i]) |> as_tibble() |> 
          rename(country_id = any_of(c("REF_AREA", "BORROWERS_CTY"))) |> 
          separate(TIME_PERIOD, into = c("year", "quarter"), sep = "-Q") |>
          mutate(year = as.numeric(year), quarter = as.numeric(quarter)) |> 
          rename_at(vars(OBS_VALUE), ~new_codes[i]) |> select("country_id", "year", "quarter", all_of(new_codes[i]))
        
        extdata_q <- extdata_q |> left_join(bisd_raw, by = c("country_id", "year", "quarter"), suffix = c("", "_old"))
        print("+")
      }
       
    }
    
  })
  
  
  ##### Import data on RAs defaults
  
  try({
    
    ra_impplan <- impplan |> filter(active == 1, database_name == "RAs", retrieve_type == "file", source_frequency == "y")
    ra_codes <- ra_impplan |> pull(retrieve_code)
    new_codes <- ra_impplan |> pull(indicator_code)
    ra_fname <- here("assets", "_DB", "_extsources", ra_impplan$file_name[1])
    ra_sheet <- ra_impplan$sheet_name[1]
    
    if (length(ra_codes) > 0 && all(!is.na(ra_codes))) {
      
      print("Observable sovereign defaults")
      ra_raw <- read_excel(ra_fname, sheet = ra_sheet, skip = 0, col_names  = T,  .name_repair = "minimal")
      ra_raw <- ra_raw |> mutate(year = as.numeric(year)) |> 
        rename_at(vars(ra_codes), ~new_codes) |> select("country_id", "year", all_of(new_codes))
      
      extdata_y <- extdata_y |> left_join(ra_raw, by = c("country_id", "year"), suffix = c("", "_old")) |>
        mutate(across(all_of(new_codes), ~ replace_na(.x, 0)))
      
      print("+") 
    }
    
  })
  
  
  ##### Import data on defaults from BOC-BOE
  
  try({
    
    def_impplan <- impplan |> filter(active == 1, database_name == "BOC-BOE", retrieve_type == "file", source_frequency == "y")
    def_codes <- def_impplan |> pull(retrieve_code)
    new_codes <- def_impplan |> pull(indicator_code)
    def_fname <- here("assets", "_DB", "_extsources", def_impplan$file_name[1])
    def_sheet <- def_impplan$sheet_name[1]
    
    if (length(def_codes) > 0 && all(!is.na(def_codes))) {
      
      print("BOC-BOE sovereign defaults (filled)")
      def_raw <- read_excel(def_fname, sheet = def_sheet, skip = 64, col_names  = T,  .name_repair = "minimal")
      def_raw <- def_raw |> separate(col = k, into  = c("country_id", "year"), sep = "_", remove = TRUE, convert = TRUE) |>
        mutate(year = as.numeric(year),
               country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', warn = F)) |> 
        rename_at(vars(def_codes), ~new_codes) |> select("country_id", "year", all_of(new_codes)) |> 
        filter(!is.na(country_id))
      
      def_corr <- def_raw |> pivot_longer(cols = all_of(new_codes), names_to = "var", 
                                          values_to = "raw", values_transform = list(raw = as.character)) |>
        mutate(raw = as.character(raw), star = str_detect(raw, "^\\*+$"), numeric = parse_number(raw)) |> 
        group_by(country_id, var) %>% arrange(year, .by_group = TRUE) |>  
        mutate(forward  = na.locf(numeric,  na.rm = FALSE), backward = na.locf(numeric, fromLast = TRUE, na.rm = FALSE),
               filled   = case_when(star ~ coalesce(forward, backward, 0), is.na(numeric) ~ 0, TRUE ~ numeric)) |>
        ungroup() |> select(-raw, -star, -numeric, -forward, -backward) |>
        pivot_wider(names_from = var, values_from = filled) |> relocate(all_of(new_codes), .after = c(country_id, year))  
      
      extdata_y <- extdata_y |> left_join(def_corr, by = c("country_id", "year"), suffix = c("", "_old")) |>
        mutate(across(all_of(new_codes), ~ replace_na(.x, 0)))
      
      print("+") 
    }
    
  })
  
  
  ##### Import data on sovereign defaults from Reinhart and Rogoff
  
  try({
    
    rr_impplan <- impplan |> filter(active == 1, database_name == "RR", retrieve_type == "file", source_frequency == "y")
    rr_codes <- rr_impplan |> pull(retrieve_code)
    new_codes <- rr_impplan |> pull(indicator_code)
    rr_fname <- here("assets", "_DB", "_extsources", rr_impplan$file_name[1])
    rr_sheet <- rr_impplan$sheet_name[1]
    
    if (length(rr_codes) > 0 && all(!is.na(rr_codes))) {
      
      print("RR sovereign defaults")
      rr_tidy <- read_excel(rr_fname, sheet = rr_sheet, skip = 0, col_names  = T,  .name_repair = "minimal") |>
                  select(-country_name) |> pivot_longer(!country_id, names_to = "year", values_to = new_codes[1]) |>
                  mutate(year = as.numeric(year))
      
      rr_tidy <- eval(parse(text = glue("rr_tidy |> mutate( {new_codes[1]} = as.numeric(({new_codes[1]} == 1)*(lag({new_codes[1]}, 1) == 0) ))") ))
    
      
      extdata_y <- extdata_y |> left_join(rr_tidy, by = c("country_id", "year"), suffix = c("", "_old")) |>
        mutate(across(all_of(new_codes), ~ replace_na(.x, 0)))
      
      print("+") 
    } 
    
  })
  
  
  ##### Import data on sovereign defaults from Global Crisis Database
  
  try({
    
    gcd_impplan <- impplan |> filter(active == 1, database_name == "GCD", retrieve_type == "file", source_frequency == "y")
    gcd_codes <- gcd_impplan |> pull(retrieve_code)
    new_codes <- gcd_impplan |> pull(indicator_code)
    gcd_fname <- here("assets", "_DB", "_extsources", gcd_impplan$file_name)
    gcd_sheet <- gcd_impplan$sheet_name
    
    if (length(gcd_codes) > 0 && all(!is.na(gcd_codes))) {
      
      print("GCD sovereign defaults")
      
      gcd_tidy <- read_excel(gcd_fname[1], sheet = gcd_sheet[1], skip = 0, col_names  = T,  .name_repair = "minimal") |>
        select(CC3, Year, all_of(gcd_codes)) |> filter(!row_number() == 1) |> rename_at(vars(gcd_codes), ~new_codes) |>
        mutate(year = as.numeric(Year), country_id = countrycode(CC3, origin = 'iso3c', destination = 'iso2c', 
            custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN', 'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS', 'ZAR' = 'CD'), warn = F)) |>
        select(country_id, year, all_of(new_codes)) |> mutate_at(.vars = all_of(new_codes), .funs = as.numeric)
      
      for (i in 1:length(new_codes)) {
        gcd_tidy <- eval(parse(text = glue("gcd_tidy |> 
                      mutate( {new_codes[i]} = as.numeric(({new_codes[i]} == 1)*(lag({new_codes[i]}, 1) == 0) ))") ))
      }
      
      extdata_y <- extdata_y |> left_join(gcd_tidy, by = c("country_id", "year"), suffix = c("", "_old")) |>
        mutate(across(all_of(new_codes), ~ replace_na(.x, 0))) 
      
      print("+") 
      }
    
    })
  
  
  ##### Import data on sovereign defaults from "Horn, Reinhart and Trebesch: Hidden Defaults"
  
  try({
    
    hrt_impplan <- impplan |> filter(active == 1, database_name == "HRT", retrieve_type == "file", source_frequency == "y")
    hrt_codes <- hrt_impplan |> pull(retrieve_code)
    new_codes <- hrt_impplan |> pull(indicator_code)
    hrt_fname <- here("assets", "_DB", "_extsources", hrt_impplan$file_name)
    hrt_sheet <- hrt_impplan$sheet_name
    
    if (length(hrt_codes) > 0 && all(!is.na(hrt_codes))) {
      
      print("HRT sovereign defaults")
      
      for (i in seq_along(new_codes)) {
        
        first_row <- read_excel(path = hrt_fname[i], sheet  = hrt_sheet[i], range  = cell_rows(1), col_names = F, na = c("", "NA"), n_max = 1)
        skip_n <- if (all(is.na(first_row))) 6 else 0
        
        hrt_tidy <- read_excel(hrt_fname[i], sheet = hrt_sheet[i], skip = skip_n, col_names  = T,  .name_repair = "minimal") |>
          select(any_of(c("Year","ISOCode", "Symbolic?", "DebtorCountry", "WDI code", "Start of default or restructuring process: default or announcement"))) 
        
        if ("Symbolic?" %in% names(hrt_tidy)) { hrt_tidy <- hrt_tidy |> filter(`Symbolic?` == 0 | is.na(`Symbolic?`)) }
        
        date_col <- "Start of default or restructuring process: default or announcement"
        if (date_col %in% names(hrt_tidy)) { 
          hrt_tidy <- hrt_tidy |> mutate(EventDate = as_date(as.numeric(.data[[date_col]]), origin = "1899-12-30"),
              Year = year(EventDate), .after = {{date_col}}) |> rename("ISOCode" = "WDI code") |>
            mutate(ISOCode = countrycode(ISOCode, "wb", "iso3c"))
        }
        # какой реально код там, если не wb (почему не находит ROM, CRO, SLO? задать руками?)
        
        code_cols <- intersect(c("ISOCode", "WDI code"), names(hrt_tidy))
        if (length(code_cols) == 0 && "DebtorCountry" %in% names(hrt_tidy)) { hrt_tidy <- hrt_tidy |>
            mutate( ISOCode = countrycode(DebtorCountry, "country.name", "iso3c"),  .after = DebtorCountry) }
        
        hrt_tidy <- hrt_tidy |> mutate(country_id = countrycode(ISOCode, "iso3c", "iso2c")) |> rename('year'='Year') |>
          select(country_id, year) |> mutate(!!sym(new_codes[i]) := 1) |> unique()
        # задать руками SER, ZBW, CBV?
        
        extdata_y <- extdata_y |> left_join(hrt_tidy, by = c("country_id", "year"), suffix = c("", "_old")) |>
          mutate(across(all_of(new_codes[i]), ~ replace_na(.x, 0))) }
      
      print("+")
      
      }
     
  })
  
  
  ##### Import data on sovereign defaults from "Sovereign Defaults: The Price of Haircuts", by Juan Cruces and Christoph Trebesch
  
  try({
    
    ct_impplan <- impplan |> filter(active == 1, database_name == "CT", retrieve_type == "file", source_frequency == "y")
    ct_codes <- ct_impplan |> pull(retrieve_code)
    new_codes <- ct_impplan |> pull(indicator_code)
    ct_fname <- here("assets", "_DB", "_extsources", ct_impplan$file_name)
    ct_sheet <- ct_impplan$sheet_name
    
    if (length(ct_codes) > 0 && all(!is.na(ct_codes))) {
      
      print("CT sovereign defaults")
      
      ct_tidy <- read_excel(ct_fname[1], sheet = ct_sheet[1], skip = 13, col_names  = F) |>
        rename(country_id = 4, def_date = 5) |> select(country_id, def_date) |>
        mutate(year = as.numeric(year(def_date)),
            country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
            custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN', 'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS', 'ZAR' = 'CD'), warn = F)) |>
        select(country_id, year) |> unique()
      
      for (i in 1:length(new_codes)) {
        ct_tidy <- eval(parse(text = glue("ct_tidy |> mutate( {new_codes[i]} = 1 )") ))
      }
      
      extdata_y <- extdata_y |> left_join(ct_tidy, by = c("country_id", "year"), suffix = c("", "_old")) |>
        mutate(across(all_of(new_codes), ~ replace_na(.x, 0))) 
    
    print("+") 
    }
    
  })
  
  
  ##### Import sovereign model modifiers
  
  try({
    
    mod_impplan <- impplan |> filter(active == 1, database_name == "Modifiers", retrieve_type == "file", source_frequency == "y")
    mod_codes <- mod_impplan |> pull(retrieve_code)
    new_codes <- mod_impplan |> pull(indicator_code)
    mod_fname <- here("assets", "_DB", "_extsources", mod_impplan$file_name)
    mod_sheet <- mod_impplan$sheet_name
    
    if (length(mod_codes) > 0 && all(!is.na(mod_codes))) {
      
      print("Modifiers")
      
      mod_tidy <- read_excel(mod_fname[1], sheet = mod_sheet[1], skip = 1, col_names  = T, .name_repair = "minimal") |> 
        mutate(year = as.numeric(year)) |> rename_at(vars(mod_codes), ~new_codes) |> select(country_id, year, any_of(new_codes))
      
      extdata_y <- extdata_y |> left_join(mod_tidy, by = c("country_id", "year"), suffix = c("", "_old"))
    
      print("+") 
    }
    
  })
  
  ##### Import and build WEO forecast vintages
  
  try({
  
      weov_impplan <- impplan |> filter(active == 1, database_name == "WEO vintages", retrieve_type == "file", source_frequency == "y")
      weov_codes <- weov_impplan |> pull(retrieve_code)
      new_codes <- weov_impplan |> pull(indicator_code)
      weov_dir    <- here::here("assets", "_DB", "_extsources", "WEO_vintages")
      weov_files  <- fs::dir_ls(weov_dir, regexp = "\\d{4}-[12]\\.xls[x]?$", type = "file")
      forecast_h <- 1L                     # горизонт прогноза (в годах вперёд)
      
      if (length(weov_codes) > 0 && all(!is.na(weov_codes))) {
        
        print("WEO vintages")
      
      safe_read_weov <- \(path) {
        tryCatch(
          readxl::read_excel(path, sheet = 1, .name_repair = "unique", na = c("n/a", "N/A", "..", ".", "-", "—", "–", "")),
          error = \(e) { warning(glue("Не прочитан файл: {basename(path)}; {e$message}")); tibble() }
        )
      }
      
      parse_pub <- \(fname) {
        m <- str_match(basename(fname), "^(\\d{4})-([12])\\.xls[x]?$")
        tibble(pub_year = as.integer(m[,2]), issue = as.integer(m[,3]))
      }
      
      first_present <- \(nm, candidates) {
        cand <- candidates[candidates %in% nm]
        if (length(cand)) cand[[1]] else NA_character_
      }
      
      weov_long <- map(weov_files, \(f) {
        pub <- parse_pub(f)
        raw <- safe_read_weov(f)
        if (nrow(raw) == 0) return(tibble())
        
        nm <- names(raw)
        
        subj_col <- first_present(nm, c("WEO Subject Code","WEO.Subject.Code","Subject Code","Subject.Code","subject_code"))
        iso2_col <- first_present(nm, c("ISO2","ISO2 Code","ISO2.Code","iso2"))
        iso3_col <- first_present(nm, c("ISO","ISO Code","ISO.Code","WEO Country Code","WEO.Country.Code","iso"))
        
        # валидация служебных колонок
        if (is.na(subj_col)) {
          warning(glue("Файл {basename(f)}: не найден столбец с кодом показателя — файл пропущен"))
          return(tibble())
        }
        if (is.na(iso2_col) && is.na(iso3_col)) {
          warning(glue("Файл {basename(f)}: не найден ISO2/ISO3 — файл пропущен"))
          return(tibble())
        }
        
        # кандидаты годовых колонок
        year_cols <- nm[str_detect(nm, "^\\d{4}$|^X\\d{4}$")]
        if (!length(year_cols)) {
          warning(glue("Файл {basename(f)}: не найдены годовые колонки — файл пропущен"))
          return(tibble())
        }
        
        # целевой год прогноза для этого файла
        target_year <- pub$pub_year + forecast_h
        target_name_candidates <- c(as.character(target_year), paste0("X", target_year))
        has_target <- any(target_name_candidates %in% year_cols)
        if (!has_target) {
          warning(glue("Файл {basename(f)}: отсутствует колонка целевого года {target_year} — пропущен"))
          return(tibble())
        }
        
        keep <- c(subj_col, iso2_col, iso3_col, year_cols) |> discard(is.na)
        keep <- keep[!is.na(keep)]           
        keep <- intersect(keep, names(raw)) 
        df <- raw |> select(any_of(keep))
        
        # country_id (предпочтительно ISO2)
        df <- df |>
          mutate(
            country_id = coalesce(
              if (!is.na(iso2_col)) .data[[iso2_col]] else NA_character_,
              if (!is.na(iso3_col)) .data[[iso3_col]] else NA_character_
            ),
            .before = 1
          )
        
        # удаляем ISO-колонки (если они реально есть)
        rm_cols <- c(iso2_col, iso3_col)
        rm_cols <- rm_cols[!is.na(rm_cols)]         
        rm_cols <- intersect(rm_cols, names(df))    
        df <- df |> select(-any_of(rm_cols))
        
        # валидация country_id
        n_missing_id <- sum(is.na(df$country_id))
        if (n_missing_id > 0) {
          warning(glue("Файл {basename(f)}: отсутствует country_id у {n_missing_id} строк — они будут отброшены"))
          df <- df |> filter(!is.na(country_id))
        }
        
        # валидация наличия нужных кодов в этом файле
        found_codes <- sort(unique(df[[subj_col]]))
        missing_codes <- setdiff(weov_codes, found_codes)
        if (length(missing_codes) > 0) {
          warning(glue("Файл {basename(f)}: не найдены коды {toString(missing_codes)}"))
        }
        
        # длинная форма + выбор только целевого года
        df_long <- df |>
          pivot_longer(cols = matches("^\\d{4}$|^X\\d{4}$"),
                       names_to = "year_chr", values_to = "value") |>
          mutate(
            year = as.integer(str_remove(year_chr, "^X")),
            subject_code = .data[[subj_col]],
            .keep = "unused"
          ) |>
          mutate(
            value = dplyr::na_if(value, "n/a"),
            value = dplyr::na_if(value, "N/A"),
            value = dplyr::na_if(value, ".."),
            value = dplyr::na_if(value, "-"),
            value = dplyr::na_if(value, "—"),
            value = dplyr::na_if(value, "–"),
            value = readr::parse_number(as.character(value),
                                        locale = readr::locale(decimal_mark = ".", grouping_mark = ",")) 
          ) |>
          filter(year == target_year, subject_code %in% weov_codes) |>
          mutate(pub_year = pub$pub_year, issue = pub$issue, .after = year) |>
          select(country_id, subject_code, year, pub_year, issue, value)
        
        # валидация «пустого после фильтра» (например, все нужные коды отсутствуют)
        if (nrow(df_long) == 0) {
          warning(glue("Файл {basename(f)}: после отбора целевого года {target_year} и кодов {toString(weo_codes)} данные отсутствуют"))
        }
        
        df_long
      }) |> list_rbind()
      
      if (nrow(weov_long) == 0) {
        weo_vintage_y <- tibble(country_id = character(), year = integer())
      } else {
        # усредняем по выпускам (-1 и -2) в рамках одного pub_year для каждого target_year
        weov_mean <- weov_long |>
          summarise(value = mean(value, na.rm = TRUE), .by = c(country_id, pub_year, subject_code))
        
        weov_wide <- weov_mean |>
          mutate(subject_code = factor(subject_code, levels = weov_codes)) |>
          pivot_wider(names_from = subject_code, values_from = value)
        
        keep_cols <- c("country_id", "pub_year", weov_codes)
        weo_vintage_y <- weov_wide |> select(any_of(keep_cols))
        
        # переименование кодов → новые имена
        rename_map <- set_names(weov_codes, new_codes)   # new := old
        weo_vintage_y <- weo_vintage_y |> rename(!!!rename_map)
        
        # типы
        weo_vintage_y <- weo_vintage_y |> mutate(year = suppressWarnings(as.integer(pub_year)),
            country_id = countrycode(country_id, origin = 'iso3c', destination = 'iso2c', 
            custom_match = c('ROM' = 'RO','ADO' = 'AD','ANT' = 'AN', 'KSV' = 'XK','TMP' = 'TL','WBG' = 'PS','ZAR' = 'CD'), warn = F)) |>
            select(-c(pub_year))
      }
      
      extdata_y <- extdata_y |> left_join(weo_vintage_y, by = c("country_id", "year"), suffix = c("", "_old"))
      print("+") 
      }
      
  })
      
  ##### Import local data
  
  try({
    
    local_impplan <- impplan |> filter(active == 1, retrieve_type == "local")
    local_codes <- local_impplan |> pull(retrieve_code)
    
    if (length(local_codes) > 0 && all(!is.na(local_codes))) {
      
      print("Local data")
      
      # соответствия кодов и частот из плана импорта
      impplan_tbl <- local_impplan |> transmute(retrieve_code, indicator_code, source_frequency)
      
      # какие временные столбцы нужны для каждой частоты
      time_vars_map <- list(d = c("date"), m = c("year", "month"), q = c("year", "quarter"), y = c("year"))
      
      # вектор частот, которые действительно встречаются в плане
      freqs <- intersect(c("d", "m", "q", "y"), unique(impplan_tbl$source_frequency))
      
      # читаем аккуратно: если файла/листа нет, возвращаем пустой tibble
      safe_read <- possibly(
        .f = \(path, sheet)
        readxl::read_excel(path, sheet = sheet, skip = 1, .name_repair = "unique"),
        otherwise = tibble()
      )
      
      # список тиблов по всем частотам
      freq_tables <- map(freqs, \(f) {
        
          codes_f <- impplan_tbl |> filter(source_frequency == f)
          old_codes <- codes_f$retrieve_code
          new_codes <- codes_f$indicator_code
          rename_map <- rlang::set_names(old_codes, new_codes)
          time_vars <- time_vars_map[[f]]
          
          # читаем все страны, добавляем country_id и склеиваем
          df_f <- map2(impparams$local_fnames, impparams$local_iso2, \(fname, iso2) {
            safe_read(fname, sheet = f) |>
              mutate(country_id = iso2, .before = 1)
          }) |> list_rbind()
          
          # выбираем служебные + нужные коды именно этой частоты
          keep_cols <- c("country_id", time_vars, old_codes)
          
          # переименовываем retrieve_code -> indicator_code
          df_f |> select(any_of(keep_cols)) |> rename(!!!rename_map)
          
      })
      
      # разложим результат в именованные объекты по частотам
      names(freq_tables) <- freqs
      
      # создаём четыре tibble-объекта; если частоты нет в плане — делаем пустой tibble с нужными служебными колонками
      local_d <- if ("d" %in% names(freq_tables)) freq_tables[["d"]] else tibble(country_id = character(), date = as.POSIXct(character()))
      local_m <- if ("m" %in% names(freq_tables)) freq_tables[["m"]] else tibble(country_id = character(), year = integer(), month = integer())
      local_q <- if ("q" %in% names(freq_tables)) freq_tables[["q"]] else tibble(country_id = character(), year = integer(), quarter = integer())
      local_y <- if ("y" %in% names(freq_tables)) freq_tables[["y"]] else tibble(country_id = character(), year = integer())
      
      # грузим в базу
      extdata_y <- extdata_y |> left_join(local_y, by = c("country_id", "year"), suffix = c("", "_old"))
      extdata_q <- extdata_q |> left_join(local_q, by = c("country_id", "year", "quarter"), suffix = c("", "_old")) 
      extdata_m <- extdata_m |> left_join(local_m, by = c("country_id", "year", "month"), suffix = c("", "_old")) 
      extdata_d <- extdata_d |> left_join(local_d, by = c("country_id", "date"), suffix = c("", "_old")) 
      
      print("+") 
    }
    
  })
  
    ##### Return imported
    return(list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d))
    print("+++")
    
}


# Functions to export data from memory

##### Function to generate dict and keep only planned imports

preExport <- function(saveplan, extdata_y, extdata_q, extdata_m, extdata_d, verbose = TRUE) {
  
  say <- function(txt) if (verbose) message(txt)
  
  say("Preparing export to files")
  say("Step 1/5 ▶ Building dictionary from save‑plan")
  dict <- saveplan |> select(indicator, theme, indicator_code, source_frequency, source_name, keep) |> 
    arrange(theme, indicator_code)
  
  say("Dictionary preview:")
  if (verbose) print(dict, n = 6)
  
  say("Step 2/5 ▶ Filtering extdata_* to planned indicators")
  
  # 2a.  Put each extdata_* into a named list so we can iterate
  extdata_list <- list(y = extdata_y, q = extdata_q, m = extdata_m, d = extdata_d)
  
  # 2b.  Helper: mandatory “key” columns per frequency
  key_cols <- list(
    y = c("country", "country_id", "year"),
    q = c("country", "country_id", "year", "quarter"),
    m = c("country", "country_id", "year", "quarter", "month"),
    d = c("country", "country_id", "date")
  )
  
  # 2c.  Split dictionary by frequency for quick look‑ups
  dict_by_freq <- split(dict, dict$source_frequency)
  
  # 2d.  Filter each data frame
  extdata_list <- imap(extdata_list,
    ~ { cols_planned <- dict_by_freq[[.y]]$indicator_code %||% character(0)
      .x %>% select(all_of(key_cols[[.y]]), any_of(cols_planned))
    }
  )
  
  ## add year helper column to daily data
  extdata_list$d <- extdata_list$d %>% mutate(year = year(date))
  
  say("Step 3/5 ▶ Marking successful downloads")
  
  downloaded <- map2_dfr(extdata_list, names(extdata_list), 
                         ~ tibble(indicator_code = names(.x), source_frequency = .y)) |> mutate(success = "+")
  
  dict <- dict |> left_join(downloaded, by = c("indicator_code", "source_frequency"))
    #mutate(n_countries = NA_integer_, start_year = NA_integer_, end_year = NA_integer_, n_points = NA_integer_)
  
  say("Step 4/5 ▶ Computing coverage statistics")
  
  calc_stats <- function(df, code, year_col = "year") {
    
    if (!code %in% names(df))
      return(list(n_countries = 0L, start_year = NA_integer_, end_year = NA_integer_, n_points = 0L))
    
    d <- df |> select(country_id, !!year_col, !!code) |> filter(!is.na(.data[[code]]))
    
    if (nrow(d) == 0)
      return(list(n_countries = 0L, start_year = NA_integer_, end_year = NA_integer_, n_points = 0L))
    
    list(
      n_countries = n_distinct(d$country_id),
      start_year  = min(d[[year_col]], na.rm = TRUE),
      end_year    = max(d[[year_col]], na.rm = TRUE),
      n_points    = nrow(d)
    )
  }
  
  dict <- dict |> rowwise() |> mutate(
      stats = list(
        calc_stats(
          extdata_list[[source_frequency]],
          indicator_code,
          year_col = "year"     # all extdata frames now have a `year` column
        )
      )
    ) |>
    unnest_wider(stats) |> ungroup()
  
  # daily data no longer needs the helper column
  extdata_list$d <- extdata_list$d |> select(-year)
  
  say("Step 5/5 ▶ Preparing return object")
  
  dict_d <- dict |> filter(source_frequency == "d")
  dict   <- dict |> filter(source_frequency != "d")
  
  list(extdata_y = extdata_list$y, extdata_q = extdata_list$q, extdata_m = extdata_list$m,
    extdata_d = extdata_list$d, dict = dict, dict_d = dict_d)
}

##### Export data on all countries to the yearly/quarterly/monthly database

writeDatafiles  <- function(y = NULL, q = NULL, m = NULL, d = NULL, dict = NULL, dict_d = NULL, stem = "Imported_DB", stem_d = NULL,
                            dir = ".", formats = c("xlsx","rds"), overwrite = TRUE, compress_rds = "gzip", verbose = interactive()) {
  
  #' @param y,q,m,d       Data frames with yearly, quarterly, monthly, daily data
  #' @param dict,dict_d   Dictionary tables for y‑q‑m and d groups
  #' @param stub          File‑name stem (e.g. "extdata" → "extdata_yqm.xlsx")
  #' @param dir           Output directory (created if absent)
  #' @param formats       Character vector: any of c("xlsx","rds")
  #' @param overwrite     Logical. Replace existing files?
  #' @param compress_rds  Passed to `saveRDS()` (e.g. "xz", "gzip", "none")
  #' @param verbose       Logical. Print progress messages?
  
  formats  <- match.arg(formats, several.ok = TRUE)
  dir      <- fs::path_abs(dir)
  fs::dir_create(dir)
  
  make_path <- function(directory, fname_no_ext, ext) {
    fs::path(directory, glue::glue("{fname_no_ext}.{ext}"))
  }
  
  # 1. Build two bundles 
  bundle_yqm <- list(y = y, q = q, m = m, dict = dict) |>
    purrr::compact()                           # drop NULL elements
  bundle_d   <- list(d = d, dict_d = dict_d)  |> purrr::compact()
  
  if (length(bundle_yqm) == 0 && length(bundle_d) == 0)
    rlang::abort("Nothing to save: every data argument is NULL.")
  
  # helper
  maybe_write <- function(path, writer) {
    writer <- rlang::as_function(writer)   # <‑‑ converts formula or leaves functions untouched
    
    if (fs::file_exists(path) && !overwrite)
      rlang::abort(glue::glue("File exists: {path}. Set `overwrite = TRUE`."))
    
    writer(path)                           # now it’s definitely callable
    if (verbose) cli::cli_alert_success("Wrote {.file {path}}")
    path
  }
  
  paths <- list()
  stem_d <- stem_d %||% paste0(stem, "_d")
  
  # 2. XLSX
  if ("xlsx" %in% formats) {
    
    if (length(bundle_yqm))
      paths$xlsx_yqm <- maybe_write(
        make_path(dir, stem, "xlsx"),
        ~ writexl::write_xlsx(bundle_yqm, path = .x, col_names = TRUE)
      )
    
    if (length(bundle_d))
      paths$xlsx_d <- maybe_write(
        make_path(dir, stem_d, "xlsx"),
        ~ writexl::write_xlsx(bundle_d, path = .x, col_names = TRUE)
      )
  }
  
  # 3. RDS 
  if ("rds" %in% formats) {
    
    if (length(bundle_yqm))
      paths$rds_yqm <- maybe_write(
        make_path(dir, stem, "rds"),
        ~ saveRDS(bundle_yqm, file = .x, compress = compress_rds)
      )
    
    if (length(bundle_d))
      paths$rds_d <- maybe_write(
        make_path(dir, stem_d, "rds"),
        ~ saveRDS(bundle_d, file = .x, compress = compress_rds)
      )
  }
  
  invisible(paths)
}

##### Copy written files

replicateSavedFiles <- function(src_paths, target_dirs, overwrite = TRUE, method = c("copy", "link")) {
  
  method <- match.arg(method)
  
  target_dirs <- fs::path_abs(target_dirs)
  fs::dir_create(target_dirs)
  
  # ── build every (src, dir) pair
  combos <- tidyr::expand_grid(src = src_paths, dir = target_dirs)
  
  # ── act on each pair ---
  purrr::pwalk(combos, function(src, dir) {
    dest <- fs::path(dir, fs::path_file(src))
    
    if (method == "copy") {
      fs::file_copy(src, dest, overwrite = overwrite)
    } else {                                # hard‑link
      fs::link_create(src, dest, overwrite = overwrite)
    }
  })
  
  invisible(TRUE)
}

##### Export data on all countries to 4 csv files: yearly/quarterly/monthly database and dict

writeDatafilesCsv <- function(datalist, path) {
  
  print("Writing csv files")
  datalist$dict <- rbind(datalist$dict, datalist$dict_d) |> select(-c(keep, success, n_countries, n_points))
  datalist$dict_d <- NULL
  
  for (i in 1:length(datalist)) {
    write_excel_csv2(datalist[[i]], file = here(path, glue("{names(datalist)[i]}.csv")), col_names = TRUE, na="")
  }
  
}

## Export data on a specific country to the yearly database

#countryname_export = "Russian Federation"

#extdata_y |> filter(country==countryname_export) |> select(-c("country","country_id","year")) -> t_data_export
#extdata_y |> filter(country==countryname_export) |> select("year") -> years

#data_export <- data.frame(t(t_data_export))
#names(data_export) <- unlist(years)
#data_export <- cbind("indicator_code" = names(t_data_export), data_export)
#data_export <- data_export |> left_join(dict, "indicator_code"="indicator_code") |> select(indicator, everything())

#data_export <- list(data_export)
#names(data_export) <- c("y")
#write_xlsx(data_export, path = paste(countryname_export, "/", countryname_export, "_data.xlsx", sep=""), 
#           col_names = T, format_headers = T)
