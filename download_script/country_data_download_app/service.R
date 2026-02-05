library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(lubridate)
library(writexl)
library(readxl)

# ---------- helpers ---------------------------------------------------------

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

subsetCountry <- function(country_id, datalist) {
  
  # D: добавим временный year для расчётов границ
  ext_d_with_year <- datalist$extdata_d |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::relocate(year, .before = date)
  
  ext_list <- list(
    y = datalist$extdata_y,
    q = datalist$extdata_q,
    m = datalist$extdata_m,
    d = ext_d_with_year
  ) |>
    purrr::imap(\(df, freq) {
      df |>
        dplyr::filter(.data$country_id == .env$country_id) |>
        dplyr::select(-dplyr::any_of(c("country", "country_id")))
    })
  
  dict <- datalist$dict
  
  # Надежный расчёт start/end без .data[[code]] в filter()
  calc_bounds <- function(freq, code) {
    df <- ext_list[[freq]]
    
    if (is.null(df) || !("year" %in% names(df)) || !(code %in% names(df))) {
      return(c(start_year = NA_real_, end_year = NA_real_))
    }
    
    years <- df$year[!is.na(df[[code]])]
    
    if (length(years) == 0) {
      return(c(start_year = NA_real_, end_year = NA_real_))
    }
    
    c(
      start_year = min(years, na.rm = TRUE),
      end_year   = max(years, na.rm = TRUE)
    )
  }
  
  bounds_mat <- purrr::map2(
    dict$source_frequency,
    dict$indicator_code,
    calc_bounds
  ) |>
    do.call(rbind, args = _)
  
  dict_clean <- dict |>
    dplyr::mutate(
      start_year = bounds_mat[, "start_year"],
      end_year   = bounds_mat[, "end_year"]
    ) |>
    dplyr::filter(is.finite(.data$start_year), is.finite(.data$end_year))
  
  # Границы по каждой частоте
  year_bounds <- dict_clean |>
    dplyr::summarise(
      min_year = min(.data$start_year, na.rm = TRUE),
      max_year = max(.data$end_year, na.rm = TRUE),
      .by = .data$source_frequency
    )
  
  ext_list_trimmed <- ext_list |>
    purrr::imap(\(df, freq) {
      if (is.null(df)) return(df)
      if (!("year" %in% names(df))) return(df)
      
      bounds <- year_bounds |>
        dplyr::filter(.data$source_frequency == freq)
      
      if (nrow(bounds) == 0) return(df)
      
      df |>
        dplyr::select(dplyr::where(\(x) !all(is.na(x)))) |>
        dplyr::filter(.data$year >= bounds$min_year, .data$year <= bounds$max_year)
    })
  
  ext_list_trimmed$d <- ext_list_trimmed$d |>
    dplyr::select(-dplyr::any_of("year"))
  
  list(
    y    = ext_list_trimmed$y,
    q    = ext_list_trimmed$q,
    m    = ext_list_trimmed$m,
    d    = ext_list_trimmed$d,
    dict = dict_clean
  )
}

## Function to make a model sheet
generateModelSheet <- function(yearly_data, dict) {
  
  # choosing needed variables
  yearly_data |> select(-c("year")) |>
    select(any_of(c("gdp_pc_usd_wb", "gdp_pc_ppp_wb", "gdp_growth", "gdp_usd", "gdp",
                    "gdp_growth_world_weo", "gdp_growth_dm_weo", "gdp_growth_em_weo",
                    "cpi_av", "deflator", "pop", "rnd", "gcfc", "gcfc_gdp", "open",
                    "gg_debt_weo", "gg_rev_weo", "gg_debttorev", "gg_exns_int", "gg_inttorev",
                    "gg_debt_conc_usd", "extdebt_conc_gdp", "gg_bal_weo", "gg_bal_gdp_weo",
                    "extdebt_gg_usd", "extdebt_gg_gdp", "gg_debt_gdp_weo",
                    "gg_debt_fc_role_fsdb", "gg_debt_held_global_usd", "gg_debt_held_global_world_usd",
                    "gg_debt_held_global_role", "gg_debt_maturity", "dpension2030",
                    "ca_usd", "ca_gdp", "ex_gs_usd", "imp_gs_usd", "intres_usd",
                    "intrestoimp", "niip_ex_ggcb_usd", "niip_ex_ggcb_gdp",
                    "ex_div", "neer_av_bis", "neer_av_ifs", "neer_av", "usdlc_eop", "usdlc_av",
                    "remit_usd_wb", "remit_gdp_wb", "extdebt_usd", "intrestoextdebt",
                    "wgi_va_est", "wgi_ps_est", "wgi1", "wgi_cc_est", "wgi_rl_est",
                    "wgi_rq_est", "wgi_ge_est", "wgi2",
                    "educ", "amr_male", "amr_female", "amr", "life_length", "hci"))) -> t_data_export
  
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

#' Function to write XLSX with per-sheet formatting (freeze panes + column widths)
#'
#' @param sheets list of data.frames/tibbles. Names are sheet names.
#' @param path output .xlsx path
#' @param freeze_by_sheet named list. Each element: list(cell = "E2") or list(row = 2, col = 5)
#' @param col_widths_by_sheet named list. Each element: numeric vector of widths for columns 1..k

write_xlsx_formatted <- function(
    sheets,
    path,
    freeze_by_sheet = NULL,
    col_widths_by_sheet = NULL
) {
  stopifnot(is.list(sheets), length(sheets) > 0)
  
  # Ensure all sheets have names (required by openxlsx)
  if (is.null(names(sheets)) || any(names(sheets) == "")) {
    names(sheets) <- paste0("sheet_", seq_along(sheets))
  }
  
  wb <- openxlsx::createWorkbook()
  
  header_style <- openxlsx::createStyle(
    textDecoration = "bold"
  )
  
  parse_cell <- function(cell) {
    # "E2" -> list(row = 2, col = 5)
    stopifnot(is.character(cell), length(cell) == 1)
    m <- regmatches(cell, regexec("^([A-Za-z]+)([0-9]+)$", cell))[[1]]
    if (length(m) != 3) stop("Bad cell format: ", cell)
    
    col_letters <- toupper(m[[2]])
    row <- as.integer(m[[3]])
    
    # Convert letters to number (A=1, Z=26, AA=27...)
    letters <- strsplit(col_letters, "")[[1]]
    col <- 0L
    for (ch in letters) {
      col <- col * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
    }
    
    list(row = row, col = col)
  }
  
  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    if (!inherits(df, c("data.frame", "tbl"))) {
      next
    }
    
    openxlsx::addWorksheet(wb, nm)
    
    openxlsx::writeData(
      wb,
      sheet = nm,
      x = df,
      withFilter = TRUE
    )
    
    # Bold header row (row 1)
    openxlsx::addStyle(
      wb,
      sheet = nm,
      style = header_style,
      rows = 1,
      cols = seq_len(ncol(df)),
      gridExpand = TRUE,
      stack = TRUE
    )
    
    # Freeze panes
    if (!is.null(freeze_by_sheet) && !is.null(freeze_by_sheet[[nm]])) {
      fr <- freeze_by_sheet[[nm]]
      
      if (!is.null(fr$cell)) {
        rc <- parse_cell(fr$cell)
        fr$row <- rc$row
        fr$col <- rc$col
      }
      
      # Excel freeze logic:
      # firstActiveRow = row, firstActiveCol = col
      # E2 => row=2 col=5, freezes row 1 and cols A:D
      openxlsx::freezePane(
        wb,
        sheet = nm,
        firstActiveRow = fr$row,
        firstActiveCol = fr$col
      )
    }
    
    # Column widths for first columns
    if (!is.null(col_widths_by_sheet) && !is.null(col_widths_by_sheet[[nm]])) {
      w <- col_widths_by_sheet[[nm]]
      if (length(w) > 0) {
        openxlsx::setColWidths(
          wb,
          sheet = nm,
          cols = seq_along(w),
          widths = w
        )
      }
    }
  }
  
  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
}

split_node_id <- function(node_id) {
  # "gdp@y" -> list(code="gdp", freq="y")
  parts <- stringr::str_split_fixed(node_id, "@", 2)
  list(
    indicator_code = parts[, 1],
    frequency      = parts[, 2]
  )
}

build_indicator_catalog_from_dict <- function(dict) {
  # ожидаем минимум: indicator_code, source_frequency, indicator (или аналог)
  # делаем label как "code@freq — name"
  name_col <- c("indicator", "indicator_name", "name", "indicator_label", "label_ru", "label_en")
  name_col <- name_col[name_col %in% names(dict)] |>
    purrr::pluck(1, .default = NA_character_)
  
  dict |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "") |>
    dplyr::filter(!is.na(.data$source_frequency), .data$source_frequency != "") |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
      node_id        = glue::glue("{indicator_code}@{source_frequency}"),
      label          = dplyr::case_when(
        !is.na(.data$indicator_name) & .data$indicator_name != "" ~
          glue::glue("{node_id} — {indicator_name}"),
        TRUE ~ .data$node_id
      )
    ) |>
    dplyr::distinct(.data$node_id, .keep_all = TRUE) |>
    dplyr::arrange(.data$indicator_code, .data$source_frequency) |>
    dplyr::select(
      node_id, label,
      indicator_code, source_frequency,
      indicator_name,
      dplyr::any_of(c("theme", "source_name"))
    )
}

get_freq_data <- function(fd, freq) {
  # fd = list(extdata_y, extdata_q, extdata_m, extdata_d, dict)
  switch(
    freq,
    y = fd$extdata_y,
    q = fd$extdata_q,
    m = fd$extdata_m,
    d = fd$extdata_d,
    NULL
  )
}

add_period_columns <- function(df, freq) {
  # добавляет year + period (строка для wide-колонок)
  if (is.null(df)) return(df)
  
  if (freq %in% c("y", "q", "m")) {
    if (!("year" %in% names(df))) return(df)
    
    if (freq == "y") {
      df |>
        dplyr::mutate(
          year   = as.integer(.data$year),
          period = as.character(.data$year)
        )
    } else if (freq == "q") {
      if (!("quarter" %in% names(df))) return(df)
      df |>
        dplyr::mutate(
          year   = as.integer(.data$year),
          period = glue::glue("{year}-Q{quarter}")
        )
    } else {
      if (!("month" %in% names(df))) return(df)
      df |>
        dplyr::mutate(
          year   = as.integer(.data$year),
          period = stringr::str_pad(.data$month, 2, pad = "0") |>
            (\(mm) glue::glue("{year}-{mm}"))()
        )
    }
  } else if (freq == "d") {
    if (!("date" %in% names(df))) return(df)
    
    df |>
      dplyr::mutate(
        year   = lubridate::year(.data$date),
        period = as.character(.data$date)
      )
  } else {
    df
  }
}

filter_by_years <- function(df, year_from = NULL, year_to = NULL) {
  if (is.null(df)) return(df)
  if (!("year" %in% names(df))) return(df)
  
  out <- df
  if (!is.null(year_from) && is.finite(year_from)) {
    out <- out |> dplyr::filter(.data$year >= year_from)
  }
  if (!is.null(year_to) && is.finite(year_to)) {
    out <- out |> dplyr::filter(.data$year <= year_to)
  }
  out
}

# ---------- stats table for selected indicators ----------------------------

calc_indicator_stats <- function(fd,
                                 dict,
                                 indicator_code,
                                 frequency,
                                 country_ids,
                                 year_from = NULL,
                                 year_to = NULL) {
  df <- get_freq_data(fd, freq = frequency)
  if (is.null(df)) {
    return(tibble::tibble(
      indicator_code = indicator_code,
      source_frequency = frequency,
      n_countries = 0L,
      start_year = NA_integer_,
      end_year = NA_integer_,
      n_points = 0L
    ))
  }
  
  df <- add_period_columns(df, freq = frequency)
  
  # фильтры стран
  if (!is.null(country_ids) && length(country_ids) > 0 && "country_id" %in% names(df)) {
    df <- df |> dplyr::filter(.data$country_id %in% country_ids)
  }
  
  # фильтры лет
  df <- filter_by_years(df, year_from = year_from, year_to = year_to)
  
  if (!(indicator_code %in% names(df))) {
    return(tibble::tibble(
      indicator_code = indicator_code,
      source_frequency = frequency,
      n_countries = 0L,
      start_year = NA_integer_,
      end_year = NA_integer_,
      n_points = 0L
    ))
  }
  
  x <- df[[indicator_code]]
  ok <- !is.na(x)
  
  # n_countries: сколько стран имеют хотя бы 1 точку
  n_countries <- if ("country_id" %in% names(df)) {
    df |>
      dplyr::filter(ok) |>
      dplyr::distinct(.data$country_id) |>
      nrow()
  } else {
    0L
  }
  
  years_ok <- df$year[ok]
  start_year <- if (length(years_ok) == 0) NA_integer_ else min(years_ok, na.rm = TRUE)
  end_year   <- if (length(years_ok) == 0) NA_integer_ else max(years_ok, na.rm = TRUE)
  
  tibble::tibble(
    indicator_code    = indicator_code,
    source_frequency  = frequency,
    n_countries       = as.integer(n_countries),
    start_year        = as.integer(start_year),
    end_year          = as.integer(end_year),
    n_points          = as.integer(sum(ok))
  )
}

build_custom_summary_table <- function(fd,
                                       dict,
                                       selected_node_ids,
                                       country_ids,
                                       year_from = NULL,
                                       year_to = NULL) {
  if (is.null(selected_node_ids) || length(selected_node_ids) == 0) {
    return(tibble::tibble())
  }
  
  parts <- split_node_id(selected_node_ids)
  
  stats <- purrr::pmap_dfr(
    list(parts$indicator_code, parts$frequency),
    \(indicator_code, frequency) {
      calc_indicator_stats(
        fd             = fd,
        dict           = dict,
        indicator_code = indicator_code,
        frequency      = frequency,
        country_ids    = country_ids,
        year_from      = year_from,
        year_to        = year_to
      )
    }
  )
  
  dict_min <- dict |>
    dplyr::filter(!is.na(.data$indicator_code), !is.na(.data$source_frequency)) |>
    dplyr::distinct(.data$indicator_code, .data$source_frequency, .keep_all = TRUE)
  
  # --- выбираем колонку с названием индикатора (если есть) ---
  name_cols <- c("indicator", "indicator_name", "name", "indicator_label", "label_ru", "label_en")
  name_col  <- name_cols[name_cols %in% names(dict_min)] |>
    purrr::pluck(1, .default = NA_character_)
  
  # --- какие мета-поля реально есть в dict ---
  has_theme       <- "theme" %in% names(dict_min)
  has_source_name <- "source_name" %in% names(dict_min)
  
  # --- оставляем в dict только безопасные мета-колонки (без n_countries и т.п.) ---
  meta_keep <- c("indicator_code", "source_frequency")
  if (!is.na(name_col)) meta_keep <- c(meta_keep, name_col)
  if (has_theme) meta_keep <- c(meta_keep, "theme")
  if (has_source_name) meta_keep <- c(meta_keep, "source_name")
  
  dict_meta <- dict_min |>
    dplyr::select(dplyr::any_of(meta_keep))
  
  out <- stats |>
    dplyr::left_join(
      dict_meta,
      by = dplyr::join_by(indicator_code, source_frequency)
    )
  
  # --- indicator/theme/source_name добавляем безопасно (только если есть) ---
  out <- out |>
    dplyr::mutate(
      indicator = if (!is.na(name_col) && name_col %in% names(out)) out[[name_col]] else NA_character_,
      theme = if (has_theme && "theme" %in% names(out)) out[["theme"]] else NA_character_,
      source_name = if (has_source_name && "source_name" %in% names(out)) out[["source_name"]] else NA_character_
    )
  
  out <- out |>
    dplyr::transmute(
      indicator,
      indicator_code,
      theme,
      source_name,
      source_frequency,
      n_countries,
      start_year,
      end_year,
      n_points
    )
  
  # --- порядок строк как в selected_node_ids ---
  out <- out |>
    dplyr::mutate(
      node_id = glue::glue("{indicator_code}@{source_frequency}"),
      .order  = match(.data$node_id, selected_node_ids)
    ) |>
    dplyr::arrange(.data$.order) |>
    dplyr::select(-.data$node_id, -.data$.order)
  
  out
}

# ---------- build download (wide) ------------------------------------------

build_custom_download_wide <- function(fd,
                                       selected_node_ids,
                                       country_ids,
                                       year_from = NULL,
                                       year_to = NULL) {
  if (is.null(selected_node_ids) || length(selected_node_ids) == 0) {
    return(tibble::tibble())
  }
  if (is.null(country_ids) || length(country_ids) == 0) {
    country_ids <- fd$extdata_y |>
      dplyr::distinct(.data$country_id) |>
      dplyr::pull(.data$country_id)
  }
  
  parts <- split_node_id(selected_node_ids)
  
  long_tbl <- purrr::pmap_dfr(
    list(parts$indicator_code, parts$frequency),
    \(indicator_code, frequency) {
      
      df <- get_freq_data(fd, freq = frequency)
      if (is.null(df)) return(tibble::tibble())
      
      df <- add_period_columns(df, freq = frequency)
      
      if (!("country_id" %in% names(df))) return(tibble::tibble())
      if (!(indicator_code %in% names(df))) return(tibble::tibble())
      
      df |>
        dplyr::filter(.data$country_id %in% country_ids) |>
        filter_by_years(year_from = year_from, year_to = year_to) |>
        dplyr::transmute(
          country_id = .data$country_id,
          indicator  = glue::glue("{indicator_code}@{frequency}"),
          period     = .data$period,
          value      = .data[[indicator_code]]
        )
    }
  )
  
  if (nrow(long_tbl) == 0) {
    return(tibble::tibble())
  }
  
  long_tbl |>
    tidyr::pivot_wider(
      names_from  = .data$period,
      values_from = .data$value
    ) |>
    dplyr::arrange(.data$country_id, .data$indicator)
}

build_custom_download_vertical <- function(fd,
                                           selected_node_ids,
                                           country_ids = NULL,
                                           year_from = NULL,
                                           year_to = NULL) {
  if (is.null(selected_node_ids) || length(selected_node_ids) == 0) {
    return(list())
  }
  
  # Если страны не выбраны — берём все
  if (is.null(country_ids) || length(country_ids) == 0) {
    country_ids <- fd$extdata_y |>
      dplyr::distinct(.data$country_id) |>
      dplyr::pull(.data$country_id)
  }
  
  parts <- split_node_id(selected_node_ids)
  req_tbl <- tibble::tibble(
    indicator_code = parts$indicator_code,
    source_frequency = parts$frequency
  )
  
  # Какие индикаторы по каждой частоте реально запросили
  wanted_by_freq <- req_tbl |>
    dplyr::distinct(.data$source_frequency, .data$indicator_code) |>
    dplyr::group_by(.data$source_frequency) |>
    dplyr::summarise(indicators = list(unique(.data$indicator_code)), .groups = "drop")
  
  build_sheet <- function(freq, time_cols) {
    df <- get_freq_data(fd, freq = freq)
    if (is.null(df)) return(NULL)
    
    inds <- wanted_by_freq |>
      dplyr::filter(.data$source_frequency == freq) |>
      dplyr::pull(.data$indicators) |>
      purrr::pluck(1, .default = character(0))
    
    # Оставляем только те индикаторы, которые реально есть в df
    inds <- intersect(inds, names(df))
    if (length(inds) == 0) return(NULL)
    
    # Фильтр стран
    df <- df |>
      dplyr::filter(.data$country_id %in% country_ids)
    
    # Добавляем year/period и фильтруем по годам (d тоже получит year)
    df <- add_period_columns(df, freq = freq)
    df <- filter_by_years(df, year_from = year_from, year_to = year_to)
    
    # Собираем: country, country_id, time cols, индикаторы
    keep_cols <- c("country", "country_id", time_cols, inds)
    
    df |>
      dplyr::select(dplyr::any_of(keep_cols)) |>
      dplyr::arrange(.data$country, .data$country_id)
  }
  
  sheets <- list(
    y = build_sheet("y", time_cols = c("year")),
    q = build_sheet("q", time_cols = c("year", "quarter")),
    m = build_sheet("m", time_cols = c("year", "quarter", "month")),
    d = {
      df <- build_sheet("d", time_cols = c("date"))
      df
    }
  )
  
  sheets <- purrr::compact(sheets)
  
  # ---- dict (пока базово: только то, что относится к реально выгружаемым индикаторам) ----
  if (!is.null(fd$dict) && nrow(fd$dict) > 0 && length(sheets) > 0) {
    exported <- purrr::imap_dfr(
      sheets,
      \(df, freq) {
        tibble::tibble(
          indicator_code = setdiff(names(df), c("country", "country_id", "year", "quarter", "month", "date")),
          source_frequency = freq
        )
      }
    ) |>
      dplyr::distinct()
    
    sheets$dict <- fd$dict |>
      dplyr::semi_join(exported, by = dplyr::join_by(indicator_code, source_frequency == source_frequency))
  }
  
  sheets
}

# ---------- functions to build the dependency graph -------------------------------------

### From import.R

readImportParams <- function (param_fname, update_mode) {
  
  #param_fname <- here("assets", "_DB", "0_database_params.xlsx")
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

### From fill.R

readFillParams <- function(param_fname, sheet = "fill") {
  
  fillplan <- readxl::read_excel(path = param_fname, sheet = sheet, col_names = TRUE, skip = 1) |> 
    dplyr::filter(.data$active1 == 1)
  
  fillplan
}

extractIndicators <- function(formula, formula_words) {
  
  if (is.na(formula) || formula == "") {
    return(character(0))
  }
  
  # Split formula 
  tokens <- stringr::str_split(string = formula, pattern = "\\/|\\(|\\)|\\+|\\-|\\*|\\=|\\^|\\,|\\s+|>|<|==|!=", 
                               simplify = TRUE)
  tokens <- tokens[tokens != ""]
  
  # Numeric tokens (like "10", "3.5") — dropped
  is_num <- !is.na(suppressWarnings(as.numeric(tokens)))
  
  # Non-indicator tokens dropped:
  # - function names (formula_words: lag, rollsum, coalesce, ...)
  # - short tokens <= 2 (usually operators / service codes)
  to_drop_fun   <- tokens %in% formula_words
  to_drop_short <- nchar(tokens) <= 2L
  
  keep <- !(to_drop_fun | to_drop_short | is_num)
  
  unique(tokens[keep])
}

### From dependencies.R

build_dependency_edges <- function(impplan,
                                   fillplan,
                                   formula_words,
                                   only_active = TRUE) {
  
  fp <- fillplan
  
  if (only_active && "active1" %in% names(fp)) {
    fp <- fp |> dplyr::filter(.data$active1 == 1L)
  }
  
  # все известные комбинации (код, частота)
  known_nodes <- build_dependency_nodes(impplan, fillplan) |>
    dplyr::select(indicator_code, frequency)
  
  fp_base <- fp |>
    dplyr::transmute(
      target_code = .data$new_indicator_code,
      target_freq = .data$new_frequency,
      formula     = .data$formula,
      old_code    = .data$old_indicator_code,
      old_freq    = .data$old_frequency
    )
  
  # 1) источники из формул (та же частота)
  fp_formula_edges <- fp_base |>
    dplyr::mutate(
      formula_sources = purrr::map(
        .data$formula,
        ~ extractIndicators(formula = .x, formula_words = formula_words)
      )
    ) |>
    tidyr::unnest_longer(.data$formula_sources, values_to = "source_code") |>
    dplyr::filter(!is.na(.data$source_code) & .data$source_code != "") |>
    dplyr::distinct(.data$target_code, .data$target_freq,
                    .data$source_code, .data$formula) |>
    dplyr::left_join(
      known_nodes,
      by = dplyr::join_by(source_code == indicator_code),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(.data$frequency),
      .data$frequency == .data$target_freq
    ) |>
    dplyr::transmute(
      source_code = .data$source_code,
      source_freq = .data$frequency,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    ) |>
    dplyr::distinct(
      .data$source_code, .data$source_freq,
      .data$target_code, .data$target_freq,
      .data$formula
    )
  
  # 2) источники по old_indicator_code / old_frequency
  fp_old_edges <- fp_base |>
    dplyr::filter(
      !is.na(.data$old_code), .data$old_code != "",
      !is.na(.data$old_freq), .data$old_freq != ""
    ) |>
    dplyr::transmute(
      source_code = .data$old_code,
      source_freq = .data$old_freq,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    ) |>
    dplyr::distinct(
      .data$source_code, .data$source_freq,
      .data$target_code, .data$target_freq,
      .data$formula
    )
  
  # 3) объединяем
  edges <- dplyr::bind_rows(fp_formula_edges, fp_old_edges) |>
    dplyr::filter(
      !is.na(.data$source_code), !is.na(.data$source_freq),
      !is.na(.data$target_code), !is.na(.data$target_freq)
    ) |>
    dplyr::mutate(
      from_id = glue::glue("{source_code}@{source_freq}"),
      to_id   = glue::glue("{target_code}@{target_freq}")
    ) |>
    dplyr::distinct(.data$from_id, .data$to_id, .keep_all = TRUE)
  
  edges
}

build_dependency_nodes <- function(impplan, fillplan) {
  
  db_col <- dplyr::case_when(
    "database_name" %in% names(impplan) ~ "database_name",
    "source_name"   %in% names(impplan) ~ "source_name",
    TRUE ~ NA_character_
  )
  
  # --- новые: определяем, где лежат названия индикаторов ---
  name_cols_imp  <- c("indicator",
                      "indicator_name", "name", "var_name", "indicator_label",
                      "label_ru", "label_en")
  
  name_cols_fill <- c("new_indicator",
                      "new_indicator_name", "indicator_name", "name", "var_name",
                      "indicator_label", "label_ru", "label_en")
  
  name_col_imp  <- name_cols_imp[name_cols_imp %in% names(impplan)] |> 
    purrr::pluck(1, .default = NA_character_)
  name_col_fill <- name_cols_fill[name_cols_fill %in% names(fillplan)] |> 
    purrr::pluck(1, .default = NA_character_)
  
  nodes_imp <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col_imp)) .data[[name_col_imp]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$indicator_code,
      frequency      = .data$source_frequency,
      db_name        = if (!is.na(db_col)) .data[[db_col]] else NA_character_,
      indicator_name = .data$indicator_name,
      type           = "imported"
    )
  
  nodes_fill <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col_fill)) .data[[name_col_fill]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$new_indicator_code,
      frequency      = .data$new_frequency,
      db_name        = NA_character_,
      indicator_name = .data$indicator_name,
      type           = "computed"
    )
  
  nodes_all <- dplyr::bind_rows(nodes_imp, nodes_fill) |>
    dplyr::filter(!is.na(.data$indicator_code), !is.na(.data$frequency)) |>
    dplyr::distinct(.data$indicator_code, .data$frequency, .keep_all = TRUE) |>
    dplyr::mutate(
      node_id = glue::glue("{indicator_code}@{frequency}"),
      label   = dplyr::case_when(
        !is.na(.data$db_name) ~ glue::glue("{indicator_code}\n({frequency}, {db_name})"),
        TRUE                  ~ glue::glue("{indicator_code}\n({frequency})")
      )
    ) |>
    dplyr::arrange(.data$indicator_code, .data$frequency)
  
  nodes_all
}

build_dependency_graph <- function(impplan,
                                   fillplan,
                                   formula_words,
                                   only_active = TRUE) {
  
  nodes <- build_dependency_nodes(impplan, fillplan)
  edges <- build_dependency_edges(impplan, fillplan, formula_words, only_active)
  
  edges_df <- edges |>
    dplyr::transmute(
      from        = .data$from_id,
      to          = .data$to_id,
      source_code = .data$source_code,
      source_freq = .data$source_freq,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    )
  
  nodes_df <- nodes |>
    dplyr::transmute(
      name           = .data$node_id,
      indicator_code = .data$indicator_code,
      frequency      = .data$frequency,
      db_name        = .data$db_name,
      type           = .data$type,
      label          = .data$label,
      indicator_name = .data$indicator_name
    )
  
  g <- igraph::graph_from_data_frame(
    d        = edges_df,
    vertices = nodes_df,
    directed = TRUE
  )
  
  igraph::V(g)$indicator_code  <- nodes_df$indicator_code[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$freq            <- nodes_df$frequency[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$label           <- nodes_df$label[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$type            <- nodes_df$type[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$db              <- nodes_df$db_name[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$indicator_name  <- nodes_df$indicator_name[match(igraph::V(g)$name, nodes_df$name)]
  
  list(
    graph = g,
    nodes = nodes,
    edges = edges
  )
}

build_indicator_catalog <- function(impplan, fillplan) {
  
  name_cols_imp  <- c("indicator",       # <-- добавили
                      "indicator_name", "name", "var_name", "indicator_label",
                      "label_ru", "label_en")
  name_cols_fill <- c("new_indicator",   # <-- добавили
                      "new_indicator_name", "indicator_name", "name", "var_name",
                      "indicator_label", "label_ru", "label_en")
  
  name_col_imp  <- name_cols_imp[name_cols_imp %in% names(impplan)] |> purrr::pluck(1, .default = NA_character_)
  name_col_fill <- name_cols_fill[name_cols_fill %in% names(fillplan)] |> purrr::pluck(1, .default = NA_character_)
  
  imp <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::mutate(
      name = if (!is.na(name_col_imp)) .data[[name_col_imp]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$indicator_code,
      frequency      = .data$source_frequency,
      name,
      type           = "imported"
    )
  
  fill <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::mutate(
      name = if (!is.na(name_col_fill)) .data[[name_col_fill]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$new_indicator_code,
      frequency      = .data$new_frequency,
      name,
      type           = "computed"
    )
  
  dplyr::bind_rows(imp, fill) |>
    dplyr::filter(!is.na(.data$indicator_code), !is.na(.data$frequency)) |>
    dplyr::distinct(.data$indicator_code, .data$frequency, .keep_all = TRUE) |>
    dplyr::mutate(
      node_id = glue::glue("{indicator_code}@{frequency}"),
      label   = dplyr::case_when(
        !is.na(.data$name) & .data$name != "" ~ glue::glue("{indicator_code}@{frequency} — {name}"),
        TRUE ~ glue::glue("{indicator_code}@{frequency}")
      )
    ) |>
    dplyr::arrange(.data$indicator_code, .data$frequency)
}


get_indicator_vertex <- function(g, indicator_code, frequency = NULL) {
  
  v_codes <- igraph::V(g)$indicator_code
  v_freqs <- igraph::V(g)$freq
  
  idx_code <- which(v_codes == indicator_code)
  
  if (length(idx_code) == 0L) {
    stop(glue::glue("Индикатор '{indicator_code}' отсутствует в графе."), call. = FALSE)
  }
  
  if (is.null(frequency)) {
    freqs <- unique(v_freqs[idx_code])
    if (length(freqs) == 1L) {
      return(igraph::V(g)[idx_code])
    } else {
      stop(
        glue::glue(
          "Индикатор '{indicator_code}' существует на нескольких частотах: {paste(freqs, collapse = ', ')}.\n",
          "Пожалуйста, укажите аргумент frequency = 'd'/'m'/'q'/'y'."
        ),
        call. = FALSE
      )
    }
  } else {
    idx <- idx_code[v_freqs[idx_code] == frequency]
    if (length(idx) == 0L) {
      freqs <- unique(v_freqs[idx_code])
      stop(
        glue::glue(
          "Индикатор '{indicator_code}' не найден на частоте '{frequency}'.\n",
          "Доступные частоты: {paste(freqs, collapse = ', ')}."
        ),
        call. = FALSE
      )
    }
    igraph::V(g)[idx]
  }
}

get_indicator_subgraph <- function(dep_graph,
                                   indicator_code,
                                   frequency = NULL,
                                   direction = c("both", "upstream", "downstream")) {
  
  direction <- match.arg(direction)
  g <- dep_graph$graph
  
  v <- get_indicator_vertex(g, indicator_code = indicator_code, frequency = frequency)
  
  vids <- switch(
    direction,
    upstream   = igraph::subcomponent(g, v, mode = "in"),
    downstream = igraph::subcomponent(g, v, mode = "out"),
    both       = {
      in_nodes  <- igraph::subcomponent(g, v, mode = "in")
      out_nodes <- igraph::subcomponent(g, v, mode = "out")
      union(in_nodes, out_nodes)
    }
  )
  
  igraph::induced_subgraph(g, vids = vids)
}

# ---------- Функция для интерактивного графика (ggiraph) -------------------

plot_indicator_graph_interactive <- function(dep_graph,
                                             indicator_code,
                                             frequency = NULL,
                                             direction = c("both", "upstream", "downstream"),
                                             max_formula_len = 200) {
  
  direction <- match.arg(direction)
  
  g_sub <- get_indicator_subgraph(
    dep_graph      = dep_graph,
    indicator_code = indicator_code,
    frequency      = frequency,
    direction      = direction
  )
  
  if (igraph::gorder(g_sub) == 0L) {
    stop(glue::glue("Подграф для '{indicator_code}' пуст."), call. = FALSE)
  }
  
  # ---- фокусный узел ----
  focus_codes <- igraph::V(g_sub)$indicator_code
  focus_freqs <- igraph::V(g_sub)$freq
  
  igraph::V(g_sub)$is_focus <- (focus_codes == indicator_code) &
    (if (is.null(frequency)) TRUE else focus_freqs == frequency)
  
  # ---- атрибуты рёбер для подсказок ----
  igraph::E(g_sub)$edge_id <- seq_len(igraph::ecount(g_sub))
  
  igraph::E(g_sub)$formula_short <- igraph::E(g_sub)$formula |>
    as.character() |>
    stringr::str_replace_na("") |>
    stringr::str_trunc(max_formula_len)
  
  fill_palette <- c(
    imported = "#CFE5FF",
    computed = "#FFF4C2",
    unknown  = "#E0E0E0"
  )
  
  stroke_palette <- c(
    normal = "#636363"
  )
  
  focus_fill <- "#FDBF6F"
  
  freq_title <- if (is.null(frequency)) {
    unique(igraph::V(g_sub)$freq[igraph::V(g_sub)$is_focus])[1]
  } else {
    frequency
  }
  
  # -------- layout через ggraph --------
  layout <- ggraph::create_layout(g_sub, layout = "sugiyama")
  
  # узлы
  nodes_df <- layout |> 
    tibble::as_tibble() |>
    dplyr::mutate(
      # всплывающая подсказка: код + человеко-читаемое название
      tooltip = dplyr::case_when(
        !is.na(.data$indicator_name) & .data$indicator_name != "" ~ 
          glue::glue("{indicator_code} — {indicator_name}"),
        TRUE ~ indicator_code
      )
    )
  
  # рёбра: данные из igraph + координаты узлов
  edges_tbl <- igraph::as_data_frame(g_sub, what = "edges") |>
    tibble::as_tibble()
  
  edges_df <- edges_tbl |>
    dplyr::left_join(
      nodes_df |>
        dplyr::select(name, x_from = .data$x, y_from = .data$y),
      by = dplyr::join_by(from == name)
    ) |>
    dplyr::left_join(
      nodes_df |>
        dplyr::select(name, x_to = .data$x, y_to = .data$y),
      by = dplyr::join_by(to == name)
    ) |>
    dplyr::transmute(
      x      = .data$x_from,
      y      = .data$y_from,
      xend   = .data$x_to,
      yend   = .data$y_to,
      edge_id,
      tooltip = .data$formula_short
    )
  
  p <- ggplot2::ggplot() +
    
    # 1) Невидимый, но толстый интерактивный слой рёбер — большая зона попадания
    ggiraph::geom_segment_interactive(
      data = edges_df,
      ggplot2::aes(
        x      = .data$x,
        y      = .data$y,
        xend   = .data$xend,
        yend   = .data$yend,
        tooltip = .data$tooltip,
        data_id = .data$edge_id
      ),
      linewidth   = 4,       # толстый hitbox
      alpha       = 0,       # полностью прозрачный
      lineend     = "round",
      show.legend = FALSE
    ) +
    
    # 2) Тонкие видимые рёбра поверх
    ggplot2::geom_segment(
      data = edges_df,
      ggplot2::aes(
        x    = .data$x,
        y    = .data$y,
        xend = .data$xend,
        yend = .data$yend
      ),
      colour  = "#A0A0A0",
      arrow   = grid::arrow(length = grid::unit(3, "mm")),
      lineend = "round",
      alpha   = 0.7,
      linewidth = 0.6,
      show.legend = FALSE
    ) +
    
    # Все узлы — интерактивные подписи
    ggiraph::geom_label_interactive(
      data = nodes_df,
      ggplot2::aes(
        x        = .data$x,
        y        = .data$y,
        label    = .data$label,
        fill     = .data$type,
        colour   = "normal",
        fontface = ifelse(.data$is_focus, "bold", "plain"),
        tooltip  = .data$tooltip,
        data_id  = .data$name
      ),
      label.size    = 0.3,
      label.r       = grid::unit(3, "pt"),
      label.padding = grid::unit(2.5, "pt"),
      size          = 3,
      show.legend   = FALSE
    ) +
    
    # Фокусный узел поверх (тоже интерактивен)
    {
      nodes_focus_df <- nodes_df |> dplyr::filter(.data$is_focus)
      if (nrow(nodes_focus_df) > 0) {
        ggiraph::geom_label_interactive(
          data = nodes_focus_df,
          ggplot2::aes(
            x       = .data$x,
            y       = .data$y,
            label   = .data$label,
            tooltip = .data$tooltip,
            data_id = .data$name
          ),
          fill          = focus_fill,
          colour        = stroke_palette["normal"],
          fontface      = "bold",
          label.size    = 0.3,
          label.r       = grid::unit(3, "pt"),
          label.padding = grid::unit(3, "pt"),
          size          = 3.2,
          show.legend   = FALSE
        )
      } else {
        ggplot2::geom_blank()
      }
    } +
    
    ggplot2::scale_fill_manual(values = fill_palette, na.value = "#E0E0E0") +
    ggplot2::scale_colour_manual(values = stroke_palette, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#F7F8FB", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "#F7F8FB", colour = NA),
      panel.border     = ggplot2::element_rect(colour = "#D0D0D0", fill = NA, linewidth = 0.6),
      plot.margin      = ggplot2::margin(t = 10, r = 20, b = 10, l = 20),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
      axis.title       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(
      glue::glue("Зависимости индикатора: {indicator_code} ({freq_title}, {direction})")
    )
  
  ggiraph::girafe(
    ggobj = p,
    width_svg  = 8,
    height_svg = 6,
    options = list(
      ggiraph::opts_hover(css = "stroke-width: 2;"),
      ggiraph::opts_tooltip(opacity = 0.9),
      ggiraph::opts_selection(
        type       = "single",   # одна выбранная сущность
        only_shiny = TRUE        # выбор передаётся в Shiny
      )
    )
  )
}
