library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(jsonlite)
library(base64enc)

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
        TRUE ~ node_id
      )
    ) |>
    dplyr::distinct(node_id, .keep_all = TRUE) |>
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
      .order  = match(node_id, selected_node_ids)
    ) |>
    dplyr::arrange(.data$.order) |>
    dplyr::select(-node_id, -.data$.order)
  
  out
}

# ---------- build download (wide) ------------------------------------------

build_custom_download_time_in_columns <- function(fd,
                                                  selected_node_ids,
                                                  country_ids = NULL,
                                                  year_from = NULL,
                                                  year_to = NULL) {
  if (is.null(selected_node_ids) || length(selected_node_ids) == 0) {
    return(list())
  }
  
  # если страны не выбраны — берём все
  if (is.null(country_ids) || length(country_ids) == 0) {
    country_ids <- fd$extdata_y |>
      dplyr::distinct(.data$country_id) |>
      dplyr::pull(.data$country_id)
  }
  
  parts <- split_node_id(selected_node_ids)
  req_tbl <- tibble::tibble(
    indicator_code    = parts$indicator_code,
    source_frequency  = parts$frequency
  )
  
  wanted_by_freq <- req_tbl |>
    dplyr::distinct(.data$source_frequency, .data$indicator_code) |>
    dplyr::group_by(.data$source_frequency) |>
    dplyr::summarise(indicators = list(unique(.data$indicator_code)), .groups = "drop")
  
  build_sheet <- function(freq) {
    df <- get_freq_data(fd, freq = freq)
    if (is.null(df)) return(NULL)
    
    inds <- wanted_by_freq |>
      dplyr::filter(.data$source_frequency == freq) |>
      dplyr::pull(.data$indicators) |>
      purrr::pluck(1, .default = character(0))
    
    inds <- intersect(inds, names(df))
    if (length(inds) == 0) return(NULL)
    
    df <- df |>
      dplyr::filter(.data$country_id %in% country_ids) |>
      add_period_columns(freq = freq) |>
      filter_by_years(year_from = year_from, year_to = year_to)
    
    # long: country/country_id/period + indicator_code/value
    long <- df |>
      dplyr::select(dplyr::any_of(c("country", "country_id", "period", inds))) |>
      tidyr::pivot_longer(
        cols      = dplyr::all_of(inds),
        names_to  = "indicator_code",
        values_to = "value"
      )
    
    if (nrow(long) == 0) return(NULL)
    
    long |>
      tidyr::pivot_wider(
        names_from  = period,
        values_from = .data$value
      ) |>
      dplyr::arrange(.data$country, .data$country_id, .data$indicator_code)
  }
  
  sheets <- list(
    y = build_sheet("y"),
    q = build_sheet("q"),
    m = build_sheet("m"),
    d = build_sheet("d")
  ) |>
    purrr::compact()
  
  # dict пока базовый: по реально попавшим indicator_code@freq
  if (!is.null(fd$dict) && nrow(fd$dict) > 0 && length(sheets) > 0) {
    exported <- purrr::imap_dfr(
      sheets,
      \(df, freq) {
        tibble::tibble(
          indicator_code   = df$indicator_code %||% character(0),
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

# ---- Recipe: encode/decode state to compact string --------------

encode_recipe <- function(recipe) {
  # recipe: list(
  #   indicators, countries, year_from, year_to, time_layout
  # )
  
  # --- 1) короткий payload (семантическое сжатие) ---
  payload <- list(
    v  = 1L,
    i  = recipe$indicators %||% character(0),
    c  = recipe$countries  %||% character(0),
    yf = recipe$year_from,
    yt = recipe$year_to,
    t  = if (identical(recipe$time_layout, "rows")) 2L else 1L
  ) |>
    purrr::discard(is.null)
  
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  raw  <- charToRaw(json)
  
  gz <- memCompress(raw, type = "gzip")
  
  b64 <- base64enc::base64encode(gz)
  b64url <- b64 |>
    gsub("\\+", "-", x = _, fixed = FALSE) |>
    gsub("/", "_",  x = _, fixed = TRUE)  |>
    gsub("=+$", "", x = _, fixed = FALSE)
  
  paste0("CUST1:", b64url)
}

decode_recipe <- function(code) {
  code <- trimws(code %||% "")
  if (code == "") rlang::abort("Empty recipe code.")
  
  if (!startsWith(code, "CUST1:")) {
    rlang::abort("Unsupported recipe format (expected prefix CUST1:).")
  }
  
  payload <- sub("^CUST1:", "", code)
  
  b64 <- payload |>
    gsub("-", "+", x = _, fixed = TRUE) |>
    gsub("_", "/", x = _, fixed = TRUE)
  
  pad <- (4 - (nchar(b64) %% 4)) %% 4
  if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
  
  gz_raw  <- base64enc::base64decode(b64)
  jsonraw <- memDecompress(gz_raw, type = "gzip")
  json    <- rawToChar(jsonraw)
  
  # simplifyVector=TRUE -> массивы сразу станут character vector
  x <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  
  # --- 2) нормализация: поддержка нового и старого payload ---
  indicators <- x$i %||% x$indicators %||% character(0)
  countries  <- x$c %||% x$countries  %||% character(0)
  
  yf <- x$yf %||% x$year_from
  yt <- x$yt %||% x$year_to
  
  # t: 1=columns, 2=rows; в старом формате было time_layout="columns"/"rows"
  time_layout <- {
    if (!is.null(x$t)) {
      if (identical(as.integer(x$t), 2L)) "rows" else "columns"
    } else {
      tl <- x$time_layout %||% "columns"
      tl <- as.character(tl)[1]
      if (is.na(tl) || tl == "") "columns" else tl
    }
  }
  
  list(
    indicators  = as.character(indicators),
    countries   = as.character(countries),
    year_from   = if (is.null(yf)) NULL else as.integer(yf),
    year_to     = if (is.null(yt)) NULL else as.integer(yt),
    time_layout = time_layout
  )
}
