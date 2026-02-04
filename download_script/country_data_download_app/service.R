library("dplyr")
library("readxl")
library("readr")
library("tidyr")
library("writexl")
library("glue")
library("here")
library("lubridate")
library("purrr")
library("openxlsx")

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