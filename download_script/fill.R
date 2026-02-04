#   All the functions to make calculations on imported data

##### Load libraries
library_names <- c("dplyr","readxl","tidyr","writexl","stringr","purrr", "tidyquant","timetk","glue","lubridate","here")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

here::i_am("download_script/fill.R")


##### Function to set filling schedule

readFillParams <- function(param_fname, sheet = "fill") {
  
  fillplan <- readxl::read_excel(path = param_fname, sheet = sheet, col_names = TRUE, skip = 1) |> 
      dplyr::filter(.data$active1 == 1)
  
  fillplan
}

##### Function to generate date columns in data (first day in each period is chosen)

createDateColumns <- function(extdata_y, extdata_q, extdata_m, extdata_d) { 

    extdata_y <- extdata_y |> dplyr::mutate(date = lubridate::make_date(year = .data$year, month = 1L, day = 1L))
    extdata_q <- extdata_q |> dplyr::mutate(date = lubridate::make_date(year = .data$year, month = 3L*(.data$quarter-1L)+1L, day = 1L))
    extdata_m <- extdata_m |> dplyr::mutate(date = lubridate::make_date(year = .data$year, month = .data$month, day = 1L))
    list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
    
}

##### Function to check variable names correctness

checkNames <- function(fillplan, formula_words) {
  
  # choose the column to check
  target_col <- dplyr::case_when(
    "new_indicator_code" %in% names(fillplan) ~ "new_indicator_code",
    "indicator_code"     %in% names(fillplan) ~ "indicator_code",
    TRUE ~ NA_character_
  )
  
  if (is.na(target_col)) {
    stop("checkNames(): neither 'new_indicator_code' or 'indicator_code' are in fillplan.")
  }
  
  codes <- fillplan[[target_col]]
  
  # if there are no formula words
  if (length(formula_words) == 0L) {
    fillplan$check_names <- 1L
    return(fillplan)
  }
  
  # matrix: each column marks the fact that each word in inside indicators name
  bad_list <- purrr::map(formula_words, ~ stringr::str_detect(codes, stringr::fixed(.x)))
  
  bad_mat <- do.call(cbind, bad_list)
  bad_any <- apply(bad_mat, 1L, any)
  fillplan$check_names <- as.integer(!bad_any)
  
  fillplan
}

##### Function to check uniqueness of variable definitions

checkUnique <- function(fillplan) {
  
  if ("new_indicator_code" %in% names(fillplan)) {
    
    fillplan |> dplyr::mutate(check_unique = dplyr::n(), .by = c(new_indicator_code, new_frequency)) |>
      dplyr::mutate(check_unique = dplyr::if_else(is.na(.data$new_indicator_code) | is.na(.data$new_frequency),
          NA_integer_, as.integer(.data$check_unique == 1L)))
    
  } else if ("indicator_code" %in% names(fillplan)) {
    
    fillplan |> dplyr::mutate(check_unique = dplyr::n(), .by = c(indicator_code, source_frequency)) |>
      dplyr::mutate(check_unique = dplyr::if_else(is.na(.data$indicator_code) | is.na(.data$source_frequency),
          NA_integer_, as.integer(.data$check_unique == 1L)))
    
  } else {
    
    stop("checkUnique(): neither 'new_indicator_code' or 'indicator_code' are in fillplan.")
    
  }
}

##### Function to extract dependencies from the formula

extractIndicators <- function(formula, formula_words) {

  if (is.na(formula) || formula == "") {
    return(character(0))
  }
  
  # 1. Special case: seasonal adjustment
  if (stringr::str_detect(formula, "seas_adj")) {
    sa_args <- parse_seas_adj_formula(formula)
    # нам нужны только индикаторы-рядов: wd и breaks
    ind <- c(sa_args$wd_vars, sa_args$breaks_vars)
    ind <- ind[ind != "" & !is.na(ind)]
    return(unique(ind))
  }
  
  # 2. Special case: indexize(); dropping second argument from indicators
  if (stringr::str_detect(formula, "indexize")) {
    # вытащим первый аргумент (имя индикатора), игнорируя второй
    m <- stringr::str_match(
      formula,
      "indexize\\s*\\(\\s*([A-Za-z0-9_]+)\\s*,"
    )
    if (!all(is.na(m))) {
      # set tokens = only the first arg and function name
      return(m[2])
    }
  }
  
  # 3. Special case: userat(a, b, c, overlap_n=..., method=..., ...)
  #     -> dependencies are ONLY series names (positional args), no keyword args.
  if (stringr::str_detect(formula, "^\\s*userat\\s*\\(")) {
    
    inner <- stringr::str_match(formula, "^\\s*userat\\s*\\((.*)\\)\\s*$")[, 2]
    
    if (is.na(inner) || stringr::str_trim(inner) == "") {
      return(character(0))
    }
    
    parts <- inner |>
      stringr::str_split(",", simplify = FALSE) |>
      purrr::pluck(1) |>
      stringr::str_trim()
    
    # positional args = series names; keyword args contain '='
    vars <- parts[!stringr::str_detect(parts, "=")] |>
      stringr::str_replace_all("^['\"]|['\"]$", "") |>
      stringr::str_trim()
    
    vars <- vars[vars != "" & !is.na(vars)]
    
    # defensively drop any accidental function words if user passes something weird
    vars <- vars[!(vars %in% formula_words)]
    
    return(unique(vars))
  }
  
  # 4. Special case: usedyn(a, b, c, side=..., method=..., ...)
  #     -> dependencies are ONLY series names (positional args), no keyword args.
  if (stringr::str_detect(formula, "^\\s*usedyn\\s*\\(")) {
    
    inner <- stringr::str_match(formula, "^\\s*usedyn\\s*\\((.*)\\)\\s*$")[, 2]
    
    if (is.na(inner) || stringr::str_trim(inner) == "") {
      return(character(0))
    }
    
    parts <- inner |>
      stringr::str_split(",", simplify = FALSE) |>
      purrr::pluck(1) |>
      stringr::str_trim()
    
    vars <- parts[!stringr::str_detect(parts, "=")] |>
      stringr::str_replace_all("^['\"]|['\"]$", "") |>
      stringr::str_trim()
    
    vars <- vars[vars != "" & !is.na(vars)]
    vars <- vars[!(vars %in% formula_words)]
    
    return(unique(vars))
  }
  
  # 5. Normal formula (split it)
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

# extractIndicators("coalesce(gg_debt_maturity_fm, gg_debt_maturity_fsdb)", formula_words = formula_words)

##### Function to check if the calculation is possible in terms of data availability

checkAvailability <- function(fillplan, impplan, formula_words) {
  
  fillplan <- fillplan |> dplyr::mutate(check_availability = 0L)
  
  freq_levels <- c("d", "q", "m", "y")
  
  available <- freq_levels |> rlang::set_names() |>
    purrr::map(\(fr) {impplan |> dplyr::filter(.data$source_frequency == fr) |>
    dplyr::pull(.data$indicator_code)})
  
  # Cycle over rows of fillplan
  n_rows <- nrow(fillplan)
  
  for (i in seq_len(n_rows)) {
    
    oldfreq <- fillplan$old_frequency[i]
    newfreq <- fillplan$new_frequency[i]
    available_now <- available[[oldfreq]]
    
    # extract needed indicators
    needed_formula <- extractIndicators(formula = fillplan$formula[i], formula_words = formula_words)
    needed_aggregation <- fillplan$old_indicator_code[i]
    needed <- c(needed_formula, needed_aggregation)
    needed <- needed[needed != "" & !is.na(needed)]
     
    # empty formula is treated as possible!
    if (length(needed) == 0L || all(needed %in% available_now)) {
      fillplan$check_availability[i] <- 1L
    } else {
      missing_codes <- needed[!(needed %in% available_now)]
      print(missing_codes)
    }
    
    available[[newfreq]] <- c(available[[newfreq]], fillplan$new_indicator_code[i])
  }
  
  fillplan
}

##### Function to check that aggregation / freq-change formulas have both old & new codes

checkAggr <- function(fillplan, change_freq_words) {
  
  pattern <- paste0("\\b(", paste(change_freq_words, collapse = "|"), ")\\b")
  has_relevant_formula <- !is.na(fillplan$formula) & stringr::str_detect(fillplan$formula, pattern)
  fillplan$check_aggr <- 1L
  
  idx <- which(fillplan$active1 == 1L & has_relevant_formula)

  if (length(idx) > 0L) {
    ok <- !is.na(fillplan$old_indicator_code[idx]) &
      !is.na(fillplan$new_indicator_code[idx])
    fillplan$check_aggr[idx] <- as.integer(ok)
  }
  
  fillplan
}

##### Function to check correctness of frequency specification

checkFreq <- function(fillplan, change_freq_words) {
  
  allowed_freq <- c("d", "m", "q", "y")
  
  pattern <- paste0("\\b(", paste(change_freq_words, collapse = "|"), ")\\b")
  has_change_fun <- !is.na(fillplan$formula) & stringr::str_detect(fillplan$formula, pattern)
  fillplan$check_freq <- 1L
  
  idx_active <- which(fillplan$active1 == 1L)
  if (length(idx_active) == 0L) {
    return(fillplan)
  }
  
  old <- fillplan$old_frequency[idx_active]
  new <- fillplan$new_frequency[idx_active]
  needs_change <- has_change_fun[idx_active]
  
  base_ok <- (is.na(old) | old %in% allowed_freq) &
    (is.na(new) | new %in% allowed_freq)
  
  extra_ok <- !needs_change | (!is.na(old) & !is.na(new) & old != new)
  fillplan$check_freq[idx_active] <- as.integer(base_ok & extra_ok)
  
  fillplan
}

####### Function to generate text from the fillplan error report table

explain_fill_errors <- function(error_report) {
  
  # 0. Empty report
  if (is.null(error_report) || nrow(error_report) == 0L) {
    print("No errors in the filling plan")
    return(invisible(NULL))
  }
  
  print("The filling plan cannot be executed. Please, check the following issues:")
  counter <- 0L
  
  # 1. Special case: 1x1 (plan was not read)
  if (nrow(error_report) == 1L && ncol(error_report) == 1L) {
    cat(paste(
      "0. Please, check that the filling plan has at least 1 active row and at least these expected columns:",
      "new_indicator, new_indicator_code, old_indicator_code, old_frequency, new_frequency,",
      "formula, theme, keep, active1",
      sep = "\n"
    ))
    return(invisible(NULL))
  }
  
  # 2. Where the indicator code lies?
  code_col <- dplyr::case_when(
    "new_indicator_code" %in% names(error_report) ~ "new_indicator_code",
    "indicator_code" %in% names(error_report)     ~ "indicator_code",
    TRUE                                          ~ NA_character_
  )
  
  # Sorted and unique codes
  get_codes <- function(df, check_col, code_col_name) {
    if (is.na(code_col_name) || !(code_col_name %in% names(df))) {
      return(character(0))
    }
    
    if (!(check_col %in% names(df))) {
      return(character(0))
    }
    
    df |>
      dplyr::filter(.data[[check_col]] == 0L) |>
      dplyr::pull(.data[[code_col_name]]) |>
      unique() |>
      sort()
  }
  
  # 3. Messages
  
  bad_codes <- get_codes(error_report, "check_names", code_col)
  if (length(bad_codes) > 0L) {
    print(
      glue::glue(
        "{counter <- counter + 1L; counter}. Unknown indicator names in:   {paste(bad_codes, collapse = ', ')}"
      )
    )
  }
  
  bad_codes <- get_codes(error_report, "check_unique", code_col)
  if (length(bad_codes) > 0L) {
    print(
      glue::glue(
        "{counter <- counter + 1L; counter}. Duplicating indicator definitions for:   {paste(bad_codes, collapse = ', ')}"
      )
    )
  }
  
  bad_codes <- get_codes(error_report, "check_availability", code_col)
  if (length(bad_codes) > 0L) {
    print(
      glue::glue(
        "{counter <- counter + 1L; counter}. Source data not available for:   {paste(bad_codes, collapse = ', ')}"
      )
    )
  }
  
  bad_codes <- get_codes(error_report, "check_aggr", code_col)
  if (length(bad_codes) > 0L) {
    print(
      glue::glue(
        "{counter <- counter + 1L; counter}. Aggregation / freq-change formulas with missing old or new indicator code in:   {paste(bad_codes, collapse = ', ')}"
      )
    )
  }
  
  bad_codes <- get_codes(error_report, "check_freq", code_col)
  if (length(bad_codes) > 0L) {
    print(
      glue::glue(
        "{counter <- counter + 1L; counter}. Incorrect old/new frequency specification in:   {paste(bad_codes, collapse = ', ')}"
      )
    )
  }
  
  print(error_report)
  invisible(NULL)
}

##### Function to save length of data containers (time x countries)

captureDimensions <- function(extdata_y, extdata_q, extdata_m, extdata_d) {
  
  c(dim(extdata_y)[1], dim(extdata_q)[1], dim(extdata_m)[1], dim(extdata_d)[1])
  
}

##### Parse textual base period like "2014", "2014Q2", "2014M07", "2014-05-31", "31.05.2014"

parse_base_period <- function(base_expr) {
  # base_expr может быть чем угодно: 2014, "2014Q2", 2014M07, 2014-05-31, 31.05.2014 ...
  
  if (is.null(base_expr) || is.na(base_expr) || base_expr == "") {
    return(NA_Date_)
  }
  
  base_str <- as.character(base_expr) |> stringr::str_trim()
  # убираем кавычки на всякий случай
  base_str <- stringr::str_replace_all(base_str, "^['\"]|['\"]$", "")
  
  # 1) Только год: 2014
  if (stringr::str_detect(base_str, "^\\d{4}$")) {
    year <- as.integer(base_str)
    return(lubridate::make_date(year = year, month = 1L, day = 1L))
  }
  
  # 2) Квартал: 2014Q2
  if (stringr::str_detect(base_str, "^(\\d{4})[Qq](0?[1-4])$")) {
    m <- stringr::str_match(base_str, "^(\\d{4})[Qq](0?[1-4])$")
    year <- as.integer(m[, 2])
    q    <- as.integer(m[, 3])
    month <- 3L * (q - 1L) + 1L
    return(lubridate::make_date(year = year, month = month, day = 1L))
  }
  
  # 3) Месяц: 2014M07
  if (stringr::str_detect(base_str, "^(\\d{4})[Mm](0?[1-9]|1[0-2])$")) {
    m <- stringr::str_match(base_str, "^(\\d{4})[Mm](0?[1-9]|1[0-2])$")
    year  <- as.integer(m[, 2])
    month <- as.integer(m[, 3])
    return(lubridate::make_date(year = year, month = month, day = 1L))
  }
  
  # 4) ISO-дата: 2014-05-31
  date_ymd <- suppressWarnings(lubridate::ymd(base_str))
  if (!is.na(date_ymd)) {
    return(date_ymd)
  }
  
  # 5) Европейский формат: 31.05.2014
  date_dmy <- suppressWarnings(lubridate::dmy(base_str))
  if (!is.na(date_dmy)) {
    return(date_dmy)
  }
  
  warning(glue::glue("parse_base_period(): cannot parse base period '{base_str}'"))
  NA_Date_
}

# parse_base_period("2014M03")

##### Universal indexizer: 100 at base period, works for any freq (y/q/m/d)

indexize_anyfreq <- function(x, date, base_expr) {
  # x    — числовой ряд
  # date — вектор Date (у тебя уже есть в extdata_* после createDateColumns())
  # base_expr — текстовое задание базового периода: 2014, 2014Q2, 2014M07, 2014-05-31, ...
  
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  
  date <- as.Date(date)
  
  base_date <- parse_base_period(base_expr) |> as.Date()
  if (is.na(base_date)) {
    return(rep(NA_real_, length(x)))
  }
  
  # ищем наблюдение, наиболее близкое к базовой дате
  dist <- abs(as.numeric(date - base_date))
  idx  <- which.min(dist)
  
  x_base <- x[idx]
  
  if (is.na(x_base) || x_base == 0) {
    return(rep(NA_real_, length(x)))
  }
  
  100 * x / x_base
}


##### Function to recode score into rating

letterize <- function(score, mode = 1, invalid = NA_character_) {
  
  # Early exit — all NA
  if (all(is.na(score))) { return(rep(invalid, length(score))) }
  
  # Validate inputs
  if (!is.numeric(score)) { stop("`score` must be a numeric (integer-like) vector.") }
  if (!mode %in% c(1L, 2L)) { stop("`mode` must be 1 (upper-case) or 2 (lower-case).") }

  scale_raw <- c("aaa", "aa+", "aa", "aa-", "a+", "a", "a-", "bbb+", "bbb", "bbb-", "bb+",
                 "bb", "bb-", "b+", "b", "b-", "ccc/c")
  
  scale_uc <- toupper(scale_raw)
  lookup   <- if (mode == 1L) scale_uc else scale_raw
  
  # Output vector initialized with invalid
  out <- rep(invalid, length(score))
  
  # Checks for valid integer-like indices
  idx_valid <- !is.na(score) & score >= 1L & score <= length(scale_raw) & (score %% 1L == 0L) 
  
  out[idx_valid] <- lookup[score[idx_valid]]
  out
}

##### Functions to impute data inside gaps

# 1) Fill gaps with last observed value inside

impute_fix_vec <- function(x) {
  # x is a numeric vector, may contain NA
  
  n <- length(x)
  if (n == 0L) return(x)
  
  idx_non_na <- which(!is.na(x))
  # If <2 non-NA points, nothing to interpolate/fill inside
  if (length(idx_non_na) <= 1L) return(x)
  
  res <- x
  
  # Walk over consecutive non-NA points and fill interior gaps
  for (k in seq_len(length(idx_non_na) - 1L)) {
    i <- idx_non_na[k]
    j <- idx_non_na[k + 1L]
    
    # There is a gap only if j > i + 1
    if (j > i + 1L) {
      # fill positions (i+1) : (j-1) with last observed value x[i]
      res[(i + 1L):(j - 1L)] <- x[i]
    }
  }
  
  res
}

# 2) Linear interpolation inside gaps, edges untouched
impute_linear_vec <- function(x) {
  # x is a numeric vector, may contain NA
  
  n <- length(x)
  if (n == 0L) return(x)
  
  idx_non_na <- which(!is.na(x))
  # If <2 non-NA points, impossible to interpolate
  if (length(idx_non_na) <= 1L) return(x)
  
  res <- x
  
  for (k in seq_len(length(idx_non_na) - 1L)) {
    i <- idx_non_na[k]
    j <- idx_non_na[k + 1L]
    
    gap_len <- j - i - 1L
    if (gap_len >= 1L) {
      y0 <- x[i]
      y1 <- x[j]
      
      # На всякий случай: если вдруг один из концов NA, просто пропускаем
      if (is.na(y0) || is.na(y1)) next
      
      # шаг изменения на один индекс
      step <- (y1 - y0) / (j - i)
      # позиции внутри (i, j)
      res[(i + 1L):(j - 1L)] <- y0 + step * seq_len(gap_len)
    }
  }
  
  res
}


##### Helper: parse seas_adj(...) formula into a list of options

parse_seas_adj_formula <- function(formula_str) {
  # Expected patterns:
  # seas_adj()
  # seas_adj(type = 'mult', smooth = 'smooth', component = 'sa', method = 'auto', wd = 'wd_m', breaks = 'br_dummy')
  
  out <- list(
    method       = "auto",    # "auto", "x13", "stl"
    type         = "auto",    # "auto", "add", "mult"
    smooth       = "default", # "default", "less", "smooth", "very_smooth"
    component    = "sa",      # "sa", "trend"
    wd_vars      = character(0),
    breaks_vars  = character(0)
  )
  
  if (is.na(formula_str) || !stringr::str_detect(formula_str, "seas_adj")) {return(out)}
  
  # Cut everything from inside of the brackets
  inner <- stringr::str_match(formula_str, "seas_adj\\s*\\((.*)\\)")[, 2]
  if (is.na(inner) || stringr::str_trim(inner) == "") {return(out)}
  
  # Break by commas of the upper level (works for easy cases)
  args_raw <- inner |> stringr::str_split(",", simplify = FALSE) |> purrr::pluck(1) |> stringr::str_trim()
  strip_quotes <- function(x) {x |> stringr::str_replace_all("^['\"]|['\"]$", "") |> stringr::str_trim()}
  
  # вспомогательная функция для разборки списков имён
  parse_name_list <- function(value_raw) {
    if (is.na(value_raw) || value_raw == "") return(character(0))
    value_raw |> strip_quotes() |> stringr::str_split("\\+", simplify = FALSE) |> purrr::pluck(1) |>
      stringr::str_trim() |> (\(v) v[v != ""])()}
  
  for (arg in args_raw) {
    if (!stringr::str_detect(arg, "=")) next
    
    kv <- stringr::str_split_fixed(arg, "=", 2)
    name <- kv[, 1] |> stringr::str_trim()
    value_raw <- kv[, 2] |> stringr::str_trim()
    value_str <- strip_quotes(value_raw)
    
    name_lc <- tolower(name)
    value_lc <- tolower(value_str)
    
    if (name_lc == "method") {
      if (value_lc %in% c("auto", "x13", "stl")) {
        out$method <- value_lc
      }
    } else if (name_lc == "type") {
      if (value_lc %in% c("auto", "add", "mult")) {
        out$type <- value_lc
      }
    } else if (name_lc == "smooth") {
      if (value_lc %in% c("default", "less", "smooth", "very_smooth")) {
        out$smooth <- value_lc
      }
    } else if (name_lc == "component") {
      if (value_lc %in% c("sa", "trend")) {
        out$component <- value_lc
      }
    } else if (name_lc %in% c("wd", "wd_var", "wd_vars")) {
      out$wd_vars <- parse_name_list(value_raw)
    } else if (name_lc %in% c("breaks", "br", "brs")) {
      out$breaks_vars <- parse_name_list(value_raw)
    }
  }
  
  out
}


##### Core seasonal & calendar adjustment helpers 

# 0. Map frequency code to seasonal period
get_seasonal_period <- function(freq) {
  dplyr::case_when(freq == "m" ~ 12L, freq == "q" ~ 4L, freq == "d" ~ 7L, TRUE  ~ 1L)
}

# 1. X-13 backend (monthly / quarterly only)
seas_adjust_x13_vec <- function(y, date, freq = c("m", "q"), type = c("auto", "add", "mult"),
    component = c("sa", "trend"), xreg = NULL) {
  
  freq  <- match.arg(freq)
  type  <- match.arg(type)
  component <- match.arg(component)
  
  if (!requireNamespace("seasonal", quietly = TRUE)) {
    stop("seas_adjust_x13_vec(): package 'seasonal' is not installed.")
  }
  
  n <- length(y)
  if (n == 0L) return(y)
  if (all(is.na(y))) return(y)
  
  if (anyNA(y)) {
    stop("seas_adjust_x13_vec(): series contains NA; please impute before seasonal adjustment.")
  }
  
  if (!is.null(xreg)) {
    if (any(purrr::map_lgl(xreg, anyNA))) {
      stop("seas_adjust_x13_vec(): regressors (wd/breaks) contain NA; please impute them.")
    }
  }
  
  # трансформация: add / mult / auto
  if (type == "mult") {
    if (any(y <= 0, na.rm = TRUE)) {
      warning("seas_adjust_x13_vec(): non-positive values in 'y' for type='mult'; falling back to additive.")
      y_trans <- y
      type_used <- "add"
    } else {
      y_trans <- log(y)
      type_used <- "mult"
    }
  } else if (type == "add") {
    y_trans <- y
    type_used <- "add"
  } else { # auto
    if (all(y > 0, na.rm = TRUE)) {
      y_trans <- log(y)
      type_used <- "mult"
    } else {
      y_trans <- y
      type_used <- "add"
    }
  }
  
  # ts-объект
  first_date <- min(as.Date(date), na.rm = TRUE)
  start_year <- lubridate::year(first_date)
  start_sub  <- if (freq == "m") lubridate::month(first_date) else lubridate::quarter(first_date)
  period     <- if (freq == "m") 12L else 4L
  
  ts_y <- stats::ts(y_trans, frequency = period, start = c(start_year, start_sub))
  
  xreg_mat <- NULL
  if (!is.null(xreg) && ncol(xreg) > 0L) { xreg_mat <- as.matrix(xreg) }
  
  # Пытаемся построить модель X-13
  fit <- seasonal::seas(x = ts_y, transform.function = "none", regression.aictest = c("td", "easter"), xreg = xreg_mat)
  
  ts_out <- if (component == "sa") {seasonal::final(fit)} else {seasonal::trendcycle(fit)}
  y_out_trans <- as.numeric(ts_out)
  
  if (type_used == "mult") {y_out <- exp(y_out_trans)} else {y_out <- y_out_trans}
  
  # Страховка: X-13 должен вернуть столько же наблюдений, сколько было
  if (length(y_out) == 0L) {
    stop("seas_adjust_x13_vec(): empty result from X-13.")
  }
  
  if (length(y_out) != n) {
    stop(glue::glue(
      "seas_adjust_x13_vec(): unexpected length of result ({length(y_out)}) ",
      "for input of length {n}."
    ))
  }
  
  y_out
}

# 2. STL backend (m/q/d)

seas_adjust_stl_vec <- function(y, date, freq = c("m", "q", "d"), type = c("auto", "add", "mult"),
    smooth = c("default", "less", "smooth", "very_smooth"), component = c("sa", "trend"), xreg = NULL) {
  
  freq     <- match.arg(freq)
  type     <- match.arg(type)
  smooth   <- match.arg(smooth)
  component <- match.arg(component)
  
  n <- length(y)
  if (n == 0L) return(y)
  
  # all-NA → возвращаем как есть
  if (all(is.na(y))) return(y)
  
  if (anyNA(y)) {stop("seas_adjust_stl_vec(): series contains NA; please impute before seasonal adjustment.") }
  
  if (!is.null(xreg)) {
    if (any(purrr::map_lgl(xreg, anyNA))) {
      stop("seas_adjust_stl_vec(): regressors (wd/breaks) contain NA; please impute them.")
    }
  }
  
  period <- get_seasonal_period(freq)
  if (period <= 1L) {
    # Нет сезонности → просто возвращаем ряд (но уже после регрессии на xreg)
    # Всё равно сделаем pre-treatment на xreg, чтобы убрать календарь/брейки.
  }
  
  # 2.1 Трансформация
  if (type == "mult") {
    if (any(y <= 0, na.rm = TRUE)) {
      warning("seas_adjust_stl_vec(): non-positive values in 'y' for type='mult'; falling back to additive.")
      y_trans <- y
      type_used <- "add"
    } else {
      y_trans <- log(y)
      type_used <- "mult"
    }
  } else if (type == "add") {
    y_trans <- y
    type_used <- "add"
  } else { # auto
    if (all(y > 0, na.rm = TRUE)) {
      y_trans <- log(y)
      type_used <- "mult"
    } else {
      y_trans <- y
      type_used <- "add"
    }
  }
  
  # 2.2 Регрессионный pre-treatment на xreg
  if (!is.null(xreg) && ncol(xreg) > 0L) {
    reg_df <- dplyr::as_tibble(xreg)
    reg_df$y_trans <- y_trans
    mod <- stats::lm(y_trans ~ ., data = reg_df)
    fit_reg   <- stats::fitted(mod)
    resid_reg <- stats::resid(mod)
  } else {
    fit_reg   <- rep(0, n)
    resid_reg <- y_trans
  }
  
  # Если нет сезонности (freq="y" или period<=1), то сразу решаем, что:
  # sa: y_trans без сезонности (просто y_trans), trend: сглаживание можно добавить отдельно.
  # В нашем кейсе seas_adj для "y" мы вообще не будем вызывать; это запасной вариант.
  if (period <= 1L) {
    if (component == "sa") {
      y_out_trans <- fit_reg + resid_reg
    } else { # "trend" — тут можно теоретически добавить сглаживание, но оставим как есть
      y_out_trans <- fit_reg + resid_reg
    }
    if (type_used == "mult") {
      return(exp(y_out_trans))
    } else {
      return(y_out_trans)
    }
  }
  
  if (n < 2L * period) {
    warning("seas_adjust_stl_vec(): series too short for STL; returning original series.")
    if (type_used == "mult") {
      return(y)
    } else {
      return(y)
    }
  }
  
  first_date <- min(as.Date(date), na.rm = TRUE)
  start_year <- lubridate::year(first_date)
  
  start_sub  <- dplyr::case_when(
    freq == "m" ~ lubridate::month(first_date),
    freq == "q" ~ lubridate::quarter(first_date),
    freq == "d" ~ as.integer(format(first_date, "%j")), # день года, но для STL это не критично
    TRUE        ~ 1L
  )
  
  ts_res <- stats::ts(resid_reg, frequency = period, start = c(start_year, start_sub))
  
  # Окна сглаживания для STL
  if (smooth == "default") {
    # "periodic" — специальный режим, без явного окна
    s_window <- "periodic"
  } else if (smooth == "less") {
    s_window <- max(7L, period * 1L)
  } else if (smooth == "smooth") {
    s_window <- max(7L, period * 2L)
  } else if (smooth == "very_smooth") {
    s_window <- max(7L, period * 3L)
  } else {
    s_window <- "periodic"
  }
  
  t_window <- as.integer(max(7L, period * 2L))
  if (t_window %% 2L == 0L) t_window <- t_window + 1L
  
  stl_fit <- stats::stl(ts_res, s.window = s_window, t.window = t_window, robust = TRUE)
  
  seasonal_comp <- as.numeric(stl_fit$time.series[, "seasonal"])
  trend_comp    <- as.numeric(stl_fit$time.series[, "trend"])
  remainder     <- as.numeric(stl_fit$time.series[, "remainder"])
  
  if (component == "sa") {
    # SA = fitted_reg + (trend + remainder)
    sa_trans <- fit_reg + trend_comp + remainder
    y_out_trans <- sa_trans
  } else { # "trend"
    # Trend = fitted_reg + trend_comp
    trend_trans <- fit_reg + trend_comp
    y_out_trans <- trend_trans
  }
  
  if (type_used == "mult") {y_out <- exp(y_out_trans)} else {y_out <- y_out_trans}
  y_out
  
}

# 3. Универсальная оболочка: выбирает метод и делает проверки

seas_adjust_series <- function(
    y,
    date,
    freq,
    method   = c("auto", "x13", "stl"),
    type     = c("auto", "add", "mult"),
    smooth   = c("default", "less", "smooth", "very_smooth"),
    component = c("sa", "trend"),
    xreg     = NULL,
    var_name = "variable"
) {
  method    <- match.arg(method)
  type      <- match.arg(type)
  smooth    <- match.arg(smooth)
  component <- match.arg(component)
  
  n <- length(y)
  if (n == 0L) return(y)
  
  # Годовая частота – вообще ничего не делаем
  if (freq == "y") {
    return(y)
  }
  
  # all-NA → возвращаем как есть
  if (all(is.na(y))) return(y)
  
  # --- Разрешаем NA по краям, но не внутри ядра ---
  non_na_idx <- which(!is.na(y))
  if (length(non_na_idx) == 0L) {
    # на всякий случай, но до сюда мы уже отфильтровали all(is.na(y))
    return(y)
  }
  
  i1 <- min(non_na_idx)
  i2 <- max(non_na_idx)
  
  # проверка внутренних NA
  if (any(is.na(y[i1:i2]))) {
    stop(glue::glue(
      "seas_adj(): variable '{var_name}' has internal NA between first and last non-NA; ",
      "please impute internal gaps before seasonal adjustment."
    ))
  }
  
  # Проверка регрессоров: также не допускаем внутренних NA
  if (!is.null(xreg)) {
    xreg_core_check <- xreg[i1:i2, , drop = FALSE]
    if (any(purrr::map_lgl(xreg_core_check, anyNA))) {
      stop(glue::glue(
        "seas_adj(): regressors (wd/breaks) for '{var_name}' have internal NA in the data span; ",
        "please impute them before seasonal adjustment."
      ))
    }
  }
  
  # ядро ряда
  y_core    <- y[i1:i2]
  date_core <- date[i1:i2]
  xreg_core <- if (is.null(xreg)) NULL else xreg[i1:i2, , drop = FALSE]
  
  # заготовка итогового вектора: пока копия исходного
  out_full <- y
  
  # --- Определяем эффективный метод ---
  method_eff <- method
  
  # auto: для m/q -> x13 (если есть seasonal), иначе stl
  if (method_eff == "auto") {
    if (freq %in% c("m", "q") && requireNamespace("seasonal", quietly = TRUE)) {
      method_eff <- "x13"
    } else {
      method_eff <- "stl"
    }
  }
  
  # Для daily мы не используем X-13
  if (freq == "d" && method_eff == "x13") {
    warning("seas_adj(): X-13 is not used for daily data; falling back to STL.")
    method_eff <- "stl"
  }
  
  # Если явно попросили x13, но seasonal не установлен
  if (method_eff == "x13" && !requireNamespace("seasonal", quietly = TRUE)) {
    warning("seas_adj(): package 'seasonal' not available; falling back to STL.")
    method_eff <- "stl"
  }
  
  # --- Вызываем backend на ядре ---
  out_core <- NULL
  
  if (method_eff == "x13") {
    # только m/q
    if (!freq %in% c("m", "q")) {
      warning("seas_adj(): X-13 is only supported for m/q; falling back to STL.")
      out_core <- seas_adjust_stl_vec(
        y         = y_core,
        date      = date_core,
        freq      = freq,
        type      = type,
        smooth    = smooth,
        component = component,
        xreg      = xreg_core
      )
    } else {
      # Пытаемся X-13, если ошибка — fallback на STL
      out_core_try <- try(
        seas_adjust_x13_vec(
          y         = y_core,
          date      = date_core,
          freq      = freq,
          type      = type,
          component = component,
          xreg      = xreg_core
        ),
        silent = TRUE
      )
      
      if (inherits(out_core_try, "try-error")) {
        warning(glue::glue(
          "seas_adj(): X-13 failed for '{var_name}' (freq='{freq}'); falling back to STL."
        ))
        out_core <- seas_adjust_stl_vec(
          y         = y_core,
          date      = date_core,
          freq      = freq,
          type      = type,
          smooth    = smooth,
          component = component,
          xreg      = xreg_core
        )
      } else {
        out_core <- out_core_try
      }
    }
  } else {
    # STL backend
    out_core <- seas_adjust_stl_vec(
      y         = y_core,
      date      = date_core,
      freq      = freq,
      type      = type,
      smooth    = smooth,
      component = component,
      xreg      = xreg_core
    )
  }
  
  # --- Страховка: backend обязан вернуть вектор длины length(y_core) ---
  if (is.null(out_core) || length(out_core) == 0L) {
    stop(glue::glue(
      "seas_adj(): backend returned empty result for '{var_name}' (freq='{freq}')."
    ))
  }
  
  if (length(out_core) != length(y_core)) {
    stop(glue::glue(
      "seas_adj(): backend returned length {length(out_core)} for '{var_name}', ",
      "expected {length(y_core)}."
    ))
  }
  
  # вставляем ядро обратно
  out_full[i1:i2] <- out_core
  
  out_full
}

##### Function to glue series using growth rates

usedyn_vec <- function(anchor,
                       candidate,
                       side = c("left", "right", "both"),
                       method = c("ratio", "diff"),
                       fill_internal = FALSE,
                       allow_nonpositive_ratio = FALSE) {
  side   <- match.arg(side)
  method <- match.arg(method)
  
  if (length(anchor) != length(candidate)) {
    stop("usedyn_vec(): anchor and candidate must have the same length (aligned by date already).")
  }
  
  n <- length(anchor)
  if (n == 0L) return(anchor)
  
  out <- anchor
  
  anchor_non_na <- which(!is.na(anchor))
  if (length(anchor_non_na) == 0L) {
    # нечего якорить: без единой опорной точки протяжка темпами невозможна
    return(out)
  }
  
  i_left  <- min(anchor_non_na) # первая точка якоря
  i_right <- max(anchor_non_na) # последняя точка якоря
  
  # --- helpers: safe growth computations --------------------------------------
  ratio_step <- function(x_next, x_prev) {
    # growth = x_next / x_prev
    if (is.na(x_next) || is.na(x_prev)) return(NA_real_)
    if (!is.finite(x_next) || !is.finite(x_prev)) return(NA_real_)
    if (!allow_nonpositive_ratio && (x_next <= 0 || x_prev <= 0)) return(NA_real_)
    if (x_prev == 0) return(NA_real_)
    x_next / x_prev
  }
  
  diff_step <- function(x_next, x_prev) {
    # growth = x_next - x_prev
    if (is.na(x_next) || is.na(x_prev)) return(NA_real_)
    if (!is.finite(x_next) || !is.finite(x_prev)) return(NA_real_)
    x_next - x_prev
  }
  
  get_step <- if (method == "ratio") ratio_step else diff_step
  
  # --- 1) Fill LEFT tail (backcast) -------------------------------------------
  # Заполняем только позиции < i_left, где у anchor NA, используя темпы candidate.
  # Рекурсия:
  #   ratio: out[t-1] = out[t] / (cand[t] / cand[t-1])
  #   diff : out[t-1] = out[t] - (cand[t] - cand[t-1])
  if (side %in% c("left", "both") && i_left > 1L) {
    # идем назад от i_left к 2, чтобы уметь смотреть t-1
    for (t in seq(from = i_left, to = 2L, by = -1L)) {
      # заполняем только если это левее начала якоря и anchor там NA
      if (!is.na(anchor[t - 1L])) next
      if (!is.na(out[t - 1L])) next
      
      step <- get_step(candidate[t], candidate[t - 1L])
      if (is.na(step)) {
        # темп не посчитать -> дальше назад идти бессмысленно
        break
      }
      
      if (method == "ratio") {
        if (step == 0 || !is.finite(step)) break
        out[t - 1L] <- out[t] / step
      } else {
        out[t - 1L] <- out[t] - step
      }
    }
  }
  
  # --- 2) Fill RIGHT tail (forecast) ------------------------------------------
  # Заполняем только позиции > i_right, где у anchor NA, используя темпы candidate.
  # Рекурсия:
  #   ratio: out[t+1] = out[t] * (cand[t+1] / cand[t])
  #   diff : out[t+1] = out[t] + (cand[t+1] - cand[t])
  if (side %in% c("right", "both") && i_right < n) {
    for (t in seq(from = i_right, to = n - 1L, by = 1L)) {
      if (!is.na(anchor[t + 1L])) next
      if (!is.na(out[t + 1L])) next
      
      step <- get_step(candidate[t + 1L], candidate[t])
      if (is.na(step)) {
        # темп не посчитать -> дальше вперед идти бессмысленно
        break
      }
      
      if (method == "ratio") {
        out[t + 1L] <- out[t] * step
      } else {
        out[t + 1L] <- out[t] + step
      }
    }
  }
  
  # --- 3) (optional) Fill internal gaps ---------------------------------------
  # По умолчанию выключено: пользователь сказал, что внутренние дырки редки
  # и будут закрываться другими методами.
  if (isTRUE(fill_internal)) {
    # Заполняем NA внутри [i_left, i_right], где у anchor NA,
    # используя темпы candidate и ближайшую слева заполненную точку out.
    # Это простой "forward carry via growth" и не гарантирует стыковку с правым якорем.
    for (t in seq(from = i_left + 1L, to = i_right, by = 1L)) {
      if (!is.na(anchor[t])) next
      if (!is.na(out[t])) next
      
      step <- get_step(candidate[t], candidate[t - 1L])
      if (is.na(step)) next
      
      if (method == "ratio") {
        out[t] <- out[t - 1L] * step
      } else {
        out[t] <- out[t - 1L] + step
      }
    }
  }
  
  out
}

##### Function to glue multiple series using growth rate

usedyn_many_vec <- function(series_list,
                            side = c("left", "right", "both"),
                            method = c("ratio", "diff"),
                            fill_internal = FALSE,
                            allow_nonpositive_ratio = FALSE) {
  side   <- match.arg(side)
  method <- match.arg(method)
  
  if (length(series_list) < 1L) {
    stop("usedyn_many_vec(): need at least 1 series.")
  }
  
  out <- series_list[[1L]]
  if (length(series_list) == 1L) return(out)
  
  for (k in 2:length(series_list)) {
    out <- usedyn_vec(
      anchor = out,
      candidate = series_list[[k]],
      side = side,
      method = method,
      fill_internal = fill_internal,
      allow_nonpositive_ratio = allow_nonpositive_ratio
    )
  }
  
  out
}


##### Function to parse usedyn formula

parse_usedyn_formula <- function(formula_str) {
  if (is.na(formula_str) || !stringr::str_detect(formula_str, "^\\s*usedyn\\s*\\(")) {
    return(NULL)
  }
  
  inner <- stringr::str_match(formula_str, "^\\s*usedyn\\s*\\((.*)\\)\\s*$")[, 2]
  if (is.na(inner) || stringr::str_trim(inner) == "") {
    stop("parse_usedyn_formula(): empty usedyn().")
  }
  
  parts <- inner |>
    stringr::str_split(",", simplify = FALSE) |>
    purrr::pluck(1) |>
    stringr::str_trim()
  
  is_kw <- stringr::str_detect(parts, "=")
  
  vars <- parts[!is_kw] |>
    stringr::str_replace_all("^['\"]|['\"]$", "") |>
    stringr::str_trim()
  vars <- vars[vars != ""]
  
  if (length(vars) < 1L) stop("parse_usedyn_formula(): no series provided in usedyn().")
  
  # defaults (под твою задачу: края, без внутренних дырок)
  args <- list(
    side = "both",                 # left | right | both
    method = "ratio",              # ratio | diff
    fill_internal = FALSE,         # по умолчанию не трогаем внутренние дырки
    allow_nonpositive_ratio = FALSE
  )
  
  strip_quotes <- function(x) {
    x |>
      stringr::str_replace_all("^['\"]|['\"]$", "") |>
      stringr::str_trim()
  }
  
  for (p in parts[is_kw]) {
    kv <- stringr::str_split_fixed(p, "=", 2)
    key <- stringr::str_trim(kv[, 1])
    val_raw <- stringr::str_trim(kv[, 2])
    val <- strip_quotes(val_raw)
    
    key_lc <- tolower(key)
    val_lc <- tolower(val)
    
    if (key_lc == "side") {
      args$side <- val_lc
    } else if (key_lc == "method") {
      args$method <- val_lc
    } else if (key_lc == "fill_internal") {
      args$fill_internal <- val_lc %in% c("true", "t", "1")
    } else if (key_lc == "allow_nonpositive_ratio") {
      args$allow_nonpositive_ratio <- val_lc %in% c("true", "t", "1")
    }
  }
  
  if (!args$side %in% c("left", "right", "both")) args$side <- "both"
  if (!args$method %in% c("ratio", "diff")) args$method <- "ratio"
  
  list(vars = vars, args = args)
}


##### Function to glue series using the relation in the overlap

userat_vec <- function(anchor, candidate, overlap_n = 1L,
                       method = c("ratio", "diff"),
                       robust = c("median", "mean"),
                       min_overlap = 3L,
                       allow_negative_ratio = FALSE) {
  method <- match.arg(method)
  robust <- match.arg(robust)
  
  if (length(anchor) != length(candidate)) {
    stop("userat_vec(): anchor and candidate must have the same length (aligned by date already).")
  }
  
  # Где якорь (первый ряд) начинается слева?
  anchor_non_na <- which(!is.na(anchor))
  if (length(anchor_non_na) == 0L) return(anchor) # нечего якорить
  
  anchor_left_idx <- min(anchor_non_na)
  
  # Берём только прошлое кандидата (строго левее начала якоря)
  cand_past_idx <- which(seq_along(candidate) < anchor_left_idx & !is.na(candidate))
  if (length(cand_past_idx) == 0L) return(anchor)
  
  # Окно пересечения: даты, где оба не NA
  overlap_idx <- which(!is.na(anchor) & !is.na(candidate))
  if (length(overlap_idx) < min_overlap) return(anchor)
  
  # "Максимально близко к месту склейки": берём последние overlap_n точек пересечения
  overlap_tail <- utils::tail(overlap_idx, overlap_n)
  
  if (method == "ratio") {
    ratios <- anchor[overlap_tail] / candidate[overlap_tail]
    ratios <- ratios[is.finite(ratios)]
    if (!allow_negative_ratio) ratios <- ratios[ratios > 0]
    
    if (length(ratios) < min_overlap) return(anchor)
    
    coef <- if (robust == "median") stats::median(ratios) else mean(ratios)
    cand_adj <- candidate * coef
  } else {
    diffs <- anchor[overlap_tail] - candidate[overlap_tail]
    diffs <- diffs[is.finite(diffs)]
    if (length(diffs) < min_overlap) return(anchor)
    
    coef <- if (robust == "median") stats::median(diffs) else mean(diffs)
    cand_adj <- candidate + coef
  }
  
  out <- anchor
  # Заполняем только там, где у якоря NA (левее его начала),
  # но кандидат там не NA
  fill_idx <- which(is.na(out) & !is.na(cand_adj) & seq_along(out) < anchor_left_idx)
  out[fill_idx] <- cand_adj[fill_idx]
  
  out
}


##### Function to glue multiple series using ratio

userat_many_vec <- function(series_list, overlap_n = 1L,
                            method = c("ratio", "diff"),
                            robust = c("median", "mean"),
                            min_overlap = 3L,
                            allow_negative_ratio = FALSE) {
  method <- match.arg(method)
  robust <- match.arg(robust)
  
  if (length(series_list) < 1L) {
    stop("userat_many_vec(): need at least 1 series.")
  }
  
  out <- series_list[[1L]]
  if (length(series_list) == 1L) return(out)
  
  for (k in 2:length(series_list)) {
    out <- userat_vec(
      anchor = out,
      candidate = series_list[[k]],
      overlap_n = overlap_n,
      method = method,
      robust = robust,
      min_overlap = min_overlap,
      allow_negative_ratio = allow_negative_ratio
    )
  }
  
  out
}

##### Function to interpret the splicing formula

parse_userat_formula <- function(formula_str) {
  if (is.na(formula_str) || !stringr::str_detect(formula_str, "^\\s*userat\\s*\\(")) {
    return(NULL)
  }
  
  inner <- stringr::str_match(formula_str, "^\\s*userat\\s*\\((.*)\\)\\s*$")[, 2]
  if (is.na(inner) || stringr::str_trim(inner) == "") {
    stop("parse_userat_formula(): empty userat).")
  }
  
  # Разбиваем по запятым на верхнем уровне (как и в ваших parse_* хелперах)
  parts <- inner |>
    stringr::str_split(",", simplify = FALSE) |>
    purrr::pluck(1) |>
    stringr::str_trim()
  
  is_kw <- stringr::str_detect(parts, "=")
  
  vars_raw <- parts[!is_kw]
  vars <- vars_raw |>
    stringr::str_replace_all("^['\"]|['\"]$", "") |>
    stringr::str_trim()
  vars <- vars[vars != ""]
  
  if (length(vars) < 1L) stop("parse_userat_formula(): no series provided in userat().")
  
  # defaults
  args <- list(
    overlap_n = 12L,
    method = "ratio",
    robust = "median",
    min_overlap = 3L,
    allow_negative_ratio = FALSE
  )
  
  strip_quotes <- function(x) {
    x |>
      stringr::str_replace_all("^['\"]|['\"]$", "") |>
      stringr::str_trim()
  }
  
  for (p in parts[is_kw]) {
    kv <- stringr::str_split_fixed(p, "=", 2)
    key <- stringr::str_trim(kv[, 1])
    val_raw <- stringr::str_trim(kv[, 2])
    val <- strip_quotes(val_raw)
    
    key_lc <- tolower(key)
    
    if (key_lc == "overlap_n") {
      args$overlap_n <- as.integer(val)
    } else if (key_lc == "method") {
      args$method <- tolower(val)
    } else if (key_lc == "robust") {
      args$robust <- tolower(val)
    } else if (key_lc == "min_overlap") {
      args$min_overlap <- as.integer(val)
    } else if (key_lc == "allow_negative_ratio") {
      args$allow_negative_ratio <- tolower(val) %in% c("true", "t", "1")
    }
  }
  
  if (!args$method %in% c("ratio", "diff")) args$method <- "ratio"
  if (!args$robust %in% c("median", "mean")) args$robust <- "median"
  if (is.na(args$overlap_n) || args$overlap_n < 1L) args$overlap_n <- 1L
  if (is.na(args$min_overlap) || args$min_overlap < 1L) args$min_overlap <- 1L
  
  list(vars = vars, args = args)
}


##### Function for the filling cycle

fill <- function(fillplan, extdata_y, extdata_q, extdata_m, extdata_d, fill_from = NULL) {

  # Optional: partial recalculation starting from a given indicator
  if (!is.null(fill_from)) {
    
    if (!"new_indicator_code" %in% names(fillplan)) {
      stop("fill(): 'fill_from' is only supported when 'new_indicator_code' is present in fillplan.")
    }
    
    if (!fill_from %in% fillplan$new_indicator_code) {
      stop(glue::glue("fill(): 'fill_from' = '{fill_from}' not found in fillplan$new_indicator_code."))
    }
    
    # Take the *first* occurrence of this indicator and drop all previous rows
    start_idx <- which(fillplan$new_indicator_code == fill_from)[1L]
    
    message(glue::glue(
      "fill(): partial recalculation from row {start_idx} / {nrow(fillplan)}, indicator '{fill_from}'"
    ))
    
    fillplan <- fillplan[start_idx:nrow(fillplan), , drop = FALSE]
  }
  
  extdata <- list(y = extdata_y, q = extdata_q, m = extdata_m, d = extdata_d)
  freq_long <- c(y = "year", q = "quarter", m = "month", d = "date")
  n_rows <- nrow(fillplan)
  
  # For disaggregation (low freq -> high freq)
  # Which columns in the high-frequency data define the parent period
  group_cols_map <- list(
    y = c("year"),              # год → все кварталы/месяцы/дни этого года
    q = c("year", "quarter"),   # квартал → все месяцы/дни этого квартала
    m = c("year", "month")      # месяц → все дни этого месяца
  )
  
  for (i in seq_len(n_rows)) {
    
    oldfreq <- fillplan$old_frequency[i]
    newfreq <- fillplan$new_frequency[i]
    active  <- fillplan$active1[i]
    oldcode <- fillplan$old_indicator_code[i]
    newcode <- fillplan$new_indicator_code[i]
    formula <- fillplan$formula[i]
    
    oldfreq_long <- freq_long[[oldfreq]]
    newfreq_long <- freq_long[[newfreq]]
    
    try({
      
      #### 1a. Simple formulas, same freq ####
      
      if (oldfreq == newfreq && active == 1L && !stringr::str_detect(formula, "roll") &&
          !stringr::str_detect(formula, "fromto") && formula != "share" && !stringr::str_detect(formula, "seas_adj") &&
          !stringr::str_detect(formula, "indexize") && !formula %in% c("impute_fix", "impute_linear") &&
          !stringr::str_detect(formula, "^\\s*userat\\s*\\(") &&
          !stringr::str_detect(formula, "^\\s*usedyn\\s*\\(")) {
        
        df   <- extdata[[oldfreq]]
        expr <- rlang::parse_expr(formula)
        new_sym <- rlang::sym(newcode)
        
        df <- df |> dplyr::group_by(country) |> dplyr::mutate(!!new_sym := (!!expr)) |> dplyr::ungroup()
        
        extdata[[oldfreq]] <- df
        print(glue::glue("i={i}: same-freq formula, new var '{newcode}' at freq '{oldfreq}'"))
      }
      
      #### 1b. Indexize: base = 100 at given period (any freq) ####
      
      if (oldfreq == newfreq && active == 1L &&
          stringr::str_detect(formula, "indexize")) {
        
        m <- stringr::str_match(
          formula,
          "indexize\\s*\\(\\s*([A-Za-z0-9_]+)\\s*,\\s*([^\\)]+)\\)"
        )
        
        if (any(is.na(m))) {
          warning(glue::glue(
            "i={i}: cannot parse indexize() formula '{formula}'. ",
            "Expected something like indexize(code, 2014Q2)"
          ))
        } else {
          base_var   <- stringr::str_trim(m[, 2])
          base_expr  <- stringr::str_trim(m[, 3])

          if (is.na(oldcode) || oldcode == "") {oldcode <- base_var}
          
          df <- extdata[[oldfreq]]
          new_sym <- rlang::sym(newcode)
          
          df <- df |>
            dplyr::group_by(country_id) |>
            dplyr::mutate(
              !!new_sym := indexize_anyfreq(
                x         = .data[[oldcode]],
                date      = .data$date,
                base_expr = base_expr
              )
            ) |>
            dplyr::ungroup()
          
          extdata[[oldfreq]] <- df
          print(glue::glue(
            "i={i}: indexize() '{oldcode}' with base='{base_expr}' -> '{newcode}' at freq '{oldfreq}'"
          ))
        }
      }
      
      #### 1c. Imputation inside gaps (impute_fix / impute_linear), same freq ####
      
      if (oldfreq == newfreq && active == 1L && formula %in% c("impute_fix", "impute_linear")) {
        
        df <- extdata[[oldfreq]]
        old_sym <- rlang::sym(oldcode)
        new_sym <- rlang::sym(newcode)
        
        if (!is.numeric(df[[oldcode]]) && !is.integer(df[[oldcode]])) {
          warning(glue::glue(
            "i={i}: imputation requested for non-numeric variable '{oldcode}' at freq '{oldfreq}'. ",
            "Result may be meaningless."
          ))
        }
        
        impute_fun <- if (formula == "impute_fix") impute_fix_vec else impute_linear_vec
        
        df <- df |> dplyr::arrange(country_id, date) |>
          dplyr::mutate(!!new_sym := impute_fun(.data[[oldcode]]), .by = country_id)
        
        extdata[[oldfreq]] <- df
        
        print(glue::glue("i={i}: {formula} for '{oldcode}' -> '{newcode}' at freq '{oldfreq}' (gaps only, edges kept NA)"))
      }
      
      #### 1d. Seasonal & calendar adjustment via seas_adj(...) ####
      
      if (oldfreq == newfreq && active == 1L && stringr::str_detect(formula, "seas_adj")) {
        
        df <- extdata[[oldfreq]]
        
        if (is.na(oldcode) || oldcode == "") {
          stop(glue::glue(
            "i={i}: 'seas_adj' requires 'old_indicator_code' to be specified."
          ))
        }
        
        if (!oldcode %in% names(df)) {
          stop(glue::glue(
            "i={i}: 'seas_adj' — variable '{oldcode}' not found in data at freq '{oldfreq}'."
          ))
        }
        
        # Разбираем параметры из формулы
        sa_args <- parse_seas_adj_formula(formula)
        
        # Эффективный type: если auto и есть догадки, можно доработать позже;
        # сейчас просто передаем, как есть.
        type_eff     <- sa_args$type
        if (!type_eff %in% c("auto", "add", "mult")) type_eff <- "auto"
        
        method_eff   <- sa_args$method
        if (!method_eff %in% c("auto", "x13", "stl")) method_eff <- "auto"
        
        smooth_eff   <- sa_args$smooth
        if (!smooth_eff %in% c("default", "less", "smooth", "very_smooth")) {
          smooth_eff <- "default"
        }
        
        component_eff <- sa_args$component
        if (!component_eff %in% c("sa", "trend")) component_eff <- "sa"
        
        # Собираем имена регрессоров: рабочие дни + брейки
        xreg_vars <- c(sa_args$wd_vars, sa_args$breaks_vars)
        xreg_vars <- xreg_vars[xreg_vars != "" & !is.na(xreg_vars)]
        
        # Проверка наличия регрессоров в данных
        if (length(xreg_vars) > 0L) {
          missing_xreg <- xreg_vars[!(xreg_vars %in% names(df))]
          if (length(missing_xreg) > 0L) {
            warning(glue::glue(
              "i={i}: 'seas_adj' refers to regressors {toString(missing_xreg)}, ",
              "but they are not found in data at freq '{oldfreq}'. They will be ignored."
            ))
            xreg_vars <- setdiff(xreg_vars, missing_xreg)
          }
        }
        
        new_sym <- rlang::sym(newcode)
        
        # Частота: если "y" — просто копируем ряд
        if (oldfreq == "y") {
          df <- df |>
            dplyr::mutate(!!new_sym := .data[[oldcode]])
          
          extdata[[oldfreq]] <- df
          
          print(glue::glue(
            "i={i}: seas_adj requested for yearly freq; '{oldcode}' copied to '{newcode}' without adjustment."
          ))
        } else {
          
          df <- df |>
            dplyr::arrange(country_id, date) |>
            dplyr::mutate(
              !!new_sym := seas_adjust_series(
                y         = .data[[oldcode]],
                date      = .data$date,
                freq      = oldfreq,
                method    = method_eff,
                type      = type_eff,
                smooth    = smooth_eff,
                component = component_eff,
                xreg      = if (length(xreg_vars) > 0L) dplyr::pick(dplyr::all_of(xreg_vars)) else NULL,
                var_name  = oldcode
              ),
              .by = country_id
            )
          
          extdata[[oldfreq]] <- df
          
          print(glue::glue(
            "i={i}: seas_adj for '{oldcode}' -> '{newcode}' at freq '{oldfreq}' ",
            "(method={method_eff}, type={type_eff}, smooth={smooth_eff}, component={component_eff})"
          ))
        }
      }
      
      #### 1e. userat (right-anchored) multiple series: userat(a, b, c, overlap_n=..., method=...) ####
      
      if (oldfreq == newfreq && active == 1L && stringr::str_detect(formula, "^\\s*userat\\s*\\(")) {
        
        df <- extdata[[oldfreq]]
        new_sym <- rlang::sym(newcode)
        
        sp <- parse_userat_formula(formula)
        if (is.null(sp)) {
          stop(glue::glue("i={i}: cannot parse userat() formula '{formula}'."))
        }
        
        vars <- sp$vars
        args <- sp$args
        
        missing_vars <- vars[!(vars %in% names(df))]
        if (length(missing_vars) > 0L) {
          stop(glue::glue(
            "i={i}: userat() refers to missing variables at freq '{oldfreq}': {toString(missing_vars)}"
          ))
        }
        
        # Важно: выравнивание по дате внутри страны — значит сначала сортируем,
        # и на уровне .by = country_id работаем с векторами одинаковой длины.
        df <- df |>
          dplyr::arrange(country_id, date) |>
          dplyr::mutate(
            !!new_sym := {
              series_list <- dplyr::pick(dplyr::all_of(vars)) |> as.list()
              
              userat_many_vec(
                series_list = series_list,
                overlap_n = args$overlap_n,
                method = args$method,
                robust = args$robust,
                min_overlap = args$min_overlap,
                allow_negative_ratio = args$allow_negative_ratio
              )
            },
            .by = country_id
          )
        
        extdata[[oldfreq]] <- df
        
        print(glue::glue(
          "i={i}: userat() vars={toString(vars)} -> '{newcode}' at freq '{oldfreq}' ",
          "(overlap_n={args$overlap_n}, method={args$method}, robust={args$robust})"
        ))
      }
      
      #### 1f. usedyn 
      
      if (oldfreq == newfreq && active == 1L && stringr::str_detect(formula, "^\\s*usedyn\\s*\\(")) {
        
        df <- extdata[[oldfreq]]
        new_sym <- rlang::sym(newcode)
        
        sp <- parse_usedyn_formula(formula)
        if (is.null(sp)) {
          stop(glue::glue("i={i}: cannot parse usedyn() formula '{formula}'."))
        }
        
        vars <- sp$vars
        args <- sp$args
        
        missing_vars <- vars[!(vars %in% names(df))]
        if (length(missing_vars) > 0L) {
          stop(glue::glue(
            "i={i}: usedyn() refers to missing variables at freq '{oldfreq}': {toString(missing_vars)}"
          ))
        }
        
        df <- df |>
          dplyr::arrange(country_id, date) |>
          dplyr::mutate(
            !!new_sym := {
              # безопасно забираем векторы колонок в list
              series_list <- dplyr::pick(dplyr::all_of(vars)) |> as.list()
              
              usedyn_many_vec(
                series_list = series_list,
                side = args$side,
                method = args$method,
                fill_internal = args$fill_internal,
                allow_nonpositive_ratio = args$allow_nonpositive_ratio
              )
            },
            .by = country_id
          )
        
        extdata[[oldfreq]] <- df
        
        print(glue::glue(
          "i={i}: usedyn() vars={toString(vars)} -> '{newcode}' at freq '{oldfreq}' ",
          "(side={args$side}, method={args$method}, fill_internal={args$fill_internal})"
        ))
      }
      
      #### 2. Rolling-indicators, same freq ####
      
      if (oldfreq == newfreq && active == 1L && stringr::str_detect(formula, "roll")) {
        
        # type: vol -> sd, avg -> mean
        roll_type_raw <- substr(formula, 5, 7)  # "vol" or "avg"
        roll_type <- dplyr::case_when(roll_type_raw == "vol" ~ "sd", roll_type_raw == "avg" ~ "mean", TRUE ~ roll_type_raw)
        
        # window length
        comma_pos <- stringr::str_locate(formula, ", ")[1, 2]
        paren_pos <- stringr::str_locate(formula, "\\)")[1, 1]
        windowlen <- as.numeric(substr(formula, comma_pos + 1L, paren_pos - 1L))
        
        # indicator to transform
        paren_open <- stringr::str_locate(formula, "\\(")[1, 1] + 1L  # start + 1
        comma_arg  <- stringr::str_locate(formula, ",")[1, 1]        # start
        oldcode_roll <- substr(formula, paren_open, comma_arg - 1L)
        print(oldcode_roll)
        
        df <- extdata[[oldfreq]]
        new_sym <- rlang::sym(newcode)
        
        df <- df |> dplyr::group_by(country) |>
          dplyr::mutate(
            !!new_sym := rollapply(
              data  = .data[[oldcode_roll]],
              width = windowlen,
              FUN   = match.fun(roll_type),
              align = "right",
              fill  = NA
            )
          ) |> dplyr::ungroup()
        
        extdata[[oldfreq]] <- df
        print(glue::glue("i={i}: rolling formula '{formula}', new var '{newcode}'"))
      }
      
      #### 3. Aggregate to lower freq (d > m > q > y) ####
      
      if (oldfreq < newfreq && active == 1L && formula %in% c("last", "first", "mean", "max", "min", "sum")) {
        
        df_from <- extdata[[oldfreq]]
        
        old_sym <- rlang::sym(oldcode)
        new_sym <- rlang::sym(newcode)
        fun     <- match.fun(formula)
        
        aggreg <- df_from |> dplyr::select(date, country_id, !!old_sym) |> dplyr::group_by(country_id) |>
          dplyr::filter(!is.na(!!old_sym)) |>
          timetk::summarise_by_time(
            .by       = newfreq_long,
            .date_var = date,
            !!new_sym := fun(!!old_sym, na.rm = TRUE),
            .type     = "floor"
          ) |> dplyr::ungroup()
        
        df_to <- extdata[[newfreq]]
        
        df_to <- df_to |> dplyr::select(-dplyr::any_of(newcode))
        df_to <- df_to |> dplyr::left_join(aggreg, dplyr::join_by(country_id, date))
        # df_to <- df_to |> dplyr::left_join(aggreg, by = c("country_id" = "country_id", "date" = "date"))
        
        extdata[[newfreq]] <- df_to
        print(glue::glue("i={i}: aggregate '{oldcode}' ({oldfreq}->{newfreq}), func='{formula}'"))
      }
      
      #### 4. Disaggregate to higher freq (y < q < m < d) ####
      # desum_fix  : равномерно "размазываем" сумму по подинтервалам, так что сумма новых значений = старое значение
      # demean_fix : просто копируем среднее на все подинтервалы, так что среднее по подинтервалам = старое значение
      
      if (oldfreq > newfreq && active == 1L && formula %in% c("desum_fix", "demean_fix")) {
        
        # Parent columns
        group_cols <- group_cols_map[[oldfreq]]
        
        if (is.null(group_cols)) {
          warning(glue::glue(
            "i={i}: disaggregation {oldfreq}->{newfreq} not supported (no group_cols_map entry)"
          ))
        } else {
          
          df_from <- extdata[[oldfreq]]
          df_to   <- extdata[[newfreq]]
          
          old_sym <- rlang::sym(oldcode)
          new_sym <- rlang::sym(newcode)

          # На всякий случай добавим year/quarter/month из date, если их нет
          add_calendar_cols <- function(df, needed) {
            if (!"date" %in% names(df)) {
              return(df)
            }
            if ("year" %in% needed && !"year" %in% names(df)) {
              df <- df |> dplyr::mutate(year = lubridate::year(.data$date))
            }
            if ("quarter" %in% needed && !"quarter" %in% names(df)) {
              df <- df |> dplyr::mutate(quarter = lubridate::quarter(.data$date))
            }
            if ("month" %in% needed && !"month" %in% names(df)) {
              df <- df |> dplyr::mutate(month = lubridate::month(.data$date))
            }
            df
          }
          
          df_from <- add_calendar_cols(df_from, group_cols)
          df_to   <- add_calendar_cols(df_to,   group_cols)
          
          # Берём старшее значение и переименовываем его, чтобы не конфликтовало
          df_from_subset <- df_from |>
            dplyr::select(country_id, dplyr::all_of(group_cols), !!old_sym) |>
            dplyr::rename(old_value = !!old_sym)
          
          # join_by(country_id, year, quarter, ...) depending on the grouping cols
          by_cols <- c("country_id", group_cols)
          
          df_joined <- df_to |>
            dplyr::left_join(df_from_subset, by = dplyr::join_by(!!!rlang::syms(by_cols)))
          
          if (formula == "desum_fix") {
            df_new <- df_joined |>
              dplyr::group_by(country_id, dplyr::across(dplyr::all_of(group_cols))) |>
              dplyr::mutate(
                n_sub = dplyr::n(),
                !!new_sym := dplyr::if_else(
                  is.na(.data$old_value),
                  NA_real_,
                  .data$old_value / .data$n_sub
                )
              ) |>
              dplyr::ungroup() |>
              dplyr::select(-old_value, -n_sub)
            extdata[[newfreq]] <- df_new
          } else if (formula == "demean_fix") {
            df_new <- df_joined |>
              dplyr::group_by(country_id, dplyr::across(dplyr::all_of(group_cols))) |>
              dplyr::mutate(
                !!new_sym := dplyr::if_else(
                  is.na(.data$old_value),
                  NA_real_,
                  .data$old_value
                )
              ) |>
              dplyr::ungroup() |>
              dplyr::select(-old_value)
            extdata[[newfreq]] <- df_new
          }
        }
      }
      
      #### 5. Share of world number or country sum ####
      
      if (oldfreq == newfreq && active == 1L && identical(fillplan$formula[i], "share")) {
        
        df <- extdata[[oldfreq]]
        old_sym <- rlang::sym(oldcode)
        new_sym <- rlang::sym(newcode)
        
        df <- df |> dplyr::group_by(.data[[oldfreq_long]]) |>
          dplyr::mutate(
            world_total_tmp = dplyr::if_else(
              any(country_id == "1W" & !is.na(.data[[oldcode]])),
              .data[[oldcode]][country_id == "1W"][1],
              sum(.data[[oldcode]][country_id != "1W"], na.rm = TRUE)
            ),
            !!new_sym := dplyr::if_else(
              world_total_tmp == 0 | is.na(world_total_tmp),
              NA_real_,
              .data[[oldcode]] * 100 / world_total_tmp
            )
          ) |> dplyr::ungroup() |> dplyr::select(-world_total_tmp)
        
        extdata[[oldfreq]] <- df
        print(glue::glue("i={i}: share for '{oldcode}' -> '{newcode}' at freq '{oldfreq}'"))
      }
      
      #### 6. fromto: copy indicator values from one country into others ####
      
      if (oldfreq == newfreq && active == 1L && stringr::str_detect(formula, "fromto")) {
        
        # analyze fromto(country_from, c(country1, country2, ...))
        country_from <- substr(
          formula,
          stringr::str_locate(formula, "\\(")[1, 1] + 1L,
          stringr::str_locate(formula, ", ")[1, 1] - 1L
        )
        
        if (is.na(country_from)) {
          country_from <- substr(
            formula,
            stringr::str_locate(formula, "\\(")[1, 1] + 1L,
            stringr::str_locate(formula, "\\)")[1, 1] - 1L
          )
        }
        
        countries_to <- substr(
          formula,
          stringr::str_locate(formula, "c\\(")[1, 1] + 2L,
          stringr::str_locate(formula, "\\)")[1, 1] - 1L
        ) |>
          stringr::str_split(", ") |>
          purrr::pluck(1)
        
        if (all(is.na(countries_to))) {
          countries_to <- extdata$y |>
            dplyr::pull(country_id) |>
            unique()
        }
        
        df_fromfreq <- extdata[[oldfreq]]
        old_sym <- rlang::sym(oldcode)
        new_sym <- rlang::sym(newcode)
        
        from <- df_fromfreq |>
          dplyr::select(date, country_id, !!old_sym) |>
          dplyr::filter(country_id == country_from) |>
          dplyr::select(-country_id) |>
          dplyr::rename(!!new_sym := !!old_sym) |>
          tidyr::crossing(country_id = countries_to)
        
        df_dest <- extdata[[newfreq]]
        df_dest <- df_dest |> dplyr::select(-dplyr::any_of(newcode))
        
        df_dest <- df_dest |>
          dplyr::left_join(
            from,
            dplyr::join_by(date, country_id)
          )
        
        extdata[[newfreq]] <- df_dest
        print(glue::glue("i={i}: fromto '{oldcode}' from {country_from} to {toString(countries_to)}"))
      }
      
      #### 7. Control memory and object size ####
      
      if (i %% 10 == 0L) { gc(verbose = TRUE) }
      
      msg <- paste(
        i,
        nrow(extdata$m), as.numeric(object.size(extdata$m)) / 10^6,
        nrow(extdata$q), as.numeric(object.size(extdata$q)) / 10^6,
        nrow(extdata$y), as.numeric(object.size(extdata$y)) / 10^6,
        sep = " "
      )
      print(msg)
      
    }) # end of try
  }
  
  list(extdata_y = extdata$y, extdata_q = extdata$q, extdata_m = extdata$m, extdata_d = extdata$d)
  
}

##### Function to drop the date

dropDateColumns <- function(extdata_y, extdata_q, extdata_m, extdata_d) {
  
  out <- list(extdata_y = extdata_y, extdata_q = extdata_q, extdata_m = extdata_m, extdata_d = extdata_d)
  out[names(out) != "extdata_d"] <- purrr::map(out[names(out) != "extdata_d"], ~ dplyr::select(.x, -date))
  out
  
}


##### Function to create an export schedule

generateSaveplan <- function(impplan, fillplan) {
  
  print("Generating a saving plan")
  
  # 1. Imported indicators -> dictionary
  impplantodict <- impplan |> dplyr::filter(.data$active == 1) |>
    dplyr::select(indicator, theme, indicator_code, source_frequency, keep, source_name)
  
  # 2. Computed indicators -> dictionary
  #    formula is temporarily put into source_name
  fillplantodict <- fillplan |> dplyr::filter(.data$active1 == 1) |>
    dplyr::transmute(indicator = .data$new_indicator, theme = .data$theme, indicator_code  = .data$new_indicator_code,
      source_frequency = .data$new_frequency, keep = .data$keep, source_name = .data$formula)
  
  n_imp   <- nrow(impplantodict)
  n_fill  <- nrow(fillplantodict)
  
  # 3. Primary saveplan: imported + computed (с formula в source_name)
  saveplan <- unique(rbind(impplantodict, fillplantodict))
  
  # Здесь предполагаем, что коды состоят из [A-Za-z0-9_], а всё остальное — разделители.
  extract_tokens <- function(x) {
    if (is.na(x) || x == "") {
      return(character(0))
    }
    tokens <- stringr::str_split(
      string  = x,
      pattern = "[^A-Za-z0-9_]+",
      simplify = TRUE
    )
    tokens <- tokens[tokens != ""]
    unique(tokens)
  }
  
  # 4. Counting sources for computed indicators
  for (i in seq_len(n_fill)) {
    
    idx_save <- n_imp + i
    raw_source <- if (!is.na(fillplan$old_indicator_code[i])) {
      fillplan$old_indicator_code[i]
    } else {
      fillplantodict$source_name[i]
    }
    
    known_codes <- saveplan$indicator_code[seq_len(n_imp + i - 1L)]
    tokens <- extract_tokens(raw_source)
    used_codes <- tokens[tokens %in% known_codes]
    
    if (length(used_codes) == 0L) {
      final_sources <- "расчеты АКРА"
    } else {
      save_sub <- saveplan[seq_len(n_imp + i - 1L), , drop = FALSE]
      sources_vec <- save_sub$source_name[
        match(used_codes, save_sub$indicator_code)
      ]
      
      parts <- strsplit(sources_vec, ", ", fixed = TRUE)
      parts_vec <- unlist(parts, use.names = FALSE)
      parts_vec <- parts_vec[parts_vec != "расчеты АКРА"]
      final_sources <- c(unique(parts_vec), "расчеты АКРА")
    }
    
    saveplan$source_name[idx_save] <- toString(final_sources)
  }
  
  saveplan |> dplyr::filter(.data$keep == 1)
}


##### Function to export data on certain countries

writeCountryFile <- function(countries, datalist) {
  
  print("Writing country files")
  
  datalist$dict <- dplyr::bind_rows(datalist$dict, datalist$dict_d) |>
    dplyr::select(-c(keep, success, n_countries, n_points))
  
  datalist$dict_d <- NULL
  freq_codes <- c("y", "q", "m", "d")
  
  for (countryname_export in countries) {
    
    print(countryname_export)
    datalist$extdata_d <- datalist$extdata_d |> dplyr::mutate(year = lubridate::year(.data$date)) |>
      dplyr::select(year, date, dplyr::everything())
    
    datalist_country <- datalist
    
    # 1) Filter by country
    for (fr in freq_codes) {
      ext_name <- paste0("extdata_", fr)
      
      datalist_country[[ext_name]] <- datalist_country[[ext_name]] |>
        dplyr::filter(.data$country == countryname_export) |>
        dplyr::select(-c(country, country_id))
    }
    
    # 2) For each indicator determine start_year / end_year
    n_ind <- length(datalist_country$dict$indicator)
    
    for (i in seq_len(n_ind)) {
      
      freq_i <- datalist_country$dict$source_frequency[i]
      code_i <- datalist_country$dict$indicator_code[i]
      
      ext_name <- paste0("extdata_", freq_i)
      df_freq  <- datalist_country[[ext_name]]
      
      a <- df_freq |> dplyr::select(year, dplyr::all_of(code_i)) |> dplyr::filter(!is.na(.data[[code_i]]))
      years_i <- unique(a$year)
      
      datalist_country$dict$start_year[i] <- suppressWarnings(min(years_i, na.rm = TRUE))
      datalist_country$dict$end_year[i] <- suppressWarnings(max(years_i, na.rm = TRUE))
    }
    
    # Dropping indicators with non-standard set (Inf, -Inf, NA)
    datalist_country$dict <- datalist_country$dict |> dplyr::filter(is.finite(.data$start_year), is.finite(.data$end_year))
    
    # 3) For each freq cutting time series and dropping empty columns
    for (fr in freq_codes) {
      
      ext_name <- paste0("extdata_", fr)
      dict_fr <- datalist_country$dict |> dplyr::filter(.data$source_frequency == fr)
      if (nrow(dict_fr) == 0L) {next}
      year_min <- dict_fr$start_year |> unique() |> suppressWarnings(min(na.rm = TRUE))
      year_max <- dict_fr$end_year |> unique() |> suppressWarnings(max(na.rm = TRUE))
      
      datalist_country[[ext_name]] <- datalist_country[[ext_name]] |>
        purrr::discard(~ all(is.na(.x))) |> dplyr::filter(.data$year >= year_min, .data$year <= year_max )
    }
    
    # 4) Preparing list in the needed format
    export_list <- list(y = datalist_country$extdata_y, q = datalist_country$extdata_q,
      m = datalist_country$extdata_m, d = datalist_country$extdata_d, dict = datalist_country$dict)
    
    writexl::write_xlsx(export_list,
      path = here::here("assets", countryname_export, "Data", glue::glue("{countryname_export}_data_filled.xlsx")),
      col_names = TRUE, format_headers = TRUE)
  }
}

##### Function to export data on certain countries in model format (only yearly)

writeCountryModelFile <- function(countries, extdata_y, saveplan) {
  
  message("Writing country model files")
  
  # словарь по годовым индикаторам (как у тебя)
  dict_y <- saveplan |> dplyr::filter(.data$source_frequency == "y")
  
  # фиксируем список индикаторов, которые хотим выгрузить
  model_vars <- c(
    "gdp_pc_usd_wb", "gdp_pc_ppp_wb", "gdp_growth", "gdp_usd", "gdp",
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
    "educ", "amr_male", "amr_female", "amr", "life_length", "hci"
  )
  
  for (countryname_export in countries) {
    
    print(countryname_export)
    country_data <- extdata_y |> dplyr::filter(.data$country == countryname_export)
    if (nrow(country_data) == 0L) {next}
  
    t_data_export <- country_data |> dplyr::select(-country, -country_id, -year) |> dplyr::select(dplyr::any_of(model_vars))
    years <- country_data |> dplyr::pull(.data$year)
    data_export <- t(t_data_export) |> as.data.frame()
    
    colnames(data_export) <- years
    data_export <- cbind(indicator_code = rownames(data_export), data_export, row.names = NULL)
    
    data_export <- data_export |> dplyr::left_join(dict_y, by = dplyr::join_by(indicator_code)) |>
      dplyr::select(indicator, indicator_code, theme, source_name, dplyr::everything()) |>
      dplyr::select(-keep, -source_frequency)
    
    export_list <- list(y = data_export)
    
    writexl::write_xlsx(export_list, path = here::here("assets", countryname_export, "Data",
        glue::glue("{countryname_export}_data_model.xlsx")),
          col_names      = TRUE,
          format_headers = TRUE)
  }
}