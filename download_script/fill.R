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

##### Function for the filling cycle

fill <- function(fillplan, extdata_y, extdata_q, extdata_m, extdata_d) {

  extdata <- list(y = extdata_y, q = extdata_q, m = extdata_m, d = extdata_d)
  freq_long <- c(y = "year", q = "quarter", m = "month", d = "date")
  n_rows <- nrow(fillplan)
  
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
      
      #### 1. Simple formulas, same freq ####
      
      if (oldfreq == newfreq && active == 1L && !stringr::str_detect(formula, "roll") &&
          !stringr::str_detect(formula, "fromto") && formula != "share") {
        
        df   <- extdata[[oldfreq]]
        expr <- rlang::parse_expr(formula)
        new_sym <- rlang::sym(newcode)
        
        df <- df |> dplyr::group_by(country) |> dplyr::mutate(!!new_sym := (!!expr)) |> dplyr::ungroup()
        
        extdata[[oldfreq]] <- df
        print(glue::glue("i={i}: same-freq formula, new var '{newcode}' at freq '{oldfreq}'"))
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
        
        df_to <- df_to |> dplyr::left_join(aggreg, by = c("country_id" = "country_id", "date" = "date"))
        
        extdata[[newfreq]] <- df_to
        print(glue::glue("i={i}: aggregate '{oldcode}' ({oldfreq}->{newfreq}), func='{formula}'"))
      }
      
      #### 4. Share of world number or country sum ####
      
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
      
      #### 5. fromto: copy indicator values from one country into others ####
      
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
        
        df_dest <- df_dest |>
          dplyr::left_join(
            from,
            dplyr::join_by(date, country_id)
          )
        
        extdata[[newfreq]] <- df_dest
        print(glue::glue("i={i}: fromto '{oldcode}' from {country_from} to {toString(countries_to)}"))
      }
      
      #### 6. Control memory and object size ####
      
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
    "gg_debt_fc_role_fsdb", "gg_debt_held_global_usd",
    "gg_debt_held_global_role", "gg_debt_maturity", "dpension2030",
    "ca_usd", "ca_gdp", "ex_gs_usd", "imp_gs_usd", "intres_usd",
    "intrestoimp", "niip_ex_ggcb_usd", "niip_ex_ggcb_gdp",
    "ex_div", "neer_av", "usdlc_eop", "usdlc_av",
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