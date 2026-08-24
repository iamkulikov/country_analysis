# Trace journal: cell provenance for Calculation Trace.
# node_type includes aggregate_input (operand snapshot; not walked as a tree).

library(dplyr)
library(glue)
library(rlang)
library(stringr)
library(purrr)
library(lubridate)

TRACE_FORMULA_WORDS <- c(
  "lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first",
  "min", "pmin", "max", "pmax", "sum", "coalesce", "share", "exp", "fromto",
  "year", "na_if", "cummax", "cummin", "cumsum", "ceiling", "letterize",
  "indexize", "demean_fix", "desum_fix", "impute_fix", "impute_linear",
  "seas_adj", "usedyn", "userat", "if_else", "ifelse", "sqrt", "log", "abs",
  "rollapply"
)

FREQ_ORDER <- c(d = 1L, m = 2L, q = 3L, y = 4L)

# ---------- saveplan for keep / metadata ------------------------------------

build_trace_saveplan <- function(impplan, fillplan) {
  imp_rows <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::transmute(
      indicator_code   = .data$indicator_code,
      source_frequency = .data$source_frequency,
      keep             = .data$keep,
      indicator        = .data$indicator,
      theme            = .data$theme
    )

  fill_rows <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::transmute(
      indicator_code   = .data$new_indicator_code,
      source_frequency = .data$new_frequency,
      keep             = .data$keep,
      indicator        = .data$new_indicator,
      theme            = .data$theme
    )

  dplyr::bind_rows(imp_rows, fill_rows) |>
    dplyr::distinct(.data$indicator_code, .data$source_frequency, .keep_all = TRUE)
}

# ---------- period helpers --------------------------------------------------

.trace_parse_base_period <- function(base_expr) {
  if (is.null(base_expr) || is.na(base_expr) || base_expr == "") {
    return(NA_Date_)
  }

  base_str <- as.character(base_expr) |> stringr::str_trim()
  base_str <- stringr::str_replace_all(base_str, "^['\"]|['\"]$", "")

  if (stringr::str_detect(base_str, "^\\d{4}$")) {
    return(lubridate::make_date(year = as.integer(base_str), month = 1L, day = 1L))
  }
  if (stringr::str_detect(base_str, "^(\\d{4})[Qq](0?[1-4])$")) {
    m <- stringr::str_match(base_str, "^(\\d{4})[Qq](0?[1-4])$")
    year  <- as.integer(m[, 2])
    q     <- as.integer(m[, 3])
    month <- 3L * (q - 1L) + 1L
    return(lubridate::make_date(year = year, month = month, day = 1L))
  }
  if (stringr::str_detect(base_str, "^(\\d{4})[Mm](0?[1-9]|1[0-2])$")) {
    m <- stringr::str_match(base_str, "^(\\d{4})[Mm](0?[1-9]|1[0-2])$")
    return(lubridate::make_date(year = as.integer(m[, 2]), month = as.integer(m[, 3]), day = 1L))
  }

  date_ymd <- suppressWarnings(lubridate::ymd(base_str))
  if (!is.na(date_ymd)) return(date_ymd)

  date_dmy <- suppressWarnings(lubridate::dmy(base_str))
  if (!is.na(date_dmy)) return(date_dmy)

  NA_Date_
}

formatDateAsPeriod <- function(date, frequency) {
  date <- as.Date(date)
  switch(
    frequency,
    y = as.character(lubridate::year(date)),
    q = glue::glue("{lubridate::year(date)}-Q{lubridate::quarter(date)}"),
    m = format(date, "%Y-%m"),
    d = as.character(date),
    as.character(date)
  )
}

shiftPeriod <- function(period, frequency, k) {
  if (is.na(period) || period == "" || k == 0L) return(period)

  switch(
    frequency,
    y = as.character(as.integer(period) + k),
    q = {
      m <- stringr::str_match(period, "^(\\d+)-Q([1-4])$")
      if (any(is.na(m))) return(period)
      total_q <- as.integer(m[, 2]) * 4L + (as.integer(m[, 3]) - 1L) + k
      new_year <- total_q %/% 4L
      new_q <- total_q %% 4L + 1L
      glue::glue("{new_year}-Q{new_q}")
    },
    m = {
      parts <- stringr::str_match(period, "^(\\d+)-(\\d{2})$")
      if (any(is.na(parts))) return(period)
      year  <- as.integer(parts[, 2])
      month <- as.integer(parts[, 3])
      total <- year * 12L + (month - 1L) + as.integer(k)
      new_year  <- total %/% 12L
      new_month <- total %% 12L + 1L
      glue::glue("{new_year}-{stringr::str_pad(new_month, 2, pad = '0')}")
    },
    d = {
      d <- suppressWarnings(lubridate::ymd(period))
      if (is.na(d)) return(period)
      as.character(d + k)
    },
    period
  )
}

parentPeriodOf <- function(period, child_frequency, parent_frequency) {
  child_rank <- FREQ_ORDER[[child_frequency]]
  parent_rank <- FREQ_ORDER[[parent_frequency]]
  if (is.null(child_rank) || is.null(parent_rank) || parent_rank <= child_rank) {
    return(period)
  }

  switch(
    child_frequency,
    d = switch(
      parent_frequency,
      m = format(lubridate::ymd(period), "%Y-%m"),
      q = {
        d <- lubridate::ymd(period)
        glue::glue("{lubridate::year(d)}-Q{lubridate::quarter(d)}")
      },
      y = as.character(lubridate::year(lubridate::ymd(period))),
      period
    ),
    m = switch(
      parent_frequency,
      q = {
        m <- stringr::str_match(period, "^(\\d+)-(\\d{2})$")
        if (any(is.na(m))) return(period)
        year  <- as.integer(m[, 2])
        month <- as.integer(m[, 3])
        glue::glue("{year}-Q{(month - 1L) %/% 3L + 1L}")
      },
      y = stringr::str_match(period, "^(\\d+)")[, 2],
      period
    ),
    q = switch(
      parent_frequency,
      y = stringr::str_match(period, "^(\\d+)")[, 2],
      period
    ),
    period
  )
}

subPeriodsOf <- function(period, parent_frequency, child_frequency) {
  parent_rank <- FREQ_ORDER[[parent_frequency]]
  child_rank  <- FREQ_ORDER[[child_frequency]]
  if (is.null(parent_rank) || is.null(child_rank) || child_rank >= parent_rank) {
    return(period)
  }

  if (parent_frequency == "y" && child_frequency == "q") {
    year <- as.integer(period)
    return(glue::glue("{year}-Q{1:4}"))
  }
  if (parent_frequency == "y" && child_frequency == "m") {
    year <- as.integer(period)
    months <- stringr::str_pad(1:12, 2, pad = "0")
    return(glue::glue("{year}-{months}"))
  }
  if (parent_frequency == "q" && child_frequency == "m") {
    m <- stringr::str_match(period, "^(\\d+)-Q([1-4])$")
    if (any(is.na(m))) return(character(0))
    year <- as.integer(m[, 2])
    q    <- as.integer(m[, 3])
    start_month <- 3L * (q - 1L) + 1L
    months <- start_month:(start_month + 2L)
    return(glue::glue("{year}-{stringr::str_pad(months, 2, pad = '0')}"))
  }
  if (parent_frequency == "m" && child_frequency == "d") {
    start <- lubridate::ymd(glue::glue("{period}-01"))
    end   <- lubridate::ceiling_date(start, "month") - 1L
    as.character(seq(start, end, by = "day"))
  } else if (parent_frequency == "y" && child_frequency == "d") {
    year <- as.integer(period)
    start <- lubridate::make_date(year, 1L, 1L)
    end   <- lubridate::make_date(year, 12L, 31L)
    as.character(seq(start, end, by = "day"))
  } else {
    character(0)
  }
}

periodsUpTo <- function(trace_db, country_id, indicator_code, frequency, period) {
  df <- get_freq_data(trace_db, frequency)
  if (is.null(df)) return(character(0))

  df <- add_period_columns(df, frequency)
  df <- df |>
    dplyr::filter(.data$country_id == country_id) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("year", "quarter", "month", "date"))))

  if (!indicator_code %in% names(df)) return(character(0))

  periods <- df$period
  idx <- match(period, periods)
  if (is.na(idx)) return(character(0))
  periods[seq_len(idx)]
}

rollPeriods <- function(period, frequency, window_len) {
  if (window_len <= 0L) return(character(0))
  vapply(seq(window_len - 1L, 0L), function(k) shiftPeriod(period, frequency, -k), character(1))
}

# ---------- cell lookup -------------------------------------------------------

getCellValue <- function(trace_db, country_id, indicator_code, frequency, period) {
  df <- get_freq_data(trace_db, frequency)
  if (is.null(df) || !indicator_code %in% names(df)) {
    return(NA_real_)
  }

  cid  <- as.character(country_id)
  code <- as.character(indicator_code)
  per  <- as.character(period)

  df <- add_period_columns(df, frequency)
  row <- df |>
    dplyr::filter(
      .data$country_id == .env$cid,
      as.character(.data$period) == .env$per
    )

  if (nrow(row) == 0L) return(NA_real_)
  val <- row[[code]][[1]]
  if (is.null(val)) return(NA_real_)
  suppressWarnings(as.numeric(val))
}

find_fillplan_row <- function(indicator_code, frequency, fillplan) {
  fillplan |>
    dplyr::filter(
      .data$new_indicator_code == indicator_code,
      .data$new_frequency == frequency,
      .data$active1 == 1L
    ) |>
    dplyr::slice(1)
}

find_impplan_row <- function(indicator_code, frequency, impplan) {
  impplan |>
    dplyr::filter(
      .data$indicator_code == indicator_code,
      .data$source_frequency == frequency,
      .data$active == 1L
    ) |>
    dplyr::slice(1)
}

lookup_indicator_kind <- function(indicator_code, frequency, fillplan, impplan) {
  fp <- find_fillplan_row(indicator_code, frequency, fillplan)
  if (nrow(fp) > 0L) {
    return(list(kind = "computed", fillplan_row = fp, impplan_row = NULL))
  }

  ip <- find_impplan_row(indicator_code, frequency, impplan)
  if (nrow(ip) > 0L) {
    return(list(kind = "imported", fillplan_row = NULL, impplan_row = ip))
  }

  list(kind = "missing", fillplan_row = NULL, impplan_row = NULL)
}

import_source_label <- function(imp_row) {
  parts <- c(
    imp_row$database_name %||% NA_character_,
    imp_row$retrieve_code %||% NA_character_,
    imp_row$file_name %||% NA_character_,
    imp_row$source_name %||% NA_character_
  )
  parts <- parts[!is.na(parts) & parts != ""]
  if (length(parts) == 0L) "imported" else paste(parts, collapse = " / ")
}

.format_trace_value <- function(x) {
  if (length(x) == 0L || is.na(x)) return("NA")
  if (is.numeric(x)) {
    formatted <- format(x, digits = 6, trim = TRUE, scientific = FALSE)
    return(formatted)
  }
  as.character(x)
}

# ---------- formula formatting -----------------------------------------------

.TRACE_INFIX_OPS <- c(
  "+", "-", "*", "/", "^", "%%",
  "==", "!=", "<", ">", "<=", ">=",
  "&", "|"
)

.trace_op_precedence <- function(op) {
  switch(
    op,
    "|" = 1L,
    "&" = 2L,
    "==" = 3L, "!=" = 3L, "<" = 3L, ">" = 3L, "<=" = 3L, ">=" = 3L,
    "+" = 4L, "-" = 4L,
    "*" = 5L, "/" = 5L, "%%" = 5L,
    "^" = 6L,
    0L
  )
}

.trace_op_right_assoc <- function(op) {
  identical(op, "^")
}

.format_expr_node <- function(node, value_map, parent_prec = 0L, is_rhs = FALSE) {
  if (is.symbol(node)) {
    nm <- rlang::as_string(node)
    if (nm %in% names(value_map)) {
      return(value_map[[nm]])
    }
    return(nm)
  }

  if (!is.call(node)) {
    return(.format_trace_value(node))
  }

  fn <- rlang::as_string(node[[1]])
  args <- node[-1]

  # R keeps source grouping as a call to `(`. Unwrap and let precedence
  # decide whether parentheses are needed in the printed form.
  if (identical(fn, "(") && length(args) == 1L) {
    return(.format_expr_node(args[[1]], value_map, parent_prec, is_rhs = is_rhs))
  }

  if (fn %in% c("lag", "lead")) {
    sym <- if (length(args) >= 1L && is.symbol(args[[1]])) rlang::as_string(args[[1]]) else "?"
    k <- if (length(args) >= 2L) as.integer(rlang::eval_tidy(args[[2]])) else 1L
    val <- value_map[[sym]] %||% sym
    return(glue::glue("{fn}({val}, {k})"))
  }

  if (fn == "coalesce") {
    vals <- vapply(args, function(a) {
      if (is.symbol(a)) {
        value_map[[rlang::as_string(a)]] %||% rlang::as_string(a)
      } else {
        .format_expr_node(a, value_map)
      }
    }, character(1))
    return(glue::glue("coalesce({paste(vals, collapse = ', ')})"))
  }

  if (fn %in% .TRACE_INFIX_OPS) {
    prec <- .trace_op_precedence(fn)

    if (length(args) == 1L && fn %in% c("+", "-")) {
      inner <- .format_expr_node(args[[1]], value_map, prec, is_rhs = TRUE)
      out <- paste0(fn, inner)
      if (prec < parent_prec || (prec == parent_prec && is_rhs)) {
        out <- paste0("(", out, ")")
      }
      return(out)
    }

    if (length(args) >= 2L) {
      left  <- .format_expr_node(args[[1]], value_map, prec, is_rhs = FALSE)
      right <- .format_expr_node(args[[2]], value_map, prec, is_rhs = TRUE)
      out <- paste0(left, fn, right)
      need_parens <- prec < parent_prec ||
        (prec == parent_prec && is_rhs && !.trace_op_right_assoc(fn))
      if (need_parens) {
        out <- paste0("(", out, ")")
      }
      return(out)
    }
  }

  inner <- vapply(args, .format_expr_node, character(1), value_map = value_map)
  glue::glue("{fn}({paste(inner, collapse = ', ')})")
}

formatFormulaWithValues <- function(formula_raw, value_map) {
  if (is.na(formula_raw) || formula_raw == "") return("")
  expr <- tryCatch(
    rlang::parse_expr(formula_raw),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    return(as.character(formula_raw))
  }
  as.character(.format_expr_node(expr, value_map))
}

# ---------- resolve inputs ----------------------------------------------------

.trace_cell_ref <- function(country_id, indicator_code, frequency, period,
                            trace_db, role = "operand", note = NA_character_,
                            recurse = TRUE, cell_getter = getCellValue) {
  list(
    country_id     = country_id,
    indicator_code = indicator_code,
    frequency      = frequency,
    period         = as.character(period),
    value          = cell_getter(trace_db, country_id, indicator_code, frequency, period),
    role           = role,
    note           = note,
    recurse        = isTRUE(recurse)
  )
}

find_impute_neighbors <- function(trace_db, country_id, indicator_code, frequency, period) {
  df <- get_freq_data(trace_db, frequency)
  if (is.null(df) || !indicator_code %in% names(df)) {
    return(list(left = NULL, right = NULL))
  }

  df <- add_period_columns(df, frequency) |>
    dplyr::filter(.data$country_id == country_id) |>
    dplyr::arrange(dplyr::across(dplyr::any_of(c("year", "quarter", "month", "date", "period"))))

  idx <- match(as.character(period), as.character(df$period))
  if (is.na(idx)) return(list(left = NULL, right = NULL))

  vals <- df[[indicator_code]]
  left <- NULL
  right <- NULL

  if (idx > 1L) {
    left_candidates <- which(!is.na(vals[seq_len(idx - 1L)]))
    if (length(left_candidates) > 0L) {
      j <- max(left_candidates)
      left <- list(period = as.character(df$period[[j]]), value = vals[[j]])
    }
  }
  if (idx < nrow(df)) {
    right_candidates <- which(!is.na(vals[(idx + 1L):nrow(df)]))
    if (length(right_candidates) > 0L) {
      j <- idx + min(right_candidates)
      right <- list(period = as.character(df$period[[j]]), value = vals[[j]])
    }
  }

  list(left = left, right = right)
}

.trace_parse_fromto <- function(formula) {
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
  list(country_from = country_from)
}

.trace_parse_roll <- function(formula) {
  roll_type_raw <- substr(formula, 5, 7)
  comma_pos <- stringr::str_locate(formula, ", ")[1, 2]
  paren_pos <- stringr::str_locate(formula, "\\)")[1, 1]
  windowlen <- as.integer(substr(formula, comma_pos + 1L, paren_pos - 1L))
  paren_open <- stringr::str_locate(formula, "\\(")[1, 1] + 1L
  comma_arg  <- stringr::str_locate(formula, ",")[1, 1]
  var_code <- substr(formula, paren_open, comma_arg - 1L)
  list(type = roll_type_raw, window = windowlen, var_code = var_code)
}

.trace_parse_indexize <- function(formula) {
  m <- stringr::str_match(formula, "indexize\\s*\\(\\s*([A-Za-z0-9_]+)\\s*,\\s*([^\\)]+)\\)")
  if (any(is.na(m))) return(NULL)
  list(var_code = stringr::str_trim(m[, 2]), base_expr = stringr::str_trim(m[, 3]))
}

.trace_parse_usedyn <- function(formula) {
  m <- stringr::str_match(formula, "usedyn\\s*\\((.+)\\)")
  if (any(is.na(m))) return(NULL)
  inner <- m[, 2]
  vars <- stringr::str_split(inner, ",")[[1]] |>
    stringr::str_trim() |>
    purrr::discard(~ grepl("=", .x))
  vars <- vars[grepl("^[A-Za-z_][A-Za-z0-9_]*$", vars)]
  list(vars = vars)
}

.trace_extract_symbols <- function(formula) {
  if (is.na(formula) || formula == "") return(character(0))
  tokens <- stringr::str_split(formula, "[^A-Za-z0-9_]+", simplify = TRUE)
  tokens <- tokens[tokens != ""]
  is_num <- !is.na(suppressWarnings(as.numeric(tokens)))
  keep <- !(tokens %in% TRACE_FORMULA_WORDS | nchar(tokens) <= 2L | is_num)
  unique(tokens[keep])
}

resolveCellInputs <- function(country_id, indicator_code, frequency, period,
                              trace_db, fillplan_row, cell_getter = getCellValue) {
  formula <- fillplan_row$formula[[1]]
  oldcode <- fillplan_row$old_indicator_code[[1]]
  oldfreq <- fillplan_row$old_frequency[[1]]
  newfreq <- fillplan_row$new_frequency[[1]]

  out <- list(
    operation     = "computed",
    formula_raw   = formula,
    time_relation = "same period",
    inputs        = list(),
    note          = NA_character_
  )

  if (identical(formula, "share")) {
    code <- oldcode
    self <- .trace_cell_ref(country_id, code, frequency, period, trace_db, role = "numerator", cell_getter = cell_getter)
    val_1w <- cell_getter(trace_db, "1W", code, frequency, period)
    if (!is.na(val_1w)) {
      denom <- list(
        country_id = "1W", indicator_code = code, frequency = frequency,
        period = period, value = val_1w, role = "denominator",
        note = "World aggregate (1W)", recurse = TRUE
      )
      out$note <- "Denominator: country 1W"
    } else {
      df <- get_freq_data(trace_db, frequency)
      df <- add_period_columns(df, frequency)
      vals <- df |>
        dplyr::filter(.data$period == period, .data$country_id != "1W") |>
        dplyr::pull(dplyr::all_of(code))
      denom_val <- sum(vals, na.rm = TRUE)
      denom <- list(
        country_id = NA_character_, indicator_code = code, frequency = frequency,
        period = period, value = denom_val, role = "denominator",
        note = "Sum over all countries except 1W", recurse = FALSE
      )
      out$note <- "Denominator: sum of all countries (excl. 1W)"
    }
    out$operation <- "share"
    out$time_relation <- "same period; numerator = country value, denominator = world total"
    out$inputs <- list(self, denom)
    return(out)
  }

  if (stringr::str_detect(formula, "fromto")) {
    ft <- .trace_parse_fromto(formula)
    src <- .trace_cell_ref(
      ft$country_from, oldcode, frequency, period, trace_db,
      role = "source country", note = glue::glue("Donor country {ft$country_from}"),
      cell_getter = cell_getter
    )
    out$operation <- "fromto"
    out$time_relation <- glue::glue("same period; value copied from {ft$country_from}")
    out$inputs <- list(src)
    return(out)
  }

  if (oldfreq != newfreq && formula %in% c("last", "first", "mean", "max", "min", "sum")) {
    sub_p <- as.character(subPeriodsOf(period, newfreq, oldfreq))
    # Trace journal: list sub-period values; deep recursion explodes (e.g. 12 x monthly coalesce).
    do_recurse <- length(sub_p) <= 3L
    inputs <- lapply(sub_p, function(p) {
      .trace_cell_ref(
        country_id, oldcode, oldfreq, p, trace_db,
        role = "sub-period", recurse = do_recurse, cell_getter = cell_getter
      )
    })
    out$operation <- glue::glue("aggregate_{formula}")
    out$time_relation <- glue::glue("{formula} over sub-periods of {period} ({oldfreq}->{newfreq})")
    if (!do_recurse) {
      out$note <- glue::glue("Showing {length(sub_p)} sub-periods without further recursion")
    }
    out$inputs <- inputs
    return(out)
  }

  if (oldfreq != newfreq && formula %in% c("desum_fix", "demean_fix")) {
    parent_p <- as.character(parentPeriodOf(period, newfreq, oldfreq))
    src <- .trace_cell_ref(country_id, oldcode, oldfreq, parent_p, trace_db, role = "parent period", cell_getter = cell_getter)
    out$operation <- formula
    out$time_relation <- glue::glue("parent period {parent_p} at {oldfreq}")
    out$note <- if (formula == "desum_fix") "Value divided by number of sub-periods" else "Same value copied to sub-periods"
    out$inputs <- list(src)
    return(out)
  }

  if (stringr::str_detect(formula, "^\\s*roll")) {
    rp <- .trace_parse_roll(formula)
    rps <- rollPeriods(period, frequency, rp$window)
    inputs <- lapply(rps, function(p) {
      .trace_cell_ref(country_id, rp$var_code, frequency, p, trace_db, role = "window", cell_getter = cell_getter)
    })
    out$operation <- glue::glue("roll{rp$type}")
    out$time_relation <- glue::glue("window [{rps[[1]]} .. {period}]")
    out$inputs <- inputs
    return(out)
  }

  if (formula %in% c("impute_fix", "impute_linear")) {
    code <- oldcode
    neighbors <- find_impute_neighbors(trace_db, country_id, code, frequency, period)
    inputs <- list()
    if (!is.null(neighbors$left)) {
      inputs[[length(inputs) + 1L]] <- .trace_cell_ref(
        country_id, code, frequency, neighbors$left$period, trace_db,
        role = "left neighbor", cell_getter = cell_getter
      )
    }
    if (!is.null(neighbors$right)) {
      inputs[[length(inputs) + 1L]] <- .trace_cell_ref(
        country_id, code, frequency, neighbors$right$period, trace_db,
        role = "right neighbor", cell_getter = cell_getter
      )
    }
    out$operation <- formula
    out$time_relation <- "two nearest non-NA neighbors in the same series"
    if (length(inputs) == 0L) {
      out$note <- "Period not found or no non-NA neighbors for imputation"
    }
    out$inputs <- inputs
    return(out)
  }

  if (stringr::str_detect(formula, "indexize")) {
    ix <- .trace_parse_indexize(formula)
    code <- if (!is.na(oldcode) && oldcode != "") oldcode else ix$var_code
    base_date <- .trace_parse_base_period(ix$base_expr)
    base_period <- formatDateAsPeriod(base_date, frequency)
    inputs <- list(
      .trace_cell_ref(country_id, code, frequency, period, trace_db, role = "current", cell_getter = cell_getter),
      .trace_cell_ref(country_id, code, frequency, base_period, trace_db, role = "base", cell_getter = cell_getter)
    )
    out$operation <- "indexize"
    out$time_relation <- glue::glue("current period and base {ix$base_expr}")
    out$inputs <- inputs
    return(out)
  }

  if (stringr::str_detect(formula, "^\\s*usedyn\\s*\\(")) {
    sp <- .trace_parse_usedyn(formula)
    inputs <- lapply(sp$vars, function(v) {
      .trace_cell_ref(country_id, v, frequency, period, trace_db, role = "series", recurse = FALSE, cell_getter = cell_getter)
    })
    out$operation <- "usedyn"
    out$time_relation <- "series-level splice (not cell-wise)"
    out$note <- "usedyn combines entire series; cell inputs are anchor/donor values at this period only"
    out$inputs <- inputs
    return(out)
  }

  if (stringr::str_detect(formula, "^\\s*userat\\s*\\(") ||
      stringr::str_detect(formula, "seas_adj")) {
    syms <- .trace_extract_symbols(formula)
    if (length(syms) == 0L && !is.na(oldcode) && oldcode != "") syms <- oldcode
    inputs <- lapply(syms, function(v) {
      .trace_cell_ref(country_id, v, frequency, period, trace_db, role = "series", recurse = FALSE, cell_getter = cell_getter)
    })
    out$operation <- if (grepl("userat", formula)) "userat" else "seas_adj"
    out$time_relation <- "series-level operation (not cell-wise)"
    out$note <- "Operation applies to the full series; only values at this period are shown"
    out$inputs <- inputs
    return(out)
  }

  if (stringr::str_detect(formula, "coalesce")) {
    expr <- rlang::parse_expr(formula)
    args <- as.list(expr[-1])
    n_args <- length(args)
    winner <- NA_integer_
    inputs <- lapply(seq_along(args), function(i) {
      a <- args[[i]]
      code <- rlang::as_string(a)
      .trace_cell_ref(
        country_id, code, frequency, period, trace_db,
        role = glue::glue("arg{i}"), cell_getter = cell_getter
      )
    })
    for (i in seq_along(inputs)) {
      if (!is.na(inputs[[i]]$value)) {
        winner <- i
        break
      }
    }
    for (i in seq_along(inputs)) {
      inputs[[i]]$recurse <- i <= 6L
    }
    out$operation <- "coalesce"
    out$time_relation <- "same period for all arguments"
    if (n_args <= 6L) {
      out$note <- if (!is.na(winner)) {
        glue::glue("Argument {winner} selected (first non-NA)")
      } else {
        "All arguments NA"
      }
    } else {
      note_parts <- if (!is.na(winner)) {
        glue::glue("Argument {winner} selected (first non-NA)")
      } else {
        "All arguments NA"
      }
      omitted <- glue::glue(
        "Showing first 6 of {n_args} arguments; arguments 7–{n_args} omitted from trace"
      )
      if (!is.na(winner) && winner > 6L) {
        omitted <- paste0(omitted, " (selected argument not expanded)")
      }
      out$note <- paste(note_parts, omitted, sep = ". ")
    }
    out$inputs <- inputs
    return(out)
  }

  expr <- tryCatch(rlang::parse_expr(formula), error = function(e) NULL)
  if (!is.null(expr)) {
    inputs <- list()
    .walk_expr_refs <- function(node) {
      if (is.symbol(node)) {
        code <- rlang::as_string(node)
        if (!(code %in% TRACE_FORMULA_WORDS) && nchar(code) > 2L &&
            is.na(suppressWarnings(as.numeric(code)))) {
          inputs[[code]] <<- .trace_cell_ref(country_id, code, frequency, period, trace_db, cell_getter = cell_getter)
        }
        return(invisible(NULL))
      }
      if (is.call(node)) {
        fn <- rlang::as_string(node[[1]])
        if (fn == "lag" && length(node) >= 2L && is.symbol(node[[2]])) {
          code <- rlang::as_string(node[[2]])
          k <- if (length(node) >= 3L) as.integer(rlang::eval_tidy(node[[3]])) else 1L
          p <- shiftPeriod(period, frequency, -k)
          inputs[[paste0("lag(", code, ",", k, ")")]] <<-
            .trace_cell_ref(country_id, code, frequency, p, trace_db, role = glue::glue("lag {k}"), cell_getter = cell_getter)
          return(invisible(NULL))
        }
        if (fn == "lead" && length(node) >= 2L && is.symbol(node[[2]])) {
          code <- rlang::as_string(node[[2]])
          k <- if (length(node) >= 3L) as.integer(rlang::eval_tidy(node[[3]])) else 1L
          p <- shiftPeriod(period, frequency, k)
          inputs[[paste0("lead(", code, ",", k, ")")]] <<-
            .trace_cell_ref(country_id, code, frequency, p, trace_db, role = glue::glue("lead {k}"), cell_getter = cell_getter)
          return(invisible(NULL))
        }
        if (fn %in% c("cumsum", "cummax", "cummin") && length(node) >= 2L && is.symbol(node[[2]])) {
          code <- rlang::as_string(node[[2]])
          seq_p <- periodsUpTo(trace_db, country_id, code, frequency, period)
          max_cum <- 24L
          if (length(seq_p) > max_cum) {
            seq_p <- seq_p[(length(seq_p) - max_cum + 1L):length(seq_p)]
            out$note <<- glue::glue("Showing last {max_cum} periods of {fn}")
          }
          out$operation <<- fn
          out$time_relation <<- glue::glue("{fn} over [{seq_p[[1]]} .. {period}]")
          for (p in seq_p) {
            key <- paste0(code, "@", p)
            inputs[[key]] <<- .trace_cell_ref(
              country_id, code, frequency, p, trace_db,
              role = fn, recurse = FALSE, cell_getter = cell_getter
            )
          }
          return(invisible(NULL))
        }
        for (i in seq_along(node)[-1]) .walk_expr_refs(node[[i]])
      }
    }
    .walk_expr_refs(expr)
    if (length(inputs) > 0L) {
      if (out$operation == "computed") out$operation <- "arithmetic"
      out$inputs <- unname(inputs)
      return(out)
    }
  }

  if (!is.na(oldcode) && oldcode != "") {
    out$inputs <- list(.trace_cell_ref(country_id, oldcode, frequency, period, trace_db, cell_getter = cell_getter))
    out$operation <- formula
  }

  out
}

# ---------- main trace builder ------------------------------------------------

buildValueTrace <- function(country_id, indicator_code, frequency, period,
                            trace_db, fillplan, impplan, saveplan_full = NULL,
                            max_depth = 8L, max_nodes = 300L) {
  if (is.null(saveplan_full)) {
    saveplan_full <- build_trace_saveplan(impplan, fillplan)
  }

  freq_df_cache <- list()
  cell_val_cache <- new.env(parent = emptyenv())
  .cell_getter <- function(trace_db, country_id, indicator_code, frequency, period) {
    cache_key <- paste(country_id, indicator_code, frequency, period, sep = "|")
    if (exists(cache_key, cell_val_cache, inherits = FALSE)) {
      return(get(cache_key, cell_val_cache, inherits = FALSE))
    }
    if (is.null(freq_df_cache[[frequency]])) {
      df <- get_freq_data(trace_db, frequency)
      if (!is.null(df)) df <- add_period_columns(df, frequency)
      freq_df_cache[[frequency]] <- df
    }
    df <- freq_df_cache[[frequency]]
    val <- if (is.null(df) || !indicator_code %in% names(df)) {
      NA_real_
    } else {
      cid <- as.character(country_id)
      per <- as.character(period)
      row <- df |>
        dplyr::filter(.data$country_id == .env$cid, as.character(.data$period) == .env$per)
      if (nrow(row) == 0L) {
        NA_real_
      } else {
        v <- row[[indicator_code]][[1]]
        if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
      }
    }
    assign(cache_key, val, cell_val_cache)
    val
  }

  nodes <- list()
  visited <- character(0)
  truncated <- FALSE

  .append_node <- function(row) {
    nodes[[length(nodes) + 1L]] <<- row
  }

  .walk <- function(cid, code, freq, per, depth, parent_step_id, child_index) {
    if (length(nodes) >= max_nodes) {
      truncated <<- TRUE
      return(invisible(NULL))
    }
    if (depth > max_depth) {
      truncated <<- TRUE
      .append_node(tibble::tibble(
        step_id = if (is.null(parent_step_id)) "1" else glue::glue("{parent_step_id}.{child_index}"),
        level = depth, parent_id = parent_step_id,
        country_id = cid, indicator_code = code, frequency = freq, period = per,
        value = NA_real_, node_type = "truncated", operation = NA_character_,
        formula_raw = NA_character_, formula_filled = NA_character_,
        time_relation = NA_character_, source_name = NA_character_,
        note = glue::glue("Trace stopped at depth {max_depth}")
      ))
      return(invisible(NULL))
    }

    visit_key <- paste(cid, code, freq, per, sep = "|")
    if (visit_key %in% visited) {
      step_id <- if (is.null(parent_step_id)) "1" else glue::glue("{parent_step_id}.{child_index}")
      .append_node(tibble::tibble(
        step_id = step_id, level = depth, parent_id = parent_step_id,
        country_id = cid, indicator_code = code, frequency = freq, period = per,
        value = .cell_getter(trace_db, cid, code, freq, per),
        node_type = "truncated", operation = "cycle", formula_raw = NA_character_,
        formula_filled = NA_character_, time_relation = NA_character_,
        source_name = NA_character_, note = "Cycle detected"
      ))
      return(invisible(NULL))
    }
    visited <<- c(visited, visit_key)

    meta <- lookup_indicator_kind(code, freq, fillplan, impplan)
    value <- .cell_getter(trace_db, cid, code, freq, per)

    step_id <- if (is.null(parent_step_id)) {
      "1"
    } else if (is.null(child_index)) {
      parent_step_id
    } else {
      glue::glue("{parent_step_id}.{child_index}")
    }

    if (meta$kind == "missing") {
      .append_node(tibble::tibble(
        step_id = step_id, level = depth, parent_id = parent_step_id,
        country_id = cid, indicator_code = code, frequency = freq, period = per,
        value = value, node_type = "missing", operation = NA_character_,
        formula_raw = NA_character_, formula_filled = NA_character_,
        time_relation = NA_character_, source_name = NA_character_,
        note = "Indicator not found in impplan/fillplan"
      ))
      return(invisible(NULL))
    }

    if (meta$kind == "imported") {
      .append_node(tibble::tibble(
        step_id = step_id, level = depth, parent_id = parent_step_id,
        country_id = cid, indicator_code = code, frequency = freq, period = per,
        value = value, node_type = "imported", operation = "import",
        formula_raw = NA_character_, formula_filled = NA_character_,
        time_relation = "source data", source_name = import_source_label(meta$impplan_row),
        note = NA_character_
      ))
      return(invisible(NULL))
    }

    fp <- meta$fillplan_row
    resolved <- resolveCellInputs(cid, code, freq, per, trace_db, fp, cell_getter = .cell_getter)
    value_map <- stats::setNames(
      vapply(resolved$inputs, function(x) .format_trace_value(x$value), character(1)),
      vapply(resolved$inputs, function(x) x$indicator_code, character(1))
    )
    formula_filled <- formatFormulaWithValues(resolved$formula_raw, value_map)

    node_type <- if (resolved$operation %in% c("usedyn", "userat", "seas_adj")) {
      "opaque"
    } else if (is.na(value)) {
      "no_value"
    } else {
      "computed"
    }

    .append_node(tibble::tibble(
      step_id = step_id, level = depth, parent_id = parent_step_id,
      country_id = cid, indicator_code = code, frequency = freq, period = per,
      value = value, node_type = node_type, operation = resolved$operation,
      formula_raw = resolved$formula_raw, formula_filled = formula_filled,
      time_relation = resolved$time_relation, source_name = NA_character_,
      note = resolved$note
    ))

    rec_inputs <- purrr::keep(resolved$inputs, ~ isTRUE(.x$recurse) && !is.na(.x$indicator_code))
    child_idx <- 1L
    for (inp in rec_inputs) {
      .walk(
        inp$country_id, inp$indicator_code, inp$frequency, inp$period,
        depth + 1L, step_id, child_idx
      )
      child_idx <- child_idx + 1L
    }

    # Limited aggregate: snapshot sub-period operands without structural recursion.
    if (grepl("^aggregate_", resolved$operation %||% "")) {
      snap_inputs <- purrr::keep(
        resolved$inputs,
        function(x) {
          !isTRUE(x$recurse) &&
            identical(x$role, "sub-period") &&
            !is.na(x$indicator_code) &&
            nzchar(as.character(x$indicator_code))
        }
      )
      if (length(snap_inputs) > 0L) {
        parent_step <- as.character(step_id)
        snap_idx <- 1L
        for (inp in snap_inputs) {
          if (length(nodes) >= max_nodes) {
            truncated <<- TRUE
            break
          }
          .append_node(tibble::tibble(
            step_id = glue::glue("{parent_step}.{snap_idx}"),
            level = depth + 1L,
            parent_id = parent_step,
            country_id = inp$country_id,
            indicator_code = inp$indicator_code,
            frequency = inp$frequency,
            period = inp$period,
            value = inp$value,
            node_type = "aggregate_input",
            operation = NA_character_,
            formula_raw = NA_character_,
            formula_filled = NA_character_,
            time_relation = NA_character_,
            source_name = NA_character_,
            note = NA_character_
          ))
          snap_idx <- snap_idx + 1L
        }
      }
    }

    invisible(NULL)
  }

  .walk(country_id, indicator_code, frequency, period, 0L, NULL, NULL)

  if (length(nodes) == 0L) {
    return(tibble::tibble(
      step_id = character(), level = integer(), parent_id = character(),
      country_id = character(), indicator_code = character(), frequency = character(),
      period = character(), value = numeric(), node_type = character(),
      operation = character(), formula_raw = character(), formula_filled = character(),
      time_relation = character(), source_name = character(), note = character()
    ))
  }

  journal <- dplyr::bind_rows(nodes)
  if (truncated && !any(journal$node_type == "truncated")) {
    journal <- dplyr::bind_rows(
      journal,
      tibble::tibble(
        step_id = glue::glue("{journal$step_id[[1]]}.T"),
        level = max(journal$level, na.rm = TRUE) + 1L,
        parent_id = journal$step_id[[1]],
        country_id = country_id, indicator_code = indicator_code,
        frequency = frequency, period = period, value = NA_real_,
        node_type = "truncated", operation = NA_character_,
        formula_raw = NA_character_, formula_filled = NA_character_,
        time_relation = NA_character_, source_name = NA_character_,
        note = glue::glue("Trace stopped at {max_nodes} nodes")
      )
    )
  }

  journal
}

filter_trace_journal <- function(journal, collapsed_prefixes = character(0),
                                 show_technical = TRUE, saveplan_full = NULL) {
  if (nrow(journal) == 0L) return(journal)
  out <- journal

  if (length(collapsed_prefixes) > 0L) {
    keep <- vapply(out$step_id, function(sid) {
      for (prefix in collapsed_prefixes) {
        if (sid != prefix && startsWith(sid, paste0(prefix, "."))) {
          return(FALSE)
        }
      }
      TRUE
    }, logical(1))
    out <- out[keep, , drop = FALSE]
  }

  if (!isTRUE(show_technical) && !is.null(saveplan_full) && nrow(saveplan_full) > 0L) {
    out <- out |>
      dplyr::left_join(
        saveplan_full |>
          dplyr::select(.data$indicator_code, .data$source_frequency, .data$keep),
        by = c("indicator_code" = "indicator_code", "frequency" = "source_frequency")
      ) |>
      dplyr::filter(is.na(.data$keep) | .data$keep == 1L) |>
      dplyr::select(-.data$keep)
  }

  out
}

trace_has_children <- function(journal, step_id) {
  child_mask <- startsWith(journal$step_id, paste0(step_id, ".")) &
    journal$step_id != step_id
  if ("node_type" %in% names(journal)) {
    child_mask <- child_mask & journal$node_type != "aggregate_input"
  }
  any(child_mask)
}

#' Step ids to collapse so only the root and its direct children are visible.
default_trace_collapsed_prefixes <- function(journal) {
  if (is.null(journal) || nrow(journal) == 0L) return(character(0))
  level1 <- unique(journal$step_id[journal$level == 1L])
  if (length(level1) == 0L) return(character(0))
  level1[vapply(level1, function(sid) trace_has_children(journal, sid), logical(1))]
}

prepare_trace_display <- function(journal, collapsed_prefixes = character(0)) {
  if (nrow(journal) == 0L) return(journal)

  journal |>
    dplyr::mutate(
      indent = paste0(strrep("\u00a0", pmax(.data$level, 0L) * 4L), .data$step_id),
      calculation = {
        val_txt <- vapply(.data$value, .format_trace_value, character(1))
        filled <- as.character(.data$formula_filled)
        has_filled <- !is.na(filled) & nzchar(filled)
        ifelse(has_filled, paste0(val_txt, "=", filled), val_txt)
      },
      toggle = vapply(.data$step_id, function(sid) {
        # Collapsed parents stay in the filtered journal while children are
        # removed, so detect collapse via prefixes rather than remaining rows.
        if (sid %in% collapsed_prefixes) {
          sprintf("<span class='trace-toggle' data-step='%s' style='cursor:pointer;'>+</span>", sid)
        } else if (trace_has_children(journal, sid)) {
          sprintf("<span class='trace-toggle' data-step='%s' style='cursor:pointer;'>−</span>", sid)
        } else {
          ""
        }
      }, character(1))
    )
}
