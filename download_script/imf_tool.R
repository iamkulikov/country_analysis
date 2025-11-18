#' IMF helper: build -> fetch -> trim by year
#'
#' @param database Character scalar, e.g. "IMF.STA/CPI".
#' @param code     Character scalar, e.g. "CPI._T.IX" (with or without .M/.Q/.A).
#' @param freq     One of "m","q","y" (case-insensitive).
#' @param start    Optional integer year (inclusive).
#' @param end      Optional integer year (inclusive).
#' @param normalize_units One of c("none","label","scale"); passed to imf_fetch_simple.
#'
#' @return tibble with columns from imf_fetch_simple (e.g. iso2, year, maybe quarter/month, value)

imfTool <- function(database, code, freq,
                    start = NULL, end = NULL,
                    normalize_units = c("none", "label", "scale")) {
  # deps
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  
  normalize_units <- rlang::arg_match(normalize_units)
  
  # build URL and fetch data
  url <- imf_build_url(database = database, code = code, freq = freq)
  df  <- imf_fetch_simple(url, normalize_units = normalize_units, include_meta = "none")
  
  # validate year limits
  to_year <- function(x, name) {
    if (is.null(x)) return(NULL)
    y <- suppressWarnings(as.integer(x))
    if (length(y) != 1L || is.na(y)) rlang::abort(paste0("`", name, "` must be a single integer year."))
    y
  }
  start <- to_year(start, "start")
  end   <- to_year(end,   "end")
  if (!is.null(start) && !is.null(end) && start > end) {
    rlang::abort("`start` cannot be greater than `end`.")
  }
  
  # trim by year (inclusive)
  if (!is.null(start)) df <- dplyr::filter(df, .data$year >= start)
  if (!is.null(end))   df <- dplyr::filter(df, .data$year <= end)
  
  has_agg <- grepl("\\(aggregate\\)", trimws(database), ignore.case = TRUE, perl = TRUE)
  if (has_agg) df <- df |> mutate(iso2 = "1W")
  df
}

# imfTool("IMF.STA/QNEA", "B1GQ.V.NSA.XDC", "q", start = 2020)
# imfTool("IMF.STA:GFS_SOO ", "S13.G1.G1_T.POGDP_PT", "y", end = 2023)
# imfTool("IMF.RES/WEO (aggregate)", "G110.NGDP_RPCH", "y", end = 2030)
# imfTool("IMF.RES/WEO", "NGDP", "y", end = 2030)
# imfTool("IMF.RES/CPI", "CPI._T.IX", "m", start = 2010, end = 2012)

#' Fetch IMF SDMX data (URL already constructed) via rsdmx transport
#'
#' @param request_url character(1). Fully-constructed IMF SDMX 3.0 data URL.
#' @param normalize_units one of c("none","label","scale"):
#'   - "none"  : leave values as-is
#'   - "label" : attach attr(unit_label) inferred from UNIT (and FSI_STO when present)
#'   - "scale" : also rescale values to base units when possible (e.g. T1K -> *1e3)
#' @param include_meta one of c("none","attrs","tibble"):
#'   - "none"  : return only the data tibble
#'   - "attrs" : attach attr(meta) with just prepared + dsd_id
#'   - "tibble": return list(data=..., meta=...) as a tibble-like list
#'
#' @return tibble with columns: iso2, year, (quarter?), (month?), value

imf_fetch_simple <- function(request_url,
                             normalize_units = c("none", "label", "scale"),
                             include_meta    = c("none", "attrs", "tibble")) {
  
  stopifnot(is.character(request_url), length(request_url) == 1L, nzchar(request_url))
  normalize_units <- rlang::arg_match(normalize_units)
  include_meta    <- rlang::arg_match(include_meta)
  
  # deps ----
  requireNamespace("rsdmx",       quietly = TRUE)
  requireNamespace("XML",         quietly = TRUE)
  requireNamespace("xml2",        quietly = TRUE)
  requireNamespace("dplyr",       quietly = TRUE)
  requireNamespace("tidyr",       quietly = TRUE)
  requireNamespace("stringr",     quietly = TRUE)
  requireNamespace("tibble",      quietly = TRUE)
  requireNamespace("purrr",       quietly = TRUE)
  requireNamespace("readr",       quietly = TRUE)
  requireNamespace("countrycode", quietly = TRUE)
  requireNamespace("rlang",       quietly = TRUE)
  
  # fetch via rsdmx ----
  sdmx <- rsdmx::readSDMX(request_url)
  xml  <- xml2::read_xml(XML::saveXML(sdmx@xmlObj))
  ns   <- xml2::xml_ns(xml)
  
  # minimal meta we keep
  prepared <- xml2::xml_text(xml2::xml_find_first(xml, ".//message:Prepared", ns = ns))
  dsd_id   <- xml2::xml_attr(xml2::xml_find_first(xml, ".//message:Structure", ns = ns), "structureID")
  
  # optional bits for unit/scale (not stored in meta)
  unit_val <- xml2::xml_text(xml2::xml_find_first(xml, ".//Group/Comp[@id='UNIT']/Value"))
  fsi_sto  <- xml2::xml_text(xml2::xml_find_first(xml, ".//Group/Comp[@id='FSI_STO']/Value"))
  
  # series table ----
  series_nodes <- xml2::xml_find_all(xml, ".//Series")
  if (length(series_nodes) == 0L) {
    rlang::abort("No <Series> nodes found in the SDMX response. Check the request URL or permissions.")
  }
  
  series_tbl <- purrr::map(series_nodes, \(s) {
    tibble::tibble(
      series_id = xml2::xml_path(s),
      country   = xml2::xml_attr(s, "COUNTRY"),
      freq      = xml2::xml_attr(s, "FREQUENCY")
    )
  }) |> purrr::list_rbind()
  
  # observations ----
  obs_tbl <- purrr::map(series_nodes, \(s) {
    sid <- xml2::xml_path(s)
    os  <- xml2::xml_find_all(s, ".//Obs")
    if (length(os) == 0L) return(NULL)
    tibble::tibble(
      series_id   = sid,
      time_period = xml2::xml_attr(os, "TIME_PERIOD"),
      value       = suppressWarnings(as.numeric(xml2::xml_attr(os, "OBS_VALUE")))
    )
  }) |> purrr::list_rbind()
  
  if (is.null(obs_tbl) || nrow(obs_tbl) == 0L) {
    rlang::abort("No <Obs> observations found in the SDMX response.")
  }
  
  # join + parse TIME_PERIOD (YYYY | YYYY-Qk | YYYY-Mm | YYYY-mm)
  raw_df <- obs_tbl |> dplyr::inner_join(series_tbl, by = dplyr::join_by(series_id))
  
  parsed <-
    raw_df |>
    tidyr::extract(
      time_period,
      into   = c("year_chr", "quarter_chr", "month_chr"),
      regex  = "^(\\d{4})(?:-(?:Q([1-4])|M?(\\d{1,2})))?$",
      remove = FALSE
    ) |>
    dplyr::mutate(
      year    = readr::parse_integer(.data$year_chr),
      month   = readr::parse_integer(.data$month_chr),
      quarter = dplyr::case_when(
        !is.na(.data$quarter_chr) ~ readr::parse_integer(.data$quarter_chr),
        !is.na(.data$month)       ~ ((.data$month - 1L) %/% 3L) + 1L,
        TRUE                      ~ NA_integer_
      )
    ) |>
    dplyr::mutate(
      quarter = dplyr::coalesce(
        .data$quarter,
        dplyr::if_else(!is.na(.data$month),
                       ((.data$month - 1L) %/% 3L) + 1L,
                       NA_integer_)
      )
    )
  
  # ISO3 -> ISO2 (fallback to original when unmapped)
  iso_lut <-
    parsed |>
    dplyr::distinct(country) |>
    dplyr::mutate(
      iso2 = countrycode::countrycode(
        .data$country, origin = "iso3c", destination = "iso2c", warn = FALSE,
        custom_match = c(WBG = "PS", KOS = "XK", ANT = "AN", G001 = "1W")
      ),
      iso2 = dplyr::coalesce(.data$iso2, .data$country)
    )
  
  out <-
    parsed |>
    dplyr::left_join(iso_lut, by = dplyr::join_by(country)) |>
    dplyr::transmute(
      iso2, year, quarter, month, value, freq
    )
  
  # trim time columns by frequency (A: drop quarter/month; Q: drop month; M: keep all)
  freqs <- unique(stats::na.omit(out$freq))
  if (length(freqs) == 1L) {
    if (freqs == "A") {
      out <- dplyr::select(out, iso2, year, value)
    } else if (freqs == "Q") {
      out <- dplyr::select(out, iso2, year, quarter, value)
    } else if (freqs == "M") {
      out <- dplyr::select(out, iso2, year, quarter, month, value)
    }
  } else {
    # mixed frequencies: keep all time columns so nothing is lost
    out <- dplyr::select(out, iso2, year, quarter, month, value)
  }
  out <- dplyr::arrange(out, iso2, year,
                        dplyr::across(dplyr::any_of(c("quarter","month"))))
  
  # optional units normalization/label (not stored in meta)
  if (normalize_units != "none") {
    unit_label <- unit_val %||% NA_character_
    if (!is.na(fsi_sto) && fsi_sto == "T1K") {
      if (normalize_units == "label") {
        unit_label <- stringr::str_c("thousand ", unit_label %||% "")
      } else if (normalize_units == "scale") {
        out <- dplyr::mutate(out, value = .data$value * 1e3)
      }
    }
    attr(out, "unit_label") <- unit_label
  }
  
  # meta (only prepared + dsd_id)
  if (include_meta == "attrs") {
    attr(out, "meta") <- tibble::tibble(prepared = prepared, dsd_id = dsd_id)
  } else if (include_meta == "tibble") {
    out <- tibble::lst(
      data = out,
      meta = tibble::tibble(prepared = prepared, dsd_id = dsd_id)
    )
  }
  
  out
}

# “nullish-coalescing” infix operator: return x if it looks usable, otherwise return y
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (is.atomic(x) && all(is.na(x)))) y else x

# imf_fetch_simple("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.FAD/FM/%2B/BRA.G1_S13_POGDP_PT.A")
# imf_fetch_simple("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.STA/FSIC/%2B/USA.S12CFSI.T1KTA_T1K_USD.Q")
# imf_fetch_simple("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.STA/IL/%2B/BRA.RGOLDNV_REVS.USD.M", include_meta = "attrs")


#' Build IMF SDMX 3.0 data URL
#' @param database Character, e.g. "IMF.STA/CPI" or "IMF.STA:GFS_SOO"
#' @param code     Character SDMX key after "*.", e.g. "CPI._T.IX.M" or "B1GQ.V.NSA.XDC"
#' @param freq     One of "m","q","y" (case-insensitive) or "monthly","quarterly","annual"
#' @return A length-1 character URL

imf_build_url <- function(database, code, freq) {
  
  stopifnot(is.character(database), length(database) == 1L, nzchar(database))
  stopifnot(is.character(code),     length(code)     == 1L, nzchar(code))
  stopifnot(is.character(freq),     length(freq)     == 1L, nzchar(freq))
  
  # map freq -> IMF suffix A|Q|M
  f <- tolower(freq)
  f <- if (f %in% c("m","monthly")) "M"
  else if (f %in% c("q","quarterly")) "Q"
  else if (f %in% c("y","a","annual","yearly")) "A"
  else if (toupper(freq) %in% c("A","Q","M")) toupper(freq)
  else stop("freq must be one of: m/q/y (or monthly/quarterly/annual).")
  
  # normalize database: trim, ':' -> '/', collapse slashes, trim '/', drop '(aggregate)'
  db <- trimws(database)
  has_agg <- grepl("\\(aggregate\\)", db, ignore.case = TRUE, perl = TRUE)
  db <- gsub(" \\(aggregate\\)", "", db, ignore.case = TRUE, perl = TRUE)
  db <- gsub(":", "/", db)
  db <- gsub("/{2,}", "/", db)
  db <- sub("^/+","", db)
  db <- sub("/+$","", db)
  
  # normalize code: trim spaces, drop leading dots
  raw_cd <- gsub("\\s+", "", trimws(code))
  raw_cd <- sub("^\\.+", "", raw_cd)
  
  # split code on first dot
  dot_pos <- regexpr("\\.", raw_cd, perl = TRUE)[1]
  if (dot_pos > 0) {
    lhs <- substr(raw_cd, 1L, dot_pos - 1L)      # часть до первой точки
    rhs <- substr(raw_cd, dot_pos + 1L, nchar(raw_cd))  # часть после первой точки
  } else {
    lhs <- raw_cd
    rhs <- ""
  }
  
  # choose which part becomes the "code" after the placeholder
  # - if aggregate: use RHS (must exist), and replace '*.' with 'lhs.'
  # - else: use the full raw code, and keep '*.' as is
  if (has_agg) {
    if (!nzchar(rhs)) {
      stop("When database contains '(aggregate)', `code` must contain at least one '.' to provide a right-hand part.")
    }
    cd_base <- rhs
    placeholder <- paste0(lhs, ".")
  } else {
    cd_base <- raw_cd
    placeholder <- "*."
  }
  
  # enforce trailing .A/.Q/.M based on freq
  if (grepl("\\.(A|Q|M)$", cd_base, perl = TRUE)) {
    cd <- sub("\\.(A|Q|M)$", paste0(".", f), cd_base, perl = TRUE)
  } else {
    cd <- paste0(cd_base, ".", f)
  }
  
  paste0("https://api.imf.org/external/sdmx/3.0/data/dataflow/", db, "/%2B/", placeholder, cd)
}
 
# imf_build_url("IMF.STA/CPI", "CPI._T.IX.M", "m")
# imf_build_url("IMF.RES/WEO (aggregate)", "G001.NGDP_RPCH", "y")
# result: https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.RES/WEO/%2B/G001.NGDP_RPCH.A
