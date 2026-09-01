#' Tax Foundation Worldwide Corporate Tax Rates helpers for tryImport
#'
#' Source: https://github.com/TaxFoundation/worldwide-corporate-tax-rates
#' Canonical table: `final_data/final_data_long.csv` (1980–2023, long format).
#'
#' `retrieve_code` is a column in that table: `rate` (statutory CIT, percent)
#' or `gdp` (real GDP used by Tax Foundation for weighting).

##### Constants ----------------------------------------------------------------

.taxfoundation_user_agent <- "country_analysis/taxfoundation-import"

.taxfoundation_default_url <- paste0(
  "https://raw.githubusercontent.com/TaxFoundation/",
  "worldwide-corporate-tax-rates/master/final_data/final_data_long.csv"
)

.taxfoundation_series <- c("rate", "gdp")

.taxfoundation_iso_custom_match <- c(
  ROM = "RO", ADO = "AD", ANT = "AN", KSV = "XK",
  TMP = "TL", WBG = "PS", ZAR = "CD"
)

##### Parse / IO ---------------------------------------------------------------

#' Allowed `retrieve_code` values.
taxfoundation_allowed_series <- function() {
  .taxfoundation_series
}

#' Validate Tax Foundation retrieve_code.
#'
#' @param retrieve_code Character scalar (`rate` or `gdp`).
#' @return Trimmed series name.
taxfoundation_parse_code <- function(retrieve_code) {
  stopifnot(length(retrieve_code) == 1L)
  code <- trimws(as.character(retrieve_code))
  if (!nzchar(code) || !code %in% .taxfoundation_series) {
    rlang::abort(paste0(
      "Tax Foundation retrieve_code must be one of: ",
      paste(.taxfoundation_series, collapse = ", "),
      ". Got: ", retrieve_code
    ))
  }
  code
}

#' Default GitHub raw URL for `final_data_long.csv`.
taxfoundation_default_url <- function() {
  .taxfoundation_default_url
}

#' Read the long-format Tax Foundation table from a file or URL.
#'
#' Namibia's ISO2 is the token `NA` and is lost as missing; recover via `iso_3`.
#'
#' @param path Optional local CSV path. Used when the file exists.
#' @param url Remote CSV URL; used when `path` is missing or not on disk.
#' @return Tibble with `iso_2`, `iso_3`, `year`, `rate`, `gdp`.
taxfoundation_read_long <- function(path = NULL,
                                    url = .taxfoundation_default_url) {
  src <- NULL
  if (!is.null(path) && length(path) == 1L && nzchar(path) && file.exists(path)) {
    src <- path
  } else {
    resp <- httr::GET(url, httr::user_agent(.taxfoundation_user_agent))
    httr::stop_for_status(resp)
    src <- rawConnection(httr::content(resp, as = "raw"))
    on.exit(close(src), add = TRUE)
  }

  raw <- readr::read_csv(
    src,
    show_col_types = FALSE,
    na = c("", "NA", "N/A", "n/a"),
    col_types = readr::cols(
      iso_2 = readr::col_character(),
      iso_3 = readr::col_character(),
      continent = readr::col_character(),
      country = readr::col_character(),
      year = readr::col_double(),
      rate = readr::col_double(),
      gdp = readr::col_double(),
      .default = readr::col_guess()
    )
  )

  needed <- c("iso_2", "iso_3", "year")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "Tax Foundation CSV missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  raw
}

#' Map source country codes to project `country_id` (ISO2).
#'
#' @param iso_2 Character ISO2 (may be missing for Namibia).
#' @param iso_3 Character ISO3 fallback.
#' @return Character ISO2, `NA` if neither maps.
taxfoundation_iso2 <- function(iso_2, iso_3) {
  iso2_raw <- trimws(as.character(iso_2))
  iso2_raw[!nzchar(iso2_raw) | iso2_raw %in% c("N/A", "n/a")] <- NA_character_
  # Unquoted NA in the CSV is Namibia; restore from ISO3.
  iso3_raw <- trimws(as.character(iso_3))
  from_iso3 <- countrycode::countrycode(
    iso3_raw,
    origin = "iso3c",
    destination = "iso2c",
    custom_match = .taxfoundation_iso_custom_match,
    warn = FALSE
  )
  dplyr::coalesce(iso2_raw, from_iso3)
}

#' Build one country-year series from the long table.
#'
#' @param long Output of `taxfoundation_read_long()`.
#' @param retrieve_code `rate` or `gdp`.
#' @param start Optional first year (inclusive).
#' @param end Optional last year (inclusive).
#' @return Tibble: `country_id`, `year`, `value`. Unique keys.
taxfoundation_to_series <- function(long,
                                    retrieve_code,
                                    start = NULL,
                                    end = NULL) {
  col <- taxfoundation_parse_code(retrieve_code)
  if (!col %in% names(long)) {
    rlang::abort(paste0("Tax Foundation CSV has no column '", col, "'"))
  }

  out <- tibble::tibble(
    country_id = taxfoundation_iso2(long$iso_2, long$iso_3),
    year = as.integer(long$year),
    value = as.numeric(long[[col]])
  ) |>
    dplyr::filter(!is.na(.data$country_id), !is.na(.data$year), !is.na(.data$value))

  if (!is.null(start)) {
    out <- dplyr::filter(out, .data$year >= as.integer(start))
  }
  if (!is.null(end)) {
    out <- dplyr::filter(out, .data$year <= as.integer(end))
  }

  dups <- out |>
    dplyr::summarise(n = dplyr::n(), .by = c("country_id", "year")) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dups) > 0L) {
    rlang::abort(paste0(
      "Tax Foundation: duplicate (country_id, year) for '", col, "'. Count: ",
      nrow(dups)
    ))
  }

  out
}

#' Fetch and reshape one Tax Foundation series.
#'
#' @param retrieve_code `rate` or `gdp`.
#' @param start,end Optional year bounds.
#' @param path Optional local CSV; otherwise download from GitHub.
#' @param url Remote URL when `path` is unused.
#' @return Tibble: `country_id`, `year`, `value`.
taxfoundationTool <- function(retrieve_code,
                              start = NULL,
                              end = NULL,
                              path = NULL,
                              url = .taxfoundation_default_url) {
  long <- taxfoundation_read_long(path = path, url = url)
  taxfoundation_to_series(
    long,
    retrieve_code = retrieve_code,
    start = start,
    end = end
  )
}
