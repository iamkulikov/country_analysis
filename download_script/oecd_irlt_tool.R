#' OECD Financial Markets helpers for tryImport
#'
#' Source: OECD STES dataflow `OECD.SDD.STES,DSD_STES@DF_FINMARK`.
#' `retrieve_code` selects the measure slice:
#'   - `IRLT` — long-term interest rates (typically 10-year government bonds)

##### Constants ----------------------------------------------------------------

.oecd_irlt_user_agent <- "country_analysis/oecd-irlt-import"

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) source(iso_path, local = FALSE)
}

.oecd_irlt_default_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.STES,",
  "DSD_STES@DF_FINMARK/.M.IRLT.PA.....?format=csvfilewithlabels"
)

.oecd_irlt_series <- c("IRLT")

.oecd_irlt_filters <- list(
  IRLT = list(
    measure = "IRLT",
    freq = "M",
    unit_measure = "PA",
    methodology = "N"
  )
)

##### Parse / IO ---------------------------------------------------------------

#' Allowed OECD IRLT retrieve_code values.
oecd_irlt_allowed_series <- function() {
  .oecd_irlt_series
}

#' Validate OECD IRLT retrieve_code.
oecd_irlt_parse_code <- function(retrieve_code) {
  stopifnot(length(retrieve_code) == 1L)
  code <- toupper(trimws(as.character(retrieve_code)))
  if (!nzchar(code) || !code %in% .oecd_irlt_series) {
    rlang::abort(paste0(
      "OECD IRLT retrieve_code must be one of: ",
      paste(.oecd_irlt_series, collapse = ", "),
      ". Got: ", retrieve_code
    ))
  }
  code
}

oecd_irlt_default_url <- function() {
  .oecd_irlt_default_url
}

#' Parse OECD TIME_PERIOD values like "1953-04" or Date.
oecd_irlt_parse_period <- function(time_period) {
  tp <- as.character(time_period)
  year <- suppressWarnings(as.integer(substr(tp, 1L, 4L)))
  month <- suppressWarnings(as.integer(substr(tp, 6L, 7L)))
  list(year = year, month = month)
}

#' Read OECD FINMARK IRLT CSV (local path or SDMX URL).
oecd_irlt_read_wide <- function(path = NULL,
                                url = .oecd_irlt_default_url) {
  if (!is.null(path) && length(path) == 1L && nzchar(path) && file.exists(path)) {
    raw <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    resp <- httr::GET(url, httr::user_agent(.oecd_irlt_user_agent), httr::timeout(180L))
    httr::stop_for_status(resp)
    raw <- readr::read_csv(
      rawConnection(httr::content(resp, as = "raw")),
      show_col_types = FALSE
    )
  }

  needed <- c(
    "REF_AREA", "FREQ", "MEASURE", "UNIT_MEASURE",
    "METHODOLOGY", "TIME_PERIOD", "OBS_VALUE"
  )
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "OECD IRLT CSV missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  period <- oecd_irlt_parse_period(raw$TIME_PERIOD)
  raw |>
    dplyr::transmute(
      iso3 = as.character(.data$REF_AREA),
      freq = as.character(.data$FREQ),
      measure = as.character(.data$MEASURE),
      unit_measure = as.character(.data$UNIT_MEASURE),
      methodology = as.character(.data$METHODOLOGY),
      year = period$year,
      month = period$month,
      value = suppressWarnings(as.numeric(.data$OBS_VALUE))
    ) |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$year), !is.na(.data$month))
}

#' Map ISO3 to project `country_id`.
oecd_irlt_iso2 <- function(iso3) {
  project_countrycode_iso3_to_iso2(trimws(as.character(iso3)))
}

#' Build one country-month IRLT series from OECD FINMARK wide table.
oecd_irlt_to_series <- function(wide,
                                retrieve_code,
                                start = NULL,
                                end = NULL) {
  code <- oecd_irlt_parse_code(retrieve_code)
  flt <- .oecd_irlt_filters[[code]]

  out <- wide |>
    dplyr::filter(
      .data$measure == flt$measure,
      .data$freq == flt$freq,
      .data$unit_measure == flt$unit_measure,
      .data$methodology == flt$methodology
    ) |>
    dplyr::transmute(
      country_id = oecd_irlt_iso2(.data$iso3),
      year = .data$year,
      month = .data$month,
      value = .data$value
    ) |>
    dplyr::filter(
      !is.na(.data$country_id),
      !is.na(.data$value),
      .data$month >= 1L,
      .data$month <= 12L
    )

  if (!is.null(start)) {
    out <- dplyr::filter(out, .data$year >= as.integer(start))
  }
  if (!is.null(end)) {
    out <- dplyr::filter(out, .data$year <= as.integer(end))
  }

  dups <- out |>
    dplyr::summarise(n = dplyr::n(), .by = c("country_id", "year", "month")) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dups) > 0L) {
    rlang::abort(paste0(
      "OECD IRLT: duplicate (country_id, year, month) for '", code, "'. Count: ",
      nrow(dups)
    ))
  }

  out
}

#' Fetch and reshape one OECD IRLT series.
oecdIrltTool <- function(retrieve_code,
                         start = NULL,
                         end = NULL,
                         path = NULL,
                         url = .oecd_irlt_default_url) {
  wide <- oecd_irlt_read_wide(path = path, url = url)
  oecd_irlt_to_series(
    wide,
    retrieve_code = retrieve_code,
    start = start,
    end = end
  )
}
