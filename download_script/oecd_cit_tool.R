#' OECD Corporate Tax Statistics helpers for tryImport
#'
#' Source: OECD CTS dataflow `OECD.CTP.TPS,DSD_TAX_CIT@DF_CIT`.
#' `retrieve_code` selects the statutory rate slice:
#'   - `COMBINED` — combined central + sub-central (MEASURE `CIT_C`)
#'   - `CENTRAL`  — central government only (`CIT`, sector central)
#'   - `SUB_CENTRAL` — sub-central only (`CIT_SCG`)

##### Constants ----------------------------------------------------------------

.oecd_cit_user_agent <- "country_analysis/oecd-cit-import"

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) source(iso_path, local = FALSE)
}

.oecd_cit_default_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.CTP.TPS,",
  "DSD_TAX_CIT@DF_CIT,1.0/?format=csvfilewithlabels"
)

.oecd_cit_series <- c("COMBINED", "CENTRAL", "SUB_CENTRAL")

.oecd_cit_filters <- list(
  COMBINED = list(measure = "CIT_C", targeting = "ST"),
  CENTRAL = list(measure = "CIT", targeting = "ST", sector = "S1311"),
  SUB_CENTRAL = list(measure = "CIT_SCG", targeting = "ST", sector = "S13M")
)

##### Parse / IO ---------------------------------------------------------------

#' Allowed OECD CIT retrieve_code values.
oecd_cit_allowed_series <- function() {
  .oecd_cit_series
}

#' Validate OECD CIT retrieve_code.
oecd_cit_parse_code <- function(retrieve_code) {
  stopifnot(length(retrieve_code) == 1L)
  code <- toupper(trimws(as.character(retrieve_code)))
  if (!nzchar(code) || !code %in% .oecd_cit_series) {
    rlang::abort(paste0(
      "OECD CIT retrieve_code must be one of: ",
      paste(.oecd_cit_series, collapse = ", "),
      ". Got: ", retrieve_code
    ))
  }
  code
}

oecd_cit_default_url <- function() {
  .oecd_cit_default_url
}

#' Read OECD CTS CSV (local path or SDMX URL).
oecd_cit_read_wide <- function(path = NULL,
                               url = .oecd_cit_default_url) {
  if (!is.null(path) && length(path) == 1L && nzchar(path) && file.exists(path)) {
    raw <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    resp <- httr::GET(url, httr::user_agent(.oecd_cit_user_agent), httr::timeout(180L))
    httr::stop_for_status(resp)
    raw <- readr::read_csv(
      rawConnection(httr::content(resp, as = "raw")),
      show_col_types = FALSE
    )
  }

  needed <- c("REF_AREA", "MEASURE", "TARGETING", "TIME_PERIOD", "OBS_VALUE")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "OECD CIT CSV missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  if ("SECTOR" %in% names(raw)) {
    raw$SECTOR <- as.character(raw$SECTOR)
  } else {
    raw$SECTOR <- NA_character_
  }

  raw |>
    dplyr::transmute(
      iso3 = as.character(.data$REF_AREA),
      measure = as.character(.data$MEASURE),
      targeting = as.character(.data$TARGETING),
      sector = .data$SECTOR,
      year = suppressWarnings(as.integer(.data$TIME_PERIOD)),
      value = suppressWarnings(as.numeric(.data$OBS_VALUE))
    ) |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$year))
}

#' Map ISO3 to project `country_id`.
oecd_cit_iso2 <- function(iso3) {
  project_countrycode_iso3_to_iso2(trimws(as.character(iso3)))
}

#' Build one country-year CIT series from OECD CTS wide table.
oecd_cit_to_series <- function(wide,
                               retrieve_code,
                               start = NULL,
                               end = NULL) {
  code <- oecd_cit_parse_code(retrieve_code)
  flt <- .oecd_cit_filters[[code]]

  out <- wide |>
    dplyr::filter(
      .data$measure == flt$measure,
      .data$targeting == flt$targeting
    )
  if (!is.null(flt$sector)) {
    out <- dplyr::filter(out, .data$sector == flt$sector)
  }

  out <- out |>
    dplyr::transmute(
      country_id = oecd_cit_iso2(.data$iso3),
      year = .data$year,
      value = .data$value
    ) |>
    dplyr::filter(!is.na(.data$country_id), !is.na(.data$value))

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
      "OECD CIT: duplicate (country_id, year) for '", code, "'. Count: ",
      nrow(dups)
    ))
  }

  out
}

#' Fetch and reshape one OECD CIT series.
oecdCitTool <- function(retrieve_code,
                        start = NULL,
                        end = NULL,
                        path = NULL,
                        url = .oecd_cit_default_url) {
  wide <- oecd_cit_read_wide(path = path, url = url)
  oecd_cit_to_series(
    wide,
    retrieve_code = retrieve_code,
    start = start,
    end = end
  )
}
