#' OECD National Accounts Table 0200 helpers for tryImport
#'
#' Source: OECD NAD dataflow `OECD.SDD.NAD,DSD_NASEC10@DF_TABLE12` (Table 0200).
#' `retrieve_code` is an open composite key:
#'   `SECTOR.ACCOUNTING_ENTRY.TRANSACTION.UNIT_MEASURE`
#' Example: `S13.D.D41.XDC` — general government interest expenditure, national currency.
#'
#' Other SDMX dimensions use fixed Table 0200 defaults (annual, consolidated-style
#' counterpart `_Z`, valuation `S`, current prices `V`, etc.).
#' Observations are scaled to **billions of national currency**:
#' `OBS_VALUE * 10^(UNIT_MULT - 9)`.

##### Constants ----------------------------------------------------------------

.oecd_nasec_user_agent <- "country_analysis/oecd-nasec-import"

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) source(iso_path, local = FALSE)
}

.oecd_nasec_defaults <- list(
  freq = "A",
  counterpart_sector = "_Z",
  instr_asset = "_Z",
  expenditure = "_Z",
  valuation = "S",
  price_base = "V",
  transformation = "N",
  table_identifier = "T0200"
)

# S13 + XDC + standard valuation slice (manageable cache for Table 0200 imports).
.oecd_nasec_default_url <- paste0(
  "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,",
  "DSD_NASEC10@DF_TABLE12/",
  "A..S13......XDC.S.V.N.T0200",
  "?format=csvfilewithlabels"
)

.oecd_nasec_required_cols <- c(
  "REF_AREA", "FREQ", "SECTOR", "COUNTERPART_SECTOR", "ACCOUNTING_ENTRY",
  "TRANSACTION", "INSTR_ASSET", "EXPENDITURE", "UNIT_MEASURE", "VALUATION",
  "PRICE_BASE", "TRANSFORMATION", "TABLE_IDENTIFIER", "TIME_PERIOD",
  "OBS_VALUE", "UNIT_MULT"
)

##### Parse / URL --------------------------------------------------------------

#' Default SDMX CSV URL for OECD NASEC Table 0200 (S13 / XDC slice).
oecd_nasec_default_url <- function() {
  .oecd_nasec_default_url
}

#' Columns required in OECD NASEC Table 0200 CSV.
oecd_nasec_required_cols <- function() {
  .oecd_nasec_required_cols
}

#' Parse `SECTOR.ACCOUNTING_ENTRY.TRANSACTION.UNIT_MEASURE` retrieve_code.
#'
#' @return Named list of filter values (including defaults).
oecd_nasec_parse_code <- function(retrieve_code) {
  stopifnot(length(retrieve_code) == 1L)
  code <- toupper(trimws(as.character(retrieve_code)))
  if (!nzchar(code)) {
    rlang::abort("OECD NASEC retrieve_code is empty")
  }

  parts <- strsplit(code, ".", fixed = TRUE)[[1]]
  if (length(parts) != 4L || any(!nzchar(parts))) {
    rlang::abort(paste0(
      "OECD NASEC retrieve_code must be ",
      "SECTOR.ACCOUNTING_ENTRY.TRANSACTION.UNIT_MEASURE ",
      "(e.g. S13.D.D41.XDC). Got: ", retrieve_code
    ))
  }

  list(
    freq = .oecd_nasec_defaults$freq,
    sector = parts[[1]],
    counterpart_sector = .oecd_nasec_defaults$counterpart_sector,
    accounting_entry = parts[[2]],
    transaction = parts[[3]],
    instr_asset = .oecd_nasec_defaults$instr_asset,
    expenditure = .oecd_nasec_defaults$expenditure,
    unit_measure = parts[[4]],
    valuation = .oecd_nasec_defaults$valuation,
    price_base = .oecd_nasec_defaults$price_base,
    transformation = .oecd_nasec_defaults$transformation,
    table_identifier = .oecd_nasec_defaults$table_identifier
  )
}

#' Build SDMX key (REF_AREA left empty = all areas) from parsed filters.
oecd_nasec_build_key <- function(flt) {
  paste(
    flt$freq,
    "",
    flt$sector,
    flt$counterpart_sector,
    flt$accounting_entry,
    flt$transaction,
    flt$instr_asset,
    flt$expenditure,
    flt$unit_measure,
    flt$valuation,
    flt$price_base,
    flt$transformation,
    flt$table_identifier,
    sep = "."
  )
}

##### IO -----------------------------------------------------------------------

#' Read OECD NASEC Table 0200 CSV (local path or SDMX URL).
oecd_nasec_read_wide <- function(path = NULL,
                                 url = .oecd_nasec_default_url) {
  if (!is.null(path) && length(path) == 1L && nzchar(path) && file.exists(path)) {
    raw <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    resp <- httr::GET(url, httr::user_agent(.oecd_nasec_user_agent), httr::timeout(180L))
    httr::stop_for_status(resp)
    raw <- readr::read_csv(
      rawConnection(httr::content(resp, as = "raw")),
      show_col_types = FALSE
    )
  }

  missing <- setdiff(.oecd_nasec_required_cols, names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "OECD NASEC CSV missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  raw |>
    dplyr::transmute(
      iso3 = as.character(.data$REF_AREA),
      freq = as.character(.data$FREQ),
      sector = as.character(.data$SECTOR),
      counterpart_sector = as.character(.data$COUNTERPART_SECTOR),
      accounting_entry = as.character(.data$ACCOUNTING_ENTRY),
      transaction = as.character(.data$TRANSACTION),
      instr_asset = as.character(.data$INSTR_ASSET),
      expenditure = as.character(.data$EXPENDITURE),
      unit_measure = as.character(.data$UNIT_MEASURE),
      valuation = as.character(.data$VALUATION),
      price_base = as.character(.data$PRICE_BASE),
      transformation = as.character(.data$TRANSFORMATION),
      table_identifier = as.character(.data$TABLE_IDENTIFIER),
      year = suppressWarnings(as.integer(.data$TIME_PERIOD)),
      obs_value = suppressWarnings(as.numeric(.data$OBS_VALUE)),
      unit_mult = suppressWarnings(as.integer(.data$UNIT_MULT))
    ) |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$year))
}

#' Map ISO3 to project `country_id`.
oecd_nasec_iso2 <- function(iso3) {
  project_countrycode_iso3_to_iso2(trimws(as.character(iso3)))
}

#' Scale OECD OBS_VALUE to billions of national currency via UNIT_MULT.
oecd_nasec_scale_bln <- function(obs_value, unit_mult) {
  if (any(is.na(unit_mult))) {
    rlang::abort("OECD NASEC: UNIT_MULT is missing/NA; cannot scale to billions")
  }
  as.numeric(obs_value) * (10 ^ (as.numeric(unit_mult) - 9))
}

#' Build one country-year series from OECD NASEC wide table.
oecd_nasec_to_series <- function(wide,
                                 retrieve_code,
                                 start = NULL,
                                 end = NULL) {
  flt <- oecd_nasec_parse_code(retrieve_code)

  out <- wide |>
    dplyr::filter(
      .data$freq == flt$freq,
      .data$sector == flt$sector,
      .data$counterpart_sector == flt$counterpart_sector,
      .data$accounting_entry == flt$accounting_entry,
      .data$transaction == flt$transaction,
      .data$instr_asset == flt$instr_asset,
      .data$expenditure == flt$expenditure,
      .data$unit_measure == flt$unit_measure,
      .data$valuation == flt$valuation,
      .data$price_base == flt$price_base,
      .data$transformation == flt$transformation,
      .data$table_identifier == flt$table_identifier
    )

  if (nrow(out) == 0L) {
    rlang::abort(paste0(
      "OECD NASEC: no rows for retrieve_code '", retrieve_code, "'"
    ))
  }

  out <- out |>
    dplyr::transmute(
      country_id = oecd_nasec_iso2(.data$iso3),
      year = .data$year,
      value = oecd_nasec_scale_bln(.data$obs_value, .data$unit_mult)
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
      "OECD NASEC: duplicate (country_id, year) for '", retrieve_code,
      "'. Count: ", nrow(dups)
    ))
  }

  out
}

#' Fetch and reshape one OECD NASEC Table 0200 series.
oecdNasecTool <- function(retrieve_code,
                          start = NULL,
                          end = NULL,
                          path = NULL,
                          url = .oecd_nasec_default_url) {
  wide <- oecd_nasec_read_wide(path = path, url = url)
  oecd_nasec_to_series(
    wide,
    retrieve_code = retrieve_code,
    start = start,
    end = end
  )
}
