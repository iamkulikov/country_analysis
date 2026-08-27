#' UNSD SNAAMA (National Accounts AMA) helpers for tryImport
#'
#' Files: Download-GDPcurrent-NCU-countries.xlsx,
#'        Download-GDPconstant-NCU-countries.xlsx
#' from https://unstats.un.org/unsd/snaama/Downloads
#'
#' `retrieve_code` is the exact `IndicatorName` string in the data sheet
#' (after trimws). Current vs constant series are distinguished by `file_name`
#' in the import plan, not by retrieve_code.

##### Constants ----------------------------------------------------------------

.snaama_year_pattern <- "^(19|20)\\d{2}$"

# UN M49 numeric → ISO2 overrides (countrycode origin = "un" misses these).
# 835 = U.R. of Tanzania: Mainland (file has no 834). Zanzibar 836 stays NA.
.snaama_un_custom_match <- c(
  `412` = "XK", # Kosovo
  `835` = "TZ"  # Tanzania Mainland
)

##### Sheets / read ------------------------------------------------------------

#' Find the SNAAMA data sheet (not footnotes).
#'
#' @param path Path to an Excel file.
#' @return Character scalar sheet name.
snaama_find_data_sheet <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  if (!file.exists(path)) {
    rlang::abort(paste0("SNAAMA file not found: ", path))
  }
  sheets <- readxl::excel_sheets(path)
  hit <- sheets[
    grepl("^Download-GDP", sheets, ignore.case = TRUE) &
      !grepl("Footnote", sheets, ignore.case = TRUE)
  ]
  if (!length(hit)) {
    rlang::abort(paste0(
      "No SNAAMA data sheet in ", basename(path),
      ". Have: ", paste(sheets, collapse = ", ")
    ))
  }
  hit[[1]]
}

#' Locate the header row that starts with CountryID.
#'
#' @param path Path to Excel file.
#' @param sheet Optional sheet name; default via snaama_find_data_sheet().
#' @return Integer 1-based row index of the header.
snaama_header_row <- function(path, sheet = NULL) {
  if (is.null(sheet)) sheet <- snaama_find_data_sheet(path)
  preview <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    n_max = 10L,
    .name_repair = "minimal"
  )
  if (!ncol(preview)) {
    rlang::abort(paste0("Empty sheet in ", basename(path)))
  }
  first_col <- as.character(preview[[1]])
  idx <- which(trimws(first_col) == "CountryID")
  if (!length(idx)) {
    rlang::abort(paste0(
      "Could not find CountryID header row in ", basename(path),
      " / ", sheet
    ))
  }
  as.integer(idx[[1]])
}

#' Read SNAAMA wide table (CountryID, Country, Currency, IndicatorName, years).
#'
#' @param path Path to Excel file.
#' @return Tibble with character IndicatorName and numeric year columns.
snaama_read_wide <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))
  sheet <- snaama_find_data_sheet(path)
  hdr_row <- snaama_header_row(path, sheet = sheet)
  # skip = rows before the header so that row becomes colnames
  wide <- readxl::read_excel(
    path,
    sheet = sheet,
    skip = hdr_row - 1L,
    col_names = TRUE,
    na = c("", "..", "NA", "n/a", "N/A"),
    .name_repair = "minimal"
  )
  need <- c("CountryID", "Country", "Currency", "IndicatorName")
  miss <- setdiff(need, names(wide))
  if (length(miss)) {
    rlang::abort(paste0(
      "SNAAMA ", basename(path), " missing column(s): ",
      paste(miss, collapse = ", ")
    ))
  }
  year_cols <- names(wide)[grepl(.snaama_year_pattern, names(wide))]
  if (!length(year_cols)) {
    rlang::abort(paste0(
      "SNAAMA ", basename(path), ": no year columns matching ",
      .snaama_year_pattern
    ))
  }
  wide |>
    dplyr::mutate(
      CountryID = as.integer(.data$CountryID),
      IndicatorName = trimws(as.character(.data$IndicatorName))
    ) |>
    dplyr::filter(!is.na(.data$CountryID), !is.na(.data$IndicatorName), nzchar(.data$IndicatorName))
}

#' Unique IndicatorName values in a SNAAMA file (for filling impplan).
#'
#' @param path Path to Excel file.
#' @return Sorted character vector.
snaama_list_indicators <- function(path) {
  wide <- snaama_read_wide(path)
  sort(unique(wide$IndicatorName))
}

##### Country mapping ----------------------------------------------------------

#' Map UN M49 CountryID to ISO2 country_id.
#'
#' Unmapped IDs (former states, Zanzibar, …) become NA and should be dropped.
#'
#' @param un_ids Numeric or integer UN country codes.
#' @return Character vector of ISO2 codes (same length; NA if unmapped).
snaama_map_iso2 <- function(un_ids) {
  ids <- as.integer(un_ids)
  custom <- .snaama_un_custom_match
  # countrycode custom_match keys are character codes
  custom_chr <- stats::setNames(as.character(custom), names(custom))
  countrycode::countrycode(
    ids,
    origin = "un",
    destination = "iso2c",
    custom_match = custom_chr,
    warn = FALSE
  )
}

##### Extract ------------------------------------------------------------------

#' Extract one IndicatorName series from a wide SNAAMA table.
#'
#' @param wide From snaama_read_wide().
#' @param retrieve_code Exact IndicatorName (trimws applied).
#' @return Tibble: country_id, year, value (unmapped countries dropped).
snaama_extract <- function(wide, retrieve_code) {
  stopifnot(is.character(retrieve_code), length(retrieve_code) == 1L)
  code <- trimws(retrieve_code)
  if (!nzchar(code)) {
    rlang::abort("SNAAMA retrieve_code is empty")
  }
  avail <- sort(unique(wide$IndicatorName))
  if (!(code %in% avail)) {
    rlang::abort(paste0(
      "SNAAMA IndicatorName not found: '", code, "'. Available:\n  ",
      paste(avail, collapse = "\n  ")
    ))
  }
  year_cols <- names(wide)[grepl(.snaama_year_pattern, names(wide))]
  wide |>
    dplyr::filter(.data$IndicatorName == code) |>
    dplyr::select("CountryID", dplyr::all_of(year_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(
      country_id = snaama_map_iso2(.data$CountryID),
      year = as.integer(.data$year),
      value = as.numeric(.data$value)
    ) |>
    dplyr::filter(!is.na(.data$country_id), !is.na(.data$year)) |>
    dplyr::select("country_id", "year", "value")
}

#' Read one SNAAMA series: country_id, year, value.
#'
#' @param path Path to Excel file.
#' @param retrieve_code Exact IndicatorName.
#' @return Tibble with country_id, year, value.
snaamaTool <- function(path, retrieve_code) {
  wide <- snaama_read_wide(path)
  snaama_extract(wide, retrieve_code)
}
