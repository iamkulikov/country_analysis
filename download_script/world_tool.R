#' IMF World Revenue Longitudinal Database (WoRLD) helpers for tryImport
#'
#' Source: https://www.imf.org/en/topics/fiscal-policies/world-revenue-longitudinal-database
#' SDMX dataflow `IMF.FAD/WORLD` (% of GDP series end with `_POGDP_PT_R`).
#'
#' `retrieve_code` uses WoRLD variable names from the technical note
#' (e.g. `TotRev`, `TaxRev`, `SocialCon`).

##### Constants ----------------------------------------------------------------

.world_user_agent <- "country_analysis/world-import"

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) source(iso_path, local = FALSE)
}

.world_sdmx_base <- paste0(
  "https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.FAD/WORLD/+/"
)

.world_transform <- "POGDP_PT"
.world_frequency <- "A"

.world_core_map <- c(
  TotRev    = "G1_POGDP_PT_R",
  TaxRev    = "G11_POGDP_PT_R",
  TaxInc    = "G111_POGDP_PT_R",
  TaxIncI   = "G1111_POGDP_PT_R",
  TaxIncC   = "G1112_POGDP_PT_R",
  TaxPro    = "G113_POGDP_PT_R",
  TaxSal    = "G114_POGDP_PT_R",
  TaxSalG   = "G11412_POGDP_PT_R",
  TaxTra    = "G115_POGDP_PT_R",
  SocialCon = "G121_POGDP_PT_R",
  Grants    = "G13_POGDP_PT_R",
  RevOth    = "G14_POGDP_PT_R",
  NonTaxRes = "NONTAXRES_POGDP_PT_R"
)

##### Parse / IO ---------------------------------------------------------------

#' Allowed WoRLD retrieve_code values (core revenue structure).
world_allowed_series <- function() {
  names(.world_core_map)
}

#' Map WoRLD retrieve_code to SDMX indicator id.
world_parse_code <- function(retrieve_code) {
  stopifnot(length(retrieve_code) == 1L)
  code <- trimws(as.character(retrieve_code))
  ind <- .world_core_map[[code]]
  if (is.null(ind) || !nzchar(ind)) {
    rlang::abort(paste0(
      "WoRLD retrieve_code must be one of: ",
      paste(names(.world_core_map), collapse = ", "),
      ". Got: ", retrieve_code
    ))
  }
  list(plan_code = code, indicator = ind)
}

#' Build SDMX CSV URL for one WoRLD indicator (all countries).
world_build_url <- function(indicator,
                            start = NULL,
                            end = NULL) {
  stopifnot(is.character(indicator), length(indicator) == 1L, nzchar(indicator))
  key <- paste0("*.", indicator, ".", .world_transform, ".", .world_frequency)
  url <- paste0(.world_sdmx_base, key)
  qs <- character()
  if (!is.null(start)) qs <- c(qs, paste0("startPeriod=", as.integer(start)))
  if (!is.null(end)) qs <- c(qs, paste0("endPeriod=", as.integer(end)))
  if (length(qs)) url <- paste0(url, "?", paste(qs, collapse = "&"))
  url
}

#' Fetch WoRLD SDMX-CSV and return iso2/year/value.
world_fetch_csv <- function(url) {
  resp <- httr::GET(
    url,
    httr::user_agent(.world_user_agent),
    httr::timeout(180L),
    httr::add_headers(Accept = "application/vnd.sdmx.data+csv; version=2")
  )
  httr::stop_for_status(resp)

  raw <- readr::read_csv(
    rawConnection(httr::content(resp, as = "raw")),
    show_col_types = FALSE
  )

  needed <- c("COUNTRY", "TIME_PERIOD", "OBS_VALUE")
  missing <- setdiff(needed, names(raw))
  if (length(missing)) {
    rlang::abort(paste0(
      "WoRLD CSV missing columns: ",
      paste(missing, collapse = ", ")
    ))
  }

  raw |>
    dplyr::transmute(
      iso3 = as.character(.data$COUNTRY),
      year = suppressWarnings(as.integer(.data$TIME_PERIOD)),
      value = suppressWarnings(as.numeric(.data$OBS_VALUE))
    ) |>
    dplyr::filter(!is.na(.data$iso3), nzchar(.data$iso3), !is.na(.data$year))
}

#' Map IMF WoRLD ISO3 to project iso2.
world_iso2 <- function(iso3) {
  project_countrycode_iso3_to_iso2(trimws(as.character(iso3)))
}

#' Build one country-year WoRLD series (% of GDP).
world_to_series <- function(raw,
                            retrieve_code,
                            start = NULL,
                            end = NULL) {
  world_parse_code(retrieve_code)

  out <- raw |>
    dplyr::transmute(
      iso2 = world_iso2(.data$iso3),
      year = .data$year,
      value = .data$value
    ) |>
    dplyr::filter(!is.na(.data$iso2), !is.na(.data$value))

  if (!is.null(start)) {
    out <- dplyr::filter(out, .data$year >= as.integer(start))
  }
  if (!is.null(end)) {
    out <- dplyr::filter(out, .data$year <= as.integer(end))
  }

  dups <- out |>
    dplyr::summarise(n = dplyr::n(), .by = c("iso2", "year")) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dups) > 0L) {
    rlang::abort(paste0(
      "WoRLD: duplicate (iso2, year) for '", retrieve_code, "'. Count: ",
      nrow(dups)
    ))
  }

  out
}

#' Fetch one WoRLD revenue-structure series.
#'
#' @return Tibble `iso2`, `year`, `value` (compatible with `imfTool()`).
worldTool <- function(retrieve_code,
                      start = NULL,
                      end = NULL) {
  meta <- world_parse_code(retrieve_code)
  url <- world_build_url(meta$indicator, start = start, end = end)
  raw <- world_fetch_csv(url)
  world_to_series(raw, retrieve_code = retrieve_code, start = start, end = end)
}
