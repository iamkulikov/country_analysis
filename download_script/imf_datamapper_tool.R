#' IMF Datamapper helper: fetch country-level Fiscal Monitor indicators
#'
#' @param code Character scalar Datamapper indicator id (impplan retrieve_code).
#' @param start Optional integer year (inclusive).
#' @param end Optional integer year (inclusive).
#' @return Tibble with columns iso2, year, value.

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) source(iso_path, local = FALSE)
}

imf_datamapper_api_url <- function(path) {
  path <- sub("^/+", "", path)
  paste0("https://www.imf.org/external/datamapper/api/v1/", path)
}

imf_datamapper_fetch_json <- function(path) {
  requireNamespace("httr", quietly = TRUE)
  requireNamespace("jsonlite", quietly = TRUE)

  url <- imf_datamapper_api_url(path)
  resp <- httr::GET(url, httr::timeout(120L))
  status <- httr::status_code(resp)
  if (status >= 400L) {
    rlang::abort(sprintf("IMF Datamapper HTTP %s for %s", status, url))
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

imf_datamapper_list_indicators <- function() {
  payload <- imf_datamapper_fetch_json("indicators")
  inds <- payload$indicators
  if (is.null(inds) || !length(inds)) {
    rlang::abort("IMF Datamapper: indicators catalog is empty.")
  }
  names(inds)
}

imf_datamapper_values_to_tibble <- function(code, vals) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  if (is.null(vals) || !length(vals)) {
    rlang::abort(sprintf("IMF Datamapper: no values for indicator '%s'.", code))
  }

  out <- purrr::map_dfr(names(vals), function(iso3) {
    year_vals <- vals[[iso3]]
    if (is.null(year_vals) || !length(year_vals)) {
      return(tibble::tibble(iso3 = character(), year = integer(), value = double()))
    }

    tibble::tibble(
      iso3 = iso3,
      year = as.integer(names(year_vals)),
      value = suppressWarnings(as.numeric(unlist(year_vals, use.names = FALSE)))
    )
  })

  if (nrow(out) == 0L) {
    rlang::abort(sprintf("IMF Datamapper: empty panel for '%s'.", code))
  }

  out
}

imf_datamapper_tool <- function(code, start = NULL, end = NULL) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("countrycode", quietly = TRUE)

  stopifnot(is.character(code), length(code) == 1L, nzchar(code))

  to_year <- function(x, name) {
    if (is.null(x)) return(NULL)
    y <- suppressWarnings(as.integer(x))
    if (length(y) != 1L || is.na(y)) {
      rlang::abort(paste0("`", name, "` must be a single integer year."))
    }
    y
  }
  start <- to_year(start, "start")
  end <- to_year(end, "end")
  if (!is.null(start) && !is.null(end) && start > end) {
    rlang::abort("`start` cannot be greater than `end`.")
  }

  payload <- imf_datamapper_fetch_json(code)
  vals <- payload$values[[code]]
  out <- imf_datamapper_values_to_tibble(code, vals)

  out <- out |>
    dplyr::mutate(
      iso2 = project_countrycode_iso3_to_iso2(.data$iso3)
    ) |>
    dplyr::filter(!is.na(.data$iso2), nchar(.data$iso2) == 2L) |>
    dplyr::transmute(iso2 = .data$iso2, year = .data$year, value = .data$value)

  if (!is.null(start)) out <- dplyr::filter(out, .data$year >= start)
  if (!is.null(end)) out <- dplyr::filter(out, .data$year <= end)

  if (nrow(out) == 0L) {
    rlang::abort(sprintf("IMF Datamapper: no mapped country observations for '%s'.", code))
  }

  dplyr::arrange(out, .data$iso2, .data$year)
}
