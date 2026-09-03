# Smoke / unit checks for ISO country-code helpers (Phase 2).
# Run: Rscript download_script/tests/test_iso_country_codes.R

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(countrycode)
})

here_root <- Sys.getenv("COUNTRY_ANALYSIS_ROOT", unset = "")
if (!nzchar(here_root)) {
  script_path <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  if (length(script_path) == 1L && nzchar(script_path)) {
    here_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/")
  } else {
    here_root <- normalizePath(getwd(), winslash = "/")
  }
}

iso_path <- file.path(here_root, "download_script", "iso_country_codes.R")
if (!file.exists(iso_path)) {
  stop("iso_country_codes.R not found at: ", iso_path)
}
source(iso_path, local = FALSE)

fail <- function(msg) stop(msg, call. = FALSE)

cat("test_iso_country_codes: helpers\n")

if (!identical(safe_iso3_to_iso2("NAM"), "NA")) {
  fail("safe_iso3_to_iso2('NAM') should be 'NA'")
}

if (!identical(safe_iso2_to_iso3("NA"), "NAM")) {
  fail("safe_iso2_to_iso3('NA') should be 'NAM'")
}

norm <- normalize_chr_vec(c("CN", NA))
if (any(is.na(norm)) || "NA" %in% norm) {
  fail("normalize_chr_vec should drop real NA, not emit ISO2 NA")
}

mapped <- safe_iso3_to_iso2(c("CHN", "NAM", NA))
if (length(mapped) != 3L) {
  fail("safe_iso3_to_iso2 must be length-preserving")
}
if (!identical(mapped[2], "NA") || !is.na(mapped[3])) {
  fail("safe_iso3_to_iso2 mapping incorrect for NAM / NA input")
}

cm <- project_iso3_custom_match()
if (!"NAM" %in% names(cm) || cm[["NAM"]] != "NA") {
  fail("project_iso3_custom_match must include NAM -> NA")
}

cat("test_iso_country_codes: WDI skeleton Namibia restore\n")

countries_raw <- tibble::tibble(
  country = c("Namibia", "China"),
  iso2c = c(NA_character_, "CN"),
  iso3c = c("NAM", "CHN"),
  region = c("Sub-Saharan Africa", "East Asia & Pacific")
)

countries <- countries_raw |>
  distinct(country, iso2c, iso3c, region) |>
  filter(region != "Aggregates") |>
  mutate(
    iso2c = dplyr::coalesce(
      iso2c,
      project_countrycode_iso3_to_iso2(iso3c)
    ),
    iso2c = dplyr::if_else(
      is.na(iso2c) & country == "Namibia",
      "NA",
      iso2c
    )
  ) |>
  filter(!is.na(iso2c), nzchar(iso2c), nchar(iso2c) == 2L) |>
  rename(country_id = iso2c)

if (!("NA" %in% countries$country_id)) {
  fail("Namibia ISO2 'NA' missing after skeleton restore")
}
if (sum(countries$country == "Namibia") != 1L) {
  fail("Expected exactly one Namibia row in countries skeleton")
}

cat("test_iso_country_codes: OK\n")
