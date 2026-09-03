# Smoke / unit checks for OECD NASEC Table 0200 helpers.
# Run: Rscript download_script/tests/test_oecd_nasec_tool.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(httr)
  library(rlang)
})

here_root <- Sys.getenv("COUNTRY_ANALYSIS_ROOT", unset = "")
if (!nzchar(here_root)) {
  script_path <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  if (length(script_path) == 1L && nzchar(script_path)) {
    here_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/")
  } else {
    here_root <- normalizePath(getwd(), winslash = "/")
  }
}

iso_path <- file.path(here_root, "download_script", "iso_country_codes.R")
tool_path <- file.path(here_root, "download_script", "oecd_nasec_tool.R")
fixture <- file.path(
  here_root, "download_script", "tests", "fixtures", "oecd_nasec_table12_sample.csv"
)
if (!file.exists(iso_path) || !file.exists(tool_path) || !file.exists(fixture)) {
  stop("Required files missing under: ", here_root)
}
source(iso_path, local = FALSE)
source(tool_path, local = FALSE)

fail <- function(msg) stop(msg, call. = FALSE)
approx_eq <- function(a, b, tol = 1e-6) abs(as.numeric(a) - as.numeric(b)) <= tol

cat("test_oecd_nasec_tool: parse_code\n")

flt <- oecd_nasec_parse_code("S13.D.D41.XDC")
if (!identical(flt$sector, "S13") ||
      !identical(flt$accounting_entry, "D") ||
      !identical(flt$transaction, "D41") ||
      !identical(flt$unit_measure, "XDC") ||
      !identical(flt$table_identifier, "T0200") ||
      !identical(flt$freq, "A")) {
  fail("parse_code S13.D.D41.XDC returned unexpected filters")
}

key <- oecd_nasec_build_key(flt)
if (!identical(key, "A..S13._Z.D.D41._Z._Z.XDC.S.V.N.T0200")) {
  fail(paste("unexpected SDMX key:", key))
}

bad <- tryCatch(oecd_nasec_parse_code("D41"), error = function(e) e)
if (!inherits(bad, "error")) {
  fail("parse_code should reject short codes")
}

cat("test_oecd_nasec_tool: scale_bln / fixture series\n")

if (!approx_eq(oecd_nasec_scale_bln(29724.1, 6L), 29.7241)) {
  fail("scale_bln(29724.1, 6) should be 29.7241")
}

wide <- oecd_nasec_read_wide(path = fixture)
ser <- oecd_nasec_to_series(wide, "S13.D.D41.XDC")
if (nrow(ser) != 2L) {
  fail(paste("expected 2 country-years for D41 expenditure, got", nrow(ser)))
}
fra <- ser |> dplyr::filter(country_id == "FR", year == 2020L)
if (nrow(fra) != 1L || !approx_eq(fra$value[[1]], 29.7241)) {
  fail("FRA 2020 D41 expenditure should be ~29.7241 bln")
}
usa <- ser |> dplyr::filter(country_id == "US", year == 2020L)
if (nrow(usa) != 1L || !approx_eq(usa$value[[1]], 500)) {
  fail("USA 2020 D41 expenditure should be 500 bln")
}

rev <- oecd_nasec_to_series(wide, "S13.C.D41.XDC")
if (nrow(rev) != 1L || !identical(rev$country_id[[1]], "FR")) {
  fail("S13.C.D41.XDC should return FRA revenue only")
}

na_mult <- tryCatch(oecd_nasec_scale_bln(1, NA_integer_), error = function(e) e)
if (!inherits(na_mult, "error")) {
  fail("scale_bln must abort on NA UNIT_MULT")
}

online <- tolower(Sys.getenv("OECD_NASEC_ONLINE", unset = "1")) %in% c("1", "true", "yes")
if (isTRUE(online)) {
  cat("test_oecd_nasec_tool: online smoke S13.D.D41.XDC (FRA 2020)\n")
  live <- tryCatch(
    oecdNasecTool("S13.D.D41.XDC", start = 2020L, end = 2020L),
    error = function(e) e
  )
  if (inherits(live, "error")) {
    fail(paste("online oecdNasecTool failed:", live$message))
  }
  fra_live <- live |> dplyr::filter(country_id == "FR", year == 2020L)
  if (nrow(fra_live) != 1L) {
    fail("online smoke: FRA 2020 missing")
  }
  if (!approx_eq(fra_live$value[[1]], 29.7241, tol = 0.05)) {
    fail(paste(
      "online smoke: FRA 2020 expected ~29.72 bln, got",
      fra_live$value[[1]]
    ))
  }
  cat("  FRA 2020 =", fra_live$value[[1]], "bln OK\n")
} else {
  cat("test_oecd_nasec_tool: online smoke skipped (OECD_NASEC_ONLINE=0)\n")
}

cat("test_oecd_nasec_tool: OK\n")
