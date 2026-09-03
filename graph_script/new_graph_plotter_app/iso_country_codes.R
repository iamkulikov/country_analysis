#' Shared ISO2 / ISO3 helpers for import and graph layers.
#'
#' Rules:
#' - Drop R `NA` before `as.character()` on country-code vectors.
#' - ISO2 Namibia is the string `"NA"`; do not treat it as missing.
#' - Length-preserving mappers are safe inside dplyr::mutate().

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L || (is.atomic(x) && all(is.na(x)))) y else x
  }
}

#' Unified ISO3 → ISO2 custom_match for project sources.
project_iso3_custom_match <- function() {
  c(
    ROM = "RO", ADO = "AD", ANT = "AN", KSV = "XK", KOS = "XK",
    TMP = "TL", WBG = "PS", ZAR = "CD", XKX = "XK", NAM = "NA",
    G001 = "1W", OWID_WRL = "1W"
  )
}

normalize_iso3_strict <- function(x, keep_unknown = TRUE) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    rlang::abort("Package 'stringr' is required for normalize_iso3_strict().")
  }
  if (!requireNamespace("countrycode", quietly = TRUE)) {
    rlang::abort("Package 'countrycode' is required for normalize_iso3_strict().")
  }

  x0 <- x |> as.character() |> stringr::str_trim() |> stringr::str_to_upper()
  x0 <- dplyr::if_else(stringr::str_detect(x0, "^[A-Z]{3}$"), x0, NA_character_)

  iso_ok <- x0 %in% countrycode::codelist$iso3c

  if (isTRUE(keep_unknown)) {
    return(x0)
  }

  dplyr::if_else(iso_ok, x0, NA_character_)
}

#' Length-preserving ISO3 → ISO2 with project patches (strict ISO3 input).
safe_iso3_to_iso2 <- function(iso3) {
  iso3_norm <- normalize_iso3_strict(iso3)
  n <- length(iso3_norm)
  if (n == 0L) {
    return(character(0))
  }

  iso2 <- rep(NA_character_, n)
  ok <- !is.na(iso3_norm) & iso3_norm != ""
  if (!any(ok)) {
    return(iso2)
  }

  mapped <- countrycode::countrycode(
    iso3_norm[ok],
    origin = "iso3c",
    destination = "iso2c",
    custom_match = project_iso3_custom_match(),
    warn = FALSE
  )
  mapped[iso3_norm[ok] == "XKX"] <- "XK"
  mapped[iso3_norm[ok] == "NAM"] <- "NA"
  iso2[ok] <- mapped
  iso2
}

#' Length-preserving ISO2 → ISO3 with project patches.
safe_iso2_to_iso3 <- function(iso2) {
  n <- length(iso2)
  if (n == 0L) {
    return(character(0))
  }

  iso3_out <- rep(NA_character_, n)
  ok <- !is.na(iso2) & iso2 != ""
  if (!any(ok)) {
    return(iso3_out)
  }

  x <- stringr::str_trim(as.character(iso2[ok]))
  x <- stringr::str_to_upper(x)

  mapped <- countrycode::countrycode(x, "iso2c", "iso3c", warn = FALSE)
  mapped[x == "XK"] <- "XKX"
  mapped[x == "NA"] <- "NAM"
  iso3_out[ok] <- mapped
  iso3_out
}

#' Import-layer ISO3 → ISO2 (trim/upper; no strict 3-letter filter).
project_countrycode_iso3_to_iso2 <- function(iso3) {
  n <- length(iso3)
  if (n == 0L) {
    return(character(0))
  }

  iso2 <- rep(NA_character_, n)
  ok <- !is.na(iso3) & iso3 != ""
  if (!any(ok)) {
    return(iso2)
  }

  codes <- stringr::str_trim(as.character(iso3[ok]))
  codes <- stringr::str_to_upper(codes)

  mapped <- countrycode::countrycode(
    codes,
    origin = "iso3c",
    destination = "iso2c",
    custom_match = project_iso3_custom_match(),
    warn = FALSE
  )
  mapped[codes == "XKX"] <- "XK"
  mapped[codes == "NAM"] <- "NA"
  iso2[ok] <- mapped
  iso2
}

normalize_chr_vec <- function(x) {
  x <- x %||% character(0)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(character(0))
  }
  x <- as.character(x) |>
    stringr::str_trim()
  x <- x[x != ""]
  unique(x)
}

compact_iso2_vec <- function(iso3) {
  normalize_chr_vec(safe_iso3_to_iso2(iso3))
}

#' Post-read cleanup for ISO code columns (no as.character on missing values).
read_iso_codes_safe <- function(x) {
  x <- x %||% character(0)
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(character(0))
  }
  stringr::str_trim(stringr::str_to_upper(as.character(x)))
}
