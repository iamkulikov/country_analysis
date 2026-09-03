#' OWID Chart / Explorer / Indicator helpers for tryImport
#'
#' `retrieve_code` format (see docs):
#'   `{grapher|explorers}/{slug}[?view_params][#column_short_name]`
#' Full `https://ourworldindata.org/...` URLs are accepted.
#' Optional fallback key: `indicator/{owidVariableId}`.

##### Constants ----------------------------------------------------------------

.owid_user_agent <- "country_analysis/owid-import"

if (!exists("project_countrycode_iso3_to_iso2", mode = "function")) {
  iso_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "iso_country_codes.R")
  if (!file.exists(iso_path) && requireNamespace("here", quietly = TRUE)) {
    iso_path <- here::here("download_script", "iso_country_codes.R")
  }
  if (file.exists(iso_path)) {
    source(iso_path, local = FALSE)
  }
}

# Browser / display query keys stripped from retrieve_code (view params kept).
.owid_display_params <- c(
  "tab", "time", "country", "overlay", "stackMode", "region", "facet",
  "uniformYAxis", "showSelectionOnlyInTable", "mapSelectDropdowns",
  "tableUnit", "focus", "compareEndpoints", "shown", "endpointsOnly",
  "showMapAnnotationLabelsInChart", "hideControls", "mapSelect"
)

##### Parse --------------------------------------------------------------------

#' Parse OWID retrieve_code into kind / slug / query / column.
#'
#' @param retrieve_code Character scalar.
#' @return Named list: kind, slug, query (named character), column, variable_id.
owid_parse_code <- function(retrieve_code) {
  stopifnot(is.character(retrieve_code), length(retrieve_code) == 1L, nzchar(retrieve_code))
  raw <- trimws(retrieve_code)

  column <- NA_character_
  if (grepl("#", raw, fixed = TRUE)) {
    hash_pos <- regexpr("#", raw, fixed = TRUE)[1]
    frag <- substr(raw, hash_pos + 1L, nchar(raw))
    raw <- substr(raw, 1L, hash_pos - 1L)
    column <- if (nzchar(frag)) frag else NA_character_
  }

  raw <- sub("^https?://(?:www\\.)?ourworldindata\\.org/", "", raw,
             ignore.case = TRUE, perl = TRUE)
  raw <- sub("^/+", "", raw)

  if (grepl("^indicator/\\d+", raw, ignore.case = TRUE, perl = TRUE)) {
    id_chr <- sub("^indicator/(\\d+).*", "\\1", raw, ignore.case = TRUE, perl = TRUE)
    return(list(
      kind        = "indicator",
      slug        = id_chr,
      query       = character(),
      column      = column,
      variable_id = as.integer(id_chr)
    ))
  }

  query_str <- ""
  if (grepl("\\?", raw, perl = TRUE)) {
    bits <- strsplit(raw, "\\?", perl = TRUE)[[1]]
    path <- bits[[1]]
    query_str <- if (length(bits) >= 2L) paste(bits[-1], collapse = "?") else ""
  } else {
    path <- raw
  }
  path <- sub("/+$", "", path)
  # Allow paste of download URLs (.../slug.csv or .../slug.metadata.json)
  path <- sub("\\.(csv|metadata\\.json)$", "", path, ignore.case = TRUE, perl = TRUE)

  m <- regexec("^(grapher|explorers)/([^/?#]+)", path, perl = TRUE, ignore.case = TRUE)
  mm <- regmatches(path, m)[[1]]
  if (length(mm) < 3L) {
    rlang::abort(paste0(
      "OWID retrieve_code must look like ",
      "'grapher/{slug}', 'explorers/{slug}[?params][#column]', ",
      "or 'indicator/{id}'. Got: ", retrieve_code
    ))
  }

  # Drop Chart API transport keys; they are re-added when building URLs
  q <- owid_strip_display_params(owid_parse_query(query_str))
  drop_api <- tolower(names(q)) %in% c("v", "csvtype", "usecolumnshortnames")
  q <- q[!drop_api]

  list(
    kind        = tolower(mm[[2]]),
    slug        = mm[[3]],
    query       = q,
    column      = column,
    variable_id = NA_integer_
  )
}

#' Stable chart identity (kind + slug + sorted view query), ignoring column.
owid_chart_key <- function(parsed) {
  q <- parsed$query
  if (length(q)) {
    ord <- order(names(q), q)
    q_part <- paste(paste0(names(q)[ord], "=", q[ord]), collapse = "&")
  } else {
    q_part <- ""
  }
  paste(parsed$kind, parsed$slug, q_part, sep = "|")
}

owid_parse_query <- function(query_str) {
  if (is.null(query_str) || !nzchar(query_str)) return(character())
  parts <- strsplit(query_str, "&", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(character())

  keys <- character(length(parts))
  vals <- character(length(parts))
  for (i in seq_along(parts)) {
    kv <- strsplit(parts[[i]], "=", fixed = TRUE)[[1]]
    keys[[i]] <- utils::URLdecode(kv[[1]])
    raw_v <- if (length(kv) >= 2L) paste(kv[-1], collapse = "=") else ""
    # Normalize + / %20 before decode so explorer params match metadata
    raw_v <- gsub("\\+", "%20", raw_v, perl = TRUE)
    vals[[i]] <- utils::URLdecode(raw_v)
  }
  stats::setNames(vals, keys)
}

owid_strip_display_params <- function(query) {
  if (!length(query)) return(query)
  drop <- tolower(names(query)) %in% tolower(.owid_display_params)
  query[!drop]
}

owid_encode_query <- function(query) {
  if (!length(query)) return("")
  enc_keys <- vapply(names(query), utils::URLencode, character(1), reserved = TRUE)
  enc_vals <- vapply(as.character(query), function(v) {
    utils::URLencode(v, reserved = TRUE)
  }, character(1))
  paste(paste0(enc_keys, "=", enc_vals), collapse = "&")
}

##### HTTP ---------------------------------------------------------------------

owid_fetch_raw <- function(url, as = c("text", "raw")) {
  as <- rlang::arg_match(as)
  requireNamespace("httr", quietly = TRUE)
  resp <- httr::GET(
    url,
    httr::user_agent(.owid_user_agent),
    httr::timeout(120)
  )
  status <- httr::status_code(resp)
  if (status >= 400L) {
    rlang::abort(sprintf("OWID HTTP %s for %s", status, url))
  }
  if (identical(as, "raw")) {
    return(httr::content(resp, as = "raw"))
  }
  httr::content(resp, as = "text", encoding = "UTF-8")
}

owid_fetch_json <- function(url) {
  requireNamespace("jsonlite", quietly = TRUE)
  txt <- owid_fetch_raw(url, as = "text")
  jsonlite::fromJSON(txt, simplifyVector = TRUE)
}

owid_chart_base_url <- function(parsed) {
  paste0("https://ourworldindata.org/", parsed$kind, "/", parsed$slug)
}

owid_chart_query_string <- function(parsed) {
  q <- parsed$query
  # Chart API defaults for full country/year panel
  q[["v"]] <- "1"
  q[["csvType"]] <- "full"
  q[["useColumnShortNames"]] <- "true"
  owid_encode_query(q)
}

owid_metadata_url <- function(parsed) {
  paste0(owid_chart_base_url(parsed), ".metadata.json?", owid_chart_query_string(parsed))
}

owid_csv_url <- function(parsed) {
  paste0(owid_chart_base_url(parsed), ".csv?", owid_chart_query_string(parsed))
}

##### Metadata / column resolve ------------------------------------------------

owid_fetch_metadata <- function(parsed) {
  if (identical(parsed$kind, "indicator")) {
    url <- paste0(
      "https://api.ourworldindata.org/v1/indicators/",
      parsed$variable_id, ".metadata.json"
    )
    return(owid_fetch_json(url))
  }
  owid_fetch_json(owid_metadata_url(parsed))
}

#' Data-column short names from Chart metadata (`columns` object keys).
owid_metadata_columns <- function(meta) {
  cols <- meta$columns
  if (is.null(cols) || !length(cols)) return(character())
  if (is.data.frame(cols)) {
    # rare shape
    if ("shortName" %in% names(cols)) return(as.character(cols$shortName))
    return(character())
  }
  names(cols)
}

#' Resolve which shortName to extract; error if ambiguous.
owid_resolve_column <- function(meta, column = NA_character_) {
  avail <- owid_metadata_columns(meta)
  # Drop non-data helpers if any slip in (entity/code/year are CSV only)
  avail <- setdiff(avail, c("entity", "code", "year", "day", "date", "Entity", "Code", "Year"))
  if (!length(avail)) {
    rlang::abort("OWID metadata has no data columns.")
  }
  if (!is.null(column) && !is.na(column) && nzchar(column)) {
    if (!column %in% avail) {
      rlang::abort(paste0(
        "OWID column '", column, "' not in metadata. Available: ",
        paste(avail, collapse = ", ")
      ))
    }
    return(column)
  }
  if (length(avail) == 1L) return(avail[[1]])
  rlang::abort(paste0(
    "OWID chart has multiple columns; add #column_short_name to retrieve_code. Available: ",
    paste(avail, collapse = ", ")
  ))
}

owid_column_variable_id <- function(meta, column) {
  cols <- meta$columns
  if (is.null(cols) || is.null(cols[[column]])) return(NA_integer_)
  vid <- cols[[column]]$owidVariableId
  if (is.null(vid) || length(vid) != 1L) return(NA_integer_)
  as.integer(vid)
}

##### Country mapping ----------------------------------------------------------

#' Map OWID entity codes (ISO3 / OWID_*) to project iso2; unmapped → NA.
owid_map_iso2 <- function(codes) {
  codes <- as.character(codes)
  out <- project_countrycode_iso3_to_iso2(codes)
  out[codes == "OWID_WRL"] <- "1W"
  out
}

##### Fetch data ---------------------------------------------------------------

owid_trim_years <- function(df, start = NULL, end = NULL) {
  to_year <- function(x, name) {
    if (is.null(x)) return(NULL)
    y <- suppressWarnings(as.integer(x))
    if (length(y) != 1L || is.na(y)) {
      rlang::abort(paste0("`", name, "` must be a single integer year."))
    }
    y
  }
  start <- to_year(start, "start")
  end   <- to_year(end, "end")
  if (!is.null(start) && !is.null(end) && start > end) {
    rlang::abort("`start` cannot be greater than `end`.")
  }
  if (!"year" %in% names(df)) return(df)
  if (!is.null(start)) df <- dplyr::filter(df, .data$year >= start)
  if (!is.null(end))   df <- dplyr::filter(df, .data$year <= end)
  df
}

#' Indicator API → tibble(iso2, year, value).
owid_fetch_indicator <- function(variable_id, start = NULL, end = NULL) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  stopifnot(length(variable_id) == 1L, !is.na(variable_id))
  base <- paste0("https://api.ourworldindata.org/v1/indicators/", as.integer(variable_id))
  data <- owid_fetch_json(paste0(base, ".data.json"))
  meta <- owid_fetch_json(paste0(base, ".metadata.json"))

  ents <- meta$dimensions$entities$values
  if (is.null(ents) || !nrow(as.data.frame(ents))) {
    rlang::abort(sprintf("Indicator %s: no entity dimension in metadata", variable_id))
  }
  ent_tbl <- tibble::as_tibble(ents) |>
    dplyr::transmute(
      entity_id = as.integer(.data$id),
      code      = as.character(.data$code)
    )

  df <- tibble::tibble(
    entity_id = as.integer(data$entities),
    year      = as.integer(data$years),
    value     = as.numeric(data$values)
  ) |>
    dplyr::left_join(ent_tbl, by = "entity_id") |>
    dplyr::mutate(iso2 = owid_map_iso2(.data$code)) |>
    dplyr::filter(!is.na(.data$iso2), !is.na(.data$value)) |>
    dplyr::select("iso2", "year", "value")

  owid_trim_years(df, start = start, end = end)
}

#' Chart CSV → wide tibble with iso2 + year + data columns.
owid_fetch_csv_wide <- function(parsed, start = NULL, end = NULL) {
  requireNamespace("readr", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  url <- owid_csv_url(parsed)
  txt <- owid_fetch_raw(url, as = "text")
  df <- readr::read_csv(I(txt), show_col_types = FALSE, progress = FALSE)

  nm <- names(df)
  code_col <- intersect(c("code", "Code"), nm)[1]
  year_col <- intersect(c("year", "Year", "day", "Day", "date", "Date"), nm)[1]
  if (is.na(code_col) || is.na(year_col)) {
    rlang::abort(paste0(
      "OWID CSV missing code/year columns. Got: ", paste(nm, collapse = ", ")
    ))
  }

  data_cols <- setdiff(nm, c(
    "entity", "Entity", "entities", code_col, year_col
  ))
  if (!length(data_cols)) {
    rlang::abort("OWID CSV has no data columns.")
  }

  out <- df |>
    dplyr::mutate(
      iso2 = owid_map_iso2(.data[[code_col]]),
      year = suppressWarnings(as.integer(.data[[year_col]]))
    ) |>
    dplyr::filter(!is.na(.data$iso2)) |>
    dplyr::select("iso2", "year", dplyr::all_of(data_cols))

  # Daily series: keep date if year parse failed but date column exists
  if (all(is.na(out$year)) && year_col %in% c("day", "Day", "date", "Date")) {
    out <- df |>
      dplyr::mutate(
        iso2 = owid_map_iso2(.data[[code_col]]),
        date = as.Date(.data[[year_col]])
      ) |>
      dplyr::filter(!is.na(.data$iso2), !is.na(.data$date)) |>
      dplyr::select("iso2", "date", dplyr::all_of(data_cols))
    return(out)
  }

  owid_trim_years(out, start = start, end = end)
}

#' Fetch chart data (CSV, fallback Indicator API for requested columns).
#'
#' @param parsed From `owid_parse_code()`.
#' @param columns Character vector of shortNames to keep; NULL = all CSV cols.
#' @return Tibble with iso2, year (or date), and value columns.
owid_fetch_chart_data <- function(parsed, start = NULL, end = NULL, columns = NULL) {
  requireNamespace("dplyr", quietly = TRUE)

  if (identical(parsed$kind, "indicator")) {
    df <- owid_fetch_indicator(parsed$variable_id, start = start, end = end)
    col_name <- if (!is.null(parsed$column) && !is.na(parsed$column) && nzchar(parsed$column)) {
      parsed$column
    } else {
      "value"
    }
    return(dplyr::rename(df, !!col_name := "value"))
  }

  meta <- owid_fetch_metadata(parsed)
  if (is.null(columns)) {
    columns <- owid_metadata_columns(meta)
  }
  columns <- unique(as.character(columns))
  columns <- columns[nzchar(columns)]

  wide <- tryCatch(
    owid_fetch_csv_wide(parsed, start = start, end = end),
    error = function(e) e
  )

  if (!inherits(wide, "error")) {
    miss <- setdiff(columns, names(wide))
    if (!length(miss)) {
      keep <- intersect(c("iso2", "year", "date", columns), names(wide))
      return(dplyr::select(wide, dplyr::all_of(keep)))
    }
    # Partial miss: fall through to indicator for missing cols after keeping CSV
  }

  # Fallback: Indicator API per column via owidVariableId
  parts <- list()
  time_key <- "year"
  for (col in columns) {
    vid <- owid_column_variable_id(meta, col)
    if (is.na(vid)) {
      rlang::abort(sprintf(
        "OWID CSV failed and no owidVariableId for column '%s' (%s)",
        col,
        if (inherits(wide, "error")) wide$message else "column missing from CSV"
      ))
    }
    ind <- owid_fetch_indicator(vid, start = start, end = end) |>
      dplyr::rename(!!col := "value")
    parts[[col]] <- ind
  }
  out <- parts[[1]]
  if (length(parts) > 1L) {
    for (j in seq_along(parts)[-1]) {
      out <- dplyr::full_join(out, parts[[j]], by = c("iso2", time_key))
    }
  }
  out
}

##### Public tool --------------------------------------------------------------

#' Fetch one OWID series as tibble(iso2, year, value) [or iso2, date, value].
#'
#' @param code   `retrieve_code` (chart/explorer path or indicator/id).
#' @param start  Optional inclusive start year.
#' @param end    Optional inclusive end year.
#' @return Tibble with iso2, year (or date), value.
owidTool <- function(code, start = NULL, end = NULL) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)

  parsed <- owid_parse_code(code)

  if (identical(parsed$kind, "indicator")) {
    return(owid_fetch_indicator(parsed$variable_id, start = start, end = end))
  }

  meta <- owid_fetch_metadata(parsed)
  col <- owid_resolve_column(meta, parsed$column)

  wide <- tryCatch(
    owid_fetch_chart_data(parsed, start = start, end = end, columns = col),
    error = function(e) e
  )
  if (inherits(wide, "error")) {
    rlang::abort(wide$message)
  }

  if ("date" %in% names(wide) && !"year" %in% names(wide)) {
    return(
      wide |>
        dplyr::transmute(
          iso2  = .data$iso2,
          date  = .data$date,
          value = as.numeric(.data[[col]])
        ) |>
        dplyr::filter(!is.na(.data$value))
    )
  }

  wide |>
    dplyr::transmute(
      iso2  = .data$iso2,
      year  = as.integer(.data$year),
      value = as.numeric(.data[[col]])
    ) |>
    dplyr::filter(!is.na(.data$iso2), !is.na(.data$value))
}

# owidTool("explorers/minerals?Mineral=Silver&Metric=Production&Type=Mine&Share+of+global=false", start = 1980)
# owidTool("grapher/median-and-mean-income-after-tax-lis#mean__welfare_type_dhi__equivalence_scale_square_root__period_year")
# owidTool("indicator/1131155", start = 1980, end = 2024)
