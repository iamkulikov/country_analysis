####### Function to get country's codes and the codes of its peers

assert_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    rlang::abort(
      paste0("Missing required packages: ", paste(missing, collapse = ", "), ".")
    )
  }
}

normalize_iso3_strict <- function(x, keep_unknown = TRUE) {
  assert_packages(c("stringr", "countrycode"))
  
  x0 <- x |> as.character() |> stringr::str_trim() |> stringr::str_to_upper()
  x0 <- dplyr::if_else(stringr::str_detect(x0, "^[A-Z]{3}$"), x0, NA_character_)
  
  iso_ok <- x0 %in% countrycode::codelist$iso3c
  
  if (isTRUE(keep_unknown)) {
    # Keep 3-letter codes even if not in ISO3166-1 (e.g., CHI for Channel Islands)
    return(x0)
  }
  
  dplyr::if_else(iso_ok, x0, NA_character_)
}

safe_iso3_to_iso2 <- function(iso3) {
  iso3 <- normalize_iso3_strict(iso3)
  iso3 <- iso3[!is.na(iso3) & iso3 != ""]
  
  if (length(iso3) == 0) {
    return(character(0))
  }
  
  iso2 <- countrycode::countrycode(iso3, "iso3c", "iso2c", warn = FALSE)
  iso2[iso3 == "XKX"] <- "XK"
  
  iso2
}

coerce_01 <- function(x) {
  # robust 0/1 coercion for a vector
  x_chr <- stringr::str_trim(as.character(x))
  out <- dplyr::case_when(
    x_chr %in% c("TRUE", "T", "true", "Yes", "YES") ~ 1,
    x_chr %in% c("FALSE", "F", "false", "No", "NO") ~ 0,
    TRUE ~ suppressWarnings(as.numeric(x_chr))
  )
  out
}

getPeersCodes <- function(country_iso3c, peers_fname) {
  
  assert_packages(c("readxl", "dplyr", "stringr", "countrycode", "tibble", "rlang"))
  
  country_iso3c <- normalize_iso3_strict(country_iso3c %||% "")
  if (!stringr::str_detect(country_iso3c, "^[A-Z]{3}$")) {
    rlang::abort("country_iso3c must be a 3-letter ISO3 code, e.g. 'RUS'.")
  }
  
  raw <- readxl::read_excel(
    path = peers_fname,
    sheet = "groups",
    col_names = FALSE
  )
  
  if (nrow(raw) < 5 || ncol(raw) < 5) {
    rlang::abort("Sheet 'groups' looks too small / malformed in peers file.")
  }
  
  # Detect boundary row: "country | region | country_code" in first 3 columns
  a <- stringr::str_to_lower(stringr::str_trim(as.character(raw[[1]])))
  b <- stringr::str_to_lower(stringr::str_trim(as.character(raw[[2]])))
  c <- stringr::str_to_lower(stringr::str_trim(as.character(raw[[3]])))
  
  boundary_candidates <- which(a == "country" & b == "region" & c == "country_code")
  boundary_row <- boundary_candidates[1]
  
  if (length(boundary_row) == 0 || is.na(boundary_row)) {
    rlang::abort(
      "Cannot detect peers-matrix boundary row. Expected a row with: country | region | country_code."
    )
  }
  if (boundary_row <= 3) {
    rlang::abort("Detected boundary row too early; file structure looks inconsistent.")
  }
  
  groups_block <- raw[seq_len(boundary_row - 1), , drop = FALSE]
  peers_block  <- raw[boundary_row:nrow(raw), , drop = FALSE]
  
  # --- Parse groups block ---------------------------------------------
  
  country_names <- as.character(groups_block[1, 4:ncol(groups_block), drop = TRUE])
  
  group_codes <- as.character(groups_block[3:(boundary_row - 1), 3, drop = TRUE]) |>
    stringr::str_trim()
  
  group_mat <- groups_block[3:(boundary_row - 1), 4:ncol(groups_block), drop = FALSE] |>
    as.data.frame()
  
  group_mat[] <- lapply(group_mat, function(x) {
    x_chr <- stringr::str_trim(as.character(x))
    dplyr::case_when(
      is.na(x_chr) ~ 0,
      x_chr %in% c("1", "TRUE", "T", "true", "Yes", "YES") ~ 1,
      TRUE ~ suppressWarnings(as.numeric(x_chr)) %||% 0
    )
  })
  
  peers_header_iso3 <- as.character(peers_block[1, 4:ncol(peers_block), drop = TRUE]) |>
    normalize_iso3_strict()
  
  if (length(country_names) != length(peers_header_iso3)) {
    rlang::abort(
      "Mismatch: number of countries in group header block != number of ISO3 columns in peers matrix."
    )
  }
  
  group_by_country <- t(as.matrix(group_mat))
  colnames(group_by_country) <- group_codes
  
  regions_tbl <- tibble::tibble(
    country_name  = country_names,
    country_iso3c = peers_header_iso3
  ) |>
    dplyr::bind_cols(tibble::as_tibble(group_by_country)) |>
    dplyr::mutate(country_iso2c = safe_iso3_to_iso2(.data$country_iso3c))
  
  if (!(country_iso3c %in% regions_tbl$country_iso3c)) {
    rlang::abort(paste0(
      "country_iso3c='", country_iso3c, "' not found in groups header block (top part of sheet)."
    ))
  }
  
  # --- Parse peers matrix block ---------------------------------------
  
  peers_df <- peers_block |>
    (\(x) {
      header <- as.character(x[1, , drop = TRUE])
      dat <- x[-1, , drop = FALSE]
      names(dat) <- header
      dat
    })() |>
    dplyr::mutate(
      country_code = normalize_iso3_strict(.data$country_code),
      region = as.character(.data$region),
      country = as.character(.data$country)
    )
  
  # Region from peers matrix (reliable)
  region_map <- peers_df |>
    dplyr::select(country_code, region) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$country_code), .data$country_code != "")
  
  country_region <- region_map |>
    dplyr::filter(.data$country_code == country_iso3c) |>
    dplyr::pull(.data$region) |>
    (\(x) x[1])()
  
  if (is.na(country_region) || !nzchar(country_region)) {
    rlang::abort(paste0(
      "Region not found for country_iso3c='", country_iso3c, "' in peers matrix block."
    ))
  }
  
  country_rows <- peers_df |>
    dplyr::filter(.data$country_code == country_iso3c)
  
  if (nrow(country_rows) == 0) {
    rlang::abort(paste0(
      "country_iso3c='", country_iso3c, "' not found in peers matrix (country_code column)."
    ))
  }
  if (nrow(country_rows) > 1) {
    rlang::abort(paste0(
      "country_iso3c='", country_iso3c, "' appears multiple times in peers matrix. Fix duplicates in peers file."
    ))
  }
  
  iso3_cols <- peers_header_iso3
  missing_iso3_cols <- setdiff(iso3_cols, names(peers_df))
  if (length(missing_iso3_cols) > 0) {
    rlang::abort("Peers matrix columns mismatch: some ISO3 columns are missing in peers matrix data.")
  }
  
  # --- FIXED: default peers extraction via POSITION (not names after unlist)
  row_vals <- country_rows |>
    dplyr::select(dplyr::all_of(iso3_cols)) |>
    as.data.frame()
  
  row_num <- coerce_01(as.vector(row_vals[1, , drop = TRUE]))
  idx_ones <- which(!is.na(row_num) & row_num == 1)
  
  peers_default_iso3c <- iso3_cols[idx_ones]
  peers_default_iso3c <- normalize_iso3_strict(peers_default_iso3c)
  peers_default_iso3c <- peers_default_iso3c[!is.na(peers_default_iso3c) & peers_default_iso3c != ""]
  peers_default_iso3c <- setdiff(peers_default_iso3c, country_iso3c)
  
  peers_default_iso2c <- safe_iso3_to_iso2(peers_default_iso3c)
  
  # Neighbours peers: same region from peers matrix
  peers_neighbours_iso3c <- peers_df |>
    dplyr::filter(.data$region == country_region) |>
    dplyr::pull(.data$country_code) |>
    normalize_iso3_strict()
  
  peers_neighbours_iso3c <- peers_neighbours_iso3c[!is.na(peers_neighbours_iso3c) & peers_neighbours_iso3c != ""]
  peers_neighbours_iso3c <- setdiff(peers_neighbours_iso3c, country_iso3c)
  peers_neighbours_iso2c <- safe_iso3_to_iso2(peers_neighbours_iso3c)
  
  regions_out <- regions_tbl |>
    dplyr::select(-c(country_name)) |>
    dplyr::relocate(.data$country_iso3c, .data$country_iso2c, .after = dplyr::last_col())
  
  country_iso2c <- safe_iso3_to_iso2(country_iso3c)
  
  list(
    country_iso2c = country_iso2c,
    country_iso3c = country_iso3c,
    peers_default_iso2c = peers_default_iso2c,
    peers_default_iso3c = peers_default_iso3c,
    peers_neighbours_iso2c = peers_neighbours_iso2c,
    peers_neighbours_iso3c = peers_neighbours_iso3c,
    regions = regions_out
  )
}


####### Function to get plot schedule and generate sources

#' Read graph plan and attach per-graph (row) aggregated sources
#' - Frequency-aware when both dict has source_frequency and plan has data_frequency
#' - Deduplicates sources by *tokens* (splits dict$source_name into separate sources)
#' - Warns about unmatched indicators for ACTIVE rows only
#' - Deterministic ordering of sources
getPlotSchedule <- function(plotparam_fname, dict, warn_unmatched = TRUE) {
  assert_packages(c("readxl", "dplyr", "tidyr", "stringr", "tibble", "rlang", "purrr"))
  
  # ---- Validate inputs -------------------------------------------------
  needed_dict_cols <- c("indicator_code", "source_name")
  missing_dict_cols <- setdiff(needed_dict_cols, names(dict))
  if (length(missing_dict_cols) > 0) {
    rlang::abort(paste0(
      "getPlotSchedule: dict is missing required columns: ",
      paste(missing_dict_cols, collapse = ", "),
      "."
    ))
  }
  
  has_dict_freq <- "source_frequency" %in% names(dict)
  
  # ---- Read plot params ------------------------------------------------
  graphplan <- readxl::read_excel(plotparam_fname, sheet = "library", col_names = TRUE, skip = 1)
  
  if (!("indicators" %in% names(graphplan))) {
    rlang::abort("getPlotSchedule: sheet 'library' must contain column 'indicators'.")
  }
  
  has_plan_freq <- "data_frequency" %in% names(graphplan)
  
  # ---- Helpers ---------------------------------------------------------
  norm_chr <- function(x) {
    x <- as.character(x)
    stringr::str_squish(stringr::str_trim(x))
  }
  
  norm_freq <- function(x) {
    x <- norm_chr(x)
    x <- stringr::str_to_lower(x)
    x <- dplyr::na_if(x, "")
    ifelse(x %in% c("y", "q", "m", "d"), x, NA_character_)
  }
  
  tokenize_sources <- function(x) {
    x <- norm_chr(x)
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) return(character(0))
    
    x |>
      stringr::str_split("\\s*[,;|/]\\s*") |>
      purrr::list_c() |>
      norm_chr() |>
      (\(z) z[!is.na(z) & z != ""])()
  }
  
  # ---- Dict: tokenized sources ----------------------------------------
  dict_sources <- tibble::as_tibble(dict) |>
    dplyr::transmute(
      indicator_code   = norm_chr(.data$indicator_code),
      source_name_raw  = as.character(.data$source_name),
      source_frequency = if (has_dict_freq) norm_freq(.data$source_frequency) else NA_character_
    ) |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "")
  
  dict_sources_long <- dict_sources |>
    dplyr::mutate(source_token = purrr::map(.data$source_name_raw, tokenize_sources)) |>
    tidyr::unnest(.data$source_token) |>
    dplyr::mutate(source_token = norm_chr(.data$source_token)) |>
    dplyr::filter(!is.na(.data$source_token), .data$source_token != "") |>
    dplyr::select(.data$indicator_code, .data$source_frequency, .data$source_token) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$indicator_code, .data$source_frequency, .data$source_token)
  
  # Keys available in dict (for unmatched checks)
  dict_keys <- if (has_dict_freq) {
    dict_sources_long |>
      dplyr::distinct(.data$indicator_code, .data$source_frequency) |>
      dplyr::transmute(
        indicator_code  = .data$indicator_code,
        data_frequency  = .data$source_frequency
      )
  } else {
    dict_sources_long |>
      dplyr::distinct(.data$indicator_code) |>
      dplyr::transmute(indicator_code = .data$indicator_code)
  }
  
  # ---- Parse indicators safely ----------------------------------------
  long_inds <- graphplan |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::mutate(
      data_frequency = if (has_plan_freq) norm_freq(.data$data_frequency) else NA_character_,
      indicators_vec = stringr::str_split(as.character(.data$indicators), ",\\s*")
    ) |>
    tidyr::unnest_longer(.data$indicators_vec, values_to = "indicator_code") |>
    dplyr::mutate(indicator_code = norm_chr(.data$indicator_code)) |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "") |>
    dplyr::select(.data$.row_id, dplyr::any_of("graph_name"), dplyr::any_of("active"),
                  .data$data_frequency, .data$indicator_code)
  
  # ---- Unmatched warnings (ACTIVE only), robust via anti_join ----------
  if (isTRUE(warn_unmatched) && nrow(long_inds) > 0) {
    
    active_rows <- graphplan |>
      dplyr::mutate(.row_id = dplyr::row_number()) |>
      dplyr::mutate(
        active_flag = dplyr::case_when(
          "active" %in% names(graphplan) ~ (.data$active == 1),
          TRUE ~ TRUE
        )
      ) |>
      dplyr::filter(.data$active_flag) |>
      dplyr::pull(.data$.row_id)
    
    active_long <- long_inds |>
      dplyr::filter(.data$.row_id %in% active_rows)
    
    if (nrow(active_long) > 0) {
      
      unmatched_tbl <- if (has_dict_freq && has_plan_freq) {
        # frequency-aware unmatched: indicator_code + data_frequency
        active_long |>
          dplyr::select(.data$.row_id, .data$indicator_code, .data$data_frequency) |>
          dplyr::anti_join(dict_keys, by = dplyr::join_by(indicator_code, data_frequency)) |>
          dplyr::summarise(unmatched = list(unique(.data$indicator_code)), .by = .data$.row_id) |>
          dplyr::filter(lengths(.data$unmatched) > 0)
      } else {
        # frequency-agnostic unmatched: indicator_code only
        active_long |>
          dplyr::select(.data$.row_id, .data$indicator_code) |>
          dplyr::anti_join(dict_keys, by = dplyr::join_by(indicator_code)) |>
          dplyr::summarise(unmatched = list(unique(.data$indicator_code)), .by = .data$.row_id) |>
          dplyr::filter(lengths(.data$unmatched) > 0)
      }
      
      if (nrow(unmatched_tbl) > 0) {
        graph_names <- graphplan |>
          dplyr::mutate(.row_id = dplyr::row_number()) |>
          dplyr::select(.data$.row_id, dplyr::any_of("graph_name"))
        
        unmatched_msg <- unmatched_tbl |>
          dplyr::left_join(graph_names, by = dplyr::join_by(.row_id)) |>
          dplyr::mutate(
            graph_name = dplyr::coalesce(as.character(.data$graph_name), paste0("row_", .data$.row_id)),
            unmatched_str = vapply(.data$unmatched, \(x) paste(x, collapse = ", "), character(1))
          ) |>
          dplyr::transmute(line = paste0("- ", .data$graph_name, ": ", .data$unmatched_str)) |>
          dplyr::pull(.data$line)
        
        rlang::warn(paste(
          "Some indicators were not found in dict (active graphs only):",
          paste(unmatched_msg, collapse = "\n"),
          sep = "\n"
        ))
      }
    }
  }
  
  # ---- Join indicators to tokenized sources ----------------------------
  long_with_sources <- if (has_dict_freq && has_plan_freq) {
    long_inds |>
      dplyr::left_join(
        dict_sources_long,
        by = dplyr::join_by(indicator_code == indicator_code,
                            data_frequency == source_frequency),
        relationship = "many-to-many"
      )
  } else {
    long_inds |>
      dplyr::left_join(
        dict_sources_long |>
          dplyr::select(.data$indicator_code, .data$source_token) |>
          dplyr::distinct(),
        by = dplyr::join_by(indicator_code == indicator_code),
        relationship = "many-to-many"
      )
  }
  
  # ---- Aggregate sources per row --------------------------------------
  sources_by_row <- long_with_sources |>
    dplyr::summarise(
      source_name = {
        s <- .data$source_token
        s <- s[!is.na(s)]
        s <- norm_chr(s)
        s <- s[s != ""]
        s <- unique(s)
        
        s <- unique(c(s, "расчеты АКРА"))
        
        paste(s, collapse = ", ")
      },
      .by = .data$.row_id
    )
  
  graphplan_out <- graphplan |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::left_join(sources_by_row, by = dplyr::join_by(.row_id)) |>
    dplyr::mutate(source_name = dplyr::coalesce(.data$source_name, "расчеты АКРА")) |>
    dplyr::select(-.data$.row_id)
  
  graphplan_out
}

# getPlotSchedule(plotparam_fname = "C:/Projects/country_analysis/assets/Belarus/Auto_report/2_graphlib.xlsx", 
#                 dict = D$dict)


####### Function to generate data-independent graph parameters

parseGraphPlan <- function(graphrow, dict, horizontal_size, vertical_size) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphrow) || !is.data.frame(graphrow) || nrow(graphrow) < 1) {
    rlang::warn("parseGraphPlan: graphrow must be a 1-row data.frame/tibble.")
    return(NULL)
  }
  
  row <- tibble::as_tibble(graphrow)[1, , drop = FALSE]
  
  # ---- Helpers (scalar getters) ----------------------------------------
  get_chr <- function(col, default = NA_character_) {
    if (!(col %in% names(row))) return(default)
    v <- as.character(row[[col]][[1]])
    v <- stringr::str_trim(v)
    v <- dplyr::na_if(v, "")
    dplyr::coalesce(v, default)
  }
  
  get_int01 <- function(col, default = 0L) {
    # binary-ish: allow 0/1 numeric/logical/character; NA -> default
    if (!(col %in% names(row))) return(default)
    v <- row[[col]][[1]]
    if (is.na(v)) return(default)
    if (is.logical(v)) return(as.integer(isTRUE(v)))
    x <- suppressWarnings(as.numeric(as.character(v)))
    if (is.na(x)) return(default)
    as.integer(x != 0)
  }
  
  get_num_or_na <- function(col) {
    if (!(col %in% names(row))) return(NA_real_)
    v <- row[[col]][[1]]
    if (is.na(v)) return(NA_real_)
    v_chr <- stringr::str_trim(as.character(v))
    v_chr <- dplyr::na_if(v_chr, "")
    if (is.na(v_chr)) return(NA_real_)
    suppressWarnings(as.numeric(v_chr))
  }
  
  get_raw <- function(col) {
    if (!(col %in% names(row))) return(NA)
    row[[col]][[1]]
  }
  
  # ---- Pull fields with defaults ---------------------------------------
  graph_name      <- get_chr("graph_name", default = NA_character_)
  graph_title     <- get_chr("graph_title", default = "")
  graph_type      <- get_chr("graph_type",  default = NA_character_)
  graph_group     <- get_chr("graph_group", default = "other")
  data_frequency  <- get_chr("data_frequency", default = "y")
  peers           <- get_chr("peers", default = "default")
  theme_name      <- get_chr("theme", default = "ipsum")
  orientation     <- get_chr("orientation", default = "horizontal")
  source_name     <- get_chr("source_name", default = NA_character_)
  trend_type      <- get_chr("trend_type", default = NA_character_)
  
  # binaries
  all         <- get_int01("all", 0L)
  x_log       <- get_int01("x_log", 0L)
  y_log       <- get_int01("y_log", 0L)
  index       <- get_int01("index", 0L)
  recession   <- get_int01("recession", 0L)
  swap_axis   <- get_int01("swap_axis", 0L)
  long_legend <- get_int01("long_legend", 0L)
  vert_lab    <- get_int01("vert_lab", 0L)
  short_names <- get_int01("short_names", 0L)
  show_title  <- get_int01("show_title", 0L)
  active      <- get_int01("active", 0L)
  
  # bounds/time fields (keep raw strings; convert later as needed)
  time_fix <- get_chr("time_fix", default = NA_character_)
  x_min    <- get_chr("x_min", default = NA_character_)
  x_max    <- get_chr("x_max", default = NA_character_)
  y_min    <- get_num_or_na("y_min")
  y_max    <- get_num_or_na("y_max")
  
  sec_y_axis <- get_chr("sec_y_axis", default = NA_character_)
  
  # ---- Convert dates for intended frequency ----------------------------
  cross_types <- c(
    "scatter_country_comparison", "scatter_before_after", "structure_country_comparison",
    "structure_country_comparison_norm", "bar_country_comparison",
    "bar_country_comparison_norm", "bar_year_comparison",
    "distribution_year_comparison", "scatter_dynamic", "density_fix"
  )
  
  dynamic_types <- c(
    "structure_dynamic", "bar_dynamic", "lines_country_comparison",
    "lines_indicator_comparison", "distribution_dynamic"
  )
  
  if (!is.na(graph_type) && graph_type %in% cross_types && !is.na(time_fix)) {
    time_fix <- prepareDates(time_fix, freq = data_frequency, end = 1)
  }
  
  if (!is.na(graph_type) && graph_type %in% dynamic_types) {
    if (!is.na(x_min)) x_min <- prepareDates(x_min, freq = data_frequency, end = 0)
    if (!is.na(x_max)) x_max <- prepareDates(x_max, freq = data_frequency, end = 1)
  }
  
  # ---- Parse indicators (split + trim, no ", " dependency) -------------
  indicators_raw <- get_chr("indicators", default = NA_character_)
  indicators <- if (is.na(indicators_raw)) {
    character(0)
  } else {
    indicators_raw |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim() |>
      (\(x) x[!is.na(x) & x != ""])()
  }
  
  x_ind <- if (length(indicators) >= 1) indicators[[1]] else NA_character_
  y_ind <- if (length(indicators) >= 2) indicators[[2]] else NA_character_
  
  # ---- Secondary axis parsing: "ind1, ind2, coeff" ---------------------
  indicators_sec <- NA_character_
  coeff <- NA_real_
  
  if (!is.na(sec_y_axis)) {
    parts <- sec_y_axis |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim() |>
      (\(x) x[!is.na(x) & x != ""])()
    
    if (length(parts) >= 2) {
      coeff <- suppressWarnings(as.numeric(tail(parts, 1)))
      indicators_sec <- head(parts, -1)
      if (length(indicators_sec) == 0) indicators_sec <- NA_character_
    } else {
      # bad format -> keep as NA; downstream can decide how to handle
      indicators_sec <- NA_character_
      coeff <- NA_real_
    }
  }
  
  # ---- x limits numeric for specific types -----------------------------
  if (!is.na(graph_type) && graph_type %in% c("scatter_country_comparison", "density_fix")) {
    x_min <- suppressWarnings(as.numeric(x_min))
    x_max <- suppressWarnings(as.numeric(x_max))
  }
  
  # ---- Theme & labels ---------------------------------------------------
  title <- if (isTRUE(show_title == 1L)) graph_title else ""
  caption <- if (is.na(source_name)) "Источники:" else paste("Источники:", source_name)
  
  # Prefer returning a theme function (no eval/parse later)
  theme_name <- get_chr("theme", default = "ipsum")
  
  if (identical(tolower(orientation), "vertical")) {
    width <- vertical_size[[1]]
    height <- vertical_size[[2]]
  } else {
    width <- horizontal_size[[1]]
    height <- horizontal_size[[2]]
  }
  
  # ---- Special case -----------------------------------------------------
  if (identical(graph_type, "distribution_dynamic")) {
    peers <- "0"
  }
  
  list(indicators = indicators, graph_type = graph_type, x_ind = x_ind, y_ind = y_ind, x_min = x_min,
    y_min = y_min, x_max = x_max, y_max = y_max, data_frequency = data_frequency, trend_type = trend_type,
    graph_name = graph_name, time_fix = time_fix, indicators_sec = indicators_sec, peers = peers,
    caption = caption, title = title, theme_name = theme_name, all = all, show_title = show_title,
    x_log = x_log, y_log = y_log, width = width, height = height, sec_y_axis = sec_y_axis,
    coeff = coeff, index = index, active = active)
}

####### Functions to decode theme based on the plan

resolve_theme <- function(theme_name) {
  assert_packages(c("rlang", "stringr", "ggplot2"))
  
  # Coerce to a single scalar string safely
  name <- theme_name |>
    as.character() |>
    (\(x) if (length(x) >= 1) x[[1]] else NA_character_)() |>
    stringr::str_trim() |>
    stringr::str_to_lower()
  
  if (is.na(name) || !nzchar(name)) {
    name <- "default"
  }
  
  switch(
    name,
    "ipsum" = {
      if (!requireNamespace("hrbrthemes", quietly = TRUE)) {
        rlang::warn("theme 'ipsum' requires hrbrthemes; using theme_grey() instead.")
        ggplot2::theme_grey()
      } else {
        hrbrthemes::theme_ipsum()
      }
    },
    "minimal" = ggplot2::theme_minimal(),
    "classic" = ggplot2::theme_classic(),
    "bw"      = ggplot2::theme_bw(),
    "grey"    = ggplot2::theme_grey(),
    ggplot2::theme_grey()
  )
}

normalize_theme_name <- function(x) {
  assert_packages(c("stringr"))
  x <- stringr::str_trim(as.character(x))
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  
  # Backward compat: "theme_ipsum()" -> "ipsum"
  x <- stringr::str_replace(x, "^theme_", "")
  x <- stringr::str_replace(x, "\\(\\)\\s*$", "")
  x
}

####### Functions to recode dates based on the graph intended frequency

numberOfDays <- function(date) {
  if (!inherits(date, "Date")) {
    rlang::warn("number_of_days: coercing 'date' to Date.")
    date <- as.Date(date)
  }
  
  # NA-safe, fully vectorized
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  
  # first day of next month (vectorized!)
  first_next_month <- as.Date(sprintf(
    "%04d-%02d-01",
    ifelse(m == 12, y + 1L, y),
    ifelse(m == 12, 1L, m + 1L)
  ))
  
  as.integer(format(first_next_month - 1L, "%d"))
}


prepareDates <- function(datetext, freq, end) {
  assert_packages(c("rlang", "stringr", "glue"))
  
  freq <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  if (length(freq) != 1 || is.na(freq) || !(freq %in% c("y", "q", "m", "d"))) {
    rlang::warn("prepareDates: 'freq' must be one of y/q/m/d. Returning input unchanged.")
    return(as.character(datetext))
  }
  
  end <- as.integer(end)
  end <- ifelse(is.na(end), 0L, end)
  end <- ifelse(end != 0L, 1L, 0L)
  
  x <- as.character(datetext)
  x <- stringr::str_trim(x)
  if (length(x) != 1 || is.na(x) || !nzchar(x)) return(x)
  
  tokens <- stringr::str_split(x, ",\\s*") |> purrr::pluck(1)
  tokens <- stringr::str_trim(tokens)
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) return("")
  
  pad2 <- function(n) sprintf("%02d", as.integer(n))
  pad4 <- function(n) sprintf("%04d", as.integer(n))
  
  month_last_day <- function(year, month) {
    numberOfDays(as.Date(sprintf("%04d-%02d-01", year, month)))
  }
  
  parse_one <- function(tok) {
    tok <- stringr::str_trim(tok)
    
    # daily: dd.mm.yyyy
    if (stringr::str_detect(tok, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$")) {
      d <- suppressWarnings(as.Date(tok, "%d.%m.%Y"))
      if (is.na(d)) return(list(ok = FALSE, tok = tok))
      y <- as.integer(format(d, "%Y"))
      m <- as.integer(format(d, "%m"))
      return(list(ok = TRUE, oldfreq = "d", year = y, sub = m, date = d, tok = tok))
    }
    
    tok2 <- stringr::str_to_lower(tok)
    y <- suppressWarnings(as.integer(stringr::str_extract(tok2, "^(19|20)\\d{2}")))
    if (is.na(y)) return(list(ok = FALSE, tok = tok))
    
    f <- stringr::str_extract(tok2, "[qm]")
    if (is.na(f)) return(list(ok = TRUE, oldfreq = "y", year = y, sub = NA_integer_, date = NA, tok = tok))
    
    n <- suppressWarnings(as.integer(stringr::str_extract(tok2, "(?<=q)\\d+$|(?<=m)\\d+$")))
    if (is.na(n)) return(list(ok = FALSE, tok = tok))
    
    if (f == "q" && !(n %in% 1:4)) return(list(ok = FALSE, tok = tok))
    if (f == "m" && !(n %in% 1:12)) return(list(ok = FALSE, tok = tok))
    
    list(ok = TRUE, oldfreq = f, year = y, sub = n, date = NA, tok = tok)
  }
  
  convert_one <- function(p) {
    if (!isTRUE(p$ok)) return(p$tok)
    
    oldfreq <- p$oldfreq
    y <- p$year
    
    # same freq: normalize output
    if (identical(oldfreq, freq)) {
      if (freq == "y") return(pad4(y))
      if (freq %in% c("q", "m")) return(glue::glue("{pad4(y)}{freq}{as.integer(p$sub)}"))
      return(format(p$date, "%d.%m.%Y"))
    }
    
    # conversions
    switch(
      oldfreq,
      
      "y" = switch(
        freq,
        "q" = glue::glue("{pad4(y)}q{ifelse(end == 0L, 1L, 4L)}"),
        "m" = glue::glue("{pad4(y)}m{ifelse(end == 0L, 1L, 12L)}"),
        "d" = if (end == 0L) glue::glue("01.01.{pad4(y)}") else glue::glue("31.12.{pad4(y)}"),
        pad4(y)
      ),
      
      "q" = {
        qn <- as.integer(p$sub)
        start_month <- (qn - 1L) * 3L + 1L
        end_month   <- (qn - 1L) * 3L + 3L
        
        switch(
          freq,
          "y" = pad4(y),
          "m" = glue::glue("{pad4(y)}m{ifelse(end == 0L, start_month, end_month)}"),
          "d" = {
            mm <- ifelse(end == 0L, start_month, end_month)
            dd <- ifelse(end == 0L, 1L, month_last_day(y, mm))
            glue::glue("{pad2(dd)}.{pad2(mm)}.{pad4(y)}")
          },
          p$tok
        )
      },
      
      "m" = {
        mm <- as.integer(p$sub)
        qq <- (mm - 1L) %/% 3L + 1L
        
        switch(
          freq,
          "y" = pad4(y),
          "q" = glue::glue("{pad4(y)}q{qq}"),
          "d" = {
            dd <- ifelse(end == 0L, 1L, month_last_day(y, mm))
            glue::glue("{pad2(dd)}.{pad2(mm)}.{pad4(y)}")
          },
          p$tok
        )
      },
      
      "d" = {
        d <- p$date
        mm <- as.integer(format(d, "%m"))
        qq <- (mm - 1L) %/% 3L + 1L
        
        switch(
          freq,
          "y" = pad4(y),
          "q" = glue::glue("{pad4(y)}q{qq}"),
          "m" = glue::glue("{pad4(y)}m{mm}"),
          p$tok
        )
      },
      
      p$tok
    )
  }
  
  parsed <- lapply(tokens, parse_one)
  ok_vec <- vapply(parsed, \(p) isTRUE(p$ok), logical(1))
  
  if (any(!ok_vec)) {
    bad <- vapply(parsed[!ok_vec], \(p) p$tok, character(1))
    rlang::warn(paste0(
      "prepareDates: some date tokens could not be parsed and were left unchanged: ",
      paste(unique(bad), collapse = ", ")
    ))
  }
  
  converted <- vapply(parsed, convert_one, character(1))
  paste(converted, collapse = ", ")
}


####### Function to fix peers for the particular graph

fixPeers <- function(country_info, params, data, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Helpers ---------------------------------------------------------
  warn <- function(msg) {
    if (isTRUE(warn_invalid)) rlang::warn(paste0("fixPeers: ", msg))
  }
  
  clean_iso2 <- function(x, self_iso2) {
    x <- as.character(x)
    x <- stringr::str_trim(x)
    x <- x[!is.na(x) & x != ""]
    x <- unique(x)
    x <- x[x != self_iso2]
    x
  }
  
  # ---- Basic guards ----------------------------------------------------
  self_iso2 <- country_info$country_iso2c %||% NA_character_
  self_iso2 <- stringr::str_trim(as.character(self_iso2))
  
  graph_type <- params$graph_type %||% NA_character_
  graph_type <- stringr::str_trim(as.character(graph_type))
  
  # Special case (as in your old code)
  if (identical(graph_type, "distribution_dynamic")) {
    if (!is.list(country_info) || is.null(country_info$regions) || !is.data.frame(country_info$regions)) {
      warn("country_info$regions is missing/malformed; cannot build peers for distribution_dynamic.")
      return(character(0))
    }
    if (!("country_iso2c" %in% names(country_info$regions))) {
      warn("country_info$regions has no column 'country_iso2c'; cannot build peers for distribution_dynamic.")
      return(character(0))
    }
    allc <- country_info$regions |> dplyr::pull(.data$country_iso2c)
    return(clean_iso2(allc, self_iso2))
  }
  
  # Normal flow: parse params$peers
  peers_raw <- params$peers %||% "default"
  peers_raw <- stringr::str_trim(as.character(peers_raw))
  peers_raw <- dplyr::na_if(peers_raw, "")
  peers_raw <- dplyr::coalesce(peers_raw, "default")
  
  # allow numeric 0 / "0"
  if (identical(peers_raw, "0") || suppressWarnings(!is.na(as.numeric(peers_raw)) && as.numeric(peers_raw) == 0)) {
    return(character(0))
  }
  
  # Split by first ":" (payload may contain commas)
  has_colon <- stringr::str_detect(peers_raw, ":")
  peers_type <- if (has_colon) {
    stringr::str_split_fixed(peers_raw, ":", n = 2)[, 1]
  } else {
    peers_raw
  }
  payload <- if (has_colon) {
    stringr::str_split_fixed(peers_raw, ":", n = 2)[, 2]
  } else {
    NA_character_
  }
  
  peers_type <- stringr::str_to_lower(stringr::str_trim(as.character(peers_type)))
  payload <- stringr::str_trim(as.character(payload))
  payload <- dplyr::na_if(payload, "")
  
  # ---- 1) Simple modes -------------------------------------------------
  if (peers_type %in% c("default", "neighbours")) {
    vec <- switch(
      peers_type,
      "default" = country_info$peers_default_iso2c,
      "neighbours" = country_info$peers_neighbours_iso2c,
      character(0)
    )
    return(clean_iso2(vec, self_iso2))
  }
  
  # "peers_type" equals some region group column name
  if (!is.null(country_info$regions) && is.data.frame(country_info$regions)) {
    reg_names <- names(country_info$regions)
    
    # map: original -> normalized (lower + trim)
    reg_names_norm <- reg_names |>
      stringr::str_trim() |>
      stringr::str_to_lower()
    
    hit <- which(reg_names_norm == peers_type)
    
    if (length(hit) >= 1) {
      peers_col <- reg_names[[hit[1]]]  # take first match, deterministic
      
      if (!("country_iso2c" %in% names(country_info$regions))) {
        warn("country_info$regions has no 'country_iso2c'; cannot use region groups.")
        return(character(0))
      }
      
      vec <- country_info$regions |>
        dplyr::filter(dplyr::coalesce(.data[[peers_col]], 0) == 1) |>
        dplyr::pull(.data$country_iso2c)
      
      return(clean_iso2(vec, self_iso2))
    }
  }
  
  # ---- 2) custom: ISO2, ISO2, ... -------------------------------------
  if (identical(peers_type, "custom")) {
    if (is.na(payload)) {
      warn("custom peers has empty payload.")
      return(character(0))
    }
    
    vec <- payload |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim()
    
    return(clean_iso2(vec, self_iso2))
  }
  
  # ---- 3) similar/top/low: ind, param, year ---------------------------
  if (peers_type %in% c("similar", "top", "low")) {
    if (is.null(data) || !is.list(data) || is.null(data$extdata_y) || !is.data.frame(data$extdata_y)) {
      warn("data$extdata_y is missing/malformed; cannot compute peers similar/top/low.")
      return(character(0))
    }
    
    if (is.na(payload)) {
      warn(paste0("peers='", peers_raw, "' has empty payload."))
      return(character(0))
    }
    
    parts <- payload |>
      stringr::str_split(",") |>
      purrr::pluck(1) |>
      stringr::str_trim() |>
      (\(x) x[!is.na(x) & x != ""])()
    
    peers_ind <- parts[[1]] %||% NA_character_
    p2 <- parts[[2]] %||% NA_character_
    p3 <- parts[[3]] %||% NA_character_
    
    peers_ind <- stringr::str_trim(as.character(peers_ind))
    peers_ind <- dplyr::na_if(peers_ind, "")
    
    if (is.na(peers_ind)) {
      warn(paste0("Cannot parse indicator in peers='", peers_raw, "'."))
      return(character(0))
    }
    
    # validate required columns
    need_cols <- c("year", "country_id", peers_ind)
    miss <- setdiff(need_cols, names(data$extdata_y))
    if (length(miss) > 0) {
      warn(paste0("extdata_y is missing columns: ", paste(miss, collapse = ", "), "."))
      return(character(0))
    }
    
    peers_year <- suppressWarnings(as.integer(p3))
    if (is.na(peers_year)) {
      warn(paste0("Cannot parse year in peers='", peers_raw, "'."))
      return(character(0))
    }
    
    df <- tibble::as_tibble(data$extdata_y) |>
      dplyr::filter(.data$year == peers_year)
    
    if (nrow(df) == 0) {
      warn(paste0("No data in extdata_y for year=", peers_year, "."))
      return(character(0))
    }
    
    if (identical(peers_type, "similar")) {
      band <- suppressWarnings(as.numeric(p2))
      if (is.na(band) || band <= 0 || band > 1) {
        warn(paste0("Bad band in peers='", peers_raw, "'; expected (0,1]."))
        return(character(0))
      }
      
      central <- df |>
        dplyr::filter(.data$country_id == self_iso2) |>
        dplyr::pull(.data[[peers_ind]])
      
      central <- central[[1]] %||% NA_real_
      central <- suppressWarnings(as.numeric(central))
      
      if (is.na(central)) {
        warn(paste0("Central value is NA for indicator=", peers_ind, ", year=", peers_year, "."))
        return(character(0))
      }
      
      vec <- df |>
        dplyr::filter(
          !is.na(.data[[peers_ind]]),
          .data[[peers_ind]] > central * (1 - band),
          .data[[peers_ind]] < central * (1 + band)
        ) |>
        dplyr::pull(.data$country_id)
      
      return(clean_iso2(vec, self_iso2))
    }
    
    # top/low
    n_take <- suppressWarnings(as.integer(p2))
    if (is.na(n_take) || n_take <= 0) {
      warn(paste0("Bad n in peers='", peers_raw, "'; expected positive integer."))
      return(character(0))
    }
    
    df2 <- df |>
      dplyr::filter(!is.na(.data[[peers_ind]]))
    
    if (nrow(df2) == 0) return(character(0))
    
    df2 <- if (identical(peers_type, "top")) {
      df2 |> dplyr::arrange(dplyr::desc(.data[[peers_ind]]))
    } else {
      df2 |> dplyr::arrange(.data[[peers_ind]])
    }
    
    vec <- df2 |>
      dplyr::slice_head(n = n_take) |>
      dplyr::pull(.data$country_id)
    
    return(clean_iso2(vec, self_iso2))
  }
  
  # ---- Unknown peers type ----------------------------------------------
  warn(paste0("Unknown peers type in peers='", peers_raw, "'. Returning empty peers."))
  character(0)
}


####### Functions to fill data-dependent graph parameters

# ------------------------------------------------------------------------------
# fillGraphPlan() v1 (systematic, deterministic, no eval/parse)
# - Computes ONLY what must be data-dependent
# - Never mutates x_ind/y_ind into strings like "log10(...)"
# - Avoids overwriting x_min/x_max axis limits for cross-section graphs
# - Produces safe scalar labels from dict (no dict[,1])
# - Keeps backward-compat fields expected by legacy plot functions
# ------------------------------------------------------------------------------

assert_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    rlang::abort(paste0("Missing required packages: ", paste(missing, collapse = ", "), "."))
  }
}

warn_active <- function(params, msg, warn_invalid = TRUE) {
  if (!isTRUE(warn_invalid)) return(invisible(NULL))
  active <- as.integer(params$active %||% 0L)
  if (active == 1L) rlang::warn(paste0("fillGraphPlan: ", msg))
  invisible(NULL)
}

normalize_freq <- function(x) {
  x <- as.character(x %||% NA_character_)
  x <- if (length(x) >= 1) x[[1]] else NA_character_
  x <- stringr::str_to_lower(stringr::str_trim(x))
  if (is.na(x) || !(x %in% c("y", "q", "m", "d"))) NA_character_ else x
}

choose_extdata <- function(data, freq, warn_invalid = TRUE, params_for_warn = list(active = 1L)) {
  assert_packages(c("tibble", "rlang", "stringr"))
  freq <- normalize_freq(freq)
  if (is.na(freq)) {
    warn_active(params_for_warn, "Unknown data_frequency; extdata is empty.", warn_invalid)
    return(tibble::tibble())
  }
  nm <- paste0("extdata_", freq)
  ext <- data[[nm]] %||% NULL
  if (is.null(ext) || !is.data.frame(ext)) {
    warn_active(params_for_warn, paste0("data[['", nm, "']] is missing/malformed; extdata is empty."), warn_invalid)
    return(tibble::tibble())
  }
  tibble::as_tibble(ext)
}

# ---- Time token parsing helpers (work with yyyy / yyyyqN / yyyymN / dd.mm.yyyy) ----

parse_time_token <- function(tok) {
  # Returns list(ok, oldfreq, year, sub, date, tok)
  tok0 <- stringr::str_trim(as.character(tok))
  if (is.na(tok0) || !nzchar(tok0)) return(list(ok = FALSE, tok = tok0))
  
  # daily: dd.mm.yyyy
  if (stringr::str_detect(tok0, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$")) {
    d <- suppressWarnings(as.Date(tok0, "%d.%m.%Y"))
    if (is.na(d)) return(list(ok = FALSE, tok = tok0))
    y <- as.integer(format(d, "%Y"))
    m <- as.integer(format(d, "%m"))
    return(list(ok = TRUE, oldfreq = "d", year = y, sub = m, date = d, tok = tok0))
  }
  
  tok2 <- stringr::str_to_lower(tok0)
  y <- suppressWarnings(as.integer(stringr::str_extract(tok2, "^(19|20)\\d{2}")))
  if (is.na(y)) return(list(ok = FALSE, tok = tok0))
  
  f <- stringr::str_extract(tok2, "[qm]")
  if (is.na(f)) return(list(ok = TRUE, oldfreq = "y", year = y, sub = NA_integer_, date = NA, tok = tok0))
  
  n <- suppressWarnings(as.integer(stringr::str_extract(tok2, "(?<=q)\\d+$|(?<=m)\\d+$")))
  if (is.na(n)) return(list(ok = FALSE, tok = tok0))
  if (f == "q" && !(n %in% 1:4)) return(list(ok = FALSE, tok = tok0))
  if (f == "m" && !(n %in% 1:12)) return(list(ok = FALSE, tok = tok0))
  
  list(ok = TRUE, oldfreq = f, year = y, sub = n, date = NA, tok = tok0)
}

# Convert single token to target freq using your prepareDates() (already in plot.R)
convert_time_token <- function(tok, freq, end, warn_invalid = TRUE, params_for_warn = list(active = 1L)) {
  out <- tryCatch(
    prepareDates(tok, freq = freq, end = end),
    error = function(e) NA_character_
  )
  if (is.na(out) || !nzchar(out)) {
    warn_active(params_for_warn, paste0("Failed to convert time token: '", tok, "'."), warn_invalid)
    return(NA_character_)
  }
  out
}

# Parse first token of a possibly comma-separated string; used for x_min/x_max/time_fix inputs
first_time_token <- function(x) {
  x <- as.character(x %||% NA_character_)
  x <- if (length(x) >= 1) x[[1]] else NA_character_
  x <- stringr::str_trim(x)
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  parts <- stringr::str_split(x, ",") |> purrr::pluck(1) |> stringr::str_trim()
  parts <- parts[parts != ""]
  if (length(parts) == 0) NA_character_ else parts[[1]]
}

# ---- Dict label helpers ----

lookup_indicator_label <- function(dict, indicator_code, freq, warn_invalid = TRUE, params_for_warn = list(active = 1L)) {
  assert_packages(c("dplyr", "stringr", "tibble", "rlang"))

  code <- indicator_code |>
    as.character() |>
    (\(x) if (length(x) >= 1) x[[1]] else NA_character_)() |>
    stringr::str_trim()
  
  freq <- normalize_freq(freq)
  
  if (is.na(code) || !nzchar(code)) {
    warn_active(params_for_warn, "indicator_code is empty; returning placeholder.", warn_invalid)
    return("[missing indicator]")
  }
  
  if (is.null(dict) || !is.data.frame(dict) || nrow(dict) == 0) {
    warn_active(params_for_warn, paste0("dict is empty; cannot label '", code, "'. Returning code."), warn_invalid)
    return(code)
  }
  
  dict <- tibble::as_tibble(dict)
  need <- c("indicator_code", "indicator")
  miss <- setdiff(need, names(dict))
  if (length(miss) > 0) {
    warn_active(params_for_warn, paste0("dict missing columns: ", paste(miss, collapse = ", "), ". Returning code."), warn_invalid)
    return(code)
  }
  
  cand <- dict |> dplyr::filter(.data$indicator_code == code)
  
  if (!is.null(freq) && !is.na(freq) && "source_frequency" %in% names(dict)) {
    cand2 <- cand |> dplyr::filter(.data$source_frequency == freq)
    if (nrow(cand2) > 0) cand <- cand2
  }
  
  lab <- cand |>
    dplyr::mutate(indicator = stringr::str_trim(as.character(.data$indicator))) |>
    dplyr::filter(!is.na(.data$indicator), .data$indicator != "") |>
    dplyr::pull(.data$indicator)
  
  if (length(lab) == 0) {
    warn_active(params_for_warn, paste0("No dict label for '", code, "' (freq=", freq, "); returning code."), warn_invalid)
    return(code)
  }
  
  if (length(unique(lab)) > 1) {
    warn_active(params_for_warn, paste0("Multiple labels for '", code, "' (freq=", freq, "); using first."), warn_invalid)
  }
  
  lab[[1]]
}

derive_axis_labels <- function(dict, params, warn_invalid = TRUE) {
  assert_packages(c("stringr"))
  
  graph_type <- params$graph_type %||% NA_character_
  graph_type <- stringr::str_trim(as.character(graph_type))
  
  freq <- params$data_frequency %||% NA_character_
  
  indicators <- params$indicators %||% character(0)
  indicators <- as.character(indicators) |> stringr::str_trim()
  indicators <- indicators[!is.na(indicators) & indicators != ""]
  
  x_code <- params$x_ind %||% NA_character_
  y_code <- params$y_ind %||% NA_character_
  x_code <- stringr::str_trim(as.character(x_code))
  y_code <- stringr::str_trim(as.character(y_code))
  
  # ---- Helper: safe label lookup (no "missing indicator" for empty codes) ----
  lab_of <- function(code) {
    code <- stringr::str_trim(as.character(code %||% NA_character_))
    if (is.na(code) || !nzchar(code)) return("")
    lookup_indicator_label(dict, code, freq, warn_invalid = warn_invalid, params_for_warn = params)
  }
  
  x_log <- as.integer(params$x_log %||% 0L)
  y_log <- as.integer(params$y_log %||% 0L)
  
  # ---- Graph-type aware axis semantics ---------------------------------------
  dynamic_time_x <- graph_type %in% c(
    "structure_dynamic", "bar_dynamic",
    "lines_country_comparison", "lines_indicator_comparison",
    "distribution_dynamic"
  )
  
  cross_country_x <- graph_type %in% c(
    "bar_country_comparison", "bar_country_comparison_norm",
    "structure_country_comparison", "structure_country_comparison_norm"
  )
  
  # defaults
  x_lab <- ""
  y_lab <- ""
  
  if (identical(graph_type, "scatter_country_comparison")) {
    # true x/y semantics
    x0 <- lab_of(x_code)
    y0 <- lab_of(y_code)
    
    x_lab <- if (x_log == 1L && nzchar(x0)) paste0("log10(", x0, ")") else x0
    y_lab <- if (y_log == 1L && nzchar(y0)) paste0("log10(", y0, ")") else y0
    
    return(list(x_lab = x_lab, y_lab = y_lab))
  }
  
  if (identical(graph_type, "density_fix")) {
    x0 <- lab_of(x_code)
    x_lab <- if (x_log == 1L && nzchar(x0)) paste0("log10(", x0, ")") else x0
    y_lab <- "Плотность"
    return(list(x_lab = x_lab, y_lab = y_lab))
  }
  
  if (isTRUE(dynamic_time_x)) {
    # X is time -> no axis title by default
    x_lab <- ""
    
    # Y is the indicator (usually the first one)
    # prefer: first indicator if present; else fallback to x_ind
    y_ind <- indicators[[1]] %||% x_code
    y0 <- lab_of(y_ind)
    
    y_lab <- if (y_log == 1L && nzchar(y0)) paste0("log10(", y0, ")") else y0
    
    # If multiple series, legend explains -> blank y axis title
    if (length(indicators) > 1) y_lab <- ""
    
    return(list(x_lab = x_lab, y_lab = y_lab))
  }
  
  if (isTRUE(cross_country_x)) {
    # X is country_id/categories -> no x axis title by default
    x_lab <- ""
    
    # Y is indicator (single indicator case)
    y_ind <- indicators[[1]] %||% x_code
    y0 <- lab_of(y_ind)
    y_lab <- if (y_log == 1L && nzchar(y0)) paste0("log10(", y0, ")") else y0
    
    # multiple indicators -> legend explains
    if (length(indicators) > 1) y_lab <- ""
    
    return(list(x_lab = x_lab, y_lab = y_lab))
  }
  
  # Fallback: old semantics (but safe)
  x0 <- lab_of(x_code)
  y0 <- lab_of(y_code)
  
  x_lab <- if (x_log == 1L && nzchar(x0)) paste0("log10(", x0, ")") else x0
  y_lab <- if (y_log == 1L && nzchar(y0)) paste0("log10(", y0, ")") else y0
  
  list(x_lab = x_lab, y_lab = y_lab)
}

# ---- Time window derivation for dynamic graphs ----
# Returns:
#  x_min_parts/x_max_parts: list(ok, year, sub)
#  time_start/time_end: numeric indexes (as in your old code: 1987-based)
#  labfreq/timetony_start/timetony_end: scalars for x-axis breaks
derive_dynamic_window_from_data <- function(extdata, params, country_iso2c, peers_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tibble", "stringr", "rlang", "tidyr"))
  
  freq <- normalize_freq(params$data_frequency)
  if (is.na(freq)) {
    warn_active(params, "Bad data_frequency for dynamic window; returning NAs.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  # ---- bounds priority ---------------------------------------------------
  # Default: take bounds from x_min/x_max.
  # Exception: for scatter_dynamic allow time_fix="t1, t2" as a window override.
  split_two_tokens <- function(x) {
    x <- as.character(x %||% NA_character_)
    x <- if (length(x) >= 1) x[[1]] else NA_character_
    x <- stringr::str_trim(x)
    x <- dplyr::na_if(x, "")
    if (is.na(x)) return(character(0))
    
    x |>
      stringr::str_split(",") |>
      (\(v) v[[1]])() |>
      stringr::str_trim() |>
      (\(v) v[!is.na(v) & v != ""])()
  }
  
  tf_tokens <- split_two_tokens(params$time_fix %||% NA_character_)
  use_tf_window <- identical(params$graph_type %||% NA_character_, "scatter_dynamic") &&
    length(tf_tokens) == 2L
  
  if (isTRUE(use_tf_window)) {
    x_min_in <- tf_tokens[[1]]
    x_max_in <- tf_tokens[[2]]
  } else {
    x_min_in <- first_time_token(params$x_min %||% NA_character_)
    x_max_in <- first_time_token(params$x_max %||% NA_character_)
  }
  
  # ---- defaults (must exist in all branches) -----------------------------
  date_start <- as.Date(NA)
  date_end   <- as.Date(NA)
  
  parse_parts <- function(tok, end) {
    if (is.na(tok) || !nzchar(tok)) return(list(ok = FALSE))
    tok2 <- convert_time_token(tok, freq = freq, end = end, warn_invalid = warn_invalid, params_for_warn = params)
    p <- parse_time_token(tok2)
    if (!isTRUE(p$ok)) return(list(ok = FALSE))
    list(ok = TRUE, year = as.integer(p$year), sub = as.integer(p$sub))
  }
  
  x_min_parts <- parse_parts(x_min_in, end = 0L)
  x_max_parts <- parse_parts(x_max_in, end = 1L)
  
  # If not provided, derive from data for main country (deterministic)
  # We use minimal requirement: at least one non-NA among needed indicators at each date
  indicators <- params$indicators %||% character(0)
  indicators <- as.character(indicators)
  indicators <- indicators[!is.na(indicators) & indicators != ""]
  
  if (length(indicators) == 0) {
    warn_active(params, "No indicators for dynamic window; returning NAs.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  if (!("country_id" %in% names(extdata))) {
    warn_active(params, "extdata has no country_id; cannot derive dynamic window.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  time_cols <- c("year", "quarter", "month")
  keep_time_cols <- time_cols[time_cols %in% names(extdata)]
  
  if (!("year" %in% names(extdata))) {
    warn_active(params, "extdata has no year; cannot derive dynamic window.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  df <- tibble::as_tibble(extdata) |>
    dplyr::filter(.data$country_id == country_iso2c) |>
    dplyr::select(dplyr::any_of(c("country_id", keep_time_cols)), dplyr::any_of(indicators))
  
  if (nrow(df) == 0) {
    warn_active(params, "No rows for country in extdata; cannot derive dynamic window.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  # Require at least 1 non-NA among indicators
  df_ok <- df |>
    dplyr::mutate(.non_na = rowSums(!is.na(dplyr::pick(dplyr::any_of(indicators))))) |>
    dplyr::filter(.data$.non_na >= 1) |>
    dplyr::arrange(.data$year, dplyr::across(dplyr::any_of(c("quarter", "month"))))
  
  if (nrow(df_ok) == 0) {
    warn_active(params, "No non-NA indicator points for dynamic window; returning NAs.", warn_invalid)
    return(list(
      x_min_parts = list(ok = FALSE), x_max_parts = list(ok = FALSE),
      time_start = NA_real_, time_end = NA_real_, labfreq = NA_real_,
      timetony_start = NA_real_, timetony_end = NA_real_
    ))
  }
  
  # Fill x_min_parts/x_max_parts from data if missing
  if (!isTRUE(x_min_parts$ok)) {
    first_row <- df_ok |> dplyr::slice(1)
    x_min_parts <- list(
      ok = TRUE,
      year = as.integer(first_row$year[[1]]),
      sub  = if (freq == "q") as.integer(first_row$quarter[[1]] %||% 1L) else if (freq == "m") as.integer(first_row$month[[1]] %||% 1L) else NA_integer_
    )
  }
  if (!isTRUE(x_max_parts$ok)) {
    last_row <- df_ok |> dplyr::slice(n())
    x_max_parts <- list(
      ok = TRUE,
      year = as.integer(last_row$year[[1]]),
      sub  = if (freq == "q") as.integer(last_row$quarter[[1]] %||% 4L) else if (freq == "m") as.integer(last_row$month[[1]] %||% 12L) else NA_integer_
    )
  }
  
  # Convert parts -> internal "time" index exactly as in your legacy code
  if (freq == "y") {
    time_start <- x_min_parts$year - 1987
    time_end   <- x_max_parts$year - 1987
    labfreq <- 1
    timetony_start <- 0
    timetony_end <- 1
  } else if (freq == "q") {
    time_start <- (x_min_parts$year - 1987) * 4 + x_min_parts$sub
    time_end   <- (x_max_parts$year - 1987) * 4 + x_max_parts$sub
    labfreq <- 4
    timetony_start <- (5 - x_min_parts$sub) %% 4
    timetony_end <- x_max_parts$sub
  } else if (freq == "m") {
    time_start <- (x_min_parts$year - 1987) * 12 + x_min_parts$sub
    time_end   <- (x_max_parts$year - 1987) * 12 + x_max_parts$sub
    labfreq <- 12
    timetony_start <- (13 - x_min_parts$sub) %% 12
    timetony_end <- x_max_parts$sub
  } else if (freq == "d") {
    # ---- Daily support: derive window by date, and return numeric time bounds too ----
    if (!("date" %in% names(extdata))) {
      warn_active(params, "Daily extdata has no 'date' column; cannot derive dynamic window.", warn_invalid)
      time_start <- NA_real_
      time_end <- NA_real_
      labfreq <- NA_real_
      timetony_start <- NA_real_
      timetony_end <- NA_real_
      date_start <- as.Date(NA)
      date_end <- as.Date(NA)
    } else {
      # helper: parse date/datetime token -> Date
      parse_date_tok <- function(tok) {
        tok <- as.character(tok %||% NA_character_)
        tok <- if (length(tok) >= 1) tok[[1]] else NA_character_
        tok <- stringr::str_trim(tok)
        tok <- dplyr::na_if(tok, "")
        if (is.na(tok)) return(as.Date(NA))
        
        d1 <- suppressWarnings(as.Date(tok))
        if (!is.na(d1)) return(d1)
        
        dt <- suppressWarnings(as.POSIXct(tok, tz = "UTC"))
        as.Date(dt)
      }
      
      # Build df_ok like before, but include date/time
      df_d <- tibble::as_tibble(extdata) |>
        dplyr::filter(.data$country_id == country_iso2c) |>
        dplyr::select(dplyr::any_of(c("country_id", "time", "date")), dplyr::any_of(indicators))
      
      if (nrow(df_d) == 0) {
        warn_active(params, "No rows for country in daily extdata; cannot derive dynamic window.", warn_invalid)
        time_start <- NA_real_
        time_end <- NA_real_
        labfreq <- NA_real_
        timetony_start <- NA_real_
        timetony_end <- NA_real_
        date_start <- as.Date(NA)
        date_end <- as.Date(NA)
      } else {
        # normalize date to Date (supports Date / POSIXt / character datetime)
        df_d <- df_d |>
          dplyr::mutate(
            .date2 = {
              dd <- .data$date
              if (inherits(dd, "Date")) dd
              else if (inherits(dd, "POSIXt")) as.Date(dd)
              else {
                dd_chr <- as.character(dd) |> stringr::str_trim() |> dplyr::na_if("")
                dd_try <- suppressWarnings(as.Date(dd_chr))
                need_posix <- is.na(dd_try) & !is.na(dd_chr)
                if (any(need_posix)) {
                  dt_try <- suppressWarnings(as.POSIXct(dd_chr[need_posix], tz = "UTC"))
                  dd_try[need_posix] <- as.Date(dt_try)
                }
                dd_try
              }
            },
            .time2 = suppressWarnings(as.numeric(.data$time))
          )
        
        df_ok <- df_d |>
          dplyr::mutate(.non_na = rowSums(!is.na(dplyr::pick(dplyr::any_of(indicators))))) |>
          dplyr::filter(.data$.non_na >= 1, !is.na(.data$.date2)) |>
          dplyr::arrange(.data$.date2)
        
        if (nrow(df_ok) == 0) {
          warn_active(params, "No non-NA indicator points with valid dates for daily window; returning NAs.", warn_invalid)
          time_start <- NA_real_
          time_end <- NA_real_
          labfreq <- NA_real_
          timetony_start <- NA_real_
          timetony_end <- NA_real_
          date_start <- as.Date(NA)
          date_end <- as.Date(NA)
        } else {
          # user-specified x_min/x_max treated as date bounds for daily
          d_min_in <- parse_date_tok(x_min_in)
          d_max_in <- parse_date_tok(x_max_in)
          
          if (!is.na(d_min_in) && !is.na(d_max_in)) {
            date_start <- min(d_min_in, d_max_in)
            date_end   <- max(d_min_in, d_max_in)
            
            df_w <- df_ok |>
              dplyr::filter(.data$.date2 >= date_start, .data$.date2 <= date_end)
            
            # if window is empty, fall back to full available range (no hidden truncation)
            if (nrow(df_w) == 0) {
              warn_active(params, "Daily x_min/x_max window produced 0 points; using full available range.", warn_invalid)
              date_start <- df_ok$.date2[[1]]
              date_end   <- df_ok$.date2[[nrow(df_ok)]]
              df_w <- df_ok
            }
          } else {
            # derive full available daily range
            date_start <- df_ok$.date2[[1]]
            date_end   <- df_ok$.date2[[nrow(df_ok)]]
            df_w <- df_ok
          }
          
          # numeric time bounds (if time exists/finite)
          if ("time" %in% names(df_w) && any(is.finite(df_w$.time2))) {
            time_start <- min(df_w$.time2, na.rm = TRUE)
            time_end   <- max(df_w$.time2, na.rm = TRUE)
          } else {
            time_start <- NA_real_
            time_end <- NA_real_
          }
          
          # daily label spacing is plot-specific; keep NA (safe default)
          labfreq <- NA_real_
          timetony_start <- NA_real_
          timetony_end <- NA_real_
        }
      }
    }
    
    # Provide x_min_parts/x_max_parts for consistency (won't be used for d in fillGraphPlan after patch 2)
    x_min_parts <- list(ok = !is.na(date_start), year = as.integer(format(date_start, "%Y")), sub = NA_integer_)
    x_max_parts <- list(ok = !is.na(date_end),   year = as.integer(format(date_end, "%Y")),   sub = NA_integer_)
  } else {
    # unknown / unsupported
    time_start <- NA_real_
    time_end <- NA_real_
    labfreq <- NA_real_
    timetony_start <- NA_real_
    timetony_end <- NA_real_
    date_start <- as.Date(NA)
    date_end <- as.Date(NA)
  }
  
  list(
    x_min_parts = x_min_parts,
    x_max_parts = x_max_parts,
    time_start = time_start,
    time_end = time_end,
    date_start = date_start %||% as.Date(NA),
    date_end   = date_end %||% as.Date(NA),
    labfreq = labfreq,
    timetony_start = timetony_start,
    timetony_end = timetony_end
  )
}

# ---- Cross-section time_fix derivation ----
# Produces:
#  time_fix_parts: list(ok, year, sub)
#  time_fix_label: "yyyy" / "yyyyqN" / "yyyymN" / "dd.mm.yyyy"
#  time_fix: internal numeric "time" index (1987-based) for y/q/m; NA for y? (legacy uses year-1987)
derive_cross_time_fix <- function(extdata,
                                  params,
                                  country_iso2c,
                                  warn_invalid = TRUE,
                                  expected_n = NULL) {
  assert_packages(c("dplyr", "tibble", "stringr", "rlang"))
  
  # ---- local helpers (deterministic, no side effects) -------------------
  split_time_tokens_local <- function(x) {
    x <- as.character(x %||% NA_character_)
    if (length(x) == 0) return(character(0))
    x <- x[[1]]
    x <- stringr::str_trim(x)
    x <- dplyr::na_if(x, "")
    if (is.na(x)) return(character(0))
    
    x |>
      stringr::str_split(",") |>
      (\(v) v[[1]])() |>
      stringr::str_trim() |>
      (\(v) v[!is.na(v) & v != ""])()
  }
  
  is_expected_n <- function(n) {
    if (is.null(n)) return(TRUE)
    n <- suppressWarnings(as.integer(n))
    is.finite(n) && n >= 1L
  }
  
  expected_n <- if (is.null(expected_n)) NULL else suppressWarnings(as.integer(expected_n))
  if (!is_expected_n(expected_n)) {
    warn_active(params, "Bad expected_n for time_fix; ignoring (treat as NULL).", warn_invalid)
    expected_n <- NULL
  }
  
  freq <- normalize_freq(params$data_frequency)
  if (is.na(freq)) {
    warn_active(params, "Bad data_frequency for time_fix; returning NA.", warn_invalid)
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  # ----------------------------------------------------------------------
  # 1) If user specified time_fix: parse 1 token (legacy) OR N tokens (new)
  # ----------------------------------------------------------------------
  if (is.null(expected_n) || identical(expected_n, 1L)) {
    # ---- legacy behavior: take only the first token ---------------------
    tf_in <- first_time_token(params$time_fix %||% NA_character_)
    if (!is.na(tf_in) && nzchar(tf_in)) {
      tf_tok <- convert_time_token(tf_in, freq = freq, end = 1L, warn_invalid = warn_invalid, params_for_warn = params)
      p <- parse_time_token(tf_tok)
      
      if (isTRUE(p$ok)) {
        year <- as.integer(p$year)
        sub  <- if (freq == "q") as.integer(p$sub) else if (freq == "m") as.integer(p$sub) else NA_integer_
        
        tf_label <- if (freq == "y") {
          sprintf("%04d", year)
        } else if (freq == "q") {
          paste0(sprintf("%04d", year), "q", sub)
        } else if (freq == "m") {
          paste0(sprintf("%04d", year), "m", sub)
        } else {
          tf_tok
        }
        
        tf_num <- if (freq == "y") {
          (year - 1987)
        } else if (freq == "q") {
          (year - 1987) * 4 + sub
        } else if (freq == "m") {
          (year - 1987) * 12 + sub
        } else {
          NA_real_
        }
        
        return(list(
          time_fix = tf_num,
          time_fix_label = tf_label,
          time_fix_parts = list(ok = TRUE, year = year, sub = sub)
        ))
      }
      
      warn_active(params, paste0("time_fix token '", tf_in, "' could not be parsed; will try derive from data."), warn_invalid)
    }
  } else {
    # ---- new behavior: require exactly expected_n tokens ----------------
    toks_raw <- split_time_tokens_local(params$time_fix %||% NA_character_)
    
    if (length(toks_raw) > 0 && length(toks_raw) != expected_n) {
      warn_active(
        params,
        paste0(
          "time_fix must contain exactly ", expected_n, " tokens (comma-separated). Got ",
          length(toks_raw), ": '", paste(toks_raw, collapse = ", "), "'. Returning NA."
        ),
        warn_invalid
      )
      return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
    }
    
    if (length(toks_raw) == expected_n) {
      # convert each token to target freq (end=1)
      toks_conv <- toks_raw |>
        purrr::map_chr(\(tok) {
          convert_time_token(tok, freq = freq, end = 1L, warn_invalid = warn_invalid, params_for_warn = params)
        })
      
      parts <- toks_conv |> purrr::map(parse_time_token)
      ok <- parts |> purrr::map_lgl(\(p) isTRUE(p$ok))
      
      if (all(ok)) {
        years <- parts |> purrr::map_int(\(p) as.integer(p$year))
        subs  <- parts |> purrr::map_int(\(p) {
          if (freq %in% c("q", "m")) as.integer(p$sub) else NA_integer_
        })
        
        labels <- if (freq == "y") {
          sprintf("%04d", years)
        } else if (freq == "q") {
          paste0(sprintf("%04d", years), "q", subs)
        } else if (freq == "m") {
          paste0(sprintf("%04d", years), "m", subs)
        } else {
          toks_conv
        }
        
        nums <- if (freq == "y") {
          (years - 1987)
        } else if (freq == "q") {
          (years - 1987) * 4 + subs
        } else if (freq == "m") {
          (years - 1987) * 12 + subs
        } else {
          rep(NA_real_, expected_n)
        }
        
        return(list(
          time_fix = as.numeric(nums),                         # length expected_n
          time_fix_label = paste(labels, collapse = ", "),
          time_fix_parts = list(
            ok = TRUE,
            year = years,
            sub = subs,
            labels = labels,
            toks_raw = toks_raw,
            toks_conv = toks_conv,
            freq = freq
          )
        ))
      }
      
      warn_active(
        params,
        paste0(
          "time_fix tokens could not be parsed: '", paste(toks_raw, collapse = ", "),
          "'; returning NA (no derivation for expected_n>1)."
        ),
        warn_invalid
      )
      return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
    }
    
    # length(toks_raw) == 0: user did not specify time_fix
    # For expected_n>1 we do NOT derive silently (avoid hidden fallback)
    warn_active(
      params,
      paste0("time_fix is missing; expected ", expected_n, " tokens. Returning NA (no derivation for expected_n>1)."),
      warn_invalid
    )
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  # ----------------------------------------------------------------------
  # 2) Derive from data (legacy path ONLY; i.e., expected_n is NULL/1)
  # ----------------------------------------------------------------------
  indicators <- params$indicators %||% character(0)
  indicators <- as.character(indicators)
  indicators <- indicators[!is.na(indicators) & indicators != ""]
  
  if (length(indicators) == 0) {
    warn_active(params, "No indicators for time_fix derivation; returning NA.", warn_invalid)
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  if (!all(c("country_id", "year") %in% names(extdata))) {
    warn_active(params, "extdata lacks country_id/year; cannot derive time_fix.", warn_invalid)
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  time_cols <- c("year", "quarter", "month")
  keep_time_cols <- time_cols[time_cols %in% names(extdata)]
  
  df <- tibble::as_tibble(extdata) |>
    dplyr::filter(.data$country_id == country_iso2c) |>
    dplyr::select(dplyr::any_of(keep_time_cols), dplyr::any_of(indicators))
  
  if (nrow(df) == 0) {
    warn_active(params, "No rows for country; cannot derive time_fix.", warn_invalid)
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  df_ok <- df |>
    dplyr::mutate(.all_ok = rowSums(!is.na(dplyr::pick(dplyr::any_of(indicators)))) == length(indicators)) |>
    dplyr::filter(.data$.all_ok) |>
    dplyr::arrange(.data$year, dplyr::across(dplyr::any_of(c("quarter", "month"))))
  
  if (nrow(df_ok) == 0) {
    warn_active(params, "No rows with all indicators present; cannot derive time_fix.", warn_invalid)
    return(list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  last_row <- df_ok |> dplyr::slice(n())
  year <- as.integer(last_row$year[[1]])
  sub  <- if (freq == "q") as.integer(last_row$quarter[[1]] %||% 4L) else if (freq == "m") as.integer(last_row$month[[1]] %||% 12L) else NA_integer_
  
  tf_label <- if (freq == "y") {
    sprintf("%04d", year)
  } else if (freq == "q") {
    paste0(sprintf("%04d", year), "q", sub)
  } else if (freq == "m") {
    paste0(sprintf("%04d", year), "m", sub)
  } else {
    NA_character_
  }
  
  tf_num <- if (freq == "y") {
    (year - 1987)
  } else if (freq == "q") {
    (year - 1987) * 4 + sub
  } else if (freq == "m") {
    (year - 1987) * 12 + sub
  } else {
    NA_real_
  }
  
  list(
    time_fix = tf_num,
    time_fix_label = tf_label,
    time_fix_parts = list(ok = TRUE, year = year, sub = sub)
  )
}

split_time_tokens <- function(x) {
  assert_packages(c("stringr", "purrr"))
  
  x <- as.character(x %||% NA_character_)
  x <- if (length(x) >= 1) x[[1]] else NA_character_
  x <- stringr::str_trim(x)
  x <- dplyr::na_if(x, "")
  if (is.na(x)) return(character(0))
  
  x |>
    stringr::str_split(",") |>
    purrr::pluck(1) |>
    stringr::str_trim() |>
    (\(v) v[!is.na(v) & v != ""])()
}

# Function to parse difficult time_fix

derive_cross_time_fix_years <- function(extdata, params, country_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tibble", "stringr", "rlang", "purrr"))
  
  # 1) try parse user-provided time_fix as a SET of years
  toks <- split_time_tokens(params$time_fix %||% NA_character_)
  
  if (length(toks) > 0) {
    years <- purrr::map_int(toks, \(tok) {
      # convert token to yearly representation (extract year)
      tok_y <- convert_time_token(
        tok, freq = "y", end = 1L,
        warn_invalid = warn_invalid, params_for_warn = params
      )
      p <- parse_time_token(tok_y)
      if (!isTRUE(p$ok)) return(NA_integer_)
      as.integer(p$year)
    })
    
    years <- years[is.finite(years)]
    years <- sort(unique(years))
    
    if (length(years) > 0) {
      return(list(
        time_fix = years,                               # <-- vector of years
        time_fix_label = paste(years, collapse = ", "), # optional; some plots may show it
        time_fix_parts = list(ok = TRUE, years = years)
      ))
    }
  }
  
  # 2) fallback: derive from data (deterministic)
  indicators <- params$indicators %||% character(0)
  indicators <- as.character(indicators)
  indicators <- indicators[!is.na(indicators) & indicators != ""]
  
  if (length(indicators) == 0) {
    warn_active(params, "No indicators for time_fix(years) derivation; returning empty.", warn_invalid)
    return(list(time_fix = integer(0), time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  if (!all(c("country_id", "year") %in% names(extdata))) {
    warn_active(params, "extdata lacks country_id/year; cannot derive years set.", warn_invalid)
    return(list(time_fix = integer(0), time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  df <- tibble::as_tibble(extdata) |>
    dplyr::filter(.data$country_id == country_iso2c) |>
    dplyr::select(.data$year, dplyr::any_of(indicators)) |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      .all_ok = rowSums(!is.na(dplyr::pick(dplyr::any_of(indicators)))) == length(indicators)
    ) |>
    dplyr::filter(is.finite(.data$year), .data$.all_ok) |>
    dplyr::distinct(.data$year) |>
    dplyr::arrange(.data$year)
  
  years <- df$year
  if (length(years) == 0) {
    warn_active(params, "No years with all indicators present; returning empty.", warn_invalid)
    return(list(time_fix = integer(0), time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE)))
  }
  
  # choose last 3 years as deterministic default
  years <- tail(years, 3)
  
  list(
    time_fix = years,
    time_fix_label = paste(years, collapse = ", "),
    time_fix_parts = list(ok = TRUE, years = years)
  )
}

# Helper to fix difficult y range cases

y_lim_policy <- function(
    kind,
    y_min,
    y_max,
    y_rng_data,
    include_zero = FALSE,
    force_coord = FALSE
) {
  assert_packages(c("rlang"))
  
  kind <- tolower(as.character(kind %||% "line"))
  
  # normalize families (keeps backward compatibility)
  if (stringr::str_detect(kind, "^bar_") || stringr::str_detect(kind, "bar")) kind <- "bar"
  if (stringr::str_detect(kind, "^structure_") && !stringr::str_detect(kind, "_norm$")) kind <- "stack"
  if (stringr::str_detect(kind, "_norm$") || stringr::str_detect(kind, "fill")) kind <- "fill"
  
  # data range fallback
  if (!is.numeric(y_rng_data) || length(y_rng_data) != 2 || any(!is.finite(y_rng_data))) {
    y_rng_data <- c(0, 1)
  }
  
  y_min0 <- suppressWarnings(as.numeric(y_min))
  y_max0 <- suppressWarnings(as.numeric(y_max))
  
  if (!is.finite(y_min0)) y_min0 <- y_rng_data[[1]]
  if (!is.finite(y_max0)) y_max0 <- y_rng_data[[2]]
  
  # If still broken, fallback
  if (!(is.finite(y_min0) && is.finite(y_max0) && y_min0 < y_max0)) {
    y_min0 <- y_rng_data[[1]]
    y_max0 <- y_rng_data[[2]]
  }
  
  # Geometry-specific policies
  if (kind %in% c("bar", "col", "bar_dodge", "bar_dynamic", "column")) {
    # geom_col needs baseline 0 inside the scale limits (when using scale limits)
    # otherwise ymin becomes NA and bars vanish.
    if (isTRUE(include_zero) || TRUE) {  # bars: always include zero
      if (y_min0 > 0) y_min0 <- 0
      if (y_max0 < 0) y_max0 <- 0
    }
  }
  
  if (kind %in% c("stack", "stacked", "structure_dynamic")) {
    # also include zero, because baseline matters for stacked bars too
    if (y_min0 > 0) y_min0 <- 0
    if (y_max0 < 0) y_max0 <- 0
  }
  
  if (kind %in% c("fill", "proportion", "structure_dynamic_norm")) {
    # normalized stacks: default [0,1] unless user explicitly overrides into something else
    # (we keep user's valid limits)
    if (!isTRUE(include_zero)) {
      # no-op
    }
    # If user did not specify (or they were non-finite originally), enforce [0,1]
    # We detect "unspecified" via comparing to data range is messy; so keep it simple:
    # only clamp if data range already within [0,1] and user didn't push it outside.
    if (y_rng_data[[1]] >= 0 && y_rng_data[[2]] <= 1) {
      y_min0 <- max(0, y_min0)
      y_max0 <- min(1, y_max0)
      if (!(y_min0 < y_max0)) {
        y_min0 <- 0
        y_max0 <- 1
      }
    }
  }
  
  # For line/scatter: optional include_zero
  if (kind %in% c("line", "lines", "scatter", "point")) {
    if (isTRUE(include_zero)) {
      if (y_min0 > 0) y_min0 <- 0
      if (y_max0 < 0) y_max0 <- 0
    }
  }
  
  list(
    y_min = y_min0,
    y_max = y_max0,
    # If you ever want to avoid censoring, you can switch to coord_cartesian
    use_coord = isTRUE(force_coord)
  )
}

apply_y_limits_viewport <- function(p, ylim, do_sec = FALSE, coeff = NA_real_) {
  assert_packages(c("ggplot2", "rlang"))
  
  ylim <- suppressWarnings(as.numeric(ylim))
  if (length(ylim) != 2 || any(!is.finite(ylim)) || ylim[[1]] >= ylim[[2]]) {
    rlang::warn("apply_y_limits_viewport: bad ylim; skip coord_cartesian.")
    return(p)
  }
  
  do_sec <- isTRUE(do_sec)
  coeff  <- suppressWarnings(as.numeric(coeff))
  
  if (do_sec && is.finite(coeff) && coeff != 0) {
    p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / coeff))
  }
  
  p + ggplot2::coord_cartesian(ylim = ylim)
}

# ------------------------------------------------------------------------------
# Improved fillGraphPlan()
# ------------------------------------------------------------------------------

fillGraphPlan <- function(parsedrow, data, country_iso2c, peers_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tibble", "stringr", "rlang", "purrr"))
  
  if (is.null(parsedrow) || !is.list(parsedrow)) {
    rlang::warn("fillGraphPlan: parsedrow must be a named list. Returning placeholder params.")
    return(list(
      graph_type = NA_character_,
      data_frequency = NA_character_,
      indicators = character(0),
      active = 0L,
      placeholder = TRUE
    ))
  }
  
  params <- parsedrow
  
  # Normalize a few scalars
  params$active <- as.integer(params$active %||% 0L)
  params$data_frequency <- normalize_freq(params$data_frequency %||% "y") %||% "y"
  params$graph_type <- as.character(params$graph_type %||% NA_character_)
  params$graph_type <- stringr::str_trim(params$graph_type)
  if (is.na(params$graph_type) || !nzchar(params$graph_type)) params$graph_type <- NA_character_
  
  # Ensure indicators is a plain character vector (handles list-columns from tibble rows)
  inds <- params$indicators %||% character(0)
  if (is.list(inds)) {
    # typical case: list(<chr vector>) coming from a list-column
    inds <- unlist(inds, recursive = TRUE, use.names = FALSE)
  }
  inds <- as.character(inds)
  inds <- stringr::str_trim(inds)
  inds <- inds[!is.na(inds) & inds != ""]
  params$indicators <- inds
  
  # Country / peers (runtime representation)
  params$country_iso2c <- as.character(country_iso2c %||% NA_character_)
  peers_iso2c <- as.character(peers_iso2c %||% character(0))
  peers_iso2c <- stringr::str_trim(peers_iso2c)
  peers_iso2c <- peers_iso2c[!is.na(peers_iso2c) & peers_iso2c != ""]
  params$peers_iso2c <- unique(peers_iso2c)
  
  # Pull dict and extdata for requested frequency
  dict <- data$dict %||% tibble::tibble()
  ext  <- choose_extdata(data, params$data_frequency, warn_invalid = warn_invalid, params_for_warn = params)
  
  # Classify graph type
  dynamic_types <- c(
    "structure_dynamic", "scatter_dynamic", "bar_dynamic", "lines_country_comparison",
    "lines_indicator_comparison", "distribution_dynamic"
  )
  
  cross_types <- c(
    "scatter_country_comparison", "scatter_before_after", "structure_country_comparison",
    "structure_country_comparison_norm", "bar_country_comparison",
    "bar_country_comparison_norm", "bar_year_comparison",
    "distribution_year_comparison", "density_fix", "triangle"
  )
  
  is_dynamic <- !is.na(params$graph_type) && params$graph_type %in% dynamic_types
  is_cross   <- !is.na(params$graph_type) && params$graph_type %in% cross_types
  
  # --- 1) Dynamic window derived fields (only for dynamic graphs) -------
  if (isTRUE(is_dynamic)) {
    dyn <- derive_dynamic_window_from_data(
      extdata = ext,
      params = params,
      country_iso2c = params$country_iso2c,
      peers_iso2c = params$peers_iso2c,
      warn_invalid = warn_invalid
    )
    
    params$x_min_parts <- dyn$x_min_parts
    params$x_max_parts <- dyn$x_max_parts
    params$time_start <- dyn$time_start
    params$time_end <- dyn$time_end
    params$date_start <- dyn$date_start %||% as.Date(NA)
    params$date_end   <- dyn$date_end %||% as.Date(NA)
    params$labfreq <- dyn$labfreq
    params$timetony_start <- dyn$timetony_start
    params$timetony_end <- dyn$timetony_end
    
    # Backward compat: some dynamic plots expect x_min/x_max to be c(year, sub)
    # BUT scatter_dynamic uses x_min/x_max as VALUE axis limits, so never overwrite there.
    freq <- normalize_freq(params$data_frequency)
    
    if (!identical(params$graph_type %||% NA_character_, "scatter_dynamic")) {
      if (isTRUE(dyn$x_min_parts$ok)) {
        params$x_min <- if (freq == "y") dyn$x_min_parts$year else c(dyn$x_min_parts$year, dyn$x_min_parts$sub)
      }
      if (isTRUE(dyn$x_max_parts$ok)) {
        params$x_max <- if (freq == "y") dyn$x_max_parts$year else c(dyn$x_max_parts$year, dyn$x_max_parts$sub)
      }
    }
  } else {
    # Not dynamic: keep NA; and DO NOT overwrite axis x_min/x_max which are value limits for many cross plots
    params$x_min_parts <- NULL
    params$x_max_parts <- NULL
    params$time_start <- NA_real_
    params$time_end <- NA_real_
    params$date_start <- as.Date(NA)
    params$date_end <- as.Date(NA)
    params$labfreq <- NA_real_
    params$timetony_start <- NA_real_
    params$timetony_end <- NA_real_
  }
  
  # --- 2) Cross-section time point (only for cross graphs) --------------
  if (isTRUE(is_cross)) {
    if (params$graph_type %in% c("bar_year_comparison", "distribution_year_comparison")) {
      cross <- derive_cross_time_fix_years(
        extdata = ext,
        params = params,
        country_iso2c = params$country_iso2c,
        warn_invalid = warn_invalid
      )
    } else if (identical(params$graph_type, "scatter_before_after")) {
      cross <- derive_cross_time_fix(
        extdata = ext,
        params = params,
        country_iso2c = params$country_iso2c,
        warn_invalid = warn_invalid,
        expected_n = 2L
      )
    } else {
      cross <- derive_cross_time_fix(
        extdata = ext,
        params = params,
        country_iso2c = params$country_iso2c,
        warn_invalid = warn_invalid
      )
    }
    
    params$time_fix <- cross$time_fix
    params$time_fix_label <- cross$time_fix_label
    params$time_fix_parts <- cross$time_fix_parts
  } else {
    # For most non-cross graphs, time_fix is not used and should be cleared.
    # EXCEPTION: scatter_dynamic uses time_fix="t1, t2" as a dynamic window override.
    if (!identical(params$graph_type %||% NA_character_, "scatter_dynamic")) {
      params$time_fix <- NA_real_
      params$time_fix_label <- NA_character_
      params$time_fix_parts <- NULL
    } else {
      # keep raw time_fix so scatter_dynamic (or dynamic-window derivation) can use it
      params$time_fix_label <- params$time_fix_label %||% NA_character_
      params$time_fix_parts <- params$time_fix_parts %||% NULL
    }
  }
  
  # --- 3) Axis labels (systematic; never dict[,1]; never mutate x_ind/y_ind) ---
  labs <- derive_axis_labels(dict = dict, params = params, warn_invalid = warn_invalid)
  params$x_lab <- labs$x_lab
  params$y_lab <- labs$y_lab
  
  # --- 4) Caption (keep old behavior but avoid duplicates/empties) -------
  # source_name is expected to be prepared in getPlotSchedule(); still sanitize just in case
  if (is.null(params$caption) || is.na(params$caption) || !nzchar(as.character(params$caption))) {
    src <- params$source_name %||% NA_character_
    src <- as.character(src)
    src <- if (length(src) >= 1) src[[1]] else NA_character_
    src <- stringr::str_squish(src)
    src <- dplyr::na_if(src, "")
    params$caption <- if (is.na(src)) "Источники:" else paste0("Источники: ", src)
  }
  
  # Marker for plotting layer
  params$placeholder <- FALSE
  
  params
}

####### Function to subset data for the particular graph

subsetData <- function(data, graph_params, country_code = NULL, peers_code = NULL, warn_invalid = TRUE) {
  assert_packages(c("rlang", "tibble"))
  
  warn <- function(msg) {
    if (isTRUE(warn_invalid)) rlang::warn(paste0("subsetData: ", msg))
  }
  
  # ---- Basic validation -------------------------------------------------
  if (is.null(data) || !is.list(data)) {
    warn("data is not a list. Returning empty extdata.")
    return(list(extdata = tibble::tibble(), dict = tibble::tibble()))
  }
  
  if (is.null(graph_params) || !is.list(graph_params)) {
    warn("graph_params is not a list. Returning empty extdata.")
    return(list(extdata = tibble::tibble(), dict = tibble::as_tibble(data$dict %||% tibble::tibble())))
  }
  
  freq <- graph_params$data_frequency %||% NA_character_
  freq <- as.character(freq)
  freq <- if (length(freq) >= 1) freq[[1]] else NA_character_
  freq <- stringr::str_to_lower(stringr::str_trim(freq))
  
  if (is.na(freq) || !(freq %in% c("y", "q", "m", "d"))) {
    warn(paste0("Unknown data_frequency='", freq, "'. Expected one of y/q/m/d. Returning empty extdata."))
    return(list(extdata = tibble::tibble(), dict = tibble::as_tibble(data$dict %||% tibble::tibble())))
  }
  
  ext_name <- paste0("extdata_", freq)
  ext <- data[[ext_name]]
  
  if (is.null(ext) || !is.data.frame(ext)) {
    warn(paste0("data[['", ext_name, "']] is missing or not a data.frame/tibble. Returning empty extdata."))
    ext <- tibble::tibble()
  } else {
    ext <- tibble::as_tibble(ext)
  }
  
  dict <- data$dict
  if (is.null(dict) || !is.data.frame(dict)) {
    warn("data$dict is missing or not a data.frame/tibble. Returning empty dict.")
    dict <- tibble::tibble()
  } else {
    dict <- tibble::as_tibble(dict)
  }
  
  # NOTE: country_code / peers_code are not used here (kept for compatibility)
  list(extdata = ext, dict = dict)
}


####### Function to choose the needed function based on the graph type (simply transforming the name) 

funcNameTransform <- function(graph_type) {
  # ---- Contract --------------------------------------------------------
  if (is.null(graph_type)) return(NA_character_)
  
  x <- as.character(graph_type)
  if (length(x) < 1) return(NA_character_)
  
  x <- stringr::str_trim(x[[1]])
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  
  # ---- snake_case -> lowerCamelCase ------------------------------------
  parts <- stringr::str_split(x, "_", simplify = FALSE)[[1]]
  parts <- parts[parts != ""]
  
  if (length(parts) == 0) return(NA_character_)
  
  camel <- c(
    tolower(parts[[1]]),
    stringr::str_to_title(parts[-1])
  )
  
  paste0(camel, collapse = "")
}

