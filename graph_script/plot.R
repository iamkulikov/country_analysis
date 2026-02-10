###### Load libraries and fonts
library_names <- c("dplyr","reshape2","ggplot2","ggthemes","countrycode","readxl","tidyr","data.table","writexl","unikn",
                   "ggtext","svglite","stringr","directlabels","fanplot", "forcats", "rlang",
                   "ggfan",  # проверять, не вернулся ли на CRAN
                   #"hrbrthemes",
                   "glue","readr", "showtext")

#devtools::install_github("jasonhilton/ggfan")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

# ---- Theme ipsum: use hrbrthemes if available, otherwise fallback ----
theme_ipsum_safe <- function(base_size = 12, base_family = "Nunito Sans") {
  if (requireNamespace("hrbrthemes", quietly = TRUE)) {
    return(hrbrthemes::theme_ipsum(base_size = base_size, base_family = base_family))
  }
  
  # Fallback: close enough to keep layouts readable
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(linewidth = 0.3),
      plot.title.position = "plot"
    )
}

theme_ipsum <- theme_ipsum_safe

font_add_google("Nunito Sans", regular.wt = 400, bold.wt = 700)
showtext_opts(dpi = 150)
showtext_auto()

###### Define custom color palettes and modifying themes

## ACRA palette
ACRA <- newpal( col = c(rgb(147, 202, 116, maxColorValue = 255),rgb(153, 38, 115, maxColorValue = 255),
                        rgb(238, 108, 64, maxColorValue = 255),rgb(155, 155, 155, maxColorValue = 255),
                        rgb(238, 162, 53, maxColorValue = 255),rgb(55, 165, 188, maxColorValue = 255),
                        rgb(69, 159, 122, maxColorValue = 255),rgb(115, 144, 159, maxColorValue = 255),
                        rgb(115, 83, 116, maxColorValue = 255),rgb(60, 100, 162, maxColorValue = 255),
                        rgb(63, 133, 165, maxColorValue = 255),rgb(220, 73, 66, maxColorValue = 255),
                        rgb(225, 225, 25, maxColorValue = 255),rgb(145, 30, 180, maxColorValue = 255),
                        rgb(230, 25, 75, maxColorValue = 255),rgb(70, 240, 240, maxColorValue = 255),
                        rgb(240, 50, 230, maxColorValue = 255), rgb(0, 0, 0, maxColorValue = 255),
                        rgb(139, 69, 19, maxColorValue = 255), rgb(255, 0, 0, maxColorValue = 255),
                        rgb(240, 179, 35, maxColorValue = 255)),
                names = c("green", "dark", "red", "grey", "sec1", "sec2", "sec3",
                          "sec4", "sec5", "sec6", "sec7", "sec8", "add1", "add2", 
                          "add3", "add4", "add5", "black", "brown", "reddest", "orange")
)

ipsum_theme <- function(base_size = 12, base_family = "Nunito Sans") {
  theme_ipsum() + theme( axis.line.x = element_line(color = "black", size = 3) )
}

# seecol(ACRA,
#       col_brd = "white", lwd_brd = 4,
#       title = "Colours of ACRA",
#       mar_note = "For fuck's sake")
#using +scale_color_manual(values = ACRA)


####### Function to get country's codes and the codes of its peers

assert_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    rlang::abort(
      paste0("Missing required packages: ", paste(missing, collapse = ", "), ".")
    )
  }
}

normalize_iso3 <- function(x) {
  x <- stringr::str_trim(toupper(as.character(x)))
  
  dplyr::case_when(
    x == "CHI" ~ "CHN",
    TRUE ~ x
  )
}

safe_iso3_to_iso2 <- function(iso3) {
  iso3 <- normalize_iso3(iso3)
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
  
  country_iso3c <- normalize_iso3(country_iso3c %||% "")
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
    normalize_iso3()
  
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
      country_code = normalize_iso3(.data$country_code),
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
  peers_default_iso3c <- normalize_iso3(peers_default_iso3c)
  peers_default_iso3c <- peers_default_iso3c[!is.na(peers_default_iso3c) & peers_default_iso3c != ""]
  peers_default_iso3c <- setdiff(peers_default_iso3c, country_iso3c)
  
  peers_default_iso2c <- safe_iso3_to_iso2(peers_default_iso3c)
  
  # Neighbours peers: same region from peers matrix
  peers_neighbours_iso3c <- peers_df |>
    dplyr::filter(.data$region == country_region) |>
    dplyr::pull(.data$country_code) |>
    normalize_iso3()
  
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

getPlotSchedule <- function(plotparam_fname, dict, warn_unmatched = TRUE) {
  assert_packages(c("readxl", "dplyr", "tidyr", "stringr", "tibble", "rlang"))
  
  # ---- Validate inputs -------------------------------------------------
  needed_dict_cols <- c("indicator_code", "source_name")
  missing_dict_cols <- setdiff(needed_dict_cols, names(dict))
  if (length(missing_dict_cols) > 0) {
    rlang::abort(paste0(
      "dict is missing required columns: ",
      paste(missing_dict_cols, collapse = ", "),
      "."
    ))
  }
  
  has_freq <- "source_frequency" %in% names(dict)
  
  # ---- Read plot params ------------------------------------------------
  graphplan <- readxl::read_excel(plotparam_fname, sheet = "library", col_names = TRUE, skip = 1)
  
  if (!("indicators" %in% names(graphplan))) {
    rlang::abort("plotparam sheet 'library' must contain column 'indicators'.")
  }
  
  has_plan_freq <- "data_frequency" %in% names(graphplan)
  
  # ---- Dict source map -------------------------------------------------
  dict_sources <- dict |>
    dplyr::transmute(
      indicator_code = stringr::str_trim(as.character(.data$indicator_code)),
      source_name    = as.character(.data$source_name),
      source_frequency = if (has_freq) as.character(.data$source_frequency) else NA_character_
    ) |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "")
  
  # ---- Parse indicators safely (no regex) ------------------------------
  long <- graphplan |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::mutate(
      indicators_vec = stringr::str_split(
        string = as.character(.data$indicators),
        pattern = ",\\s*"
      )
    ) |>
    tidyr::unnest_longer(.data$indicators_vec, values_to = "indicator_code") |>
    dplyr::mutate(
      indicator_code = stringr::str_trim(as.character(.data$indicator_code))
    ) |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "")
  
  # ---- Join to dict (prefer frequency-aware) ---------------------------
  if (has_freq && has_plan_freq) {
    long <- long |>
      dplyr::left_join(
        dict_sources,
        by = dplyr::join_by(indicator_code == indicator_code,
                            data_frequency == source_frequency)
      )
  } else {
    long <- long |>
      dplyr::left_join(
        dict_sources |>
          dplyr::select(.data$indicator_code, .data$source_name) |>
          dplyr::distinct(),
        by = dplyr::join_by(indicator_code == indicator_code)
      )
  }
  
  # ---- Optional warnings for unmatched indicators (active only) --------
  if (isTRUE(warn_unmatched)) {
    
    # determine which rows are active
    active_rows <- graphplan |>
      dplyr::mutate(
        .row_id = dplyr::row_number(),
        active_flag = dplyr::case_when(
          "active" %in% names(graphplan) ~ (.data$active == 1),
          TRUE ~ TRUE
        )
      ) |>
      dplyr::filter(.data$active_flag) |>
      dplyr::pull(.data$.row_id)
    
    unmatched_tbl <- long |>
      dplyr::filter(
        .data$.row_id %in% active_rows,
        is.na(.data$source_name)
      ) |>
      dplyr::summarise(
        unmatched = list(unique(.data$indicator_code)),
        .by = .data$.row_id
      ) |>
      dplyr::filter(lengths(.data$unmatched) > 0)
    
    if (nrow(unmatched_tbl) > 0) {
      graph_names <- graphplan |>
        dplyr::mutate(.row_id = dplyr::row_number()) |>
        dplyr::select(.data$.row_id, dplyr::any_of("graph_name"))
      
      unmatched_msg <- unmatched_tbl |>
        dplyr::left_join(graph_names, by = dplyr::join_by(.row_id)) |>
        dplyr::mutate(
          graph_name = dplyr::coalesce(
            as.character(.data$graph_name),
            paste0("row_", .data$.row_id)
          ),
          unmatched_str = vapply(
            .data$unmatched,
            \(x) paste(x, collapse = ", "),
            character(1)
          )
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
  
  # ---- Aggregate sources per row --------------------------------------
  sources_by_row <- long |>
    dplyr::summarise(
      source_name = {
        s <- .data$source_name
        s <- s[!is.na(s)]
        s <- s[s != "расчеты АКРА"]
        s <- unique(s)
        s <- c(s, "расчеты АКРА")  # always
        paste(s, collapse = ", ")
      },
      .by = .data$.row_id
    )
  
  # Rows with no parsed indicators -> keep ACRA only
  graphplan_out <- graphplan |>
    dplyr::mutate(.row_id = dplyr::row_number()) |>
    dplyr::left_join(sources_by_row, by = dplyr::join_by(.row_id)) |>
    dplyr::mutate(
      source_name = dplyr::coalesce(.data$source_name, "расчеты АКРА")
    ) |>
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
    "scatter_country_comparison", "structure_country_comparison",
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
    caption = caption, title = title, theme_name = theme_name, all = all,
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
    if (peers_type %in% names(country_info$regions)) {
      if (!("country_iso2c" %in% names(country_info$regions))) {
        warn("country_info$regions has no 'country_iso2c'; cannot use region groups.")
        return(character(0))
      }
      
      vec <- country_info$regions |>
        dplyr::filter(dplyr::coalesce(.data[[peers_type]], 0) == 1) |>
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

assert_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    rlang::abort(paste0("Missing required packages: ", paste(missing, collapse = ", "), "."))
  }
}

warn_active <- function(params, msg, warn_invalid = TRUE) {
  if (!isTRUE(warn_invalid)) return(invisible(NULL))
  active <- params$active %||% 0L
  if (isTRUE(as.integer(active) == 1L)) {
    rlang::warn(paste0("fillGraphPlan: ", msg))
  }
  invisible(NULL)
}

choose_extdata <- function(data, freq, warn_invalid = TRUE, params_for_warn = list(active = 1L)) {
  assert_packages(c("tibble", "stringr", "rlang"))
  
  f <- freq |>
    as.character() |>
    (\(x) if (length(x) >= 1) x[[1]] else NA_character_)() |>
    stringr::str_trim() |>
    stringr::str_to_lower()
  
  if (is.na(f) || !(f %in% c("y", "q", "m", "d"))) {
    warn_active(params_for_warn, paste0("Unknown data_frequency='", freq, "'. Returning empty extdata."), warn_invalid)
    return(tibble::tibble())
  }
  
  nm <- paste0("extdata_", f)
  ext <- data[[nm]]
  
  if (is.null(ext) || !is.data.frame(ext)) {
    warn_active(params_for_warn, paste0("data[['", nm, "']] is missing/malformed. Returning empty extdata."), warn_invalid)
    return(tibble::tibble())
  }
  
  tibble::as_tibble(ext)
}

# Parse a single time token into parts (year, sub) given target freq.
# Accepts:
# - numeric scalar year (for y)
# - numeric vector c(year, sub) (for q/m)
# - strings like "2020", "2020q4", "2020m12"
parse_time_parts <- function(x, freq) {
  assert_packages(c("stringr"))
  
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  if (is.na(f) || !(f %in% c("y", "q", "m"))) return(list(ok = FALSE))
  
  # numeric vector already
  if (is.numeric(x)) {
    if (f == "y" && length(x) >= 1) return(list(ok = TRUE, year = as.integer(x[[1]]), sub = NA_integer_))
    if (f %in% c("q", "m") && length(x) >= 2) return(list(ok = TRUE, year = as.integer(x[[1]]), sub = as.integer(x[[2]])))
  }
  
  # character
  x1 <- as.character(x)
  x1 <- if (length(x1) >= 1) x1[[1]] else NA_character_
  x1 <- stringr::str_trim(x1)
  if (is.na(x1) || !nzchar(x1)) return(list(ok = FALSE))
  
  if (stringr::str_detect(x1, "^(19|20)\\d{2}$")) {
    return(list(ok = TRUE, year = as.integer(x1), sub = NA_integer_))
  }
  
  if (f == "q" && stringr::str_detect(stringr::str_to_lower(x1), "^(19|20)\\d{2}q[1-4]$")) {
    year <- as.integer(stringr::str_sub(x1, 1, 4))
    sub  <- as.integer(stringr::str_sub(x1, 6, 6))
    return(list(ok = TRUE, year = year, sub = sub))
  }
  
  if (f == "m" && stringr::str_detect(stringr::str_to_lower(x1), "^(19|20)\\d{2}m(1[0-2]|[1-9])$")) {
    year <- as.integer(stringr::str_sub(x1, 1, 4))
    sub  <- as.integer(stringr::str_sub(stringr::str_to_lower(x1), 6))
    return(list(ok = TRUE, year = year, sub = sub))
  }
  
  list(ok = FALSE)
}

time_to_index <- function(year, sub = NA_integer_, freq, origin_year = 1987L) {
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  y <- suppressWarnings(as.integer(year))
  s <- suppressWarnings(as.integer(sub))
  
  if (is.na(y)) return(NA_real_)
  
  if (f == "y") return(as.numeric(y - origin_year))
  if (f == "q") return(as.numeric((y - origin_year) * 4L + s))
  if (f == "m") return(as.numeric((y - origin_year) * 12L + s))
  
  NA_real_
}

default_time_parts <- function(freq, which = c("min", "max")) {
  which <- match.arg(which)
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  
  if (f == "y") return(list(year = 2030L, sub = NA_integer_))
  if (f == "q") return(list(year = 2030L, sub = if (which == "min") 1L else 4L))
  if (f == "m") return(list(year = 2030L, sub = if (which == "min") 1L else 12L))
  list(year = 2030L, sub = NA_integer_)
}

make_time_label <- function(year, sub = NA_integer_, freq) {
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  if (f == "y") return(as.character(year))
  if (f %in% c("q", "m")) return(paste0(year, f, sub))
  NA_character_
}

derive_dynamic_window_from_data <- function(extdata, params, country_iso2c, peers_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tidyr", "tibble", "stringr", "rlang"))
  
  graph_type <- params$graph_type %||% NA_character_
  freq       <- params$data_frequency %||% NA_character_
  indicators <- params$indicators %||% character(0)
  
  # Default return = "no dynamic window"
  out <- list(
    x_min_parts = list(ok = FALSE),
    x_max_parts = list(ok = FALSE),
    time_start = NA_real_,
    time_end = NA_real_,
    labfreq = NA_real_,
    timetony_start = NA_real_,
    timetony_end = NA_real_
  )
  
  if (is.na(graph_type) || !nzchar(graph_type)) return(out)
  
  dynamic_types <- c(
    "bar_dynamic", "lines_country_comparison", "lines_indicator_comparison",
    "distribution_dynamic", "structure_dynamic", "structure_dynamic_norm",
    "scatter_dynamic", "bar_year_comparison", "distribution_year_comparison"
  )
  if (!(graph_type %in% dynamic_types)) return(out)
  
  if (!("time" %in% names(extdata))) {
    warn_active(params, "extdata has no column 'time'; cannot derive dynamic window.", warn_invalid)
    return(out)
  }
  
  ext <- extdata |>
    dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month", "country_id")), dplyr::all_of(indicators)) |>
    dplyr::arrange(.data$time)
  
  countries_needed <- unique(c(country_iso2c, peers_iso2c))
  countries_needed <- countries_needed[!is.na(countries_needed) & countries_needed != ""]
  
  # --- x_min / x_max parsing if user set them (time tokens) -------------
  x_min_raw <- params$x_min %||% NA
  x_max_raw <- params$x_max %||% NA
  
  # If user already provided time bounds, try to parse them to parts.
  # (We accept either c(year, sub) numeric or strings "YYYYqN"/"YYYYmN"/"YYYY".)
  x_min_parts <- parse_time_parts(x_min_raw, freq)
  x_max_parts <- parse_time_parts(x_max_raw, freq)
  
  # --- If missing, derive from data (keep behavior close to old code) ---
  if (!isTRUE(x_min_parts$ok) || !isTRUE(x_max_parts$ok)) {
    
    # Case A: need enough indicators for the main country
    main_country_types <- c(
      "bar_dynamic", "lines_indicator_comparison",
      "structure_dynamic", "structure_dynamic_norm", "scatter_dynamic",
      "bar_year_comparison", "distribution_year_comparison"
    )
    
    # Case B: need enough countries for a fixed indicator
    countries_types <- c("lines_country_comparison", "distribution_dynamic")
    
    # Helper: find first/last row for main country where rowSums non-NA >= required
    find_country_row <- function(which = c("first", "last")) {
      which <- match.arg(which)
      
      required_n <- if (graph_type %in% c("structure_dynamic", "scatter_dynamic", "bar_year_comparison", "distribution_year_comparison")) {
        length(indicators)
      } else {
        1L
      }
      
      ext |>
        dplyr::filter(.data$country_id == country_iso2c) |>
        dplyr::mutate(
          .n_ok = rowSums(!is.na(dplyr::pick(dplyr::all_of(indicators))))
        ) |>
        dplyr::filter(.data$.n_ok >= required_n) |>
        (\(df) if (which == "first") dplyr::slice_head(df, n = 1) else dplyr::slice_tail(df, n = 1))()
    }
    
    # Helper: find first/last row where enough countries have non-NA for indicators[1]
    find_countries_row <- function(which = c("first", "last")) {
      which <- match.arg(which)
      
      ind1 <- indicators[[1]] %||% NA_character_
      if (is.na(ind1) || !nzchar(ind1)) return(tibble::tibble())
      
      threshold <- if (identical(graph_type, "distribution_dynamic")) 40L else 1L
      
      ext |>
        dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month", "country_id")), dplyr::all_of(ind1)) |>
        dplyr::filter(.data$country_id %in% countries_needed) |>
        tidyr::pivot_wider(names_from = .data$country_id, values_from = dplyr::all_of(ind1)) |>
        dplyr::mutate(
          .n_ok = rowSums(!is.na(dplyr::pick(dplyr::all_of(countries_needed))))
        ) |>
        dplyr::filter(.data$.n_ok >= threshold) |>
        (\(df) if (which == "first") dplyr::slice_head(df, n = 1) else dplyr::slice_tail(df, n = 1))()
    }
    
    # Derive x_min if needed
    if (!isTRUE(x_min_parts$ok)) {
      row_min <- if (graph_type %in% main_country_types) find_country_row("first") else find_countries_row("first")
      
      if (nrow(row_min) == 0) {
        warn_active(params, "Could not derive x_min from data; using fallback.", warn_invalid)
        fb <- default_time_parts(freq, "min")
        x_min_parts <- list(ok = TRUE, year = fb$year, sub = fb$sub)
      } else {
        y <- row_min$year[[1]] %||% NA_integer_
        sub <- if (stringr::str_to_lower(freq) == "q") row_min$quarter[[1]] else if (stringr::str_to_lower(freq) == "m") row_min$month[[1]] else NA_integer_
        x_min_parts <- list(ok = TRUE, year = as.integer(y), sub = as.integer(sub))
      }
    }
    
    # Derive x_max if needed
    if (!isTRUE(x_max_parts$ok)) {
      row_max <- if (graph_type %in% main_country_types) find_country_row("last") else find_countries_row("last")
      
      if (nrow(row_max) == 0) {
        warn_active(params, "Could not derive x_max from data; using fallback.", warn_invalid)
        fb <- default_time_parts(freq, "max")
        x_max_parts <- list(ok = TRUE, year = fb$year, sub = fb$sub)
      } else {
        y <- row_max$year[[1]] %||% NA_integer_
        sub <- if (stringr::str_to_lower(freq) == "q") row_max$quarter[[1]] else if (stringr::str_to_lower(freq) == "m") row_max$month[[1]] else NA_integer_
        x_max_parts <- list(ok = TRUE, year = as.integer(y), sub = as.integer(sub))
      }
    }
  }
  
  # --- Convert to internal time index scale (origin_year fixed by contract) ---
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  y0 <- 1987L
  
  ts <- time_to_index(x_min_parts$year, x_min_parts$sub, f, origin_year = y0)
  te <- time_to_index(x_max_parts$year, x_max_parts$sub, f, origin_year = y0)
  
  if (is.na(ts) || is.na(te)) {
    warn_active(params, "Failed to convert x_min/x_max to internal time index; leaving time window NA.", warn_invalid)
    return(out)
  }
  
  if (f == "y") {
    labfreq <- 1
    tony_s <- 0
    tony_e <- 1
  } else if (f == "q") {
    labfreq <- 4
    tony_s <- (5L - as.integer(x_min_parts$sub)) %% 4L
    tony_e <- as.integer(x_max_parts$sub)
  } else if (f == "m") {
    labfreq <- 12
    tony_s <- (13L - as.integer(x_min_parts$sub)) %% 12L
    tony_e <- as.integer(x_max_parts$sub)
  } else {
    labfreq <- NA_real_
    tony_s <- NA_real_
    tony_e <- NA_real_
  }
  
  list(
    x_min_parts = x_min_parts,
    x_max_parts = x_max_parts,
    time_start = ts,
    time_end = te,
    labfreq = labfreq,
    timetony_start = tony_s,
    timetony_end = tony_e
  )
}

derive_cross_time_fix <- function(extdata, params, country_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tibble", "stringr"))
  
  graph_type <- params$graph_type %||% NA_character_
  freq       <- params$data_frequency %||% NA_character_
  indicators <- params$indicators %||% character(0)
  
  out <- list(time_fix = NA_real_, time_fix_label = NA_character_, time_fix_parts = list(ok = FALSE))
  
  cross_types <- c(
    "bar_year_comparison", "distribution_year_comparison",
    "scatter_country_comparison", "bar_country_comparison",
    "structure_country_comparison", "structure_country_comparison_norm",
    "density_fix", "triangle"
  )
  if (is.na(graph_type) || !(graph_type %in% cross_types)) return(out)
  
  # Special: year-comparison uses a vector of years (not internal time index)
  if (graph_type %in% c("bar_year_comparison", "distribution_year_comparison")) {
    tf_raw <- params$time_fix %||% NA_character_
    
    if (!is.na(tf_raw) && nzchar(tf_raw)) {
      years <- tf_raw |>
        stringr::str_split(",") |>
        purrr::pluck(1) |>
        stringr::str_trim() |>
        (\(x) x[x != ""])() |>
        suppressWarnings(as.integer())
      
      years <- years[!is.na(years)]
      if (length(years) > 0) {
        return(list(time_fix = years, time_fix_label = paste(years, collapse = ", "), time_fix_parts = list(ok = TRUE)))
      }
    }
    
    # fallback: use x_min/x_max year bounds if available
    xmin <- params$x_min %||% NA
    xmax <- params$x_max %||% NA
    pmin <- parse_time_parts(xmin, "y")
    pmax <- parse_time_parts(xmax, "y")
    
    years <- suppressWarnings(as.integer(c(pmin$year %||% NA_integer_, pmax$year %||% NA_integer_)))
    years <- years[!is.na(years)]
    if (length(years) > 0) {
      return(list(time_fix = years, time_fix_label = paste(years, collapse = ", "), time_fix_parts = list(ok = TRUE)))
    }
    
    warn_active(params, "Could not derive time_fix for year-comparison; leaving NA.", warn_invalid)
    return(out)
  }
  
  # Other cross types: time_fix is a single point (internal time index) + label
  tf_raw <- params$time_fix %||% NA
  tf_parts <- parse_time_parts(tf_raw, freq)
  
  if (!isTRUE(tf_parts$ok)) {
    
    if (!("time" %in% names(extdata))) {
      warn_active(params, "extdata has no column 'time'; cannot derive time_fix.", warn_invalid)
      return(out)
    }
    
    ext <- extdata |>
      dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month", "country_id")), dplyr::all_of(indicators)) |>
      dplyr::filter(.data$country_id == country_iso2c) |>
      dplyr::arrange(.data$time)
    
    # Need ALL indicators for main country (as in old code)
    ext_ok <- ext |>
      dplyr::mutate(.n_ok = rowSums(!is.na(dplyr::pick(dplyr::all_of(indicators))))) |>
      dplyr::filter(.data$.n_ok == length(indicators))
    
    if (nrow(ext_ok) == 0) {
      warn_active(params, "Could not derive time_fix from data; using fallback.", warn_invalid)
      fb <- default_time_parts(freq, "max")
      tf_parts <- list(ok = TRUE, year = fb$year, sub = fb$sub)
    } else {
      last <- dplyr::slice_tail(ext_ok, n = 1)
      y <- last$year[[1]] %||% NA_integer_
      sub <- if (stringr::str_to_lower(freq) == "q") last$quarter[[1]] else if (stringr::str_to_lower(freq) == "m") last$month[[1]] else NA_integer_
      tf_parts <- list(ok = TRUE, year = as.integer(y), sub = as.integer(sub))
    }
  }
  
  label <- make_time_label(tf_parts$year, tf_parts$sub, freq)
  idx <- time_to_index(tf_parts$year, tf_parts$sub, freq, origin_year = 1987L)
  
  list(time_fix = idx, time_fix_label = label, time_fix_parts = tf_parts)
}

lookup_indicator_label <- function(dict, indicator_code, freq) {
  assert_packages(c("dplyr", "tibble", "stringr"))
  
  if (is.null(dict) || !is.data.frame(dict)) return(NA_character_)
  d <- tibble::as_tibble(dict)
  
  if (!all(c("indicator_code", "indicator") %in% names(d))) return(NA_character_)
  
  f <- stringr::str_to_lower(stringr::str_trim(as.character(freq)))
  has_freq <- "source_frequency" %in% names(d)
  
  hit <- if (has_freq) {
    d |>
      dplyr::filter(.data$indicator_code == indicator_code, .data$source_frequency == f) |>
      dplyr::slice_head(n = 1)
  } else {
    d |>
      dplyr::filter(.data$indicator_code == indicator_code) |>
      dplyr::slice_head(n = 1)
  }
  
  if (nrow(hit) == 0) return(NA_character_)
  as.character(hit$indicator[[1]])
}

derive_axis_labels <- function(dict, params, warn_invalid = TRUE) {
  assert_packages(c("stringr"))
  
  freq <- params$data_frequency %||% NA_character_
  x_ind <- params$x_ind %||% NA_character_
  y_ind <- params$y_ind %||% NA_character_
  
  time_fix_label <- params$time_fix_label %||% NA_character_
  add_time <- !is.na(time_fix_label) && nzchar(time_fix_label)
  
  x_name <- if (!is.na(x_ind) && nzchar(x_ind)) lookup_indicator_label(dict, x_ind, freq) else NA_character_
  y_name <- if (!is.na(y_ind) && nzchar(y_ind)) lookup_indicator_label(dict, y_ind, freq) else NA_character_
  
  if (is.na(x_name) && !is.na(x_ind) && nzchar(x_ind)) {
    warn_active(params, paste0("Dict label not found for x_ind='", x_ind, "' (freq=", freq, ")."), warn_invalid)
  }
  if (is.na(y_name) && !is.na(y_ind) && nzchar(y_ind)) {
    warn_active(params, paste0("Dict label not found for y_ind='", y_ind, "' (freq=", freq, ")."), warn_invalid)
  }
  
  x_log <- as.integer(params$x_log %||% 0L)
  y_log <- as.integer(params$y_log %||% 0L)
  
  # NOTE: semantics chosen by you: if *_log == 1, axis is in log10-space
  x_base <- dplyr::coalesce(x_name, x_ind, "")
  y_base <- dplyr::coalesce(y_name, y_ind, "")
  
  x_lab <- if (x_log == 1L) paste0("log10(", x_base, ")") else x_base
  y_lab <- if (y_log == 1L) paste0("log10(", y_base, ")") else y_base
  
  if (add_time) {
    if (nzchar(x_lab)) x_lab <- paste0(x_lab, ", ", time_fix_label)
    if (nzchar(y_lab)) y_lab <- paste0(y_lab, ", ", time_fix_label)
  }
  
  list(x_lab = x_lab, y_lab = y_lab)
}

# ------------------------------------------------------------------------------
# fillGraphPlan() v1
# ------------------------------------------------------------------------------
fillGraphPlan <- function(parsedrow, data, country_iso2c, peers_iso2c, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tibble", "stringr", "rlang"))
  
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
  
  # Shallow copy + normalize a few scalars
  params <- parsedrow
  params$active <- as.integer(params$active %||% 0L)
  params$data_frequency <- params$data_frequency %||% "y"
  params$graph_type <- params$graph_type %||% NA_character_
  
  # Pull dict and extdata
  dict <- data$dict %||% tibble::tibble()
  ext  <- choose_extdata(data, params$data_frequency, warn_invalid = warn_invalid, params_for_warn = params)
  
  # Peers: runtime representation is vector character (your choice)
  peers_iso2c <- as.character(peers_iso2c %||% character(0))
  peers_iso2c <- stringr::str_trim(peers_iso2c)
  peers_iso2c <- peers_iso2c[!is.na(peers_iso2c) & peers_iso2c != ""]
  peers_iso2c <- unique(peers_iso2c)
  
  params$country_iso2c <- as.character(country_iso2c %||% NA_character_)
  params$peers_iso2c <- peers_iso2c
  
  # --- 1) Dynamic window derived fields (time_start/time_end/labels helpers) ---
  dyn <- derive_dynamic_window_from_data(
    extdata = ext,
    params = params,
    country_iso2c = params$country_iso2c,
    peers_iso2c = peers_iso2c,
    warn_invalid = warn_invalid
  )
  
  params$x_min_parts <- dyn$x_min_parts
  params$x_max_parts <- dyn$x_max_parts
  params$time_start <- dyn$time_start
  params$time_end <- dyn$time_end
  params$labfreq <- dyn$labfreq
  params$timetony_start <- dyn$timetony_start
  params$timetony_end <- dyn$timetony_end
  
  # For downstream convenience (preserve old-style x_min/x_max as numeric vectors)
  freq <- stringr::str_to_lower(stringr::str_trim(as.character(params$data_frequency)))
  if (isTRUE(dyn$x_min_parts$ok)) {
    params$x_min <- if (freq == "y") dyn$x_min_parts$year else c(dyn$x_min_parts$year, dyn$x_min_parts$sub)
  }
  if (isTRUE(dyn$x_max_parts$ok)) {
    params$x_max <- if (freq == "y") dyn$x_max_parts$year else c(dyn$x_max_parts$year, dyn$x_max_parts$sub)
  }
  
  # --- 2) Cross-section time point (time_fix + label) ---
  cross <- derive_cross_time_fix(
    extdata = ext,
    params = params,
    country_iso2c = params$country_iso2c,
    warn_invalid = warn_invalid
  )
  
  params$time_fix <- cross$time_fix
  params$time_fix_label <- cross$time_fix_label
  params$time_fix_parts <- cross$time_fix_parts
  
  # --- 3) Axis labels (do NOT mutate x_ind/y_ind; log is a space semantics) ---
  labs <- derive_axis_labels(dict = dict, params = params, warn_invalid = warn_invalid)
  params$x_lab <- labs$x_lab
  params$y_lab <- labs$y_lab
  
  # --- 4) Keep compatibility fields that some plots expect --------------------
  # Caption default (keep your old behavior)
  if (is.null(params$caption) || is.na(params$caption) || !nzchar(as.character(params$caption))) {
    src <- params$source_name %||% NA_character_
    params$caption <- if (is.na(src)) "Источники:" else paste0("Источники: ", src)
  }
  
  # Placeholder marker (not a failure; plotting layer will decide)
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

###### Scatter dynamic

# year <- function(x) as.POSIXlt(x)$year + 1900
# ggplot(economics, aes(unemploy / pop, uempmed)) + 
#   geom_path(colour = "grey50") +
#   geom_point(aes(colour = year(date)))


###### Scatter plot

scatterCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {

  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_all <- data$extdata |> filter(time==time_fix) |> select("country", "country_id", "year", all_of(indicators))
  data_highlight <- data_all |> filter(country_id==country_iso2c)
  if (peers!=0) {data_peers <- data_all |> filter(country_id %in% peers_iso2c)} else {data_peers <- NULL}
  x_min <- as.numeric(x_min); x_max <- as.numeric(x_max); y_min <- as.numeric(y_min); y_max <- as.numeric(y_max)
  
  eval(parse(text = paste("theplot <- ggplot(data = data_all, aes(", x_ind, ",", y_ind, "))", sep="") ))
  theplot <- theplot + geom_point(alpha=ifelse(all==1, 1, 0), fill="#619cff", color=ACRA['green'])
  theplot <- theplot + scale_x_continuous(limits=c(x_min, x_max)) + scale_y_continuous(limits=c(y_min, y_max)) +
    geom_smooth(formula = y ~ x, method = trend_type) + ggtitle(title) + labs(x = x_lab, y = y_lab, caption = caption)
  
  eval(parse(text = paste("theplot <- theplot + geom_point(data=data_peers, aes(", x_ind, ", ", y_ind, "), color=ACRA['sec2'], alpha=1, size=3)", sep="") ))
  eval(parse(text = paste("theplot <- theplot + geom_point(data=data_highlight, aes(", x_ind, ", ", y_ind, "), color=ACRA['dark'], alpha=1, size=3)", sep="") ))
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + geom_text(data=data_peers, aes(label = country_id), 
                                 nudge_x = 0.03*(layer_scales(theplot)$x$range$range[2]-layer_scales(theplot)$x$range$range[1]), 
                                 nudge_y = 0.03*(layer_scales(theplot)$y$range$range[2]-layer_scales(theplot)$y$range$range[1]), 
                                 check_overlap = F, colour = ACRA['sec2']) +
    geom_text(data=data_highlight, aes(label = country_id), 
              nudge_x = 0.03*(layer_scales(theplot)$x$range$range[2]-layer_scales(theplot)$x$range$range[1]), 
              nudge_y = 0.03*(layer_scales(theplot)$y$range$range[2]-layer_scales(theplot)$y$range$range[1]), 
              check_overlap = F, colour=ACRA['dark'])
  theplot <- theplot + theme(plot.title = element_textbox_simple()) + 
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  # if (identical(normalize_theme_name(graph_params$theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  
  return(list(graph = theplot, data = data_all)) 
}

###### Scatter before-after
# to-do

###### Bar dynamic - dodged, stacked or stacked and normalized

barDynamic <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_temp <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, time >= time_start, time <= time_end, country_id == country_iso2c, !is.na(value)) 
  if (is.na(sec_y_axis)==F) { data_all <- data_all |> mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  if (graph_type=="structure_dynamic") {data_total <- data_all |> group_by(time) |> summarize(total=sum(value)) |> ungroup()}
  
  dict_temp <- data$dict |> filter(indicator_code %in% indicators, source_frequency == data_frequency) |> 
    arrange(factor(indicator_code, levels = indicators)) |> select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  theplot <- ggplot(data_all, aes(time, value, fill=as.factor(variable)))
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  
  theplot <- theplot + scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                                          labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1])) 
  
  if (graph_type=="bar_dynamic") {theplot <- theplot + geom_col(alpha=.8, width=0.65, position = position_dodge(width = 0.65)) } else {
    theplot <- theplot + geom_col(alpha=.8, width=0.65, 
                                  position = case_when(graph_type=="structure_dynamic" ~ "stack", graph_type=="structure_dynamic_norm" ~ "fill"))
  }
  
  if (graph_type=="structure_dynamic") {theplot <- theplot + stat_summary(fun = sum, geom = "point",
                                                                          shape = 17, size = 2, mapping = aes(group = time), show.legend = F)}
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6','black','brown','sec5','sec7', 'sec8',
                                                'add1', 'add2', 'reddest', 'add4', 'add5')]), name="", labels=y_lab) +
    ggtitle(title) + labs(caption = caption, x=NULL, y=NULL) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), legend.position="bottom")
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}

structureDynamic <- barDynamic
structureDynamicNorm <- barDynamic


###### Bar country comparison - dodged, stacked or stacked and normalized

barCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  dict_temp <- data$dict |> filter(indicator_code %in% indicators, source_frequency == data_frequency) |> 
    arrange(factor(indicator_code, levels = indicators)) |> select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  data_temp <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators))) 
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, time==time_fix, !is.na(value)) |>
    mutate(highlight2 = as.factor(ifelse(country_id==country_iso2c,1,0))) 
  
  if (length(indicators)==1) {data_all <- data_all |> mutate(highlight = as.factor(case_when(country_id %in% peers_iso2c ~ 1,
                                                                                              country_id %in% country_iso2c ~ 2, TRUE ~ 0)) )}
  if (is.na(sec_y_axis)==F) { data_all <- data_all |> mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  
  if (all==1) {} else {data_all <- data_all |> filter(country_id %in% c(peers_iso2c, country_iso2c)) }
  
  ordering_table <- data_all |> filter(variable == indicators[1]) |> select(country_id, value) |> arrange(desc(value)) |> 
    mutate(ordering = row_number()) |> select(country_id, ordering)
  data_all <- data_all |> left_join(ordering_table, by="country_id") |> arrange(desc(variable), ordering)
  data_all <- data_all |> mutate(variable = factor(variable, levels = indicators))
  
  if (length(indicators)==1) {theplot <- ggplot(data_all, aes(reorder(country_id, -value), value, fill = highlight)) } else 
  {theplot <- ggplot(data_all, aes(reorder(country_id, ordering), value, fill = variable)) }
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + geom_col(aes(size=highlight2), color="black", alpha=.8,
                                position=case_when(graph_type=="bar_country_comparison" ~ "dodge", 
                                                   graph_type=="structure_country_comparison" ~ "stack",
                                                   graph_type=="structure_country_comparison_norm" ~ "fill"))
  
  theplot <- theplot + annotate(geom = 'text', label = time_fix_label, x = Inf, y = Inf, hjust = 1.5, vjust = 1.5)
    
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  theplot <- theplot + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6','black','brown','sec5','sec7', 'sec8',
                                                'add1', 'add2', 'reddest', 'add4', 'add5')]), name="", labels=y_lab) +
    labs(x=NULL, y = ifelse(length(indicators)==1, y_lab, ""), caption = caption) + ggtitle(title) +
    guides(size="none") + scale_size_manual(values = c(0,1)) +
    theme(plot.title = element_textbox_simple(), legend.position="bottom") + 
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  if (length(indicators)==1) {theplot <- theplot + guides(fill = "none")}
  
  if (all==1) {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
  }  else {
    theplot <- theplot + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1))
  }
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}

structureCountryComparison <- barCountryComparison
structureCountryComparisonNorm <- barCountryComparison

###### Bar multi-year comparison for multiple variables

barYearComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_all <- reshape2::melt(data$extdata, id.vars=c("country", "country_id", "year"), variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, year %in% time_fix, country_id == country_iso2c, !is.na(value)) |> 
    mutate(variable = fct_relevel(variable, indicators), value = as.numeric(value))
  
  dict_temp <- data$dict |> filter(indicator_code %in% indicators, source_frequency == data_frequency) |> 
    arrange(factor(indicator_code, levels = indicators)) |> select(indicator)
  x_lab <- unname(unlist(dict_temp))
  x_lab <- str_wrap(x_lab, width = 15)
  
  theplot <- ggplot(data_all, aes(variable, value, fill=as.factor(year))) + geom_col(alpha=.8, position ='dodge') +
    scale_y_continuous(limits=c(y_min, y_max)) + 
    scale_fill_manual(values = as.vector(ACRA[c('green','sec2','dark','sec1','red','sec3','sec6')]), name="Год" ) +
    scale_x_discrete(labels=x_lab) +
    ggtitle(title) + labs(caption = caption, x=NULL, y=NULL)
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  theplot <- theplot + theme(plot.title = element_textbox_simple(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}


###### Lines indicator comparison

linesIndicatorComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  dict_temp <- data$dict |> filter(indicator_code %in% indicators, source_frequency == data_frequency) |> 
    arrange(factor(indicator_code, levels = indicators)) |> select(indicator)
  y_lab <- unname(unlist(dict_temp))
  if (is.na(sec_y_axis)==F) {
    y_lab[indicators %in% indicators_sec] <- paste(y_lab[indicators %in% indicators_sec], ", rha", sep="")
    y_lab[!(indicators %in% indicators_sec)] <- paste(y_lab[!(indicators %in% indicators_sec)], ", lha", sep="")
  }
  
  data_temp <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, time >= time_start, time <= time_end, country_id == country_iso2c, !is.na(value)) 
  data_all <- data_all |> left_join(data$dict, by=c("variable"="indicator_code")) |> filter(source_frequency == data_frequency)
  if (is.na(sec_y_axis)==F) { data_all <- data_all |> mutate(value = ifelse(variable %in% indicators_sec, value*coeff, value)) }
  
  theplot <- ggplot(data_all, aes(time, value)) + geom_line(aes(group=variable, color=variable), alpha=1, size=2)
  if (is.na(sec_y_axis)==T) {theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max))} else {
    theplot <- theplot + scale_y_continuous(limits=c(y_min, y_max), sec.axis = sec_axis(~./coeff))
  }
  
  theplot <- theplot + scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                                          labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1])) +
    scale_color_manual(values = as.vector(ACRA[c('dark','sec2','green','sec1','sec6')]) ) +
    ggtitle(title) + labs(caption = caption, x="Год", y=NULL) + 
    geom_dl(aes(label = indicator, colour = variable), method = list(dl.trans(x = x + 0.3), "extreme.grid", cex = 0.8, alpha=0.8))
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + theme(plot.title = element_textbox_simple(), plot.margin = margin(r = 15)) +
    labs(caption = caption, x=NULL, y=NULL) + guides(colour = "none") +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}


###### Lines country comparison

linesCountryComparison <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  y_ind <- x_ind
  y_lab <- x_lab
  
  data_temp <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, time >= time_start, time <= time_end)
  if (!(is.na(index)|index==0)) { data_all <- data_all |> group_by(variable, country_id) |> mutate(value = value/first(value)) |> ungroup() }
  
  if (peers != 0) {data_peers <- data_all |> filter(country_id %in% peers_iso2c)}
  data_country <- data_all |> filter(country_id %in% country_iso2c)
  
  theplot <- ggplot(data_all, aes(time, value))
  
  if (all == 1) {theplot <- theplot + geom_line(data=data_all, aes(group=country), colour=ACRA['grey'], size = 0.7, alpha=.4)} 
  if (peers != 0) {theplot <- theplot + geom_line(data=data_peers, aes(group=country), colour=ACRA['sec2'], size = 2, alpha=.8)}
  
  theplot <- theplot + geom_line(data=data_country, aes(group=country), colour=ACRA['dark'], size = 3, alpha=.8) +
    scale_y_continuous(limits=c(y_min, y_max)) + 
    scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                       labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1]))
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  if (!(is.na(index)|index==0)) { y_lab <- paste(y_lab, ", index: ", x_min, " = 1", sep = "" ) }
  
  theplot <- theplot + ggtitle(title) + theme(plot.title = element_textbox_simple()) +
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none") +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_peers, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['sec2'], alpha=0.8)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8, colour = ACRA['dark'], alpha=1)) +
    geom_dl(data = data_country, aes(label = country_id), method = list(dl.trans(x = x - 0.2), "first.points", cex = 0.8, colour = ACRA['dark'], alpha=1)) +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}


###### Distribution fix (horizontal across-country density estimate for the fixed time period)

densityFix <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  data_all <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators[1])))
  names(data_all)[length(names(data_all))] <- "variable"
  data_all <- data_all |> filter(time == time_fix)
  
  theplot <- ggplot(data_all, aes(variable, after_stat(density)))
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + geom_histogram(fill = ACRA['green'], colour = ACRA['grey'], size = .2) + geom_density(colour = ACRA['dark']) +
    xlim(x_min, x_max) + ggtitle(title) + labs(x = x_lab, y = 'Плотность', caption = caption) +
    annotate(geom = 'text', label = time_fix_label, x = Inf, y = Inf, hjust = 1.5, vjust = 1.5)
  
  return(list(graph = theplot, data = data_all))
  
}


###### Distribution dynamics (fan plot)

distributionDynamic <- function(data, graph_params, country_iso2c, peers_iso2c, verbose=T) {
  
  for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
  if (verbose == T) {print(graph_name)}
  
  y_ind <- x_ind
  y_lab <- x_lab
  
  data_temp <- data$extdata |> select(any_of(c("country", "country_id", "year", "quarter", "month", "time", indicators)))
  data_all <- reshape2::melt(data_temp, id.vars=names(data_temp)[1:(dim(data_temp)[2]-length(indicators))],
                             variable.name="variable", value.name="value")
  data_all <- data_all |> filter(variable %in% indicators, time >= time_start, time <= time_end, !is.na(value)) |>
    select(-c(variable))
  data_quant <- data_all |> calc_quantiles(intervals=(1:19)/20, x_var="time", y_var="value", rename=F) |>
    arrange(time, desc(quantile))
  data_country <- data_all |> filter(country_id == country_iso2c) |> select(time, value)
  data_quant <- data_quant |> left_join(data_country, by=c("time"="time"), suffix=c("","_c"))
  
  theplot <- ggplot(data=data_quant, aes(time, value, quantile=quantile)) + geom_fan(intervals=c(seq(10,90, by=10))/100)
  theplot <- theplot + coord_cartesian(ylim=c(y_min, y_max))+scale_fill_gradient(low=ACRA['dark'], high=ACRA['green']) +
    scale_x_continuous(breaks = seq(time_start + timetony_start, time_end - timetony_end + 1, by = labfreq), 
                       labels = c(ifelse(timetony_start==0,x_min[1],x_min[1]+1):x_max[1]))
  theplot <- theplot + geom_line(data=data_quant, aes(time, value_c), colour=ACRA['sec6'], size=2)
  
  theplot <- theplot + resolve_theme(graph_params$theme_name)
  
  theplot <- theplot + ggtitle(title) + theme(plot.title = element_textbox_simple()) +
    labs(caption = caption, x=NULL, y=y_lab) + guides(fill = "none") +
    geom_hline(yintercept = 0, color = "dark grey", size = 1)
  
  # if (identical(normalize_theme_name(theme_name), "ipsum")) {
  #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
  # }
  return(list(graph = theplot, data = data_all))
  
}


###### Distribution year comparison (candle plot, fixed indicator)

distributionYearComparison <- function(datagraph_params, country_iso2c, peers_iso2c, verbose=T) {

    for (j in seq_along(graph_params)) { eval(parse(text = paste0(names(graph_params)[j], " <- graph_params$", names(graph_params)[j]) )) }
    if (verbose == T) {print(graph_name)}
    
    data_all <- reshape2::melt(data$extdata, id.vars=c("country", "country_id", "year"), variable.name="variable", value.name="value")
    data_all <- data_all |> filter(variable %in% indicators, year %in% time_fix, country_id == country_iso2c, !is.na(value)) |> 
      mutate(variable = fct_relevel(variable, indicators))
    
    dict_temp <- data$dict |> filter(indicator_code %in% indicators, source_frequency == data_frequency) |> 
      arrange(factor(indicator_code, levels = indicators)) |> select(indicator)
    x_lab <- unname(unlist(dict_temp))
    x_lab <- str_wrap(x_lab, width = 15)
    
    theplot <- ggplot(data_all, aes(as.factor(year), value)) + geom_boxplot() +
      scale_y_continuous(limits=c(y_min, y_max)) +
      scale_x_discrete(labels=x_lab) +
      ggtitle(title) + labs(caption = caption, x=NULL, y=NULL)
    
    theplot <- theplot + resolve_theme(graph_params$theme_name)
    theplot <- theplot + theme(plot.title = element_textbox_simple(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      geom_hline(yintercept = 0, color = "dark grey", size = 1)
    
    # if (identical(normalize_theme_name(theme_name), "ipsum")) {
    #   theplot <- theplot + ggplot2::theme(text = ggplot2::element_text(family = "Nunito Sans"))
    # }
    return(list(graph = theplot, data = data_all))
    
}

# ggplot(Oxboys, aes(Occasion, height)) + 
#   geom_boxplot() +
# stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "white")

###### Distribution indicator comparison (candle plot, fixed time period)

# distributionIndicatorComparison <- function(datagraph_params, country_iso2c, peers_iso2c, verbose=T) {}