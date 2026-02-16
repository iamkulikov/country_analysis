## Functions to check the plot schedule

active_flag_vec <- function(df) {
  if (!is.data.frame(df)) {
    rlang::abort("active_flag_vec: input must be a data.frame/tibble.")
  }
  
  if (!("active" %in% names(df))) {
    return(rep(TRUE, nrow(df)))
  }
  
  x <- df$active
  
  # IMPORTANT: always return logical
  out <- dplyr::case_when(
    is.na(x) ~ FALSE,
    is.logical(x) ~ x,
    TRUE ~ (coerce_01(x) == 1)
  )
  
  as.logical(out)
}

pluck_chr_or_na <- function(x, i) {
  # x: list of character vectors
  # i: integer position
  vapply(
    x,
    \(v) if (length(v) >= i) as.character(v[[i]]) else NA_character_,
    character(1)
  )
}

checkColumns <- function(graphplan, graphplan_columns, strict = FALSE) {
  assert_packages(c("rlang", "stringr"))
  
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    if (isTRUE(strict)) rlang::abort("graphplan must be a data.frame/tibble.")
    rlang::warn("checkColumns: graphplan is not a data.frame/tibble.")
    return(0L)
  }
  
  expected <- graphplan_columns |> as.character() |> stringr::str_trim() |>
    (\(x) x[!is.na(x) & x != ""])() |> unique()
  
  if (length(expected) == 0) {
    # Vacuous truth: if nothing is required, columns check passes
    return(1L)
  }
  
  missing <- setdiff(expected, names(graphplan))
  
  if (length(missing) == 0) { return(1L) }
  
  msg <- paste0("Graphplan is missing expected columns: ", paste(missing, collapse = ", "))
  
  if (isTRUE(strict)) {rlang::abort(msg)} else {
    rlang::warn(msg)
    return(0L)
  }
}

checkEmpty <- function(graphplan) {
  assert_packages(c("dplyr", "rlang"))
  
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkEmpty: graphplan is not a data.frame/tibble.")
    return(0L)
  }
  
  if (!("active" %in% names(graphplan))) {
    # Backward compatible: if there's no active flag, treat as active
    return(ifelse(nrow(graphplan) > 0, 1L, 0L))
  }
  
  active_n <- graphplan |>
    dplyr::mutate(active_flag = active_flag_vec(graphplan)) |>
    dplyr::summarise(n = sum(.data$active_flag), .groups = "drop") |>
    dplyr::pull(.data$n)
  
  ifelse(active_n > 0, 1L, 0L)
}

checkGraphTypes <- function(graphplan, graph_types, warn_unknown = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkGraphTypes: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_types = 0L))
  }
  
  # ---- Normalize allowed types -----------------------------------------
  allowed <- graph_types |>
    as.character() |>
    stringr::str_trim() |>
    (\(x) x[!is.na(x) & x != ""])() |>
    unique()
  
  if (length(allowed) == 0) {
    rlang::warn("checkGraphTypes: graph_types is empty after cleaning. Active rows will be marked invalid.")
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  has_type_col <- "graph_type" %in% names(gp)
  
  # ---- Missing column: fail only active rows ----------------------------
  if (!has_type_col) {
    rlang::warn("checkGraphTypes: graphplan has no column 'graph_type'. Active rows will be marked invalid.")
    
    out <- gp |>
      dplyr::mutate(check_types = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  out <- gp |>
    dplyr::mutate(
      graph_type_norm = stringr::str_trim(as.character(.data$graph_type)),
      graph_type_norm = dplyr::na_if(.data$graph_type_norm, ""),  # empty -> NA
      ok_type = dplyr::case_when(
        length(allowed) == 0 ~ FALSE,
        !is.na(.data$graph_type_norm) & (.data$graph_type_norm %in% allowed) ~ TRUE,
        TRUE ~ FALSE
      ),
      check_types = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_type ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_unknown)) {
    has_graph_name <- "graph_name" %in% names(out)
    
    warn_lines <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_types == 0L) |>
      dplyr::mutate(
        graph_name = if (has_graph_name) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        type_show = dplyr::coalesce(as.character(.data$graph_type), "NA")
      ) |>
      dplyr::transmute(
        line = paste0("- ", .data$graph_name, ": graph_type='", .data$type_show, "'")
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_lines) > 0) {
      rlang::warn(paste(
        "Unknown graph_type values (active graphs only):",
        paste(unique(warn_lines), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag", "graph_type_norm")),
      -dplyr::any_of("ok_type")
    )
}


checkFreq <- function(graphplan, warn_unknown = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkFreq: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_freq = 0L))
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  has_freq_col <- "data_frequency" %in% names(gp)
  
  # ---- Missing column: fail only active rows ----------------------------
  if (!has_freq_col) {
    rlang::warn("checkFreq: graphplan has no column 'data_frequency'. Active rows will be marked invalid.")
    
    out <- gp |>
      dplyr::mutate(check_freq = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  allowed <- c("y", "q", "m", "d")
  
  out <- gp |>
    dplyr::mutate(
      freq_norm = stringr::str_to_lower(stringr::str_trim(as.character(.data$data_frequency))),
      
      # STRICT semantics:
      # only y/q/m/d are allowed; NA and "" are errors
      ok_freq = !is.na(.data$freq_norm) & (.data$freq_norm %in% allowed),
      
      check_freq = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_freq ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_unknown)) {
    warn_tbl <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_freq == 0L) |>
      dplyr::mutate(
        freq_show = dplyr::coalesce(as.character(.data$data_frequency), "NA")
      ) |>
      dplyr::summarise(
        unknown = list(sort(unique(.data$freq_show))),
        .groups = "drop"
      )
    
    unknown <- warn_tbl$unknown[[1]]
    
    if (length(unknown) > 0) {
      rlang::warn(paste0(
        "Invalid data_frequency values (active graphs only): ",
        paste(unknown, collapse = ", "),
        ". Allowed values: y, q, m, d."
      ))
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag", "freq_norm")),
      -dplyr::any_of("ok_freq")
    )
}

checkUnique <- function(graphplan, active_only = FALSE, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkUnique: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_unique = 0L))
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  if (!("graph_name" %in% names(gp))) {
    rlang::warn("checkUnique: graphplan has no column 'graph_name'. Active rows will be marked invalid.")
    out <- gp |>
      dplyr::mutate(check_unique = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    return(out)
  }
  
  out <- gp |>
    dplyr::mutate(
      graph_name_norm = stringr::str_trim(as.character(.data$graph_name)),
      graph_name_norm = dplyr::na_if(.data$graph_name_norm, "")
    )
  
  # ---- Decide which rows participate in uniqueness counting -------------
  # active_only = TRUE  -> count only among active rows
  # active_only = FALSE -> count among all rows
  use_flag <- if (isTRUE(active_only)) out$active_flag else rep(TRUE, nrow(out))
  
  counts <- out |>
    dplyr::mutate(.use = use_flag) |>
    dplyr::filter(.data$.use) |>
    dplyr::count(.data$graph_name_norm, name = "n") |>
    dplyr::select(-dplyr::any_of(".use"))
  
  out <- out |>
    dplyr::left_join(counts, by = dplyr::join_by(graph_name_norm)) |>
    dplyr::mutate(
      ok_name_present = !is.na(.data$graph_name_norm),
      ok_unique = dplyr::coalesce(.data$n, 0L) == 1L,
      check_unique = dplyr::case_when(
        !.data$active_flag ~ 1L,
        !.data$ok_name_present ~ 0L,
        .data$ok_unique ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    bad <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_unique == 0L)
    
    if (nrow(bad) > 0) {
      warn_missing <- bad |>
        dplyr::filter(is.na(.data$graph_name_norm)) |>
        dplyr::summarise(
          rows = list(sort(unique(.data$.row_id))),
          .groups = "drop"
        ) |>
        dplyr::pull(.data$rows) |>
        purrr::pluck(1)
      
      warn_dups <- bad |>
        dplyr::filter(!is.na(.data$graph_name_norm)) |>
        dplyr::summarise(
          names = list(sort(unique(.data$graph_name_norm))),
          .groups = "drop"
        ) |>
        dplyr::pull(.data$names) |>
        purrr::pluck(1)
      
      lines <- character(0)
      if (length(warn_missing) > 0) {
        lines <- c(lines, paste0("- missing/blank graph_name in rows: ", paste(warn_missing, collapse = ", ")))
      }
      if (length(warn_dups) > 0) {
        lines <- c(lines, paste0("- duplicated graph_name: ", paste(warn_dups, collapse = ", ")))
      }
      
      if (length(lines) > 0) {
        rlang::warn(paste(
          "Non-unique or missing graph_name (active graphs only):",
          paste(lines, collapse = "\n"),
          sep = "\n"
        ))
      }
    }
  }
  
  out |>
    dplyr::select(-dplyr::any_of(c(".row_id", "active_flag", "graph_name_norm", "n", "ok_name_present", "ok_unique")))
}


checkAvailability <- function(graphplan, dict, warn_unavailable = TRUE) {
  assert_packages(c("dplyr", "tidyr", "stringr", "rlang", "tibble"))
  
  # ---- Basic validation ------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkAvailability: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_availability = 0L))
  }
  if (is.null(dict) || !is.data.frame(dict)) {
    rlang::warn("checkAvailability: dict is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_availability = 0L))
  }
  
  needed_gp_cols <- c("indicators", "data_frequency")
  missing_gp_cols <- setdiff(needed_gp_cols, names(graphplan))
  if (length(missing_gp_cols) > 0) {
    rlang::warn(paste0(
      "checkAvailability: graphplan is missing columns: ",
      paste(missing_gp_cols, collapse = ", "),
      ". Setting check_availability = 0."
    ))
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_availability = 0L))
  }
  
  if (!("indicator_code" %in% names(dict))) {
    rlang::warn("checkAvailability: dict has no column 'indicator_code'. Setting check_availability = 0.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_availability = 0L))
  }
  
  has_dict_freq <- "source_frequency" %in% names(dict)
  
  # ---- Active flag (warnings only for active==1) -----------------------
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan),
      data_frequency = stringr::str_to_lower(
        stringr::str_trim(as.character(.data$data_frequency))
      )
    )
  
  # ---- Availability table (dict) ---------------------------------------
  avail <- tibble::as_tibble(dict) |>
    dplyr::transmute(
      indicator_code = stringr::str_trim(as.character(.data$indicator_code)),
      source_frequency = if (has_dict_freq) {
        stringr::str_to_lower(stringr::str_trim(as.character(.data$source_frequency)))
      } else {
        NA_character_
      },
      available = 1L  # marker that survives join
    ) |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "") |>
    dplyr::distinct()
  
  # ---- Parse indicators (no eval/parse; split + unnest) ----------------
  needed_long <- gp |>
    dplyr::select(.data$.row_id, .data$active_flag, .data$data_frequency, dplyr::any_of("graph_name"), .data$indicators) |>
    dplyr::mutate(indicators_vec = stringr::str_split(as.character(.data$indicators), ",")) |>
    tidyr::unnest_longer(.data$indicators_vec, values_to = "indicator_code") |>
    dplyr::mutate(
      indicator_code = stringr::str_trim(as.character(.data$indicator_code)),
      indicator_code = dplyr::na_if(.data$indicator_code, "")
    ) |>
    dplyr::filter(!is.na(.data$indicator_code))
  
  # ---- Join + presence flag --------------------------------------------
  checked_long <- if (has_dict_freq) {
    needed_long |>
      dplyr::left_join(
        avail,
        by = dplyr::join_by(
          indicator_code == indicator_code,
          data_frequency == source_frequency
        )
      ) |>
      dplyr::mutate(present = !is.na(.data$available))
  } else {
    # Fallback: dict has no frequency column -> check by indicator_code only
    needed_long |>
      dplyr::left_join(
        avail |>
          dplyr::select(.data$indicator_code, .data$available) |>
          dplyr::distinct(),
        by = dplyr::join_by(indicator_code == indicator_code)
      ) |>
      dplyr::mutate(present = !is.na(.data$available))
  }
  
  # ---- Row verdict + missing list --------------------------------------
  row_verdict <- checked_long |>
    dplyr::summarise(
      check_availability = as.integer(all(.data$present)),
      missing = list(sort(unique(.data$indicator_code[!.data$present]))),
      .by = .data$.row_id
    )
  
  # ---- Join verdict back to graphplan ----------------------------------
  out <- gp |>
    dplyr::left_join(row_verdict, by = dplyr::join_by(.row_id)) |>
    dplyr::mutate(
      # If a row had no parsed indicators:
      # - active -> fail (0)
      # - inactive -> pass (1)
      check_availability = dplyr::case_when(
        !.data$active_flag ~ 1L,
        is.na(.data$check_availability) ~ 0L,
        TRUE ~ .data$check_availability
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_unavailable)) {
    warn_tbl <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_availability == 0L) |>
      dplyr::mutate(
        missing = dplyr::coalesce(.data$missing, list(character(0))),
        graph_name = dplyr::coalesce(as.character(.data$graph_name), paste0("row_", .data$.row_id))
      ) |>
      dplyr::filter(lengths(.data$missing) > 0) |>
      dplyr::transmute(
        line = paste0(
          "- ", .data$graph_name, ": ",
          vapply(.data$missing, \(x) paste(x, collapse = ", "), character(1))
        )
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_tbl) > 0) {
      rlang::warn(paste(
        "Some indicators are not available in dict for the requested frequency (active graphs only):",
        paste(warn_tbl, collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(-dplyr::any_of(c(".row_id", "active_flag", "missing")))
}


checkPeers <- function(graphplan, peer_groups, dict, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "tidyr", "stringr", "rlang", "tibble"))
  
  # ---- Basic validation ------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkPeers: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_peers = 0L))
  }
  if (is.null(peer_groups) || !is.list(peer_groups) || is.null(peer_groups$regions)) {
    rlang::warn("checkPeers: peer_groups is malformed (expected list with $regions). Setting check_peers = 0.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_peers = 0L))
  }
  if (is.null(dict) || !is.data.frame(dict) || !("indicator_code" %in% names(dict))) {
    rlang::warn("checkPeers: dict is missing or has no 'indicator_code'. Indicator-based peer modes may fail.")
  }
  
  if (!("peers" %in% names(graphplan))) {
    rlang::warn("checkPeers: graphplan has no column 'peers'. Setting check_peers = 0 for active rows.")
    out <- tibble::as_tibble(graphplan) |>
      dplyr::mutate(
        active_flag = active_flag_vec(graphplan),
        check_peers = dplyr::if_else(.data$active_flag, 0L, 1L)
      ) |>
      dplyr::select(-dplyr::any_of("active_flag"))
    return(out)
  }
  
  # We need frequency to validate similar/top/low
  has_freq <- "data_frequency" %in% names(graphplan)
  if (!has_freq) {
    rlang::warn("checkPeers: graphplan has no 'data_frequency'. Indicator-based peer modes (similar/top/low) will be marked invalid for active rows.")
  }
  
  # ---- Reference sets --------------------------------------------------
  # Valid group names are columns in peer_groups$regions excluding service cols
  service_cols <- c("country_iso2c", "country_iso3c", "country_name")
  valid_groups <- setdiff(names(peer_groups$regions), service_cols)
  valid_groups_norm <- stringr::str_to_lower(valid_groups)
  
  # Valid ISO2 list for custom: from peer_groups$regions$country_iso2c if exists,
  # otherwise try peer_groups$country_iso2c
  valid_iso2 <- NULL
  if ("country_iso2c" %in% names(peer_groups$regions)) {
    valid_iso2 <- peer_groups$regions |>
      dplyr::pull("country_iso2c") |>
      as.character()
  } else if ("country_iso2c" %in% names(peer_groups)) {
    valid_iso2 <- as.character(peer_groups$country_iso2c)
  }
  valid_iso2 <- valid_iso2 |>
    (\(x) x[!is.na(x) & x != ""])() |>
    stringr::str_trim() |>
    toupper() |>
    unique()
  
  # Available indicators by frequency (for similar/top/low)
  has_dict_freq <- "source_frequency" %in% names(dict)
  avail_by_freq <- if (has_dict_freq) {
    tibble::as_tibble(dict) |>
      dplyr::transmute(
        source_frequency = stringr::str_to_lower(stringr::str_trim(as.character(.data$source_frequency))),
        indicator_code   = stringr::str_trim(as.character(.data$indicator_code)),
        available        = 1L
      ) |>
      dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "") |>
      dplyr::distinct()
  } else {
    tibble::tibble(source_frequency = NA_character_, indicator_code = character(0), available = integer(0))
  }
  
  # ---- Normalize graphplan --------------------------------------------
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan),
      peers_raw = as.character(.data$peers) |> stringr::str_trim(),
      peers_raw = dplyr::na_if(.data$peers_raw, ""),
      peers_norm = stringr::str_to_lower(.data$peers_raw),
      data_frequency = if (has_freq) {
        stringr::str_to_lower(stringr::str_trim(as.character(.data$data_frequency)))
      } else {
        NA_character_
      }
    )
  
  # ---- Split "type: payload" (robust to spaces) ------------------------
  # We split by the first ":" only; payload may contain commas.
  split_tbl <- gp |>
    dplyr::mutate(
      has_colon = !is.na(.data$peers_raw) & stringr::str_detect(.data$peers_raw, ":")
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        .data$has_colon,
        stringr::str_split_fixed(.data$peers_raw, ":", n = 2)[, 1],
        .data$peers_raw
      ),
      payload = dplyr::if_else(
        .data$has_colon,
        stringr::str_split_fixed(.data$peers_raw, ":", n = 2)[, 2],
        NA_character_
      ),
      type = stringr::str_to_lower(stringr::str_trim(as.character(.data$type))),
      payload = stringr::str_trim(as.character(.data$payload))
    )
  
  # ---- "Simple" peers values (no colon) --------------------------------
  simple_ok <- split_tbl |>
    dplyr::transmute(
      .row_id,
      ok_simple = dplyr::case_when(
        is.na(.data$peers_raw) ~ TRUE,
        .data$type %in% c("0", "default", "neighbours") ~ TRUE,
        (!.data$has_colon) & (.data$type %in% valid_groups_norm) ~ TRUE,
        TRUE ~ FALSE
      ),
      simple_reason = dplyr::case_when(
        is.na(.data$peers_raw) ~ NA_character_,
        .data$type %in% c("0", "default", "neighbours") ~ NA_character_,
        (!.data$has_colon) & (.data$type %in% valid_groups_norm) ~ NA_character_,
        (!.data$has_colon) ~ "unknown_value_or_group",
        TRUE ~ NA_character_
      )
    )
  
  # ---- custom: ISO2, ISO2, ... ----------------------------------------
  custom_long <- split_tbl |>
    dplyr::filter(.data$has_colon, .data$type == "custom") |>
    dplyr::mutate(payload = dplyr::na_if(.data$payload, "")) |>
    dplyr::mutate(parts = stringr::str_split(.data$payload, ",")) |>
    tidyr::unnest_longer(.data$parts, values_to = "iso2") |>
    dplyr::mutate(
      iso2 = toupper(stringr::str_trim(as.character(.data$iso2))),
      iso2 = dplyr::na_if(.data$iso2, "")
    ) |>
    dplyr::filter(!is.na(.data$iso2))
  
  custom_check <- split_tbl |>
    dplyr::filter(.data$has_colon, .data$type == "custom") |>
    dplyr::select(.data$.row_id, .data$payload) |>
    dplyr::left_join(
      custom_long |>
        dplyr::mutate(ok = .data$iso2 %in% valid_iso2) |>
        dplyr::summarise(
          ok_custom = dplyr::if_else(dplyr::n() == 0, FALSE, all(.data$ok)),
          bad_iso2  = list(sort(unique(.data$iso2[!.data$ok]))),
          .by = .data$.row_id
        ),
      by = dplyr::join_by(.row_id)
    ) |>
    dplyr::mutate(
      ok_custom = dplyr::coalesce(.data$ok_custom, FALSE),
      payload_empty = is.na(.data$payload) | !nzchar(stringr::str_trim(.data$payload)),
      reason_custom = dplyr::case_when(
        .data$payload_empty ~ "custom_empty",
        .data$ok_custom ~ NA_character_,
        lengths(.data$bad_iso2) > 0 ~ "custom_unknown_iso2",
        TRUE ~ "custom_bad_format"
      )
    ) |>
    dplyr::select(.data$.row_id, .data$ok_custom, .data$reason_custom, .data$bad_iso2)
  
  # ---- similar/top/low: indicator, param, year -------------------------
  comp_tbl <- split_tbl |>
    dplyr::filter(.data$has_colon, .data$type %in% c("similar", "top", "low")) |>
    dplyr::mutate(
      parts = stringr::str_split(.data$payload, ","),
      ind = pluck_chr_or_na(.data$parts, 1),
      p2  = pluck_chr_or_na(.data$parts, 2),
      p3  = pluck_chr_or_na(.data$parts, 3),
      ind = dplyr::na_if(stringr::str_trim(.data$ind), ""),
      p2  = dplyr::na_if(stringr::str_trim(.data$p2), ""),
      p3  = dplyr::na_if(stringr::str_trim(.data$p3), ""),
      p2n = suppressWarnings(as.numeric(.data$p2)),
      y3n = suppressWarnings(as.numeric(.data$p3)),
      ok_format = !is.na(.data$ind) & !is.na(.data$p2n) & !is.na(.data$y3n),
      year_ok = !is.na(.data$y3n) & (.data$y3n >= 1980) & (.data$y3n < 2100),
      band_ok = !is.na(.data$p2n) & (.data$p2n > 0) & (.data$p2n <= 1),
      n_ok    = !is.na(.data$p2n) & (.data$p2n > 0) & (.data$p2n %% 1 == 0),
      freq_ok = !is.na(.data$data_frequency)
    ) |>
    dplyr::left_join(
      avail_by_freq,
      by = dplyr::join_by(data_frequency == source_frequency, ind == indicator_code)
    ) |>
    dplyr::mutate(ind_available = !is.na(.data$available)) |>
    dplyr::summarise(
      ok_comp = dplyr::case_when(
        !all(.data$ok_format) ~ FALSE,
        !all(.data$freq_ok) ~ FALSE,
        !all(.data$ind_available) ~ FALSE,
        !all(.data$year_ok) ~ FALSE,
        all(.data$type == "similar") ~ all(.data$band_ok),
        all(.data$type %in% c("top", "low")) ~ all(.data$n_ok),
        TRUE ~ FALSE
      ),
      reason_comp = {
        if (!all(.data$ok_format)) "bad_format"
        else if (!all(.data$freq_ok)) "missing_data_frequency"
        else if (!all(.data$ind_available)) "indicator_unavailable"
        else if (!all(.data$year_ok)) "bad_year"
        else if (all(.data$type == "similar") && !all(.data$band_ok)) "bad_band"
        else if (all(.data$type %in% c("top", "low")) && !all(.data$n_ok)) "bad_n"
        else NA_character_
      },
      ind = dplyr::first(.data$ind),
      .by = .data$.row_id
    )
  
  # ---- Unknown type with colon -----------------------------------------
  unknown_type <- split_tbl |>
    dplyr::filter(.data$has_colon, !(.data$type %in% c("custom", "similar", "top", "low", "default", "neighbours"))) |>
    dplyr::transmute(.row_id, ok_unknown = FALSE, reason_unknown = "unknown_type")
  
  # ---- Combine checks per row ------------------------------------------
  out <- split_tbl |>
    dplyr::left_join(simple_ok, by = dplyr::join_by(.row_id)) |>
    dplyr::left_join(custom_check, by = dplyr::join_by(.row_id)) |>
    dplyr::left_join(comp_tbl, by = dplyr::join_by(.row_id)) |>
    dplyr::left_join(unknown_type, by = dplyr::join_by(.row_id)) |>
    dplyr::mutate(
      ok_custom = dplyr::coalesce(.data$ok_custom, FALSE),
      ok_comp   = dplyr::coalesce(.data$ok_comp, FALSE),
      ok_unknown = dplyr::coalesce(.data$ok_unknown, TRUE), # TRUE if not in unknown_type table
      
      check_peers = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_simple ~ 1L,
        (.data$has_colon & .data$type == "custom" & .data$ok_custom) ~ 1L,
        (.data$has_colon & .data$type %in% c("similar", "top", "low") & .data$ok_comp) ~ 1L,
        # Colon + unknown type => invalid
        (.data$has_colon & !.data$ok_unknown) ~ 0L,
        TRUE ~ 0L
      ),
      
      reason = dplyr::case_when(
        !.data$active_flag ~ NA_character_,
        .data$check_peers == 1L ~ NA_character_,
        .data$has_colon & !.data$ok_unknown ~ "unknown_type",
        .data$has_colon & .data$type == "custom" ~ dplyr::coalesce(.data$reason_custom, "custom_invalid"),
        .data$has_colon & .data$type %in% c("similar", "top", "low") ~ dplyr::coalesce(.data$reason_comp, "comp_invalid"),
        TRUE ~ dplyr::coalesce(.data$simple_reason, "invalid")
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    warn_lines <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_peers == 0L) |>
      dplyr::mutate(
        graph_name = if ("graph_name" %in% names(out)) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        peers_show = dplyr::coalesce(.data$peers_raw, "<NA>"),
        reason     = dplyr::coalesce(.data$reason, "invalid")
      ) |>
      dplyr::transmute(
        line = paste0("- ", .data$graph_name, ": peers='", .data$peers_show, "' (", .data$reason, ")")
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_lines) > 0) {
      rlang::warn(paste(
        "Invalid peers specification (active graphs only):",
        paste(unique(warn_lines), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(-dplyr::any_of(c(
      ".row_id", "active_flag", "peers_raw", "peers_norm", "has_colon", "type", "payload",
      "ok_simple", "simple_reason", "ok_custom", "reason_custom", "bad_iso2",
      "ok_comp", "reason_comp", "ok_unknown", "reason_unknown", "reason"
    )))
}


checkTimes <- function(graphplan, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkTimes: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_times = 0L))
  }
  
  # If we can't classify types, we can't validate times meaningfully
  if (!("graph_type" %in% names(graphplan))) {
    rlang::warn("checkTimes: graphplan has no column 'graph_type'.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_times = 0L))
  }
  
  has_x_min   <- "x_min" %in% names(graphplan)
  has_x_max   <- "x_max" %in% names(graphplan)
  has_timefix <- "time_fix" %in% names(graphplan)
  has_graph_name <- "graph_name" %in% names(graphplan)
  
  # ---- Type sets -------------------------------------------------------
  dynamic_types <- c(
    "structure_dynamic", "bar_dynamic", "lines_country_comparison",
    "lines_indicator_comparison", "distribution_dynamic"
  )
  
  cross_types <- c(
    "scatter_country_comparison", "structure_country_comparison",
    "structure_country_comparison_norm", "bar_country_comparison",
    "bar_country_comparison_norm", "bar_year_comparison",
    "scatter_dynamic", "density_fix", "distribution_year_comparison"
  )
  
  # ---- Vector helpers --------------------------------------------------
  is_time_vec <- function(x) {
    vapply(x, isTime, logical(1))
  }
  
  is_time_or_empty_vec <- function(x) {
    vapply(
      x,
      \(v) {
        vv <- stringr::str_trim(as.character(v))
        if (is.na(vv) || !nzchar(vv)) return(TRUE)
        isTime(vv)
      },
      logical(1)
    )
  }
  
  is_numeric_or_empty_vec <- function(x) {
    vapply(
      x,
      \(v) {
        vv <- stringr::str_trim(as.character(v))
        if (is.na(vv) || !nzchar(vv)) return(TRUE)
        !is.na(suppressWarnings(as.numeric(vv)))
      },
      logical(1)
    )
  }
  
  out <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan),
      graph_type_norm = stringr::str_trim(as.character(.data$graph_type)),
      is_dynamic = .data$graph_type_norm %in% dynamic_types,
      is_cross   = .data$graph_type_norm %in% cross_types,
      check_times = 1L,
      reason_times = NA_character_
    )
  
  # ---- Dynamic types: x_min/x_max must be time-or-empty ----------------
  if (has_x_min && has_x_max) {
    x_min_ok <- is_time_or_empty_vec(out$x_min)
    x_max_ok <- is_time_or_empty_vec(out$x_max)
    
    out <- out |>
      dplyr::mutate(
        check_times = dplyr::case_when(
          .data$is_dynamic ~ as.integer(x_min_ok & x_max_ok),
          TRUE ~ .data$check_times
        ),
        reason_times = dplyr::case_when(
          .data$is_dynamic & !x_min_ok ~ "dynamic_bad_x_min",
          .data$is_dynamic & !x_max_ok ~ "dynamic_bad_x_max",
          TRUE ~ .data$reason_times
        )
      )
  } else {
    # Missing required cols -> fail only dynamic rows
    out <- out |>
      dplyr::mutate(
        check_times = dplyr::case_when(
          .data$is_dynamic ~ 0L,
          TRUE ~ .data$check_times
        ),
        reason_times = dplyr::case_when(
          .data$is_dynamic & !has_x_min ~ "missing_x_min",
          .data$is_dynamic & !has_x_max ~ "missing_x_max",
          TRUE ~ .data$reason_times
        )
      )
  }
  
  # ---- Cross checks: x_min/x_max numeric-or-empty AND time_fix time-or-empty ----
  # NOTE: time_fix may be empty because fillGraphPlan() can populate it later.
  if (has_timefix) {
    tf_ok <- is_time_or_empty_vec(out$time_fix)
    
    if (has_x_min && has_x_max) {
      x_min_num_ok <- is_numeric_or_empty_vec(out$x_min)
      x_max_num_ok <- is_numeric_or_empty_vec(out$x_max)
      
      out <- out |>
        dplyr::mutate(
          check_times = dplyr::case_when(
            .data$is_cross ~ as.integer(x_min_num_ok & x_max_num_ok & tf_ok),
            TRUE ~ .data$check_times
          ),
          reason_times = dplyr::case_when(
            .data$is_cross & !x_min_num_ok ~ "cross_bad_x_min",
            .data$is_cross & !x_max_num_ok ~ "cross_bad_x_max",
            .data$is_cross & !tf_ok ~ "cross_bad_time_fix",
            TRUE ~ .data$reason_times
          )
        )
    } else {
      # If x_min/x_max are missing entirely, keep current behavior:
      # cross-types depend on these columns -> fail only those rows.
      out <- out |>
        dplyr::mutate(
          check_times = dplyr::case_when(
            .data$is_cross ~ 0L,
            TRUE ~ .data$check_times
          ),
          reason_times = dplyr::case_when(
            .data$is_cross & !has_x_min ~ "missing_x_min",
            .data$is_cross & !has_x_max ~ "missing_x_max",
            TRUE ~ .data$reason_times
          )
        )
    }
  } else {
    # No time_fix column -> do NOT fail (fillGraphPlan() can add/populate later)
    # We keep check_times as-is for cross types.
    out <- out
  }
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    warn_tbl <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_times == 0L) |>
      dplyr::mutate(
        graph_name = if (has_graph_name) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        x_min_show = if (has_x_min) dplyr::coalesce(as.character(.data$x_min), "NA") else "<no x_min>",
        x_max_show = if (has_x_max) dplyr::coalesce(as.character(.data$x_max), "NA") else "<no x_max>",
        tf_show    = if (has_timefix) dplyr::coalesce(as.character(.data$time_fix), "NA") else "<no time_fix>"
      ) |>
      dplyr::transmute(
        line = paste0(
          "- ", .data$graph_name,
          " [", dplyr::coalesce(.data$reason_times, "invalid"), "]",
          " x_min=", .data$x_min_show,
          " x_max=", .data$x_max_show,
          " time_fix=", .data$tf_show
        )
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_tbl) > 0) {
      rlang::warn(paste(
        "Some time parameters look invalid (active graphs only):",
        paste(unique(warn_tbl), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(-dplyr::any_of(c(
      ".row_id", "active_flag", "graph_type_norm", "is_dynamic", "is_cross", "reason_times"
    )))
}


is_numeric <- function(text_number) {suppressWarnings(!is.na(as.numeric(text_number)))}

isTime <- function(text_date) {
  assert_packages(c("stringr"))
  
  # Backward-compatible: NA/empty treated as OK
  x <- as.character(text_date)
  if (length(x) != 1) {
    # isTime historically worked as scalar; be safe
    x <- x[[1]]
  }
  x <- stringr::str_trim(x)
  if (is.na(x) || !nzchar(x)) return(TRUE)
  
  # Split list: allow ",", ", ", ",   "
  tokens <- stringr::str_split(x, ",\\s*") |>
    purrr::pluck(1) |>
    stringr::str_trim()
  tokens <- tokens[tokens != ""]
  
  if (length(tokens) == 0) return(TRUE)
  
  is_token_ok <- function(tok) {
    tok <- stringr::str_trim(tok)
    if (!nzchar(tok)) return(TRUE)
    
    # daily: dd.mm.yyyy (1-2 digit day/month, 4-digit year)
    if (stringr::str_detect(tok, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}$")) {
      d <- suppressWarnings(as.Date(tok, format = "%d.%m.%Y"))
      return(!is.na(d))
    }
    
    tok2 <- stringr::str_to_lower(tok)
    
    # year only: yyyy
    if (stringr::str_detect(tok2, "^(19|20)\\d{2}$")) {
      return(TRUE)
    }
    
    # quarter: yyyyqN
    if (stringr::str_detect(tok2, "^(19|20)\\d{2}q[1-4]$")) {
      return(TRUE)
    }
    
    # month: yyyymN where N=1..12 (allow 1 or 2 digits)
    if (stringr::str_detect(tok2, "^(19|20)\\d{2}m(\\d{1,2})$")) {
      n <- suppressWarnings(as.integer(stringr::str_match(tok2, "m(\\d{1,2})$")[, 2]))
      return(!is.na(n) && n >= 1L && n <= 12L)
    }
    
    FALSE
  }
  
  all(vapply(tokens, is_token_ok, logical(1)))
}

checkBinaryParams <- function(
    graphplan,
    warn_invalid = TRUE,
    binary_cols = c(
      "all", "x_log", "y_log", "index", "recession", "swap_axis",
      "long_legend", "vert_lab", "short_names", "show_title", "active"
    )
) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkBinaryParams: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_binary = 0L))
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  present_cols <- intersect(binary_cols, names(gp))
  missing_cols <- setdiff(binary_cols, names(gp))
  
  if (length(missing_cols) > 0) {
    rlang::warn(paste0(
      "checkBinaryParams: missing columns in graphplan (skipped): ",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  if (length(present_cols) == 0) {
    # Nothing to validate -> vacuous PASS
    return(gp |>
             dplyr::mutate(check_binary = 1L) |>
             dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    )
  }
  
  # ---- Predicate: only 0/1 or NA are allowed ---------------------------
  is_binary01_strict <- function(x) {
    x_chr <- stringr::str_trim(as.character(x))
    
    # Empty / NA are NOT allowed in strict mode
    ifelse(
      is.na(x_chr) | x_chr == "",
      FALSE,
      {
        # allow "0"/"1" and numeric forms like "0.0"/"1.00"
        x_num <- suppressWarnings(as.numeric(x_chr))
        (x_chr %in% c("0", "1")) | (!is.na(x_num) & (x_num %in% c(0, 1)))
      }
    )
  }
  
  # Matrix of checks (logical), one column per binary parameter
  checks_tbl <- gp |>
    dplyr::transmute(
      .row_id = .data$.row_id,
      dplyr::across(dplyr::all_of(present_cols), is_binary01_strict, .names = "ok_{.col}")
    )
  
  out <- gp |>
    dplyr::left_join(checks_tbl, by = dplyr::join_by(.row_id)) |>
    dplyr::mutate(
      check_binary = dplyr::if_else(
        dplyr::if_all(dplyr::starts_with("ok_"), \(x) x),
        1L,
        0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    has_graph_name <- "graph_name" %in% names(out)
    ok_cols_present <- names(out)[stringr::str_starts(names(out), "ok_")]
    
    if (length(ok_cols_present) > 0) {
      
      bad_fields_tbl <- out |>
        dplyr::filter(
          .data$active_flag == TRUE,
          dplyr::if_any(dplyr::all_of(ok_cols_present), \(x) !x)
        ) |>
        dplyr::mutate(
          graph_name = if (has_graph_name) {
            dplyr::coalesce(
              dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
              paste0("row_", .data$.row_id)
            )
          } else {
            paste0("row_", .data$.row_id)
          }
        ) |>
        dplyr::select(.data$.row_id, .data$graph_name, dplyr::all_of(ok_cols_present)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(ok_cols_present),
          names_to = "ok_col",
          values_to = "ok_val"
        ) |>
        dplyr::filter(.data$ok_val == FALSE) |>
        dplyr::mutate(field = stringr::str_remove(.data$ok_col, "^ok_")) |>
        dplyr::summarise(
          bad_fields = list(sort(unique(.data$field))),
          .by = c(.data$.row_id, .data$graph_name)
        ) |>
        dplyr::filter(lengths(.data$bad_fields) > 0) |>
        dplyr::mutate(
          line = paste0(
            "- ", .data$graph_name, ": ",
            vapply(.data$bad_fields, \(x) paste(x, collapse = ", "), character(1))
          )
        ) |>
        dplyr::pull(.data$line)
      
      if (length(bad_fields_tbl) > 0) {
        rlang::warn(paste(
          "Some binary parameters are not 0/1/NA (active graphs only):",
          paste(bad_fields_tbl, collapse = "\n"),
          sep = "\n"
        ))
      }
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag")),
      -dplyr::starts_with("ok_")
    )
}

checkNumericParams <- function(graphplan, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble", "tidyr"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkNumericParams: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_num = 0L))
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  needed <- c("y_min", "y_max")
  missing <- setdiff(needed, names(gp))
  
  # ---- Missing columns: fail only active rows ---------------------------
  if (length(missing) > 0) {
    rlang::warn(paste0(
      "checkNumericParams: graphplan is missing columns: ",
      paste(missing, collapse = ", "),
      ". Active rows will be marked invalid."
    ))
    
    out <- gp |>
      dplyr::mutate(check_num = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  # ---- Helper: empty is allowed; otherwise must parse to numeric --------
  is_numeric_or_empty_vec <- function(x) {
    x_chr <- stringr::str_trim(as.character(x))
    x_chr <- dplyr::na_if(x_chr, "")
    
    parsed <- suppressWarnings(as.numeric(x_chr))
    is.na(x_chr) | !is.na(parsed)
  }
  
  out <- gp |>
    dplyr::mutate(
      ok_y_min = is_numeric_or_empty_vec(.data$y_min),
      ok_y_max = is_numeric_or_empty_vec(.data$y_max),
      check_num = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_y_min & .data$ok_y_max ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    has_graph_name <- "graph_name" %in% names(out)
    ok_cols <- names(out)[stringr::str_starts(names(out), "ok_")]
    
    if (length(ok_cols) > 0) {
      bad_lines <- out |>
        dplyr::filter(.data$active_flag == TRUE, .data$check_num == 0L) |>
        dplyr::mutate(
          graph_name = if (has_graph_name) {
            dplyr::coalesce(
              dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
              paste0("row_", .data$.row_id)
            )
          } else {
            paste0("row_", .data$.row_id)
          }
        ) |>
        dplyr::select(.data$.row_id, .data$graph_name, dplyr::all_of(ok_cols)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(ok_cols),
          names_to = "ok_col",
          values_to = "ok_val"
        ) |>
        dplyr::filter(.data$ok_val == FALSE) |>
        dplyr::mutate(field = stringr::str_remove(.data$ok_col, "^ok_")) |>
        dplyr::summarise(
          bad_fields = list(sort(unique(.data$field))),
          .by = c(.data$.row_id, .data$graph_name)
        ) |>
        dplyr::mutate(
          line = paste0(
            "- ", .data$graph_name, ": ",
            vapply(.data$bad_fields, \(x) paste(x, collapse = ", "), character(1))
          )
        ) |>
        dplyr::pull(.data$line)
      
      if (length(bad_lines) > 0) {
        rlang::warn(paste(
          "Some numeric parameters are not numeric (empty/NA allowed) (active graphs only):",
          paste(unique(bad_lines), collapse = "\n"),
          sep = "\n"
        ))
      }
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag")),
      -dplyr::starts_with("ok_")
    )
}

checkTrend <- function(graphplan, trend_types, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkTrend: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_trend = 0L))
  }
  
  # ---- Normalize allowed types -----------------------------------------
  allowed <- trend_types |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    (\(x) x[!is.na(x) & x != ""])() |>
    unique()
  
  if (length(allowed) == 0) {
    rlang::warn("checkTrend: trend_types is empty after cleaning. Active rows will be marked invalid.")
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  has_trend_col <- "trend_type" %in% names(gp)
  
  # ---- Missing column: fail only active rows ----------------------------
  if (!has_trend_col) {
    rlang::warn("checkTrend: graphplan has no column 'trend_type'. Active rows will be marked invalid.")
    
    out <- gp |>
      dplyr::mutate(check_trend = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  out <- gp |>
    dplyr::mutate(
      trend_type_norm = stringr::str_to_lower(stringr::str_trim(as.character(.data$trend_type))),
      trend_type_norm = dplyr::na_if(.data$trend_type_norm, ""),
      ok_trend = dplyr::case_when(
        is.na(.data$trend_type_norm) ~ TRUE,               # NA/empty allowed
        length(allowed) == 0 ~ FALSE,                      # no allowed set -> can't validate
        TRUE ~ .data$trend_type_norm %in% allowed
      ),
      check_trend = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_trend ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    has_graph_name <- "graph_name" %in% names(out)
    
    warn_lines <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_trend == 0L) |>
      dplyr::mutate(
        graph_name = if (has_graph_name) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        trend_show = dplyr::coalesce(as.character(.data$trend_type), "NA")
      ) |>
      dplyr::transmute(
        line = paste0("- ", .data$graph_name, ": trend_type='", .data$trend_show, "'")
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_lines) > 0) {
      rlang::warn(paste(
        "Unknown trend_type values (NA/empty allowed) (active graphs only):",
        paste(unique(warn_lines), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag", "trend_type_norm")),
      -dplyr::any_of("ok_trend")
    )
}

checkTheme <- function(graphplan, theme_types, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkTheme: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_theme = 0L))
  }
  
  # ---- Normalize allowed themes ----------------------------------------
  allowed <- theme_types |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    (\(x) x[!is.na(x) & x != ""])() |>
    unique()
  
  if (length(allowed) == 0) {
    rlang::warn("checkTheme: theme_types is empty after cleaning. Active rows will be marked invalid.")
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  has_theme_col <- "theme" %in% names(gp)
  
  # ---- Missing column: fail only active rows ----------------------------
  if (!has_theme_col) {
    rlang::warn("checkTheme: graphplan has no column 'theme'. Active rows will be marked invalid.")
    
    out <- gp |>
      dplyr::mutate(check_theme = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  out <- gp |>
    dplyr::mutate(
      theme_norm = stringr::str_to_lower(stringr::str_trim(as.character(.data$theme))),
      theme_norm = dplyr::na_if(.data$theme_norm, ""),
      ok_theme = dplyr::case_when(
        is.na(.data$theme_norm) ~ TRUE,     # NA/empty allowed
        length(allowed) == 0 ~ FALSE,
        TRUE ~ .data$theme_norm %in% allowed
      ),
      check_theme = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_theme ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    has_graph_name <- "graph_name" %in% names(out)
    
    warn_lines <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_theme == 0L) |>
      dplyr::mutate(
        graph_name = if (has_graph_name) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        theme_show = dplyr::coalesce(as.character(.data$theme), "NA")
      ) |>
      dplyr::transmute(
        line = paste0("- ", .data$graph_name, ": theme='", .data$theme_show, "'")
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_lines) > 0) {
      rlang::warn(paste(
        "Unknown theme values (NA/empty allowed) (active graphs only):",
        paste(unique(warn_lines), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag", "theme_norm")),
      -dplyr::any_of("ok_theme")
    )
}


checkOrientation <- function(graphplan, orient_types, warn_invalid = TRUE) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(graphplan) || !is.data.frame(graphplan)) {
    rlang::warn("checkOrientation: graphplan is not a data.frame/tibble.")
    return(tibble::as_tibble(graphplan) |> dplyr::mutate(check_orient = 0L))
  }
  
  # ---- Normalize allowed orientations ----------------------------------
  allowed <- orient_types |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    (\(x) x[!is.na(x) & x != ""])() |>
    unique()
  
  if (length(allowed) == 0) {
    rlang::warn("checkOrientation: orient_types is empty after cleaning. Active rows will be marked invalid.")
  }
  
  gp <- tibble::as_tibble(graphplan) |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      active_flag = active_flag_vec(graphplan)
    )
  
  has_orient_col <- "orientation" %in% names(gp)
  
  # ---- Missing column: fail only active rows ----------------------------
  if (!has_orient_col) {
    rlang::warn("checkOrientation: graphplan has no column 'orientation'. Active rows will be marked invalid.")
    
    out <- gp |>
      dplyr::mutate(check_orient = dplyr::if_else(.data$active_flag, 0L, 1L)) |>
      dplyr::select(-dplyr::any_of(c(".row_id", "active_flag")))
    
    return(out)
  }
  
  out <- gp |>
    dplyr::mutate(
      orient_norm = stringr::str_to_lower(stringr::str_trim(as.character(.data$orientation))),
      orient_norm = dplyr::na_if(.data$orient_norm, ""),
      ok_orient = dplyr::case_when(
        is.na(.data$orient_norm) ~ TRUE,    # NA/empty allowed
        length(allowed) == 0 ~ FALSE,
        TRUE ~ .data$orient_norm %in% allowed
      ),
      check_orient = dplyr::case_when(
        !.data$active_flag ~ 1L,
        .data$ok_orient ~ 1L,
        TRUE ~ 0L
      )
    )
  
  # ---- Optional warnings (active only) ---------------------------------
  if (isTRUE(warn_invalid)) {
    has_graph_name <- "graph_name" %in% names(out)
    
    warn_lines <- out |>
      dplyr::filter(.data$active_flag == TRUE, .data$check_orient == 0L) |>
      dplyr::mutate(
        graph_name = if (has_graph_name) {
          dplyr::coalesce(
            dplyr::na_if(stringr::str_trim(as.character(.data$graph_name)), ""),
            paste0("row_", .data$.row_id)
          )
        } else {
          paste0("row_", .data$.row_id)
        },
        orient_show = dplyr::coalesce(as.character(.data$orientation), "NA")
      ) |>
      dplyr::transmute(
        line = paste0("- ", .data$graph_name, ": orientation='", .data$orient_show, "'")
      ) |>
      dplyr::pull(.data$line)
    
    if (length(warn_lines) > 0) {
      rlang::warn(paste(
        "Unknown orientation values (NA/empty allowed) (active graphs only):",
        paste(unique(warn_lines), collapse = "\n"),
        sep = "\n"
      ))
    }
  }
  
  out |>
    dplyr::select(
      -dplyr::any_of(c(".row_id", "active_flag", "orient_norm")),
      -dplyr::any_of("ok_orient")
    )
}


####### Function to generate text from the error report table

explainErrors <- function(error_report) {
  assert_packages(c("dplyr", "stringr", "rlang", "tibble"))
  
  # ---- Contract ---------------------------------------------------------
  if (is.null(error_report) || !is.data.frame(error_report)) {
    rlang::warn("explainErrors: error_report is not a data.frame/tibble.")
    return(invisible(NULL))
  }
  
  er <- tibble::as_tibble(error_report)
  
  if (nrow(er) == 0) {
    print("No errors")
    return(invisible(NULL))
  }
  
  # ---- Active filter (same rule as checks) ------------------------------
  active_flag <- active_flag_vec(er)  # logical, safe
  
  # ---- Helper: pick graph identifiers safely ----------------------------
  get_graph_ids <- function(df) {
    if ("graph_name" %in% names(df)) {
      id <- stringr::str_trim(as.character(df$graph_name))
      id <- dplyr::na_if(id, "")
      id <- dplyr::coalesce(id, paste0("row_", df$.row_id))
      return(id)
    }
    paste0("row_", df$.row_id)
  }
  
  # Ensure we have row ids for stable fallback naming
  er <- er |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .active_flag = active_flag
    )
  
  print("The plan cannot be executed. Please, check the following issues:")
  
  counter <- 0L
  
  # ---- Define checks (flag -> message) ---------------------------------
  checks <- list(
    list(flag = "check_types",        msg = "Unknown graph types in:"),
    list(flag = "check_freq",         msg = "Unknown data frequency in:"),
    list(flag = "check_unique",       msg = "Duplicating graph names:"),
    list(flag = "check_availability", msg = "Some indicators are not available in the database. Check their codenames in:"),
    list(flag = "check_peers",        msg = "Invalid/unknown peers specification in:"),
    list(flag = "check_times",        msg = "Unknown bounds or time formats. Check x_min, x_max or time_fix in:"),
    list(flag = "check_binary",       msg = "Non-binary parameters. Check all, x_log, y_log, index, recession, swap_axis, long_legend, vert_lab, short_names, show_title and active in:"),
    list(flag = "check_num",          msg = "Non-numeric parameters. Check y_min and y_max in:"),
    list(flag = "check_trend",        msg = "Unknown trend types in:"),
    list(flag = "check_theme",        msg = "Unknown theme values in:"),
    list(flag = "check_orient",       msg = "Unknown orientation in:")
  )
  
  # ---- Print issues -----------------------------------------------------
  for (rule in checks) {
    flag <- rule$flag
    msg  <- rule$msg
    
    if (!(flag %in% names(er))) next
    
    bad <- er |>
      dplyr::filter(.data$.active_flag == TRUE, .data[[flag]] == 0L)
    
    if (nrow(bad) == 0) next
    
    counter <- counter + 1L
    
    ids <- bad |>
      dplyr::mutate(graph_id = get_graph_ids(bad)) |>
      dplyr::pull(.data$graph_id) |>
      unique()
    
    print(glue::glue("{counter}. {msg} {paste(ids, collapse = ', ')}"))
  }
  
  invisible(NULL)
}
