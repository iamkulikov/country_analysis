# Trace Ledger: presentation view model + HTML renderer for Calculation Trace.
# Does not alter the journal contract; UI-only enrichment lives here.

# ---------- period display helpers ------------------------------------------

format_period_display <- function(period, frequency) {
  if (is.null(period) || is.na(period) || period == "") return("")
  period <- as.character(period)
  switch(
    as.character(frequency),
    y = period,
    q = {
      m <- stringr::str_match(period, "^(\\d+)-?[Qq]([1-4])$")
      if (any(is.na(m))) period else paste0(m[, 2], "Q", m[, 3])
    },
    m = {
      m <- stringr::str_match(period, "^(\\d+)-(\\d{2})$")
      if (any(is.na(m))) period else paste0(m[, 2], "M", m[, 3])
    },
    d = period,
    period
  )
}

.freq_chip_label <- function(frequency) {
  toupper(as.character(frequency %||% ""))
}

.month_short_label <- function(period) {
  m <- stringr::str_match(as.character(period), "^(\\d+)-(\\d{2})$")
  if (any(is.na(m))) return(as.character(period))
  month.abb[as.integer(m[, 3])]
}

.quarter_short_label <- function(period) {
  m <- stringr::str_match(as.character(period), "^(\\d+)-?[Qq]([1-4])$")
  if (any(is.na(m))) return(as.character(period))
  paste0("Q", m[, 3])
}

.period_strip_label <- function(period, frequency) {
  switch(
    as.character(frequency),
    m = .month_short_label(period),
    q = .quarter_short_label(period),
    d = {
      d <- suppressWarnings(lubridate::ymd(period))
      if (is.na(d)) as.character(period) else format(d, "%d")
    },
    as.character(period)
  )
}

# ---------- operation / transition classification ----------------------------

.is_aggregate_op <- function(operation) {
  !is.na(operation) & grepl("^aggregate_", as.character(operation))
}

.is_distribution_op <- function(operation) {
  !is.na(operation) & operation %in% c("desum_fix", "demean_fix")
}

.is_cross_frequency_op <- function(operation) {
  .is_aggregate_op(operation) | .is_distribution_op(operation)
}

.transition_direction_of <- function(operation) {
  if (.is_aggregate_op(operation)) return("up")
  if (.is_distribution_op(operation)) return("down")
  NA_character_
}

.transition_label_of <- function(operation) {
  if (is.na(operation) || operation == "") return(NA_character_)
  if (.is_aggregate_op(operation)) {
    return(toupper(sub("^aggregate_", "", operation)))
  }
  if (identical(operation, "desum_fix")) return("DESUM FIX")
  if (identical(operation, "demean_fix")) return("DEMEAN FIX")
  toupper(as.character(operation))
}

.safe_expected_subperiods <- function(period, parent_freq, child_freq) {
  parent_rank <- FREQ_ORDER[[parent_freq]]
  child_rank  <- FREQ_ORDER[[child_freq]]
  if (is.null(parent_rank) || is.null(child_rank) || child_rank >= parent_rank) {
    return(NA_integer_)
  }
  # Only unambiguous calendar pairs.
  ok <- (parent_freq == "y" && child_freq %in% c("q", "m")) ||
    (parent_freq == "q" && child_freq == "m") ||
    (parent_freq == "m" && child_freq == "d")
  if (!ok) return(NA_integer_)
  length(subPeriodsOf(period, parent_freq, child_freq))
}

.node_type_badge <- function(node_type) {
  switch(
    as.character(node_type %||% ""),
    computed  = "COMPUTED",
    imported  = "IMPORTED",
    opaque    = "OPAQUE",
    missing   = "MISSING",
    no_value  = "NO VALUE",
    truncated = "TRACE LIMITED",
    toupper(as.character(node_type %||% ""))
  )
}

.node_status_icon <- function(node_type) {
  switch(
    as.character(node_type %||% ""),
    computed  = "\u25CF",
    imported  = "\u25CF",
    opaque    = "\u26A0",
    missing   = "!",
    no_value  = "\u2205",
    truncated = "\u2026",
    "\u25CB"
  )
}

# ---------- view model ------------------------------------------------------

#' Build UI-only view model from a calculation-trace journal.
#'
#' @param journal Raw journal from buildValueTrace() (unchanged contract).
#' @param saveplan_full Plan with keep / indicator labels (may be NULL).
#' @param show_technical If FALSE, technical (keep=0) nodes are reparented away
#'   and bridge surrogates are inserted.
#' @return A list with `nodes` (tibble) and `bridges` (tibble of surrogate rows).
build_trace_ledger_vm <- function(journal,
                                  saveplan_full = NULL,
                                  show_technical = TRUE) {
  empty_nodes <- tibble::tibble(
    step_id = character(),
    level = integer(),
    parent_id = character(),
    display_parent_id = character(),
    country_id = character(),
    indicator_code = character(),
    indicator_label = character(),
    frequency = character(),
    period = character(),
    period_display = character(),
    value = numeric(),
    node_type = character(),
    operation = character(),
    formula_raw = character(),
    formula_filled = character(),
    time_relation = character(),
    source_name = character(),
    note = character(),
    has_children = logical(),
    child_count = integer(),
    is_technical = logical(),
    is_cross_frequency = logical(),
    transition_direction = character(),
    transition_label = character(),
    child_frequency = character(),
    subperiod_count = integer(),
    coverage_traced = integer(),
    coverage_expected = integer(),
    coverage_non_na = integer(),
    is_coalesce_winner = logical(),
    coalesce_winner_arg = integer(),
    is_bridge = logical(),
    bridge_hidden_count = integer(),
    recursion_limited = logical(),
    default_collapsed = logical(),
    strip_periods = I(list()),
    strip_values = I(list()),
    strip_labels = I(list()),
    strip_freq = character(),
    operand_indicator_code = character()
  )

  empty_bridges <- tibble::tibble(
    bridge_id = character(),
    display_parent_id = character(),
    hidden_count = integer(),
    level = integer(),
    after_step_id = character(),
    before_step_id = character()
  )

  if (is.null(journal) || nrow(journal) == 0L) {
    return(list(nodes = empty_nodes, bridges = empty_bridges))
  }

  nodes <- journal
  # Single-node journals (imported leaves) may omit parent_id when the root
  # was built with parent_step_id = NULL and tibble dropped the column.
  if (!"parent_id" %in% names(nodes)) {
    nodes$parent_id <- NA_character_
  } else {
    nodes$parent_id <- as.character(nodes$parent_id)
  }

  # Enrich keep / label from saveplan (presentation only).
  if (!is.null(saveplan_full) && nrow(saveplan_full) > 0L) {
    meta <- saveplan_full |>
      dplyr::select(
        dplyr::any_of(c("indicator_code", "source_frequency", "keep", "indicator"))
      ) |>
      dplyr::distinct(.data$indicator_code, .data$source_frequency, .keep_all = TRUE)
    nodes <- nodes |>
      dplyr::left_join(
        meta,
        by = c("indicator_code" = "indicator_code", "frequency" = "source_frequency")
      )
  } else {
    nodes$keep <- NA_integer_
    nodes$indicator <- NA_character_
  }

  nodes <- nodes |>
    dplyr::mutate(
      is_technical = !is.na(.data$keep) & .data$keep == 0L,
      indicator_label = dplyr::coalesce(
        as.character(.data$indicator),
        .data$indicator_code
      ),
      period_display = mapply(
        format_period_display,
        .data$period,
        .data$frequency,
        USE.NAMES = FALSE
      ),
      is_cross_frequency = .is_cross_frequency_op(.data$operation),
      transition_direction = vapply(
        .data$operation, .transition_direction_of, character(1)
      ),
      transition_label = vapply(
        .data$operation, .transition_label_of, character(1)
      ),
      recursion_limited = !is.na(.data$note) &
        grepl("without further recursion", .data$note, fixed = TRUE),
      is_bridge = FALSE,
      bridge_hidden_count = 0L,
      coalesce_winner_arg = NA_integer_,
      is_coalesce_winner = FALSE,
      child_frequency = NA_character_,
      subperiod_count = 0L,
      coverage_traced = NA_integer_,
      coverage_expected = NA_integer_,
      coverage_non_na = NA_integer_,
      strip_periods = I(vector("list", dplyr::n())),
      strip_values = I(vector("list", dplyr::n())),
      strip_labels = I(vector("list", dplyr::n())),
      strip_freq = NA_character_,
      operand_indicator_code = NA_character_
    )

  # Direct children map (journal parent_id, before technical reparent).
  is_operand <- !is.na(nodes$node_type) & nodes$node_type == "aggregate_input"
  children_of <- split(
    nodes$step_id[!is.na(nodes$parent_id)],
    nodes$parent_id[!is.na(nodes$parent_id)]
  )
  structural_children_of <- split(
    nodes$step_id[!is.na(nodes$parent_id) & !is_operand],
    nodes$parent_id[!is.na(nodes$parent_id) & !is_operand]
  )

  nodes$has_children <- vapply(
    nodes$step_id,
    function(sid) length(structural_children_of[[sid]] %||% character(0)) > 0L,
    logical(1)
  )
  nodes$child_count <- vapply(
    nodes$step_id,
    function(sid) length(structural_children_of[[sid]] %||% character(0)),
    integer(1)
  )

  # Coalesce winners: first non-NA direct structural child.
  coalesce_parents <- nodes$step_id[
    !is.na(nodes$operation) & nodes$operation == "coalesce"
  ]
  for (pid in coalesce_parents) {
    kids <- structural_children_of[[pid]] %||% character(0)
    if (length(kids) == 0L) next
    kid_rows <- nodes[match(kids, nodes$step_id), , drop = FALSE]
    winner_i <- which(!is.na(kid_rows$value))[1]
    if (length(winner_i) == 1L && !is.na(winner_i)) {
      nodes$coalesce_winner_arg[nodes$step_id == pid] <- as.integer(winner_i)
      nodes$is_coalesce_winner[nodes$step_id == kids[[winner_i]]] <- TRUE
    }
  }

  # Cross-frequency summaries from children.
  for (i in seq_len(nrow(nodes))) {
    if (!isTRUE(nodes$is_cross_frequency[[i]])) next
    sid <- nodes$step_id[[i]]
    kids <- children_of[[sid]] %||% character(0)
    if (length(kids) == 0L) next
    kid_rows <- nodes[match(kids, nodes$step_id), , drop = FALSE]
    operand_mask <- !is.na(kid_rows$node_type) & kid_rows$node_type == "aggregate_input"
    strip_rows <- if (any(operand_mask)) {
      kid_rows[operand_mask, , drop = FALSE]
    } else {
      kid_rows[!operand_mask, , drop = FALSE]
    }
    if (nrow(strip_rows) == 0L) next

    if (.is_aggregate_op(nodes$operation[[i]])) {
      child_freq <- strip_rows$frequency[[1]]
      nodes$child_frequency[[i]] <- child_freq
      nodes$subperiod_count[[i]] <- nrow(strip_rows)
      nodes$coverage_traced[[i]] <- nrow(strip_rows)
      nodes$coverage_non_na[[i]] <- sum(!is.na(strip_rows$value))
      nodes$coverage_expected[[i]] <- .safe_expected_subperiods(
        nodes$period[[i]], nodes$frequency[[i]], child_freq
      )
      nodes$strip_freq[[i]] <- child_freq
      nodes$strip_periods[[i]] <- as.character(strip_rows$period)
      nodes$strip_values[[i]] <- as.numeric(strip_rows$value)
      nodes$strip_labels[[i]] <- vapply(
        seq_len(nrow(strip_rows)),
        function(j) .period_strip_label(strip_rows$period[[j]], child_freq),
        character(1)
      )
      nodes$operand_indicator_code[[i]] <- strip_rows$indicator_code[[1]]
    } else if (.is_distribution_op(nodes$operation[[i]])) {
      # One parent-period child at coarser frequency.
      child_freq <- kid_rows$frequency[[1]]
      nodes$child_frequency[[i]] <- child_freq
      nodes$subperiod_count[[i]] <- 1L
      nodes$coverage_traced[[i]] <- 1L
      nodes$coverage_expected[[i]] <- NA_integer_
      nodes$coverage_non_na[[i]] <- sum(!is.na(kid_rows$value))
      nodes$strip_freq[[i]] <- child_freq
      nodes$strip_periods[[i]] <- as.character(kid_rows$period)
      nodes$strip_values[[i]] <- as.numeric(kid_rows$value)
      nodes$strip_labels[[i]] <- as.character(kid_rows$period_display)
    }
  }

  # Default collapse: level-1 nodes that have descendants.
  default_collapsed <- default_trace_collapsed_prefixes(nodes)
  nodes$default_collapsed <- nodes$step_id %in% default_collapsed

  # Display parent: identity when showing technical; reparent when hiding.
  nodes$display_parent_id <- nodes$parent_id
  nodes$hidden_hops <- 0L

  bridges <- empty_bridges

  if (!isTRUE(show_technical)) {
    # Root always kept; truncated always kept; hide keep==0 otherwise.
    is_root <- is.na(nodes$parent_id) | nodes$level == 0L
    keep_visible <- is_root | !nodes$is_technical | nodes$node_type == "truncated"
    visible_ids <- nodes$step_id[keep_visible]

    id_to_row <- stats::setNames(seq_len(nrow(nodes)), nodes$step_id)

    nearest_visible_ancestor <- function(sid) {
      cur <- nodes$parent_id[[id_to_row[[sid]]]]
      while (!is.na(cur) && nzchar(as.character(cur))) {
        if (cur %in% visible_ids) return(cur)
        cur <- nodes$parent_id[[id_to_row[[cur]]]]
      }
      NA_character_
    }

    count_hidden_between <- function(sid, display_parent) {
      if (is.na(display_parent)) return(0L)
      n <- 0L
      cur <- nodes$parent_id[[id_to_row[[sid]]]]
      while (!is.na(cur) && cur != display_parent) {
        if (!(cur %in% visible_ids)) n <- n + 1L
        cur <- nodes$parent_id[[id_to_row[[cur]]]]
      }
      as.integer(n)
    }

    display_parent <- rep(NA_character_, nrow(nodes))
    hidden_hops <- integer(nrow(nodes))
    for (i in seq_len(nrow(nodes))) {
      sid <- nodes$step_id[[i]]
      if (!(sid %in% visible_ids) || is.na(nodes$parent_id[[i]])) {
        display_parent[[i]] <- NA_character_
        hidden_hops[[i]] <- 0L
        next
      }
      dp <- nearest_visible_ancestor(sid)
      display_parent[[i]] <- dp
      hidden_hops[[i]] <- count_hidden_between(sid, dp)
    }
    nodes$display_parent_id <- display_parent
    nodes$hidden_hops <- hidden_hops

    # Bridge rows from visible children that jumped over technical ancestors.
    bridge_src <- nodes[
      nodes$step_id %in% visible_ids &
        !is.na(nodes$display_parent_id) &
        nodes$hidden_hops > 0L,
      ,
      drop = FALSE
    ]
    if (nrow(bridge_src) > 0L) {
      bridges <- bridge_src |>
        dplyr::group_by(.data$display_parent_id) |>
        dplyr::summarise(
          hidden_count = max(.data$hidden_hops),
          before_step_id = dplyr::first(.data$step_id),
          level = dplyr::first(.data$level),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          bridge_id = paste0("bridge:", .data$display_parent_id),
          after_step_id = NA_character_
        ) |>
        dplyr::select(
          "bridge_id", "display_parent_id", "hidden_count",
          "level", "after_step_id", "before_step_id"
        )
    }

    nodes <- nodes[nodes$step_id %in% visible_ids, , drop = FALSE]

    disp_children <- split(
      nodes$step_id[!is.na(nodes$display_parent_id)],
      nodes$display_parent_id[!is.na(nodes$display_parent_id)]
    )
    nodes$has_children <- vapply(
      nodes$step_id,
      function(sid) length(disp_children[[sid]] %||% character(0)) > 0L,
      logical(1)
    )
    nodes$child_count <- vapply(
      nodes$step_id,
      function(sid) length(disp_children[[sid]] %||% character(0)),
      integer(1)
    )
  }

  if ("hidden_hops" %in% names(nodes)) {
    nodes <- nodes |> dplyr::select(-"hidden_hops")
  }

  # Drop plan-only columns from the public VM surface (keep flags we need).
  drop_cols <- intersect(c("keep", "indicator"), names(nodes))
  if (length(drop_cols) > 0L) {
    nodes <- nodes |> dplyr::select(-dplyr::all_of(drop_cols))
  }

  # Operand snapshots are not structural tree nodes.
  nodes <- nodes |>
    dplyr::filter(is.na(.data$node_type) | .data$node_type != "aggregate_input")

  list(nodes = nodes, bridges = bridges)
}

# ---------- HTML renderer ---------------------------------------------------

.ledger_escape <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("")
  htmltools::htmlEscape(as.character(x))
}

.ledger_value_txt <- function(x) {
  .format_trace_value(x)
}

.ledger_assets_tags <- function() {
  # CSS only — trace-ledger.js is loaded once from mod_value_trace_panel_ui.
  htmltools::tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "trace-ledger.css"
  )
}

render_trace_ledger_selected_card <- function(cell, indicator_label = NULL) {
  if (is.null(cell)) return(NULL)
  label <- indicator_label %||% cell$indicator_code
  htmltools::tags$div(
    class = "tl-selected-card",
    htmltools::tags$div(
      class = "tl-selected-top",
      htmltools::tags$span(
        class = "tl-selected-country",
        paste0(toupper(cell$country %||% ""), "  ·  ", cell$country_id)
      )
    ),
    htmltools::tags$div(
      class = "tl-selected-main",
      htmltools::tags$div(
        class = "tl-selected-left",
        htmltools::tags$span(
          class = "tl-selected-code",
          cell$indicator_code
        ),
        htmltools::tags$span(
          class = "tl-selected-label",
          label
        )
      ),
      htmltools::tags$div(
        class = "tl-selected-right",
        htmltools::tags$span(
          class = "tl-freq-chip",
          .freq_chip_label(cell$frequency)
        ),
        htmltools::tags$span(
          class = "tl-period",
          format_period_display(cell$period, cell$frequency)
        ),
        htmltools::tags$span(
          class = "tl-value",
          .ledger_value_txt(cell$value)
        )
      )
    )
  )
}

.render_period_strip <- function(node, drilldown_input_id = NULL) {
  periods <- node$strip_periods[[1]]
  values  <- node$strip_values[[1]]
  labels  <- node$strip_labels[[1]]
  freq    <- node$strip_freq
  operand_code <- node$operand_indicator_code %||% node$indicator_code

  if (is.null(periods) || length(periods) == 0L) return(NULL)

  # Daily: summary only, no 31 cells.
  if (identical(freq, "d") || length(periods) > 12L) {
    non_na <- sum(!is.na(values))
    range_txt <- paste0(
      format_period_display(periods[[1]], freq),
      " — ",
      format_period_display(periods[[length(periods)]], freq)
    )
    return(htmltools::tags$div(
      class = "tl-period-strip tl-period-strip-summary",
      htmltools::tags$span(
        class = "tl-coverage",
        sprintf("%d / %d days", non_na, length(periods))
      ),
      htmltools::tags$span(class = "tl-range", range_txt)
    ))
  }

  cells <- lapply(seq_along(periods), function(j) {
    val <- values[[j]]
    has_val <- !is.na(val)
    cell_class <- paste(
      "tl-strip-cell",
      if (has_val) "has-value" else "missing-value",
      if (!is.null(drilldown_input_id)) "tl-strip-cell-drilldown"
    )
    htmltools::tags$div(
      class = cell_class,
      title = paste0(
        format_period_display(periods[[j]], freq),
        ": ",
        .ledger_value_txt(val),
        if (!is.null(drilldown_input_id)) " (click to trace)"
      ),
      `data-tl-drilldown` = if (!is.null(drilldown_input_id)) "1" else NULL,
      `data-input-id` = drilldown_input_id,
      `data-step-id` = node$step_id,
      `data-country-id` = node$country_id,
      `data-indicator-code` = operand_code,
      `data-frequency` = freq,
      `data-period` = periods[[j]],
      htmltools::tags$span(class = "tl-strip-label", labels[[j]] %||% ""),
      htmltools::tags$span(
        class = "tl-strip-value",
        if (has_val) {
          format(round(val, 1), nsmall = 1, trim = TRUE)
        } else {
          "\u25AA"
        }
      )
    )
  })

  htmltools::tags$div(class = "tl-period-strip", cells)
}

#' Render operand drill-down panel HTML for client-side injection.
render_operand_drilldown_html <- function(drilldown_state, drilldown_journal) {
  frag <- .render_operand_drilldown(drilldown_state, drilldown_journal)
  if (is.null(frag)) return("")
  as.character(htmltools::tagList(frag))
}

.render_operand_drilldown <- function(drilldown_state, drilldown_journal) {
  if (is.null(drilldown_state) || !is.list(drilldown_state)) return(NULL)
  if (is.null(drilldown_journal) || nrow(drilldown_journal) == 0L) {
    return(htmltools::tags$div(
      class = "tl-operand-drilldown tl-operand-drilldown-empty",
      "No trace steps for this sub-period."
    ))
  }

  period_disp <- format_period_display(
    drilldown_state$period,
    drilldown_state$frequency
  )
  header <- htmltools::tags$div(
    class = "tl-operand-drilldown-header",
    htmltools::tags$span(
      class = "tl-operand-drilldown-title",
      sprintf(
        "Trace for %s @ %s / %s",
        drilldown_state$indicator_code,
        toupper(drilldown_state$frequency),
        period_disp
      )
    )
  )

  rows <- lapply(seq_len(min(nrow(drilldown_journal), 12L)), function(i) {
    row <- drilldown_journal[i, , drop = FALSE]
    htmltools::tags$div(
      class = "tl-operand-drilldown-row",
      htmltools::tags$span(class = "tl-mono tl-operand-drilldown-step", row$step_id),
      htmltools::tags$span(class = "tl-operand-drilldown-code", row$indicator_code),
      htmltools::tags$span(class = "tl-freq-chip", .freq_chip_label(row$frequency)),
      htmltools::tags$span(class = "tl-operand-drilldown-value", .ledger_value_txt(row$value)),
      if (!is.na(row$operation) && nzchar(row$operation)) {
        htmltools::tags$span(class = "tl-operation", row$operation)
      }
    )
  })

  htmltools::tags$div(
    class = "tl-operand-drilldown",
    header,
    rows
  )
}

.render_frequency_panel <- function(node, drilldown_input_id = NULL) {
  if (!isTRUE(node$is_cross_frequency)) return(NULL)

  from_freq <- if (identical(node$transition_direction, "up")) {
    node$child_frequency
  } else {
    node$child_frequency
  }
  to_freq <- node$frequency

  # Aggregation: child freq → node freq; distribution: parent (child) → node.
  if (identical(node$transition_direction, "down")) {
    arrow_from <- .freq_chip_label(from_freq)
    arrow_to   <- .freq_chip_label(to_freq)
  } else {
    arrow_from <- .freq_chip_label(from_freq)
    arrow_to   <- .freq_chip_label(to_freq)
  }

  coverage_el <- NULL
  if (identical(node$transition_direction, "up")) {
    if (!is.na(node$coverage_expected)) {
      coverage_el <- htmltools::tags$div(
        class = "tl-ft-coverage",
        sprintf(
          "Coverage  %d / %d %s",
          node$coverage_non_na %||% node$coverage_traced,
          node$coverage_expected,
          switch(as.character(node$child_frequency),
                 m = "months", q = "quarters", d = "days", "sub-periods")
        )
      )
    } else {
      coverage_el <- htmltools::tags$div(
        class = "tl-ft-coverage",
        sprintf("%d sub-periods traced", node$coverage_traced %||% 0L)
      )
    }
  }

  period_line <- NULL
  if (identical(node$transition_direction, "up") &&
      length(node$strip_periods[[1]]) > 0L) {
    ps <- node$strip_periods[[1]]
    period_line <- htmltools::tags$div(
      class = "tl-ft-periods",
      paste0(
        format_period_display(ps[[1]], node$child_frequency),
        " \u2026 ",
        format_period_display(ps[[length(ps)]], node$child_frequency),
        "  \u2192  ",
        node$period_display
      )
    )
  } else if (identical(node$transition_direction, "down")) {
    parent_period_disp <- if (length(node$strip_periods[[1]]) > 0L) {
      format_period_display(node$strip_periods[[1]][[1]], node$child_frequency)
    } else {
      ""
    }
    parent_value <- if (length(node$strip_values[[1]]) > 0L) {
      .ledger_value_txt(node$strip_values[[1]][[1]])
    } else {
      "NA"
    }
    period_line <- htmltools::tags$div(
      class = "tl-ft-periods tl-ft-distribution",
      htmltools::tags$div(
        class = "tl-ft-dist-row",
        htmltools::tags$span(
          class = "tl-ft-dist-label", "Parent period"
        ),
        htmltools::tags$span(class = "tl-ft-dist-label", "Current period")
      ),
      htmltools::tags$div(
        class = "tl-ft-dist-row",
        htmltools::tags$span(parent_period_disp),
        htmltools::tags$span(class = "tl-freq-arrow", "\u2192"),
        htmltools::tags$span(node$period_display)
      ),
      htmltools::tags$div(
        class = "tl-ft-dist-row",
        htmltools::tags$span(
          paste0("Parent value  ", parent_value)
        ),
        htmltools::tags$span(
          paste0("Result  ", .ledger_value_txt(node$value))
        )
      )
    )
  }

  htmltools::tags$div(
    class = "tl-freq-panel",
    htmltools::tags$div(
      class = "tl-freq-panel-header",
      "FREQUENCY TRANSITION"
    ),
    htmltools::tags$div(
      class = "tl-freq-panel-arrow",
      htmltools::tags$span(class = "tl-freq-chip tl-freq-chip-violet", arrow_from),
      htmltools::tags$span(class = "tl-freq-arrow", "\u2192"),
      htmltools::tags$span(class = "tl-freq-chip tl-freq-chip-violet", arrow_to),
      htmltools::tags$span(class = "tl-freq-op", node$transition_label)
    ),
    period_line,
    coverage_el,
    if (identical(node$transition_direction, "up")) {
      .render_period_strip(node, drilldown_input_id = drilldown_input_id)
    },
    if (isTRUE(node$recursion_limited) && identical(node$transition_direction, "up")) {
      htmltools::tags$div(
        class = "tl-ft-note tl-ft-operand-note",
        sprintf(
          "Sub-period values shown; provenance not expanded (%d branches)",
          node$subperiod_count %||% length(node$strip_periods[[1]] %||% list())
        )
      )
    },
    if (!is.null(drilldown_input_id) && identical(node$transition_direction, "up")) {
      htmltools::tags$div(
        class = "tl-operand-drilldown-slot",
        `data-step-id` = node$step_id
      )
    },
    if (!is.na(node$note) && nzchar(node$note) &&
        identical(node$transition_direction, "down")) {
      htmltools::tags$div(class = "tl-ft-note", node$note)
    }
  )
}

.render_node_details <- function(node, drilldown_input_id = NULL) {
  rows <- list()

  if (!is.na(node$formula_raw) && nzchar(node$formula_raw)) {
    rows[[length(rows) + 1L]] <- htmltools::tags$div(
      class = "tl-detail-row",
      htmltools::tags$span(class = "tl-detail-key", "Formula"),
      htmltools::tags$span(class = "tl-detail-val tl-mono", node$formula_raw)
    )
  }
  if (!is.na(node$formula_filled) && nzchar(node$formula_filled)) {
    rows[[length(rows) + 1L]] <- htmltools::tags$div(
      class = "tl-detail-row",
      htmltools::tags$span(class = "tl-detail-key", "Calculation"),
      htmltools::tags$span(class = "tl-detail-val tl-mono", node$formula_filled)
    )
  }
  if (!is.na(node$time_relation) && nzchar(node$time_relation)) {
    rows[[length(rows) + 1L]] <- htmltools::tags$div(
      class = "tl-detail-row",
      htmltools::tags$span(class = "tl-detail-key", "Time relation"),
      htmltools::tags$span(class = "tl-detail-val", node$time_relation)
    )
  }
  if (!is.na(node$source_name) && nzchar(node$source_name)) {
    rows[[length(rows) + 1L]] <- htmltools::tags$div(
      class = "tl-detail-row",
      htmltools::tags$span(class = "tl-detail-key", "Source"),
      htmltools::tags$span(class = "tl-detail-val", node$source_name)
    )
  }
  if (!is.na(node$note) && nzchar(node$note) && !isTRUE(node$is_cross_frequency)) {
    rows[[length(rows) + 1L]] <- htmltools::tags$div(
      class = "tl-detail-row",
      htmltools::tags$span(class = "tl-detail-key", "Note"),
      htmltools::tags$span(class = "tl-detail-val", node$note)
    )
  }
  rows[[length(rows) + 1L]] <- htmltools::tags$div(
    class = "tl-detail-row tl-detail-meta",
    htmltools::tags$span(class = "tl-detail-key", "Step"),
    htmltools::tags$span(class = "tl-detail-val tl-mono", node$step_id)
  )

  freq_panel <- .render_frequency_panel(node, drilldown_input_id = drilldown_input_id)

  if (length(rows) == 0L && is.null(freq_panel)) return(NULL)

  htmltools::tags$div(
    class = "tl-node-details",
    freq_panel,
    if (isTRUE(node$recursion_limited) && !isTRUE(node$is_cross_frequency)) {
      htmltools::tags$div(
        class = "tl-warning",
        sprintf(
          "\u26A0 Sub-period values shown; provenance not expanded (%d branches). Click a period cell to trace one sub-period.",
          node$subperiod_count %||% 0L
        )
      )
    },
    rows
  )
}

.render_bridge_node <- function(bridge, show_technical_input_id = NULL) {
  htmltools::tags$div(
    class = "tl-node tl-bridge",
    `data-step-id` = bridge$bridge_id,
    `data-level` = bridge$level,
    style = sprintf(
      "--tl-level:%d; padding-left: %dpx;",
      bridge$level,
      min(bridge$level * 20L, 120L)
    ),
    htmltools::tags$div(
      class = "tl-node-body",
      htmltools::tags$div(
        class = "tl-line1",
        htmltools::tags$span(class = "tl-chevron-spacer"),
        htmltools::tags$span(class = "tl-status-icon", "\u22EE"),
        htmltools::tags$span(
          class = "tl-bridge-text",
          sprintf(
            "%d technical step%s hidden",
            bridge$hidden_count,
            if (bridge$hidden_count == 1L) "" else "s"
          )
        ),
        htmltools::tags$button(
          class = "tl-bridge-show btn btn-link btn-sm",
          type = "button",
          `data-input-id` = show_technical_input_id %||% "",
          "Show"
        )
      )
    )
  )
}

.render_ledger_node <- function(node,
                                child_nodes_html = NULL,
                                drilldown_input_id = NULL) {
  nt <- as.character(node$node_type %||% "")
  collapsed_default <- isTRUE(node$default_collapsed)

  chevron <- if (isTRUE(node$has_children)) {
    htmltools::tags$button(
      class = "tl-chevron",
      type = "button",
      `aria-expanded` = if (collapsed_default) "false" else "true",
      `data-step-id` = node$step_id,
      if (collapsed_default) "\u25B8" else "\u25BE"
    )
  } else {
    htmltools::tags$span(class = "tl-chevron-spacer")
  }

  winner_badge <- if (isTRUE(node$is_coalesce_winner)) {
    htmltools::tags$span(class = "tl-winner", "WINNER")
  } else {
    NULL
  }

  coalesce_summary <- NULL
  if (!is.na(node$operation) && identical(node$operation, "coalesce") &&
      !is.na(node$coalesce_winner_arg)) {
    coalesce_summary <- htmltools::tags$div(
      class = "tl-line2-extra",
      sprintf(
        "\u2713 Argument %d selected \u2014 first non-NA",
        node$coalesce_winner_arg
      )
    )
  }

  compact_transition <- NULL
  if (isTRUE(node$is_cross_frequency) && isTRUE(node$has_children)) {
    compact_transition <- htmltools::tags$span(
      class = "tl-compact-transition",
      sprintf(
        "[%s] \u2192 [%s]  %s \u00b7 %d sub-period%s",
        .freq_chip_label(node$child_frequency),
        .freq_chip_label(node$frequency),
        node$transition_label %||% "",
        node$subperiod_count %||% 0L,
        if ((node$subperiod_count %||% 0L) == 1L) "" else "s"
      )
    )
  }

  child_hint <- NULL
  if (isTRUE(node$has_children) && isTRUE(collapsed_default) &&
      !isTRUE(node$is_cross_frequency)) {
    child_hint <- htmltools::tags$span(
      class = "tl-child-hint",
      sprintf(
        "\u25B8 %d input%s",
        node$child_count,
        if (node$child_count == 1L) "" else "s"
      )
    )
  }

  details <- .render_node_details(node, drilldown_input_id = drilldown_input_id)

  htmltools::tags$div(
    class = paste(
      "tl-node",
      paste0("tl-type-", nt),
      if (collapsed_default) "is-collapsed" else NULL,
      if (isTRUE(node$is_coalesce_winner)) "is-winner" else NULL
    ),
    `data-step-id` = node$step_id,
    `data-level` = node$level,
    `data-parent-id` = node$display_parent_id %||% "",
    title = paste0("step ", node$step_id),
    style = sprintf(
      "--tl-level:%d; padding-left: %dpx;",
      node$level,
      min(as.integer(node$level) * 20L, 120L)
    ),
    htmltools::tags$div(
      class = "tl-node-body",
      role = "button",
      tabindex = "0",
      `aria-expanded` = "false",
      htmltools::tags$div(
        class = "tl-line1",
        chevron,
        htmltools::tags$span(
          class = "tl-status-icon",
          .node_status_icon(nt)
        ),
        htmltools::tags$span(
          class = "tl-indicator",
          node$indicator_code
        ),
        winner_badge,
        htmltools::tags$span(
          class = "tl-freq-chip",
          .freq_chip_label(node$frequency)
        ),
        htmltools::tags$span(
          class = "tl-period",
          node$period_display
        ),
        htmltools::tags$span(
          class = "tl-value",
          .ledger_value_txt(node$value)
        )
      ),
      htmltools::tags$div(
        class = "tl-line2",
        htmltools::tags$span(
          class = paste("tl-badge", paste0("tl-badge-", nt)),
          .node_type_badge(nt)
        ),
        if (!is.na(node$operation) && nzchar(node$operation)) {
          htmltools::tags$span(
            class = "tl-operation",
            paste0("\u00b7  ", node$operation)
          )
        },
        if (!is.na(node$source_name) && nzchar(node$source_name) &&
            identical(nt, "imported")) {
          htmltools::tags$span(
            class = "tl-source-inline",
            paste0("\u00b7  ", node$source_name)
          )
        },
        compact_transition,
        child_hint
      ),
      coalesce_summary
    ),
    details,
    htmltools::tags$div(
      class = "tl-children",
      child_nodes_html
    )
  )
}

#' Render a full Trace Ledger tree as htmltools tags.
#'
#' @param vm Result of build_trace_ledger_vm().
#' @param cell Selected preview cell (for header card); optional.
#' @param indicator_label Human label for selected indicator.
#' @param show_technical_input_id Full Shiny input id for enabling technical nodes.
render_trace_ledger_tree <- function(vm,
                                     cell = NULL,
                                     indicator_label = NULL,
                                     show_technical_input_id = NULL,
                                     drilldown_input_id = NULL) {
  nodes <- vm$nodes
  bridges <- vm$bridges

  if (is.null(nodes) || nrow(nodes) == 0L) {
    return(htmltools::tags$div(
      class = "tl-root",
      `data-tl-show-technical-input` = show_technical_input_id %||% "",
      .ledger_assets_tags(),
      htmltools::tags$p(class = "tl-empty", "No trace steps to display.")
    ))
  }

  .blank_id <- function(x) {
    is.null(x) || length(x) != 1L || is.na(x) || !nzchar(as.character(x))
  }

  # Build adjacency on display_parent_id.
  children_map <- list()
  for (i in seq_len(nrow(nodes))) {
    pid <- if ("display_parent_id" %in% names(nodes)) {
      nodes$display_parent_id[[i]]
    } else {
      NA_character_
    }
    if (.blank_id(pid)) next
    children_map[[pid]] <- c(children_map[[pid]] %||% character(0), nodes$step_id[[i]])
  }

  bridges_by_parent <- list()
  if (!is.null(bridges) && nrow(bridges) > 0L) {
    for (i in seq_len(nrow(bridges))) {
      pid <- bridges$display_parent_id[[i]]
      bridges_by_parent[[pid]] <- c(
        bridges_by_parent[[pid]] %||% list(),
        list(bridges[i, , drop = FALSE])
      )
    }
  }

  render_subtree <- function(step_id) {
    row <- nodes[nodes$step_id == step_id, , drop = FALSE]
    if (nrow(row) != 1L) return(NULL)

    kid_ids <- children_map[[step_id]] %||% character(0)
    kid_html <- list()

    parent_bridges <- bridges_by_parent[[step_id]] %||% list()
    bridge_before <- list()
    for (b in parent_bridges) {
      key <- b$before_step_id[[1]]
      bridge_before[[key]] <- b
    }

    for (kid in kid_ids) {
      if (!is.null(bridge_before[[kid]])) {
        kid_html[[length(kid_html) + 1L]] <- .render_bridge_node(
          bridge_before[[kid]],
          show_technical_input_id = show_technical_input_id
        )
      }
      kid_html[[length(kid_html) + 1L]] <- render_subtree(kid)
    }

    .render_ledger_node(
      row,
      child_nodes_html = kid_html,
      drilldown_input_id = drilldown_input_id
    )
  }

  display_parent <- if ("display_parent_id" %in% names(nodes)) {
    nodes$display_parent_id
  } else {
    rep(NA_character_, nrow(nodes))
  }
  roots <- nodes$step_id[vapply(display_parent, .blank_id, logical(1))]
  if (length(roots) == 0L) {
    roots <- nodes$step_id[nodes$level == min(nodes$level, na.rm = TRUE)]
  }

  tree_html <- lapply(roots, render_subtree)

  legend <- htmltools::tags$div(
    class = "tl-legend",
    htmltools::tags$span(class = "tl-legend-item",
      htmltools::tags$span(class = "tl-status-icon tl-type-computed", "\u25CF"),
      " COMPUTED"
    ),
    htmltools::tags$span(class = "tl-legend-item",
      htmltools::tags$span(class = "tl-status-icon tl-type-imported", "\u25CF"),
      " IMPORTED"
    ),
    htmltools::tags$span(class = "tl-legend-item",
      htmltools::tags$span(class = "tl-status-icon tl-type-no_value", "\u2205"),
      " NO VALUE"
    ),
    htmltools::tags$span(class = "tl-legend-item",
      htmltools::tags$span(class = "tl-status-icon tl-type-opaque", "\u26A0"),
      " OPAQUE"
    ),
    htmltools::tags$span(class = "tl-legend-item tl-legend-freq",
      "Frequency transition: ",
      htmltools::tags$span(class = "tl-freq-chip tl-freq-chip-violet", "M"),
      " \u2192 ",
      htmltools::tags$span(class = "tl-freq-chip tl-freq-chip-violet", "Y")
    )
  )

  htmltools::tags$div(
    class = "tl-root",
    `data-tl-show-technical-input` = show_technical_input_id %||% "",
    .ledger_assets_tags(),
    if (!is.null(cell)) {
      render_trace_ledger_selected_card(cell, indicator_label)
    },
    legend,
    htmltools::tags$div(
      class = "tl-tree",
      tree_html
    )
  )
}
