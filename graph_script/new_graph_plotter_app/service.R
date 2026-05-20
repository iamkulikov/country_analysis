library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(rlang)
library(tibble)
library(readxl)
library(writexl)
library(openxlsx)

# ---------- profiling (options(plotter.profile = TRUE) or PLOTTER_PROFILE=1) ----

plotter_profile_enabled <- function() {
  isTRUE(getOption("plotter.profile", FALSE)) ||
    identical(Sys.getenv("PLOTTER_PROFILE", ""), "1")
}

plotter_profile_all_buildable_enabled <- function() {
  isTRUE(getOption("plotter.profile_all_buildable", FALSE)) ||
    identical(Sys.getenv("PLOTTER_PROFILE_ALL_BUILDABLE", ""), "1")
}

#' Defer `rv$built` updates during incremental gallery build (phase 11.1).
#' Default ON; set `GALLERY_DEFER_UI=0` or `options(gallery.defer_ui = FALSE)` to compare.
gallery_defer_ui_enabled <- function() {
  env <- Sys.getenv("GALLERY_DEFER_UI", "")
  if (identical(env, "0")) return(FALSE)
  if (identical(env, "1")) return(TRUE)
  opt <- getOption("gallery.defer_ui", NA)
  if (isTRUE(opt)) return(TRUE)
  if (isFALSE(opt)) return(FALSE)
  TRUE
}

plot_export_dpi <- 150L
gallery_thumb_height_px <- 260L
# Upper bound for editor preview (main panel col-8). R scales PNG for renderImage.
# Visible height is min(editor_preview_max_height_px, calc(100vh - editor_preview_vh_chrome_rem)).
editor_preview_max_width_px <- 640L
editor_preview_max_height_px <- 450L
editor_preview_vh_chrome_rem <- 14

fit_image_display_dims <- function(graph_params, max_width_px, max_height_px) {
  w <- as.numeric(graph_params$width %||% 1800)
  h <- as.numeric(graph_params$height %||% 900)
  if (!is.finite(w) || !is.finite(h) || w <= 0 || h <= 0) {
    return(list(
      width = as.integer(max_width_px),
      height = as.integer(max_height_px)
    ))
  }
  scale <- min(max_width_px / w, max_height_px / h)
  list(
    width = max(1L, round(w * scale)),
    height = max(1L, round(h * scale))
  )
}

gallery_thumb_dims <- function(graph_params, height_px = gallery_thumb_height_px) {
  if (is.null(graph_params)) {
    return(list(width = 600L, height = as.integer(height_px)))
  }
  w <- graph_params$width %||% 1800
  h <- graph_params$height %||% 900
  fit_image_display_dims(
    list(width = w, height = h),
    max_width_px = max(1L, round(height_px * w / h)),
    max_height_px = as.integer(height_px)
  )
}

editor_preview_display_dims <- function(graph_params) {
  w <- as.numeric(graph_params$width %||% 1800)
  h <- as.numeric(graph_params$height %||% 900)
  max_h <- editor_preview_max_height_px
  max_w <- editor_preview_max_width_px
  # Landscape: width cap 640 would limit height to ~320 at 2:1; widen so height can use max_h.
  if (is.finite(w) && is.finite(h) && h > 0 && w > h) {
    max_w <- max(max_w, round(max_h * w / h))
  }
  fit_image_display_dims(
    list(width = w, height = h),
    max_width_px = max_w,
    max_height_px = max_h
  )
}

gallery_session_thumb_dir <- function(session) {
  base <- file.path(tempdir(), "plotter_gallery_thumbs")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  token <- session$token %||% paste0("pid_", Sys.getpid())
  file.path(base, token)
}

gallery_thumb_path_for <- function(cache_dir, graph_name) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", graph_name %||% "graph")
  file.path(cache_dir, paste0(safe, ".png"))
}

write_export_image <- function(plot,
                               graph_params,
                               dest_path,
                               device = c("png", "jpeg"),
                               dpi = plot_export_dpi) {
  device <- match.arg(device)
  w <- graph_params$width %||% 1800
  h <- graph_params$height %||% 900
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  args <- list(
    filename = dest_path,
    plot = plot,
    device = device,
    width = w,
    height = h,
    units = "px",
    dpi = dpi
  )
  if (identical(device, "jpeg")) {
    args$bg <- "white"
  }
  do.call(ggplot2::ggsave, args)
  invisible(dest_path)
}

write_export_png <- function(plot, graph_params, dest_path, dpi = plot_export_dpi) {
  write_export_image(plot, graph_params, dest_path, device = "png", dpi = dpi)
}

write_gallery_thumbnail <- function(graph, graph_params, dest_path) {
  write_export_png(graph, graph_params, dest_path)
}

enrich_gallery_built_item <- function(item, cache_dir) {
  if (!isTRUE(item$ok) || is.null(item$graph)) {
    return(item)
  }
  path <- gallery_thumb_path_for(cache_dir, item$graph_name %||% "graph")
  thumb_ok <- tryCatch({
    write_gallery_thumbnail(item$graph, item$graph_params, path)
    TRUE
  }, error = function(e) {
    item$thumb_error <- conditionMessage(e)
    FALSE
  })
  if (!isTRUE(thumb_ok)) {
    return(item)
  }
  c(item, list(thumb_path = path))
}

cleanup_gallery_thumb_dir <- function(cache_dir) {
  if (!is.null(cache_dir) && nzchar(cache_dir) && dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

editor_session_preview_dir <- function(session) {
  base <- file.path(tempdir(), "plotter_editor_preview")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)
  token <- session$token %||% paste0("pid_", Sys.getpid())
  file.path(base, token)
}

editor_preview_path <- function(cache_dir) {
  file.path(cache_dir, "preview.png")
}

write_editor_preview <- function(built_item, cache_dir) {
  path <- editor_preview_path(cache_dir)
  write_export_png(built_item$graph, built_item$graph_params, path)
  path
}

#' Ensure `preview_path` exists for editor `renderImage` (reuse gallery thumb when present).
prepare_editor_preview_item <- function(built_item, session) {
  if (!isTRUE(built_item$ok) || is.null(built_item$graph)) {
    return(NULL)
  }
  preview_path <- built_item$preview_path %||% built_item$thumb_path
  if (!is.null(preview_path) && nzchar(preview_path) && file.exists(preview_path)) {
    return(c(built_item, list(preview_path = preview_path)))
  }
  preview_dir <- session$userData$editor_preview_dir
  if (is.null(preview_dir)) {
    preview_dir <- editor_session_preview_dir(session)
    session$userData$editor_preview_dir <- preview_dir
  }
  dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)
  preview_path <- tryCatch(
    write_editor_preview(built_item, preview_dir),
    error = function(e) {
      built_item$preview_error <- conditionMessage(e)
      NULL
    }
  )
  if (is.null(preview_path)) {
    return(NULL)
  }
  c(built_item, list(preview_path = preview_path))
}

graphplan_activate_all <- function(plan) {
  out <- tibble::as_tibble(plan)
  out$active <- 1L
  out
}

graphplan_deactivate_all <- function(plan) {
  out <- tibble::as_tibble(plan)
  out$active <- 0L
  out
}

#' Set `active = 1` for one graphplan row (phase 13).
activate_graphplan_row <- function(plan, row_id) {
  plan <- tibble::as_tibble(plan)
  row_id <- as.integer(row_id)[1]
  if (is.na(row_id) || row_id < 1L || row_id > nrow(plan)) {
    rlang::abort(glue("activate_graphplan_row: invalid row_id {row_id}."))
  }
  if ("active" %in% names(plan)) {
    plan$active[row_id] <- 1L
  }
  plan
}

#' Gallery card display status from validation + optional built cache (phase 13–14).
#' @param row_active If FALSE, card is placed in the inactive gallery section regardless of check_status.
gallery_display_status <- function(check_status, can_build, built_item = NULL, row_active = TRUE) {
  if (!isTRUE(row_active)) {
    return("inactive")
  }
  cs <- as.character(check_status)[1]
  if (identical(cs, "warning")) {
    return("warning")
  }
  if (!isTRUE(can_build)) {
    return("validation_error")
  }
  if (is.null(built_item)) {
    return("not_built")
  }
  if (isTRUE(built_item$ok)) {
    return("built_ok")
  }
  "build_failed"
}

#' Badge label and CSS class for a gallery display status.
gallery_status_badge <- function(display_status) {
  switch(
    display_status,
    built_ok = list(label = "Built", class = "gallery-status-built"),
    build_failed = list(label = "Build failed", class = "gallery-status-err"),
    validation_error = list(label = "Validation error", class = "gallery-status-err"),
    warning = list(label = "Warning", class = "gallery-status-warning"),
    not_built = list(label = "Not built", class = "gallery-status-not-built"),
    inactive = list(label = "Inactive", class = "gallery-status-inactive"),
    list(label = as.character(display_status)[1], class = "gallery-status-inactive")
  )
}

built_item_for_row <- function(built_list, row_id, graph_name = NULL) {
  if (length(built_list) == 0L) {
    return(NULL)
  }
  row_id <- as.integer(row_id)[1]
  for (item in built_list) {
    if (!is.null(item$row_id) && identical(as.integer(item$row_id), row_id)) {
      return(item)
    }
  }
  if (!is.null(graph_name) && nzchar(graph_name)) {
    nm <- as.character(graph_name)[1]
    if (nm %in% names(built_list)) {
      return(built_list[[nm]])
    }
  }
  NULL
}

#' Merge validation `row_status` with `rv$built` for Gallery UI (phase 13–14).
build_gallery_manifest <- function(validation, built_list = list()) {
  empty <- list(active = list(), inactive = list())
  if (is.null(validation) || is.null(validation$row_status) ||
      nrow(validation$row_status) == 0L) {
    return(empty)
  }
  built_list <- as.list(built_list %||% list())
  rs <- validation$row_status
  active_cards <- list()
  inactive_cards <- list()
  for (i in seq_len(nrow(rs))) {
    row <- rs[i, , drop = FALSE]
    nm <- as.character(row$graph_name[[1]] %||% paste0("row_", row$row_id[[1]]))
    rid <- as.integer(row$row_id[[1]])
    built_item <- built_item_for_row(built_list, rid, nm)
    row_active <- isTRUE(active_flag_vec(row)[1])
    display_status <- gallery_display_status(
      row$check_status[[1]],
      row$can_build[[1]],
      built_item,
      row_active = row_active
    )
    card <- list(
      graph_name = nm,
      row_id = rid,
      display_status = display_status,
      check_status = as.character(row$check_status[[1]]),
      can_build = isTRUE(row$can_build[[1]]),
      messages = row$messages[[1]],
      built_item = built_item
    )
    if (identical(display_status, "inactive")) {
      inactive_cards[[nm]] <- card
    } else {
      active_cards[[nm]] <- card
    }
  }
  list(active = active_cards, inactive = inactive_cards)
}

gallery_manifest_card <- function(manifest, graph_name) {
  if (is.null(manifest)) {
    return(NULL)
  }
  nm <- as.character(graph_name)[1]
  manifest$active[[nm]] %||% manifest$inactive[[nm]]
}

#' Drop built cache entries that no longer match validation (inactive / not buildable).
prune_built_list_for_validation <- function(built_list, validation) {
  built_list <- as.list(built_list %||% list())
  if (length(built_list) == 0L || is.null(validation) ||
      is.null(validation$row_status) || nrow(validation$row_status) == 0L) {
    return(built_list)
  }
  rs <- validation$row_status
  # Vectorized: isTRUE() on a column is wrong (only TRUE for length-1 logical).
  buildable <- !is.na(rs$can_build) & as.logical(rs$can_build)
  keep_ids <- rs$row_id[buildable]
  keep_names <- as.character(rs$graph_name[buildable])
  keep <- vapply(
    built_list,
    function(item) {
      rid <- item$row_id
      if (!is.null(rid) && as.integer(rid) %in% keep_ids) {
        return(TRUE)
      }
      nm <- item$graph_name %||% names(built_list)[1]
      as.character(nm) %in% keep_names
    },
    logical(1)
  )
  built_list[keep]
}

remove_built_list_row <- function(built_list, row_id) {
  built_list <- as.list(built_list %||% list())
  if (length(built_list) == 0L) {
    return(built_list)
  }
  row_id <- as.integer(row_id)[1]
  built_list[!vapply(
    built_list,
    function(item) !is.null(item$row_id) && identical(as.integer(item$row_id), row_id),
    logical(1)
  )]
}

gallery_output_id_sanitize <- function(nm) {
  paste0("gallery_plot_", gsub("[^A-Za-z0-9]", "_", nm))
}

#' Run `expr` and log elapsed time when profiling is enabled.
profile_step <- function(label, expr, enabled = plotter_profile_enabled()) {
  if (!enabled) return(force(expr))
  t0 <- proc.time()
  on.exit(
    message(sprintf("[profile] %s: %.2fs", label, (proc.time() - t0)[["elapsed"]])),
    add = TRUE
  )
  force(expr)
}

#' Lightweight DOM rebuild matching gallery `renderUI` (headless UI overhead proxy).
gallery_ui_tag_list <- function(built_named_list, cards_per_row = 3L) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("gallery_ui_tag_list requires the htmltools package", call. = FALSE)
  }
  nms <- names(built_named_list)
  if (length(nms) == 0L) {
    return(htmltools::tagList())
  }
  row_groups <- split(nms, ceiling(seq_along(nms) / cards_per_row))
  rows <- lapply(row_groups, function(group) {
    cards <- lapply(group, function(nm) {
      item <- built_named_list[[nm]]
      ok <- isTRUE(item$ok)
      htmltools::tags$div(
        class = if (ok) "gallery-card" else "gallery-card gallery-card-error",
        htmltools::tags$div(class = "gallery-card-title", nm),
        if (ok) {
          htmltools::tags$div(
            class = "gallery-card-plot",
            htmltools::tags$div(
              class = "shiny-plot-output",
              id = gallery_output_id_sanitize(nm)
            )
          )
        } else {
          htmltools::tags$p(
            class = "text-danger small",
            item$error %||% "build failed"
          )
        }
      )
    })
    htmltools::tagList(cards)
  })
  htmltools::tagList(rows)
}

#' Per-graph `build_graph_row` timings (component A — pure compute).
profile_gallery_compute_per_graph <- function(graphplan,
                                              row_ids,
                                              FD,
                                              country_iso3c,
                                              peers_fname,
                                              verbose = FALSE) {
  if (length(row_ids) == 0L) {
    empty <- tibble::tibble(
      graph_index = integer(0),
      row_id = integer(0),
      graph_name = character(0),
      ok = logical(0),
      seconds_build = numeric(0),
      seconds_cumulative_build = numeric(0)
    )
    attr(empty, "built") <- list()
    return(empty)
  }

  times <- vector("double", length(row_ids))
  graph_names <- character(length(row_ids))
  ok_flags <- logical(length(row_ids))
  built <- list()

  for (k in seq_along(row_ids)) {
    rid <- row_ids[[k]]
    row <- graphplan[rid, , drop = FALSE]
    t0 <- proc.time()
    item <- build_graph_row(
      graphplan_row = row,
      FD = FD,
      country_iso3c = country_iso3c,
      peers_fname = peers_fname,
      verbose = verbose
    )
    times[k] <- (proc.time() - t0)[["elapsed"]]
    key <- item$graph_name %||% paste0("row_", rid)
    graph_names[k] <- key
    ok_flags[k] <- isTRUE(item$ok)
    built[[key]] <- c(
      item,
      list(row_id = rid, status = if (isTRUE(item$ok)) "ok" else "error")
    )
  }

  out <- tibble::tibble(
    graph_index = seq_along(row_ids),
    row_id = unlist(row_ids, use.names = FALSE),
    graph_name = graph_names,
    ok = ok_flags,
    seconds_build = round(times, 4),
    seconds_cumulative_build = round(cumsum(times), 3)
  )
  attr(out, "built") <- built
  class(out) <- c("gallery_per_graph_profile", class(out))
  out
}

#' Simulate incremental `renderUI` cost: rebuild DOM for 1..n cards each step (O(n^2) proxy).
simulate_gallery_renderui_steps <- function(built_list, cards_per_row = 3L) {
  nms <- names(built_list)
  n <- length(nms)
  if (n == 0L) {
    return(tibble::tibble(
      step_i = integer(0),
      n_cards = integer(0),
      seconds_renderui = numeric(0),
      seconds_cumulative_ui_sim = numeric(0)
    ))
  }
  sec <- numeric(n)
  for (i in seq_len(n)) {
    subset <- built_list[seq_len(i)]
    t0 <- proc.time()
    invisible(gallery_ui_tag_list(subset, cards_per_row = cards_per_row))
    sec[i] <- (proc.time() - t0)[["elapsed"]]
  }
  tibble::tibble(
    step_i = seq_len(n),
    n_cards = seq_len(n),
    seconds_renderui = round(sec, 5),
    seconds_cumulative_ui_sim = round(cumsum(sec), 3)
  )
}

summarize_gallery_scale_profile <- function(per_graph_df,
                                            ui_sim_df = NULL,
                                            n_plan_rows = NA_integer_) {
  n <- nrow(per_graph_df)
  build_total <- if (n > 0L) sum(per_graph_df$seconds_build) else 0
  build_mean <- if (n > 0L) mean(per_graph_df$seconds_build) else NA_real_
  build_sd <- if (n > 1L) stats::sd(per_graph_df$seconds_build) else NA_real_

  idx_mid <- if (n >= 10L) max(1L, floor(n / 2L)) else NA_integer_
  idx_late <- if (n >= 10L) max(1L, floor(n * 0.9)) else NA_integer_
  slope_build <- if (!is.na(idx_mid) && n > 0L) {
    per_graph_df$seconds_build[idx_late] / per_graph_df$seconds_build[idx_mid]
  } else {
    NA_real_
  }

  ui_total <- NA_real_
  ui_step_growth <- NA_real_
  if (!is.null(ui_sim_df) && nrow(ui_sim_df) > 0L) {
    ui_total <- sum(ui_sim_df$seconds_renderui)
    if (n >= 10L) {
      ui_step_growth <- ui_sim_df$seconds_renderui[idx_late] /
        ui_sim_df$seconds_renderui[idx_mid]
    }
  }

  compute_vs_ui_ratio <- if (!is.na(ui_total) && build_total > 0) {
    ui_total / build_total
  } else {
    NA_real_
  }

  renderui_linear_coef <- NA_real_
  if (!is.null(ui_sim_df) && nrow(ui_sim_df) >= 5L) {
    fit <- stats::lm(seconds_renderui ~ step_i, data = ui_sim_df)
    renderui_linear_coef <- unname(stats::coef(fit)[["step_i"]])
  }

  tibble::tibble(
    n_plan_rows = n_plan_rows,
    n_buildable = n,
    n_built_ok = sum(per_graph_df$ok, na.rm = TRUE),
    seconds_compute_total = round(build_total, 2),
    seconds_compute_mean = round(build_mean, 4),
    seconds_compute_sd = round(build_sd, 4),
    seconds_ui_sim_total = round(ui_total, 2),
    ui_sim_vs_compute_ratio = round(compute_vs_ui_ratio, 2),
    build_time_slope_late_vs_mid = round(slope_build, 3),
    renderui_step_slope_late_vs_mid = round(ui_step_growth, 3),
    renderui_linear_coef_per_step = round(renderui_linear_coef, 6)
  )
}

#' Theoretical interactive overhead if each `rv$built` assign re-renders all plots.
estimate_renderplot_o_n2_seconds <- function(n_graphs, seconds_per_plot = 0.05) {
  if (n_graphs <= 0L) return(0)
  sum(seq_len(n_graphs)) * seconds_per_plot
}

#' Headless phase-10 research: compute (A) + simulated Shiny `renderUI` (B).
run_plotter_gallery_scale_research <- function(FD,
                                               plan,
                                               country_iso3c,
                                               peers_fname,
                                               activate_all = TRUE,
                                               verbose = FALSE,
                                               write_csv_dir = NULL) {
  plan_use <- if (isTRUE(activate_all)) graphplan_activate_all(plan) else plan
  val <- validate_graphplan_for_app(
    plan_use, FD, country_iso3c, peers_fname
  )
  build_ids <- val$row_status |>
    dplyr::filter(.data$can_build) |>
    dplyr::pull(.data$row_id)

  message(glue::glue(
    "Gallery scale research: {nrow(plan_use)} plan rows, ",
    "{length(build_ids)} buildable (RUS / {country_iso3c})"
  ))

  t0 <- proc.time()
  per_graph <- profile_gallery_compute_per_graph(
    graphplan = plan_use,
    row_ids = build_ids,
    FD = FD,
    country_iso3c = country_iso3c,
    peers_fname = peers_fname,
    verbose = verbose
  )
  compute_elapsed <- (proc.time() - t0)[["elapsed"]]
  message(sprintf(
    "[profile] gallery.per_graph_compute (%d rows): %.2fs",
    length(build_ids),
    compute_elapsed
  ))

  built <- attr(per_graph, "built", exact = TRUE)
  t0 <- proc.time()
  ui_sim <- simulate_gallery_renderui_steps(built)
  ui_sim_elapsed <- (proc.time() - t0)[["elapsed"]]
  message(sprintf(
    "[profile] gallery.simulate_renderui_o_n2 (%d steps): %.2fs (cumulative sim %.2fs)",
    nrow(ui_sim),
    ui_sim_elapsed,
    if (nrow(ui_sim) > 0L) max(ui_sim$seconds_cumulative_ui_sim) else 0
  ))

  summary_tbl <- summarize_gallery_scale_profile(
    per_graph,
    ui_sim_df = ui_sim,
    n_plan_rows = nrow(plan_use)
  )
  attr(summary_tbl, "validation_summary") <- val$summary
  attr(summary_tbl, "per_graph") <- per_graph
  attr(summary_tbl, "ui_sim") <- ui_sim

  if (!is.null(write_csv_dir) && dir.exists(write_csv_dir)) {
    utils::write.csv(
      per_graph,
      file.path(write_csv_dir, "gallery_per_graph_compute.csv"),
      row.names = FALSE
    )
    utils::write.csv(
      ui_sim,
      file.path(write_csv_dir, "gallery_renderui_simulation.csv"),
      row.names = FALSE
    )
    utils::write.csv(
      summary_tbl,
      file.path(write_csv_dir, "gallery_scale_summary.csv"),
      row.names = FALSE
    )
    message("Wrote CSV artifacts to ", write_csv_dir)
  }

  print(summary_tbl, row.names = FALSE)
  invisible(summary_tbl)
}

#' Headless baseline for fixture RUS / 2_graphlib.xlsx (smoke_test or manual).
run_plotter_profile_baseline <- function(FD,
                                         plan,
                                         country_iso3c,
                                         peers_fname,
                                         import_paths = NULL,
                                         update_plot_reps = 5L,
                                         profile_all_buildable = FALSE,
                                         per_graph_timing = NULL,
                                         simulate_gallery_ui = NULL) {
  rows <- list()
  add_row <- function(step, seconds) {
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      step = step,
      seconds = round(seconds, 3)
    )
    message(sprintf("[profile] %s: %.2fs", step, seconds))
  }
  timed <- function(step, expr) {
    t0 <- proc.time()
    res <- force(expr)
    add_row(step, (proc.time() - t0)[["elapsed"]])
    res
  }

  profile_all <- isTRUE(profile_all_buildable) ||
    plotter_profile_all_buildable_enabled()
  if (is.null(per_graph_timing)) per_graph_timing <- profile_all
  if (is.null(simulate_gallery_ui)) simulate_gallery_ui <- profile_all

  plan_work <- if (profile_all) graphplan_activate_all(plan) else plan

  if (!is.null(import_paths)) {
    yqm <- import_paths$yqm_file %||% import_paths[["yqm"]]
    d_file <- import_paths$d_file %||% import_paths[["d"]]
    timed(
      "cold_start.importData",
      importData(
        yqm_file = yqm,
        d_file = d_file,
        sheet_keys = import_paths$sheet_keys %||% c(y = "y", q = "q", m = "m"),
        format = import_paths$format %||% "auto",
        add_time = isTRUE(import_paths$add_time %||% TRUE)
      )
    )
  }

  val <- timed(
    if (profile_all) {
      "import.validate_graphplan_for_app (all active)"
    } else {
      "import.validate_graphplan_for_app"
    },
    validate_graphplan_for_app(plan_work, FD, country_iso3c, peers_fname)
  )
  build_ids <- val$row_status |>
    dplyr::filter(.data$can_build) |>
    dplyr::pull(.data$row_id)
  n_build <- length(build_ids)

  built <- list()
  if (n_build > 0L) {
    if (isTRUE(per_graph_timing)) {
      per_graph <- timed(
        glue::glue("gallery.per_graph_compute ({n_build} rows)"),
        profile_gallery_compute_per_graph(
          graphplan = plan_work,
          row_ids = build_ids,
          FD = FD,
          country_iso3c = country_iso3c,
          peers_fname = peers_fname
        )
      )
      built <- attr(per_graph, "built", exact = TRUE)
      if (isTRUE(simulate_gallery_ui)) {
        timed(
          glue::glue("gallery.simulate_renderui_o_n2 ({n_build} steps)"),
          simulate_gallery_renderui_steps(built)
        )
      }
    } else {
      built <- timed(
        glue::glue("gallery.build_valid_batch ({n_build} rows)"),
        build_graphplan_rows(
          graphplan = plan_work,
          row_ids = build_ids,
          FD = FD,
          country_iso3c = country_iso3c,
          peers_fname = peers_fname,
          validation = val
        )
      )
    }
  }

  if (n_build > 0L && !isTRUE(per_graph_timing)) {
    preview_row <- plan_work[build_ids[1], , drop = FALSE]
    t0 <- proc.time()
    preview_row_id <- build_ids[1]
    editor_cache_key <- NULL
    editor_cached_val <- NULL
    plan_rev <- 0L
    for (i in seq_len(update_plot_reps)) {
      cache_key <- editor_validation_cache_key(
        preview_row,
        country_iso3c,
        plan,
        row_id = preview_row_id,
        editor_mode = "edit",
        plan_validation_revision = plan_rev
      )
      cache_hit <- !is.null(editor_cached_val) &&
        identical(cache_key, editor_cache_key)
      if (!cache_hit) {
        editor_cached_val <- validate_graphplan_row(
          preview_row,
          FD,
          country_iso3c,
          peers_fname,
          graphplan = plan,
          row_id = preview_row_id,
          editor_mode = "edit"
        )
        editor_cache_key <- cache_key
      }
      build_graph_row(preview_row, FD, country_iso3c, peers_fname)
    }
    add_row(
      glue::glue("editor.update_plot x{update_plot_reps} (validate_row + build_row)"),
      (proc.time() - t0)[["elapsed"]]
    )

    saved_row_id <- build_ids[1]
    save_plan <- plan_work
    t0 <- proc.time()
    validation <- validate_graphplan_for_app(
      save_plan, FD, country_iso3c, peers_fname
    )
    refresh_gallery_built_for_row(
      built_list = built,
      row_id = saved_row_id,
      graphplan = validation$plan,
      FD = FD,
      country_iso3c = country_iso3c,
      peers_fname = peers_fname
    )
    add_row("save.end_to_end (validate + refresh_gallery)", (proc.time() - t0)[["elapsed"]])
  }

  out <- dplyr::bind_rows(rows)
  attr(out, "summary") <- val$summary
  attr(out, "n_buildable") <- n_build
  attr(out, "profile_all_buildable") <- profile_all
  if (exists("per_graph", inherits = FALSE)) {
    attr(out, "per_graph") <- per_graph
  }
  out
}

print_plotter_profile_baseline <- function(timing_df) {
  if (!is.data.frame(timing_df) || nrow(timing_df) == 0) {
    message("[profile] No timing rows recorded.")
    return(invisible(timing_df))
  }
  s <- attr(timing_df, "summary", exact = TRUE)
  hdr <- if (!is.null(s)) {
    glue::glue(
      "Fixture summary: active={s$n_active}, buildable={s$n_buildable}, ",
      "errors={s$n_errors}, inactive={s$n_inactive}"
    )
  } else {
    "Profile baseline"
  }
  message(hdr)
  print(timing_df, row.names = FALSE)
  invisible(timing_df)
}

# importData — копия из download_script/import.R (строки 132–274).
# При изменении загрузки Filled DB в ядре проекта синхронизировать этот блок вручную.
# Не source() полный import.R: он тянет imf_tool.R и лишние library().

#' @return list(extdata_y, extdata_q, extdata_m, extdata_d, dict)
importData <- function(yqm_file, d_file, sheet_keys = c(y = "y", q = "q", m = "m"),
                       format = c("auto", "rds", "xlsx"), add_time = FALSE) {

  format <- rlang::arg_match(format)
  if (format == "auto") {
    ext <- tools::file_ext(yqm_file)
    format <- if (tolower(ext) == "rds") "rds" else "xlsx"
  }

  .read_dict_from_rds_exact <- function(bundle, name, origin_label) {
    if (is.null(bundle[[name]])) {
      cli::cli_warn("В {origin_label} не найден объект словаря с именем '{name}'. Продолжаю без него.")
      return(NULL)
    }
    bundle[[name]]
  }

  .read_dict_from_xlsx_exact <- function(path, sheet, origin_label) {
    if (!file.exists(path)) {
      cli::cli_warn("Файл {.path {path}} ({origin_label}) не найден для чтения словаря '{sheet}'. Продолжаю без него.")
      return(NULL)
    }
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
    if (!(sheet %in% sheets)) {
      cli::cli_warn("В {origin_label} не найден лист словаря '{sheet}'. Продолжаю без него.")
      return(NULL)
    }
    ncols <- ncol(readxl::read_excel(path, sheet = sheet, n_max = 0))
    if (is.na(ncols) || ncols == 0) {
      cli::cli_warn("Лист словаря '{sheet}' в {origin_label} пуст. Продолжаю без него.")
      return(NULL)
    }
    readxl::read_excel(
      path,
      sheet     = sheet,
      col_names = TRUE,
      col_types = rep("text", ncols)
    ) |>
      tibble::as_tibble()
  }

  .maybe_add_time <- function(out, add_time_flag) {
    if (!isTRUE(add_time_flag)) return(out)
    out$extdata_y <- .add_time_safe_y(out$extdata_y)
    out$extdata_q <- .add_time_safe_q(out$extdata_q)
    out$extdata_m <- .add_time_safe_m(out$extdata_m)
    out
  }

  .add_time_safe_y <- function(df) {
    if (is.null(df)) return(df)
    if (!all(c("year") %in% names(df))) {
      cli::cli_warn("Для Y не удалось добавить 'time': отсутствует колонка 'year'.")
      return(df)
    }
    dplyr::mutate(df, time = .data$year - 1987L)
  }

  .add_time_safe_q <- function(df) {
    if (is.null(df)) return(df)
    req <- c("year", "quarter")
    if (!all(req %in% names(df))) {
      cli::cli_warn("Для Q не удалось добавить 'time': нет колонок {setdiff(req, names(df))}.")
      return(df)
    }
    dplyr::mutate(df, time = (.data$year - 1987L) * 4L + .data$quarter)
  }

  .add_time_safe_m <- function(df) {
    if (is.null(df)) return(df)
    req <- c("year", "month")
    if (!all(req %in% names(df))) {
      cli::cli_warn("Для M не удалось добавить 'time': нет колонок {setdiff(req, names(df))}.")
      return(df)
    }
    dplyr::mutate(df, time = (.data$year - 1987L) * 12L + .data$month)
  }

  .bind_dicts <- function(d1, d2) {
    dicts <- purrr::compact(list(d1, d2))
    if (length(dicts) == 0) return(NULL)
    dplyr::bind_rows(dicts)
  }

  if (format == "rds") {
    yqm_bundle <- readRDS(yqm_file)
    d_bundle   <- readRDS(d_file)

    out <- purrr::map(
      sheet_keys,
      ~ yqm_bundle[[.x]]
    ) |>
      purrr::set_names(paste0("extdata_", names(sheet_keys)))

    out$extdata_d <- d_bundle[["d"]]

    dict_yqm <- .read_dict_from_rds_exact(yqm_bundle, "dict",   origin_label = "файле Y/Q/M (RDS)")
    dict_d   <- .read_dict_from_rds_exact(d_bundle,   "dict_d", origin_label = "файле D (RDS)")
    out$dict <- .bind_dicts(dict_yqm, dict_d)

    out <- .maybe_add_time(out, add_time)
    return(out)
  }

  if (format == "xlsx") {
    out <- purrr::map(
      sheet_keys,
      ~ readSeriesSheet(yqm_file, sheet = .x)
    ) |>
      purrr::set_names(paste0("extdata_", names(sheet_keys)))

    out$extdata_d <- readSeriesSheet(
      d_file,
      sheet       = "d",
      fixed_types = c("text", "text", "date")
    )

    dict_yqm <- .read_dict_from_xlsx_exact(yqm_file, sheet = "dict",   origin_label = "файле Y/Q/M (XLSX)")
    dict_d   <- .read_dict_from_xlsx_exact(d_file,   sheet = "dict_d", origin_label = "файле D (XLSX)")
    out$dict <- .bind_dicts(dict_yqm, dict_d)

    out <- .maybe_add_time(out, add_time)
    return(out)
  }

  rlang::abort("Unsupported format – choose 'rds', 'xlsx', or 'auto'.")
}


# ---------- graphplan constants (aligned with do_plot.R) --------------------

graphplan_columns <- c(
  "graph_name", "graph_title", "graph_type", "graph_group", "data_frequency",
  "indicators", "time_fix", "peers", "all", "x_log", "y_log", "x_min", "x_max",
  "y_min", "y_max", "trend_type", "index", "recession", "sec_y_axis", "swap_axis",
  "long_legend", "vert_lab", "short_names", "theme", "orientation", "show_title",
  "active"
)

graph_types <- c(
  "scatter_dynamic", "scatter_country_comparison", "scatter_before_after",
  "bar_dynamic", "bar_country_comparison", "bar_country_comparison_norm",
  "bar_year_comparison",
  "structure_dynamic", "structure_country_comparison", "structure_country_comparison_norm",
  "lines_indicator_comparison", "lines_country_comparison",
  "density_fix", "distribution_dynamic", "distribution_time_comparison",
  "distribution_indicator_comparison", "triangle"
)

trend_types   <- c("lm", "loess")
orient_types  <- c("horizontal", "vertical")
theme_types   <- c("ipsum", "acra_light", "acra_dark", "black_white", "economist", "minimal")
horizontal_size <- c(1800, 900)
vertical_size   <- c(850, 850)

blocking_check_cols <- c(
  "check_types", "check_freq", "check_unique", "check_availability", "check_peers",
  "check_times", "check_binary", "check_num", "check_trend", "check_theme", "check_orient"
)

graphplan_check_artifact_cols <- function() {
  unique(c("checks", blocking_check_cols))
}

strip_graphplan_check_artifacts <- function(plan) {
  plan <- tibble::as_tibble(plan)
  drop <- intersect(graphplan_check_artifact_cols(), names(plan))
  drop <- c(drop, names(plan)[stringr::str_starts(names(plan), "ok_")])
  drop <- unique(drop)
  if (length(drop) == 0L) {
    return(plan)
  }
  dplyr::select(plan, -dplyr::all_of(drop))
}

check_rule_messages <- list(
  check_types        = "Unknown graph type",
  check_freq         = "Unknown data frequency",
  check_unique       = "Duplicate graph name",
  check_availability = "Indicator not available in database",
  check_peers        = "Invalid peers specification",
  check_times        = "Invalid time bounds or time_fix",
  check_binary       = "Non-binary parameter value",
  check_num          = "Non-numeric y_min/y_max",
  check_trend        = "Unknown trend type",
  check_theme        = "Unknown theme",
  check_orient       = "Unknown orientation"
)


# ---------- validation --------------------------------------------------------

.row_check_messages <- function(row, check_cols = blocking_check_cols) {
  msgs <- character(0)
  for (col in check_cols) {
    if (!col %in% names(row)) next
    val <- row[[col]][1]
    if (!is.na(val) && val == 0L) {
      msgs <- c(msgs, check_rule_messages[[col]] %||% col)
    }
  }
  if (length(msgs) == 0) NA_character_ else paste(msgs, collapse = "; ")
}

.run_graphplan_checks <- function(graphplan, FD, country_info, peers_fname,
                                 graph_types, trend_types, theme_types, orient_types) {
  cols_ok  <- checkColumns(graphplan = graphplan, graphplan_columns = graphplan_columns) == 1L
  empty_ok <- checkEmpty(graphplan = graphplan) == 1L

  if (!cols_ok || !empty_ok) {
    plan <- tibble::as_tibble(graphplan)
    return(list(
      plan = plan,
      cols_ok = cols_ok,
      empty_ok = empty_ok
    ))
  }

  plan <- graphplan |>
    checkGraphTypes(graph_types = graph_types) |>
    checkFreq() |>
    checkUnique() |>
    checkAvailability(dict = FD$dict) |>
    checkPeers(peer_groups = country_info, dict = FD$dict) |>
    checkTimes(warn_invalid = TRUE) |>
    checkBinaryParams() |>
    checkNumericParams() |>
    checkTrend(trend_types = trend_types) |>
    checkTheme(theme_types = theme_types) |>
    checkOrientation(orient_types = orient_types) |>
    dplyr::mutate(
      checks = .data$check_types * .data$check_freq * .data$check_unique *
        .data$check_availability * .data$check_peers * .data$check_times *
        .data$check_binary * .data$check_num * .data$check_trend *
        .data$check_theme * .data$check_orient
    )

  list(plan = plan, cols_ok = TRUE, empty_ok = TRUE)
}

.graphplan_row_status_one <- function(plan, i, active_flags) {
  row <- plan[i, , drop = FALSE]
  is_active <- isTRUE(active_flags[i])
  gname <- as.character(row$graph_name[[1]] %||% paste0("row_", i))

  checks_val <- row$checks[[1]]
  checks_ok <- !is.na(checks_val) && isTRUE(as.integer(checks_val) == 1L)
  status <- if (checks_ok) "valid" else "error"
  tibble::tibble(
    row_id = i,
    graph_name = gname,
    active = row$active[[1]] %||% 0L,
    check_status = status,
    can_build = is_active && checks_ok,
    messages = if (checks_ok) NA_character_ else .row_check_messages(row)
  )
}

.graphplan_row_status_table <- function(plan, row_ids = NULL) {
  n_rows <- nrow(plan)
  active_flags <- active_flag_vec(plan)
  indices <- if (is.null(row_ids)) {
    seq_len(n_rows)
  } else {
    unique(as.integer(row_ids))
  }
  indices <- indices[!is.na(indices) & indices >= 1L & indices <= n_rows]
  if (length(indices) == 0L) {
    return(tibble::tibble(
      row_id = integer(), graph_name = character(), active = integer(),
      check_status = character(), can_build = logical(), messages = character()
    ))
  }
  purrr::map_dfr(indices, function(i) {
    .graphplan_row_status_one(plan, i, active_flags)
  })
}

#' Merge an editor row into a graphplan for validation (full-plan checks, incl. unique).
graphplan_with_editor_row <- function(graphplan,
                                      row,
                                      row_id = NULL,
                                      editor_mode = c("auto", "edit", "new")) {
  editor_mode <- rlang::arg_match(editor_mode)
  if (editor_mode == "auto") {
    editor_mode <- if (is.null(row_id)) "new" else "edit"
  }

  if (editor_mode == "edit") {
    if (is.null(row_id)) {
      rlang::abort("graphplan_with_editor_row: row_id is required for edit mode.")
    }
    if (is.null(graphplan) || nrow(graphplan) == 0) {
      rlang::abort("graphplan_with_editor_row: graphplan is empty; cannot edit a row.")
    }
  }

  if (is.null(graphplan) || nrow(graphplan) == 0) {
    plan <- append_graphplan_row(NULL, row)
    return(list(plan = plan, row_id = nrow(plan)))
  }

  if (editor_mode == "edit") {
    list(
      plan = update_graphplan_row(graphplan, row_id, row),
      row_id = row_id
    )
  } else {
    plan <- append_graphplan_row(graphplan, row)
    list(plan = plan, row_id = nrow(plan))
  }
}

validate_graphplan_for_app <- function(graphplan,
                                       FD,
                                       country_iso3c,
                                       peers_fname,
                                       row_ids = NULL) {
  if (is.null(graphplan) || nrow(graphplan) == 0) {
    empty_summary <- tibble::tibble(
      n_rows = 0L, n_active = 0L, n_buildable = 0L,
      n_errors = 0L, n_warnings = 0L, n_inactive = 0L
    )
    return(list(
      plan = tibble::tibble(),
      row_status = tibble::tibble(
        row_id = integer(), graph_name = character(), active = integer(),
        check_status = character(), can_build = logical(), messages = character()
      ),
      summary = empty_summary
    ))
  }

  graphplan <- migrate_graphplan_if_needed(graphplan)
  graphplan <- strip_graphplan_check_artifacts(graphplan)
  country_info <- get_peers_cached(country_iso3c = country_iso3c, peers_fname = peers_fname)

  chk <- .run_graphplan_checks(
    graphplan = graphplan,
    FD = FD,
    country_info = country_info,
    peers_fname = peers_fname,
    graph_types = graph_types,
    trend_types = trend_types,
    theme_types = theme_types,
    orient_types = orient_types
  )

  plan <- chk$plan
  n_rows <- nrow(plan)

  if (!isTRUE(chk$cols_ok) || !isTRUE(chk$empty_ok)) {
    global_msg <- if (!chk$cols_ok) {
      "Graphplan is missing required columns."
    } else {
      "Graphplan has no active rows."
    }
    row_status <- tibble::tibble(
      row_id = seq_len(n_rows),
      graph_name = as.character(plan$graph_name %||% paste0("row_", seq_len(n_rows))),
      active = if ("active" %in% names(plan)) plan$active else rep(1L, n_rows),
      check_status = "error",
      can_build = FALSE,
      messages = global_msg
    )
    n_active <- sum(active_flag_vec(plan))
    summary <- tibble::tibble(
      n_rows = n_rows,
      n_active = as.integer(n_active),
      n_buildable = 0L,
      n_errors = n_rows,
      n_warnings = 0L,
      n_inactive = as.integer(n_rows - n_active)
    )
    return(list(plan = plan, row_status = row_status, summary = summary))
  }

  row_status <- .graphplan_row_status_table(plan, row_ids = row_ids)
  active_flags <- active_flag_vec(plan)
  checks_ok <- !is.na(plan$checks) & as.integer(plan$checks) == 1L
  can_build_flags <- active_flags & checks_ok

  summary <- tibble::tibble(
    n_rows = n_rows,
    n_active = sum(active_flags),
    n_buildable = sum(can_build_flags),
    n_errors = sum(!checks_ok),
    n_warnings = 0L,
    n_inactive = sum(!active_flags)
  )

  list(plan = plan, row_status = row_status, summary = summary)
}


# ---------- build -------------------------------------------------------------

build_graph_row <- function(graphplan_row,
                            FD,
                            country_iso3c,
                            peers_fname,
                            verbose = FALSE) {
  tryCatch(
    {
      country_info <- get_peers_cached(country_iso3c = country_iso3c, peers_fname = peers_fname)

      graph_params <- parseGraphPlan(
        graphrow = graphplan_row,
        dict = FD$dict,
        horizontal_size = horizontal_size,
        vertical_size = vertical_size
      )
      peers_iso2c <- fixPeers(country_info = country_info, params = graph_params, data = FD)
      graph_params <- fillGraphPlan(
        parsedrow = graph_params,
        data = FD,
        country_iso2c = country_info$country_iso2c,
        peers_iso2c = peers_iso2c
      )
      data_temp <- subsetData(
        data = FD,
        graph_params = graph_params,
        country_code = country_info$country_iso2c,
        peers_code = peers_iso2c
      )

      func_name <- funcNameTransform(graph_type = graph_params$graph_type)
      if (is.na(func_name) || !nzchar(func_name)) {
        rlang::abort("Unknown or empty graph_type.")
      }
      plot_fun <- get(func_name, mode = "function")
      theplot <- do.call(
        plot_fun,
        list(
          data = data_temp,
          graph_params = graph_params,
          country_iso2c = country_info$country_iso2c,
          peers_iso2c = peers_iso2c,
          verbose = verbose
        )
      )

      list(
        ok = TRUE,
        graph = theplot$graph,
        data = theplot$data,
        graph_params = graph_params,
        graph_name = graph_params$graph_name,
        warnings = character(0),
        error = NA_character_
      )
    },
    error = function(e) {
      gname <- if ("graph_name" %in% names(graphplan_row)) {
        as.character(graphplan_row$graph_name[[1]])
      } else {
        "unknown"
      }
      list(
        ok = FALSE,
        graph = NULL,
        data = NULL,
        graph_params = NULL,
        graph_name = gname,
        warnings = character(0),
        error = conditionMessage(e)
      )
    }
  )
}

build_graphplan_rows <- function(graphplan,
                                 row_ids,
                                 FD,
                                 country_iso3c,
                                 peers_fname,
                                 validation = NULL,
                                 verbose = FALSE) {
  if (is.null(row_ids) || length(row_ids) == 0) {
    return(list())
  }

  if (is.null(validation)) {
    validation <- validate_graphplan_for_app(
      graphplan = graphplan,
      FD = FD,
      country_iso3c = country_iso3c,
      peers_fname = peers_fname
    )
  }

  can_build_ids <- validation$row_status |>
    dplyr::filter(.data$can_build, .data$row_id %in% row_ids) |>
    dplyr::pull(.data$row_id)

  results <- list()
  for (rid in can_build_ids) {
    row <- graphplan[rid, , drop = FALSE]
    built <- build_graph_row(
      graphplan_row = row,
      FD = FD,
      country_iso3c = country_iso3c,
      peers_fname = peers_fname,
      verbose = verbose
    )
    key <- built$graph_name %||% paste0("row_", rid)
    results[[key]] <- c(built, list(row_id = rid, status = if (built$ok) "ok" else "error"))
  }

  results
}


# ---------- graphplan CRUD ----------------------------------------------------

generateSources <- function(indicators, dict) {
  codes <- na.omit(dict$indicator_code)
  if (length(codes) == 0) return(NA_character_)
  x <- unlist(stringr::str_extract_all(
    string  = indicators,
    pattern = paste(codes, collapse = "|")
  ))
  x <- dict$source_name[match(x, dict$indicator_code)]
  x <- unlist(stringr::str_split(x, ", "))
  x <- x[!is.na(x) & x != "расчеты АКРА"]
  x <- c(unique(x), "расчеты АКРА")
  toString(x)
}

migrate_graphplan_if_needed <- function(plan, expected_columns = NULL) {
  if (is.null(expected_columns)) expected_columns <- graphplan_columns
  plan <- tibble::as_tibble(plan)
  missing <- setdiff(expected_columns, names(plan))
  if (length(missing) > 0) {
    for (col in missing) {
      plan[[col]] <- NA
    }
    rlang::inform(paste("migrate_graphplan_if_needed: added columns:", paste(missing, collapse = ", ")))
  }
  extra <- setdiff(names(plan), c(expected_columns, "source_name", "checks", blocking_check_cols))
  plan
}

read_graphplan_file <- function(path, dict) {
  plan <- getPlotSchedule(plotparam_fname = path, dict = dict)
  info <- NULL
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  if ("info" %in% sheets) {
    info <- tryCatch(
      readxl::read_excel(path, sheet = "info"),
      error = function(e) NULL
    )
  }
  list(plan = migrate_graphplan_if_needed(plan), info = info)
}

empty_graphplan <- function() {
  cols <- c(graphplan_columns, "source_name")
  tibble::as_tibble(setNames(
    rep(list(character()), length(cols)),
    cols
  ))[0, ]
}

graphplan_int_columns <- c(
  "all", "x_log", "y_log", "index", "recession", "swap_axis",
  "long_legend", "vert_lab", "short_names", "show_title", "active"
)

graphplan_num_columns <- c("y_min", "y_max")

graphplan_chr_columns <- setdiff(
  c(graphplan_columns, "source_name"),
  c(graphplan_int_columns, graphplan_num_columns)
)

coerce_graphplan_value <- function(col, value) {
  if (col %in% graphplan_int_columns) {
    if (length(value) == 0L || (length(value) == 1L && is.na(value))) {
      return(NA_integer_)
    }
    if (is.logical(value)) return(as.integer(value))
    if (is.numeric(value)) return(as.integer(value))
    if (is.character(value) && !nzchar(value)) return(NA_integer_)
    return(as.integer(value != 0))
  }
  if (col %in% graphplan_num_columns) {
    if (length(value) == 0L || (length(value) == 1L && is.na(value))) {
      return(NA_real_)
    }
    if (is.character(value) && !nzchar(value)) return(NA_real_)
    return(suppressWarnings(as.numeric(value)))
  }
  if (length(value) == 0L || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  out <- as.character(value)
  if (!nzchar(out)) NA_character_ else out
}

align_graphplan_column <- function(x, col) {
  if (col %in% graphplan_chr_columns) {
    out <- as.character(x)
    out[!is.na(out) & out == ""] <- NA_character_
    return(out)
  }
  if (col %in% graphplan_num_columns) {
    return(suppressWarnings(as.numeric(x)))
  }
  if (col %in% graphplan_int_columns) {
    return(vapply(x, coerce_graphplan_value, integer(1), col = col))
  }
  x
}

align_graphplan_types <- function(plan) {
  plan <- tibble::as_tibble(plan)
  for (col in intersect(names(plan), c(graphplan_columns, "source_name"))) {
    plan[[col]] <- align_graphplan_column(plan[[col]], col)
  }
  plan
}

coerce_graphplan_row <- function(row) {
  row <- tibble::as_tibble(row[1, , drop = FALSE])
  cols <- intersect(names(row), c(graphplan_columns, "source_name"))
  for (col in cols) {
    row[[col]] <- coerce_graphplan_value(col, row[[col]][[1]])
  }
  row
}

.assign_graphplan_cell <- function(column, value, col) {
  coerced <- coerce_graphplan_value(col, value)
  if (length(column) == 0L) {
    return(coerced)
  }
  if (col %in% graphplan_int_columns) {
    return(as.integer(coerced))
  }
  if (col %in% graphplan_num_columns) {
    return(as.numeric(coerced))
  }
  as.character(coerced)
}

resolve_editor_data_frequency <- function(freq, indicators, dict) {
  if (!is.null(freq) && !identical(freq, " ") && nzchar(as.character(freq))) {
    return(as.character(freq))
  }
  if (is.null(dict) || !"indicator_code" %in% names(dict)) {
    return(NA_character_)
  }
  codes <- unlist(strsplit(paste(indicators, collapse = ","), ",\\s*"))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0L) {
    return(NA_character_)
  }
  freqs <- dict$source_frequency[match(codes, dict$indicator_code)]
  freqs <- freqs[!is.na(freqs) & nzchar(freqs)]
  if (length(freqs) == 0L) {
    return(NA_character_)
  }
  as.character(freqs[1])
}

graphplan_row_from_inputs <- function(input_row, dict,
                                      graph_groups = NULL,
                                      generate_sources = TRUE) {
  row <- tibble::as_tibble(input_row)
  needed <- graphplan_columns
  for (col in needed) {
    if (!col %in% names(row)) row[[col]] <- NA
  }
  if (generate_sources && "indicators" %in% names(row) && !is.null(dict)) {
    ind <- as.character(row$indicators[[1]] %||% "")
    if (nzchar(ind)) {
      row$source_name <- generateSources(indicators = ind, dict = dict)
    }
  }
  row <- row |>
    dplyr::select(dplyr::any_of(c(needed, "source_name"))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ dplyr::if_else(.x == "", NA, .x)))
  row
}

update_graphplan_row <- function(plan, row_id, new_row) {
  plan <- tibble::as_tibble(plan)
  if (row_id < 1L || row_id > nrow(plan)) {
    rlang::abort(glue("update_graphplan_row: invalid row_id {row_id}."))
  }
  new_row <- graphplan_row_from_inputs(new_row, dict = NULL, generate_sources = FALSE)
  new_row <- coerce_graphplan_row(new_row)
  cols <- intersect(names(plan), names(new_row))
  for (col in cols) {
    plan[[col]][row_id] <- .assign_graphplan_cell(
      plan[[col]],
      new_row[[col]][[1]],
      col
    )
  }
  plan
}

append_graphplan_row <- function(plan, new_row) {
  plan <- if (is.null(plan) || nrow(plan) == 0) {
    align_graphplan_types(empty_graphplan())
  } else {
    align_graphplan_types(plan)
  }
  new_row <- graphplan_row_from_inputs(new_row, dict = NULL, generate_sources = FALSE)
  new_row <- coerce_graphplan_row(new_row)
  all_cols <- union(names(plan), names(new_row))
  for (col in setdiff(all_cols, names(plan))) plan[[col]] <- NA
  for (col in setdiff(all_cols, names(new_row))) new_row[[col]] <- NA
  dplyr::bind_rows(plan, new_row[, all_cols, drop = FALSE])
}

soft_delete_row <- function(plan, row_id) {
  plan <- tibble::as_tibble(plan)
  if (row_id < 1L || row_id > nrow(plan)) {
    rlang::abort(glue("soft_delete_row: invalid row_id {row_id}."))
  }
  if ("active" %in% names(plan)) {
    plan$active[row_id] <- 0L
  }
  plan
}

graphplan_row_to_tsv <- function(row) {
  out <- row |>
    dplyr::select(-dplyr::any_of("source_name"))
  paste(capture.output(utils::write.table(
    out, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t", na = ""
  )), collapse = "\n")
}

parse_graphplan_row_tsv <- function(text) {
  df <- utils::read.table(
    text = text,
    sep = "\t",
    na.strings = c("", "NA"),
    header = FALSE,
    stringsAsFactors = FALSE
  )
  if (ncol(df) < length(graphplan_columns)) {
    rlang::abort("TSV row has fewer columns than expected for graphplan.")
  }
  names(df) <- graphplan_columns[seq_len(ncol(df))]
  tibble::as_tibble(df)
}


# ---------- editor helpers ----------------------------------------------------

graph_groups <- c(
  macro = "ec", budget = "budg", external = "ext", institutional = "inst",
  demography = "demogr", covid = "covid", model = "model", other = "oth"
)

peers_presets <- c(
  "default", "neighbours", "EU", "EZ", "EEU", "IT", "OPEC_plus", "BRICS",
  "BRICS_plus", "EM", "DM", "ACRA"
)

peers_choice <- c("none", "default", "custom", "neighbours", "formula", peers_presets)

trend_types_ui <- c("", trend_types)

# Indicator group presets (legacy graph_plotter_app/app.R)
indicator_groups <- c(
  "", "GDP growth decomposition (expenses)", "GDP growth decomposition (prod. function)",
  "World shares", "BOP (Y)", "BOP (Q)",
  "Trade balance (Y)", "Trade balance (Q)", "IIP assets (Y)", "IIP liabilities (Y)",
  "Exchange rates",
  "Budget revenue definitions (Y)", "Budget revenue structure (Y)",
  "Budget expenditure definitions (Y)", "Budget expense structure (Y)",
  "Budget balance definitions (Y)", "Budget debt definitions (Y)",
  "WGI components", "Population drivers", "Model indicative scores",
  "Macro scores", "Public finance scores", "External scores", "Institutional scores",
  "Macroprudential measures (Y)", "Banks: capital (Q/Y)", "Banks: asset quality (Q/Y)",
  "Banks: earnings (Q/Y)", "Banks: liquidity (Q/Y)", "Banks: FC (Q/Y)",
  "Global prices"
)

indicator_groups_content <- list(
  "",
  c("cons_role", "govcons_role", "gcfc_role", "netex_role", "other_role"),
  c("labor_quant_contrib_cb", "labor_qual_contrib_cb", "cap_contrib_cb", "tfp_growth_cb"),
  c("pop_shr", "gdp_ppp_shr", "ex_gs_shr"),
  c("ca_gdp", "bop_finacc_nrmns_gdp", "bop_res_mns_gdp", "bop_capacc_gdp", "bop_err_gdp"),
  c("ca_gdp_sm", "bop_finacc_nrmns_gdp_sm", "bop_res_mns_gdp_sm", "bop_capacc_gdp_sm", "bop_err_gdp_sm"),
  c("tb_g_gdp", "tb_s_gdp", "primainc_gdp", "secinc_gdp"),
  c("tb_g_gdp_sm", "tb_s_gdp_sm", "primainc_gdp_sm", "secinc_gdp_sm"),
  c("iip_a_di_gdp", "iip_a_pi_gdp", "iip_a_der_gdp", "iip_a_oth_gdp", "iip_a_res_gdp"),
  c("iip_l_di_gdp", "iip_l_pi_gdp", "iip_l_der_gdp", "iip_l_oth_gdp"),
  c("usdlc_av", "neer_av", "reer_av"),
  c("gg_rev_gdp_fm", "gg_rev_gdp_weo", "gg_rev_gdp_gfs", "gg_rev_gdp_gmd"),
  c("gg_rev_oth_gdp", "gg_rev_grants_gdp", "gg_rev_soc_gdp", "gg_taxes_gdp"),
  c("gg_exnd_gdp_fm", "gg_exnd_gdp_weo", "gg_exnd_gdp_gfs", "gg_exnd_gdp_gmd"),
  c("gg_exns_oth_gdp", "gg_exns_transf_gdp", "gg_exns_sub_gdp", "gg_exns_int_gdp", "gg_exns_usegs_gdp", "gg_exns_wages_gdp"),
  c("gg_bal_gdp_fm", "gg_bal_gdp_weo", "gg_bal_gdp_gfs", "gg_bal_gdp_gmd"),
  c("gg_debt_gdp_fm", "gg_debt_gdp_weo", "gg_debt_gdp_gfs", "gg_debt_gdp_gmd"),
  c("wgi_va_rnk", "wgi_ps_rnk", "wgi_cc_rnk", "wgi_rl_rnk", "wgi_rq_rnk", "wgi_ge_rnk"),
  c("birth_rate", "death_rate", "migr_rate"),
  c("ind_rat_r_score", "m_r_score", "p_r_score", "e_r_score", "i_r_score"),
  c("m1r_wealth", "m2r_growth", "m3r_size", "m4r_inflation"),
  c("p1r_gg_bal", "p2r_debt_burden", "p3r_extdebt_gg_gdp"),
  c("e1r_ca_gdp", "e2r_intres_cover", "e3r_niip", "e4r_ex_div", "e5r_cur_vol"),
  c("i1r_polstab", "i2r_ecgov", "i3r_hci"),
  c("ccb", "consb", "cap", "lvr", "llp", "lcg", "loanr", "lfc", "ltv", "dsti", "tax", "liq", "ltd", "lfx", "rr", "sifi", "ot"),
  c("regcap_to_rwa", "t_one_cap_to_rwa", "cet_one_to_rwa", "t_one_cap_to_ass"),
  c("npl_to_loans", "top_thr_sect_to_loans", "net_npl_to_cap", "prov_to_npl", "bank_conc_loans"),
  c("bank_roa", "bank_roe", "nim_to_income", "nonint_exns_to_income"),
  c("liquid_to_assets", "liquid_to_sr_liab", "lcr", "nsfr"),
  c("bank_open_position", "bank_dom_loans", "fc_loans_role", "fc_liab_role"),
  c("p_com", "p_oil", "p_metals", "p_agro")
)

build_indicator_catalog_from_dict <- function(dict) {
  name_col <- c("indicator", "indicator_name", "name", "indicator_label", "label_ru", "label_en")
  name_col <- name_col[name_col %in% names(dict)] |>
    purrr::pluck(1, .default = NA_character_)

  dict |>
    dplyr::filter(!is.na(.data$indicator_code), .data$indicator_code != "") |>
    dplyr::filter(!is.na(.data$source_frequency), .data$source_frequency != "") |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col)) .data[[name_col]] else NA_character_,
      node_id = glue::glue("{indicator_code}@{source_frequency}"),
      label = dplyr::case_when(
        !is.na(.data$indicator_name) & .data$indicator_name != "" ~
          glue::glue("{node_id} — {indicator_name}"),
        TRUE ~ node_id
      )
    ) |>
    dplyr::distinct(.data$indicator_code, .data$source_frequency, .keep_all = TRUE) |>
    dplyr::arrange(.data$indicator_code, .data$source_frequency) |>
    dplyr::select(
      node_id, label,
      indicator_code, source_frequency,
      indicator_name,
      dplyr::any_of(c("theme", "source_name"))
    )
}

editor_indicator_choices <- function(indicator_catalog,
                                   frequency = " ",
                                   ind_group = "",
                                   groups = indicator_groups,
                                   groups_content = indicator_groups_content) {
  inds <- indicator_catalog
  if (!is.null(frequency) && frequency != " ") {
    inds <- inds |> dplyr::filter(.data$source_frequency == frequency)
  }
  choices <- stats::setNames(inds$indicator_code, inds$label)
  selected <- NULL
  if (nzchar(ind_group %||% "")) {
    idx <- match(ind_group, groups)
    if (!is.na(idx)) selected <- groups_content[[idx]]
  }
  list(choices = choices, selected = selected)
}

sec_y_choices_from_indicators <- function(indicator_catalog, indicator_codes) {
  codes <- indicator_codes[!is.na(indicator_codes) & indicator_codes != ""]
  if (length(codes) == 0) return(stats::setNames(character(), character()))
  inds <- indicator_catalog |> dplyr::filter(.data$indicator_code %in% codes)
  stats::setNames(inds$indicator_code, inds$label)
}

strip_graph_name_suffix <- function(graph_name, groups_map = NULL) {
  if (is.null(groups_map)) groups_map <- graph_groups
  gname <- as.character(graph_name %||% "")
  shorts <- unlist(groups_map)
  if (!length(shorts)) return(gname)
  pattern <- paste0("^(", paste(shorts, collapse = "|"), ")_")
  sub(pattern, "", gname)
}

graph_group_long_to_short <- function(graph_group_long, groups_map = NULL) {
  if (is.null(groups_map)) groups_map <- graph_groups
  g <- as.character(graph_group_long %||% "")
  if (g %in% names(groups_map)) return(unname(groups_map[[g]]))
  if (g %in% unlist(groups_map)) return(g)
  unname(groups_map[[1]])
}

#' Resolve peer group / formula to concrete ISO2 peer codes for the Editor UI.
#'
#' Mirrors legacy `graph_plotter_app`: after choosing a preset or formula, the
#' multiselect shows the expanded peer list from [fixPeers]. Returns `NULL` when
#' the custom field should not be overwritten (`custom` mode or missing country).
#'
#' @param country_iso3c Single ISO3 code (e.g. from `rv$country_iso3c`).
#' @param peers Editor `ed_peers` value: `none`, `custom`, `formula`, or a preset name.
#' @param peers_formula Used when `peers == "formula"`.
#' @param graph_type Editor graph type (needed for `distribution_dynamic`).
#' @param peers_fname Path to `1_peers_params.xlsx`.
#' @param data Loaded FD object (`importData` result).
expand_editor_peer_selection_to_iso2c <- function(
    country_iso3c,
    peers,
    peers_formula,
    graph_type,
    peers_fname,
    data) {
  if (is.null(peers)) peers <- "none"
  peers <- as.character(peers)[1]
  if (identical(peers, "custom")) {
    return(NULL)
  }
  if (identical(peers, "none")) {
    return(character(0))
  }
  iso3 <- country_iso3c[[1]] %||% country_iso3c
  iso3 <- as.character(iso3)[1]
  if (is.null(iso3) || !nzchar(iso3)) {
    return(NULL)
  }
  ps <- if (identical(peers, "formula")) {
    f <- as.character(peers_formula %||% "")[1]
    if (!nzchar(f)) {
      return(NULL)
    }
    f
  } else {
    pr <- peers_string_from_editor(peers, NULL, "", preset_list = peers_presets)
    if (identical(pr, 0)) {
      return(NULL)
    }
    as.character(pr)[1]
  }
  country_info <- get_peers_cached(iso3, peers_fname)
  params <- list(
    peers = ps,
    graph_type = graph_type %||% NA_character_
  )
  fixPeers(
    country_info = country_info,
    params = params,
    data = data,
    warn_invalid = FALSE
  )
}

peers_string_from_editor <- function(peers, peers_custom, peers_formula, preset_list = NULL) {
  if (is.null(preset_list)) preset_list <- peers_presets
  if (is.null(peers) || peers == "none") return(0)
  if (peers %in% preset_list) return(peers)
  if (peers == "custom") {
    cc <- peers_custom[!is.na(peers_custom) & peers_custom != ""]
    if (!length(cc)) return(0)
    return(paste0("custom: ", paste(cc, collapse = ", ")))
  }
  if (peers == "formula") {
    f <- as.character(peers_formula %||% "")
    if (!nzchar(f)) return(0)
    return(f)
  }
  0
}

sec_y_axis_string_from_editor <- function(sec_y_ind, sec_y_coeff) {
  ind_part <- if (length(sec_y_ind) && !all(sec_y_ind == "")) {
    paste(sec_y_ind, collapse = ", ")
  } else {
    NA_character_
  }
  coeff <- sec_y_coeff
  if (!is.null(coeff) && !is.na(coeff) && nzchar(as.character(coeff))) {
    paste(c(ind_part, as.character(coeff)), collapse = ", ")
  } else {
    ind_part
  }
}

parse_peers_for_editor <- function(peers_value, preset_list = NULL) {
  if (is.null(preset_list)) preset_list <- peers_presets
  out <- list(peers = "none", peers_custom = character(), peers_formula = "")
  pv <- peers_value[[1]] %||% peers_value
  if (is.null(pv) || is.na(pv) || pv == 0 || pv == "0") return(out)
  pv <- as.character(pv)
  if (pv %in% preset_list) {
    out$peers <- pv
    return(out)
  }
  if (grepl("^custom\\s*:", pv, ignore.case = TRUE)) {
    out$peers <- "custom"
    rest <- sub("^custom\\s*:\\s*", "", pv)
    out$peers_custom <- stringr::str_trim(unlist(strsplit(rest, ",\\s*")))
    return(out)
  }
  out$peers <- "formula"
  out$peers_formula <- pv
  out
}

parse_sec_y_axis_for_editor <- function(sec_y_value) {
  out <- list(sec_y_ind = character(), sec_y_coeff = NA_real_)
  if (is.null(sec_y_value) || is.na(sec_y_value) || !nzchar(as.character(sec_y_value))) return(out)
  parts <- unlist(strsplit(as.character(sec_y_value), ",\\s*"))
  nums <- grepl("^\\d+(\\.\\d+)?$", parts)
  out$sec_y_ind <- parts[!nums]
  if (any(nums)) out$sec_y_coeff <- as.numeric(parts[nums][1])
  out
}

editor_inputs_to_graphplan_row <- function(state, dict, groups_map = NULL) {
  if (is.null(groups_map)) groups_map <- graph_groups
  group_short <- state$graph_group_short %||% "ec"
  group_long <- names(groups_map)[match(group_short, groups_map)]
  if (length(group_long) == 0 || is.na(group_long)) group_long <- "macro"

  suffix <- as.character(state$graph_name_suffix %||% "graph")
  graph_name <- glue::glue("{group_short}_{suffix}")

  indicators <- state$indicators
  if (length(indicators) > 1) {
    indicators <- paste(indicators, collapse = ", ")
  }

  indicators_chr <- indicators
  row <- tibble::tibble(
    graph_name       = graph_name,
    graph_title      = state$graph_title %||% NA_character_,
    graph_type       = state$graph_type %||% NA_character_,
    graph_group      = group_long,
    data_frequency   = resolve_editor_data_frequency(
      state$data_frequency,
      indicators_chr,
      dict
    ),
    indicators       = indicators,
    time_fix         = state$time_fix %||% NA_character_,
    peers            = peers_string_from_editor(
      state$peers, state$peers_custom, state$peers_formula
    ),
    all              = as.integer(isTRUE(state$all)),
    x_log            = as.integer(isTRUE(state$x_log)),
    y_log            = as.integer(isTRUE(state$y_log)),
    x_min            = state$x_min %||% NA_character_,
    x_max            = state$x_max %||% NA_character_,
    y_min            = state$y_min %||% NA_real_,
    y_max            = state$y_max %||% NA_real_,
    trend_type       = state$trend_type %||% NA_character_,
    index            = as.integer(isTRUE(state$index)),
    recession        = as.integer(isTRUE(state$recession)),
    sec_y_axis       = sec_y_axis_string_from_editor(state$sec_y_ind, state$sec_y_coeff),
    swap_axis        = as.integer(isTRUE(state$swap_axis)),
    long_legend      = as.integer(isTRUE(state$long_legend)),
    vert_lab         = as.integer(isTRUE(state$vert_lab)),
    short_names      = as.integer(isTRUE(state$short_names)),
    theme            = state$theme %||% "ipsum",
    orientation      = state$orientation %||% "horizontal",
    show_title       = as.integer(isTRUE(state$show_title)),
    active           = as.integer(isTRUE(state$active %||% TRUE))
  )

  graphplan_row_from_inputs(row, dict = dict, generate_sources = TRUE)
}

graphplan_row_to_editor_state <- function(row, groups_map = NULL) {
  if (is.null(groups_map)) groups_map <- graph_groups
  row <- migrate_graphplan_if_needed(tibble::as_tibble(row[1, , drop = FALSE]))
  peers_p <- parse_peers_for_editor(row$peers)
  secy <- parse_sec_y_axis_for_editor(row$sec_y_axis[[1]])

  ind <- as.character(row$indicators[[1]] %||% "")
  ind_vec <- if (nzchar(ind)) stringr::str_trim(unlist(strsplit(ind, ",\\s*"))) else character()

  list(
    graph_name_suffix = strip_graph_name_suffix(row$graph_name[[1]], groups_map),
    graph_title       = as.character(row$graph_title[[1]] %||% ""),
    graph_type        = as.character(row$graph_type[[1]] %||% graph_types[1]),
    graph_group_short = graph_group_long_to_short(row$graph_group[[1]], groups_map),
    data_frequency    = as.character(row$data_frequency[[1]] %||% " "),
    indicators        = ind_vec,
    time_fix          = as.character(row$time_fix[[1]] %||% ""),
    peers             = peers_p$peers,
    peers_custom      = peers_p$peers_custom,
    peers_formula     = peers_p$peers_formula,
    all               = isTRUE(as.integer(row$all[[1]]) == 1L),
    x_log             = isTRUE(as.integer(row$x_log[[1]]) == 1L),
    y_log             = isTRUE(as.integer(row$y_log[[1]]) == 1L),
    x_min             = as.character(row$x_min[[1]] %||% ""),
    x_max             = as.character(row$x_max[[1]] %||% ""),
    y_min             = suppressWarnings(as.numeric(row$y_min[[1]])),
    y_max             = suppressWarnings(as.numeric(row$y_max[[1]])),
    trend_type        = as.character(row$trend_type[[1]] %||% ""),
    index             = isTRUE(as.integer(row$index[[1]]) == 1L),
    recession         = isTRUE(as.integer(row$recession[[1]]) == 1L),
    sec_y_ind         = secy$sec_y_ind,
    sec_y_coeff       = secy$sec_y_coeff,
    swap_axis         = isTRUE(as.integer(row$swap_axis[[1]]) == 1L),
    long_legend       = isTRUE(as.integer(row$long_legend[[1]]) == 1L),
    vert_lab          = isTRUE(as.integer(row$vert_lab[[1]]) == 1L),
    short_names       = isTRUE(as.integer(row$short_names[[1]]) == 1L),
    theme             = as.character(row$theme[[1]] %||% "ipsum"),
    orientation       = as.character(row$orientation[[1]] %||% "horizontal"),
    show_title        = isTRUE(as.integer(row$show_title[[1]]) == 1L),
    active            = isTRUE(as.integer(row$active[[1]] %||% 1L) == 1L)
  )
}

apply_editor_fill_defaults <- function(state) {
  gt <- state$graph_type
  freq <- state$data_frequency
  if (gt %in% c(
    "scatter_country_comparison", "structure_country_comparison",
    "structure_country_comparison_norm", "bar_country_comparison",
    "bar_country_comparison_norm"
  ) && !nzchar(state$time_fix %||% "")) {
    state$time_fix <- dplyr::case_when(
      freq == "y" ~ "2023",
      freq == "q" ~ "2024q4",
      freq == "m" ~ "2025m6",
      TRUE ~ "2023"
    )
  }
  if (gt %in% c(
    "scatter_dynamic", "structure_dynamic", "bar_dynamic",
    "lines_country_comparison", "lines_indicator_comparison", "distribution_dynamic"
  )) {
    if (!nzchar(state$x_min %||% "")) {
      state$x_min <- dplyr::case_when(
        freq == "y" ~ "2005", freq == "q" ~ "2018q1", freq == "m" ~ "2020m1", TRUE ~ "2005"
      )
    }
    if (!nzchar(state$x_max %||% "")) {
      state$x_max <- dplyr::case_when(
        freq == "y" ~ "2023", freq == "q" ~ "2024q4", freq == "m" ~ "2025m7", TRUE ~ "2023"
      )
    }
  }
  if (gt == "bar_year_comparison") {
    state$time_fix <- "2005, 2014, 2023"
  }
  state
}

validate_graphplan_row <- function(row,
                                   FD,
                                   country_iso3c,
                                   peers_fname,
                                   graphplan = NULL,
                                   row_id = NULL,
                                   editor_mode = c("auto", "edit", "new")) {
  if (is.null(graphplan)) {
    merged <- list(plan = tibble::as_tibble(row), row_id = 1L)
  } else {
    merged <- graphplan_with_editor_row(
      graphplan = graphplan,
      row = row,
      row_id = row_id,
      editor_mode = editor_mode
    )
  }

  v <- validate_graphplan_for_app(
    graphplan     = merged$plan,
    FD            = FD,
    country_iso3c = country_iso3c,
    peers_fname   = peers_fname,
    row_ids       = merged$row_id
  )
  if (nrow(v$row_status) == 0) {
    return(NULL)
  }
  v$row_status[v$row_status$row_id == merged$row_id, , drop = FALSE]
}

editor_row_fingerprint <- function(row) {
  row <- strip_graphplan_check_artifacts(tibble::as_tibble(row))
  jsonlite::toJSON(
    as.list(row),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

editor_validation_cache_key <- function(row,
                                        country_iso3c,
                                        graphplan,
                                        row_id = NULL,
                                        editor_mode = c("auto", "edit", "new"),
                                        plan_validation_revision = 0L) {
  editor_mode <- rlang::arg_match(editor_mode)
  if (editor_mode == "auto") {
    editor_mode <- if (is.null(row_id)) "new" else "edit"
  }
  paste(
    country_iso3c %||% "",
    editor_mode,
    if (is.null(row_id)) "new" else as.character(row_id),
    if (is.null(graphplan)) 0L else nrow(graphplan),
    as.integer(plan_validation_revision %||% 0L),
    editor_row_fingerprint(row),
    sep = "\x1e"
  )
}

refresh_gallery_built_for_row <- function(built_list,
                                          row_id,
                                          graphplan,
                                          FD,
                                          country_iso3c,
                                          peers_fname,
                                          thumb_cache_dir = NULL) {
  built_list <- remove_built_list_row(as.list(built_list %||% list()), row_id)
  if (is.null(country_iso3c) || is.null(row_id) || row_id < 1L || row_id > nrow(graphplan)) {
    return(list(built_list = built_list, editor_preview = NULL, error = NA_character_))
  }
  saved_row <- graphplan[row_id, , drop = FALSE]
  if ("active" %in% names(saved_row) && !isTRUE(active_flag_vec(saved_row)[1])) {
    return(list(built_list = built_list, editor_preview = NULL, error = NA_character_))
  }
  built <- build_graph_row(
    graphplan_row = saved_row,
    FD = FD,
    country_iso3c = country_iso3c,
    peers_fname = peers_fname
  )
  if (isTRUE(built$ok)) {
    item <- c(built, list(row_id = row_id, status = "ok"))
    if (!is.null(thumb_cache_dir) && nzchar(thumb_cache_dir)) {
      dir.create(thumb_cache_dir, recursive = TRUE, showWarnings = FALSE)
      item <- enrich_gallery_built_item(item, thumb_cache_dir)
    }
    built_list[[built$graph_name]] <- item
    return(list(built_list = built_list, editor_preview = built, error = NA_character_))
  }
  err_name <- as.character(saved_row$graph_name[[1]] %||% paste0("row_", row_id))
  built_list[[err_name]] <- list(
    ok = FALSE,
    graph = NULL,
    data = NULL,
    graph_params = NULL,
    graph_name = err_name,
    error = built$error,
    row_id = row_id,
    status = "error"
  )
  list(
    built_list = built_list,
    editor_preview = NULL,
    error = built$error %||% "build failed"
  )
}

filter_built_graphs <- function(built, scope = c("all_built", "valid_only", "gallery_selected"),
                                validation = NULL, selected_names = character()) {
  scope <- match.arg(scope)
  if (length(built) == 0) return(list())
  if (scope == "all_built") return(built)
  if (scope == "gallery_selected") {
    return(built[names(built) %in% selected_names])
  }
  if (scope == "valid_only") {
    ok_names <- names(built)[vapply(built, function(x) isTRUE(x$ok), logical(1))]
    return(built[names(built) %in% ok_names])
  }
  built
}


# ---------- export -------------------------------------------------------------

.graphplan_xlsx_row <- function(values, col_names) {
  vals <- as.list(values)
  if (is.null(names(vals))) names(vals) <- col_names
  as.data.frame(vals, stringsAsFactors = FALSE)
}

export_graphplan_xlsx <- function(plan,
                                  path,
                                  info = NULL,
                                  title_row = NULL) {
  # Match on-disk graphlib schema: library sheet has graphplan_columns only.
  # source_name is derived on read by getPlotSchedule().
  plan_out <- plan |>
    dplyr::select(dplyr::any_of(graphplan_columns))

  col_names <- names(plan_out)
  if (is.null(title_row)) {
    title_row <- stats::setNames(rep(list(NA_character_), length(col_names)), col_names)
  } else if (!is.data.frame(title_row)) {
    title_row <- .graphplan_xlsx_row(title_row, col_names)
  }
  header_row <- .graphplan_xlsx_row(col_names, col_names)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "library")
  openxlsx::writeData(wb, "library", title_row, startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "library", header_row, startRow = 2, colNames = FALSE)
  openxlsx::writeData(wb, "library", plan_out, startRow = 3, colNames = FALSE)

  if (!is.null(info)) {
    openxlsx::addWorksheet(wb, "info")
    openxlsx::writeData(wb, "info", info)
  }

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

export_built_graphs <- function(built,
                                dest_dir = tempdir(),
                                device = "png",
                                dpi = 150L) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)
  for (nm in names(built)) {
    item <- built[[nm]]
    if (!isTRUE(item$ok) || is.null(item$graph)) next
    gp <- item$graph_params
    if (is.null(gp)) next
    filename <- paste0(gp$graph_name, ".", device)
    fpath <- file.path(dest_dir, filename)
    if (identical(device, "png")) {
      write_export_png(item$graph, gp, fpath, dpi = dpi)
    } else {
      ggplot2::ggsave(
        filename = fpath,
        plot = item$graph,
        device = device,
        width = gp$width,
        height = gp$height,
        units = "px",
        dpi = dpi
      )
    }
    paths <- c(paths, fpath)
  }
  paths
}

export_built_graphs_zip <- function(built,
                                    zip_path = tempfile(fileext = ".zip"),
                                    device = "png",
                                    dpi = 150L) {
  tmp <- tempfile("graph_export_")
  dir.create(tmp)
  paths <- export_built_graphs(built, dest_dir = tmp, device = device, dpi = dpi)
  if (length(paths) == 0) {
    rlang::warn("export_built_graphs_zip: no graphs to export.")
    return(zip_path)
  }
  utils::zip(zipfile = zip_path, files = paths, flags = "-j")
  invisible(zip_path)
}

export_graph_data_xlsx <- function(built, path) {
  sheets <- list()
  for (nm in names(built)) {
    item <- built[[nm]]
    if (isTRUE(item$ok) && !is.null(item$data)) {
      sheets[[nm]] <- item$data
    }
  }
  if (length(sheets) == 0) {
    rlang::warn("export_graph_data_xlsx: no data to export.")
    return(invisible(path))
  }
  writexl::write_xlsx(sheets, path = path)
  invisible(path)
}

default_graphplan_info <- function() {
  tibble::tibble(
    field = c(
      "graphplan_version",
      "exported_from",
      "notes"
    ),
    value = c(
      "1",
      "new_graph_plotter_app",
      "info sheet is reference only; library sheet is the source of truth"
    )
  )
}

export_graphplan_recipes_text <- function(plan, include_inactive = TRUE) {
  if (is.null(plan) || nrow(plan) == 0) {
    return("")
  }
  plan <- strip_graphplan_check_artifacts(migrate_graphplan_if_needed(plan))
  if (!include_inactive && "active" %in% names(plan)) {
    flags <- active_flag_vec(plan)
    plan <- plan[flags, , drop = FALSE]
  }
  if (nrow(plan) == 0) {
    return("")
  }
  paste(
    vapply(
      seq_len(nrow(plan)),
      function(i) graphplan_row_to_tsv(plan[i, , drop = FALSE]),
      character(1)
    ),
    collapse = "\n\n"
  )
}

graphplan_snapshot <- function(plan) {
  if (is.null(plan) || nrow(plan) == 0) {
    return(tibble::tibble(
      row_id = integer(),
      graph_name = character(),
      active = integer()
    ))
  }
  plan <- migrate_graphplan_if_needed(plan)
  tibble::tibble(
    row_id = seq_len(nrow(plan)),
    graph_name = as.character(plan$graph_name),
    active = if ("active" %in% names(plan)) {
      as.integer(plan$active)
    } else {
      rep(1L, nrow(plan))
    }
  )
}

#' Full graphplan at import (no check artifacts) for row-level "Edited" comparison.
graphplan_baseline_capture <- function(plan) {
  if (is.null(plan) || nrow(plan) == 0L) {
    return(NULL)
  }
  strip_graphplan_check_artifacts(tibble::as_tibble(migrate_graphplan_if_needed(plan)))
}

#' TRUE if gallery cache has an export-fidelity PNG thumbnail for this row.
graphplan_row_built_in_gallery <- function(built_list, row_id) {
  built_list <- as.list(built_list %||% list())
  if (length(built_list) == 0L) {
    return(FALSE)
  }
  rid <- as.integer(row_id)[1]
  if (is.na(rid)) {
    return(FALSE)
  }
  for (item in built_list) {
    if (!is.null(item$row_id) && identical(as.integer(item$row_id), rid)) {
      path <- item$thumb_path
      return(isTRUE(item$ok) && !is.null(path) && nzchar(path) && isTRUE(file.exists(path)))
    }
  }
  FALSE
}

#' User-semantic graphplan columns for "Edited" (phase 17.4, path B).
#'
#' Excludes `active` so Activate/Deactivate alone does not mark a row Edited.
#' Columns outside [graphplan_columns] (e.g. derived `source_name`) are ignored
#' for baseline comparison so system fills there do not flip Edited.
graphplan_edited_u_cols <- function() {
  setdiff(graphplan_columns, "active")
}

#' Canonicalize graphplan row(s) before "Edited" comparison (phase 17.4).
#'
#' [validate_graphplan_for_app] runs `check*` on the **entire** plan. In particular
#' [checkAvailability] and [checkPeers] assign `data_frequency <- tolower(trim(...))`
#' for every row. The import baseline is captured **before** that pass, so path B
#' would otherwise mark almost all rows Edited after the first Validate/Save even
#' when the user changed nothing. [align_graphplan_types] aligns 0/1 and empty
#' strings with the same coercion used when updating rows.
canonicalize_graphplan_row_for_edited_compare <- function(row_df) {
  row_df <- strip_graphplan_check_artifacts(tibble::as_tibble(row_df))
  row_df <- migrate_graphplan_if_needed(row_df)
  row_df <- align_graphplan_types(row_df)
  if ("data_frequency" %in% names(row_df)) {
    row_df <- dplyr::mutate(
      row_df,
      data_frequency = {
        x <- stringr::str_trim(as.character(.data$data_frequency))
        x <- dplyr::na_if(x, "")
        stringr::str_to_lower(x)
      }
    )
  }
  row_df
}

#' Path B (phase 17.4): row differs from import baseline on user-semantic columns only.
#'
#' Ignores `active`, check artifacts, and columns outside [graphplan_edited_u_cols].
#' Rows added after baseline (`row_id > nrow(baseline)`) count as Edited.
graphplan_row_edited_b_vs_baseline <- function(row_id, current_plan, baseline_plan) {
  if (is.null(baseline_plan) || nrow(baseline_plan) == 0L) {
    return(FALSE)
  }
  row_id <- as.integer(row_id)[1]
  if (is.na(row_id) || row_id < 1L) {
    return(FALSE)
  }
  if (row_id > nrow(baseline_plan)) {
    return(TRUE)
  }
  if (is.null(current_plan) || nrow(current_plan) < row_id) {
    return(FALSE)
  }
  ucols <- graphplan_edited_u_cols()
  cr <- canonicalize_graphplan_row_for_edited_compare(current_plan[row_id, , drop = FALSE])
  br <- canonicalize_graphplan_row_for_edited_compare(baseline_plan[row_id, , drop = FALSE])
  cn <- sort(intersect(intersect(names(cr), names(br)), ucols))
  if (length(cn) == 0L) {
    return(FALSE)
  }
  !identical(
    editor_row_fingerprint(cr[, cn, drop = FALSE]),
    editor_row_fingerprint(br[, cn, drop = FALSE])
  )
}

#' Path A (phase 17.4): editor save changed at least one U-column vs pre-save row.
#'
#' Compares stripped rows on [graphplan_edited_u_cols] only (so `active`-only saves
#' return FALSE). Uses the editor-produced row before validation merge.
meaningful_editor_save_for_edited_flag <- function(before_row, after_row) {
  before_row <- canonicalize_graphplan_row_for_edited_compare(before_row)
  after_row <- canonicalize_graphplan_row_for_edited_compare(after_row)
  ucols <- graphplan_edited_u_cols()
  cn <- sort(intersect(ucols, intersect(names(before_row), names(after_row))))
  if (length(cn) == 0L) {
    return(FALSE)
  }
  !identical(
    editor_row_fingerprint(before_row[, cn, drop = FALSE]),
    editor_row_fingerprint(after_row[, cn, drop = FALSE])
  )
}

#' Hybrid "Edited" flag (phase 17.4): editor commit touches OR baseline drift on U-cols.
#'
#' @param editor_touch_row_ids Integer vector of `row_id` values that had a qualifying
#'   **Save to graphplan** in this session (path A). Reset on new xlsx load.
graphplan_row_edited_hybrid <- function(row_id,
                                        current_plan,
                                        baseline_plan,
                                        editor_touch_row_ids = integer(0)) {
  row_id <- as.integer(row_id)[1]
  touch <- as.integer(editor_touch_row_ids %||% integer(0))
  if (!is.na(row_id) && row_id %in% touch) {
    return(TRUE)
  }
  graphplan_row_edited_b_vs_baseline(row_id, current_plan, baseline_plan)
}

#' Legacy wrapper: same as [graphplan_row_edited_hybrid] with no editor touch ids (B-path only).
graphplan_row_edited_excluding_active <- function(row_id, current_plan, baseline_plan) {
  graphplan_row_edited_hybrid(row_id, current_plan, baseline_plan, integer(0))
}

graphplan_change_summary <- function(baseline, current) {
  if (is.null(baseline) || nrow(baseline) == 0) {
    return(list(
      rows_added = if (is.null(current)) 0L else nrow(current),
      rows_removed = 0L,
      rows_deactivated = 0L,
      rows_reactivated = 0L,
      rows_renamed = 0L,
      has_baseline = FALSE
    ))
  }
  if (is.null(current) || nrow(current) == 0) {
    return(list(
      rows_added = 0L,
      rows_removed = nrow(baseline),
      rows_deactivated = 0L,
      rows_reactivated = 0L,
      rows_renamed = 0L,
      has_baseline = TRUE
    ))
  }
  cur <- graphplan_snapshot(current)
  base <- baseline
  added <- setdiff(cur$graph_name, base$graph_name)
  removed <- setdiff(base$graph_name, cur$graph_name)
  common <- intersect(base$graph_name, cur$graph_name)
  deactivated <- 0L
  reactivated <- 0L
  renamed <- 0L
  for (nm in common) {
    b_act <- base$active[match(nm, base$graph_name)]
    c_act <- cur$active[match(nm, cur$graph_name)]
    if (!is.na(b_act) && !is.na(c_act) && b_act == 1L && c_act == 0L) {
      deactivated <- deactivated + 1L
    }
    if (!is.na(b_act) && !is.na(c_act) && b_act == 0L && c_act == 1L) {
      reactivated <- reactivated + 1L
    }
  }
  list(
    rows_added = length(added),
    rows_removed = length(removed),
    rows_deactivated = deactivated,
    rows_reactivated = reactivated,
    rows_renamed = renamed,
    has_baseline = TRUE
  )
}

compute_export_report <- function(graphplan,
                                  validation = NULL,
                                  built = list(),
                                  dirty = FALSE,
                                  baseline = NULL,
                                  export_scope = "all_built",
                                  selected_names = character()) {
  built <- as.list(built %||% list())
  built_ok <- sum(vapply(built, function(x) isTRUE(x$ok), logical(1)))
  built_err <- sum(vapply(built, function(x) !isTRUE(x$ok), logical(1)))
  scoped <- filter_built_graphs(
    built = built,
    scope = export_scope,
    validation = validation,
    selected_names = selected_names
  )
  scoped_ok <- sum(vapply(scoped, function(x) isTRUE(x$ok), logical(1)))
  n_plan <- if (is.null(graphplan)) 0L else nrow(graphplan)
  n_active <- if (is.null(graphplan) || n_plan == 0) {
    0L
  } else {
    sum(active_flag_vec(graphplan))
  }
  changes <- graphplan_change_summary(baseline, graphplan)
  list(
    graphplan_rows = n_plan,
    graphplan_active = as.integer(n_active),
    graphplan_inactive = as.integer(n_plan - n_active),
    validation_buildable = if (is.null(validation)) {
      NA_integer_
    } else {
      as.integer(validation$summary$n_buildable[[1]] %||% 0L)
    },
    built_total = length(built),
    built_ok = as.integer(built_ok),
    built_error = as.integer(built_err),
    export_scope = export_scope,
    export_graphs_count = length(scoped),
    export_graphs_ok = as.integer(scoped_ok),
    dirty = isTRUE(dirty),
    changes = changes
  )
}
