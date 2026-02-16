###### Scatter plot

# ------------------------------------------------------------------------------
# Plot style system (themes + functional colors) + common finalize
# ------------------------------------------------------------------------------

wrap_axis_title <- function(x, width = 38) {
  assert_packages(c("stringr"))
  x <- as.character(x)
  x <- if (length(x) >= 1) x[[1]] else ""
  x <- stringr::str_trim(x)
  if (is.na(x) || !nzchar(x)) return("")
  stringr::str_wrap(x, width = width)
}

compute_title_wrap_width <- function(width) {
  assert_packages(c("rlang"))
  
  w <- suppressWarnings(as.numeric(width))
  if (!is.finite(w) || w <= 0) w <- 8
  
  # Empirical: ~12 chars per inch for default title sizes.
  # Clamp to keep behavior stable across very small/large canvases.
  out <- floor(w * 12)
  out <- max(35, min(out, 120))
  out
}

wrap_if_needed <- function(x, width) {
  assert_packages(c("stringr"))
  x <- as.character(x %||% "")
  x <- stringr::str_trim(x)
  if (!nzchar(x)) return("")
  stringr::str_wrap(x, width = width)
}

is_true01 <- function(x) {
  x <- suppressWarnings(as.integer(x))
  isTRUE(!is.na(x) && x == 1L)
}

finalize_plot_common <- function(p, params, style, legend = TRUE) {
  assert_packages(c("ggplot2", "ggtext", "stringr", "rlang", "grid"))
  
  params <- params %||% list()
  style  <- style  %||% list()
  
  # -------------------- Style blocks (safe defaults) --------------------
  palette <- style$palette %||% list(
    text = "grey10", muted_text = "grey35", axis = "grey20",
    bg = "white", panel_bg = "white",
    grid_major = "grey90", grid_minor = "grey94"
  )
  
  typography <- style$typography %||% list(
    base_family = "sans", base_size = 11,
    title = list(size = 14, face = "bold", lineheight = 1.05, color = palette$text, margin_b = 8),
    subtitle = list(size = 11, face = "plain", lineheight = 1.05, color = palette$muted_text, margin_b = 6),
    axis_title = list(size = 10, face = "plain", color = palette$text, margin_t = 8, margin_r = 8),
    axis_text  = list(size = 9,  face = "plain", color = palette$muted_text),
    caption    = list(size = 9,  face = "plain", lineheight = 1.10, color = palette$muted_text, margin_t = 8),
    legend_text = list(size = 9, color = palette$text)
  )
  
  grid_cfg <- style$grid %||% list(
    major = list(show = TRUE,  color = palette$grid_major, linewidth = 0.30),
    minor = list(show = FALSE, color = palette$grid_minor, linewidth = 0.20),
    axis_line  = list(show = TRUE,  color = palette$axis, linewidth = 0.35),
    axis_ticks = list(show = TRUE,  color = palette$axis, linewidth = 0.25)
  )
  
  layout <- style$layout %||% list(
    plot_margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10),
    legend = list(
      position = "bottom",
      direction = "horizontal",
      justification = "left",
      box = "horizontal",
      title_blank = TRUE
    )
  )
  
  ann <- style$annotations %||% list(
    time_tag = list(show = TRUE, position = "tr", text_color = palette$axis, size = 3.2, hjust = 1.05, vjust = 1.25),
    # NOTE: trend/zero_line are consumed in plot functions, but we also expose defaults here
    trend = list(ci_show = TRUE, ci_alpha = 0.25, ci_fill = palette$grid_major, line_color = palette$axis, linewidth = 0.60),
    zero_line = list(show = TRUE, color = "grey35", linewidth = 0.30, alpha = 0.60)
  )
  
  # -------------------- Local helpers --------------------
  is_true01 <- function(x) {
    # Robust to factor/character/logical/numeric (Excel imports often yield factors)
    if (is.null(x) || length(x) == 0) return(FALSE)
    
    x <- x[[1]]
    
    if (is.factor(x)) x <- as.character(x)
    
    if (is.logical(x)) {
      return(isTRUE(x))
    }
    
    if (is.character(x)) {
      x_chr <- stringr::str_trim(stringr::str_to_lower(x))
      if (x_chr %in% c("true", "t", "yes", "y"))  return(TRUE)
      if (x_chr %in% c("false", "f", "no", "n")) return(FALSE)
    }
    
    xi <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(xi) && xi == 1L)
  }
  
  compute_title_wrap_width <- function(width) {
    w <- suppressWarnings(as.numeric(width))
    if (!is.finite(w) || w <= 0) w <- 8
    out <- floor(w * 12)
    out <- max(35, min(out, 120))
    out
  }
  
  wrap_if_needed <- function(x, width) {
    x <- as.character(x %||% "")
    x <- stringr::str_trim(x)
    if (!nzchar(x)) return("")
    stringr::str_wrap(x, width = width)
  }
  
  set_coord_cartesian_safe <- function(p, xlim = NULL, ylim = NULL, clip = "on") {
    coord <- p$coordinates
    
    # If plot already has cartesian-like coordinates, preserve existing limits
    if (inherits(coord, "CoordCartesian")) {
      cur_xlim <- coord$limits$x %||% NULL
      cur_ylim <- coord$limits$y %||% NULL
      
      # only override if explicitly provided
      xlim2 <- xlim %||% cur_xlim
      ylim2 <- ylim %||% cur_ylim
      
      # Preserve 'expand' if present
      expand2 <- coord$expand %||% TRUE
      
      # coord_fixed is a CoordCartesian subclass; preserve ratio if present
      if (inherits(coord, "CoordFixed")) {
        ratio2 <- coord$ratio %||% 1
        p$coordinates <- ggplot2::coord_fixed(
          ratio = ratio2,
          xlim = xlim2,
          ylim = ylim2,
          clip = clip,
          expand = expand2
        )
        return(p)
      }
      
      p$coordinates <- ggplot2::coord_cartesian(
        xlim = xlim2,
        ylim = ylim2,
        clip = clip,
        expand = expand2
      )
      return(p)
    }
    
    # Non-cartesian coords: don't replace (could break plots). Only set clip if default.
    # So we do nothing here.
    p
  }
  
  # --- helpers: scalarize + parse time codes safely -----------------------
  as_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x[[1]]
  }
  
  parse_time_code <- function(x, freq) {
    x <- as.character(as_scalar(x))
    x <- stringr::str_trim(x)
    if (!nzchar(x) || is.na(x)) return(NA_real_)
    
    # numeric already?
    xn <- suppressWarnings(as.numeric(x))
    if (is.finite(xn)) return(xn)
    
    freq <- tolower(stringr::str_trim(as.character(freq %||% "")))
    
    # YYYYmM
    if (freq == "m" && stringr::str_detect(x, "^\\d{4}m\\d{1,2}$")) {
      y <- as.integer(stringr::str_sub(x, 1, 4))
      m <- as.integer(stringr::str_sub(x, 6))
      if (is.finite(y) && is.finite(m)) return((y - 1987) * 12 + m)
    }
    
    # YYYYqQ
    if (freq == "q" && stringr::str_detect(x, "^\\d{4}q\\d$")) {
      y <- as.integer(stringr::str_sub(x, 1, 4))
      q <- as.integer(stringr::str_sub(x, 6))
      if (is.finite(y) && is.finite(q)) return((y - 1987) * 4 + q)
    }
    
    # YYYY
    if (freq == "y" && stringr::str_detect(x, "^\\d{4}$")) {
      y <- as.integer(x)
      if (is.finite(y)) return(y - 1987)
    }
    
    NA_real_
  }
  
  # -------------------- Title / labels --------------------
  title_primary  <- stringr::str_trim(as.character(params$title %||% ""))
  title_fallback <- stringr::str_trim(as.character(params$graph_title %||% ""))
  title_name     <- stringr::str_trim(as.character(params$graph_name %||% ""))
  
  title_candidate <- title_primary
  if (!nzchar(title_candidate)) title_candidate <- title_fallback
  if (!nzchar(title_candidate)) title_candidate <- title_name
  
  show_title_raw <- params$show_title
  
  show_title <- if (is.null(show_title_raw) || (length(show_title_raw) >= 1 && is.na(show_title_raw[[1]]))) {
    # Default: show only if we have something to show
    nzchar(title_candidate)
  } else {
    # Explicit override (robust to factor/character)
    is_true01(show_title_raw)
  }
  
  title_raw <- if (isTRUE(show_title)) title_candidate else ""
  
  wrap_w <- compute_title_wrap_width(params$width %||% NA_real_)
  title_txt <- wrap_if_needed(title_raw, width = wrap_w)
  
  caption_txt <- stringr::str_trim(as.character(params$caption %||% ""))
  x_lab_raw <- as.character(params$x_lab %||% "")
  y_lab_raw <- as.character(params$y_lab %||% "")
  
  axis_wrap_w <- max(18, floor(wrap_w * 0.55))
  x_lab <- wrap_if_needed(x_lab_raw, width = axis_wrap_w)
  y_lab <- wrap_if_needed(y_lab_raw, width = axis_wrap_w)
  
  # -------------------- Axis title policy (default OFF for X) --------------------
  graph_type <- params$graph_type %||% NA_character_
  graph_type <- stringr::str_trim(as.character(graph_type))
  graph_type <- if (!nzchar(graph_type)) NA_character_ else graph_type
  
  # Explicit override: params$show_x_lab (robust to factor/character)
  show_x_lab_raw <- params$show_x_lab
  show_x_lab <- if (is.null(show_x_lab_raw) || (length(show_x_lab_raw) >= 1 && is.na(show_x_lab_raw[[1]]))) {
    FALSE
  } else {
    is_true01(show_x_lab_raw)
  }
  
  x_lab_allowed_types <- c(
    "scatter_country_comparison",
    "scatter_before_after",
    "density_fix"
  )
  
  use_x_lab <- isTRUE(show_x_lab) || (!is.na(graph_type) && graph_type %in% x_lab_allowed_types)
  
  if (!isTRUE(use_x_lab)) {
    x_lab <- ""
  }
  
  title_arg <- if (nzchar(title_txt)) title_txt else NULL
  
  p <- p + ggplot2::labs(
    title   = title_arg,
    caption = caption_txt,
    x       = x_lab,
    y       = y_lab
  )
  
  # -------------------- Time tag (plain text; position via style) --------------------
  time_tag <- params$time_fix_label %||% NA_character_
  time_tag <- stringr::str_trim(as.character(time_tag))
  
  if (isTRUE(ann$time_tag$show %||% TRUE) && !is.na(time_tag) && nzchar(time_tag)) {
    pos <- as.character(ann$time_tag$position %||% "tr")
    pos <- stringr::str_to_lower(stringr::str_trim(pos))
    
    # 4 corners: tr/br/tl/bl
    x0 <- if (pos %in% c("tl", "bl")) -Inf else Inf
    y0 <- if (pos %in% c("bl", "br")) -Inf else Inf
    
    hjust <- ann$time_tag$hjust %||% if (x0 < 0) -0.05 else 1.05
    vjust <- ann$time_tag$vjust %||% if (y0 < 0) -0.25 else 1.25
    
    p <- p + ggplot2::annotate(
      geom  = "text",
      x     = x0,
      y     = y0,
      label = time_tag,
      hjust = hjust,
      vjust = vjust,
      size  = ann$time_tag$size %||% 3.2,
      color = ann$time_tag$text_color %||% palette$axis
    )
  }
  
  # -------------------- Legend policy (style-controlled) --------------------
  legend_pos <- if (isTRUE(legend)) (layout$legend$position %||% "bottom") else "none"
  legend_dir <- layout$legend$direction %||% "horizontal"
  legend_box <- layout$legend$box %||% "horizontal"
  legend_just <- layout$legend$justification %||% "left"
  blank_title <- isTRUE(layout$legend$title_blank %||% TRUE)
  
  # -------------------- Keeping annotations visible + optional X viewport ----------------------
  coord_clip <- as.character(params$coord_clip %||% "on")
  coord_clip <- if (coord_clip %in% c("on", "off")) coord_clip else "on"
  
  xlim_from_params <- NULL
  
  freq <- normalize_freq(params$data_frequency %||% NA_character_) %||% "y"
  
  x_min_num <- parse_time_code(params$x_min, freq)
  x_max_num <- parse_time_code(params$x_max, freq)
  
  have_min <- isTRUE(is.finite(x_min_num))
  have_max <- isTRUE(is.finite(x_max_num))
  
  graph_type_chr <- stringr::str_trim(as.character(params$graph_type %||% ""))
  
  xlim_allowed_types <- c(
    "density_fix",
    "scatter_country_comparison",
    "scatter_before_after"
  )
  
  use_xlim_from_params <- nzchar(graph_type_chr) && graph_type_chr %in% xlim_allowed_types
  
  if (isTRUE(use_xlim_from_params) && (have_min || have_max)) {
    xlim_from_params <- c(
      if (have_min) x_min_num else NA_real_,
      if (have_max) x_max_num else NA_real_
    )
    p <- set_coord_cartesian_safe(p, xlim = xlim_from_params, ylim = NULL, clip = coord_clip)
  }
  
  # Всегда: просто обеспечиваем clip=... без переопределения лимитов
  if (inherits(p$coordinates, "CoordCartesian")) {
    old <- p$coordinates
    old_x <- old$limits$x %||% NULL
    old_y <- old$limits$y %||% NULL
    old_expand <- old$expand %||% TRUE
    
    p$coordinates <- ggplot2::coord_cartesian(
      xlim = old_x,
      ylim = old_y,
      expand = old_expand,
      clip = coord_clip
    )
  }
  
  # -------------------- Base theme + overrides --------------------
  base_theme <- style$gg_theme %||% style$theme %||% ggplot2::theme_grey()
  
  p <- p + base_theme +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.background  = ggplot2::element_rect(fill = palette$bg %||% "white", color = NA),
      panel.background = ggplot2::element_rect(fill = palette$panel_bg %||% "white", color = NA),
      
      # --- grid visibility toggles (override whatever base theme did) ---
      panel.grid.major = if (isTRUE(grid_cfg$major$show %||% TRUE)) {
        ggplot2::element_line(color = grid_cfg$major$color %||% palette$grid_major, linewidth = grid_cfg$major$linewidth %||% 0.30)
      } else ggplot2::element_blank(),
      panel.grid.minor = if (isTRUE(grid_cfg$minor$show %||% FALSE)) {
        ggplot2::element_line(color = grid_cfg$minor$color %||% palette$grid_minor, linewidth = grid_cfg$minor$linewidth %||% 0.20)
      } else ggplot2::element_blank(),
      
      axis.line = if (isTRUE(grid_cfg$axis_line$show %||% TRUE)) {
        ggplot2::element_line(color = grid_cfg$axis_line$color %||% palette$axis, linewidth = grid_cfg$axis_line$linewidth %||% 0.35)
      } else ggplot2::element_blank(),
      axis.ticks = if (isTRUE(grid_cfg$axis_ticks$show %||% TRUE)) {
        ggplot2::element_line(color = grid_cfg$axis_ticks$color %||% palette$axis, linewidth = grid_cfg$axis_ticks$linewidth %||% 0.25)
      } else ggplot2::element_blank(),
      
      # --- typography (centralized) ---
      text = ggplot2::element_text(family = typography$base_family %||% "sans", size = typography$base_size %||% 11, color = palette$text),
      
      plot.title = if (is.null(title_arg)) {
        ggplot2::element_blank()
      } else {
        ggtext::element_textbox_simple(
          width      = grid::unit(1, "npc"),
          lineheight = typography$title$lineheight %||% 1.05,
          size       = typography$title$size %||% 14,
          face       = typography$title$face %||% "bold",
          color      = typography$title$color %||% palette$text,
          margin     = ggplot2::margin(b = typography$title$margin_b %||% 8)
        )
      },
      
      axis.title.x = ggplot2::element_text(
        size  = typography$axis_title$size %||% 10,
        face  = typography$axis_title$face %||% "plain",
        color = typography$axis_title$color %||% palette$text,
        margin = ggplot2::margin(t = typography$axis_title$margin_t %||% 8)
      ),
      axis.title.y = ggplot2::element_text(
        size  = typography$axis_title$size %||% 10,
        face  = typography$axis_title$face %||% "plain",
        color = typography$axis_title$color %||% palette$text,
        margin = ggplot2::margin(r = typography$axis_title$margin_r %||% 8)
      ),
      axis.text = ggplot2::element_text(
        size  = typography$axis_text$size %||% 9,
        face  = typography$axis_text$face %||% "plain",
        color = typography$axis_text$color %||% palette$muted_text
      ),
      
      plot.caption = ggplot2::element_text(
        size  = typography$caption$size %||% 9,
        face  = typography$caption$face %||% "plain",
        lineheight = typography$caption$lineheight %||% 1.10,
        color = typography$caption$color %||% palette$muted_text,
        margin = ggplot2::margin(t = typography$caption$margin_t %||% 8)
      ),
      
      plot.margin = if (is.null(title_arg)) {
        # можно оставить как есть, но если хочешь убрать “ощущаемый” верхний зазор:
        ggplot2::margin(t = 6, r = 10, b = 10, l = 10)
      } else {
        layout$plot_margin %||% ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
      },
      
      legend.position = legend_pos,
      legend.direction = legend_dir,
      legend.box = legend_box,
      legend.justification = legend_just,
      legend.title = if (isTRUE(blank_title)) ggplot2::element_blank() else ggplot2::element_text(),
      legend.text = ggplot2::element_text(
        size  = typography$legend_text$size %||% 9,
        color = typography$legend_text$color %||% palette$text
      ),
      legend.margin = ggplot2::margin(t = 2),
      legend.box.margin = ggplot2::margin(t = 2)
    )
  
  p
}


compute_plot_scaling <- function(n_pts,
                                 width = NA_real_,
                                 height = NA_real_,
                                 n_labels = NA_integer_,
                                 kind = "scatter") {
  assert_packages(c("rlang", "dplyr"))
  
  # ---- Validate --------------------------------------------------------
  n_pts <- suppressWarnings(as.integer(n_pts))
  if (is.na(n_pts) || n_pts < 0) n_pts <- 0L
  
  width <- suppressWarnings(as.numeric(width))
  height <- suppressWarnings(as.numeric(height))
  
  # Reasonable fallback to keep behavior deterministic
  if (!is.finite(width) || width <= 0) width <- 8
  if (!is.finite(height) || height <= 0) height <- 5
  
  area <- width * height
  density <- n_pts / area
  
  # Label count can matter for clutter (optional)
  n_labels <- suppressWarnings(as.integer(n_labels))
  if (is.na(n_labels) || n_labels < 0) n_labels <- NA_integer_
  
  # ---- Scaling policy --------------------------------------------------
  # Main idea:
  # - pt_size decreases with density (more points per inch^2 -> smaller points)
  # - lab_mm decreases with density too, but we keep it relatively readable
  #   in the "typical" band of 100-200 points (your requirement).
  #
  # We keep it piecewise for transparency and stability.
  
  # Points: ggplot "size" units
  pt_size <- dplyr::case_when(
    density <= 0.6 ~ 3.2,
    density <= 1.2 ~ 2.7,
    density <= 2.0 ~ 2.3,
    density <= 3.0 ~ 2.0,
    TRUE ~ 1.7
  )
  
  # Labels: mm (convert to ggplot size via / .pt)
  # Tuned to be larger around 100-200 pts than earlier baseline.
  lab_mm <- dplyr::case_when(
    density <= 0.6 ~ 4.4,
    density <= 1.2 ~ 4.0,
    density <= 2.0 ~ 4.2,  # "typical 100-200 band" often lands here
    density <= 3.0 ~ 3.7,
    TRUE ~ 3.3
  )
  
  # Lines: linewidth in mm-ish, used as multiplier over style$scale$base_linewidth
  line_width <- dplyr::case_when(
    density <= 0.6 ~ 1.15,
    density <= 1.2 ~ 1.05,
    density <= 2.0 ~ 1.00,
    density <= 3.0 ~ 0.90,
    TRUE ~ 0.80
  )
  
  # Nudges as fractions of plot range (plot computes range → abs)
  nudge_x_frac <- dplyr::case_when(
    density <= 0.8 ~ 0.022,
    density <= 1.6 ~ 0.020,
    density <= 2.4 ~ 0.018,
    TRUE ~ 0.016
  )
  nudge_y_frac <- nudge_x_frac
  
  list(
    pt_size = pt_size,
    lab_mm = lab_mm,
    line_width = line_width,
    nudge_x_frac = nudge_x_frac,
    nudge_y_frac = nudge_y_frac,
    density = density,
    area = area
  )
}

make_placeholder_plot <- function(params, style, msg = "No data to plot") {
  assert_packages(c("ggplot2"))
  
  p <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 4) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    ggplot2::theme_void()
  
  finalize_plot_common(p, params, style, legend = FALSE)
}

# ------------------------------------------------------------------------------
# Scatter: refactored (no eval/parse), log-space semantics respected
# ------------------------------------------------------------------------------
scatterCountryComparison <- function(data,
                                     graph_params,
                                     country_iso2c,
                                     peers_iso2c,
                                     verbose = TRUE,
                                     warn_invalid = TRUE,
                                     debug = FALSE) {
  assert_packages(c("dplyr", "ggplot2", "stringr", "rlang", "tibble"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  # ---- Local helpers ---------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("scatterCountryComparison: ", msg))
    }
  }
  
  # robust: log transform in “plot space” without scalar-if_else bugs
  apply_log10_plotspace <- function(x, do_log) {
    x <- suppressWarnings(as.numeric(x))
    if (!isTRUE(do_log)) return(x)
    out <- rep(NA_real_, length(x))
    ok <- is.finite(x) & x > 0
    out[ok] <- log10(x[ok])
    out
  }
  
  palette <- style$palette %||% list()
  ann     <- style$annotations %||% list()
  
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- Basic guards ----------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_cols <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_cols, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", "), "."))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  x_ind <- params$x_ind %||% NA_character_
  y_ind <- params$y_ind %||% NA_character_
  if (is.na(x_ind) || is.na(y_ind) || !nzchar(x_ind) || !nzchar(y_ind)) {
    warn_active("x_ind/y_ind are not set; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (x_ind/y_ind not set)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "x_ind/y_ind missing"))
    return(out)
  }
  
  if (!(x_ind %in% names(ext)) || !(y_ind %in% names(ext))) {
    warn_active(paste0("extdata has no columns for x_ind/y_ind: ", x_ind, ", ", y_ind))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "indicator cols missing"))
    return(out)
  }
  
  time_fix <- params$time_fix %||% NA_real_
  if (!is.finite(suppressWarnings(as.numeric(time_fix)))) {
    warn_active("time_fix is NA for scatter; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (time_fix is NA)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "time_fix NA"))
    return(out)
  }
  time_fix <- suppressWarnings(as.numeric(time_fix))
  
  # Normalize peers
  peers_iso2c <- as.character(peers_iso2c %||% character(0)) |>
    stringr::str_trim()
  peers_iso2c <- peers_iso2c[!is.na(peers_iso2c) & peers_iso2c != ""]
  peers_iso2c <- unique(peers_iso2c)
  
  all_flag <- as.integer(params$all %||% 0L)
  
  # ---- Prepare data at time_fix ---------------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$time == time_fix) |>
    dplyr::select(dplyr::any_of(c("country", "country_id", "year")), dplyr::all_of(c(x_ind, y_ind)))
  
  if (nrow(df0) == 0) {
    warn_active("No rows at time_fix; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data at time_fix")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, meta = tibble::tibble(msg = "no rows at time_fix"))
    return(out)
  }
  
  # ---- Apply log-space semantics (plot space) --------------------------
  x_log <- is_true01(params$x_log %||% 0L)
  y_log <- is_true01(params$y_log %||% 0L)
  
  df <- df0 |>
    dplyr::mutate(
      x_raw  = suppressWarnings(as.numeric(.data[[x_ind]])),
      y_raw  = suppressWarnings(as.numeric(.data[[y_ind]])),
      x_plot = apply_log10_plotspace(.data$x_raw, x_log),
      y_plot = apply_log10_plotspace(.data$y_raw, y_log)
    ) |>
    dplyr::filter(is.finite(.data$x_plot), is.finite(.data$y_plot))
  
  if (nrow(df) == 0) {
    warn_active("All points were removed after numeric/log transforms.")
    p0 <- make_placeholder_plot(params, style, "No valid points after transforms")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, df = df, meta = tibble::tibble(msg = "no points after transform"))
    return(out)
  }
  
  # Apply all/peers filtering
  if (all_flag != 1L) {
    df <- df |>
      dplyr::filter(.data$country_id %in% unique(c(country_iso2c, peers_iso2c)))
  }
  
  if (nrow(df) == 0) {
    warn_active("No points left after all/peers filtering.")
    p0 <- make_placeholder_plot(params, style, "No points after filtering")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, df = df, meta = tibble::tibble(msg = "no points after filter"))
    return(out)
  }
  
  # Role tagging (system-wide semantics)
  df <- add_country_role(df, country_iso2c = country_iso2c, peers_iso2c = peers_iso2c, id_col = "country_id")
  
  # ---- Limits in plot space -------------------------------------------
  x_min <- suppressWarnings(as.numeric(params$x_min %||% NA_real_))
  x_max <- suppressWarnings(as.numeric(params$x_max %||% NA_real_))
  y_min <- suppressWarnings(as.numeric(params$y_min %||% NA_real_))
  y_max <- suppressWarnings(as.numeric(params$y_max %||% NA_real_))
  
  x_rng <- range(df$x_plot, na.rm = TRUE)
  y_rng <- range(df$y_plot, na.rm = TRUE)
  
  if (!is.finite(x_min)) x_min <- x_rng[[1]]
  if (!is.finite(x_max)) x_max <- x_rng[[2]]
  if (!is.finite(y_min)) y_min <- y_rng[[1]]
  if (!is.finite(y_max)) y_max <- y_rng[[2]]
  
  if (!(is.finite(x_min) && is.finite(x_max) && x_min < x_max)) {
    warn_active("Bad x limits; fallback to data range.")
    x_min <- x_rng[[1]]; x_max <- x_rng[[2]]
  }
  if (!(is.finite(y_min) && is.finite(y_max) && y_min < y_max)) {
    warn_active("Bad y limits; fallback to data range.")
    y_min <- y_rng[[1]]; y_max <- y_rng[[2]]
  }
  
  # ---- Scaling (system helper) ----------------------------------------
  n_pts <- nrow(df)
  n_labels <- sum(df$role %in% c("country", "peers"))
  sc <- compute_plot_scaling(
    n_pts = n_pts,
    width = params$width %||% NA_real_,
    height = params$height %||% NA_real_,
    n_labels = n_labels,
    kind = "scatter"
  )
  
  base_pt     <- sc$pt_size
  base_lab_mm <- sc$lab_mm
  base_lw     <- (style$scale$base_linewidth %||% 0.6) * (sc$line_width %||% 1)
  
  dx <- (x_max - x_min)
  dy <- (y_max - y_min)
  nudge_x <- (sc$nudge_x_frac %||% 0.02) * ifelse(is.finite(dx) && dx > 0, dx, 1)
  nudge_y <- (sc$nudge_y_frac %||% 0.02) * ifelse(is.finite(dy) && dy > 0, dy, 1)
  
  # ---- Semantic styles (role × object) --------------------------------
  st_others_pt <- style_for(style, "others",  "point")
  st_peers_pt  <- style_for(style, "peers",   "point")
  st_ctry_pt   <- style_for(style, "country", "point")
  
  st_peers_tx  <- style_for(style, "peers",   "text")
  st_ctry_tx   <- style_for(style, "country", "text")
  
  axis_col <- palette$axis %||% "grey20"
  
  # ---- Optional style-driven policies ---------------------------------
  zero_cfg <- ann$zero_line %||% list(show = TRUE, color = axis_col, linewidth = 0.30, alpha = 0.60)
  trend_cfg <- ann$trend %||% list(ci_show = TRUE, ci_alpha = 0.25, ci_fill = palette$grid_major %||% "grey90",
                                   line_color = axis_col, linewidth = 0.60)
  
  # you may later put this into style$labels; safe fallback here
  check_overlap <- FALSE
  
  # ---- Trend line policy ----------------------------------------------
  trend_type <- params$trend_type %||% NA_character_
  add_trend <- !is.na(trend_type) && nzchar(trend_type)
  
  # ---- Build plot ------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$x_plot, y = .data$y_plot)) +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max))
  
  # Others (only visible when all==1)
  p <- p +
    ggplot2::geom_point(
      data = dplyr::filter(df, .data$role == "others"),
      color = st_others_pt$color,
      alpha = if (all_flag == 1L) st_others_pt$alpha else 0,
      size  = base_pt * (st_others_pt$size_mult %||% 1)
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, .data$role == "peers"),
      color = st_peers_pt$color,
      alpha = st_peers_pt$alpha,
      size  = base_pt * (st_peers_pt$size_mult %||% 1)
    ) +
    ggplot2::geom_point(
      data = dplyr::filter(df, .data$role == "country"),
      color = st_ctry_pt$color,
      alpha = st_ctry_pt$alpha,
      size  = base_pt * (st_ctry_pt$size_mult %||% 1)
    )
  
  # Trend: style-driven CI (default on when trend requested)
  if (isTRUE(add_trend)) {
    p <- p +
      ggplot2::geom_smooth(
        method = trend_type,
        formula = y ~ x,
        se = isTRUE(trend_cfg$ci_show %||% TRUE),
        color = trend_cfg$line_color %||% axis_col,
        fill = trend_cfg$ci_fill %||% (palette$grid_major %||% "grey90"),
        linewidth = (trend_cfg$linewidth %||% 0.6),
        alpha = (trend_cfg$ci_alpha %||% 0.25)
      )
  }
  
  # Labels: peers + country (two layers so we can control role-specific styles cleanly)
  df_peers <- dplyr::filter(df, .data$role == "peers")
  df_ctry  <- dplyr::filter(df, .data$role == "country")
  
  p <- p +
    ggplot2::geom_text(
      data = df_peers,
      ggplot2::aes(label = .data$country_id),
      color = st_peers_tx$color,
      alpha = st_peers_tx$alpha,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      size = (base_lab_mm * (st_peers_tx$size_mult %||% 1)) / ggplot2::.pt,
      check_overlap = check_overlap
    ) +
    ggplot2::geom_text(
      data = df_ctry,
      ggplot2::aes(label = .data$country_id),
      color = st_ctry_tx$color,
      alpha = st_ctry_tx$alpha,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      size = (base_lab_mm * (st_ctry_tx$size_mult %||% 1)) / ggplot2::.pt,
      check_overlap = check_overlap
    )
  
  # Zero line (style-driven)
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        color = zero_cfg$color %||% axis_col,
        linewidth = zero_cfg$linewidth %||% 0.30,
        alpha = zero_cfg$alpha %||% 0.60
      )
  }
  
  # Common finalize: uses style$palette / style$typography / style$grid / style$layout / style$annotations$time_tag
  p <- finalize_plot_common(p, params, style, legend = FALSE)
  
  out <- list(graph = p, data = df)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      df0 = df0,
      df  = df,
      meta = tibble::tibble(
        n_pts = n_pts,
        n_labels = n_labels,
        density = sc$density,
        pt_size = base_pt,
        lab_mm = base_lab_mm,
        x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
        all_flag = all_flag,
        x_log = x_log,
        y_log = y_log,
        add_trend = add_trend
      )
    )
  }
  
  out
}


###### Scatter before-after
# to-do

###### Scatter dynamic

# year <- function(x) as.POSIXlt(x)$year + 1900
# ggplot(economics, aes(unemploy / pop, uempmed)) + 
#   geom_path(colour = "grey50") +
#   geom_point(aes(colour = year(date)))

###### Bar dynamic - dodged, stacked or stacked and normalized

barDynamic <- function(data,
                       graph_params,
                       country_iso2c,
                       peers_iso2c = NULL,
                       verbose = TRUE,
                       warn_invalid = TRUE,
                       debug = TRUE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  graph_type <- params$graph_type %||% "bar_dynamic"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "bar_dynamic"
  
  # ---- Local helpers ---------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("barDynamic: ", msg))
    }
  }
  
  build_time_breaks_labels <- function(params) {
    time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
    time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
    labfreq    <- suppressWarnings(as.numeric(params$labfreq %||% NA_real_))
    
    if (!is.finite(time_start) || !is.finite(time_end) || !is.finite(labfreq) || labfreq <= 0) {
      return(list(breaks = NULL, labels = NULL))
    }
    
    tts <- suppressWarnings(as.numeric(params$timetony_start %||% 0))
    tte <- suppressWarnings(as.numeric(params$timetony_end %||% 0))
    if (!is.finite(tts)) tts <- 0
    if (!is.finite(tte)) tte <- 0
    
    breaks <- seq(time_start + tts, time_end - tte + 1, by = labfreq)
    
    # Backward-compat: x_min/x_max may be c(year, sub) or scalar year
    x_min <- params$x_min %||% NA
    x_max <- params$x_max %||% NA
    
    x_min_year <- suppressWarnings(as.integer(if (length(x_min) >= 1) x_min[[1]] else NA))
    x_max_year <- suppressWarnings(as.integer(if (length(x_max) >= 1) x_max[[1]] else NA))
    
    if (!is.finite(x_min_year) || !is.finite(x_max_year) || x_min_year > x_max_year) {
      return(list(breaks = breaks, labels = NULL))
    }
    
    # The legacy behavior: if timetony_start==0 label starts at x_min, else x_min+1
    label_start <- if (tts == 0) x_min_year else (x_min_year + 1L)
    labels <- seq.int(label_start, x_max_year, by = 1L)
    
    # If lengths mismatch, do not force (ggplot would recycle/mislabel).
    if (length(labels) != length(breaks)) {
      return(list(breaks = breaks, labels = NULL))
    }
    
    list(breaks = breaks, labels = labels)
  }
  
  # ---- Ann/palette -----------------------------------------------------
  palette <- style$palette %||% list()
  ann     <- style$annotations %||% list()
  
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- Guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_base <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", "), "."))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  indicators <- params$indicators %||% character(0)
  indicators <- as.character(indicators)
  indicators <- stringr::str_trim(indicators)
  indicators <- indicators[!is.na(indicators) & indicators != ""]
  
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty indicators"))
    return(out)
  }
  
  missing_inds <- setdiff(indicators, names(ext))
  if (length(missing_inds) > 0) {
    warn_active(paste0("extdata has no columns for indicators: ", paste(missing_inds, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing indicator cols"))
    return(out)
  }
  
  time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
  time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
  if (!is.finite(time_start) || !is.finite(time_end) || time_start > time_end) {
    warn_active("time_start/time_end are not valid for dynamic plot; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No time window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "bad time window"))
    return(out)
  }
  
  # ---- Long data -------------------------------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::select(dplyr::any_of(c("country", "country_id", "year", "quarter", "month", "time")),
                  dplyr::all_of(indicators)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      value_raw = suppressWarnings(as.numeric(.data$value_raw)),
      time      = suppressWarnings(as.numeric(.data$time))
    ) |>
    dplyr::filter(
      .data$country_id == country_iso2c,
      is.finite(.data$time),
      .data$time >= time_start,
      .data$time <= time_end,
      is.finite(.data$value_raw)
    )
  
  if (nrow(df0) == 0) {
    warn_active("No rows after filtering by window/country/NA; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data in window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, meta = tibble::tibble(msg = "no rows after filter"))
    return(out)
  }
  
  # Preserve indicator order
  df0 <- df0 |>
    dplyr::mutate(variable = factor(.data$variable, levels = indicators))
  
  # ---- Secondary axis (robust, patch) ---------------------------------
  # Plan may store sec_y_axis as a string like "a, b, coeff", not 0/1.
  # So we activate sec axis iff indicators_sec overlaps with indicators AND coeff is valid.
  coeff <- suppressWarnings(as.numeric(params$coeff %||% NA_real_))
  
  indicators_sec_raw <- params$indicators_sec %||% character(0)
  indicators_sec_raw <- as.character(indicators_sec_raw) |> stringr::str_trim()
  indicators_sec_raw <- indicators_sec_raw[!is.na(indicators_sec_raw) & indicators_sec_raw != ""]
  
  indicators_sec <- intersect(indicators_sec_raw, indicators)
  
  do_sec <- length(indicators_sec) > 0 && is.finite(coeff) && coeff != 0
  
  # optional warning if plan asked for sec-axis but we can't enable it
  if (!isTRUE(do_sec) && length(indicators_sec_raw) > 0) {
    warn_active("Secondary axis requested but disabled: check indicators_sec overlap and coeff.")
  }
  
  df <- df0 |>
    dplyr::mutate(
      value_plot = dplyr::if_else(
        do_sec & .data$variable %in% indicators_sec,
        .data$value_raw * coeff,
        .data$value_raw
      )
    )
  
  # For stacked bars, limits must be based on cumulative sums, not component maxima.
  df_stack_range <- NULL
  df_total <- NULL
  
  if (graph_type %in% c("structure_dynamic", "structure_dynamic_norm")) {
    df_stack_range <- df |>
      dplyr::summarise(
        pos_sum = sum(pmax(.data$value_plot, 0), na.rm = TRUE),
        neg_sum = sum(pmin(.data$value_plot, 0), na.rm = TRUE),
        .by = "time"
      )
    
    df_total <- df_stack_range |>
      dplyr::mutate(total = .data$pos_sum + .data$neg_sum) |>
      dplyr::select(.data$time, .data$total)
  }
  
  # ---- Labels for legend ----------------------------------------------
  dict <- data$dict %||% tibble::tibble()
  legend_labels <- indicators
  
  if (is.data.frame(dict) && nrow(dict) > 0 && all(c("indicator_code", "indicator") %in% names(dict))) {
    dict_map <- dict |>
      dplyr::filter(.data$indicator_code %in% indicators) |>
      dplyr::distinct(.data$indicator_code, .data$indicator) |>
      dplyr::mutate(indicator_code = factor(.data$indicator_code, levels = indicators)) |>
      dplyr::arrange(.data$indicator_code)
    
    if (nrow(dict_map) > 0) {
      legend_labels <- dict_map$indicator
      legend_labels <- as.character(legend_labels)
      if (length(legend_labels) != length(indicators)) {
        legend_labels <- indicators
      } else {
        if (isTRUE(do_sec) && length(indicators_sec) > 0) {
          is_sec <- indicators %in% indicators_sec
          legend_labels[is_sec]  <- paste0(legend_labels[is_sec],  ", rha")
          legend_labels[!is_sec] <- paste0(legend_labels[!is_sec], ", lha")
        }
      }
    }
  }
  
  # ---- Legend labels: wrap to prevent overflow --------------------------
  wrap_w <- compute_title_wrap_width(params$width %||% NA_real_)
  legend_wrap_w <- max(18, floor(wrap_w * 0.55))
  
  legend_labels_wrapped <- stringr::str_wrap(legend_labels, width = legend_wrap_w)
  
  plot_w <- suppressWarnings(as.numeric(params$width %||% NA_real_))
  if (!is.finite(plot_w) || plot_w <= 0) plot_w <- 8
  
  k <- length(indicators)
  legend_nrow <- dplyr::case_when(
    k <= 6 ~ 1L,
    plot_w >= 9 ~ 2L,
    TRUE ~ 3L
  )
  
  # ---- Colors for indicators (deterministic fallback) ------------------
  # 1) try style$palette$categorical (named or vector)
  # 2) fallback to ACRA order (legacy palette you already have)
  colors <- NULL
  cat_pal <- style$palette$categorical %||% NULL
  
  if (!is.null(cat_pal)) {
    if (is.character(cat_pal) && length(cat_pal) >= length(indicators)) {
      colors <- unname(cat_pal[seq_along(indicators)])
    }
    if (is.null(colors) && is.character(cat_pal) && !is.null(names(cat_pal))) {
      # If named, map by indicator_code
      colors <- unname(cat_pal[indicators])
      if (any(is.na(colors))) colors <- NULL
    }
  }
  
  if (is.null(colors)) {
    # safe legacy fallback: stable order
    acra_keys <- c(
      "green","sec2","dark","sec1","red","sec3","sec6","black","brown","sec5","sec7","sec8",
      "add1","add2","reddest","add4","add5","orange"
    )
    # If ACRA is not available for some reason, ggplot will pick defaults.
    colors <- tryCatch(
      as.vector(ACRA[acra_keys])[seq_len(length(indicators))],
      error = function(e) NULL
    )
  }
  
  # ---- Data range for y limits (y_rng) ---------------------------------
  y_rng <- if (identical(graph_type, "structure_dynamic") &&
               !is.null(df_stack_range) && nrow(df_stack_range) > 0) {
    range(c(df_stack_range$neg_sum, df_stack_range$pos_sum), na.rm = TRUE)
  } else if (identical(graph_type, "structure_dynamic_norm")) {
    c(0, 1)
  } else {
    range(df$value_plot, na.rm = TRUE)
  }
  
  # safety: fully degenerate or non-finite -> fallback to something sane
  if (!all(is.finite(y_rng)) || length(y_rng) != 2 || y_rng[[1]] >= y_rng[[2]]) {
    warn_active("Bad y range from data; fallback to [0,1].")
    y_rng <- c(0, 1)
  }
  
  # ---- Y limits --------------------------------------------------------
  y_min_in <- params$y_min %||% NA_real_
  y_max_in <- params$y_max %||% NA_real_
  
  # y_rng already computed above (keep your existing logic)
  pol <- y_lim_policy(
    kind = graph_type,          # or map graph_type -> kind
    y_min = y_min_in,
    y_max = y_max_in,
    y_rng_data = y_rng
  )
  
  y_min <- pol$y_min
  y_max <- pol$y_max
  
  # ---- X breaks/labels -------------------------------------------------
  xcfg <- build_time_breaks_labels(params)
  
  # ---- Scaling ---------------------------------------------------------
  sc <- compute_plot_scaling(
    n_pts   = nrow(df),
    width   = params$width %||% NA_real_,
    height  = params$height %||% NA_real_,
    n_labels = NA_integer_,
    kind    = "bar"
  )
  
  st_bar <- style_for(style, role = "country", object = "bar")
  base_lw <- (style$scale$base_linewidth %||% 0.6) * (sc$line_width %||% 1)
  
  # ---- Position policy -------------------------------------------------
  pos <- switch(
    graph_type,
    "bar_dynamic"            = ggplot2::position_dodge(width = 0.65),
    "structure_dynamic"      = "stack",
    "structure_dynamic_norm" = "fill",
    ggplot2::position_dodge(width = 0.65)
  )
  
  # ---- Build plot ------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$value_plot, fill = .data$variable))
  
  p <- p +
    ggplot2::geom_col(
      alpha = st_bar$alpha %||% 0.85,
      width = 0.65,
      position = pos
    )
  
  # Totals for structure_dynamic (legacy behavior)
  if (identical(graph_type, "structure_dynamic") && length(indicators) > 1 && !is.null(df_total) && nrow(df_total) > 0) {
    st_tot <- style_for(style, role = "country", object = "point")
    
    p <- p +
      ggplot2::geom_point(
        data = df_total,
        ggplot2::aes(x = .data$time, y = .data$total),
        inherit.aes = FALSE,
        shape = 17,
        size  = (sc$pt_size %||% 2.2) * (st_tot$size_mult %||% 1),
        color = st_tot$color,
        alpha = st_tot$alpha
      )
  }
  
  # ---- Y limits: NEVER clip bars via scale limits -----------------------
  ylim_vec <- c(pol$y_min, pol$y_max)
  
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) {
    warn_active("y_lim_policy produced non-finite/degenerate limits; fallback to data range.")
    ylim_vec <- y_rng
  }
  
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = do_sec,
    coeff  = coeff
  )
  
  # X scale (if we built breaks)
  if (!is.null(xcfg$breaks)) {
    if (!is.null(xcfg$labels)) {
      p <- p + ggplot2::scale_x_continuous(breaks = xcfg$breaks, labels = xcfg$labels)
    } else {
      p <- p + ggplot2::scale_x_continuous(breaks = xcfg$breaks)
    }
  }
  
  # Fill scale with labels/colors
  if (!is.null(colors)) {
    p <- p + ggplot2::scale_fill_manual(
      values = colors,
      labels = legend_labels_wrapped,
      name   = ""
    ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE)
      )
  } else {
    p <- p + ggplot2::scale_fill_discrete(labels = legend_labels_wrapped, name = "") +
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE)
      )
  }
  
  # Zero line (style-driven)
  zero_cfg <- ann$zero_line %||% list(show = TRUE, color = palette$axis %||% "grey20", linewidth = 0.30, alpha = 0.60)
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        color = zero_cfg$color %||% (palette$axis %||% "grey20"),
        linewidth = (zero_cfg$linewidth %||% 0.30),
        alpha = (zero_cfg$alpha %||% 0.60)
      )
  }
  
  # Legend policy: if single indicator, hide legend (legacy behavior)
  show_legend <- length(indicators) > 1
  
  params2 <- params
  
  if (graph_type %in% c("structure_dynamic", "structure_dynamic_norm") && length(indicators) > 1) {
    params2$y_lab <- ""
  }
  
  p <- finalize_plot_common(p, params2, style, legend = show_legend)
  
  out <- list(graph = p, data = df)

  
  # if (isTRUE(debug)) {
  #   out$debug <- list(
  #     df0 = df0,
  #     df  = df,
  #     meta = tibble::tibble(
  #       graph_type = graph_type,
  #       do_sec = do_sec,
  #       coeff = coeff,
  #       n = nrow(df),
  #       time_start = time_start,
  #       time_end = time_end,
  #       y_min = y_min,
  #       y_max = y_max,
  #       y_min_user = y_min_user,
  #       y_max_user = y_max_user,
  #       indicators = paste(indicators, collapse = ","),
  #       indicators_sec = paste(indicators_sec, collapse = ","),
  #       range_raw_min = min(df$value_raw, na.rm = TRUE),
  #       range_raw_max = max(df$value_raw, na.rm = TRUE),
  #       range_plot_min = min(df$value_plot, na.rm = TRUE),
  #       range_plot_max = max(df$value_plot, na.rm = TRUE)
  #     )
  #   )
  # }
  
  out
}

structureDynamic <- barDynamic
structureDynamicNorm <- barDynamic


###### Bar country comparison - dodged, stacked or stacked and normalized

barCountryComparison <- function(data,
                                 graph_params,
                                 country_iso2c,
                                 peers_iso2c,
                                 verbose = TRUE,
                                 warn_invalid = TRUE,
                                 debug = TRUE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  # ---- Local helpers ---------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("barCountryComparison: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |>
      stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  compute_sec_axis_spec <- function(indicators, indicators_sec_raw, coeff) {
    indicators_sec_raw <- normalize_chr_vec(indicators_sec_raw)
    indicators_sec <- intersect(indicators_sec_raw, indicators)
    
    coeff <- suppressWarnings(as.numeric(coeff))
    do_sec <- length(indicators_sec) > 0 && is.finite(coeff) && coeff != 0
    
    list(do_sec = do_sec, indicators_sec = indicators_sec, coeff = coeff)
  }
  
  build_indicator_labels <- function(dict, indicators, do_sec, indicators_sec) {
    out <- indicators
    
    if (is.data.frame(dict) &&
        nrow(dict) > 0 &&
        all(c("indicator_code", "indicator") %in% names(dict))) {
      map <- tibble::as_tibble(dict) |>
        dplyr::filter(.data$indicator_code %in% indicators) |>
        dplyr::distinct(.data$indicator_code, .data$indicator) |>
        dplyr::mutate(indicator_code = factor(.data$indicator_code, levels = indicators)) |>
        dplyr::arrange(.data$indicator_code)
      
      if (nrow(map) == length(indicators)) {
        out <- as.character(map$indicator)
      }
    }
    
    if (isTRUE(do_sec) && length(indicators_sec) > 0) {
      is_sec <- indicators %in% indicators_sec
      out[is_sec]  <- paste0(out[is_sec],  ", rha")
      out[!is_sec] <- paste0(out[!is_sec], ", lha")
    }
    
    out
  }
  
  pick_fill_colors <- function(style, keys) {
    # Preference: style$palette$categorical (named or long enough vector)
    cat_pal <- style$palette$categorical %||% NULL
    
    if (is.character(cat_pal) && !is.null(names(cat_pal))) {
      col <- unname(cat_pal[keys])
      if (!any(is.na(col))) return(col)
    }
    if (is.character(cat_pal) && length(cat_pal) >= length(keys)) {
      return(unname(cat_pal[seq_along(keys)]))
    }
    
    # Fallback: stable ACRA order (if exists)
    acra_keys <- c(
      "green","sec2","dark","sec1","red","sec3","sec6","black","brown","sec5","sec7","sec8",
      "add1","add2","reddest","add4","add5","orange"
    )
    
    tryCatch(
      as.vector(ACRA[acra_keys])[seq_len(length(keys))],
      error = function(e) NULL
    )
  }
  
  compute_y_rng_country <- function(kind, df) {
    # df: country_id, variable, value_plot
    if (kind %in% c("structure_country_comparison")) {
      rng_df <- df |>
        dplyr::summarise(
          pos_sum = sum(pmax(.data$value_plot, 0), na.rm = TRUE),
          neg_sum = sum(pmin(.data$value_plot, 0), na.rm = TRUE),
          .by = "country_id"
        )
      return(range(c(rng_df$neg_sum, rng_df$pos_sum), na.rm = TRUE))
    }
    
    if (kind %in% c("structure_country_comparison_norm")) {
      return(c(0, 1))
    }
    
    range(df$value_plot, na.rm = TRUE)
  }
  
  # ---- Params / type ---------------------------------------------------
  graph_type <- params$graph_type %||% "bar_country_comparison"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "bar_country_comparison"
  
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  all_flag <- suppressWarnings(as.integer(params$all %||% 0L))
  if (is.na(all_flag)) all_flag <- 0L
  
  # ---- Guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_base <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", "), "."))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty indicators"))
    return(out)
  }
  
  missing_inds <- setdiff(indicators, names(ext))
  if (length(missing_inds) > 0) {
    warn_active(paste0("extdata has no columns for indicators: ", paste(missing_inds, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing indicator cols"))
    return(out)
  }
  
  time_fix <- suppressWarnings(as.numeric(params$time_fix %||% NA_real_))
  if (!is.finite(time_fix)) {
    warn_active("time_fix is NA; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (time_fix is NA)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "time_fix NA"))
    return(out)
  }
  
  peers_iso2c <- normalize_chr_vec(peers_iso2c)
  
  # ---- Long data at time_fix ------------------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$time == time_fix) |>
    dplyr::select(dplyr::any_of(c("country", "country_id", "year", "quarter", "month", "time")),
                  dplyr::all_of(indicators)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      value_raw = suppressWarnings(as.numeric(.data$value_raw))
    ) |>
    dplyr::filter(is.finite(.data$value_raw))
  
  if (nrow(df0) == 0) {
    warn_active("No non-missing values at time_fix; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data at time_fix")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, meta = tibble::tibble(msg = "no rows at time_fix"))
    return(out)
  }
  
  # Preserve indicator order
  df0 <- df0 |>
    dplyr::mutate(variable = factor(.data$variable, levels = indicators))
  
  # ---- Secondary axis spec (strict rule) -------------------------------
  sec <- compute_sec_axis_spec(
    indicators = indicators,
    indicators_sec_raw = params$indicators_sec %||% character(0),
    coeff = params$coeff %||% NA_real_
  )
  if (!isTRUE(sec$do_sec) && length(normalize_chr_vec(params$indicators_sec)) > 0) {
    warn_active("Secondary axis requested but disabled: check indicators_sec overlap and coeff.")
  }
  
  df <- df0
  if (isTRUE(sec$do_sec)) {
    df <- df |>
      dplyr::mutate(
        value_plot = dplyr::if_else(
          as.character(.data$variable) %in% sec$indicators_sec,
          .data$value_raw * sec$coeff,
          .data$value_raw
        )
      )
  } else {
    df <- df |>
      dplyr::mutate(value_plot = .data$value_raw)
  }
  
  # ---- Country filtering (all vs peers+country) -------------------------
  if (all_flag != 1L) {
    df <- df |>
      dplyr::filter(.data$country_id %in% unique(c(country_iso2c, peers_iso2c)))
  }
  
  if (nrow(df) == 0) {
    warn_active("No rows left after all/peers filtering; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data after filtering")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(df0 = df0, df = df, meta = tibble::tibble(msg = "no rows after filter"))
    return(out)
  }
  
  # Role tagging for single-indicator mode (country/peers/others)
  df <- add_country_role(df, country_iso2c = country_iso2c, peers_iso2c = peers_iso2c, id_col = "country_id")
  
  # ---- Ordering by first indicator -------------------------------------
  order_ind <- indicators[[1]]
  
  ordering_tbl <- df |>
    dplyr::filter(as.character(.data$variable) == order_ind) |>
    dplyr::summarise(
      ord_value = dplyr::first(.data$value_plot),
      .by = c("country_id", "country")
    ) |>
    dplyr::arrange(dplyr::desc(.data$ord_value)) |>
    dplyr::mutate(ordering = dplyr::row_number()) |>
    dplyr::select(.data$country_id, .data$ordering)
  
  order_levels <- ordering_tbl |>
    dplyr::arrange(.data$ordering) |>
    dplyr::pull(.data$country_id) |>
    as.character()
  
  df <- df |>
    dplyr::left_join(ordering_tbl, by = dplyr::join_by(country_id), relationship = "many-to-one") |>
    dplyr::mutate(country_id = factor(.data$country_id, levels = order_levels))
  
  # ---- Labels/colors ----------------------------------------------------
  dict <- data$dict %||% tibble::tibble()
  
  legend_labels <- build_indicator_labels(
    dict = dict,
    indicators = indicators,
    do_sec = sec$do_sec,
    indicators_sec = sec$indicators_sec
  )
  
  wrap_w <- compute_title_wrap_width(params$width %||% NA_real_)
  legend_wrap_w <- max(18, floor(wrap_w * 0.55))
  legend_labels_wrapped <- stringr::str_wrap(legend_labels, width = legend_wrap_w)
  
  fill_colors <- pick_fill_colors(style, indicators)
  
  # ---- Y-limits via policy ---------------------------------------------
  y_rng <- compute_y_rng_country(kind = graph_type, df = df)
  
  if (!all(is.finite(y_rng)) || length(y_rng) != 2 || y_rng[[1]] >= y_rng[[2]]) {
    warn_active("Bad y range from data; fallback to [0,1].")
    y_rng <- c(0, 1)
  }
  
  pol <- y_lim_policy(
    kind = graph_type,
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  
  # ---- Position policy --------------------------------------------------
  pos <- switch(
    graph_type,
    "bar_country_comparison"            = ggplot2::position_dodge(width = 0.70),
    "structure_country_comparison"      = "stack",
    "structure_country_comparison_norm" = "fill",
    ggplot2::position_dodge(width = 0.70)
  )
  
  # ---- Build plot -------------------------------------------------------
  palette <- style$palette %||% list()
  ann     <- style$annotations %||% list()
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$country_id, y = .data$value_plot))
  
  if (length(indicators) == 1) {
    # Single indicator: fill by role (country/peers/others) for readability
    st_country <- style_for(style, "country", "bar")
    st_peers   <- style_for(style, "peers",   "bar")
    st_others  <- style_for(style, "others",  "bar")
    
    role_cols <- c(
      country = st_country$fill,
      peers   = st_peers$fill,
      others  = st_others$fill
    )
    
    p <- p +
      ggplot2::geom_col(
        ggplot2::aes(fill = .data$role),
        width = 0.70,
        alpha = 0.85,
        position = "identity",
        color = NA
      ) +
      ggplot2::scale_fill_manual(values = role_cols, name = "")
  } else {
    # Multiple indicators: fill by indicator variable
    p <- p +
      ggplot2::geom_col(
        ggplot2::aes(fill = .data$variable),
        width = 0.70,
        alpha = (style_for(style, "country", "bar")$alpha %||% 0.85),
        position = pos,
        color = NA
      )
    
    if (!is.null(fill_colors)) {
      p <- p +
        ggplot2::scale_fill_manual(
          values = fill_colors,
          labels = legend_labels_wrapped,
          name   = ""
        )
    } else {
      p <- p +
        ggplot2::scale_fill_discrete(
          labels = legend_labels_wrapped,
          name   = ""
        )
    }
    
    # Legend rows: deterministic heuristic (as in barDynamic)
    plot_w <- suppressWarnings(as.numeric(params$width %||% NA_real_))
    if (!is.finite(plot_w) || plot_w <= 0) plot_w <- 8
    
    k <- length(indicators)
    legend_nrow <- dplyr::case_when(
      k <= 6 ~ 1L,
      plot_w >= 9 ~ 2L,
      TRUE ~ 3L
    )
    
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(nrow = legend_nrow, byrow = TRUE))
  }
  
  # Y scale + optional secondary axis
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) {
    warn_active("y_lim_policy produced non-finite/degenerate limits; fallback to data range.")
    ylim_vec <- y_rng
  }
  
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = sec$do_sec,
    coeff  = sec$coeff
  )
  
  # Zero line (style-driven)
  zero_cfg <- ann$zero_line %||% list(show = TRUE, color = palette$axis %||% "grey20", linewidth = 0.30, alpha = 0.60)
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 0,
        color = zero_cfg$color %||% (palette$axis %||% "grey20"),
        linewidth = (zero_cfg$linewidth %||% 0.30),
        alpha = (zero_cfg$alpha %||% 0.60)
      )
  }
  
  # X tick policy: legacy had angle=90 only when all==1; keep as compatibility tweak
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = if (all_flag == 1L) 90 else 0,
                                        hjust = if (all_flag == 1L) 1 else 0.5,
                                        vjust = if (all_flag == 1L) 0.5 else 1)
  )
  
  # Legend policy: if single indicator, hide legend (legacy)
  show_legend <- length(indicators) > 1
  
  # Structure plots: y label is typically blank (legacy)
  params2 <- params
  if (graph_type %in% c("structure_country_comparison", "structure_country_comparison_norm") &&
      length(indicators) > 1) {
    params2$y_lab <- ""
  }
  
  p <- finalize_plot_common(p, params2, style, legend = show_legend)
  
  out <- list(graph = p, data = df)
  
  out
}

structureCountryComparison <- barCountryComparison
structureCountryComparisonNorm <- barCountryComparison


###### Bar multi-year comparison for multiple variables
barYearComparison <- function(data,
                              graph_params,
                              country_iso2c,
                              peers_iso2c = NULL,
                              verbose = TRUE,
                              warn_invalid = TRUE,
                              debug = FALSE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble", "forcats", "purrr"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  # ---- helpers ---------------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("barYearComparison: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |> stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  normalize_int_vec <- function(x) {
    # robust to numeric / character / factors / already-int vectors
    if (is.character(x) && length(x) == 1L) {
      x <- x |>
        stringr::str_split(",") |>
        purrr::pluck(1) |>
        stringr::str_trim()
    }
    x <- suppressWarnings(as.integer(x %||% integer(0)))
    x <- x[is.finite(x)]
    sort(unique(x))
  }
  
  pick_fill_colors_years <- function(year_levels, style) {
    cat_pal <- style$palette$categorical %||% NULL
    if (is.character(cat_pal) && length(cat_pal) >= length(year_levels)) {
      return(unname(cat_pal[seq_along(year_levels)]))
    }
    acra_keys <- c(
      "green","sec2","dark","sec1","red","sec3","sec6","black","brown","sec5","sec7","sec8",
      "add1","add2","reddest","add4","add5","orange"
    )
    tryCatch(unname(as.vector(ACRA[acra_keys])[seq_len(length(year_levels))]),
             error = function(e) NULL
    )
  }
  
  build_x_labels_from_dict <- function(dict, indicators, freq) {
    out <- indicators
    if (is.data.frame(dict) && nrow(dict) > 0 &&
        all(c("indicator_code", "indicator") %in% names(dict))) {
      d <- tibble::as_tibble(dict) |>
        dplyr::filter(.data$indicator_code %in% indicators)
      
      if ("source_frequency" %in% names(d) && !is.na(freq) && nzchar(freq)) {
        d <- dplyr::filter(d, .data$source_frequency == freq)
      }
      
      d <- d |>
        dplyr::distinct(.data$indicator_code, .data$indicator) |>
        dplyr::mutate(indicator_code = factor(.data$indicator_code, levels = indicators)) |>
        dplyr::arrange(.data$indicator_code)
      
      if (nrow(d) == length(indicators)) out <- as.character(d$indicator)
    }
    out
  }
  
  # ---- meta ------------------------------------------------------------
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  years_fix <- normalize_int_vec(params$time_fix)   # expected: integer years set
  if (length(years_fix) == 0) {
    warn_active("time_fix (years) is empty/invalid; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No years in time_fix")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  miss <- setdiff(c("country_id", "country", "year"), names(ext))
  if (length(miss) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(miss, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  missing_inds <- setdiff(indicators, names(ext))
  if (length(missing_inds) > 0) {
    # This is the key guarantee: if an indicator isn't present, the plot cannot build it.
    warn_active(paste0("extdata has no columns for indicators: ", paste(missing_inds, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # ---- data ------------------------------------------------------------
  df_long <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id == country_iso2c, .data$year %in% years_fix) |>
    dplyr::select(dplyr::any_of(c("country", "country_id", "year")), dplyr::all_of(indicators)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(.data$year)),
      value_raw = suppressWarnings(as.numeric(.data$value_raw))
    ) |>
    dplyr::filter(is.finite(.data$year)) |>
    dplyr::mutate(
      variable = factor(.data$variable, levels = indicators),
      year     = factor(.data$year, levels = years_fix, ordered = TRUE)
    ) |>
    # Stabilize the "indicator × year" grid so dodge slots don't jump between indicators
    tidyr::complete(
      variable,
      year,
      fill = list(value_raw = NA_real_)
    )
  
  if (all(!is.finite(df_long$value_raw))) {
    warn_active("All values are NA/invalid for selected years; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No numeric data for selected years")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # ---- x labels --------------------------------------------------------
  freq <- normalize_freq(params$data_frequency %||% "y") %||% "y"
  dict <- data$dict %||% tibble::tibble()
  
  x_labels <- build_x_labels_from_dict(dict, indicators, freq)
  x_labels_wrapped <- stringr::str_wrap(x_labels, width = 15)
  
  # ---- y limits (viewport only) ---------------------------------------
  y_rng <- range(df_long$value_raw, na.rm = TRUE)
  if (!all(is.finite(y_rng)) || y_rng[[1]] >= y_rng[[2]]) y_rng <- c(0, 1)
  
  pol <- y_lim_policy(
    kind = "bar",
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) ylim_vec <- y_rng
  
  # ---- colors ----------------------------------------------------------
  fill_cols <- pick_fill_colors_years(levels(df_long$year), style)
  
  # ---- plot ------------------------------------------------------------
  st_bar <- style_for(style, role = "country", object = "bar")
  
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = .data$variable, y = .data$value_raw, fill = .data$year)
  ) +
    ggplot2::geom_col(
      alpha = st_bar$alpha %||% 0.85,
      width = 0.70,
      position = ggplot2::position_dodge(width = 0.75),
      color = NA,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_discrete(labels = x_labels_wrapped, drop = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  if (!is.null(fill_cols)) {
    p <- p + ggplot2::scale_fill_manual(values = fill_cols, name = "Год", drop = FALSE)
  } else {
    p <- p + ggplot2::scale_fill_discrete(name = "Год", drop = FALSE)
  }
  
  # critical: never drop bars with scale limits
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = FALSE,
    coeff  = NA_real_
  )
  
  # zero line (style-driven)
  palette <- style$palette %||% list()
  ann     <- style$annotations %||% list()
  zero_cfg <- ann$zero_line %||% list(
    show = TRUE,
    color = palette$axis %||% "grey20",
    linewidth = 0.30,
    alpha = 0.60
  )
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0,
      color = zero_cfg$color %||% (palette$axis %||% "grey20"),
      linewidth = zero_cfg$linewidth %||% 0.30,
      alpha = zero_cfg$alpha %||% 0.60
    )
  }
  
  # ---- finalize: suppress axis labels + suppress any time_fix label ----
  params2 <- params
  params2$x_lab <- ""
  params2$y_lab <- ""
  params2$time_fix_label <- NA_character_
  params2$time_fix_parts <- NULL
  
  p <- p + ggplot2::labs(x = NULL, y = NULL)
  
  p <- finalize_plot_common(p, params2, style, legend = TRUE)
  
  out <- list(graph = p, data = df_long)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      meta = tibble::tibble(
        n = nrow(df_long),
        years_fix = paste(years_fix, collapse = ","),
        year_levels = paste(levels(df_long$year), collapse = ","),
        indicator_levels = paste(levels(df_long$variable), collapse = ","),
        n_non_na = sum(is.finite(df_long$value_raw))
      )
    )
  }
  
  out
}


###### Lines indicator comparison

linesIndicatorComparison <- function(data,
                                     graph_params,
                                     country_iso2c,
                                     peers_iso2c = NULL,
                                     verbose = TRUE,
                                     warn_invalid = TRUE,
                                     debug = FALSE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble", "purrr"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  graph_type <- params$graph_type %||% "lines_indicator_comparison"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "lines_indicator_comparison"
  
  # ---- helpers ---------------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("linesIndicatorComparison: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |> stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  # Build breaks in "time" space, but labels from extdata calendar cols (robust to m/q tokens)
  build_time_breaks_labels <- function(ext_country, params, freq) {
    time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
    time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
    labfreq    <- suppressWarnings(as.numeric(params$labfreq %||% NA_real_))
    
    if (!is.finite(time_start) || !is.finite(time_end) || !is.finite(labfreq) || labfreq <= 0) {
      return(list(breaks = NULL, labels = NULL))
    }
    
    tts <- suppressWarnings(as.numeric(params$timetony_start %||% 0))
    tte <- suppressWarnings(as.numeric(params$timetony_end %||% 0))
    if (!is.finite(tts)) tts <- 0
    if (!is.finite(tte)) tte <- 0
    
    breaks <- seq(time_start + tts, time_end - tte + 1, by = labfreq)
    
    # Map time -> label using observed calendar columns
    cal_cols <- intersect(c("year", "quarter", "month"), names(ext_country))
    if (!("time" %in% names(ext_country)) || length(cal_cols) == 0) {
      return(list(breaks = breaks, labels = NULL))
    }
    
    map <- tibble::as_tibble(ext_country) |>
      dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month"))) |>
      dplyr::mutate(time = suppressWarnings(as.numeric(.data$time))) |>
      dplyr::filter(is.finite(.data$time), .data$time %in% breaks) |>
      dplyr::distinct(.data$time, .keep_all = TRUE) |>
      dplyr::arrange(.data$time)
    
    if (nrow(map) == 0) return(list(breaks = breaks, labels = NULL))
    
    fmt <- function(y, q, m, freq) {
      if (freq == "y") return(as.character(y))
      if (freq == "q") return(paste0(y, "Q", q))
      if (freq == "m") return(paste0(y, "-", stringr::str_pad(m, 2, pad = "0")))
      as.character(y)
    }
    
    labels <- purrr::pmap_chr(
      list(
        y = map$year %||% rep(NA_integer_, nrow(map)),
        q = map$quarter %||% rep(NA_integer_, nrow(map)),
        m = map$month %||% rep(NA_integer_, nrow(map))
      ),
      fmt,
      freq = freq
    )
    
    # align labels to breaks order (breaks may include time points not present in map)
    labels_full <- rep("", length(breaks))
    idx <- match(map$time, breaks)
    ok <- is.finite(idx)
    labels_full[idx[ok]] <- labels[ok]
    
    list(breaks = breaks, labels = labels_full)
  }
  
  pick_colors <- function(indicators, style) {
    cat_pal <- style$palette$categorical %||% NULL
    if (is.character(cat_pal) && length(cat_pal) >= length(indicators)) {
      return(unname(cat_pal[seq_along(indicators)]))
    }
    if (is.character(cat_pal) && !is.null(names(cat_pal))) {
      cols <- unname(cat_pal[indicators])
      if (!anyNA(cols)) return(cols)
    }
    
    acra_keys <- c(
      "green","sec2","dark","sec1","red","sec3","sec6","black","brown","sec5","sec7","sec8",
      "add1","add2","reddest","add4","add5","orange"
    )
    tryCatch(
      unname(as.vector(ACRA[acra_keys])[seq_len(length(indicators))]),
      error = function(e) NULL
    )
  }
  
  # ---- logging ---------------------------------------------------------
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_base <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty indicators"))
    return(out)
  }
  
  missing_inds <- setdiff(indicators, names(ext))
  if (length(missing_inds) > 0) {
    warn_active(paste0("extdata has no columns for indicators: ", paste(missing_inds, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing indicator cols"))
    return(out)
  }
  
  time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
  time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
  if (!is.finite(time_start) || !is.finite(time_end) || time_start > time_end) {
    warn_active("time_start/time_end are not valid; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No time window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "bad time window"))
    return(out)
  }
  
  freq <- normalize_freq(params$data_frequency %||% NA_character_) %||% "y"
  
  # ---- long data -------------------------------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::select(
      dplyr::any_of(c("country", "country_id", "year", "quarter", "month", "time")),
      dplyr::all_of(indicators)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      time      = suppressWarnings(as.numeric(.data$time)),
      value_raw = suppressWarnings(as.numeric(.data$value_raw))
    ) |>
    dplyr::filter(
      .data$country_id == country_iso2c,
      is.finite(.data$time),
      .data$time >= time_start,
      .data$time <= time_end
    ) |>
    dplyr::mutate(variable = factor(.data$variable, levels = indicators))
  
  if (nrow(df0) == 0) {
    warn_active("No rows after filtering by window/country; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data in window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "no rows after filter"))
    return(out)
  }
  
  # Warn explicitly if some indicators have no finite points in the window
  present_by_var <- df0 |>
    dplyr::summarise(has_point = any(is.finite(.data$value_raw)), .by = .data$variable)
  vars_no_points <- present_by_var |>
    dplyr::filter(!.data$has_point) |>
    dplyr::pull(.data$variable) |>
    as.character()
  
  if (length(vars_no_points) > 0) {
    warn_active(paste0(
      "Indicators in plan but no numeric points in the selected window: ",
      paste(vars_no_points, collapse = ", ")
    ))
  }
  
  df0 <- df0 |>
    dplyr::filter(is.finite(.data$value_raw))
  
  do_index <- isTRUE(suppressWarnings(as.integer(params$index %||% 0L)) == 1L)
  
  if (isTRUE(do_index)) {
    bases <- df0 |>
      dplyr::arrange(.data$variable, .data$time) |>
      dplyr::summarise(base_value = dplyr::first(.data$value_raw), .by = .data$variable)
    
    bad_base <- bases |>
      dplyr::filter(!is.finite(.data$base_value) | .data$base_value == 0) |>
      dplyr::pull(.data$variable) |>
      as.character()
    
    if (length(bad_base) > 0) {
      warn_active(paste0(
        "index=1: cannot compute base (NA/0) for indicators: ",
        paste(bad_base, collapse = ", ")
      ))
    }
    
    df0 <- df0 |>
      dplyr::left_join(bases, by = dplyr::join_by(variable)) |>
      dplyr::mutate(
        value_raw = dplyr::if_else(
          is.finite(.data$base_value) & .data$base_value != 0,
          .data$value_raw / .data$base_value * 100,
          NA_real_
        )
      ) |>
      dplyr::select(-.data$base_value) |>
      dplyr::filter(is.finite(.data$value_raw))
    
    if (nrow(df0) == 0) {
      warn_active("index=1: all series became empty after indexing (base NA/0).")
      p0 <- make_placeholder_plot(params, style, "No numeric data after indexing")
      out <- list(graph = p0, data = tibble::tibble())
      if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "index produced empty df"))
      return(out)
    }
  }
  
  if (nrow(df0) == 0) {
    warn_active("All values are NA/invalid in window; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No numeric data in window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "no numeric points"))
    return(out)
  }
  
  # ---- secondary axis (strict rule) -----------------------------------
  coeff <- suppressWarnings(as.numeric(params$coeff %||% NA_real_))
  
  indicators_sec_raw <- normalize_chr_vec(params$indicators_sec)
  indicators_sec <- intersect(indicators_sec_raw, indicators)
  
  do_sec <- length(indicators_sec) > 0 && is.finite(coeff) && coeff != 0
  if (!isTRUE(do_sec) && length(indicators_sec_raw) > 0) {
    warn_active("Secondary axis requested but disabled: check indicators_sec overlap and coeff.")
  }
  
  df <- df0 |>
    dplyr::mutate(
      value_plot = dplyr::if_else(
        do_sec & .data$variable %in% indicators_sec,
        .data$value_raw * coeff,
        .data$value_raw
      )
    )
  
  # ---- labels (dict) ---------------------------------------------------
  dict <- data$dict %||% tibble::tibble()
  legend_labels <- indicators
  
  if (is.data.frame(dict) &&
      nrow(dict) > 0 &&
      all(c("indicator_code", "indicator") %in% names(dict))) {
    
    map <- tibble::as_tibble(dict) |>
      dplyr::filter(.data$indicator_code %in% indicators)
    
    if ("source_frequency" %in% names(map) && !is.na(freq) && nzchar(freq)) {
      map <- map |> dplyr::filter(.data$source_frequency == freq)
    }
    
    map <- map |>
      dplyr::distinct(.data$indicator_code, .data$indicator) |>
      dplyr::mutate(indicator_code = factor(.data$indicator_code, levels = indicators)) |>
      dplyr::arrange(.data$indicator_code)
    
    if (nrow(map) == length(indicators)) legend_labels <- as.character(map$indicator)
  }
  
  if (isTRUE(do_sec) && length(indicators_sec) > 0) {
    is_sec <- indicators %in% indicators_sec
    legend_labels[is_sec]  <- paste0(legend_labels[is_sec],  ", rha")
    legend_labels[!is_sec] <- paste0(legend_labels[!is_sec], ", lha")
  }
  
  wrap_w <- compute_title_wrap_width(params$width %||% NA_real_)
  legend_wrap_w <- max(18, floor(wrap_w * 0.55))
  legend_labels_wrapped <- stringr::str_wrap(legend_labels, width = legend_wrap_w)
  
  # ---- y limits via policy --------------------------------------------
  y_rng <- range(df$value_plot, na.rm = TRUE)
  if (!all(is.finite(y_rng)) || y_rng[[1]] >= y_rng[[2]]) {
    warn_active("Bad y range from data; fallback to [0,1].")
    y_rng <- c(0, 1)
  }
  
  pol <- y_lim_policy(
    kind = graph_type,
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) ylim_vec <- y_rng
  
  # ---- x ticks (robust labels for m/q/y) -------------------------------
  ext_country <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id == country_iso2c)
  
  xt <- build_time_breaks_labels(ext_country, params, freq)
  
  # ---- plot ------------------------------------------------------------
  st_line <- style_for(style, role = "country", object = "line")
  cols <- pick_colors(indicators, style)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$value_plot, color = .data$variable)) +
    ggplot2::geom_line(
      linewidth = st_line$linewidth %||% 1.1,
      alpha     = st_line$alpha %||% 0.95
    )
  
  if (!is.null(xt$breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = xt$breaks, labels = xt$labels)
  }
  
  if (!is.null(cols)) {
    p <- p + ggplot2::scale_color_manual(values = cols, labels = legend_labels_wrapped, drop = FALSE)
  } else {
    p <- p + ggplot2::scale_color_discrete(labels = legend_labels_wrapped, drop = FALSE)
  }
  
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = do_sec,
    coeff  = if (isTRUE(do_sec)) coeff else NA_real_
  )
  
  # zero line (style-driven)
  palette <- style$palette %||% list()
  ann     <- style$annotations %||% list()
  zero_cfg <- ann$zero_line %||% list(
    show = TRUE,
    color = palette$axis %||% "grey20",
    linewidth = 0.30,
    alpha = 0.60
  )
  
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0,
      color = zero_cfg$color %||% (palette$axis %||% "grey20"),
      linewidth = zero_cfg$linewidth %||% 0.30,
      alpha = zero_cfg$alpha %||% 0.60
    )
  }
  
  # ---- optional direct labels -----------------------------------------
  use_directlabels <- isTRUE(params$use_directlabels %||% TRUE)
  show_legend <- TRUE
  
  if (isTRUE(use_directlabels) &&
      nlevels(df$variable) <= 7 &&
      requireNamespace("directlabels", quietly = TRUE)) {
    
    map_lbl <- tibble::tibble(
      variable = factor(indicators, levels = indicators),
      indicator = legend_labels_wrapped
    )
    
    df_lbl <- df |>
      dplyr::left_join(map_lbl, by = "variable")
    
    p <- p +
      directlabels::geom_dl(
        data = df_lbl,
        ggplot2::aes(label = .data$indicator),
        method = list(directlabels::dl.trans(x = x + 0.3), "extreme.grid", cex = 0.8, alpha = 0.8)
      )
    
    show_legend <- FALSE
  }
  
  # ---- finalize --------------------------------------------------------
  params2 <- params
  params2$x_lab <- ""
  params2$y_lab <- ""
  
  p <- finalize_plot_common(p, params2, style, legend = show_legend)
  
  out <- list(graph = p, data = df)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      meta = tibble::tibble(
        n = nrow(df),
        indicators = paste(indicators, collapse = ","),
        missing_indicator_points = paste(vars_no_points, collapse = ","),
        do_sec = do_sec,
        coeff = coeff,
        time_start = time_start,
        time_end = time_end,
        freq = freq
      )
    )
  }
  
  out
}


###### Lines country comparison

linesCountryComparison <- function(data,
                                   graph_params,
                                   country_iso2c,
                                   peers_iso2c = NULL,
                                   verbose = TRUE,
                                   warn_invalid = TRUE,
                                   debug = FALSE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble", "purrr"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  graph_type <- params$graph_type %||% "lines_country_comparison"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "lines_country_comparison"
  
  # ---- helpers ---------------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("linesCountryComparison: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |> stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  build_time_breaks_labels <- function(ext_subset, params, freq) {
    time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
    time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
    labfreq    <- suppressWarnings(as.numeric(params$labfreq %||% NA_real_))
    
    if (!is.finite(time_start) || !is.finite(time_end) || !is.finite(labfreq) || labfreq <= 0) {
      return(list(breaks = NULL, labels = NULL))
    }
    
    tts <- suppressWarnings(as.numeric(params$timetony_start %||% 0))
    tte <- suppressWarnings(as.numeric(params$timetony_end %||% 0))
    if (!is.finite(tts)) tts <- 0
    if (!is.finite(tte)) tte <- 0
    
    breaks <- seq(time_start + tts, time_end - tte + 1, by = labfreq)
    
    if (!("time" %in% names(ext_subset))) return(list(breaks = breaks, labels = NULL))
    
    map <- tibble::as_tibble(ext_subset) |>
      dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month"))) |>
      dplyr::mutate(time = suppressWarnings(as.numeric(.data$time))) |>
      dplyr::filter(is.finite(.data$time), .data$time %in% breaks) |>
      dplyr::distinct(.data$time, .keep_all = TRUE) |>
      dplyr::arrange(.data$time)
    
    if (nrow(map) == 0) return(list(breaks = breaks, labels = NULL))
    
    fmt_one <- function(y, q, m, freq) {
      if (freq == "y") return(as.character(y))
      if (freq == "q") return(paste0(y, "Q", q))
      if (freq == "m") return(paste0(y, "-", stringr::str_pad(m, 2, pad = "0")))
      as.character(y)
    }
    
    labels <- purrr::pmap_chr(
      list(
        y = map$year %||% rep(NA_integer_, nrow(map)),
        q = map$quarter %||% rep(NA_integer_, nrow(map)),
        m = map$month %||% rep(NA_integer_, nrow(map))
      ),
      fmt_one,
      freq = freq
    )
    
    labels_full <- rep("", length(breaks))
    idx <- match(map$time, breaks)
    ok <- is.finite(idx)
    labels_full[idx[ok]] <- labels[ok]
    
    list(breaks = breaks, labels = labels_full)
  }
  
  # ---- params-first routing -------------------------------------------
  country_id <- params$country_iso2c %||% country_iso2c
  country_id <- normalize_chr_vec(country_id)
  country_id <- if (length(country_id) >= 1) country_id[[1]] else NA_character_
  
  peers_vec <- params$peers_iso2c %||% peers_iso2c
  peers_vec <- normalize_chr_vec(peers_vec)
  
  # deterministic country order: target first, then peers (excluding duplicates)
  country_set <- unique(c(country_id, peers_vec))
  country_set <- country_set[!is.na(country_set) & country_set != ""]
  
  # ---- logging ---------------------------------------------------------
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_base <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  if (length(country_set) == 0) {
    warn_active("No countries resolved from params$country_iso2c + params$peers_iso2c.")
    p0 <- make_placeholder_plot(params, style, "No countries")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty indicators"))
    return(out)
  }
  
  missing_inds <- setdiff(indicators, names(ext))
  if (length(missing_inds) > 0) {
    warn_active(paste0("extdata has no columns for indicators: ", paste(missing_inds, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing indicator cols"))
    return(out)
  }
  
  time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
  time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
  if (!is.finite(time_start) || !is.finite(time_end) || time_start > time_end) {
    warn_active("time_start/time_end are not valid; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No time window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "bad time window"))
    return(out)
  }
  
  freq <- normalize_freq(params$data_frequency %||% NA_character_) %||% "y"
  
  # ---- y label (single-indicator default) -------------------------------
  resolve_indicator_label <- function(dict, indicator_code, freq) {
    if (!is.data.frame(dict) || nrow(dict) == 0) return(indicator_code)
    
    if (!all(c("indicator_code", "indicator") %in% names(dict))) return(indicator_code)
    
    map <- tibble::as_tibble(dict) |>
      dplyr::filter(.data$indicator_code == indicator_code)
    
    if ("source_frequency" %in% names(map) && !is.na(freq) && nzchar(freq)) {
      map <- map |> dplyr::filter(.data$source_frequency == freq)
    }
    
    lbl <- map |>
      dplyr::filter(!is.na(.data$indicator) & .data$indicator != "") |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(.data$indicator)
    
    if (length(lbl) == 1 && nzchar(lbl)) lbl else indicator_code
  }
  
  # ---- long data -------------------------------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id %in% country_set) |>
    dplyr::select(
      dplyr::any_of(c("country", "country_id", "year", "quarter", "month", "time")),
      dplyr::all_of(indicators)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      time      = suppressWarnings(as.numeric(.data$time)),
      value_raw = suppressWarnings(as.numeric(.data$value_raw))
    ) |>
    dplyr::filter(
      is.finite(.data$time),
      .data$time >= time_start,
      .data$time <= time_end
    )
  
  if (nrow(df0) == 0) {
    warn_active("No rows after filtering by window/countries; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data in window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "no rows after filter"))
    return(out)
  }
  
  # warn: series with no numeric points
  present_ci <- df0 |>
    dplyr::summarise(has_point = any(is.finite(.data$value_raw)),
                     .by = c(.data$country_id, .data$variable))
  
  bad_pairs <- present_ci |>
    dplyr::filter(!.data$has_point) |>
    dplyr::mutate(pair = paste0(.data$country_id, ":", .data$variable)) |>
    dplyr::pull(.data$pair)
  
  if (length(bad_pairs) > 0) {
    warn_active(paste0(
      "Some country-indicator series have no numeric points in window: ",
      paste(bad_pairs, collapse = ", ")
    ))
  }
  
  df0 <- df0 |>
    dplyr::filter(is.finite(.data$value_raw))
  
  if (nrow(df0) == 0) {
    warn_active("All values are NA/invalid in window; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No numeric data in window")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "no numeric points"))
    return(out)
  }
  
  df0 <- df0 |>
    dplyr::mutate(
      variable   = factor(.data$variable, levels = indicators),
      country_id = factor(.data$country_id, levels = country_set)
    )
  
  # ---- optional indexing to 100 at first point -------------------------
  do_index <- isTRUE(suppressWarnings(as.integer(params$index %||% 0L)) == 1L)
  
  if (isTRUE(do_index)) {
    bases <- df0 |>
      dplyr::arrange(.data$country_id, .data$variable, .data$time) |>
      dplyr::summarise(base_value = dplyr::first(.data$value_raw), .by = c(.data$country_id, .data$variable))
    
    bad_base <- bases |>
      dplyr::filter(!is.finite(.data$base_value) | .data$base_value == 0) |>
      dplyr::mutate(pair = paste0(.data$country_id, ":", .data$variable)) |>
      dplyr::pull(.data$pair)
    
    if (length(bad_base) > 0) {
      warn_active(paste0(
        "index=1: cannot compute base (NA/0) for series: ",
        paste(bad_base, collapse = ", ")
      ))
    }
    
    df0 <- df0 |>
      dplyr::left_join(bases, by = dplyr::join_by(country_id, variable)) |>
      dplyr::mutate(
        value_raw = dplyr::if_else(
          is.finite(.data$base_value) & .data$base_value != 0,
          .data$value_raw / .data$base_value * 100,
          NA_real_
        )
      ) |>
      dplyr::select(-.data$base_value) |>
      dplyr::filter(is.finite(.data$value_raw))
    
    if (nrow(df0) == 0) {
      warn_active("index=1: all series became empty after indexing (base NA/0).")
      p0 <- make_placeholder_plot(params, style, "No numeric data after indexing")
      out <- list(graph = p0, data = tibble::tibble())
      if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "index produced empty df"))
      return(out)
    }
  }
  
  # ---- secondary axis (strict rule) -----------------------------------
  coeff <- suppressWarnings(as.numeric(params$coeff %||% NA_real_))
  indicators_sec_raw <- normalize_chr_vec(params$indicators_sec)
  indicators_sec <- intersect(indicators_sec_raw, indicators)
  
  do_sec <- length(indicators_sec) > 0 && is.finite(coeff) && coeff != 0
  if (!isTRUE(do_sec) && length(indicators_sec_raw) > 0) {
    warn_active("Secondary axis requested but disabled: check indicators_sec overlap and coeff.")
  }
  
  df <- df0 |>
    dplyr::mutate(
      value_plot = dplyr::if_else(
        do_sec & .data$variable %in% indicators_sec,
        .data$value_raw * coeff,
        .data$value_raw
      )
    )
  
  # ---- y limits via policy --------------------------------------------
  y_rng <- range(df$value_plot, na.rm = TRUE)
  if (!all(is.finite(y_rng)) || y_rng[[1]] >= y_rng[[2]]) {
    warn_active("Bad y range from data; fallback to [0,1].")
    y_rng <- c(0, 1)
  }
  
  pol <- y_lim_policy(
    kind = graph_type,
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) ylim_vec <- y_rng
  
  # ---- x ticks ---------------------------------------------------------
  ext_one <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id == country_id)
  
  if (nrow(ext_one) == 0) {
    ext_one <- tibble::as_tibble(ext) |>
      dplyr::filter(.data$country_id %in% country_set) |>
      dplyr::slice_head(n = 5000)
  }
  
  xt <- build_time_breaks_labels(ext_one, params, freq)
  
  # ---- style: main vs others ------------------------------------------
  main_id <- country_id
  df <- df |>
    dplyr::mutate(is_main = (.data$country_id == main_id))
  
  # deterministic linetypes for non-main
  lt_pool <- c("dashed", "dotted", "dotdash", "longdash", "twodash")
  others <- country_set[country_set != main_id]
  
  lt_map <- stats::setNames(
    c("solid", rep(lt_pool, length.out = length(others))),
    c(main_id, others)
  )
  
  # colors/widths from style where possible
  st_line <- style_for(style, role = "country", object = "line")
  palette <- style$palette %||% list()
  
  # main line color: prefer role-based style if provided
  main_col <- st_line$color %||% palette$country %||% palette$accent %||% palette$text %||% "grey10"
  
  # peers are always grey
  peer_col <- palette$muted_text %||% "grey65"
  
  main_lwd   <- st_line$linewidth_main %||% (st_line$linewidth %||% 1.2)
  peer_lwd   <- st_line$linewidth_peer %||% max(0.45, (st_line$linewidth %||% 1.2) * 0.55)
  main_alpha <- st_line$alpha_main %||% (st_line$alpha %||% 0.95)
  peer_alpha <- st_line$alpha_peer %||% 0.85
  
  # text colors (theme-driven defaults)
  text_col_main <- palette$text %||% "grey10"
  text_col_peer <- palette$muted_text %||% "grey55"
  
  # ---- labels at start/end (ISO2) -------------------------------------
  dx <- (time_end - time_start) * 0.01
  if (!is.finite(dx) || dx == 0) dx <- 0.25
  
  df_lbl_start <- df |>
    dplyr::arrange(.data$country_id, .data$variable, .data$time) |>
    dplyr::summarise(
      time = dplyr::first(.data$time),
      y    = dplyr::first(.data$value_plot),
      .by  = c(.data$country_id, .data$variable, .data$is_main)
    ) |>
    dplyr::mutate(time = .data$time - dx, label = as.character(.data$country_id))
  
  df_lbl_end <- df |>
    dplyr::arrange(.data$country_id, .data$variable, .data$time) |>
    dplyr::summarise(
      time = dplyr::last(.data$time),
      y    = dplyr::last(.data$value_plot),
      .by  = c(.data$country_id, .data$variable, .data$is_main)
    ) |>
    dplyr::mutate(time = .data$time + dx, label = as.character(.data$country_id))
  
  # ---- plot ------------------------------------------------------------
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$time, y = .data$value_plot)) +
    # peers: thin grey with varying linetype
    ggplot2::geom_line(
      data = df |> dplyr::filter(!.data$is_main),
      ggplot2::aes(
        linetype = .data$country_id,
        group = interaction(.data$country_id, .data$variable, drop = TRUE)
      ),
      color = peer_col,
      linewidth = peer_lwd,
      alpha = peer_alpha,
      show.legend = FALSE
    ) +
    # main: thick colored solid
    ggplot2::geom_line(
      data = df |> dplyr::filter(.data$is_main),
      ggplot2::aes(group = interaction(.data$country_id, .data$variable, drop = TRUE)),
      color = main_col,
      linewidth = main_lwd,
      alpha = main_alpha,
      show.legend = FALSE
    ) +
    ggplot2::scale_linetype_manual(values = lt_map, drop = FALSE)
  
  if (!is.null(xt$breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = xt$breaks, labels = xt$labels)
  }
  
  # Multiple indicators: facet stays (labels are per facet thanks to grouping by variable)
  if (length(indicators) > 1) {
    dict <- data$dict %||% tibble::tibble()
    indicator_labels <- indicators
    
    if (is.data.frame(dict) &&
        nrow(dict) > 0 &&
        all(c("indicator_code", "indicator") %in% names(dict))) {
      map <- tibble::as_tibble(dict) |>
        dplyr::filter(.data$indicator_code %in% indicators)
      
      if ("source_frequency" %in% names(map) && !is.na(freq) && nzchar(freq)) {
        map <- map |> dplyr::filter(.data$source_frequency == freq)
      }
      
      map <- map |>
        dplyr::distinct(.data$indicator_code, .data$indicator) |>
        dplyr::mutate(indicator_code = factor(.data$indicator_code, levels = indicators)) |>
        dplyr::arrange(.data$indicator_code)
      
      if (nrow(map) == length(indicators)) indicator_labels <- as.character(map$indicator)
    }
    
    facet_labels <- indicator_labels
    names(facet_labels) <- indicators
    
    p <- p + ggplot2::facet_wrap(
      ~ variable,
      ncol = 1,
      scales = "free_y",
      labeller = ggplot2::labeller(variable = facet_labels)
    )
  }
  
  # y-limits viewport (+ optional secondary axis)
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = do_sec,
    coeff  = if (isTRUE(do_sec)) coeff else NA_real_
  )
  
  # ISO2 labels (two layers to avoid vectorized color issues)
  p <- p +
    ggplot2::geom_text(
      data = df_lbl_start |> dplyr::filter(!.data$is_main),
      ggplot2::aes(x = .data$time, y = .data$y, label = .data$label),
      hjust = 1, vjust = 0.5,
      color = text_col_peer,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = df_lbl_start |> dplyr::filter(.data$is_main),
      ggplot2::aes(x = .data$time, y = .data$y, label = .data$label),
      hjust = 1, vjust = 0.5,
      color = text_col_main,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = df_lbl_end |> dplyr::filter(!.data$is_main),
      ggplot2::aes(x = .data$time, y = .data$y, label = .data$label),
      hjust = 0, vjust = 0.5,
      color = text_col_peer,
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = df_lbl_end |> dplyr::filter(.data$is_main),
      ggplot2::aes(x = .data$time, y = .data$y, label = .data$label),
      hjust = 0, vjust = 0.5,
      color = text_col_main,
      size = 3.2,
      show.legend = FALSE
    )
  
  # zero line (style-driven)
  ann      <- style$annotations %||% list()
  zero_cfg <- ann$zero_line %||% list(
    show = TRUE,
    color = palette$axis %||% "grey20",
    linewidth = 0.30,
    alpha = 0.60
  )
  
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0,
      color = zero_cfg$color %||% (palette$axis %||% "grey20"),
      linewidth = zero_cfg$linewidth %||% 0.30,
      alpha = zero_cfg$alpha %||% 0.60
    )
  }
  
  # ---- finalize --------------------------------------------------------
  params2 <- params
  params2$country_iso2c <- country_id
  params2$peers_iso2c <- peers_vec
  params2$x_lab <- ""
  params2$coord_clip <- "off"
  
  # Prefer y_lab from fillGraphPlan(); derive only if it's empty AND exactly one indicator
  y_lab_from_plan <- stringr::str_trim(as.character(params$y_lab %||% ""))
  if (nzchar(y_lab_from_plan)) {
    params2$y_lab <- y_lab_from_plan
  } else if (length(indicators) == 1) {
    dict <- data$dict %||% tibble::tibble()
    ind1 <- indicators[[1]]
    params2$y_lab <- resolve_indicator_label(dict, ind1, freq)
  } else {
    params2$y_lab <- ""
  }
  
  p <- finalize_plot_common(p, params2, style, legend = FALSE)
  
  out <- list(graph = p, data = df)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      meta = tibble::tibble(
        n = nrow(df),
        countries = paste(country_set, collapse = ","),
        indicators = paste(indicators, collapse = ","),
        do_index = do_index,
        do_sec = do_sec,
        coeff = coeff,
        time_start = time_start,
        time_end = time_end,
        freq = freq
      )
    )
  }
  
  out
}


###### Distribution fix (horizontal across-country density estimate for the fixed time period)

densityFix <- function(data, graph_params,
                                 country_iso2c,
                                 peers_iso2c = NULL,
                                 verbose = TRUE,
                                 warn_invalid = TRUE,
                                 debug = FALSE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  graph_type <- params$graph_type %||% "density_fix"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "density_fix"
  
  # ---- helpers ---------------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("densityFix: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |> stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  # ---- logging ---------------------------------------------------------
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- params-first routing -------------------------------------------
  country_id <- params$country_iso2c %||% country_iso2c
  country_id <- normalize_chr_vec(country_id)
  country_id <- if (length(country_id) >= 1) country_id[[1]] else NA_character_
  
  peers_vec <- params$peers_iso2c %||% peers_iso2c
  peers_vec <- normalize_chr_vec(peers_vec)
  
  # deterministic: main first, then peers (unique)
  peers_vec <- setdiff(peers_vec, country_id)
  country_set <- c(country_id, peers_vec)
  country_set <- country_set[!is.na(country_set) & country_set != ""]
  
  # ---- guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  needed_base <- c("country_id", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # densityFix is conceptually single-indicator (one distribution)
  if (length(indicators) > 1) {
    warn_active(paste0(
      "Multiple indicators provided (", length(indicators),
      "); densityFix uses only the first one: ", indicators[[1]]
    ))
    indicators <- indicators[[1]]
  }
  
  if (!isTRUE(indicators %in% names(ext))) {
    warn_active(paste0("extdata has no column for indicator: ", indicators))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator column)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  time_fix <- suppressWarnings(as.numeric(params$time_fix %||% NA_real_))
  if (!is.finite(time_fix)) {
    # safe fallback: if user didn't set time_fix, try time_end (works for many plans)
    time_fix <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
  }
  if (!is.finite(time_fix)) {
    warn_active("time_fix is not valid (and no fallback time_end); returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No time_fix")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # if peers not specified, we can still draw distribution for all countries,
  # but markers for peers will be empty
  use_all_countries <- isTRUE(is_true01(params$use_all_countries %||% 1L))
  
  # ---- data: cross-section at time_fix --------------------------------
  df0 <- tibble::as_tibble(ext) |>
    dplyr::select(dplyr::any_of(c("country_id", "country", "time")), dplyr::all_of(indicators)) |>
    dplyr::mutate(
      time = suppressWarnings(as.numeric(.data$time)),
      value_raw = suppressWarnings(as.numeric(.data[[indicators]])),
      country_id = as.character(.data$country_id)
    ) |>
    dplyr::filter(is.finite(.data$time), .data$time == time_fix, is.finite(.data$value_raw))
  
  if (nrow(df0) == 0) {
    warn_active("No numeric rows at time_fix; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data at time_fix")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  df_dist <- if (isTRUE(use_all_countries)) {
    df0
  } else if (length(country_set) > 0) {
    df0 |> dplyr::filter(.data$country_id %in% country_set)
  } else {
    df0
  }
  
  if (nrow(df_dist) < 5) {
    warn_active("Too few points for density/hist; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "Too few observations")
    return(list(graph = p0, data = df_dist))
  }
  
  # marker data (main + peers)
  mark_set <- unique(c(country_id, peers_vec))
  mark_set <- mark_set[!is.na(mark_set) & mark_set != ""]
  
  df_mark <- df0 |>
    dplyr::mutate(country_id = as.character(.data$country_id)) |>
    dplyr::filter(.data$country_id %in% mark_set) |>
    dplyr::mutate(
      role = dplyr::case_when(
        .data$country_id == .env$country_id ~ "country",
        .data$country_id %in% .env$peers_vec ~ "peers",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$role)) |>
    dplyr::select(.data$country_id, .data$value_raw, .data$role)
  
  # ---- compute a stable y-scale for marker heights ---------------------
  dens <- tryCatch(
    stats::density(df_dist$value_raw, na.rm = TRUE),
    error = function(e) NULL
  )
  y_max <- if (!is.null(dens) && length(dens$y) > 0) max(dens$y, na.rm = TRUE) else NA_real_
  if (!is.finite(y_max) || y_max <= 0) y_max <- 1
  
  y_main  <- y_max * 1.03
  y_peers <- y_max * 0.65
  
  df_mark <- df_mark |>
    dplyr::mutate(
      y_end = dplyr::if_else(.data$role == "country", y_main, y_peers),
      y_lab = dplyr::if_else(.data$role == "country", y_main, y_peers)
    )
  
  # ---- plot ------------------------------------------------------------
  palette <- style$palette %||% list()
  
  st_country <- style_for(style, role = "country", object = "line")
  st_peers   <- style_for(style, role = "peers",   object = "line")
  
  col_country <- st_country$color %||% palette$country %||% palette$accent %||% "grey10"
  col_peers   <- st_peers$color   %||% palette$peers   %||% palette$muted_text %||% "grey60"
  
  # background distribution styling
  st_area <- style_for(style, role = "others", object = "area")
  fill_dist <- st_area$fill %||% palette$panel_bg %||% "grey90"
  col_dist  <- st_area$color %||% palette$grid_major %||% "grey80"
  
  show_hist    <- is_true01(params$show_hist %||% 1L)
  show_density <- is_true01(params$show_density %||% 1L)
  
  # --- bins policy --------------------------------

  x <- df_dist$value_raw
  x <- x[is.finite(x)]
  n <- length(x)
  
  min_bins_center <- 15L
  target_bins_center <- 25L  # фикс, можно потом тюнить
  max_bins_center <- 60L     # анти-"расческа" (слишком много бинов)
  
  if (n >= 10L) {
    q <- stats::quantile(x, probs = c(0.05, 0.95), na.rm = TRUE, names = FALSE, type = 7)
    x05 <- as.numeric(q[[1]])
    x95 <- as.numeric(q[[2]])
    central_width <- x95 - x05
    
    bw_center_target <- if (is.finite(central_width) && central_width > 0) {
      central_width / target_bins_center
    } else {
      NA_real_
    }
    
    iqr <- stats::IQR(x, na.rm = TRUE, type = 7)
    bw_fd <- if (is.finite(iqr) && iqr > 0) {
      2 * iqr / (n^(1/3))
    } else {
      NA_real_
    }
    
    # robust choice: no Scott(sd)
    bw <- max(c(bw_center_target, bw_fd), na.rm = TRUE)
    
    # fallback if something went wrong
    if (!is.finite(bw) || bw <= 0) {
      bw <- if (is.finite(central_width) && central_width > 0) central_width / min_bins_center else 1
    }
    
    # anti-comb: limit max bins in central 90%
    if (is.finite(central_width) && central_width > 0) {
      bw_floor <- central_width / max_bins_center
      bw <- max(bw, bw_floor)
    }
  } else {
    # very small n: keep simple & stable
    bw <- 1
  }
  
  # base plot in x-space of indicator
  p <- ggplot2::ggplot(df_dist, ggplot2::aes(x = .data$value_raw))
  
  if (isTRUE(show_hist)) {
    p <- p + ggplot2::geom_histogram(
      ggplot2::aes(y = after_stat(density)),
      binwidth = bw,
      boundary = x05, 
      closed = "left",
      fill = fill_dist,
      color = col_dist,
      linewidth = 0.25,
      alpha = 0.55,
      show.legend = FALSE
    )
  }
  
  if (isTRUE(show_density)) {
    p <- p + ggplot2::geom_density(
      linewidth = 0.9,
      color = col_dist,
      alpha = 0,
      show.legend = FALSE
    )
  }
  
  # ---- markers: vertical segments + ISO labels -------------------------
  if (nrow(df_mark) > 0) {
    df_mark <- df_mark |>
      dplyr::mutate(
        role = factor(.data$role, levels = c("peers", "country"))
      )
    
    p <- p +
      ggplot2::geom_segment(
        data = df_mark,
        ggplot2::aes(
          x = .data$value_raw,
          xend = .data$value_raw,
          y = 0,
          yend = .data$y_end,
          color = .data$role,
          linewidth = .data$role
        ),
        lineend = "round",
        show.legend = FALSE
      ) +
      ggplot2::geom_text(
        data = df_mark,
        ggplot2::aes(
          x = .data$value_raw,
          y = .data$y_lab,
          label = .data$country_id,
          color = .data$role
        ),
        vjust = -0.2,
        size = 3.2,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        values = c(peers = col_peers, country = col_country),
        guide = "none"
      ) +
      ggplot2::scale_linewidth_manual(
        values = c(peers = 0.55, country = 1.25),
        guide = "none"
      )
  }
  
  # ---- y-limits via policy --------------------------------------------
  # For densityFix y-axis is density; compute viewport range from data
  y_rng <- c(0, y_main * 1.10)
  pol <- y_lim_policy(
    kind = graph_type,
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) ylim_vec <- y_rng
  
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = FALSE,
    coeff  = NA_real_
  )
  
  # ---- finalize --------------------------------------------------------
  params2 <- params
  params2$country_iso2c <- country_id
  params2$peers_iso2c   <- peers_vec
  # density_fix is allowed to show x label by your rule, so don't blank it here.
  # y_lab тоже не трогаем: пусть управляется планом / derive_axis_labels.
  
  p <- finalize_plot_common(p, params2, style, legend = FALSE)
  
  out <- list(graph = p, data = df_dist)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      meta = tibble::tibble(
        indicator = indicators,
        time_fix = time_fix,
        n_dist = nrow(df_dist),
        n_mark = nrow(df_mark),
        bins = bins,
        show_hist = show_hist,
        show_density = show_density
      ),
      marks = df_mark
    )
  }
  
  out
}


###### Distribution dynamics (fan plot)

distributionDynamic <- function(data,
                                graph_params,
                                country_iso2c,
                                peers_iso2c = NULL,
                                verbose = TRUE,
                                warn_invalid = TRUE,
                                debug = TRUE) {
  assert_packages(c("dplyr", "tidyr", "ggplot2", "stringr", "rlang", "tibble"))
  
  params <- graph_params %||% list(active = 1L)
  style  <- resolve_plot_style(params$theme_name %||% "acra_light")
  
  graph_type <- params$graph_type %||% "distribution_dynamic"
  graph_type <- stringr::str_trim(as.character(graph_type))
  if (!nzchar(graph_type)) graph_type <- "distribution_dynamic"
  
  # ---- helpers ---------------------------------------------------------
  is_true01 <- function(x) {
    x <- suppressWarnings(as.integer(x))
    isTRUE(!is.na(x) && x == 1L)
  }
  
  warn_active <- function(msg) {
    if (isTRUE(warn_invalid) && is_true01(params$active %||% 1L)) {
      rlang::warn(paste0("distributionDynamic: ", msg))
    }
  }
  
  normalize_chr_vec <- function(x) {
    x <- as.character(x %||% character(0)) |> stringr::str_trim()
    x <- x[!is.na(x) & x != ""]
    unique(x)
  }
  
  build_time_breaks_labels <- function(ext_subset, params, freq) {
    time_start <- suppressWarnings(as.numeric(params$time_start %||% NA_real_))
    time_end   <- suppressWarnings(as.numeric(params$time_end %||% NA_real_))
    labfreq    <- suppressWarnings(as.numeric(params$labfreq %||% NA_real_))
    
    if (!is.finite(time_start) || !is.finite(time_end) || !is.finite(labfreq) || labfreq <= 0) {
      return(list(breaks = NULL, labels = NULL))
    }
    
    tts <- suppressWarnings(as.numeric(params$timetony_start %||% 0))
    tte <- suppressWarnings(as.numeric(params$timetony_end %||% 0))
    if (!is.finite(tts)) tts <- 0
    if (!is.finite(tte)) tte <- 0
    
    breaks <- seq(time_start + tts, time_end - tte + 1, by = labfreq)
    
    if (!("time" %in% names(ext_subset))) return(list(breaks = breaks, labels = NULL))
    
    map <- tibble::as_tibble(ext_subset) |>
      dplyr::select(dplyr::any_of(c("time", "year", "quarter", "month"))) |>
      dplyr::mutate(time = suppressWarnings(as.numeric(.data$time))) |>
      dplyr::filter(is.finite(.data$time), .data$time %in% breaks) |>
      dplyr::distinct(.data$time, .keep_all = TRUE) |>
      dplyr::arrange(.data$time)
    
    if (nrow(map) == 0) return(list(breaks = breaks, labels = NULL))
    
    fmt_one <- function(y, q, m, freq) {
      if (freq == "y") return(as.character(y))
      if (freq == "q") return(paste0(y, "Q", q))
      if (freq == "m") return(paste0(y, "-", stringr::str_pad(m, 2, pad = "0")))
      as.character(y)
    }
    
    labels <- purrr::pmap_chr(
      list(
        y = map$year %||% rep(NA_integer_, nrow(map)),
        q = map$quarter %||% rep(NA_integer_, nrow(map)),
        m = map$month %||% rep(NA_integer_, nrow(map))
      ),
      fmt_one,
      freq = freq
    )
    
    labels_full <- rep("", length(breaks))
    idx <- match(map$time, breaks)
    ok <- is.finite(idx)
    labels_full[idx[ok]] <- labels[ok]
    
    list(breaks = breaks, labels = labels_full)
  }
  
  # --- helpers: scalarize + parse time codes safely -----------------------
  as_scalar <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    x[[1]]
  }
  
  parse_time_code <- function(x, freq) {
    x <- as.character(as_scalar(x))
    x <- stringr::str_trim(x)
    if (!nzchar(x) || is.na(x)) return(NA_real_)
    
    xn <- suppressWarnings(as.numeric(x))
    if (is.finite(xn)) return(xn)
    
    freq <- tolower(stringr::str_trim(as.character(freq %||% "")))
    
    if (freq == "m" && stringr::str_detect(x, "^\\d{4}m\\d{1,2}$")) {
      y <- as.integer(stringr::str_sub(x, 1, 4))
      m <- as.integer(stringr::str_sub(x, 6))
      if (is.finite(y) && is.finite(m)) return((y - 1987) * 12 + m)
    }
    
    if (freq == "q" && stringr::str_detect(x, "^\\d{4}q\\d$")) {
      y <- as.integer(stringr::str_sub(x, 1, 4))
      q <- as.integer(stringr::str_sub(x, 6))
      if (is.finite(y) && is.finite(q)) return((y - 1987) * 4 + q)
    }
    
    if (freq == "y" && stringr::str_detect(x, "^\\d{4}$")) {
      y <- as.integer(x)
      if (is.finite(y)) return(y - 1987)
    }
    
    NA_real_
  }
  
  # --- time window --------------------------------------------------------
  freq <- normalize_freq(params$data_frequency %||% NA_character_) %||% "y"
  
  time_start <- parse_time_code(params$time_start, freq)
  time_end   <- parse_time_code(params$time_end,   freq)
  
  if (!is.finite(time_start) || !is.finite(time_end) || time_start > time_end) {
    warn_active(paste0(
      "time_start/time_end are not valid; got time_start=",
      as.character(params$time_start %||% NA_character_),
      " time_end=",
      as.character(params$time_end %||% NA_character_)
    ))
    p0 <- make_placeholder_plot(params, style, "No time window")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # ---- logging ---------------------------------------------------------
  if (isTRUE(verbose)) {
    nm <- params$graph_name %||% NA_character_
    if (!is.na(nm) && nzchar(nm)) message("Plot: ", nm)
  }
  
  # ---- params-first routing -------------------------------------------
  country_id <- params$country_iso2c %||% country_iso2c
  country_id <- normalize_chr_vec(country_id)
  country_id <- if (length(country_id) >= 1) country_id[[1]] else NA_character_
  
  peers_vec <- params$peers_iso2c %||% peers_iso2c
  peers_vec <- normalize_chr_vec(peers_vec)
  
  country_set <- unique(c(country_id, peers_vec))
  country_set <- country_set[!is.na(country_set) & country_set != ""]
  
  # ---- guards ----------------------------------------------------------
  ext <- data$extdata %||% tibble::tibble()
  if (!is.data.frame(ext) || nrow(ext) == 0) {
    warn_active("extdata is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data (extdata is empty)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "empty extdata"))
    return(out)
  }
  
  needed_base <- c("country_id", "country", "time")
  missing_base <- setdiff(needed_base, names(ext))
  if (length(missing_base) > 0) {
    warn_active(paste0("extdata missing columns: ", paste(missing_base, collapse = ", ")))
    p0 <- make_placeholder_plot(params, style, "No data (missing required columns)")
    out <- list(graph = p0, data = tibble::tibble())
    if (isTRUE(debug)) out$debug <- list(meta = tibble::tibble(msg = "missing base cols"))
    return(out)
  }
  
  indicators <- normalize_chr_vec(params$indicators)
  if (length(indicators) == 0) {
    warn_active("indicators is empty; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No indicators")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # distributionDynamic is conceptually single-indicator (fan chart).
  if (length(indicators) > 1) {
    warn_active(paste0(
      "Multiple indicators provided (", length(indicators),
      "); distributionDynamic uses only the first one: ", indicators[[1]]
    ))
    indicators <- indicators[[1]]
  }
  
  if (!all(indicators %in% names(ext))) {
    warn_active(paste0("extdata has no column for indicator: ", indicators))
    p0 <- make_placeholder_plot(params, style, "No data (missing indicator column)")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # --- time window --------------------------------------------------------
  freq <- normalize_freq(params$data_frequency %||% NA_character_) %||% "y"
  
  time_start <- parse_time_code(params$time_start, freq)
  time_end   <- parse_time_code(params$time_end,   freq)
  
  if (!is.finite(time_start) || !is.finite(time_end) || time_start > time_end) {
    warn_active(paste0(
      "time_start/time_end are not valid; got time_start=",
      as.character(params$time_start %||% NA_character_),
      " time_end=",
      as.character(params$time_end %||% NA_character_)
    ))
    p0 <- make_placeholder_plot(params, style, "No time window")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # If peers not specified: fallback to all countries (but deterministic & explicit)
  if (length(country_set) == 0) {
    warn_active("No countries resolved; falling back to all countries in extdata.")
    country_set <- tibble::as_tibble(ext) |>
      dplyr::distinct(.data$country_id) |>
      dplyr::arrange(.data$country_id) |>
      dplyr::pull(.data$country_id)
  }
  
  # ---- long data (single indicator) -----------------------------------
  df_all <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id %in% country_set) |>
    dplyr::select(dplyr::any_of(c("country", "country_id", "year", "quarter", "month", "time")),
                  dplyr::all_of(indicators)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "variable",
      values_to = "value_raw"
    ) |>
    dplyr::mutate(
      time      = suppressWarnings(as.numeric(.data$time)),
      value_raw = suppressWarnings(as.numeric(.data$value_raw))
    ) |>
    dplyr::filter(
      is.finite(.data$time),
      .data$time >= time_start,
      .data$time <= time_end,
      is.finite(.data$value_raw)
    )
  
  if (nrow(df_all) == 0) {
    warn_active("No numeric rows in window for distribution; returning placeholder.")
    p0 <- make_placeholder_plot(params, style, "No data in window")
    return(list(graph = p0, data = tibble::tibble()))
  }
  
  # ---- quantile bands (fan chart) -------------------------------------
  # symmetric bands: (0.05,0.95), (0.10,0.90), ..., (0.45,0.55)
  band_lo <- seq(0.05, 0.45, by = 0.05)
  band_hi <- 1 - band_lo
  band_id <- seq_along(band_lo) # 1 = widest
  
  # compute needed quantiles per time (distribution across countries)
  probs <- sort(unique(c(band_lo, band_hi)))
  probs_chr <- sprintf("q%02d", as.integer(round(probs * 100)))
  
  q_wide <- df_all |>
    dplyr::summarise(
      !!!rlang::set_names(
        lapply(probs, function(p) rlang::expr(stats::quantile(.data$value_raw, probs = !!p, na.rm = TRUE, names = FALSE, type = 7))),
        probs_chr
      ),
      .by = .data$time
    ) |>
    dplyr::arrange(.data$time)
  
  # bands long
  bands <- tibble::tibble(
    band = band_id,
    lo_p = band_lo,
    hi_p = band_hi,
    lo_nm = sprintf("q%02d", as.integer(round(band_lo * 100))),
    hi_nm = sprintf("q%02d", as.integer(round(band_hi * 100)))
  )
  
  # long quantiles: time × q_name → q_value
  q_long <- q_wide |>
    tidyr::pivot_longer(
      cols = -time,
      names_to = "q_name",
      values_to = "q_value"
    )
  
  df_band <- q_wide |>
    dplyr::select(.data$time) |>
    tidyr::crossing(bands) |>
    dplyr::left_join(
      q_long |> dplyr::rename(lo_nm = .data$q_name, ymin = .data$q_value),
      by = dplyr::join_by(time, lo_nm)
    ) |>
    dplyr::left_join(
      q_long |> dplyr::rename(hi_nm = .data$q_name, ymax = .data$q_value),
      by = dplyr::join_by(time, hi_nm)
    ) |>
    dplyr::select(.data$time, .data$band, .data$ymin, .data$ymax) |>
    dplyr::filter(is.finite(.data$ymin), is.finite(.data$ymax))
  
  # main country line
  df_country <- df_all |>
    dplyr::filter(.data$country_id == .env$country_id) |>
    dplyr::select(.data$country_id, .data$time, value_c = .data$value_raw) |>
    dplyr::arrange(.data$time)
  
  if (nrow(df_country) == 0) {
    warn_active("Main country has no points in window; fan chart will be shown without country line.")
  }
  
  # ---- y limits via policy --------------------------------------------
  y_rng <- range(c(df_band$ymin, df_band$ymax, df_country$value_c), na.rm = TRUE)
  if (!all(is.finite(y_rng)) || y_rng[[1]] >= y_rng[[2]]) {
    warn_active("Bad y range from data; fallback to [0,1].")
    y_rng <- c(0, 1)
  }
  
  pol <- y_lim_policy(
    kind = graph_type,
    y_min = params$y_min %||% NA_real_,
    y_max = params$y_max %||% NA_real_,
    y_rng_data = y_rng
  )
  ylim_vec <- c(pol$y_min, pol$y_max)
  if (!all(is.finite(ylim_vec)) || ylim_vec[[1]] >= ylim_vec[[2]]) ylim_vec <- y_rng
  
  # ---- x ticks ---------------------------------------------------------
  ext_one <- tibble::as_tibble(ext) |>
    dplyr::filter(.data$country_id == country_id)
  
  if (nrow(ext_one) == 0) {
    ext_one <- tibble::as_tibble(ext) |>
      dplyr::filter(.data$country_id %in% country_set) |>
      dplyr::slice_head(n = 5000)
  }
  
  xt <- build_time_breaks_labels(ext_one, params, freq)
  
  df_band <- df_band |>
    dplyr::arrange(.data$band, .data$time)
  
  # ---- plot ------------------------------------------------------------
  palette <- style$palette %||% list()
  st_line <- style_for(style, role = "country", object = "line")
  
  st_peers  <- style_for(style, role = "peers",  object = "area")
  st_others <- style_for(style, role = "others", object = "area")
  
  fill_center <- st_peers$fill  %||% st_peers$color  %||% palette$peers  %||% palette$accent %||% "grey70"
  fill_tail   <- st_others$fill %||% st_others$color %||% palette$others %||% palette$muted_text %||% "grey90"
  
  fill_low  <- palette$dark %||% palette$text %||% "grey20"
  fill_high <- palette$green %||% palette$accent %||% "grey80"
  
  main_col <- st_line$color %||% palette$country %||% palette$accent %||% palette$text %||% "grey10"
  main_lwd <- st_line$linewidth_main %||% (st_line$linewidth %||% 1.5)
  main_al  <- st_line$alpha_main %||% (st_line$alpha %||% 0.95)
  
  # band alpha: wider bands more transparent; deterministic
  band_max <- max(df_band$band, na.rm = TRUE)
  
  df_band <- df_band |>
    dplyr::mutate(
      # 1 = center, 0 = tails
      band_center = (.data$band - 1) / pmax(1, band_max - 1),
      alpha = 0.18 + 0.10 * .data$band_center
    )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = df_band,
      ggplot2::aes(
        x = .data$time,
        ymin = .data$ymin,
        ymax = .data$ymax,
        group = .data$band,
        fill = .data$band_center,  # 1 = center (peers), 0 = tails (others)
        alpha = .data$alpha
      ),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_gradient(
      low = fill_tail,
      high = fill_center,
      guide = "none"
    ) +
    ggplot2::scale_alpha_identity(guide = "none")
  
  if (nrow(df_country) > 0) {
    p <- p + ggplot2::geom_line(
      data = df_country,
      ggplot2::aes(x = .data$time, y = .data$value_c),
      color = main_col,
      linewidth = main_lwd,
      alpha = main_al,
      show.legend = FALSE
    )
  }
  
  if (!is.null(xt$breaks)) {
    p <- p + ggplot2::scale_x_continuous(breaks = xt$breaks, labels = xt$labels)
  }
  
  # y-limits via centralized viewport helper
  p <- apply_y_limits_viewport(
    p      = p,
    ylim   = ylim_vec,
    do_sec = FALSE,
    coeff  = NA_real_
  )
  
  # zero line (style-driven)
  ann      <- style$annotations %||% list()
  zero_cfg <- ann$zero_line %||% list(show = TRUE, color = palette$axis %||% "grey20",
                                      linewidth = 0.30, alpha = 0.60)
  
  if (isTRUE(zero_cfg$show %||% TRUE)) {
    p <- p + ggplot2::geom_hline(
      yintercept = 0,
      color = zero_cfg$color %||% (palette$axis %||% "grey20"),
      linewidth = zero_cfg$linewidth %||% 0.30,
      alpha = zero_cfg$alpha %||% 0.60
    )
  }
  
  # ---- finalize (no theme/title/legend here) --------------------------
  params2 <- params
  params2$country_iso2c <- country_id
  params2$peers_iso2c   <- peers_vec
  params2$x_lab <- "" # dynamic x label off by default
  # y_lab comes from fillGraphPlan / derive_axis_labels; do not override here
  
  p <- finalize_plot_common(p, params2, style, legend = FALSE)
  
  out <- list(graph = p, data = df_all)
  
  if (isTRUE(debug)) {
    out$debug <- list(
      meta = tibble::tibble(
        n_all = nrow(df_all),
        n_band = nrow(df_band),
        n_country = nrow(df_country),
        indicator = indicators,
        countries = length(country_set),
        time_start = time_start,
        time_end = time_end
      )
    )
  }
  
  out
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