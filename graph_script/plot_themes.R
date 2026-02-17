
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

###### Plot styles

make_style_acra_light <- function() {
  assert_packages(c("ggplot2", "rlang"))
  
  # --------------------------------------------------------------------
  # Palette: semantic colors (roles + infrastructure + annotations)
  # --------------------------------------------------------------------
  palette <- list(
    # canvas / infrastructure
    bg        = "white",
    panel_bg  = "white",
    text      = "grey10",
    muted_text= "grey35",
    axis      = "grey20",
    grid_major= "grey90",
    grid_minor= "grey94",
    
    # roles
    country = unname(ACRA["dark"]),
    peers   = unname(ACRA["sec2"]),
    others  = unname(ACRA["green"]),
    accent  = unname(ACRA["sec4"]),
    
    # reference lines / signals
    zero_line   = "grey35",
    target_line = unname(ACRA["sec6"]),
    warn        = unname(ACRA["red"])
  )
  
  palettes <- list(
    time_bins = unname(ACRA[c(
      "sec1", "sec2", "sec3", "sec4", "sec5", "sec6", "sec7", "sec8",
      "add1", "add2", "add3", "add4", "add5", "reddest", "orange", "brown",
      "green", "dark"
    )])
  )
  
  # --------------------------------------------------------------------
  # Typography: centralized control (finalize_plot_common uses these)
  # --------------------------------------------------------------------
  typography <- list(
    base_family = "Nunito Sans",
    base_size   = 11,
    
    title = list(
      size = 14,
      face = "bold",
      lineheight = 1.05,
      color = palette$text,
      margin_b = 8
    ),
    subtitle = list(
      size = 11,
      face = "plain",
      lineheight = 1.05,
      color = palette$muted_text,
      margin_b = 6
    ),
    axis_title = list(
      size = 10,
      face = "plain",
      color = palette$text,
      margin_t = 8,
      margin_r = 8
    ),
    axis_text = list(
      size = 9,
      face = "plain",
      color = palette$muted_text
    ),
    caption = list(
      size = 9,
      face = "plain",
      lineheight = 1.10,
      color = palette$muted_text,
      margin_t = 8
    ),
    point_label = list(
      size_mm = 10,        # base, later multiplied by compute_plot_scaling()
      face    = "plain",
      color_country = palette$country,
      color_peers   = palette$peers,
      alpha   = 1.0
    ),
    legend_text = list(
      size = 9,
      color = palette$text
    )
  )
  
  # --------------------------------------------------------------------
  # Grid / axes policy (theme differences should change these)
  # --------------------------------------------------------------------
  grid <- list(
    major = list(show = TRUE,  color = palette$grid_major, linewidth = 0.30),
    minor = list(show = FALSE, color = palette$grid_minor, linewidth = 0.20),
    axis_line  = list(show = TRUE,  color = palette$axis, linewidth = 0.35),
    axis_ticks = list(show = TRUE,  color = palette$axis, linewidth = 0.25)
  )
  
  # --------------------------------------------------------------------
  # Layout: margins, legend conventions, spacing
  # --------------------------------------------------------------------
  layout <- list(
    plot_margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10),
    legend = list(
      position = "bottom",
      direction = "horizontal",
      justification = "left",
      box = "horizontal",
      title_blank = TRUE
    )
  )
  
  # --------------------------------------------------------------------
  # Annotations: standardized extras (time tag, trend, etc.)
  # --------------------------------------------------------------------
  annotations <- list(
    time_tag = list(
      show = TRUE,
      position = "tr",       # tr/br/tl/bl
      text_color = palette$axis,
      size = 3.2,            # mm in ggplot annotate("text")
      hjust = 1.05,
      vjust = 1.25
    ),
    trend = list(
      ci_show = TRUE,
      ci_alpha = 0.25,
      ci_fill  = palette$grid_major, # neutral “uncertainty”
      line_color = palette$axis,
      linewidth  = 0.60
    ),
    zero_line = list(
      show = TRUE,
      color = palette$zero_line,
      linewidth = 0.30,
      alpha = 0.60
    )
  )
  
  # --------------------------------------------------------------------
  # Roles: semantics (importance multipliers) + default label policy
  # --------------------------------------------------------------------
  roles <- list(
    country = list(weight = 1.30, alpha = 1.00, label_show = TRUE,  z = 3),
    peers   = list(weight = 1.10, alpha = 0.92, label_show = TRUE,  z = 2),
    others  = list(weight = 0.80, alpha = 0.25, label_show = FALSE, z = 1)
  )
  
  # --------------------------------------------------------------------
  # Objects: geometry-family defaults (can be shared across plot types)
  # --------------------------------------------------------------------
  objects <- list(
    point  = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid", shape = 16, stroke = 0),
    line   = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid"),
    ribbon = list(size = 1.00, alpha = 0.30, linewidth = 1.00, linetype = "solid"),
    bar    = list(size = 1.00, alpha = 0.85, linewidth = 1.00, linetype = "solid"),
    text   = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid")
  )
  
  # --------------------------------------------------------------------
  # Units: base “physical” sizes (plots multiply these deterministically)
  # --------------------------------------------------------------------
  units <- list(
    base_pt_size   = 2.0,
    base_lab_mm    = 3.2,
    base_linewidth = 0.6
  )
  
  # --------------------------------------------------------------------
  # gg_theme: keep minimal; let finalize_plot_common() enforce typography/layout
  # --------------------------------------------------------------------
  gg_theme <- ggplot2::theme_minimal(
    base_size   = typography$base_size,
    base_family = typography$base_family
  ) +
    ggplot2::theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      plot.background  = ggplot2::element_rect(fill = palette$bg, color = NA),
      panel.background = ggplot2::element_rect(fill = palette$panel_bg, color = NA),
      
      text = ggplot2::element_text(color = palette$text),
      
      panel.grid.major = if (isTRUE(grid$major$show)) {
        ggplot2::element_line(color = grid$major$color, linewidth = grid$major$linewidth)
      } else ggplot2::element_blank(),
      panel.grid.minor = if (isTRUE(grid$minor$show)) {
        ggplot2::element_line(color = grid$minor$color, linewidth = grid$minor$linewidth)
      } else ggplot2::element_blank(),
      
      axis.line  = if (isTRUE(grid$axis_line$show))  ggplot2::element_line(color = grid$axis_line$color,  linewidth = grid$axis_line$linewidth) else ggplot2::element_blank(),
      axis.ticks = if (isTRUE(grid$axis_ticks$show)) ggplot2::element_line(color = grid$axis_ticks$color, linewidth = grid$axis_ticks$linewidth) else ggplot2::element_blank(),
      
      legend.position = layout$legend$position
    )
  
  list(
    name = "acra_light",
    gg_theme = gg_theme,
    
    palette = palette,
    palettes = palettes,
    typography = typography,
    grid = grid,
    layout = layout,
    annotations = annotations,
    
    roles = roles,
    objects = objects,
    units = units
  )
}


make_style_bw <- function() {
  assert_packages(c("ggplot2"))
  
  palette <- list(
    bg   = "white",
    text = "black",
    axis = "black",
    grid = "grey85",
    
    country = "black",
    peers   = "grey20",
    others  = "grey70",
    accent  = "black"
  )
  
  gg_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = palette$grid, linewidth = 0.3),
      text = ggplot2::element_text(color = palette$text),
      plot.title.position = "plot"
    )
  
  list(
    name = "bw",
    gg_theme = gg_theme,
    palette = palette,
    roles = list(
      country = list(weight = 1.25, alpha = 1.00),
      peers   = list(weight = 1.05, alpha = 0.90),
      others  = list(weight = 0.80, alpha = 0.25)
    ),
    objects = list(
      point  = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid"),
      line   = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid"),
      ribbon = list(size = 1.00, alpha = 0.25, linewidth = 1.00, linetype = "solid"),
      bar    = list(size = 1.00, alpha = 0.85, linewidth = 1.00, linetype = "solid"),
      text   = list(size = 1.00, alpha = 1.00, linewidth = 1.00, linetype = "solid")
    ),
    scale = list(
      base_pt_size   = 2.0,
      base_lab_size  = 3.2,
      base_linewidth = 0.6
    ),
    layout = list(
      legend_position = "bottom"
    )
  )
}

resolve_plot_style <- function(theme_name) {
  assert_packages(c("stringr"))
  
  nm <- theme_name |>
    as.character() |>
    (\(x) if (length(x) >= 1) x[[1]] else NA_character_)() |>
    stringr::str_trim() |>
    stringr::str_to_lower()
  
  nm <- normalize_theme_name(nm) %||% "acra_light"
  
  switch(
    nm,
    "acra_light" = make_style_acra_light(),
    "bw"         = make_style_bw(),
    # you can add: acra_dark / minimal / ipsum later
    make_style_acra_light()
  )
}

style_for <- function(style, role, object) {
  assert_packages(c("rlang"))
  
  style <- style %||% list()
  palette <- style$palette %||% list()
  roles   <- style$roles %||% list()
  objects <- style$objects %||% list()
  scale   <- style$scale %||% list(base_pt_size = 2.0, base_lab_size = 3.2, base_linewidth = 0.6)
  
  role <- as.character(role %||% "others")
  object <- as.character(object %||% "point")
  
  role_def <- roles[[role]] %||% roles$others %||% list(weight = 1.0, alpha = 1.0)
  obj_def  <- objects[[object]] %||% objects$point %||% list(size = 1.0, alpha = 1.0, linewidth = 1.0, linetype = "solid")
  
  # semantic base color: by role
  col <- palette[[role]] %||% palette$others %||% "grey70"
  
  # combine role × object deterministically
  alpha <- (role_def$alpha %||% 1.0) * (obj_def$alpha %||% 1.0)
  weight <- (role_def$weight %||% 1.0)
  
  list(
    color = col,
    fill  = col,
    alpha = alpha,
    
    # multipliers (the plot decides absolute sizes; these shape them by semantics)
    size_mult      = weight * (obj_def$size %||% 1.0),
    linewidth_mult = weight * (obj_def$linewidth %||% 1.0),
    
    linetype = obj_def$linetype %||% "solid",
    
    # base units (optional use)
    base_pt_size   = scale$base_pt_size %||% 2.0,
    base_lab_size  = scale$base_lab_size %||% 3.2,
    base_linewidth = scale$base_linewidth %||% 0.6
  )
}

add_country_role <- function(df, country_iso2c, peers_iso2c, id_col = "country_id") {
  assert_packages(c("dplyr", "rlang", "stringr"))
  
  id_col <- rlang::ensym(id_col)
  
  peers_iso2c <- as.character(peers_iso2c %||% character(0)) |>
    stringr::str_trim()
  peers_iso2c <- peers_iso2c[!is.na(peers_iso2c) & peers_iso2c != ""]
  peers_iso2c <- unique(peers_iso2c)
  
  df |>
    dplyr::mutate(
      role = dplyr::case_when(
        !!id_col == country_iso2c ~ "country",
        !!id_col %in% peers_iso2c ~ "peers",
        TRUE ~ "others"
      )
    )
}

# ---- Theme ipsum: use hrbrthemes if available, otherwise fallback ----
# theme_ipsum_safe <- function(base_size = 12, base_family = "Nunito Sans") {
#   if (requireNamespace("hrbrthemes", quietly = TRUE)) {
#     return(hrbrthemes::theme_ipsum(base_size = base_size, base_family = base_family))
#   }
#   
#   # Fallback: close enough to keep layouts readable
#   ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
#     ggplot2::theme(
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.grid.major.x = ggplot2::element_blank(),
#       axis.ticks = ggplot2::element_line(linewidth = 0.3),
#       plot.title.position = "plot"
#     )
# }
# 
# theme_ipsum <- theme_ipsum_safe
# 
# font_add_google("Nunito Sans", regular.wt = 400, bold.wt = 700)
# showtext_opts(dpi = 150)
# showtext_auto()
