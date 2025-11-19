#   All the functions to graph indicator dependencies

### Load libraries
library_names <- c("dplyr","readxl","tidyr","stringr","purrr", "igraph","ggraph","glue","here")

for (library_name in library_names) {
  library(library_name, character.only = TRUE)
}

here::i_am("download_script/dependencies.R")


### Import function definitions
source(here("download_script","import.R"))
source(here("download_script","fill.R"))


### Function to calculate edges (based on formulas of fill)
build_dependency_edges <- function(impplan,
                                   fillplan,
                                   formula_words,
                                   only_active = TRUE) {
  
  fp <- fillplan
  
  if (only_active && "active1" %in% names(fp)) {
    fp <- fp |> dplyr::filter(.data$active1 == 1L)
  }
  
  # все известные комбинации (код, частота)
  known_nodes <- build_dependency_nodes(impplan, fillplan) |>
    dplyr::select(indicator_code, frequency)
  
  fp_base <- fp |>
    dplyr::transmute(
      target_code = .data$new_indicator_code,
      target_freq = .data$new_frequency,
      formula     = .data$formula,
      old_code    = .data$old_indicator_code,
      old_freq    = .data$old_frequency
    )
  
  # ---- 1) источники из формул: только та же частота, что и target ----
  fp_formula_edges <- fp_base |>
    dplyr::mutate(
      formula_sources = purrr::map(
        .data$formula,
        ~ extractIndicators(formula = .x, formula_words = formula_words)
      )
    ) |>
    tidyr::unnest_longer(.data$formula_sources, values_to = "source_code") |>
    dplyr::filter(!is.na(.data$source_code) & .data$source_code != "") |>
    dplyr::distinct(.data$target_code, .data$target_freq,
                    .data$source_code, .data$formula) |>
    # матчим только по коду, частоту фильтруем отдельно
    dplyr::left_join(
      known_nodes,
      by = dplyr::join_by(source_code == indicator_code)
    ) |>
    # оставляем только те комбинации, где частота источника совпадает с частотой target
    dplyr::filter(!is.na(.data$frequency),
                  .data$frequency == .data$target_freq) |>
    dplyr::transmute(
      source_code = .data$source_code,
      source_freq = .data$frequency,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    ) |>
    dplyr::distinct(
      .data$source_code, .data$source_freq,
      .data$target_code, .data$target_freq,
      .data$formula
    )
  
  # ---- 2) источники по old_indicator_code / old_frequency (как раньше) ----
  fp_old_edges <- fp_base |>
    dplyr::filter(!is.na(.data$old_code), .data$old_code != "",
                  !is.na(.data$old_freq), .data$old_freq != "") |>
    dplyr::transmute(
      source_code = .data$old_code,
      source_freq = .data$old_freq,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    ) |>
    dplyr::distinct(
      .data$source_code, .data$source_freq,
      .data$target_code, .data$target_freq,
      .data$formula
    )
  
  # ---- 3) объединяем и добавляем from_id/to_id ----
  edges <- dplyr::bind_rows(fp_formula_edges, fp_old_edges) |>
    dplyr::filter(
      !is.na(.data$source_code), !is.na(.data$source_freq),
      !is.na(.data$target_code), !is.na(.data$target_freq)
    ) |>
    dplyr::mutate(
      from_id = glue::glue("{source_code}@{source_freq}"),
      to_id   = glue::glue("{target_code}@{target_freq}")
    ) |>
    dplyr::distinct(.data$from_id, .data$to_id, .keep_all = TRUE)
  
  edges
}


### Function to calculate the indicator nodes

build_dependency_nodes <- function(impplan, fillplan) {
  
  # колонка с названием БД (может называться по-разному)
  db_col <- dplyr::case_when(
    "database_name" %in% names(impplan) ~ "database_name",
    "source_name"   %in% names(impplan) ~ "source_name",
    TRUE ~ NA_character_
  )
  
  # импортные индикаторы
  nodes_imp <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::transmute(
      indicator_code = .data$indicator_code,
      frequency      = .data$source_frequency,
      db_name        = if (!is.na(db_col)) .data[[db_col]] else NA_character_,
      type           = "imported"
    )
  
  # рассчитанные индикаторы (new_indicator_code/new_frequency)
  nodes_fill <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::transmute(
      indicator_code = .data$new_indicator_code,
      frequency      = .data$new_frequency,
      db_name        = NA_character_,
      type           = "computed"
    )
  
  nodes_all <- dplyr::bind_rows(nodes_imp, nodes_fill) |>
    dplyr::filter(!is.na(.data$indicator_code), !is.na(.data$frequency)) |>
    dplyr::distinct(.data$indicator_code, .data$frequency, .keep_all = TRUE) |>
    dplyr::mutate(
      node_id = glue::glue("{indicator_code}@{frequency}"),
      label   = dplyr::case_when(
        !is.na(.data$db_name) ~ glue::glue("{indicator_code}\n({frequency}, {db_name})"),
        TRUE                  ~ glue::glue("{indicator_code}\n({frequency})")
      )
    ) |>
    dplyr::arrange(.data$indicator_code, .data$frequency)
  
  nodes_all
}


### Function to compose the graph, based on edges and nodes

build_dependency_graph <- function(impplan,
                                   fillplan,
                                   formula_words,
                                   only_active = TRUE) {
  
  nodes <- build_dependency_nodes(impplan, fillplan)
  edges <- build_dependency_edges(impplan, fillplan, formula_words, only_active)
  
  # data.frame для igraph
  edges_df <- edges |>
    dplyr::transmute(
      from        = .data$from_id,
      to          = .data$to_id,
      source_code = .data$source_code,
      source_freq = .data$source_freq,
      target_code = .data$target_code,
      target_freq = .data$target_freq,
      formula     = .data$formula
    )
  
  nodes_df <- nodes |>
    dplyr::transmute(
      name           = .data$node_id,
      indicator_code = .data$indicator_code,
      frequency      = .data$frequency,
      db_name        = .data$db_name,
      type           = .data$type,
      label          = .data$label
    )
  
  g <- igraph::graph_from_data_frame(
    d         = edges_df,
    vertices  = nodes_df,
    directed  = TRUE
  )
  
  # дублируем удобные атрибуты (на всякий)
  igraph::V(g)$indicator_code <- nodes_df$indicator_code[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$freq           <- nodes_df$frequency[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$label          <- nodes_df$label[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$type           <- nodes_df$type[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$db             <- nodes_df$db_name[match(igraph::V(g)$name, nodes_df$name)]
  
  list(
    graph = g,
    nodes = nodes,
    edges = edges
  )
}


### Function to find vertex

get_indicator_vertex <- function(g, indicator_code, frequency = NULL) {
  
  v_codes <- igraph::V(g)$indicator_code
  v_freqs <- igraph::V(g)$freq
  
  idx_code <- which(v_codes == indicator_code)
  
  if (length(idx_code) == 0L) {
    stop(glue::glue("Индикатор '{indicator_code}' отсутствует в графе."), call. = FALSE)
  }
  
  if (is.null(frequency)) {
    freqs <- unique(v_freqs[idx_code])
    if (length(freqs) == 1L) {
      return(igraph::V(g)[idx_code])
    } else {
      stop(
        glue::glue(
          "Индикатор '{indicator_code}' существует на нескольких частотах: {paste(freqs, collapse = ', ')}.\n",
          "Пожалуйста, укажите аргумент frequency = 'd'/'m'/'q'/'y'."
        ),
        call. = FALSE
      )
    }
  } else {
    idx <- idx_code[v_freqs[idx_code] == frequency]
    if (length(idx) == 0L) {
      freqs <- unique(v_freqs[idx_code])
      stop(
        glue::glue(
          "Индикатор '{indicator_code}' не найден на частоте '{frequency}'.\n",
          "Доступные частоты: {paste(freqs, collapse = ', ')}."
        ),
        call. = FALSE
      )
    }
    igraph::V(g)[idx]
  }
}


### Function to get the subgraph of dependencies

get_indicator_subgraph <- function(dep_graph,
                                   indicator_code,
                                   frequency = NULL,
                                   direction = c("both", "upstream", "downstream")) {
  
  direction <- match.arg(direction)
  g <- dep_graph$graph
  
  v <- get_indicator_vertex(g, indicator_code = indicator_code, frequency = frequency)
  
  vids <- switch(
    direction,
    upstream   = igraph::subcomponent(g, v, mode = "in"),
    downstream = igraph::subcomponent(g, v, mode = "out"),
    both       = {
      in_nodes  <- igraph::subcomponent(g, v, mode = "in")
      out_nodes <- igraph::subcomponent(g, v, mode = "out")
      union(in_nodes, out_nodes)
    }
  )
  
  igraph::induced_subgraph(g, vids = vids)
}


### Function to plot the subgraph of dependencies

plot_indicator_graph <- function(dep_graph,
                                 indicator_code,
                                 frequency = NULL,            # НОВЫЙ аргумент
                                 direction = c("both", "upstream", "downstream"),
                                 show_edge_formula = FALSE,
                                 max_formula_len = 40) {
  
  direction <- match.arg(direction)
  
  g_sub <- get_indicator_subgraph(
    dep_graph      = dep_graph,
    indicator_code = indicator_code,
    frequency      = frequency
  )
  
  if (igraph::gorder(g_sub) == 0L) {
    stop(glue::glue("Подграф для '{indicator_code}' пуст."), call. = FALSE)
  }
  
  # фокусный узел
  focus_codes <- igraph::V(g_sub)$indicator_code
  focus_freqs <- igraph::V(g_sub)$freq
  focus_name  <- if (is.null(frequency)) {
    # сюда мы попадаем только если для кода была одна частота
    indicator_code
  } else {
    indicator_code
  }
  
  igraph::V(g_sub)$is_focus <- (focus_codes == indicator_code) &
    (if (is.null(frequency)) TRUE else focus_freqs == frequency)
  
  fill_palette <- c(
    imported = "#CFE5FF",
    computed = "#FFF4C2",
    unknown  = "#E0E0E0"
  )
  
  stroke_palette <- c(
    normal = "#636363"
  )
  
  focus_fill <- "#FDBF6F"  # цвет фокусного узла
  
  freq_title <- if (is.null(frequency)) {
    unique(igraph::V(g_sub)$freq[igraph::V(g_sub)$is_focus])[1]
  } else {
    frequency
  }
  
  ggraph::ggraph(g_sub, layout = "sugiyama") +
    
    # Рёбра: делаем больший "зазор" вокруг узла
    ggraph::geom_edge_link(
      colour  = "#A0A0A0",
      arrow   = grid::arrow(length = grid::unit(3, "mm")),
      end_cap = ggraph::circle(9, "mm"),   # было 3 мм → стало 6 мм
      alpha   = 0.7
    ) +
    
    # Слой 1: все узлы
    ggraph::geom_node_label(
      ggplot2::aes(
        label    = .data$label,
        fill     = .data$type,
        colour   = "normal",
        fontface = ifelse(.data$is_focus, "bold", "plain")
      ),
      label.size    = 0.3,
      label.r       = grid::unit(3, "pt"),
      label.padding = grid::unit(2.5, "pt"),  # чуть меньше, чтобы коробка была компактнее
      size          = 3,
      show.legend   = FALSE
    ) +
    
    # Слой 2: фокусный узел
    ggraph::geom_node_label(
      data = function(d) d[d$is_focus, ],
      ggplot2::aes(
        label = .data$label
      ),
      fill          = focus_fill,
      colour        = stroke_palette["normal"],
      fontface      = "bold",
      label.size    = 0.3,
      label.r       = grid::unit(3, "pt"),
      label.padding = grid::unit(3, "pt"),
      size          = 3.2,
      show.legend   = FALSE
    ) +
    
    ggplot2::scale_fill_manual(values = fill_palette, na.value = "#E0E0E0") +
    ggplot2::scale_colour_manual(values = stroke_palette, guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) +
    
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#F7F8FB", colour = NA),
      plot.background  = ggplot2::element_rect(fill = "#F7F8FB", colour = NA),
      panel.border     = ggplot2::element_rect(colour = "#D0D0D0", fill = NA, linewidth = 0.6),
      plot.margin      = ggplot2::margin(t = 10, r = 20, b = 10, l = 20),
      plot.title       = ggplot2::element_text(face = "bold", hjust = 0, size = 12),
      axis.title       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(
      glue::glue("Зависимости индикатора: {indicator_code} ({freq_title}, {direction})")
    )
}
