library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggiraph)
library(glue)
library(here)

### From import.R

readImportParams <- function (param_fname, update_mode) {
  
  #param_fname <- here("assets", "_DB", "0_database_params.xlsx")
  impplan <- read_excel(param_fname, sheet = "import", col_names = T, skip=1)
  parameters <- read_excel(param_fname, sheet = "scope", col_names = T, skip=1, n_max=1)
  locals <- read_excel(param_fname, sheet = "scope", col_names = T, skip=6)
  local_countries <- locals |> filter(active == 1) |> pull(country)
  local_iso2 <- locals |> filter(active == 1) |> pull(country_id)
  local_fnames <- here("assets", local_countries, "Data", glue("{local_countries}_local.xlsx"))
  year_first <- parameters$start
  year_final <- parameters$end
  saveplan <- impplan |> filter(active == 1)
  if (update_mode == 1) {impplan <- impplan |> filter(update == 1)}
  impplan <- impplan |> filter(active == 1)
  return(list(year_first = year_first, year_final = year_final, saveplan = saveplan, impplan = impplan, 
              local_countries = local_countries, local_iso2 = local_iso2, local_fnames = local_fnames))
  
}

### From fill.R

readFillParams <- function(param_fname, sheet = "fill") {
  
  fillplan <- readxl::read_excel(path = param_fname, sheet = sheet, col_names = TRUE, skip = 1) |> 
    dplyr::filter(.data$active1 == 1)
  
  fillplan
}

extractIndicators <- function(formula, formula_words) {
  
  if (is.na(formula) || formula == "") {
    return(character(0))
  }
  
  # Split formula 
  tokens <- stringr::str_split(string = formula, pattern = "\\/|\\(|\\)|\\+|\\-|\\*|\\=|\\^|\\,|\\s+|>|<|==|!=", 
                               simplify = TRUE)
  tokens <- tokens[tokens != ""]
  
  # Numeric tokens (like "10", "3.5") — dropped
  is_num <- !is.na(suppressWarnings(as.numeric(tokens)))
  
  # Non-indicator tokens dropped:
  # - function names (formula_words: lag, rollsum, coalesce, ...)
  # - short tokens <= 2 (usually operators / service codes)
  to_drop_fun   <- tokens %in% formula_words
  to_drop_short <- nchar(tokens) <= 2L
  
  keep <- !(to_drop_fun | to_drop_short | is_num)
  
  unique(tokens[keep])
}

### From dependencies.R

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
  
  # 1) источники из формул (та же частота)
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
    dplyr::left_join(
      known_nodes,
      by = dplyr::join_by(source_code == indicator_code),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      !is.na(.data$frequency),
      .data$frequency == .data$target_freq
    ) |>
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
  
  # 2) источники по old_indicator_code / old_frequency
  fp_old_edges <- fp_base |>
    dplyr::filter(
      !is.na(.data$old_code), .data$old_code != "",
      !is.na(.data$old_freq), .data$old_freq != ""
    ) |>
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
  
  # 3) объединяем
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

build_dependency_nodes <- function(impplan, fillplan) {
  
  db_col <- dplyr::case_when(
    "database_name" %in% names(impplan) ~ "database_name",
    "source_name"   %in% names(impplan) ~ "source_name",
    TRUE ~ NA_character_
  )
  
  # --- новые: определяем, где лежат названия индикаторов ---
  name_cols_imp  <- c("indicator",
                      "indicator_name", "name", "var_name", "indicator_label",
                      "label_ru", "label_en")
  
  name_cols_fill <- c("new_indicator",
                      "new_indicator_name", "indicator_name", "name", "var_name",
                      "indicator_label", "label_ru", "label_en")
  
  name_col_imp  <- name_cols_imp[name_cols_imp %in% names(impplan)] |> 
    purrr::pluck(1, .default = NA_character_)
  name_col_fill <- name_cols_fill[name_cols_fill %in% names(fillplan)] |> 
    purrr::pluck(1, .default = NA_character_)
  
  nodes_imp <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col_imp)) .data[[name_col_imp]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$indicator_code,
      frequency      = .data$source_frequency,
      db_name        = if (!is.na(db_col)) .data[[db_col]] else NA_character_,
      indicator_name = .data$indicator_name,
      type           = "imported"
    )
  
  nodes_fill <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::mutate(
      indicator_name = if (!is.na(name_col_fill)) .data[[name_col_fill]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$new_indicator_code,
      frequency      = .data$new_frequency,
      db_name        = NA_character_,
      indicator_name = .data$indicator_name,
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

build_dependency_graph <- function(impplan,
                                   fillplan,
                                   formula_words,
                                   only_active = TRUE) {
  
  nodes <- build_dependency_nodes(impplan, fillplan)
  edges <- build_dependency_edges(impplan, fillplan, formula_words, only_active)
  
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
      label          = .data$label,
      indicator_name = .data$indicator_name
    )
  
  g <- igraph::graph_from_data_frame(
    d        = edges_df,
    vertices = nodes_df,
    directed = TRUE
  )
  
  igraph::V(g)$indicator_code  <- nodes_df$indicator_code[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$freq            <- nodes_df$frequency[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$label           <- nodes_df$label[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$type            <- nodes_df$type[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$db              <- nodes_df$db_name[match(igraph::V(g)$name, nodes_df$name)]
  igraph::V(g)$indicator_name  <- nodes_df$indicator_name[match(igraph::V(g)$name, nodes_df$name)]
  
  list(
    graph = g,
    nodes = nodes,
    edges = edges
  )
}

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

# ---------- Вспомогательный каталог индикаторов для выпадающего списка -----

build_indicator_catalog <- function(impplan, fillplan) {
  
  name_cols_imp  <- c("indicator",       # <-- добавили
                      "indicator_name", "name", "var_name", "indicator_label",
                      "label_ru", "label_en")
  name_cols_fill <- c("new_indicator",   # <-- добавили
                      "new_indicator_name", "indicator_name", "name", "var_name",
                      "indicator_label", "label_ru", "label_en")
  
  name_col_imp  <- name_cols_imp[name_cols_imp %in% names(impplan)] |> purrr::pluck(1, .default = NA_character_)
  name_col_fill <- name_cols_fill[name_cols_fill %in% names(fillplan)] |> purrr::pluck(1, .default = NA_character_)
  
  imp <- impplan |>
    dplyr::filter(.data$active == 1L) |>
    dplyr::mutate(
      name = if (!is.na(name_col_imp)) .data[[name_col_imp]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$indicator_code,
      frequency      = .data$source_frequency,
      name,
      type           = "imported"
    )
  
  fill <- fillplan |>
    dplyr::filter(.data$active1 == 1L) |>
    dplyr::mutate(
      name = if (!is.na(name_col_fill)) .data[[name_col_fill]] else NA_character_
    ) |>
    dplyr::transmute(
      indicator_code = .data$new_indicator_code,
      frequency      = .data$new_frequency,
      name,
      type           = "computed"
    )
  
  dplyr::bind_rows(imp, fill) |>
    dplyr::filter(!is.na(.data$indicator_code), !is.na(.data$frequency)) |>
    dplyr::distinct(.data$indicator_code, .data$frequency, .keep_all = TRUE) |>
    dplyr::mutate(
      node_id = glue::glue("{indicator_code}@{frequency}"),
      label   = dplyr::case_when(
        !is.na(.data$name) & .data$name != "" ~ glue::glue("{indicator_code}@{frequency} — {name}"),
        TRUE ~ glue::glue("{indicator_code}@{frequency}")
      )
    ) |>
    dplyr::arrange(.data$indicator_code, .data$frequency)
}

# ---------- Функция для интерактивного графика (ggiraph) -------------------

plot_indicator_graph_interactive <- function(dep_graph,
                                             indicator_code,
                                             frequency = NULL,
                                             direction = c("both", "upstream", "downstream"),
                                             max_formula_len = 200) {
  
  direction <- match.arg(direction)
  
  g_sub <- get_indicator_subgraph(
    dep_graph      = dep_graph,
    indicator_code = indicator_code,
    frequency      = frequency,
    direction      = direction
  )
  
  if (igraph::gorder(g_sub) == 0L) {
    stop(glue::glue("Подграф для '{indicator_code}' пуст."), call. = FALSE)
  }
  
  # ---- фокусный узел ----
  focus_codes <- igraph::V(g_sub)$indicator_code
  focus_freqs <- igraph::V(g_sub)$freq
  
  igraph::V(g_sub)$is_focus <- (focus_codes == indicator_code) &
    (if (is.null(frequency)) TRUE else focus_freqs == frequency)
  
  # ---- атрибуты рёбер для подсказок ----
  igraph::E(g_sub)$edge_id <- seq_len(igraph::ecount(g_sub))
  
  igraph::E(g_sub)$formula_short <- igraph::E(g_sub)$formula |>
    as.character() |>
    stringr::str_replace_na("") |>
    stringr::str_trunc(max_formula_len)
  
  fill_palette <- c(
    imported = "#CFE5FF",
    computed = "#FFF4C2",
    unknown  = "#E0E0E0"
  )
  
  stroke_palette <- c(
    normal = "#636363"
  )
  
  focus_fill <- "#FDBF6F"
  
  freq_title <- if (is.null(frequency)) {
    unique(igraph::V(g_sub)$freq[igraph::V(g_sub)$is_focus])[1]
  } else {
    frequency
  }
  
  # -------- layout через ggraph --------
  layout <- ggraph::create_layout(g_sub, layout = "sugiyama")
  
  # узлы
  nodes_df <- layout |> 
    tibble::as_tibble() |>
    dplyr::mutate(
      # всплывающая подсказка: код + человеко-читаемое название
      tooltip = dplyr::case_when(
        !is.na(.data$indicator_name) & .data$indicator_name != "" ~ 
          glue::glue("{indicator_code} — {indicator_name}"),
        TRUE ~ indicator_code
      )
    )
  
  # рёбра: данные из igraph + координаты узлов
  edges_tbl <- igraph::as_data_frame(g_sub, what = "edges") |>
    tibble::as_tibble()
  
  edges_df <- edges_tbl |>
    dplyr::left_join(
      nodes_df |>
        dplyr::select(name, x_from = .data$x, y_from = .data$y),
      by = dplyr::join_by(from == name)
    ) |>
    dplyr::left_join(
      nodes_df |>
        dplyr::select(name, x_to = .data$x, y_to = .data$y),
      by = dplyr::join_by(to == name)
    ) |>
    dplyr::transmute(
      x      = .data$x_from,
      y      = .data$y_from,
      xend   = .data$x_to,
      yend   = .data$y_to,
      edge_id,
      tooltip = .data$formula_short
    )
  
  # -------- сам ggplot --------
  p <- ggplot2::ggplot() +
    
    # 1) Невидимый, но толстый интерактивный слой рёбер — большая зона попадания
    ggiraph::geom_segment_interactive(
      data = edges_df,
      ggplot2::aes(
        x      = .data$x,
        y      = .data$y,
        xend   = .data$xend,
        yend   = .data$yend,
        tooltip = .data$tooltip,
        data_id = .data$edge_id
      ),
      linewidth   = 4,       # толстый hitbox
      alpha       = 0,       # полностью прозрачный
      lineend     = "round",
      show.legend = FALSE
    ) +
    
    # 2) Тонкие видимые рёбра поверх
    ggplot2::geom_segment(
      data = edges_df,
      ggplot2::aes(
        x    = .data$x,
        y    = .data$y,
        xend = .data$xend,
        yend = .data$yend
      ),
      colour  = "#A0A0A0",
      arrow   = grid::arrow(length = grid::unit(3, "mm")),
      lineend = "round",
      alpha   = 0.7,
      linewidth = 0.6,
      show.legend = FALSE
    ) +
    
    # Все узлы — интерактивные подписи
    ggiraph::geom_label_interactive(
      data = nodes_df,
      ggplot2::aes(
        x        = .data$x,
        y        = .data$y,
        label    = .data$label,
        fill     = .data$type,
        colour   = "normal",
        fontface = ifelse(.data$is_focus, "bold", "plain"),
        tooltip  = .data$tooltip,
        data_id  = .data$name
      ),
      label.size    = 0.3,
      label.r       = grid::unit(3, "pt"),
      label.padding = grid::unit(2.5, "pt"),
      size          = 3,
      show.legend   = FALSE
    ) +
    
    # Фокусный узел поверх (тоже интерактивен)
    {
      nodes_focus_df <- nodes_df |> dplyr::filter(.data$is_focus)
      if (nrow(nodes_focus_df) > 0) {
        ggiraph::geom_label_interactive(
          data = nodes_focus_df,
          ggplot2::aes(
            x       = .data$x,
            y       = .data$y,
            label   = .data$label,
            tooltip = .data$tooltip,
            data_id = .data$name
          ),
          fill          = focus_fill,
          colour        = stroke_palette["normal"],
          fontface      = "bold",
          label.size    = 0.3,
          label.r       = grid::unit(3, "pt"),
          label.padding = grid::unit(3, "pt"),
          size          = 3.2,
          show.legend   = FALSE
        )
      } else {
        ggplot2::geom_blank()
      }
    } +
    
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
  
  ggiraph::girafe(
    ggobj = p,
    width_svg  = 8,
    height_svg = 6,
    options = list(
      ggiraph::opts_hover(css = "stroke-width: 2;"),
      ggiraph::opts_tooltip(opacity = 0.9)
    )
  )
}

# --------------------------- Загрузка данных -------------------------------

# путь к excel с планом
param_fname <- "0_database_params.xlsx"
update_mode <- 0L

here::i_am("app.R")
import_params <- readImportParams(param_fname = here(param_fname), update_mode = update_mode)
impplan       <- import_params$impplan
fillplan      <- readFillParams(param_fname, sheet = "fill")

# слова-функции для парсинга формул
formula_words <- c(
  "lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first",
  "min", "pmin", "max", "pmax", "sum", "coalesce", "share", "exp", "fromto",
  "year", "na_if", "cummax", "cummin", "cumsum", "ceiling", "letterize",
  "if_else", "ifelse", "sqrt", "log", "abs", "rollapply"
)

# каталог индикаторов для selectize
indicator_catalog <- build_indicator_catalog(impplan, fillplan)

# ------------------------------- UI ---------------------------------------

ui <- fluidPage(
  titlePanel("Граф зависимостей индикаторов"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "indicator",
        label   = "Индикатор и частота",
        choices = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Начните вводить код или название индикатора...",
          maxOptions  = 5000
        )
      ),
      
      radioButtons(
        inputId = "direction",
        label   = "Направление зависимостей",
        choices = c(
          "Влияющие (upstream)"   = "upstream",
          "Зависящие (downstream)" = "downstream",
          "Все (both)"            = "both"
        ),
        selected = "both"
      ),
      
      checkboxInput(
        inputId = "only_active",
        label   = "Только активные формулы",
        value   = TRUE
      )
    ),
    
    mainPanel(
      ggiraph::girafeOutput("graph", width = "100%", height = "600px")
    )
  )
)

# ------------------------------ SERVER ------------------------------------

server <- function(input, output, session) {
  
  # реактивный граф (перестраивается только по флажку "только активные")
  dep_graph <- reactive({
    build_dependency_graph(
      impplan       = impplan,
      fillplan      = fillplan,
      formula_words = formula_words,
      only_active   = input$only_active
    )
  })
  
  # один раз заполняем selectize server-side
  observe({
    choices <- rlang::set_names(
      indicator_catalog$node_id,
      indicator_catalog$label
    )
    
    updateSelectizeInput(
      session,
      inputId = "indicator",
      choices = choices,
      server  = TRUE
    )
  })
  
  output$graph <- ggiraph::renderGirafe({
    req(input$indicator)
    
    node_row <- indicator_catalog |>
      dplyr::filter(.data$node_id == .env$input$indicator) |>
      dplyr::slice(1)
    
    indicator_code <- node_row$indicator_code
    frequency      <- node_row$frequency
    
    plot_indicator_graph_interactive(
      dep_graph      = dep_graph(),
      indicator_code = indicator_code,
      frequency      = frequency,
      direction      = input$direction
    )
  })
}

# ------------------------------ runApp ------------------------------------

shinyApp(ui = ui, server = server)