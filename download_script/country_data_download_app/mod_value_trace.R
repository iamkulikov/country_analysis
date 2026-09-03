# Shiny module: Preview data (wide table) + Trace Ledger calculation journal.
# UI is split so the Preview button can sit next to Download in the parent layout.

mod_value_trace_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("preview_data"),
    "Preview data",
    class = "btn-info"
  )
}

mod_value_trace_panel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "trace-ledger.css"
    ),
    shiny::tags$script(src = "trace-ledger.js"),
    shiny::tags$style(shiny::HTML(paste0("
      #", ns("preview_table"), " table.dataTable tbody td.selected {
        box-shadow: inset 0 0 0 2px #0d6efd !important;
        outline: 2px solid #0d6efd;
        outline-offset: -2px;
      }
    "))),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::uiOutput(ns("preview_main")),
        shinyjs::hidden(
          shiny::div(
            id = ns("trace_section"),
            shiny::tags$hr(),
            shiny::tags$h4("Calculation trace"),
            shiny::uiOutput(ns("trace_selected_card")),
            shiny::div(
              class = "tl-controls",
              shiny::div(
                style = "white-space: nowrap; flex-shrink: 0;",
                shiny::checkboxInput(
                  ns("trace_show_technical"),
                  "Show technical intermediate indicators",
                  value = TRUE
                )
              ),
              shiny::div(
                style = "display: flex; align-items: center; gap: 8px; margin-left: auto;",
                shiny::actionButton(
                  ns("trace_collapse_all"),
                  "Collapse all",
                  class = "btn-sm btn-default",
                  `data-tl-collapse-all` = "1"
                ),
                shiny::downloadButton(
                  ns("download_trace"),
                  "Download journal",
                  class = "btn-sm"
                )
              )
            ),
            shiny::textOutput(ns("trace_unavailable")),
            shiny::uiOutput(ns("trace_tree_host"))
          )
        )
      )
    )
  )
}

.parse_preview_cell_selection <- function(sel, info) {
  if (is.null(sel) || length(sel) == 0 || (is.matrix(sel) && nrow(sel) == 0)) {
    return(NULL)
  }

  if (is.matrix(sel)) {
    if (!is.null(colnames(sel)) && all(c("row", "col") %in% colnames(sel))) {
      row_idx <- as.integer(sel[1, "row"])
      col_idx <- as.integer(sel[1, "col"])
    } else {
      row_idx <- as.integer(sel[1, 1])
      col_idx <- as.integer(sel[1, 2])
    }
  } else {
    row_idx <- as.integer(sel[[1]])
    col_idx <- as.integer(sel[[2]])
  }

  # DT with rownames = FALSE reports 0-based column indices (rstudio/DT#427).
  col_idx <- as.integer(col_idx) + 1L

  df <- info$data
  if (is.null(df) || is.na(row_idx) || is.na(col_idx) ||
      row_idx < 1 || row_idx > nrow(df) ||
      col_idx < 1 || col_idx > ncol(df)) {
    return(NULL)
  }

  meta_n <- length(intersect(PREVIEW_META_COLS, names(df)))
  if (col_idx <= meta_n) return(NULL)

  period <- names(df)[col_idx]
  row <- df[row_idx, , drop = FALSE]

  list(
    country_id     = row$country_id[[1]],
    country        = row$country[[1]],
    indicator_code = row$indicator_code[[1]],
    frequency      = info$freq,
    period         = period,
    value          = df[[col_idx]][row_idx]
  )
}

#' @param id Module id.
#' @param fd Filled DB list (reactives not required; static list is fine).
#' @param trace_db Trace DB list or NULL.
#' @param fillplan,impplan,saveplan_full Plans / saveplan for tracing.
#' @param selected_node_ids Reactive character vector of indicator node ids.
#' @param country_ids Reactive character vector of country ids (may be empty).
#' @param year_from,year_to Reactive integer or NULL.
mod_value_trace_server <- function(id,
                                   fd,
                                   trace_db,
                                   fillplan,
                                   impplan,
                                   saveplan_full,
                                   selected_node_ids,
                                   country_ids,
                                   year_from,
                                   year_to) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    preview_active    <- shiny::reactiveVal(FALSE)
    preview_cell      <- shiny::reactiveVal(NULL)
    trace_active      <- shiny::reactiveVal(FALSE)
    trace_build_error <- shiny::reactiveVal(NULL)

    preview_sheets <- shiny::reactive({
      shiny::req(preview_active())
      node_ids <- selected_node_ids()
      shiny::req(node_ids)

      build_custom_download_time_in_columns(
        fd                = fd,
        selected_node_ids = node_ids,
        country_ids       = country_ids(),
        year_from         = year_from(),
        year_to           = year_to()
      )
    })

    preview_freqs <- shiny::reactive({
      sheets <- preview_sheets()
      names(sheets)[names(sheets) != "dict"]
    })

    shiny::observeEvent(input$preview_data, {
      node_ids <- selected_node_ids()
      if (is.null(node_ids) || length(node_ids) == 0) {
        shiny::showNotification(
          "Select at least one indicator to preview.",
          type = "warning",
          duration = 4
        )
        return()
      }

      preview_active(TRUE)
      preview_cell(NULL)
      trace_active(FALSE)
      trace_build_error(NULL)
      shinyjs::hide("trace_section")
    })

    preview_table_info <- shiny::reactive({
      shiny::req(preview_active())

      freqs <- preview_freqs()
      shiny::req(length(freqs) > 0)

      freq <- input$preview_freq
      if (is.null(freq) || !nzchar(freq) || !(freq %in% freqs)) {
        freq <- freqs[[1]]
      }

      sheets <- preview_sheets()
      shiny::req(sheets[[freq]])

      info <- truncate_preview_table(sheets[[freq]])
      info$freq <- freq
      info
    })

    output$preview_main <- shiny::renderUI({
      if (!isTRUE(preview_active())) {
        return(shiny::tags$p(
          style = "opacity: 0.8; margin-top: 12px;",
          "Click Preview data on Custom query to build a preview table here."
        ))
      }

      shiny::tagList(
        shiny::tags$h4("Preview data"),
        shiny::tags$p(
          style = "opacity: 0.75; font-size: 12.5px; margin-top: -4px;",
          "Click a value cell to open the calculation trace below."
        ),
        shiny::uiOutput(ns("preview_freq_ui")),
        shiny::textOutput(ns("preview_warning")),
        shiny::uiOutput(ns("preview_table_host"))
      )
    })

    output$preview_freq_ui <- shiny::renderUI({
      shiny::req(preview_active())
      freqs <- preview_freqs()
      shiny::validate(
        shiny::need(length(freqs) > 0, "No data available for preview with the current filters.")
      )

      # Isolate so changing the radio does not rebuild the control (which
      # briefly NULLs the input and snaps the table back to the first freq).
      selected <- shiny::isolate(input$preview_freq) %||% freqs[[1]]
      if (!selected %in% freqs) selected <- freqs[[1]]

      shiny::radioButtons(
        inputId  = ns("preview_freq"),
        label    = "Frequency",
        choices  = freqs,
        selected = selected,
        inline   = TRUE
      )
    })

    output$preview_warning <- shiny::renderText({
      info <- preview_table_info()
      msgs <- character(0)

      if (isTRUE(info$row_truncated)) {
        msgs <- c(
          msgs,
          glue::glue(
            "Showing first {PREVIEW_MAX_ROWS} rows of {info$total_rows}; download the file for the full export."
          )
        )
      }
      if (isTRUE(info$col_truncated)) {
        msgs <- c(
          msgs,
          glue::glue(
            "Showing first {PREVIEW_MAX_COLS} columns of {info$total_cols}; download the file for the full export."
          )
        )
      }

      paste(msgs, collapse = " ")
    })

    output$preview_table_host <- shiny::renderUI({
      shiny::req(preview_active())
      freqs <- preview_freqs()
      shiny::req(length(freqs) > 0)
      # Do not depend on the selected frequency: recreating DTOutput on each
      # switch drops the redraw and leaves the first table on screen.
      DT::DTOutput(ns("preview_table"))
    })

    output$preview_table <- DT::renderDT({
      info <- preview_table_info()
      df <- info$data
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "No rows to display for this frequency.")
      )

      dt <- DT::datatable(
        df,
        selection = list(mode = "single", target = "cell"),
        rownames   = FALSE,
        options = list(
          scrollX    = TRUE,
          pageLength = 25,
          autoWidth  = TRUE
        )
      )
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      if (length(num_cols) > 0L) {
        dt <- DT::formatRound(dt, columns = num_cols, digits = 2L)
      }
      dt
    })

    shiny::observeEvent(input$preview_table_cells_selected, {
      sel <- input$preview_table_cells_selected
      info <- tryCatch(preview_table_info(), error = function(e) NULL)
      if (is.null(info)) {
        preview_cell(NULL)
        return()
      }

      cell <- .parse_preview_cell_selection(sel, info)
      if (is.null(cell)) {
        preview_cell(NULL)
        return()
      }

      preview_cell(cell)
      trace_build_error(NULL)
      shinyjs::show("trace_section")
      trace_active(TRUE)
    })

    shiny::observeEvent(input$trace_operand_drilldown, {
      req <- input$trace_operand_drilldown
      if (is.null(req) || !is.list(req)) return()
      shiny::req(!is.null(trace_db))

      journal <- tryCatch(
        buildValueTrace(
          country_id     = req$country_id,
          indicator_code = req$indicator_code,
          frequency      = req$frequency,
          period         = req$period,
          trace_db       = trace_db,
          fillplan       = fillplan,
          impplan        = impplan,
          saveplan_full  = saveplan_full,
          max_depth      = 5L,
          max_nodes      = 80L
        ),
        error = function(e) tibble::tibble()
      )

      session$sendCustomMessage(
        "tl_operand_drilldown",
        list(
          step_id = req$step_id,
          period  = req$period,
          html    = render_operand_drilldown_html(req, journal)
        )
      )
    }, ignoreInit = TRUE)

    trace_journal_raw <- shiny::reactive({
      cell <- preview_cell()
      shiny::req(cell, !is.null(trace_db))

      journal <- tryCatch(
        buildValueTrace(
          country_id     = cell$country_id,
          indicator_code = cell$indicator_code,
          frequency      = cell$frequency,
          period         = cell$period,
          trace_db       = trace_db,
          fillplan       = fillplan,
          impplan        = impplan,
          saveplan_full  = saveplan_full
        ),
        error = function(e) {
          trace_build_error(conditionMessage(e))
          tibble::tibble()
        }
      )
      if (nrow(journal) > 0L) {
        trace_build_error(NULL)
      }
      journal
    })

    .trace_status_message <- function() {
      err <- trace_build_error()
      if (!is.null(err) && nzchar(err)) {
        return(paste("Trace error:", err))
      }
      if (is.null(trace_db)) {
        return("Trace_DB.rds not found. Run do_fill.R to generate the trace database.")
      }
      if (is.null(preview_cell())) {
        return("Click a value cell in the preview table to trace its calculation.")
      }
      if (isTRUE(trace_active())) {
        journal <- tryCatch(trace_journal_raw(), error = function(e) NULL)
        if (!is.null(journal) && nrow(journal) == 0L) {
          return("No trace steps to display for this cell.")
        }
      }
      ""
    }

    output$trace_unavailable <- shiny::renderText({
      .trace_status_message()
    })

    .indicator_label_for_cell <- function(cell) {
      ind_label <- saveplan_full |>
        dplyr::filter(
          .data$indicator_code == cell$indicator_code,
          .data$source_frequency == cell$frequency
        ) |>
        dplyr::pull(.data$indicator) |>
        dplyr::first()
      if (is.null(ind_label) || is.na(ind_label) || ind_label == "") {
        return(cell$indicator_code)
      }
      ind_label
    }

    shiny::observeEvent(input$trace_show_technical_force, {
      shiny::updateCheckboxInput(session, "trace_show_technical", value = TRUE)
    }, ignoreInit = TRUE)

    output$trace_selected_card <- shiny::renderUI({
      shiny::req(trace_active())
      cell <- preview_cell()
      shiny::req(cell)
      render_trace_ledger_selected_card(cell, .indicator_label_for_cell(cell))
    })

    output$trace_tree_host <- shiny::renderUI({
      shiny::req(trace_active())
      cell <- preview_cell()
      shiny::req(cell, !is.null(trace_db))

      journal <- tryCatch(trace_journal_raw(), error = function(e) tibble::tibble())
      shiny::validate(shiny::need(nrow(journal) > 0, "No trace steps to display."))

      show_tech <- if (is.null(input$trace_show_technical)) {
        TRUE
      } else {
        isTRUE(input$trace_show_technical)
      }

      vm <- build_trace_ledger_vm(
        journal = journal,
        saveplan_full = saveplan_full,
        show_technical = show_tech
      )

      render_trace_ledger_tree(
        vm = vm,
        cell = NULL,
        indicator_label = NULL,
        show_technical_input_id = ns("trace_show_technical_force"),
        drilldown_input_id = ns("trace_operand_drilldown")
      )
    })

    shiny::observeEvent(input$trace_collapse_all, {
      invisible(NULL)
    }, ignoreInit = TRUE)

    output$download_trace <- shiny::downloadHandler(
      filename = function() {
        cell <- preview_cell()
        shiny::req(cell)
        glue::glue(
          "trace_{cell$country_id}_{cell$indicator_code}_{cell$frequency}_{cell$period}.xlsx"
        )
      },
      content = function(file) {
        cell <- preview_cell()
        shiny::req(cell, !is.null(trace_db))

        journal <- trace_journal_raw()
        header <- tibble::tibble(
          field = c("country", "country_id", "indicator_code", "frequency", "period", "value"),
          value = c(
            cell$country, cell$country_id, cell$indicator_code,
            cell$frequency, cell$period, as.character(cell$value)
          )
        )

        write_xlsx_formatted(
          sheets = list(header = header, trace = journal),
          path   = file
        )
      }
    )

    shiny::observeEvent(
      list(selected_node_ids(), country_ids(), year_from(), year_to()),
      {
        preview_active(FALSE)
        preview_cell(NULL)
        trace_active(FALSE)
        trace_build_error(NULL)
        shinyjs::hide("trace_section")
      },
      ignoreInit = TRUE
    )

    shiny::outputOptions(output, "preview_table", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "trace_tree_host", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}
