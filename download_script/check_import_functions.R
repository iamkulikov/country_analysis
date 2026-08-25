# Check import-structure contracts against downloaded files and (optionally) APIs.
#
# Entry point: checkImportFunctions()
# Side-effect writer: writeImportCheckReport()

##### Setup -------------------------------------------------------------------

here::i_am("download_script/check_import_functions.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(data.table)
  library(here)
  library(purrr)
  library(rlang)
})

source(here("download_script", "import_contracts.R"))
source(here("download_script", "imf_tool.R"))
source(here("download_script", "owid_tool.R"))

# WDI / ILO are only needed for online probes; load quietly when available
.has_pkg <- function(pkg) requireNamespace(pkg, quietly = TRUE)

##### Report row helper -------------------------------------------------------

#' One check-result row.
#'
#' @param contract_id Character id or NA for global/orphan checks.
#' @param label Human label.
#' @param level Check family: plan / code / file / sheet / structure / codes / api / probe.
#' @param status One of fail / warn / info / skip / ok.
#' @param message Free-text detail.
#' @param detail Optional extra (file path, missing cols, etc.).
check_row <- function(contract_id,
                      label,
                      level,
                      status,
                      message,
                      detail = NA_character_) {
  tibble::tibble(
    contract_id = as.character(contract_id %||% NA_character_),
    label       = as.character(label %||% NA_character_),
    level       = as.character(level),
    status      = as.character(status),
    message     = as.character(message),
    detail      = as.character(detail %||% NA_character_)
  )
}

empty_check_report <- function() {
  check_row(NA, NA, "plan", "info", "empty")[0, ]
}

##### Path / header utilities -------------------------------------------------

extsources_dir <- function() {
  here::here("assets", "_DB", "_extsources")
}

#' Resolve file path(s) for a contract given filtered plan rows.
#'
#' @return Character vector of absolute paths (may be length 0).
resolve_contract_paths <- function(contract, plan, local_fnames = NULL) {
  if (identical(contract$id, "weo_vintages")) {
    return(here::here("assets", "_DB", "_extsources", "WEO_vintages"))
  }
  if (identical(contract$kind, "local")) {
    return(as.character(local_fnames %||% character()))
  }
  if (identical(contract$kind, "api")) {
    return(character())
  }

  fnames <- if (!is.null(contract$file_name) && nzchar(contract$file_name)) {
    contract$file_name
  } else {
    unique(stats::na.omit(as.character(plan$file_name)))
  }
  fnames <- fnames[nzchar(fnames)]
  if (!length(fnames)) return(character())
  file.path(extsources_dir(), fnames)
}

#' Fast header read: Excel via n_max = 0, CSV/other via fread nrows = 0.
#'
#' CSV names are passed through `make.names()` to mirror base `read.csv()`
#' (`check.names = TRUE`), which is what most `import.R` file blocks use.
#'
#' @return Named empty data.frame / data.table, or NULL on failure.
read_file_header <- function(path, sheet = NULL, skip = 0L) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  skip <- as.integer(skip %||% 0L)
  if (is.na(skip) || skip < 0L) skip <- 0L

  ext <- tolower(tools::file_ext(path))
  tryCatch(
    {
      if (ext %in% c("xlsx", "xls", "xlsm")) {
        sh <- sheet
        if (is.null(sh) || !nzchar(as.character(sh))) {
          sh <- 1L
        } else if (identical(as.character(sh), "1")) {
          sh <- 1L
        }
        readxl::read_excel(
          path,
          sheet = sh,
          skip = skip,
          n_max = 0,
          col_names = TRUE,
          .name_repair = "unique"
        )
      } else {
        hdr <- data.table::fread(path, nrows = 0, skip = skip, showProgress = FALSE)
        # Match read.csv() name mangling (BIS SDMX-CSV: "REF_AREA:Reference area"
        # → "REF_AREA.Reference.area")
        names(hdr) <- make.names(names(hdr), unique = TRUE)
        hdr
      }
    },
    error = function(e) NULL
  )
}

#' Apply optional col_transform (e.g. janitor::clean_names) to a 0-row header.
transform_header_names <- function(hdr, col_transform) {
  if (is.null(hdr)) return(NULL)
  if (is.null(col_transform)) return(hdr)
  # clean_names needs a data.frame-like object
  out <- tryCatch(col_transform(as.data.frame(hdr)), error = function(e) NULL)
  if (is.null(out)) return(hdr)
  out
}

file_age_days <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "days"))
}

##### Drift: import.R section headers ----------------------------------------

#' Extract active `##### Import ...` section titles from import.R.
#'
#' Only headers whose next non-blank line is not a comment (and that are
#' followed by an uncommented `try(` within 20 lines) are kept. Legacy titles
#' above fully commented-out blocks (old WGI) are ignored.
listImportSectionTitles <- function(import_path = here("download_script", "import.R")) {
  lines <- readLines(import_path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("\r$", "", lines)
  # Indented inside tryImport; keep trailing spaces (CDS: "Import data on CDS ")
  hdr_idx <- which(stringr::str_detect(lines, "^\\s*#####\\s+Import\\s+"))
  titles <- character()
  for (i in hdr_idx) {
    title <- stringr::str_match(lines[[i]], "^\\s*#####\\s+(Import\\s+.*)$")[, 2]
    if (is.na(title)) next
    window <- lines[seq(i + 1L, min(length(lines), i + 20L))]
    nonblank <- window[nzchar(trimws(window))]
    if (!length(nonblank)) next
    if (stringr::str_detect(nonblank[[1]], "^\\s*#")) next
    try_lines <- nonblank[stringr::str_detect(nonblank, "try\\s*\\(")]
    if (!length(try_lines)) next
    if (stringr::str_detect(try_lines[[1]], "^\\s*#")) next
    titles <- c(titles, title)
  }
  unique(titles)
}

#' Compare registry `code_block` values with import.R section headers.
check_code_drift <- function(contracts, import_path = here("download_script", "import.R")) {
  rows <- list()
  titles <- listImportSectionTitles(import_path)
  reg_blocks <- vapply(contracts, `[[`, character(1), "code_block")
  reg_ids <- names(contracts)

  for (i in seq_along(contracts)) {
    cb <- reg_blocks[[i]]
    id <- reg_ids[[i]]
    lab <- contracts[[i]]$label
    if (cb %in% titles) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "code", "ok",
        sprintf("code_block found in import.R: '%s'", cb)
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "code", "warn",
        sprintf("code_block missing from import.R: '%s'", cb)
      )
    }
  }

  orphan_titles <- setdiff(titles, reg_blocks)
  for (t in orphan_titles) {
    rows[[length(rows) + 1L]] <- check_row(
      NA_character_, t, "code", "warn",
      sprintf("import.R section has no contract: '%s'", t)
    )
  }

  dplyr::bind_rows(rows)
}

##### Plan coverage / orphans -------------------------------------------------

#' Active plan rows matched by any contract; orphans = unmatched actives.
#'
#' @return list(matched = named list of plan tibbles, orphans = tibble, rows = check rows)
check_plan_coverage <- function(contracts, impplan) {
  n <- nrow(impplan)
  covered <- rep(FALSE, n)
  matched <- list()
  rows <- list()

  for (id in names(contracts)) {
    c <- contracts[[id]]
    hit <- tryCatch(c$match(impplan), error = function(e) {
      warning(sprintf("match() failed for contract '%s': %s", id, e$message))
      rep(FALSE, n)
    })
    hit <- as.logical(hit)
    if (length(hit) != n) {
      rows[[length(rows) + 1L]] <- check_row(
        id, c$label, "plan", "fail",
        sprintf("match() returned length %d, expected %d", length(hit), n)
      )
      hit <- rep(FALSE, n)
    }
    plan_i <- impplan[hit & !is.na(hit), , drop = FALSE]
    matched[[id]] <- plan_i
    covered[hit & !is.na(hit)] <- TRUE

    if (nrow(plan_i) == 0L) {
      rows[[length(rows) + 1L]] <- check_row(
        id, c$label, "plan", "info",
        "No active impplan rows for this contract"
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, c$label, "plan", "ok",
        sprintf("%d active row(s) matched", nrow(plan_i))
      )
    }
  }

  orphans <- impplan[!covered, , drop = FALSE]
  if (nrow(orphans) > 0L) {
    for (i in seq_len(nrow(orphans))) {
      r <- orphans[i, , drop = FALSE]
      rows[[length(rows) + 1L]] <- check_row(
        NA_character_,
        paste(r$indicator_code %||% "?", r$database_name %||% "?", sep = " / "),
        "plan",
        "fail",
        "Active impplan row matches no contract (will never be imported by tryImport)",
        detail = paste(
          c(
            if (!is.null(r$source_name)) paste0("source=", r$source_name),
            if (!is.null(r$database_name)) paste0("db=", r$database_name),
            if (!is.null(r$retrieve_type)) paste0("type=", r$retrieve_type),
            if (!is.null(r$source_frequency)) paste0("freq=", r$source_frequency),
            if (!is.null(r$file_name)) paste0("file=", r$file_name),
            if (!is.null(r$indicator_code)) paste0("ind=", r$indicator_code)
          ),
          collapse = "; "
        )
      )
    }
  }

  list(matched = matched, orphans = orphans, rows = dplyr::bind_rows(rows))
}

##### File / sheet / structure / codes ----------------------------------------

check_file_level <- function(contract, paths) {
  rows <- list()
  id <- contract$id
  lab <- contract$label

  if (identical(contract$kind, "api")) {
    return(empty_check_report())
  }

  if (identical(contract$id, "weo_vintages")) {
    path <- paths[[1]] %||% here::here("assets", "_DB", "_extsources", "WEO_vintages")
    if (!dir.exists(path)) {
      rows[[1]] <- check_row(id, lab, "file", "fail", paste("WEO vintages directory missing:", path))
    } else {
      n_files <- length(fs::dir_ls(path, regexp = "\\d{4}-[12]\\.xls[x]?$", type = "file"))
      rows[[1]] <- check_row(
        id, lab, "file", if (n_files > 0) "ok" else "fail",
        sprintf("WEO vintages dir: %d matching file(s)", n_files),
        detail = path
      )
    }
    return(dplyr::bind_rows(rows))
  }

  if (!length(paths)) {
    return(check_row(id, lab, "file", "fail", "No file_name in plan or contract"))
  }

  for (path in paths) {
    if (!file.exists(path)) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "file", "fail",
        paste("File missing:", basename(path)),
        detail = path
      )
    } else {
      age <- file_age_days(path)
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "file", "ok",
        sprintf("File exists (age %.1f days)", age),
        detail = path
      )
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "file", "info",
        sprintf("mtime age: %.1f days", age),
        detail = path
      )
    }
  }
  dplyr::bind_rows(rows)
}

check_sheet_level <- function(contract, plan, paths) {
  id <- contract$id
  lab <- contract$label
  if (is.null(contract$sheets)) return(empty_check_report())
  if (identical(contract$kind, "api")) return(empty_check_report())
  if (identical(contract$id, "weo_vintages")) return(empty_check_report()) # probe handles

  want <- tryCatch(contract$sheets(plan), error = function(e) character())
  want <- unique(as.character(stats::na.omit(want)))
  want <- want[nzchar(want)]
  if (!length(want)) return(empty_check_report())

  # Local: probe checks sheets per country file
  if (identical(contract$kind, "local")) return(empty_check_report())

  rows <- list()
  existing_paths <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(existing_paths)) {
    return(check_row(id, lab, "sheet", "skip", "No existing file to list sheets"))
  }

  for (path in existing_paths) {
    ext <- tolower(tools::file_ext(path))
    if (!ext %in% c("xlsx", "xls", "xlsm")) next
    have <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
    # Numeric sheet "1" means first sheet — always ok if workbook opens
    need <- want[want != "1"]
    missing <- setdiff(need, have)
    if (length(missing)) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "sheet", "fail",
        sprintf(
          "Missing sheet(s) in %s: %s",
          basename(path), paste(missing, collapse = ", ")
        ),
        detail = paste("have:", paste(have, collapse = ", "))
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "sheet", "ok",
        sprintf("Sheets OK in %s: %s", basename(path), paste(want, collapse = ", "))
      )
    }
  }
  if (!length(rows)) return(empty_check_report())
  dplyr::bind_rows(rows)
}

check_structure_level <- function(contract, plan, paths) {
  id <- contract$id
  lab <- contract$label
  if (identical(contract$kind, "api")) return(empty_check_report())
  if (identical(contract$kind, "local")) return(empty_check_report())
  if (identical(contract$id, "weo_vintages")) return(empty_check_report()) # probe
  # Conference Board / CT / HRT rely on probes more than a single named header
  if (identical(contract$id, "conference_board")) return(empty_check_report())
  if (identical(contract$id, "ct")) return(empty_check_report())
  if (identical(contract$id, "hrt")) return(empty_check_report())

  need_req <- contract$required_cols %||% character()
  need_opt <- contract$optional_cols %||% character()
  pat <- contract$col_pattern
  has_any_anchor <- length(need_req) || length(need_opt) || !is.null(pat)
  if (!has_any_anchor) return(empty_check_report())

  existing <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(existing)) {
    return(check_row(id, lab, "structure", "skip", "No file available for header read"))
  }

  sheets <- if (!is.null(contract$sheets)) {
    tryCatch(contract$sheets(plan), error = function(e) character())
  } else {
    character()
  }
  sheets <- unique(as.character(stats::na.omit(sheets)))
  if (!length(sheets)) sheets <- list(NULL)

  rows <- list()
  path <- existing[[1]]
  sheet <- sheets[[1]]
  hdr <- read_file_header(path, sheet = sheet, skip = contract$skip %||% 0L)
  hdr <- transform_header_names(hdr, contract$col_transform)

  if (is.null(hdr)) {
    return(check_row(
      id, lab, "structure", "fail",
      sprintf("Could not read header from %s (sheet=%s, skip=%s)",
              basename(path), sheet %||% "1", contract$skip %||% 0L)
    ))
  }

  nm <- names(hdr)

  if (length(need_req)) {
    miss <- setdiff(need_req, nm)
    if (length(miss)) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "fail",
        paste("Missing required column(s):", paste(miss, collapse = ", ")),
        detail = paste("have:", paste(utils::head(nm, 40), collapse = ", "))
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "ok",
        paste("Required columns present:", paste(need_req, collapse = ", "))
      )
    }
  }

  if (length(need_opt)) {
    miss_opt <- setdiff(need_opt, nm)
    if (length(miss_opt)) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "warn",
        paste("Missing optional column(s):", paste(miss_opt, collapse = ", "))
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "ok",
        "All optional columns present"
      )
    }
  }

  if (!is.null(pat) && nzchar(pat)) {
    hit <- nm[stringr::str_detect(nm, pat)]
    if (!length(hit)) {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "fail",
        sprintf("No columns match col_pattern '%s'", pat)
      )
    } else {
      rows[[length(rows) + 1L]] <- check_row(
        id, lab, "structure", "ok",
        sprintf("%d column(s) match col_pattern", length(hit)),
        detail = paste(utils::head(hit, 12), collapse = ", ")
      )
    }
  }

  if (!length(rows)) return(empty_check_report())
  dplyr::bind_rows(rows)
}

#' Deep check: retrieve_code values appear in code_col (reads real data).
check_codes_level <- function(contract, plan, paths, deep = FALSE) {
  id <- contract$id
  lab <- contract$label
  code_col <- contract$code_col
  if (is.null(code_col) || !nzchar(code_col)) return(empty_check_report())
  if (!isTRUE(deep)) {
    return(check_row(id, lab, "codes", "skip", "deep = FALSE; codes check skipped"))
  }
  if (identical(contract$kind, "api")) {
    return(check_row(id, lab, "codes", "skip", "codes check not applicable to API contracts here"))
  }

  existing <- paths[file.exists(paths) & !dir.exists(paths)]
  if (!length(existing) && identical(contract$id, "weo_vintages")) {
    dir <- paths[[1]] %||% here::here("assets", "_DB", "_extsources", "WEO_vintages")
    existing <- as.character(fs::dir_ls(dir, regexp = "\\d{4}-[12]\\.xls[x]?$", type = "file"))
  }
  if (!length(existing)) {
    return(check_row(id, lab, "codes", "skip", "No file for codes check"))
  }

  codes <- unique(stats::na.omit(as.character(plan$retrieve_code)))
  codes <- codes[nzchar(codes)]
  # WGI retrieve_code is type/dimension — skip deep value check unless code_col set meaningfully
  if (!length(codes)) {
    return(check_row(id, lab, "codes", "info", "No retrieve_code values in plan"))
  }

  sheets <- if (!is.null(contract$sheets)) {
    tryCatch(contract$sheets(plan), error = function(e) character())
  } else {
    character()
  }
  sheet <- if (length(sheets)) sheets[[1]] else NULL
  if (identical(as.character(sheet), "1")) sheet <- 1L

  path <- existing[[1]]
  ext <- tolower(tools::file_ext(path))
  skip <- as.integer(contract$skip %||% 0L)

  # WEO vintages: codes must appear in at least one vintage file (oldest file
  # alone is a false fail — subject codes are added over time).
  if (identical(contract$id, "weo_vintages")) {
    col_candidates <- c(
      "WEO Subject Code", "WEO.Subject.Code", "Subject Code", "Subject.Code", "subject_code"
    )
    have <- character()
    n_read <- 0L
    for (p in existing) {
      dat_i <- tryCatch(
        readxl::read_excel(
          p, sheet = 1L, skip = 0L,
          col_names = TRUE, .name_repair = "unique"
        ),
        error = function(e) NULL
      )
      if (is.null(dat_i)) next
      col_use <- col_candidates[col_candidates %in% names(dat_i)][1]
      if (is.na(col_use) || !nzchar(col_use)) next
      have <- union(have, unique(as.character(dat_i[[col_use]])))
      n_read <- n_read + 1L
    }
    if (!n_read) {
      return(check_row(id, lab, "codes", "fail", "Could not read any WEO vintage file for codes check"))
    }
    miss <- setdiff(codes, have)
    if (length(miss)) {
      return(check_row(
        id, lab, "codes", "fail",
        sprintf(
          "%d retrieve_code(s) missing from WEO Subject Code across %d vintage file(s)",
          length(miss), n_read
        ),
        detail = paste(utils::head(miss, 20), collapse = ", ")
      ))
    }
    return(check_row(
      id, lab, "codes", "ok",
      sprintf("All %d retrieve_code(s) found across %d vintage file(s)", length(codes), n_read)
    ))
  }

  dat <- tryCatch(
    {
      if (ext %in% c("xlsx", "xls", "xlsm")) {
        readxl::read_excel(
          path, sheet = sheet %||% 1L, skip = skip,
          col_names = TRUE, .name_repair = "unique"
        )
      } else {
        data.table::fread(path, skip = skip, showProgress = FALSE)
      }
    },
    error = function(e) NULL
  )
  if (is.null(dat)) {
    return(check_row(id, lab, "codes", "fail", paste("Failed to read data from", basename(path))))
  }
  if (!is.null(contract$col_transform)) {
    dat <- tryCatch(contract$col_transform(as.data.frame(dat)), error = function(e) dat)
  }
  col_candidates <- code_col
  col_use <- col_candidates[col_candidates %in% names(dat)][1]
  if (is.na(col_use) || !nzchar(col_use)) {
    return(check_row(
      id, lab, "codes", "fail",
      paste("code_col not found:", paste(col_candidates, collapse = " | "))
    ))
  }

  have <- unique(as.character(dat[[col_use]]))
  # For WGI-like type/dimension codes take the part after /
  check_codes <- codes
  if (any(stringr::str_detect(check_codes, "/"))) {
    check_codes <- unique(c(check_codes, stringr::str_split_fixed(check_codes, "/", 2)[, 2]))
  }
  miss <- setdiff(check_codes, have)
  # Soften: if retrieve_code has path-like values, only require dimension half
  if (length(miss) && any(stringr::str_detect(codes, "/"))) {
    miss <- setdiff(stringr::str_split_fixed(codes, "/", 2)[, 2], have)
  }

  if (length(miss)) {
    check_row(
      id, lab, "codes", "fail",
      sprintf("%d retrieve_code(s) missing from '%s'", length(miss), col_use),
      detail = paste(utils::head(miss, 20), collapse = ", ")
    )
  } else {
    check_row(
      id, lab, "codes", "ok",
      sprintf("All %d retrieve_code(s) found in '%s'", length(codes), col_use)
    )
  }
}

##### Contract probe (registry probe field) -----------------------------------

run_registry_probe <- function(contract, plan, paths, local_fnames = NULL, online = TRUE) {
  if (is.null(contract$probe)) return(empty_check_report())
  # Network registry probes (BIS debt CSV URL) only when online
  if (identical(contract$id, "bis_debt") && !isTRUE(online)) {
    return(check_row(
      contract$id, contract$label, "probe", "skip",
      "online = FALSE; BIS debt URL probe skipped"
    ))
  }
  ctx <- list(
    plan = plan,
    path = if (length(paths)) paths[[1]] else NULL,
    paths = paths,
    contract = contract,
    sheet = if (!is.null(contract$sheets)) {
      tryCatch(contract$sheets(plan)[1], error = function(e) NULL)
    } else {
      NULL
    },
    local_fnames = local_fnames
  )
  res <- tryCatch(
    contract$probe(ctx),
    error = function(e) list(ok = FALSE, level = "fail", message = paste("probe error:", e$message))
  )
  status <- if (isTRUE(res$ok)) {
    if (!is.null(res$level) && res$level %in% c("info", "warn", "ok")) res$level else "ok"
  } else {
    res$level %||% "fail"
  }
  check_row(
    contract$id, contract$label, "probe", status,
    res$message %||% "probe finished"
  )
}

##### Online API probes -------------------------------------------------------

#' WDI probe: one cheap WDI() call per yearly/quarterly family.
probe_wdi <- function(plan, quarterly = FALSE) {
  if (!.has_pkg("WDI")) {
    return(list(ok = FALSE, level = "fail", message = "Package WDI not installed"))
  }
  codes <- unique(stats::na.omit(as.character(plan$retrieve_code)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) {
    return(list(ok = FALSE, level = "fail", message = "No WDI retrieve_code in plan"))
  }
  code <- codes[[1]]
  dat <- tryCatch(
    WDI::WDI(indicator = code, start = 2020, end = 2021, extra = FALSE),
    error = function(e) e
  )
  if (inherits(dat, "error")) {
    return(list(ok = FALSE, level = "fail", message = paste("WDI() failed:", dat$message)))
  }
  nm <- names(dat)
  # Yearly WDI: iso2c + year + indicator. Quarterly may omit iso2c and keep iso3c.
  if (isTRUE(quarterly)) {
    if (!"year" %in% nm) {
      return(list(ok = FALSE, level = "fail", message = "WDI-q response missing year"))
    }
    if (!code %in% nm) {
      return(list(ok = FALSE, level = "fail", message = paste("WDI-q missing indicator column", code)))
    }
    years <- as.character(dat$year)
    q_ok <- any(stringr::str_detect(years, "^\\d{4}Q[1-4]$"), na.rm = TRUE)
    if (!q_ok) {
      return(list(
        ok = FALSE, level = "fail",
        message = "WDI-q: year is not in YYYYQn form"
      ))
    }
    # Import: prefer iso3c when present; else iso2c holding iso3 codes.
    if ("iso3c" %in% nm || "iso2c" %in% nm) {
      sample_iso <- if ("iso3c" %in% nm) {
        as.character(stats::na.omit(dat$iso3c))[1]
      } else {
        as.character(stats::na.omit(dat$iso2c))[1]
      }
      iso3_like <- !is.na(sample_iso) && nchar(sample_iso) == 3L
      if (!iso3_like) {
        return(list(
          ok = FALSE, level = "warn",
          message = sprintf(
            "WDI-q: country code sample '%s' is not 3-letter iso3",
            sample_iso %||% "NA"
          )
        ))
      }
      return(list(
        ok = TRUE, level = "info",
        message = sprintf(
          "WDI-q OK for %s (YYYYQn + iso3 via %s)",
          code,
          if ("iso3c" %in% nm) "iso3c" else "iso2c"
        )
      ))
    }
    return(list(ok = FALSE, level = "fail", message = "WDI-q: neither iso2c nor iso3c in response"))
  }

  need <- c("iso2c", "year", code)
  miss <- setdiff(need, nm)
  if (length(miss)) {
    return(list(
      ok = FALSE, level = "fail",
      message = paste("WDI response missing columns:", paste(miss, collapse = ", "))
    ))
  }
  list(ok = TRUE, level = "info", message = sprintf("WDI-y OK for %s", code))
}

#' IMF SDMX: one code per database_name.
#'
#' Real import uses `imf_build_url()` with country stub `*.` (all countries /
#' series). A cheap single-country substitution (USA/BRA/…) is only an
#' optimization: some flows (PCPS commodities, FAD/FM aggregates) have no
#' ISO3 country series and only answer to `*.` — falling back to the wildcard
#' URL matches production `imfTool()` behaviour.
probe_imf_sdmx <- function(plan, freq = "y") {
  if (!length(unique(stats::na.omit(plan$database_name)))) {
    return(list(ok = FALSE, level = "fail", message = "No IMF database_name in plan"))
  }
  # One row per database_name
  by_db <- plan |>
    dplyr::filter(!is.na(database_name), !is.na(retrieve_code)) |>
    dplyr::distinct(database_name, .keep_all = TRUE)

  msgs_ok <- character()
  msgs_fail <- character()
  for (i in seq_len(nrow(by_db))) {
    db <- by_db$database_name[[i]]
    code <- by_db$retrieve_code[[i]]
    url <- tryCatch(
      imf_build_url(database = db, code = code, freq = freq),
      error = function(e) e
    )
    if (inherits(url, "error")) {
      msgs_fail <- c(msgs_fail, sprintf("%s build: %s", db, url$message))
      next
    }

    df <- NULL
    last_err <- NULL
    used <- NULL
    # Prefer cheap single-country probe when the key still has `*.`
    if (grepl("/%2B/\\*\\.", url, perl = TRUE)) {
      for (cc in c("USA", "BRA", "DEU", "GBR")) {
        cheap_url <- sub("/%2B/*.", paste0("/%2B/", cc, "."), url, fixed = TRUE)
        got <- tryCatch(
          imf_fetch_simple(cheap_url, normalize_units = "none", include_meta = "none"),
          error = function(e) e
        )
        if (!inherits(got, "error")) {
          df <- got
          used <- cc
          break
        }
        last_err <- got
      }
    }
    # Fallback: same wildcard URL that imfTool / import uses
    if (is.null(df)) {
      got <- tryCatch(
        imf_fetch_simple(url, normalize_units = "none", include_meta = "none"),
        error = function(e) e
      )
      if (!inherits(got, "error")) {
        df <- got
        used <- "*"
      } else {
        last_err <- got
      }
    }

    if (is.null(df)) {
      msgs_fail <- c(
        msgs_fail,
        sprintf("%s / %s: %s", db, code, last_err$message %||% "fetch failed")
      )
      next
    }
    need <- c("iso2", "year", "value")
    miss <- setdiff(need, names(df))
    if (length(miss)) {
      msgs_fail <- c(
        msgs_fail,
        sprintf("%s missing cols: %s", db, paste(miss, collapse = ", "))
      )
      next
    }
    msgs_ok <- c(
      msgs_ok,
      sprintf("%s OK (%d rows, key=%s)", db, nrow(df), used %||% "?")
    )
  }

  if (length(msgs_fail) && !length(msgs_ok)) {
    return(list(
      ok = FALSE, level = "fail",
      message = paste(msgs_fail, collapse = "; ")
    ))
  }
  if (length(msgs_fail) && length(msgs_ok)) {
    return(list(
      ok = TRUE, level = "warn",
      message = paste(
        "Some IMF databases failed probe:",
        paste(msgs_fail, collapse = "; "),
        "| OK:", paste(msgs_ok, collapse = "; ")
      )
    ))
  }
  list(ok = TRUE, level = "info", message = paste(msgs_ok, collapse = "; "))
}

#' OWID Chart API: metadata.json for unique retrieve_code; column must exist.
probe_owid <- function(plan) {
  codes <- unique(stats::na.omit(as.character(plan$retrieve_code)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) {
    return(list(ok = FALSE, level = "fail", message = "No OWID retrieve_code in plan"))
  }
  if (!.has_pkg("httr") || !.has_pkg("jsonlite")) {
    return(list(
      ok = FALSE, level = "fail",
      message = "Packages httr/jsonlite required for OWID probe"
    ))
  }

  msgs_ok <- character()
  msgs_fail <- character()
  # One metadata fetch per chart (ignore #column for grouping)
  parsed_list <- lapply(codes, function(c) {
    tryCatch(owid_parse_code(c), error = function(e) e)
  })
  for (i in seq_along(codes)) {
    parsed <- parsed_list[[i]]
    if (inherits(parsed, "error")) {
      msgs_fail <- c(msgs_fail, sprintf("%s parse: %s", codes[[i]], parsed$message))
      next
    }
    meta <- tryCatch(owid_fetch_metadata(parsed), error = function(e) e)
    if (inherits(meta, "error")) {
      msgs_fail <- c(msgs_fail, sprintf("%s metadata: %s", codes[[i]], meta$message))
      next
    }
    col <- tryCatch(
      owid_resolve_column(meta, parsed$column),
      error = function(e) e
    )
    if (inherits(col, "error")) {
      msgs_fail <- c(msgs_fail, sprintf("%s column: %s", codes[[i]], col$message))
      next
    }
    msgs_ok <- c(msgs_ok, sprintf("%s OK (column=%s)", codes[[i]], col))
  }

  if (length(msgs_fail) && !length(msgs_ok)) {
    return(list(
      ok = FALSE, level = "fail",
      message = paste(msgs_fail, collapse = "; ")
    ))
  }
  if (length(msgs_fail) && length(msgs_ok)) {
    return(list(
      ok = TRUE, level = "warn",
      message = paste(
        "Some OWID codes failed probe:",
        paste(msgs_fail, collapse = "; "),
        "| OK:", paste(msgs_ok, collapse = "; ")
      )
    ))
  }
  list(ok = TRUE, level = "info", message = paste(msgs_ok, collapse = "; "))
}

#' ILO: codes present in get_ilostat_toc() (no full download).
probe_ilo <- function(plan) {
  if (!.has_pkg("Rilostat")) {
    return(list(ok = FALSE, level = "fail", message = "Package Rilostat not installed"))
  }
  codes <- unique(stats::na.omit(as.character(plan$retrieve_code)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) {
    return(list(ok = FALSE, level = "fail", message = "No ILO retrieve_code in plan"))
  }
  toc <- tryCatch(
    Rilostat::get_ilostat_toc(),
    error = function(e) e
  )
  if (inherits(toc, "error")) {
    return(list(ok = FALSE, level = "fail", message = paste("get_ilostat_toc failed:", toc$message)))
  }
  # toc usually has column `id` or `indicator`
  id_col <- intersect(c("id", "indicator"), names(toc))[1]
  if (is.na(id_col)) {
    return(list(ok = FALSE, level = "fail", message = "ILO toc has no id/indicator column"))
  }
  have <- as.character(toc[[id_col]])
  miss <- setdiff(codes, have)
  if (length(miss)) {
    return(list(
      ok = FALSE, level = "fail",
      message = sprintf("%d ILO code(s) not in toc", length(miss)),
      detail = paste(utils::head(miss, 15), collapse = ", ")
    ))
  }
  list(ok = TRUE, level = "info", message = sprintf("All %d ILO code(s) present in toc", length(codes)))
}

#' Dispatch online API probes for a contract family (one call per family).
run_api_probes <- function(contract, plan, online = TRUE) {
  id <- contract$id
  lab <- contract$label
  if (!identical(contract$kind, "api")) return(empty_check_report())
  if (!isTRUE(online)) {
    return(check_row(id, lab, "api", "skip", "online = FALSE; API probe skipped"))
  }
  if (nrow(plan) == 0L) return(empty_check_report())

  res <- switch(
    id,
    "wdi_y" = probe_wdi(plan, quarterly = FALSE),
    "wdi_q" = probe_wdi(plan, quarterly = TRUE),
    "imf_y" = probe_imf_sdmx(plan, freq = "y"),
    "imf_q" = probe_imf_sdmx(plan, freq = "q"),
    "imf_m" = probe_imf_sdmx(plan, freq = "m"),
    "ilo" = probe_ilo(plan),
    "owid_api" = probe_owid(plan),
    "bis_debt" = {
      # Same check as registry probe_bis_debt_url (CSV header via URL)
      out <- probe_bis_debt_url(list(plan = plan, path = NULL, contract = contract))
      out
    },
    list(ok = TRUE, level = "skip", message = sprintf("No dedicated API probe for '%s'", id))
  )

  status <- if (isTRUE(res$ok)) {
    if (!is.null(res$level) && res$level %in% c("info", "warn", "ok", "skip")) res$level else "ok"
  } else if (!is.null(res$level) && res$level == "skip") {
    "skip"
  } else {
    res$level %||% "fail"
  }
  check_row(
    id, lab, "api", status,
    res$message %||% "api probe finished",
    detail = res$detail %||% NA_character_
  )
}

##### Per-contract runner -----------------------------------------------------

check_one_contract <- function(contract,
                               plan,
                               online = TRUE,
                               deep = FALSE,
                               local_fnames = NULL) {
  # No active plan rows → plan-level already emitted `info`; skip file/API work
  # unless the contract hard-codes a file or is local (always has country files).
  no_plan <- is.null(plan) || !nrow(plan)
  hard_file <- !is.null(contract$file_name) && nzchar(contract$file_name)
  if (no_plan && !hard_file && !identical(contract$kind, "local")) {
    return(empty_check_report())
  }

  paths <- resolve_contract_paths(contract, plan, local_fnames = local_fnames)
  # Avoid double-running BIS debt: registry probe when online, api level records it
  probe_rows <- run_registry_probe(
    contract, plan, paths,
    local_fnames = local_fnames, online = online
  )
  api_rows <- run_api_probes(contract, plan, online = online)
  if (identical(contract$id, "bis_debt") && isTRUE(online)) {
    # Registry probe already hit the URL; keep a single api-level row from that result
    api_rows <- probe_rows |> dplyr::mutate(level = "api")
    probe_rows <- empty_check_report()
  }
  dplyr::bind_rows(
    check_file_level(contract, paths),
    check_sheet_level(contract, plan, paths),
    check_structure_level(contract, plan, paths),
    check_codes_level(contract, plan, paths, deep = deep),
    probe_rows,
    api_rows
  )
}

##### Summary + console (cli) + Excel report ----------------------------------

#' Per-contract rollup of check rows.
summarise_check_report <- function(details) {
  if (is.null(details) || !nrow(details)) {
    return(tibble::tibble(
      contract_id = character(),
      label = character(),
      n_ok = integer(),
      n_warn = integer(),
      n_fail = integer(),
      n_info = integer(),
      n_skip = integer(),
      worst = character()
    ))
  }
  details |>
    dplyr::mutate(
      contract_id = dplyr::coalesce(contract_id, "(global)"),
      label = dplyr::coalesce(label, contract_id)
    ) |>
    dplyr::group_by(contract_id, label) |>
    dplyr::summarise(
      n_ok   = sum(status == "ok", na.rm = TRUE),
      n_warn = sum(status == "warn", na.rm = TRUE),
      n_fail = sum(status == "fail", na.rm = TRUE),
      n_info = sum(status == "info", na.rm = TRUE),
      n_skip = sum(status == "skip", na.rm = TRUE),
      worst  = dplyr::case_when(
        any(status == "fail") ~ "fail",
        any(status == "warn") ~ "warn",
        any(status == "ok")   ~ "ok",
        TRUE ~ "info"
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::case_when(
        worst == "fail" ~ 1L,
        worst == "warn" ~ 2L,
        worst == "ok"   ~ 3L,
        TRUE ~ 4L
      ),
      contract_id
    )
}

#' Compact cli-style console summary of check results.
#'
#' Prints counts, then problematic contracts with their fail/warn detail lines.
#'
#' @param details Tibble of check rows.
#' @param max_bad Max number of bad contracts to list.
#' @param max_detail_per Max detail lines per bad contract.
#' @return Invisibly, the summary tibble.
print_check_summary <- function(details, max_bad = 25L, max_detail_per = 4L) {
  use_cli <- requireNamespace("cli", quietly = TRUE)
  summary <- summarise_check_report(details)

  n_fail <- sum(details$status == "fail", na.rm = TRUE)
  n_warn <- sum(details$status == "warn", na.rm = TRUE)
  n_ok   <- sum(details$status == "ok", na.rm = TRUE)
  n_info <- sum(details$status == "info", na.rm = TRUE)
  n_skip <- sum(details$status == "skip", na.rm = TRUE)
  n_contracts <- nrow(summary)
  n_bad_contracts <- sum(summary$worst %in% c("fail", "warn"))

  headline <- sprintf(
    "Import check: %d ok, %d warn, %d fail, %d info, %d skip (%d checks, %d contracts)",
    n_ok, n_warn, n_fail, n_info, n_skip, nrow(details), n_contracts
  )

  if (use_cli) {
    if (n_fail > 0) {
      cli::cli_alert_danger(headline)
    } else if (n_warn > 0) {
      cli::cli_alert_warning(headline)
    } else {
      cli::cli_alert_success(headline)
    }
  } else {
    message(headline)
  }

  bad <- summary |> dplyr::filter(worst %in% c("fail", "warn"))
  if (!nrow(bad)) {
    if (use_cli) {
      cli::cli_alert_success("No problematic contracts.")
    } else {
      message("No problematic contracts.")
    }
    return(invisible(summary))
  }

  if (use_cli) {
    cli::cli_h2("Problematic contracts ({n_bad_contracts})")
  } else {
    message(sprintf("Problematic contracts (%d):", n_bad_contracts))
  }

  show <- utils::head(bad, max_bad)
  for (i in seq_len(nrow(show))) {
    cid <- show$contract_id[[i]]
    lab <- show$label[[i]]
    worst <- show$worst[[i]]
    title <- sprintf(
      "[%s] %s (%s) — fail=%d warn=%d",
      worst, cid, lab, show$n_fail[[i]], show$n_warn[[i]]
    )
    if (use_cli) {
      if (identical(worst, "fail")) {
        cli::cli_alert_danger(title)
      } else {
        cli::cli_alert_warning(title)
      }
    } else {
      message("  ", title)
    }

    detail_rows <- details |>
      dplyr::filter(
        dplyr::coalesce(contract_id, "(global)") == cid,
        status %in% c("fail", "warn")
      ) |>
      utils::head(max_detail_per)

    for (j in seq_len(nrow(detail_rows))) {
      line <- sprintf(
        "  %s/%s: %s",
        detail_rows$level[[j]],
        detail_rows$status[[j]],
        detail_rows$message[[j]]
      )
      if (use_cli) {
        cli::cli_text("{.dim {line}}")
      } else {
        message(line)
      }
    }
  }

  if (nrow(bad) > max_bad) {
    more <- sprintf("... and %d more problematic contracts", nrow(bad) - max_bad)
    if (use_cli) cli::cli_text("{.dim {more}}") else message(more)
  }

  invisible(summary)
}

##### Report writer -----------------------------------------------------------

#' Write check results to Excel (summary / details / orphans).
#'
#' Side-effect of writing is isolated here so callers can run checks without
#' touching disk (`report_path = NULL`) or write later from a saved tibble.
#'
#' @param details Tibble from checkImportFunctions().
#' @param orphans Orphan impplan rows (may be empty).
#' @param path Output path; default assets/_DB/import_check_report.xlsx.
#' @return Invisibly, `path`.
writeImportCheckReport <- function(details,
                                   orphans = NULL,
                                   path = here::here("assets", "_DB", "import_check_report.xlsx")) {
  stopifnot(!is.null(details), is.data.frame(details))
  summary <- summarise_check_report(details)
  if (is.null(orphans)) {
    orphans <- attr(details, "orphans")
  }
  if (is.null(orphans)) orphans <- tibble::tibble()
  # Drop attributes that writexl / Excel cannot store
  details_out <- details |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  orphans_out <- if (nrow(orphans)) {
    orphans |> dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  } else {
    tibble::tibble(note = "No orphan active impplan rows")
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required to write import_check_report.xlsx", call. = FALSE)
  }
  writexl::write_xlsx(
    list(
      summary = summary,
      details = details_out,
      orphans = orphans_out
    ),
    path = path
  )
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_alert_success("Wrote {.path {path}}")
  } else {
    message("Wrote ", path)
  }
  invisible(path)
}

##### Entry point -------------------------------------------------------------

#' Check import-structure contracts against files / APIs.
#'
#' @param param_fname Path to 0_database_params.xlsx (or test twin).
#' @param online Run network probes (WDI, IMF, ILO, BIS debt URL).
#' @param deep Read full columns for `codes` value checks.
#' @param only Optional character vector of contract ids to run.
#' @param report_path If not NULL, write Excel report via writeImportCheckReport().
#' @param update_mode Passed to plan read: 0 = all active, 1 = update==1 only.
#' @return Tibble of check rows; attr `orphans` and `summary` attached.
checkImportFunctions <- function(param_fname = here("assets", "_DB", "0_database_params.xlsx"),
                                 online = TRUE,
                                 deep = FALSE,
                                 only = NULL,
                                 report_path = here("assets", "_DB", "import_check_report.xlsx"),
                                 update_mode = 0) {
  # Lightweight plan load (avoid sourcing full import.R side effects)
  stopifnot(file.exists(param_fname))
  impplan <- readxl::read_excel(param_fname, sheet = "import", col_names = TRUE, skip = 1)
  locals <- readxl::read_excel(param_fname, sheet = "scope", col_names = TRUE, skip = 6)
  local_countries <- locals |> dplyr::filter(active == 1) |> dplyr::pull(country)
  local_fnames <- here::here("assets", local_countries, "Data", paste0(local_countries, "_local.xlsx"))

  if (identical(as.integer(update_mode), 1L) && "update" %in% names(impplan)) {
    impplan <- impplan |> dplyr::filter(update == 1)
  }
  impplan <- impplan |> dplyr::filter(active == 1)

  contracts_all <- getImportContracts()
  contracts <- contracts_all
  if (!is.null(only)) {
    miss <- setdiff(only, names(contracts_all))
    if (length(miss)) {
      stop("Unknown contract id(s) in `only`: ", paste(miss, collapse = ", "), call. = FALSE)
    }
    contracts <- contracts_all[only]
  }

  # 1) plan coverage + orphans — always against the full registry
  cov_full <- check_plan_coverage(contracts_all, impplan)
  # Restrict matched plans to the contracts we will check
  matched <- cov_full$matched[names(contracts)]

  # 2) code drift vs import.R — full registry (detects both directions)
  drift <- check_code_drift(contracts_all)
  if (!is.null(only)) {
    # Keep drift rows that mention selected contracts or global Import-* orphans
    drift <- drift |>
      dplyr::filter(
        is.na(contract_id) | contract_id %in% names(contracts)
      )
  }

  # Plan rows for selected contracts only (plus orphan fails from full coverage)
  plan_rows <- cov_full$rows
  if (!is.null(only)) {
    plan_rows <- plan_rows |>
      dplyr::filter(
        is.na(contract_id) | contract_id %in% names(contracts)
      )
  }

  # 3) per-contract file/sheet/structure/codes/probe/api
  per <- purrr::map(names(contracts), \(id) {
    check_one_contract(
      contracts[[id]],
      plan = matched[[id]] %||% impplan[0, , drop = FALSE],
      online = online,
      deep = deep,
      local_fnames = local_fnames
    )
  }) |> dplyr::bind_rows()

  details <- dplyr::bind_rows(plan_rows, drift, per)
  attr(details, "orphans") <- cov_full$orphans
  attr(details, "summary") <- summarise_check_report(details)

  print_check_summary(details)

  if (!is.null(report_path)) {
    writeImportCheckReport(details, orphans = cov_full$orphans, path = report_path)
  }

  details
}
