# Import structure contracts
#
# Declarative structure anchors for each active tryImport block in import.R.
# Consumed by check_import_functions.R (cheap header / probe checks before import).
#
# Schema (see import_contract()):
#   id, label, kind, code_block, match, file_name, sheets, skip,
#   required_cols, optional_cols, col_pattern, col_transform, code_col, probe

##### Constructor and shared helpers ------------------------------------------

#' Build one import-structure contract.
#'
#' @param id Stable registry key.
#' @param label Human-readable name.
#' @param kind `"file"`, `"api"`, or `"local"`.
#' @param code_block Section title from `import.R` after `##### `
#'   (used to detect registry/code drift).
#' @param match Predicate `function(impplan)` returning a logical vector of
#'   length `nrow(impplan)`, mirroring the block's `filter(...)`.
#' @param file_name Hard-coded file name if the block ignores the plan, else `NULL`.
#' @param sheets Function of filtered plan rows → character sheet names, or `NULL`.
#' @param skip Header offset for reads, or `NULL`.
#' @param required_cols Columns that must exist (`fail` if missing).
#' @param optional_cols Columns whose absence is only a `warn`.
#' @param col_pattern Regex for year-like / patterned columns.
#' @param col_transform Optional name transform before column checks
#'   (e.g. `janitor::clean_names` for WGI).
#' @param code_col Column that must contain `retrieve_code` values (deep mode).
#' @param probe Optional `function(ctx)` when a header read is not enough.
#'   `ctx` has at least `plan`, `path`, `contract`.
#' @return Named list with the schema fields above.
import_contract <- function(id,
                            label,
                            kind = c("file", "api", "local"),
                            code_block,
                            match,
                            file_name = NULL,
                            sheets = NULL,
                            skip = NULL,
                            required_cols = character(),
                            optional_cols = character(),
                            col_pattern = NULL,
                            col_transform = NULL,
                            code_col = NULL,
                            probe = NULL) {
  kind <- match.arg(kind)
  stopifnot(is.character(id), length(id) == 1L, nzchar(id))
  stopifnot(is.character(label), length(label) == 1L)
  stopifnot(is.character(code_block), length(code_block) == 1L)
  stopifnot(is.function(match))
  if (!is.null(sheets)) stopifnot(is.function(sheets))
  if (!is.null(probe)) stopifnot(is.function(probe))
  if (!is.null(col_transform)) stopifnot(is.function(col_transform))

  list(
    id            = id,
    label         = label,
    kind          = kind,
    code_block    = code_block,
    match         = match,
    file_name     = file_name,
    sheets        = sheets,
    skip          = skip,
    required_cols = as.character(required_cols),
    optional_cols = as.character(optional_cols),
    col_pattern   = col_pattern,
    col_transform = col_transform,
    code_col      = code_col,
    probe         = probe
  )
}

#' Default sheet resolver: unique non-missing `sheet_name` from plan rows.
sheets_from_plan <- function(plan) {
  unique(stats::na.omit(as.character(plan$sheet_name)))
}

#' WGI sheets = dimension half of `retrieve_code` (`type/dimension`).
sheets_wgi_from_plan <- function(plan) {
  unique(stringr::str_split_fixed(as.character(plan$retrieve_code), "/", 2)[, 2])
}

# Year-column patterns used by several file blocks
col_pattern_year_19_20 <- "^(19|20)\\d{2}$"
col_pattern_year_4     <- "^\\d{4}$|^X\\d{4}$"

`%||%` <- function(x, y) if (is.null(x)) y else x

##### Probes (header alone is insufficient) -----------------------------------

#' Conference Board TED: Metadata + TED n sheet with mnemonic / Date rows.
probe_conference_board <- function(ctx) {
  paths <- ctx$paths %||% ctx$path
  paths <- unique(as.character(stats::na.omit(paths)))
  paths <- paths[nzchar(paths)]
  if (!length(paths) && !is.null(ctx$path)) paths <- ctx$path
  if (!length(paths)) {
    return(list(ok = FALSE, level = "fail", message = "Conference Board file missing"))
  }

  msgs <- character()
  for (path in paths) {
    if (!file.exists(path)) {
      return(list(ok = FALSE, level = "fail", message = paste("File missing:", basename(path))))
    }
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
    ted_sheet <- sheets[stringr::str_detect(sheets, "^TED")][1]
    legacy <- "Data" %in% sheets
    if (is.na(ted_sheet) && !legacy) {
      return(list(
        ok = FALSE,
        level = "fail",
        message = paste(
          basename(path), ": no TED* or Data sheet; have:",
          paste(sheets, collapse = ", ")
        )
      ))
    }
    if (!is.na(ted_sheet)) {
      raw <- tryCatch(
        readxl::read_excel(path, sheet = ted_sheet, col_names = FALSE, n_max = 12, .name_repair = "minimal"),
        error = function(e) NULL
      )
      if (is.null(raw)) {
        return(list(ok = FALSE, level = "fail", message = paste("Could not read sheet", ted_sheet)))
      }
      labels <- as.character(raw[[1]])
      if (!("mnemonic" %in% labels && "Date" %in% labels)) {
        return(list(ok = FALSE, level = "fail", message = paste(basename(path), ": missing mnemonic/Date")))
      }
      mn_row <- which(labels == "mnemonic")[1]
      mn <- as.character(unlist(raw[mn_row, ], use.names = FALSE))
      n_tok <- sum(!is.na(mn) & nzchar(mn) & mn != "mnemonic")
      plan_i <- ctx$plan
      if ("file_name" %in% names(plan_i)) {
        plan_i <- plan_i[plan_i$file_name == basename(path), , drop = FALSE]
      }
      plan_codes <- unique(stats::na.omit(as.character(plan_i$retrieve_code)))
      inds <- unique(stats::na.omit(stringr::str_match(mn, "^[^_]+_[A-Za-z0-9]+_(.+)$")[, 2]))
      miss <- setdiff(plan_codes, inds)
      if (length(miss)) {
        return(list(
          ok = FALSE,
          level = "fail",
          message = sprintf(
            "%s / %s: %d plan retrieve_code(s) missing",
            basename(path), ted_sheet, length(miss)
          )
        ))
      }
      msgs <- c(msgs, sprintf("%s:%s ok (%d mnemonics)", basename(path), ted_sheet, n_tok))
    } else {
      msgs <- c(msgs, sprintf("%s: legacy Data sheet", basename(path)))
    }
  }
  list(ok = TRUE, level = "info", message = paste(msgs, collapse = "; "))
}

#' HRT: each plan sheet needs a usable country/year key after dynamic skip.
#'
#' Import uses `any_of()` across sheet-specific columns and `skip = 6` when row 1
#' is blank — a single-header optional_cols check is a false positive.
probe_hrt <- function(ctx) {
  path <- ctx$path
  if (is.null(path) || !file.exists(path)) {
    return(list(ok = FALSE, level = "fail", message = "HRT file missing"))
  }
  sheets <- unique(stats::na.omit(as.character(ctx$plan$sheet_name)))
  sheets <- sheets[nzchar(sheets)]
  if (!length(sheets)) {
    return(list(ok = FALSE, level = "fail", message = "No HRT sheet_name in plan"))
  }
  have <- tryCatch(readxl::excel_sheets(path), error = function(e) character())
  missing_sheets <- setdiff(sheets, have)
  if (length(missing_sheets)) {
    return(list(
      ok = FALSE,
      level = "fail",
      message = paste("Missing HRT sheet(s):", paste(missing_sheets, collapse = ", "))
    ))
  }
  key_cols <- c(
    "Year", "ISOCode", "DebtorCountry", "WDI code",
    "Start of default or restructuring process: default or announcement"
  )
  problems <- character()
  for (sh in sheets) {
    first_row <- tryCatch(
      readxl::read_excel(
        path, sheet = sh, range = readxl::cell_rows(1),
        col_names = FALSE, n_max = 1
      ),
      error = function(e) NULL
    )
    skip_n <- if (!is.null(first_row) && all(is.na(unlist(first_row)))) 6L else 0L
    hdr <- tryCatch(
      readxl::read_excel(
        path, sheet = sh, skip = skip_n, n_max = 0,
        col_names = TRUE, .name_repair = "minimal"
      ),
      error = function(e) NULL
    )
    if (is.null(hdr)) {
      problems <- c(problems, sprintf("%s: could not read header (skip=%d)", sh, skip_n))
      next
    }
    hit <- intersect(key_cols, names(hdr))
    if (!length(hit)) {
      problems <- c(
        problems,
        sprintf("%s: no key columns among %s", sh, paste(key_cols, collapse = ", "))
      )
    }
  }
  if (length(problems)) {
    return(list(ok = FALSE, level = "fail", message = paste(problems, collapse = "; ")))
  }
  list(
    ok = TRUE,
    level = "ok",
    message = sprintf("HRT sheets OK (%d): key columns present after dynamic skip", length(sheets))
  )
}

#' CT: positional columns 4 (iso3) and 5 (date) after skip = 13.
probe_ct_positional <- function(ctx) {
  path <- ctx$path
  sheet <- ctx$sheet %||% sheets_from_plan(ctx$plan)[1]
  if (is.null(path) || !file.exists(path)) {
    return(list(ok = FALSE, level = "fail", message = "CT file missing"))
  }
  raw <- tryCatch(
    readxl::read_excel(path, sheet = sheet, skip = 13, col_names = FALSE, n_max = 5),
    error = function(e) NULL
  )
  if (is.null(raw) || ncol(raw) < 5L) {
    return(list(
      ok = FALSE,
      level = "fail",
      message = "CT sheet has fewer than 5 columns after skip = 13"
    ))
  }
  list(ok = TRUE, level = "info", message = sprintf("CT positional layout: %d columns", ncol(raw)))
}

#' BIS debt API: `retrieve_code` is a CSV URL.
probe_bis_debt_url <- function(ctx) {
  urls <- unique(stats::na.omit(ctx$plan$retrieve_code))
  if (!length(urls)) {
    return(list(ok = FALSE, level = "fail", message = "No BIS debt URLs in plan"))
  }
  url <- urls[[1]]
  hdr <- tryCatch(data.table::fread(url, nrows = 0), error = function(e) NULL)
  if (is.null(hdr)) {
    return(list(ok = FALSE, level = "fail", message = paste("Failed to read header from", url)))
  }
  nm <- names(hdr)
  has_area <- any(c("REF_AREA", "BORROWERS_CTY") %in% nm)
  has_time <- "TIME_PERIOD" %in% nm
  has_val  <- "OBS_VALUE" %in% nm
  if (!has_area || !has_time || !has_val) {
    return(list(
      ok = FALSE,
      level = "fail",
      message = sprintf(
        "BIS debt CSV missing columns (area=%s, TIME_PERIOD=%s, OBS_VALUE=%s)",
        has_area, has_time, has_val
      )
    ))
  }
  list(ok = TRUE, level = "info", message = paste("BIS debt header OK:", basename(url)))
}

#' WEO vintages: directory of `YYYY-[12].xls[x]` with subject + ISO + year cols.
probe_weo_vintages <- function(ctx) {
  weov_dir <- ctx$path %||%
    here::here("assets", "_DB", "_extsources", "WEO_vintages")
  if (!dir.exists(weov_dir)) {
    return(list(ok = FALSE, level = "fail", message = paste("WEO vintages dir missing:", weov_dir)))
  }
  files <- fs::dir_ls(weov_dir, regexp = "\\d{4}-[12]\\.xls[x]?$", type = "file")
  if (!length(files)) {
    return(list(ok = FALSE, level = "fail", message = "No WEO vintage files matching YYYY-[12].xls[x]"))
  }
  raw <- tryCatch(
    readxl::read_excel(files[[1]], sheet = 1, n_max = 0, .name_repair = "unique"),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(list(ok = FALSE, level = "fail", message = paste("Could not read", basename(files[[1]]))))
  }
  nm <- names(raw)
  subj_ok <- any(c(
    "WEO Subject Code", "WEO.Subject.Code", "Subject Code", "Subject.Code", "subject_code"
  ) %in% nm)
  iso_ok <- any(c(
    "ISO2", "ISO2 Code", "ISO2.Code", "iso2",
    "ISO", "ISO Code", "ISO.Code", "WEO Country Code", "WEO.Country.Code", "iso"
  ) %in% nm)
  year_ok <- any(stringr::str_detect(nm, "^\\d{4}$|^X\\d{4}$"))
  if (!subj_ok || !iso_ok || !year_ok) {
    return(list(
      ok = FALSE,
      level = "fail",
      message = sprintf(
        "Vintage %s missing anchors (subject=%s, iso=%s, years=%s)",
        basename(files[[1]]), subj_ok, iso_ok, year_ok
      )
    ))
  }
  list(
    ok = TRUE,
    level = "info",
    message = sprintf("%d vintage file(s); sample %s OK", length(files), basename(files[[1]]))
  )
}

#' Local country workbooks: each must expose the frequency sheets in the plan.
probe_local_files <- function(ctx) {
  fnames <- ctx$local_fnames
  if (is.null(fnames) || !length(fnames)) {
    return(list(ok = FALSE, level = "fail", message = "No local_fnames in probe context"))
  }
  freqs <- unique(stats::na.omit(ctx$plan$source_frequency))
  need <- intersect(c("d", "m", "q", "y"), freqs)
  missing <- character()
  bad_sheets <- character()
  for (f in fnames) {
    if (!file.exists(f)) {
      missing <- c(missing, f)
      next
    }
    sh <- tryCatch(readxl::excel_sheets(f), error = function(e) character())
    absent <- setdiff(need, sh)
    if (length(absent)) {
      bad_sheets <- c(bad_sheets, sprintf("%s [%s]", basename(f), paste(absent, collapse = ",")))
    }
  }
  if (length(missing) || length(bad_sheets)) {
    parts <- c(
      if (length(missing)) paste("Missing:", paste(basename(missing), collapse = ", ")),
      if (length(bad_sheets)) paste("Bad sheets:", paste(bad_sheets, collapse = "; "))
    )
    return(list(ok = FALSE, level = "fail", message = paste(parts, collapse = " | ")))
  }
  list(ok = TRUE, level = "info", message = sprintf("%d local file(s) OK", length(fnames)))
}

##### Registry (one entry per active tryImport block) -------------------------

#' Named list of import contracts. Keys are contract `id`s.
#'
#' `required_cols` / `optional_cols` / `code_col` / `skip` / `sheets` were
#' checked line-by-line against the corresponding blocks in `import.R`.
import_contracts <- list(

  # --- WDI yearly ------------------------------------------------------------
  wdi_y = import_contract(
    id         = "wdi_y",
    label      = "WDI yearly",
    kind       = "api",
    code_block = "Import WDI yearly",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WDI" &
        impplan$retrieve_type == "API" &
        impplan$source_frequency == "y"
    },
    required_cols = c("iso2c", "year"),
    optional_cols = c("country")
  ),

  # --- WDI quarterly ---------------------------------------------------------
  wdi_q = import_contract(
    id         = "wdi_q",
    label      = "WDI quarterly",
    kind       = "api",
    code_block = "Import WDI quarterly",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WDI" &
        impplan$retrieve_type == "API" &
        impplan$source_frequency == "q"
    },
    # Block drops country/iso3c; treats iso2c as iso3c; year like YYYYQn
    required_cols = c("iso2c", "year"),
    optional_cols = c("country", "iso3c")
  ),

  # --- WGI (one sheet per dimension; old ##### Import WGI block is commented) -
  wgi = import_contract(
    id         = "wgi",
    label      = "WGI",
    kind       = "file",
    code_block = "Import WGI (new WGI file format: one sheet per dimension)",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WGI" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_wgi_from_plan,
    skip          = 0L,
    # After janitor::clean_names() as in the block
    required_cols = c("year", "economy_code"),
    optional_cols = c(
      "governance_estimate_approx_2_5_to_2_5",
      "governance_score_0_100"
    ),
    col_transform = janitor::clean_names
  ),

  # --- UNCTAD ----------------------------------------------------------------
  unctad = import_contract(
    id         = "unctad",
    label      = "UNCTAD diversification",
    kind       = "file",
    code_block = "Import UNCTAD export diversification index",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "UNCTAD" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    skip          = 0L,
    required_cols = c(
      "Year", "Economy", "Economy.Label", "Flow.Label", "Diversification.Index"
    )
  ),

  # --- IMF SDMX --------------------------------------------------------------
  imf_y = import_contract(
    id         = "imf_y",
    label      = "IMF yearly (SDMX)",
    kind       = "api",
    code_block = "Import yearly IMF data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "IMF" &
        impplan$retrieve_type == "API" &
        impplan$source_frequency == "y"
    },
    required_cols = c("iso2", "year", "value")
  ),

  imf_q = import_contract(
    id         = "imf_q",
    label      = "IMF quarterly (SDMX)",
    kind       = "api",
    code_block = "Import quarterly IMF data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "IMF" &
        impplan$retrieve_type == "API" &
        impplan$source_frequency == "q"
    },
    required_cols = c("iso2", "year", "quarter", "value")
  ),

  imf_m = import_contract(
    id         = "imf_m",
    label      = "IMF monthly (SDMX)",
    kind       = "api",
    code_block = "Import monthly IMF data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "IMF" &
        impplan$retrieve_type == "API" &
        impplan$source_frequency == "m"
    },
    required_cols = c("iso2", "year", "month", "value")
  ),

  # --- IDS -------------------------------------------------------------------
  ids = import_contract(
    id         = "ids",
    label      = "IDS external debt",
    kind       = "file",
    code_block = "Import IDS external debt statistics",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "IDS" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets = sheets_from_plan,
    required_cols = c(
      "Country Name", "Country Code", "Series Code", "Series Name",
      "Counterpart-Area Name", "Counterpart-Area Code"
    ),
    col_pattern = col_pattern_year_19_20,
    code_col    = "Series Code"
  ),

  # --- BIS policy / FX rates -------------------------------------------------
  bis_rates = import_contract(
    id         = "bis_rates",
    label      = "BIS policy and FX rates",
    kind       = "file",
    code_block = "Import daily and monthly BIS data on policy rates and exchange rates",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "BIS" &
        impplan$retrieve_type == "file" &
        impplan$database_name %in% c(
          "Policy rates (flat)",
          "US dollar exchange rates (flat)"
        )
    },
    skip = 0L,
    required_cols = c(
      "REF_AREA.Reference.area",
      "TIME_PERIOD.Time.period.or.range",
      "OBS_VALUE.Observation.Value",
      "FREQ.Frequency"
    ),
    optional_cols = c("COLLECTION.Collection")
  ),

  # --- BIS effective exchange rates ------------------------------------------
  bis_eer = import_contract(
    id         = "bis_eer",
    label      = "BIS effective exchange rates",
    kind       = "file",
    code_block = "Import monthly BIS data on effective exchange rates",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "BIS" &
        impplan$retrieve_type == "file" &
        impplan$database_name == "Effective exchange rate indices (monthly)"
    },
    skip = 0L,
    required_cols = c(
      "REF_AREA.Reference.area",
      "TIME_PERIOD.Time.period.or.range",
      "OBS_VALUE.Observation.Value",
      "FREQ.Frequency",
      "EER_TYPE.Type",
      "EER_BASKET.Basket"
    )
  ),

  # --- IMF Fiscal Monitor ----------------------------------------------------
  imf_fm = import_contract(
    id         = "imf_fm",
    label      = "IMF Fiscal Monitor",
    kind       = "file",
    code_block = "Import IMF Fiscal monitor structural indicators",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "FM" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    required_cols = c("country_code"),
    col_pattern   = col_pattern_year_19_20
  ),

  # --- ILOstat ---------------------------------------------------------------
  ilo = import_contract(
    id         = "ilo",
    label      = "ILOstat",
    kind       = "api",
    code_block = "Import ILOstat data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "ILO" &
        impplan$retrieve_type == "API"
    },
    required_cols = c("ref_area", "time", "obs_value"),
    optional_cols = c("sex", "classif1", "best_source")
  ),

  # --- Our World in Data COVID -----------------------------------------------
  owid = import_contract(
    id         = "owid",
    label      = "OWID COVID",
    kind       = "file",
    code_block = "Import daily data on COVID",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "Ourworldindata" &
        impplan$retrieve_type == "file" &
        impplan$database_name == "COVID tracker"
    },
    skip          = 0L,
    required_cols = c("date", "iso_code")
  ),

  # --- UN HDR ----------------------------------------------------------------
  un_hdr = import_contract(
    id         = "un_hdr",
    label      = "UN HDR",
    kind       = "file",
    code_block = "Import UN HDR data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "UN" &
        impplan$retrieve_type == "file" &
        impplan$database_name == "HDR"
    },
    required_cols = c("iso3")
  ),

  # --- Chinn-Ito -------------------------------------------------------------
  chinn_ito = import_contract(
    id         = "chinn_ito",
    label      = "Chinn-Ito",
    kind       = "file",
    code_block = "Import Chinn-Ito financial system classification",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "Chinn-Ito" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    required_cols = c("IMF-World Bank Country Code", "year")
  ),

  # --- WEO -------------------------------------------------------------------
  weo = import_contract(
    id         = "weo",
    label      = "IMF WEO",
    kind       = "file",
    code_block = "Import WEO outlook",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WEO" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    required_cols = c("ISO", "WEO Subject Code"),
    col_pattern   = col_pattern_year_19_20,
    code_col      = "WEO Subject Code"
  ),

  # --- WEO aggregates --------------------------------------------------------
  weo_aggr = import_contract(
    id         = "weo_aggr",
    label      = "IMF WEO aggregates",
    kind       = "file",
    code_block = "Import WEO outlook for aggregates",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WEO_aggr" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets = sheets_from_plan,
    required_cols = c(
      "Country Group Name",
      "WEO Subject Code",
      "Country/Series-specific Notes"
    ),
    col_pattern = col_pattern_year_19_20,
    code_col    = "Country Group Name"
  ),

  # --- WPP aggregates --------------------------------------------------------
  wpp_aggr = import_contract(
    id         = "wpp_aggr",
    label      = "UNPD WPP aggregates",
    kind       = "file",
    code_block = "Import UNPD aggregated data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WPP_aggr" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    skip          = 0L,
    required_cols = c("ISO2_code", "Time", "Variant")
  ),

  # --- WPP 5-year age groups -------------------------------------------------
  wpp_5yr = import_contract(
    id         = "wpp_5yr",
    label      = "UNPD WPP 5-year groups",
    kind       = "file",
    code_block = "Import UNPD 5-year groups data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WPP_5yr" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    skip          = 0L,
    required_cols = c("ISO2_code", "Time", "Variant", "AgeGrp", "PopTotal"),
    code_col      = "AgeGrp"
  ),

  # --- Bonds in reserves -----------------------------------------------------
  bonds_in_reserves = import_contract(
    id         = "bonds_in_reserves",
    label      = "BondsInReserves",
    kind       = "file",
    code_block = "Import data on debt held in reserves",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "BondsInReserves" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets      = sheets_from_plan,
    skip        = 3L,
    col_pattern = "DEC"
  ),

  # --- Fiscal Space Database -------------------------------------------------
  fsdb = import_contract(
    id         = "fsdb",
    label      = "FSDB",
    kind       = "file",
    code_block = "Import data from Fiscal Space Database",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "FSDB" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets      = sheets_from_plan,
    col_pattern = col_pattern_year_19_20
  ),

  # --- iMaPP -----------------------------------------------------------------
  imapp = import_contract(
    id         = "imapp",
    label      = "iMaPP",
    kind       = "file",
    code_block = "Import data from the Integrated Macroprudential Policy (iMaPP) Database",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "iMaPP" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "m"
    },
    sheets        = sheets_from_plan,
    required_cols = c("iso2", "Year", "Month")
  ),

  # --- FSB -------------------------------------------------------------------
  fsb = import_contract(
    id         = "fsb",
    label      = "FSB financial structure",
    kind       = "file",
    code_block = "Import data on the financial systems structure (from Financial Stability Board)",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "FSB" &
        impplan$retrieve_type == "file"
    },
    sheets = sheets_from_plan,
    required_cols = c(
      "Jurisdiction code",
      "Year",
      "Entity/Economic function",
      "Value, in USD trillions",
      "Topic"
    ),
    code_col = "Entity/Economic function"
  ),

  # --- Global Macro Data -----------------------------------------------------
  gmd = import_contract(
    id         = "gmd",
    label      = "GMD",
    kind       = "file",
    code_block = "Import data from the Global Macro Data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "GMD" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    skip          = 0L,
    required_cols = c("ISO3", "year")
  ),

  # --- Conference Board ------------------------------------------------------
  conference_board = import_contract(
    id         = "conference_board",
    label      = "Conference Board",
    kind       = "file",
    code_block = "Import data from the Conference Board",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y" &
        impplan$file_name %in% c("CB_GrowthAccounting.xlsx", "CB_GrowthFactors.xlsx")
    },
    sheets = function(plan) character(),
    skip   = NULL,
    probe  = probe_conference_board
  ),

  # --- CPI targets -----------------------------------------------------------
  cpi_targets = import_contract(
    id         = "cpi_targets",
    label      = "CPI targets",
    kind       = "file",
    code_block = "Import data on CPI targets",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "CPI targets" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 0L,
    required_cols = c("country_id", "year"),
    col_pattern   = "^cpi_target"
  ),

  # --- CDS -------------------------------------------------------------------
  cds = import_contract(
    id         = "cds",
    label      = "CDS",
    kind       = "file",
    code_block = "Import data on CDS ",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "CDS" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "d"
    },
    sheets        = sheets_from_plan,
    skip          = 1L,
    required_cols = c("date")
  ),

  # --- Neutral rates ---------------------------------------------------------
  neutral_rates = import_contract(
    id         = "neutral_rates",
    label      = "Neutral rates",
    kind       = "file",
    code_block = "Import data on neutral real rates",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "Neutral rates" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 0L,
    required_cols = c("country_id", "year"),
    col_pattern   = "neutral_rate"
  ),

  # --- BIS debt (API / CSV URLs) ---------------------------------------------
  bis_debt = import_contract(
    id         = "bis_debt",
    label      = "BIS debt (API CSV)",
    kind       = "api",
    code_block = "Import BIS debt data",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$source_name == "BIS" &
        impplan$retrieve_type == "API" &
        impplan$database_name %in% c("Debt securities", "DSR", "Credit-to-GDP")
    },
    required_cols = c("TIME_PERIOD", "OBS_VALUE"),
    optional_cols = c("REF_AREA", "BORROWERS_CTY"),
    probe         = probe_bis_debt_url
  ),

  # --- RAs defaults ----------------------------------------------------------
  ras = import_contract(
    id         = "ras",
    label      = "RAs defaults",
    kind       = "file",
    code_block = "Import data on RAs defaults",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "RAs" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 0L,
    required_cols = c("country_id", "year")
  ),

  # --- BOC-BOE ---------------------------------------------------------------
  boc_boe = import_contract(
    id         = "boc_boe",
    label      = "BOC-BOE defaults",
    kind       = "file",
    code_block = "Import data on defaults from BOC-BOE",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "BOC-BOE" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 64L,
    required_cols = c("k")
  ),

  # --- Reinhart–Rogoff -------------------------------------------------------
  rr = import_contract(
    id         = "rr",
    label      = "RR sovereign defaults",
    kind       = "file",
    code_block = "Import data on sovereign defaults from Reinhart and Rogoff",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "RR" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 0L,
    required_cols = c("country_id", "country_name"),
    col_pattern   = col_pattern_year_19_20
  ),

  # --- Global Crisis Database ------------------------------------------------
  gcd = import_contract(
    id         = "gcd",
    label      = "GCD sovereign defaults",
    kind       = "file",
    code_block = "Import data on sovereign defaults from Global Crisis Database",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "GCD" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 0L,
    required_cols = c("CC3", "Year")
  ),

  # --- Horn, Reinhart, Trebesch ----------------------------------------------
  hrt = import_contract(
    id         = "hrt",
    label      = "HRT hidden defaults",
    kind       = "file",
    code_block = 'Import data on sovereign defaults from "Horn, Reinhart and Trebesch: Hidden Defaults"',
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "HRT" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets = sheets_from_plan,
    # Columns differ by sheet; import uses any_of() + dynamic skip — see probe_hrt
    probe  = probe_hrt
  ),

  # --- Cruces–Trebesch -------------------------------------------------------
  ct = import_contract(
    id         = "ct",
    label      = "CT haircuts",
    kind       = "file",
    code_block = 'Import data on sovereign defaults from "Sovereign Defaults: The Price of Haircuts", by Juan Cruces and Christoph Trebesch',
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "CT" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets = sheets_from_plan,
    skip   = 13L,
    probe  = probe_ct_positional
  ),

  # --- Modifiers -------------------------------------------------------------
  modifiers = import_contract(
    id         = "modifiers",
    label      = "Model modifiers",
    kind       = "file",
    code_block = "Import sovereign model modifiers",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "Modifiers" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets        = sheets_from_plan,
    skip          = 1L,
    required_cols = c("country_id", "year")
  ),

  # --- WEO vintages ----------------------------------------------------------
  weo_vintages = import_contract(
    id         = "weo_vintages",
    label      = "WEO forecast vintages",
    kind       = "file",
    code_block = "Import and build WEO forecast vintages",
    match      = function(impplan) {
      impplan$active == 1 &
        impplan$database_name == "WEO vintages" &
        impplan$retrieve_type == "file" &
        impplan$source_frequency == "y"
    },
    sheets = function(plan) "1",
    optional_cols = c(
      "WEO Subject Code", "WEO.Subject.Code", "Subject Code", "Subject.Code", "subject_code",
      "ISO2", "ISO2 Code", "ISO2.Code", "iso2",
      "ISO", "ISO Code", "ISO.Code", "WEO Country Code", "WEO.Country.Code", "iso"
    ),
    col_pattern = col_pattern_year_4,
    code_col    = "WEO Subject Code",
    probe       = probe_weo_vintages
  ),

  # --- Local country files ---------------------------------------------------
  local = import_contract(
    id         = "local",
    label      = "Local country data",
    kind       = "local",
    code_block = "Import local data",
    match      = function(impplan) {
      impplan$active == 1 & impplan$retrieve_type == "local"
    },
    skip   = 1L,
    sheets = function(plan) {
      intersect(c("d", "m", "q", "y"), unique(as.character(plan$source_frequency)))
    },
    probe = probe_local_files
  )
)

##### Registry accessors ------------------------------------------------------

#' Return the import-contracts registry (named list).
getImportContracts <- function() {
  import_contracts
}

#' Look up one contract by id; error if missing.
getImportContract <- function(id) {
  stopifnot(is.character(id), length(id) == 1L)
  out <- import_contracts[[id]]
  if (is.null(out)) {
    stop(sprintf("Unknown import contract id: '%s'", id), call. = FALSE)
  }
  out
}

#' Character vector of `code_block` titles for drift checks against import.R.
importContractCodeBlocks <- function() {
  vapply(import_contracts, `[[`, character(1), "code_block")
}
