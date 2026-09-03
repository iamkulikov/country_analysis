# Smoke checks for value-trace helpers and buildValueTrace().
#
# Usage (from repo root):
#   Rscript download_script/check_trace.R
#
# Prefers Trace_DB.rds when present; falls back to Filled_DB.rds
# (intermediate keep==0 columns may then be missing).

here::i_am("download_script/check_trace.R")

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(glue)
  library(rlang)
})

app_dir <- here("download_script", "country_data_download_app")
source(file.path(app_dir, "service.R"))
source(file.path(app_dir, "dep_graph.R"))
source(file.path(app_dir, "trace.R"))
source(file.path(app_dir, "trace_ledger.R"))

##### Tiny assert helpers -----------------------------------------------------

.n_fail <- 0L

check_eq <- function(label, got, expected) {
  ok <- identical(as.character(got), as.character(expected))
  if (!ok) {
    .n_fail <<- .n_fail + 1L
    message(glue::glue("FAIL  {label}\n  got:      {paste(as.character(got), collapse = ', ')}\n  expected: {paste(as.character(expected), collapse = ', ')}"))
  } else {
    message(glue::glue("ok    {label}"))
  }
  invisible(ok)
}

check_true <- function(label, cond) {
  if (!isTRUE(cond)) {
    .n_fail <<- .n_fail + 1L
    message(glue::glue("FAIL  {label}"))
  } else {
    message(glue::glue("ok    {label}"))
  }
  invisible(isTRUE(cond))
}

##### 1. Period arithmetic unit tests ----------------------------------------

message("\n=== Period helpers ===")

check_eq("shiftPeriod y +1", shiftPeriod("2020", "y", 1L), "2021")
check_eq("shiftPeriod y -2", shiftPeriod("2020", "y", -2L), "2018")
check_eq("shiftPeriod q +1 wraps year", shiftPeriod("2020-Q4", "q", 1L), "2021-Q1")
check_eq("shiftPeriod q -1 wraps year", shiftPeriod("2021-Q1", "q", -1L), "2020-Q4")
check_eq("shiftPeriod m +1", shiftPeriod("2020-12", "m", 1L), "2021-01")
check_eq("shiftPeriod m -3", shiftPeriod("2020-03", "m", -3L), "2019-12")
check_eq("shiftPeriod d +1", shiftPeriod("2020-01-31", "d", 1L), "2020-02-01")

check_eq("parentPeriodOf m->q", parentPeriodOf("2020-03", "m", "q"), "2020-Q1")
check_eq("parentPeriodOf m->y", parentPeriodOf("2020-11", "m", "y"), "2020")
check_eq("parentPeriodOf q->y", parentPeriodOf("2020-Q3", "q", "y"), "2020")
check_eq("parentPeriodOf d->m", parentPeriodOf("2020-03-15", "d", "m"), "2020-03")

check_eq(
  "subPeriodsOf y->q",
  as.character(subPeriodsOf("2020", "y", "q")),
  c("2020-Q1", "2020-Q2", "2020-Q3", "2020-Q4")
)
check_eq(
  "subPeriodsOf q->m",
  as.character(subPeriodsOf("2020-Q2", "q", "m")),
  c("2020-04", "2020-05", "2020-06")
)
check_eq(
  "subPeriodsOf y->m length",
  length(subPeriodsOf("2020", "y", "m")),
  12L
)

check_eq(
  "rollPeriods window 3",
  as.character(rollPeriods("2020-Q3", "q", 3L)),
  c("2020-Q1", "2020-Q2", "2020-Q3")
)

##### 1a. Impplan lookup must not collide with column names -------------------

message("\n=== Impplan source lookup ===")

fake_impplan <- tibble::tibble(
  indicator_code = c("gdp_g", "age_median"),
  source_frequency = "y",
  active = 1L,
  database_name = c("WDI", "WPP"),
  retrieve_code = c("NY.GDP.MKTP.KD.ZG", "MedianAge"),
  file_name = NA_character_,
  source_name = c("World Bank", "UN")
)

row_gdp <- find_impplan_row("gdp_g", "y", fake_impplan)
row_age <- find_impplan_row("age_median", "y", fake_impplan)
check_eq(
  "find_impplan_row does not return first row for gdp_g",
  row_gdp$retrieve_code[[1]],
  "NY.GDP.MKTP.KD.ZG"
)
check_eq(
  "find_impplan_row returns age_median row",
  row_age$retrieve_code[[1]],
  "MedianAge"
)
check_eq(
  "import_source_label gdp_g",
  import_source_label(row_gdp),
  "WDI / NY.GDP.MKTP.KD.ZG / World Bank"
)
check_eq(
  "import_source_label age_median",
  import_source_label(row_age),
  "WPP / MedianAge / UN"
)
check_true(
  "import labels differ across indicators",
  !identical(import_source_label(row_gdp), import_source_label(row_age))
)

##### 1b. Formula formatting + collapse toggle -------------------------------

message("\n=== Formula formatting / collapse ===")

check_eq(
  "formatFormulaWithValues infix +",
  formatFormulaWithValues(
    "x+y",
    c(x = "36.08", y = "56.77")
  ),
  "36.08+56.77"
)

check_true(
  "formatFormulaWithValues not prefix +",
  !grepl("^\\+\\(", formatFormulaWithValues("x+y", c(x = "36.08", y = "56.77")))
)

check_eq(
  "formatFormulaWithValues nested precedence",
  formatFormulaWithValues(
    "a-(b+c)",
    c(a = "1", b = "2", c = "3")
  ),
  "1-(2+3)"
)

check_eq(
  "formatFormulaWithValues unary minus",
  formatFormulaWithValues("-x", c(x = "5")),
  "-5"
)

check_eq(
  "formatFormulaWithValues lag stays functional",
  formatFormulaWithValues(
    "lag(x, 1)",
    c(x = "10")
  ),
  "lag(10, 1)"
)

check_eq(
  "formatFormulaWithValues mix lag and arithmetic",
  formatFormulaWithValues(
    "x+lag(y, 1)",
    c(x = "1", y = "2")
  ),
  "1+lag(2, 1)"
)

collapse_journal <- tibble::tibble(
  step_id = c("1", "1.1", "1.2"),
  level = c(0L, 1L, 1L),
  parent_id = c(NA_character_, "1", "1"),
  indicator_code = c("A", "B", "C"),
  node_type = c("computed", "imported", "imported"),
  operation = c("arithmetic", "import", "import"),
  formula_filled = c("1+2", NA_character_, NA_character_),
  time_relation = c("same", NA_character_, NA_character_),
  value = c(3, 1, 2),
  note = c(NA_character_, NA_character_, NA_character_)
)

collapsed_visible <- filter_trace_journal(
  collapse_journal,
  collapsed_prefixes = "1",
  show_technical = TRUE
)
check_eq(
  "collapse keeps parent only",
  collapsed_visible$step_id,
  "1"
)

collapsed_display <- prepare_trace_display(
  collapsed_visible,
  collapsed_prefixes = "1"
)
check_true(
  "collapsed parent toggle is +",
  grepl(">\\+<", collapsed_display$toggle[[1]])
)

expanded_display <- prepare_trace_display(
  collapse_journal,
  collapsed_prefixes = character(0)
)
check_true(
  "expanded parent toggle is −",
  grepl(">−<", expanded_display$toggle[[1]]) || grepl(">&minus;<", expanded_display$toggle[[1]])
)

check_eq(
  "calculation prefixes value=",
  prepare_trace_display(collapse_journal)$calculation[[1]],
  "3=1+2"
)

##### 1c. Trace Ledger view model --------------------------------------------

message("\n=== Trace Ledger view model ===")

check_eq(
  "format_period_display m",
  format_period_display("2023-01", "m"),
  "2023M01"
)
check_eq(
  "format_period_display q",
  format_period_display("2023-Q2", "q"),
  "2023Q2"
)

ledger_journal <- tibble::tibble(
  step_id = c("1", "1.1", "1.2", "1.3", "1.3.1", "1.3.2", "1.4"),
  level = c(0L, 1L, 1L, 1L, 2L, 2L, 1L),
  parent_id = c(NA_character_, "1", "1", "1", "1.3", "1.3", "1"),
  country_id = "RU",
  indicator_code = c(
    "cpi_av", "cpi_av_local", "cpi_av_weo", "cpi_av_ifs",
    "cpi_ind_av_ifs", "cpi_ind_av_ifs", "cpi_av_gmd"
  ),
  frequency = c("y", "y", "y", "y", "y", "y", "y"),
  period = c("2023", "2023", "2023", "2023", "2023", "2022", "2023"),
  value = c(6.4, NA_real_, NA_real_, 6.4, 118.2, 111.09, 6.1),
  node_type = c(
    "computed", "no_value", "no_value", "computed",
    "computed", "computed", "imported"
  ),
  operation = c(
    "coalesce", "import", "import", "arithmetic",
    "aggregate_mean", "aggregate_mean", "import"
  ),
  formula_raw = c(
    "coalesce(cpi_av_local,cpi_av_weo,cpi_av_ifs,cpi_av_gmd)",
    NA, NA, "(cpi_ind_av_ifs/lag(cpi_ind_av_ifs)-1)*100",
    "mean", "mean", NA
  ),
  formula_filled = c(
    "coalesce(NA,NA,6.4,6.1)", NA, NA, "(118.2/111.09-1)*100",
    NA, NA, NA
  ),
  time_relation = c(
    "same period for all arguments", "source data", "source data",
    "current and lagged annual periods",
    "mean over sub-periods of 2023 (m->y)",
    "mean over sub-periods of 2022 (m->y)",
    "source data"
  ),
  source_name = c(NA, "Local NSOs", "IMF WEO", NA, NA, NA, "GMD"),
  note = c(
    "Argument 3 selected (first non-NA)", NA, NA, NA,
    "Showing 12 sub-periods without further recursion",
    "Showing 12 sub-periods without further recursion",
    NA
  )
)

# Add monthly children under first aggregate for coverage / strip.
agg_months <- tibble::tibble(
  step_id = paste0("1.3.1.", 1:12),
  level = 3L,
  parent_id = "1.3.1",
  country_id = "RU",
  indicator_code = "cpi_ind_av_ifs",
  frequency = "m",
  period = sprintf("2023-%02d", 1:12),
  value = c(115.1, 115.8, 116.4, NA, 117.0, 117.5, 118.0, 118.5, 119.0, NA, 120.0, 120.5),
  node_type = "imported",
  operation = "import",
  formula_raw = NA_character_,
  formula_filled = NA_character_,
  time_relation = "source data",
  source_name = "IMF IFS",
  note = NA_character_
)
ledger_journal <- dplyr::bind_rows(ledger_journal, agg_months)

# Distribution-down node (separate mini journal).
dist_journal <- tibble::tibble(
  step_id = c("1", "1.1"),
  level = c(0L, 1L),
  parent_id = c(NA_character_, "1"),
  country_id = "RU",
  indicator_code = c("credits_m", "credits_q"),
  frequency = c("m", "q"),
  period = c("2023-04", "2023-Q2"),
  value = c(25, 75),
  node_type = c("computed", "imported"),
  operation = c("desum_fix", "import"),
  formula_raw = c("desum_fix", NA_character_),
  formula_filled = c(NA_character_, NA_character_),
  time_relation = c("parent period 2023-Q2 at q", "source data"),
  source_name = c(NA_character_, "test"),
  note = c("Value divided by number of sub-periods", NA_character_)
)

# Technical bridge journal: root -> tech -> visible leaf.
tech_journal <- tibble::tibble(
  step_id = c("1", "1.1", "1.1.1"),
  level = c(0L, 1L, 2L),
  parent_id = c(NA_character_, "1", "1.1"),
  country_id = "RU",
  indicator_code = c("vis_root", "tech_mid", "vis_leaf"),
  frequency = c("y", "y", "y"),
  period = c("2023", "2023", "2023"),
  value = c(1, 2, 3),
  node_type = c("computed", "computed", "imported"),
  operation = c("arithmetic", "arithmetic", "import"),
  formula_raw = NA_character_,
  formula_filled = NA_character_,
  time_relation = NA_character_,
  source_name = NA_character_,
  note = NA_character_
)

tech_saveplan <- tibble::tibble(
  indicator_code = c("vis_root", "tech_mid", "vis_leaf"),
  source_frequency = "y",
  keep = c(1L, 0L, 1L),
  indicator = c("Root", "Technical", "Leaf")
)

# Truncated / opaque must survive.
special_journal <- tibble::tibble(
  step_id = c("1", "1.1", "1.2"),
  level = c(0L, 1L, 1L),
  parent_id = c(NA_character_, "1", "1"),
  country_id = "RU",
  indicator_code = c("a", "b", "c"),
  frequency = "y",
  period = "2023",
  value = c(1, NA_real_, NA_real_),
  node_type = c("computed", "opaque", "truncated"),
  operation = c("usedyn", "usedyn", NA_character_),
  formula_raw = NA_character_,
  formula_filled = NA_character_,
  time_relation = NA_character_,
  source_name = NA_character_,
  note = c(NA, "series-level", "Trace stopped at depth 8")
)

vm_coal <- build_trace_ledger_vm(ledger_journal, show_technical = TRUE)
check_true(
  "coalesce winner is arg 3 (cpi_av_ifs)",
  isTRUE(vm_coal$nodes$is_coalesce_winner[vm_coal$nodes$step_id == "1.3"])
)
check_eq(
  "coalesce winner arg index",
  vm_coal$nodes$coalesce_winner_arg[vm_coal$nodes$step_id == "1"][[1]],
  3L
)
check_true(
  "aggregate_mean marked cross-frequency",
  isTRUE(vm_coal$nodes$is_cross_frequency[vm_coal$nodes$step_id == "1.3.1"])
)
check_eq(
  "aggregate transition direction up",
  vm_coal$nodes$transition_direction[vm_coal$nodes$step_id == "1.3.1"][[1]],
  "up"
)
check_eq(
  "aggregate coverage expected 12",
  vm_coal$nodes$coverage_expected[vm_coal$nodes$step_id == "1.3.1"][[1]],
  12L
)
check_eq(
  "aggregate coverage non-NA 10",
  vm_coal$nodes$coverage_non_na[vm_coal$nodes$step_id == "1.3.1"][[1]],
  10L
)
check_true(
  "aggregate recursion_limited flag",
  isTRUE(vm_coal$nodes$recursion_limited[vm_coal$nodes$step_id == "1.3.1"])
)
check_true(
  "default collapsed includes 1.3",
  isTRUE(vm_coal$nodes$default_collapsed[vm_coal$nodes$step_id == "1.3"])
)

vm_dist <- build_trace_ledger_vm(dist_journal, show_technical = TRUE)
check_true(
  "desum_fix marked cross-frequency",
  isTRUE(vm_dist$nodes$is_cross_frequency[vm_dist$nodes$step_id == "1"])
)
check_eq(
  "desum transition direction down",
  vm_dist$nodes$transition_direction[vm_dist$nodes$step_id == "1"][[1]],
  "down"
)
check_true(
  "desum coverage_expected is NA",
  is.na(vm_dist$nodes$coverage_expected[vm_dist$nodes$step_id == "1"][[1]])
)

vm_tech_show <- build_trace_ledger_vm(
  tech_journal, saveplan_full = tech_saveplan, show_technical = TRUE
)
check_eq("technical show keeps 3 nodes", nrow(vm_tech_show$nodes), 3L)
check_eq("technical show no bridges", nrow(vm_tech_show$bridges), 0L)

vm_tech_hide <- build_trace_ledger_vm(
  tech_journal, saveplan_full = tech_saveplan, show_technical = FALSE
)
check_eq("technical hide drops mid node", nrow(vm_tech_hide$nodes), 2L)
check_true(
  "technical hide reparents leaf to root",
  identical(
    vm_tech_hide$nodes$display_parent_id[vm_tech_hide$nodes$step_id == "1.1.1"][[1]],
    "1"
  )
)
check_eq("technical hide creates bridge", nrow(vm_tech_hide$bridges), 1L)
check_eq(
  "bridge hidden_count is 1",
  vm_tech_hide$bridges$hidden_count[[1]],
  1L
)

vm_special <- build_trace_ledger_vm(special_journal, show_technical = TRUE)
check_true(
  "opaque node retained",
  "opaque" %in% vm_special$nodes$node_type
)
check_true(
  "truncated node retained",
  "truncated" %in% vm_special$nodes$node_type
)

html_out <- render_trace_ledger_tree(vm_coal)
check_true(
  "renderer returns shiny/htmltools tag",
  inherits(html_out, c("shiny.tag", "shiny.tag.list"))
)
check_true(
  "renderer HTML mentions WINNER",
  grepl("WINNER", as.character(html_out), fixed = TRUE)
)

# Imported leaf: journal with no parent_id column (tibble drops NULL).
imported_leaf_journal <- tibble::tibble(
  step_id = "1",
  level = 0L,
  country_id = "RU",
  indicator_code = "age_median",
  frequency = "y",
  period = "2023",
  value = 38.5,
  node_type = "imported",
  operation = "import",
  formula_raw = NA_character_,
  formula_filled = NA_character_,
  time_relation = "source data",
  source_name = "OWID",
  note = NA_character_
)
vm_leaf <- build_trace_ledger_vm(imported_leaf_journal, show_technical = TRUE)
check_true(
  "imported leaf VM has parent_id",
  "parent_id" %in% names(vm_leaf$nodes)
)
check_true(
  "imported leaf VM has display_parent_id",
  "display_parent_id" %in% names(vm_leaf$nodes)
)
html_leaf <- tryCatch(
  render_trace_ledger_tree(vm_leaf),
  error = function(e) e
)
check_true(
  "imported leaf renderer does not error",
  inherits(html_leaf, c("shiny.tag", "shiny.tag.list"))
)
check_true(
  "imported leaf HTML mentions age_median",
  grepl("age_median", as.character(html_leaf), fixed = TRUE)
)

# aggregate_input operand snapshot (limited recursion, no structural children).
operand_journal <- tibble::tibble(
  step_id = c("1", "1.2", paste0("1.2.", 1:12)),
  level = c(0L, 1L, rep(2L, 12L)),
  parent_id = c(NA_character_, "1", rep("1.2", 12L)),
  country_id = "RU",
  indicator_code = c("usdlc_av", "usdlc_av_temp", rep("usdlc_av", 12L)),
  frequency = c("y", "y", rep("m", 12L)),
  period = c("2000", "2000", sprintf("2000-%02d", 1:12)),
  value = c(28.1, 28.2, seq(27.8, 28.8, length.out = 12)),
  node_type = c("computed", "computed", rep("aggregate_input", 12L)),
  operation = c(
    "coalesce", "aggregate_mean",
    rep(NA_character_, 12L)
  ),
  formula_raw = c(
    "coalesce(usdlc_av_weo,usdlc_av_temp,usdlc_av_gmd)",
    "mean",
    rep(NA_character_, 12L)
  ),
  formula_filled = c(NA_character_, NA_character_, rep(NA_character_, 12L)),
  time_relation = c(
    "same period for all arguments",
    "mean over sub-periods of 2000 (m->y)",
    rep(NA_character_, 12L)
  ),
  source_name = rep(NA_character_, 14L),
  note = c(
    NA_character_,
    "Showing 12 sub-periods without further recursion",
    rep(NA_character_, 12L)
  )
)

vm_operand <- build_trace_ledger_vm(operand_journal, show_technical = TRUE)
check_eq(
  "aggregate_input VM node count excludes operands",
  nrow(vm_operand$nodes),
  2L
)
check_true(
  "aggregate_input parent has no structural children",
  !isTRUE(vm_operand$nodes$has_children[vm_operand$nodes$step_id == "1.2"])
)
operand_row_i <- which(vm_operand$nodes$step_id == "1.2")
check_eq(
  "aggregate_input strip length 12",
  length(vm_operand$nodes$strip_periods[[operand_row_i[[1]]]]),
  12L
)
check_eq(
  "aggregate_input coverage expected 12",
  vm_operand$nodes$coverage_expected[[operand_row_i[[1]]]],
  12L
)
check_true(
  "aggregate_input recursion_limited flag",
  isTRUE(vm_operand$nodes$recursion_limited[[operand_row_i[[1]]]])
)

# Regression: <=3 sub-periods keep full structural children (no aggregate_input).
small_agg_journal <- tibble::tibble(
  step_id = c("1", "1.1", "1.1.1", "1.1.2", "1.1.3"),
  level = c(0L, 1L, 2L, 2L, 2L),
  parent_id = c(NA_character_, "1", rep("1.1", 3L)),
  country_id = "RU",
  indicator_code = c("x_q", "x_m", rep("x_m", 3L)),
  frequency = c("q", "q", rep("m", 3L)),
  period = c("2020-Q2", "2020-Q2", c("2020-04", "2020-05", "2020-06")),
  value = c(10, 10, c(9, 10, 11)),
  node_type = c("computed", "computed", rep("computed", 3L)),
  operation = c("arithmetic", "aggregate_mean", rep("import", 3L)),
  formula_raw = c("x_m", "mean", rep(NA_character_, 3L)),
  formula_filled = c(NA_character_, NA_character_, rep(NA_character_, 3L)),
  time_relation = c(
    "same", "mean over sub-periods of 2020-Q2 (m->q)", rep("source data", 3L)
  ),
  source_name = rep(NA_character_, 5L),
  note = rep(NA_character_, 5L)
)
vm_small <- build_trace_ledger_vm(small_agg_journal, show_technical = TRUE)
check_true(
  "small aggregate has structural children",
  isTRUE(vm_small$nodes$has_children[vm_small$nodes$step_id == "1.1"])
)
small_row_i <- which(vm_small$nodes$step_id == "1.1")
check_eq(
  "small aggregate strip from structural children",
  length(vm_small$nodes$strip_periods[[small_row_i[[1]]]]),
  3L
)
check_true(
  "small aggregate not recursion_limited",
  !isTRUE(vm_small$nodes$recursion_limited[[small_row_i[[1]]]])
)

##### 2. Load DB + plans ------------------------------------------------------

message("\n=== Load data ===")

param_fname <- file.path(app_dir, "0_database_params.xlsx")
if (!file.exists(param_fname)) {
  param_fname <- here("assets", "_DB", "0_database_params.xlsx")
}

trace_yqm <- file.path(app_dir, "Trace_DB.rds")
filled_yqm <- file.path(app_dir, "Filled_DB.rds")
db_path <- if (file.exists(trace_yqm)) {
  message("Using Trace_DB.rds")
  list(yqm = trace_yqm, d = file.path(app_dir, "Trace_DB_d.rds"))
} else if (file.exists(filled_yqm)) {
  message("Trace_DB.rds missing — falling back to Filled_DB.rds")
  list(yqm = filled_yqm, d = file.path(app_dir, "Filled_DB_d.rds"))
} else {
  message("No Filled_DB / Trace_DB in app folder — skipping buildValueTrace smoke.")
  if (.n_fail > 0L) {
    stop(glue::glue("{.n_fail} period-helper check(s) failed."), call. = FALSE)
  }
  message("\nAll period-helper checks passed.")
  quit(save = "no", status = 0)
}

trace_db <- importData(
  yqm_file   = db_path$yqm,
  d_file     = db_path$d,
  sheet_keys = c(y = "y", q = "q", m = "m"),
  format     = "auto",
  add_time   = TRUE
)

imp_params <- readImportParams(param_fname = param_fname, update_mode = 0L)
impplan    <- imp_params$impplan
fillplan   <- readFillParams(param_fname, sheet = "fill")
saveplan_full <- build_trace_saveplan(impplan, fillplan)

##### 3. Pick smoke cases from fillplan ---------------------------------------

pick_non_na_cell <- function(code, freq, prefer_country = "RU", min_year = 2010L) {
  df <- get_freq_data(trace_db, freq)
  if (is.null(df) || !code %in% names(df)) return(NULL)
  df <- add_period_columns(df, freq)
  if ("year" %in% names(df)) {
    df <- df |> dplyr::filter(.data$year >= min_year, .data$year <= 2024L)
  }
  df <- df |>
    dplyr::filter(is.finite(.data[[code]]))

  preferred <- df |> dplyr::filter(.data$country_id == prefer_country)
  row <- if (nrow(preferred) > 0) {
    preferred |> dplyr::slice(min(20L, nrow(preferred)))
  } else if (nrow(df) > 0) {
    df |> dplyr::slice(1)
  } else {
    return(NULL)
  }
  list(
    country_id     = row$country_id[[1]],
    indicator_code = code,
    frequency      = freq,
    period         = as.character(row$period[[1]]),
    value          = row[[code]][[1]]
  )
}

##### 3b. usdlc_av@y aggregate operand snapshot ------------------------------

message("\n=== usdlc_av@y aggregate operands ===")

usdlc_cell <- pick_non_na_cell("usdlc_av", "y", prefer_country = "RU", min_year = 1998L)
if (is.null(usdlc_cell)) {
  message("skip  usdlc_av@y: no non-NA annual cell in DB")
} else {
  usdlc_journal <- tryCatch(
    buildValueTrace(
      country_id     = usdlc_cell$country_id,
      indicator_code = usdlc_cell$indicator_code,
      frequency      = usdlc_cell$frequency,
      period         = usdlc_cell$period,
      trace_db       = trace_db,
      fillplan       = fillplan,
      impplan        = impplan,
      saveplan_full  = saveplan_full,
      max_depth      = 8L,
      max_nodes      = 300L
    ),
    error = function(e) {
      .n_fail <<- .n_fail + 1L
      message(glue::glue("FAIL  usdlc_av buildValueTrace: {conditionMessage(e)}"))
      NULL
    }
  )

  if (!is.null(usdlc_journal) && nrow(usdlc_journal) > 0L) {
    temp_rows <- usdlc_journal[
      usdlc_journal$indicator_code == "usdlc_av_temp" &
        grepl("^aggregate_", usdlc_journal$operation),
      ,
      drop = FALSE
    ]
    if (nrow(temp_rows) == 0L) {
      .n_fail <<- .n_fail + 1L
      message("FAIL  usdlc_av@y: no usdlc_av_temp aggregate_mean node in journal")
    } else {
      temp_id <- temp_rows$step_id[[1]]
      operand_rows <- usdlc_journal[
        usdlc_journal$parent_id == temp_id &
          usdlc_journal$node_type == "aggregate_input",
        ,
        drop = FALSE
      ]
      check_eq(
        "usdlc_av_temp aggregate_input row count",
        nrow(operand_rows),
        12L
      )

      vm_usdlc <- build_trace_ledger_vm(usdlc_journal, saveplan_full = saveplan_full)
      temp_vm <- vm_usdlc$nodes[vm_usdlc$nodes$step_id == temp_id, , drop = FALSE]
      if (nrow(temp_vm) != 1L) {
        .n_fail <<- .n_fail + 1L
        message("FAIL  usdlc_av@y: usdlc_av_temp missing from VM")
      } else {
        check_eq(
          "usdlc_av_temp VM strip length",
          length(temp_vm$strip_periods[[1]]),
          12L
        )
        check_true(
          "usdlc_av_temp no chevron children",
          !isTRUE(temp_vm$has_children)
        )
      }
    }
  }
}

message("\n=== imported leaf age_median ===")

age_cell <- pick_non_na_cell("age_median", "y")
if (is.null(age_cell)) {
  message("skip  age_median@y: no non-NA annual cell in DB")
} else {
  age_journal <- tryCatch(
    buildValueTrace(
      country_id     = age_cell$country_id,
      indicator_code = age_cell$indicator_code,
      frequency      = age_cell$frequency,
      period         = age_cell$period,
      trace_db       = trace_db,
      fillplan       = fillplan,
      impplan        = impplan,
      saveplan_full  = saveplan_full
    ),
    error = function(e) {
      .n_fail <<- .n_fail + 1L
      message(glue::glue("FAIL  age_median buildValueTrace: {conditionMessage(e)}"))
      NULL
    }
  )
  if (!is.null(age_journal) && nrow(age_journal) > 0L) {
    check_true(
      "age_median journal has parent_id",
      "parent_id" %in% names(age_journal)
    )
    age_src <- age_journal$source_name[[1]]
    check_true(
      "age_median source is not the first WDI GDP row",
      is.na(age_src) || !grepl("NY.GDP.MKTP.KD.ZG", age_src, fixed = TRUE)
    )
    expected_age_src <- import_source_label(
      find_impplan_row("age_median", "y", impplan)
    )
    check_eq(
      "age_median source matches its impplan row",
      age_src,
      expected_age_src
    )
    html_age <- tryCatch(
      {
        vm_age <- build_trace_ledger_vm(age_journal, saveplan_full = saveplan_full)
        render_trace_ledger_tree(vm_age)
      },
      error = function(e) e
    )
    check_true(
      "age_median renderer does not error",
      inherits(html_age, c("shiny.tag", "shiny.tag.list"))
    )
  }
}

find_case <- function(label, predicate, max_scan = 40L) {
  mask <- predicate(fillplan$formula, fillplan$old_frequency, fillplan$new_frequency)
  mask <- mask %in% TRUE
  idx <- which(mask)
  if (length(idx) == 0L) {
    message(glue::glue("skip  {label}: no matching fillplan rows"))
    return(NULL)
  }
  rows <- fillplan[idx[seq_len(min(max_scan, length(idx)))], , drop = FALSE]
  for (i in seq_len(nrow(rows))) {
    code <- rows$new_indicator_code[[i]]
    freq <- rows$new_frequency[[i]]
    cell <- pick_non_na_cell(code, freq)
    if (!is.null(cell)) {
      cell$label <- label
      cell$formula <- rows$formula[[i]]
      return(cell)
    }
  }
  message(glue::glue("skip  {label}: matched fillplan but no non-NA cell in DB"))
  NULL
}

cases <- list(
  find_case(
    "arithmetic+lag",
    function(f, old, new) {
      old == new &
        grepl("lag\\s*\\(", f) &
        !grepl("roll|fromto|indexize|impute|usedyn|userat|seas_adj|coalesce", f)
    }
  ),
  find_case(
    "coalesce",
    function(f, old, new) old == new & grepl("coalesce\\s*\\(", f)
  ),
  find_case(
    "aggregate m→q",
    function(f, old, new) {
      old == "m" & new == "q" &
        f %in% c("last", "first", "mean", "max", "min", "sum")
    }
  ),
  find_case(
    "fromto",
    function(f, old, new) old == new & grepl("fromto", f)
  ),
  find_case(
    "impute",
    function(f, old, new) {
      old == new &
        new %in% c("y", "q", "m") &
        f %in% c("impute_fix", "impute_linear")
    }
  )
) |>
  purrr::compact()

##### 4. Run buildValueTrace --------------------------------------------------

message("\n=== buildValueTrace smoke ===")

if (length(cases) == 0L) {
  message("No smoke cases found — only period helpers were checked.")
} else {
  for (cell in cases) {
    message(glue::glue(
      "\n--- {cell$label}: {cell$indicator_code}@{cell$frequency} ",
      "{cell$country_id} / {cell$period} ---"
    ))
    message(glue::glue("formula: {cell$formula}"))

    journal <- tryCatch(
      buildValueTrace(
        country_id     = cell$country_id,
        indicator_code = cell$indicator_code,
        frequency      = cell$frequency,
        period         = cell$period,
        trace_db       = trace_db,
        fillplan       = fillplan,
        impplan        = impplan,
        saveplan_full  = saveplan_full,
        max_depth      = 5L,
        max_nodes      = 80L
      ),
      error = function(e) {
        .n_fail <<- .n_fail + 1L
        message(glue::glue("FAIL  buildValueTrace crashed: {conditionMessage(e)}"))
        NULL
      }
    )

    if (is.null(journal)) next

    check_true(
      glue::glue("{cell$label}: journal non-empty"),
      nrow(journal) >= 1L
    )
    check_true(
      glue::glue("{cell$label}: root step_id is 1"),
      identical(as.character(journal$step_id[[1]]), "1")
    )
    check_true(
      glue::glue("{cell$label}: root indicator matches"),
      identical(journal$indicator_code[[1]], cell$indicator_code)
    )

    imported <- journal[journal$node_type == "imported", , drop = FALSE]
    if (nrow(imported) > 0L) {
      src_ok <- TRUE
      for (i in seq_len(nrow(imported))) {
        expected_src <- import_source_label(
          find_impplan_row(
            imported$indicator_code[[i]],
            imported$frequency[[i]],
            impplan
          )
        )
        got_src <- imported$source_name[[i]]
        if (!identical(as.character(got_src), as.character(expected_src))) {
          src_ok <- FALSE
          message(glue::glue(
            "  mismatch {imported$indicator_code[[i]]}@{imported$frequency[[i]]}: ",
            "got '{got_src}', expected '{expected_src}'"
          ))
        }
      }
      check_true(
        glue::glue("{cell$label}: imported source labels match impplan"),
        src_ok
      )
    }

    print(
      journal |>
        dplyr::select(
          step_id, node_type, operation, indicator_code,
          frequency, period, value
        ) |>
        dplyr::slice_head(n = 12),
      n = 12
    )
  }
}

##### Summary -----------------------------------------------------------------

message("\n=== Summary ===")
if (.n_fail > 0L) {
  stop(glue::glue("{.n_fail} check(s) failed."), call. = FALSE)
}
message("All checks passed.")
message(
  "Manual follow-up: open Custom query → Preview data → click 3–5 cells ",
  "and cross-check journal values against an Excel download."
)
