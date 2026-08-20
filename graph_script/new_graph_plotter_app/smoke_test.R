# Smoke tests for new_graph_plotter_app (phase 9 regression — headless).
# Run from repo root: Rscript graph_script/new_graph_plotter_app/smoke_test.R
#
# Manual UI checklist: UI_CHECKLIST.md in this folder.
#
# Profile baseline (optional):
#   PLOTTER_PROFILE=1 Rscript graph_script/new_graph_plotter_app/smoke_test.R
# Full buildable scale (phase 10):
#   PLOTTER_PROFILE=1 PLOTTER_PROFILE_ALL_BUILDABLE=1 Rscript .../smoke_test.R
# Dedicated scale research + report:
#   Rscript graph_script/new_graph_plotter_app/profile_gallery_scale.R
# or in R: options(plotter.profile = TRUE) before sourcing.
#
# Fixture: 2_graphlib.xlsx in app folder (136 rows, 4 active for RUS).
# Broken row when activated: budg_debttorev_dist (indicator not in DB).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
})

here::i_am("graph_script/new_graph_plotter_app/app.R")
app_dir <- here::here("graph_script", "new_graph_plotter_app")

for (f in c(
  "check_graphplan.R", "prepare_elements.R", "plot_themes.R",
  "plot_types.R", "service.R"
)) {
  source(file.path(app_dir, f), local = FALSE)
}

# Smoke test is launched from repo root, so `here()` points to the repository,
# not to the app directory. Resolve deploy data relative to `app_dir` to match
# the app's runtime contract: CONNECT_DATA_DIR -> app/deploy_data -> app root.
resolve_smoke_data_dir <- function(app_dir) {
  env <- Sys.getenv("CONNECT_DATA_DIR", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }

  deploy_sub <- file.path(app_dir, "deploy_data")
  if (dir.exists(deploy_sub)) {
    return(normalizePath(deploy_sub, winslash = "/", mustWork = TRUE))
  }

  normalizePath(app_dir, winslash = "/", mustWork = TRUE)
}

r_files <- setdiff(
  list.files(app_dir, pattern = "\\.R$", full.names = TRUE),
  file.path(app_dir, "smoke_test.R")
)
for (rf in r_files) {
  if (any(grepl("eval\\(parse", readLines(rf, warn = FALSE)))) {
    stop("eval(parse) found in ", rf)
  }
}

data_dir <- resolve_smoke_data_dir(app_dir)
peers_fname <- file.path(data_dir, "1_peers_params.xlsx")
data_fname <- file.path(data_dir, "Filled_DB.rds")
data_d_fname <- file.path(data_dir, "Filled_DB_d.rds")
graphplan_path <- file.path(app_dir, "2_graphlib.xlsx")

fixture_country_iso3c <- "RUS"
fixture_broken_graph_name <- "budg_debttorev_dist"

if (!file.exists(data_fname)) {
  stop("Deploy RDS missing: ", data_fname)
}
if (!file.exists(graphplan_path)) {
  stop("Fixture missing: ", graphplan_path)
}

rds_mtime_before <- file.info(data_fname)$mtime

FD <- importData(
  yqm_file = data_fname,
  d_file = data_d_fname,
  sheet_keys = c(y = "y", q = "q", m = "m"),
  format = "auto",
  add_time = TRUE
)

# ---------- Fixture graphplan (2_graphlib.xlsx) -------------------------------

imported <- read_graphplan_file(graphplan_path, dict = FD$dict)
plan <- imported$plan
stopifnot(nrow(plan) >= 1L)
message("Fixture loaded: ", nrow(plan), " rows")

val <- validate_graphplan_for_app(plan, FD, fixture_country_iso3c, peers_fname)
s <- val$summary
stopifnot(
  s$n_active[[1]] == 9L,
  s$n_buildable[[1]] == 8L,
  s$n_errors[[1]] == 1L
)
message("Fixture validation (RUS, default active): 9 active, 8 buildable, 1 active error row")

bl <- graphplan_baseline_capture(plan)
stopifnot(nrow(bl) == nrow(plan), !graphplan_row_edited_excluding_active(1L, plan, bl))
stopifnot(graphplan_row_edited_hybrid(1L, plan, bl, 1L))
stopifnot(!meaningful_editor_save_for_edited_flag(plan[1, , drop = FALSE], {
  x <- plan[1, , drop = FALSE]
  x$active[[1]] <- 0L
  x
}))
stopifnot(meaningful_editor_save_for_edited_flag(plan[1, , drop = FALSE], {
  x <- plan[1, , drop = FALSE]
  x$graph_title[[1]] <- paste0(x$graph_title[[1]], " (smoke)")
  x
}))
stopifnot(sum(val$row_status$check_status == "valid") == 135L)
stopifnot("limits" %in% names(val$row_status), "peers" %in% names(val$row_status))
limits_modes <- table(val$row_status$limits, useNA = "ifany")
peers_modes <- table(val$row_status$peers, useNA = "ifany")
stopifnot(all(c("auto", "manual") %in% names(limits_modes)))
stopifnot(all(c("auto", "manual") %in% names(peers_modes)))
empty_limits_row <- tibble::tibble(
  time_fix = NA_character_,
  x_min = NA_character_,
  x_max = NA_character_,
  y_min = NA_real_,
  y_max = NA_real_,
  peers = 0
)
stopifnot(
  graphplan_limits_display_mode(empty_limits_row) == "auto",
  graphplan_peers_display_mode(empty_limits_row) == "auto"
)
manual_limits_row <- empty_limits_row
manual_limits_row$time_fix <- "2020"
stopifnot(graphplan_limits_display_mode(manual_limits_row) == "manual")
auto_peers_row <- empty_limits_row
auto_peers_row$peers <- "default"
stopifnot(graphplan_peers_display_mode(auto_peers_row) == "auto")
custom_peers_row <- empty_limits_row
custom_peers_row$peers <- "custom: US, DE"
stopifnot(graphplan_peers_display_mode(custom_peers_row) == "manual")
message("Import table Limits/Peers display modes: OK")
message("Baseline capture + hybrid Edited (17.4) helpers: OK")

# ---------- Use for new / copy suffix helper ----------------------------------

stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "ec", NULL),
    "gdp_growth_copy"
  ),
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "ec", tibble::tibble()),
    "gdp_growth_copy"
  )
)
plan_with_copy <- tibble::tibble(
  graph_name = c("ec_gdp_growth", "ec_gdp_growth_copy", "budg_other")
)
stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "ec", plan_with_copy),
    "gdp_growth_copy2"
  )
)
plan_with_copy2 <- tibble::tibble(
  graph_name = c(
    "ec_gdp_growth",
    "ec_gdp_growth_copy",
    "ec_gdp_growth_copy2",
    "budg_other"
  )
)
stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "ec", plan_with_copy2),
    "gdp_growth_copy3"
  )
)
# Same suffix in another group does not collide.
stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "budg", plan_with_copy),
    "gdp_growth_copy"
  )
)
# Inactive / any row name is considered (collision avoidance).
plan_inactive_copy <- tibble::tibble(
  graph_name = "ec_gdp_growth_copy",
  active = 0L
)
stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth", "ec", plan_inactive_copy),
    "gdp_growth_copy2"
  )
)
# Suffix that already ends with _copy gets another marker.
stopifnot(
  identical(
    editor_copy_graph_name_suffix("gdp_growth_copy", "ec", plan_with_copy),
    "gdp_growth_copy_copy"
  )
)
message("editor_copy_graph_name_suffix (Use for new): OK")

# ---------- Insert-after helpers (Use for new placement) ----------------------

seed_row <- plan[1, , drop = FALSE]
seed_row$graph_name[[1]] <- "ec_a_copy"
seed_row$graph_title[[1]] <- "A copy"
inserted <- insert_graphplan_row_after(plan, 1L, seed_row)
stopifnot(
  nrow(inserted) == nrow(plan) + 1L,
  identical(as.character(inserted$graph_name[[1]]), as.character(plan$graph_name[[1]])),
  identical(as.character(inserted$graph_name[[2]]), "ec_a_copy"),
  identical(
    as.character(inserted$graph_name[[3]]),
    as.character(plan$graph_name[[2]])
  )
)
# Insert after last == append position
inserted_last <- insert_graphplan_row_after(plan, nrow(plan), seed_row)
stopifnot(
  nrow(inserted_last) == nrow(plan) + 1L,
  identical(
    as.character(inserted_last$graph_name[[nrow(inserted_last)]]),
    "ec_a_copy"
  )
)
# Out-of-range falls back to append
inserted_oob <- insert_graphplan_row_after(plan, 0L, seed_row)
stopifnot(
  identical(
    as.character(inserted_oob$graph_name[[nrow(inserted_oob)]]),
    "ec_a_copy"
  )
)
built_shift <- list(
  ec_a = list(row_id = 1L, graph_name = "ec_a", ok = TRUE),
  ec_b = list(row_id = 2L, graph_name = "ec_b", ok = TRUE),
  ec_c = list(row_id = 3L, graph_name = "ec_c", ok = TRUE)
)
built_shifted <- shift_built_list_row_ids_after(built_shift, 1L)
stopifnot(
  identical(as.integer(built_shifted$ec_a$row_id), 1L),
  identical(as.integer(built_shifted$ec_b$row_id), 3L),
  identical(as.integer(built_shifted$ec_c$row_id), 4L)
)
stopifnot(
  identical(shift_row_ids_after(c(1L, 2L, 5L), 1L), c(1L, 3L, 6L))
)
base_cap <- graphplan_baseline_capture(plan[1:3, , drop = FALSE])
base_ins <- insert_graphplan_baseline_row_after(base_cap, 1L, seed_row)
stopifnot(
  nrow(base_ins) == 4L,
  identical(as.character(base_ins$graph_name[[2]]), "ec_a_copy"),
  identical(
    as.character(base_ins$graph_name[[3]]),
    as.character(base_cap$graph_name[[2]])
  )
)
# after_row_id beyond baseline: unchanged
stopifnot(
  identical(
    nrow(insert_graphplan_baseline_row_after(base_cap, 99L, seed_row)),
    nrow(base_cap)
  )
)
message("insert_graphplan_row_after + row_id shift helpers: OK")

broken <- val$row_status |>
  dplyr::filter(.data$graph_name == fixture_broken_graph_name)
stopifnot(
  nrow(broken) == 1L,
  broken$check_status[[1]] == "error",
  !isTRUE(broken$can_build[[1]]),
  grepl("Indicator not available", broken$messages[[1]])
)
message("Broken row active validation error: ", fixture_broken_graph_name)

manifest_default <- build_gallery_manifest(val, list())
default_error_card <- manifest_default$active[[fixture_broken_graph_name]]
stopifnot(
  default_error_card$display_status == "validation_error",
  grepl("Indicator not available", default_error_card$messages[[1]])
)
stopifnot(
  sum(vapply(
    manifest_default$active,
    function(c) c$display_status == "not_built",
    logical(1)
  )) == 8L
)
message("Gallery manifest (validate, no build): active validation error + 8 not_built rows: OK")

plan_broken_off <- plan
plan_broken_off$active[broken$row_id[[1]]] <- 0L
val_broken_off <- validate_graphplan_for_app(
  plan_broken_off, FD, fixture_country_iso3c, peers_fname
)
manifest_inactive <- build_gallery_manifest(val_broken_off, list())
stopifnot(
  length(manifest_inactive$inactive) >= 1L,
  manifest_inactive$inactive[[fixture_broken_graph_name]]$display_status == "inactive"
)
message("Gallery manifest after deactivate (inactive card): OK")

plan_activated <- activate_graphplan_row(plan_broken_off, broken$row_id[[1]])
val_activated <- validate_graphplan_for_app(
  plan_activated, FD, fixture_country_iso3c, peers_fname
)
err_row <- val_activated$row_status |>
  dplyr::filter(.data$graph_name == fixture_broken_graph_name)
stopifnot(
  err_row$check_status[[1]] == "error",
  !isTRUE(err_row$can_build[[1]]),
  grepl("Indicator not available", err_row$messages[[1]])
)
message("Broken row error when active=1: OK")
manifest_activated <- build_gallery_manifest(val_activated, list())
activated_card <- manifest_activated$active[[fixture_broken_graph_name]]
stopifnot(
  activated_card$display_status == "validation_error",
  grepl("Indicator not available", activated_card$messages[[1]])
)
message("Gallery manifest after activate (validation error card): OK")

stopifnot(!graphplan_row_edited_b_vs_baseline(broken$row_id[[1]], plan_activated, bl))

build_ids <- val$row_status |>
  dplyr::filter(.data$can_build) |>
  dplyr::pull(.data$row_id)
stopifnot(length(build_ids) >= 2L)

dup_row <- plan[build_ids[1], , drop = FALSE]
dup_row$graph_name <- plan$graph_name[[build_ids[2]]]
dup_val <- validate_graphplan_row(
  dup_row,
  FD,
  fixture_country_iso3c,
  peers_fname,
  graphplan = plan,
  row_id = build_ids[1],
  editor_mode = "edit"
)
stopifnot(
  !isTRUE(dup_val$can_build),
  grepl("Duplicate graph name", dup_val$messages[[1]])
)
message("Editor row validation uses full graphplan (duplicate name): OK")

built_list <- list()
for (rid in build_ids) {
  b <- build_graph_row(plan[rid, , drop = FALSE], FD, fixture_country_iso3c, peers_fname)
  stopifnot(isTRUE(b$ok))
  built_list[[b$graph_name]] <- c(b, list(row_id = rid, status = "ok"))
}
message("Built ", length(built_list), " active fixture graphs: OK")

pruned_ok <- prune_built_list_for_validation(built_list, val)
stopifnot(
  length(pruned_ok) == length(built_list),
  setequal(names(pruned_ok), names(built_list))
)
message("prune_built_list_for_validation keeps all buildable rows: OK")

thumb_tmp <- tempfile("gallery_thumbs_smoke_")
dir.create(thumb_tmp)
first_nm <- names(built_list)[1]
enriched <- enrich_gallery_built_item(built_list[[first_nm]], thumb_tmp)
stopifnot(
  !is.null(enriched$thumb_path),
  file.exists(enriched$thumb_path)
)
gp <- enriched$graph_params
thumb_img <- png::readPNG(enriched$thumb_path, info = TRUE)
png_dim <- attr(thumb_img, "dim")
stopifnot(
  png_dim[1] == gp$width,
  png_dim[2] == gp$height
)
dims <- gallery_thumb_dims(gp)
stopifnot(
  dims$height == gallery_thumb_height_px,
  dims$width == max(1L, round(gallery_thumb_height_px * gp$width / gp$height))
)
stopifnot(isTRUE(gallery_defer_ui_enabled()))
cleanup_gallery_thumb_dir(thumb_tmp)
hints <- editor_field_hints()
stopifnot(
  length(hints) >= 10L,
  identical(hints$ed_graph_type, "Check Graph examples"),
  grepl("autofill", hints$ed_time_fix, fixed = TRUE),
  nzchar(hints$ed_active),
  editor_default_graph_type %in% graph_types,
  editor_default_peers %in% peers_choice,
  editor_default_theme %in% theme_types,
  editor_default_orientation %in% orient_types
)
seed <- editor_new_graph_seed_row()
stopifnot(
  seed$graph_type[[1]] == editor_default_graph_type,
  seed$peers[[1]] == editor_default_peers,
  seed$theme[[1]] == editor_default_theme,
  seed$orientation[[1]] == editor_default_orientation
)
message("Editor field hints: OK")

stopifnot(
  editor_y_limit_to_text(NA) == "",
  editor_y_limit_to_text(12.5) == "12.5",
  is.na(parse_editor_y_limit("")),
  parse_editor_y_limit("3.14") == 3.14
)
message("Editor y limit text helpers: OK")

horiz_dims <- editor_preview_display_dims(list(width = 1800, height = 900))
vert_dims <- editor_preview_display_dims(list(width = 850, height = 850))
stopifnot(
  horiz_dims$height == editor_preview_max_height_px,
  horiz_dims$width == round(editor_preview_max_height_px * 1800 / 900),
  vert_dims$height == editor_preview_max_height_px,
  vert_dims$width == vert_dims$height
)
message("Gallery export-fidelity thumbnails + display dims: OK")

tmp <- tempfile("export_smoke_")
dir.create(tmp)
zip_path <- file.path(tmp, "graphs_fixture.zip")
export_built_graphs_zip(built = built_list, zip_path = zip_path, device = "png")
stopifnot(file.exists(zip_path), file.info(zip_path)$size > 0)
message("Fixture graphs zip: OK")

xlsx_out <- file.path(tmp, "2_graphlib_out.xlsx")
title_src <- graphplan_export_title_row(
  country_iso3c = fixture_country_iso3c,
  country_label = "Russian Federation",
  base = imported$title_row %||% default_graphplan_title_row(),
  fd = FD
)
info_default <- default_graphplan_info()
peers_info <- info_default$`возможные значения`[info_default$`поле` == "peers"][[1]]
gt_info <- info_default$`возможные значения`[info_default$`поле` == "graph_type"][[1]]
stopifnot(
  nrow(info_default) == 27L,
  grepl("scatter_before_after", gt_info),
  grepl("фиксированный момент времени для одной страны", gt_info),
  !grepl("еще не реализован, каждая точка", gt_info),
  grepl("Имя файла графика", info_default$`возможные значения`[info_default$`поле` == "graph_name"][[1]]),
  grepl("similar: hci", peers_info),
  grepl("custom: KZ", peers_info),
  grepl("acra_light \\(по умолчанию\\)", info_default$`возможные значения`[info_default$`поле` == "theme"][[1]]),
  grepl("economist", info_default$`возможные значения`[info_default$`поле` == "theme"][[1]]),
  grepl("black_white", info_default$`возможные значения`[info_default$`поле` == "theme"][[1]]),
  grepl("viridis", info_default$`возможные значения`[info_default$`поле` == "theme"][[1]]),
  grepl("ipsum", info_default$`возможные значения`[info_default$`поле` == "theme"][[1]])
)
message("default_graphplan_info: OK")

export_graphplan_xlsx(
  plan = strip_graphplan_check_artifacts(val$plan),
  path = xlsx_out,
  info = info_default,
  title_row = title_src
)
reimport <- read_graphplan_file(xlsx_out, dict = FD$dict)
stopifnot(nrow(reimport$plan) == nrow(plan))
wb_out <- openxlsx::loadWorkbook(xlsx_out)
stopifnot(
  "library" %in% names(wb_out),
  "info" %in% names(wb_out),
  grepl("frozen", wb_out$worksheets[[1]]$freezePane %||% "", fixed = TRUE)
)
title_out <- read_graphplan_title_row(xlsx_out, graphplan_columns)
stopifnot(
  identical(title_out$graph_type[[1]], "Russian Federation"),
  identical(unlist(title_out[1:3]), unlist(title_src[1:3]))
)
message("Fixture xlsx export round-trip + graphlib styling: OK")

data_xlsx_out <- file.path(tmp, "graph_data_out.xlsx")
export_graph_data_xlsx(
  built = built_list,
  path = data_xlsx_out,
  country_iso3c = fixture_country_iso3c
)
stopifnot(file.exists(data_xlsx_out), file.info(data_xlsx_out)$size > 0)
first_built <- built_list[[1]]
rid_export <- build_ids[1]
pruned <- prune_export_data_columns(
  first_built$data,
  graph_type = first_built$graph_params$graph_type,
  data_frequency = first_built$graph_params$data_frequency,
  graphplan_row = plan[rid_export, , drop = FALSE],
  graph_params = first_built$graph_params
)
stopifnot(
  !("value_raw" %in% names(pruned)),
  !("value_plot" %in% names(pruned)),
  "value" %in% names(pruned),
  tail(names(pruned), 1L) == "value",
  !("quarter" %in% names(pruned)),
  !("month" %in% names(pruned)),
  !("date" %in% names(pruned))
)
nms <- names(pruned)
duplicate_pairs <- character()
for (i in seq_along(nms)) {
  if (i >= length(nms)) break
  for (j in (i + 1L):length(nms)) {
    if (columns_semantically_equal(pruned[[nms[[i]]]], pruned[[nms[[j]]]])) {
      duplicate_pairs <- c(duplicate_pairs, paste(nms[[i]], nms[[j]], sep = "::"))
    }
  }
}
stopifnot(identical(unique(duplicate_pairs), "year::time"))
editor_data_out <- file.path(tmp, "editor_data_out.xlsx")
export_graph_data_workbook(
  item = first_built,
  path = editor_data_out,
  country_iso3c = fixture_country_iso3c,
  graphplan_row = plan[rid_export, , drop = FALSE],
  dict = FD$dict,
  peers_iso2c = first_built$peers_iso2c,
  country_iso2c = first_built$country_iso2c,
  country_label = "Russian Federation",
  fd = FD
)
sheets_out <- readxl::excel_sheets(editor_data_out)
stopifnot(
  file.exists(editor_data_out),
  identical(sheets_out, c("data", "meta", "recipe"))
)
recipe_hdr <- readxl::read_excel(editor_data_out, sheet = "recipe", col_names = FALSE, n_max = 1)
stopifnot(identical(as.character(unlist(recipe_hdr[1, ])), graphplan_columns))
meta_tbl <- readxl::read_excel(editor_data_out, sheet = "meta")
cust1_row <- meta_tbl$value[!is.na(meta_tbl$field) & meta_tbl$field == "cust1_recipe"]
country_name_row <- meta_tbl$value[!is.na(meta_tbl$field) & meta_tbl$field == "country_name"]
stopifnot(
  length(cust1_row) == 1L,
  startsWith(cust1_row[[1]], "CUST1:"),
  length(country_name_row) == 1L,
  nzchar(country_name_row[[1]])
)
message("Graph data export (pruned columns + workbook): OK")

# ---------- Minimal synthetic row (export helpers) --------------------------

row <- tibble::tibble(
  graph_name = "ec_smoketest",
  graph_title = "Smoke test",
  graph_type = "lines_country_comparison",
  graph_group = "macro",
  data_frequency = "y",
  indicators = "gg_bal_gdp_weo",
  time_fix = NA_character_,
  peers = "default",
  all = 0L, x_log = 0L, y_log = 0L,
  x_min = "2010", x_max = "2023",
  y_min = NA_real_, y_max = NA_real_,
  trend_type = NA_character_,
  index = 0L, recession = 0L, sec_y_axis = NA_character_,
  swap_axis = 0L, long_legend = 0L, vert_lab = 0L, short_names = 0L,
  theme = "ipsum", orientation = "horizontal", show_title = 1L, active = 1L
)

built_one <- build_graph_row(row, FD, fixture_country_iso3c, peers_fname)
stopifnot(isTRUE(built_one$ok))

recipe <- export_graphplan_recipes_text(row)
parsed <- parse_graphplan_row_tsv(recipe)
stopifnot(identical(as.character(parsed$graph_name), as.character(row$graph_name)))
message("Synthetic recipe TSV round-trip: OK")

# do_plot pipeline parity on one fixture row
rid <- build_ids[1]
built_ref <- build_graph_row(plan[rid, , drop = FALSE], FD, fixture_country_iso3c, peers_fname)
country_info <- getPeersCodes(country_iso3c = fixture_country_iso3c, peers_fname = peers_fname)
graph_params <- parseGraphPlan(
  graphrow = plan[rid, , drop = FALSE],
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
stopifnot(
  built_ref$graph_params$width == graph_params$width,
  built_ref$graph_params$height == graph_params$height,
  built_ref$graph_params$graph_name == graph_params$graph_name
)
message("do_plot pipeline parity on fixture row: OK")

# ---------- Trend type + peers formula smoke ----------------------------------

trend_row <- tibble::tibble(
  graph_name = "ec_trend_smoke",
  graph_title = "Trend smoke",
  graph_type = "scatter_country_comparison",
  graph_group = "macro",
  data_frequency = "y",
  indicators = "gg_bal_gdp_weo, gg_rev_gdp_weo",
  time_fix = "2020",
  peers = "default",
  trend_type = NA_character_,
  active = 1L
)
trend_state <- graphplan_row_to_editor_state(trend_row)
stopifnot(identical(trend_state$trend_type, "none"))
trend_back <- editor_inputs_to_graphplan_row(trend_state, dict = FD$dict)
trend_back_val <- as.character(trend_back$trend_type[[1]])
stopifnot(is.na(trend_back$trend_type[[1]]) || !nzchar(trend_back_val))
trend_state2 <- graphplan_row_to_editor_state(trend_back)
stopifnot(identical(trend_state2$trend_type, "none"))
stopifnot(
  editor_trend_type_to_ui(NA) == "none",
  editor_trend_type_to_ui("") == "none",
  editor_trend_type_to_ui(NULL) == "none",
  editor_trend_type_to_ui("NA") == "none",
  editor_trend_type_to_ui("lm") == "lm"
)
message("Trend type editor round-trip (NA -> none -> NA): OK")

country_info_smoke <- get_peers_cached(
  country_iso3c = fixture_country_iso3c,
  peers_fname = peers_fname
)
invalid_peers_row <- plan[build_ids[1], , drop = FALSE]
invalid_peers_row$peers <- "similar: NONEXISTENT_IND, 0.2, 2020"
invalid_peers_row$active <- 1L
invalid_chk <- checkPeers(
  graphplan = invalid_peers_row,
  peer_groups = country_info_smoke,
  dict = FD$dict,
  warn_invalid = FALSE
)
stopifnot(invalid_chk$check_peers[[1]] == 0L)
message("Invalid peer formula (unknown indicator): check_peers == 0: OK")

empty_peers_formula <- "similar: gdp_g, 0.2, 1800"
empty_peers_fix <- fixPeers(
  country_info = country_info_smoke,
  params = list(peers = empty_peers_formula, graph_type = "bar_country_comparison"),
  data = FD,
  warn_invalid = FALSE
)
stopifnot(length(empty_peers_fix) == 0L)
empty_peers_row <- plan[build_ids[1], , drop = FALSE]
empty_peers_row$peers <- empty_peers_formula
empty_peers_row$active <- 1L
built_empty_peers <- build_graph_row(
  empty_peers_row, FD, fixture_country_iso3c, peers_fname
)
stopifnot(
  isTRUE(built_empty_peers$ok),
  length(built_empty_peers$peers_iso2c) == 0L
)
message("Runtime-empty peer formula (fixPeers + build_graph_row): OK")

scatter_trend_base <- tibble::tibble(
  graph_name = "ec_scatter_trend_smoke",
  graph_title = "Scatter trend smoke",
  graph_type = "scatter_country_comparison",
  graph_group = "macro",
  data_frequency = "y",
  indicators = "gg_bal_gdp_weo, gg_rev_gdp_weo",
  time_fix = "2020",
  peers = "default",
  all = 0L, x_log = 0L, y_log = 0L,
  x_min = NA_character_, x_max = NA_character_,
  y_min = NA_real_, y_max = NA_real_,
  trend_type = NA_character_,
  index = 0L, recession = 0L, sec_y_axis = NA_character_,
  swap_axis = 0L, long_legend = 0L, vert_lab = 0L, short_names = 0L,
  theme = "ipsum", orientation = "horizontal", show_title = 1L, active = 1L
)
for (tt in c("rlm", "loess_sym")) {
  row_tt <- scatter_trend_base
  row_tt$trend_type <- tt
  built_tt <- build_graph_row(row_tt, FD, fixture_country_iso3c, peers_fname)
  stopifnot(isTRUE(built_tt$ok), identical(built_tt$graph_params$trend_type, tt))
}
message("Robust trend types (rlm, loess_sym) build: OK")

# ---------- Theme styles: one graph per theme + legacy / ggpattern ------------

theme_smoke_base <- tibble::tibble(
  graph_name = "ec_theme_smoke",
  graph_title = "Theme smoke",
  graph_type = "lines_country_comparison",
  graph_group = "macro",
  data_frequency = "y",
  indicators = "gg_bal_gdp_weo",
  time_fix = NA_character_,
  peers = "default",
  all = 0L, x_log = 0L, y_log = 0L,
  x_min = "2010", x_max = "2023",
  y_min = NA_real_, y_max = NA_real_,
  trend_type = NA_character_,
  index = 0L, recession = 0L, sec_y_axis = NA_character_,
  swap_axis = 0L, long_legend = 0L, vert_lab = 0L, short_names = 0L,
  orientation = "horizontal", show_title = 1L, active = 1L
)

for (th in theme_types) {
  row_th <- theme_smoke_base
  row_th$graph_name <- paste0("ec_theme_", th)
  row_th$theme <- th
  built_th <- build_graph_row(row_th, FD, fixture_country_iso3c, peers_fname)
  stopifnot(isTRUE(built_th$ok), inherits(built_th$graph, "ggplot"))
  st <- resolve_plot_style(th)
  stopifnot(identical(st$name, th))
  if (identical(th, "black_white")) {
    stopifnot(identical(st$differentiate, "bw"))
  }
}
message("One graph per theme (", length(theme_types), "): OK")

empty_theme_row <- theme_smoke_base
empty_theme_row$graph_name <- "ec_theme_empty_default"
empty_theme_row$theme <- NA_character_
built_empty_theme <- build_graph_row(
  empty_theme_row, FD, fixture_country_iso3c, peers_fname
)
stopifnot(
  isTRUE(built_empty_theme$ok),
  identical(built_empty_theme$graph_params$theme_name, "acra_light")
)
message("Empty theme -> acra_light default: OK")

minimal_warn <- NULL
withCallingHandlers(
  {
    stopifnot(identical(normalize_theme_name("minimal"), "acra_light"))
    stopifnot(identical(
      resolve_plot_style("minimal")$name,
      "acra_light"
    ))
  },
  warning = function(w) {
    if (grepl("minimal.*deprecated", conditionMessage(w), ignore.case = TRUE)) {
      minimal_warn <<- w
      invokeRestart("muffleWarning")
    }
  }
)
stopifnot(!is.null(minimal_warn))
message("Legacy theme minimal -> acra_light + warning: OK")

ggpat_installed <- requireNamespace("ggpattern", quietly = TRUE)
stopifnot(identical(ggpattern_available(), ggpat_installed))
bw_style <- resolve_plot_style("black_white")
if (ggpat_installed) {
  stopifnot(isTRUE(ggpattern_available()))
  message("ggpattern available: black_white uses pattern geoms")
} else {
  stopifnot(!ggpattern_available())
  fills <- series_grey_fills(c("a", "b", "c"), bw_style)
  stopifnot(
    length(fills) == 3L,
    all(nzchar(fills)),
    identical(names(fills), c("a", "b", "c"))
  )
  message("ggpattern unavailable: greyscale fallback paths active")
}

rds_mtime_after <- file.info(data_fname)$mtime
stopifnot(identical(rds_mtime_before, rds_mtime_after))
message("Filled_DB.rds mtime unchanged: OK")

app_r_lines <- readLines(file.path(app_dir, "app.R"), warn = FALSE)
server_line <- grep("^server\\s*<-\\s*function", app_r_lines)[1]
stopifnot(length(server_line) == 1L, !is.na(server_line))
import_in_server <- any(
  grepl("importData\\s*\\(", app_r_lines) &
    seq_along(app_r_lines) > server_line
)
stopifnot(!import_in_server)
message("app.R: importData only at startup (not in server): OK")

stopifnot(
  any(grepl("Use for new", app_r_lines, fixed = TRUE)),
  any(grepl('gallery_action_js\\("use_for_new"', app_r_lines)),
  any(grepl('mode = "copy"', app_r_lines, fixed = TRUE)),
  any(grepl("editor_copy_source_name", app_r_lines, fixed = TRUE)),
  any(grepl("editor_copy_source_row_id", app_r_lines, fixed = TRUE)),
  any(grepl("insert_graphplan_row_after", app_r_lines, fixed = TRUE))
)
message("app.R: Use for new gallery action wired: OK")

if (plotter_profile_enabled()) {
  profile_all <- plotter_profile_all_buildable_enabled()
  message(if (profile_all) {
    "--- Profile baseline (all buildable rows, phase 10) ---"
  } else {
    "--- Profile baseline (typical RUS fixture) ---"
  })
  timing <- run_plotter_profile_baseline(
    FD = FD,
    plan = plan,
    country_iso3c = fixture_country_iso3c,
    peers_fname = peers_fname,
    import_paths = list(
      yqm_file = data_fname,
      d_file = data_d_fname,
      sheet_keys = c(y = "y", q = "q", m = "m"),
      format = "auto",
      add_time = TRUE
    ),
    update_plot_reps = if (profile_all) 0L else 5L,
    profile_all_buildable = profile_all,
    per_graph_timing = profile_all,
    simulate_gallery_ui = profile_all
  )
  print_plotter_profile_baseline(timing)
  min_steps <- if (profile_all) 3L else 4L
  stopifnot(nrow(timing) >= min_steps)
  message("Profile baseline recorded ", nrow(timing), " step(s).")
}

message("All smoke tests passed.")
