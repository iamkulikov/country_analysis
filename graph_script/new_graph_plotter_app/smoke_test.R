# Smoke tests for new_graph_plotter_app.
# Run from repo root: Rscript graph_script/new_graph_plotter_app/smoke_test.R
#
# Profile baseline (optional):
#   PLOTTER_PROFILE=1 Rscript graph_script/new_graph_plotter_app/smoke_test.R
# or in R: options(plotter.profile = TRUE) before sourcing.
#
# Fixture: 2_graphlib.xlsx in app folder (136 rows, 4 active for RUS).
# Broken row when activated: budg_debttorev_dist (indicator not in DB).

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tibble)
  library(unikn)
})

here::i_am("graph_script/new_graph_plotter_app/app.R")
app_dir <- here::here("graph_script", "new_graph_plotter_app")

for (f in c(
  "check_graphplan.R", "prepare_elements.R", "plot_themes.R",
  "plot_types.R", "service.R"
)) {
  source(file.path(app_dir, f), local = FALSE)
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

peers_fname <- file.path(app_dir, "1_peers_params.xlsx")
data_fname <- file.path(app_dir, "Filled_DB.rds")
data_d_fname <- file.path(app_dir, "Filled_DB_d.rds")
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
  s$n_active[[1]] == 4L,
  s$n_buildable[[1]] == 4L,
  s$n_errors[[1]] == 0L
)
message("Fixture validation (RUS, default active): 4 active, 4 buildable, 0 errors")

broken <- val$row_status |>
  dplyr::filter(.data$graph_name == fixture_broken_graph_name)
stopifnot(nrow(broken) == 1L, broken$check_status[[1]] == "inactive")
message("Broken row present as inactive: ", fixture_broken_graph_name)

plan_broken_on <- plan
plan_broken_on$active[broken$row_id[[1]]] <- 1L
val_broken <- validate_graphplan_for_app(
  plan_broken_on, FD, fixture_country_iso3c, peers_fname
)
err_row <- val_broken$row_status |>
  dplyr::filter(.data$graph_name == fixture_broken_graph_name)
stopifnot(
  err_row$check_status[[1]] == "error",
  !isTRUE(err_row$can_build[[1]]),
  grepl("Indicator not available", err_row$messages[[1]])
)
message("Broken row error when active=1: OK")

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

tmp <- tempfile("export_smoke_")
dir.create(tmp)
zip_path <- file.path(tmp, "graphs_fixture.zip")
export_built_graphs_zip(built = built_list, zip_path = zip_path, device = "png")
stopifnot(file.exists(zip_path), file.info(zip_path)$size > 0)
message("Fixture graphs zip: OK")

xlsx_out <- file.path(tmp, "2_graphlib_out.xlsx")
export_graphplan_xlsx(
  plan = strip_graphplan_check_artifacts(val$plan),
  path = xlsx_out,
  info = imported$info %||% default_graphplan_info()
)
reimport <- read_graphplan_file(xlsx_out, dict = FD$dict)
stopifnot(nrow(reimport$plan) == nrow(plan))
message("Fixture xlsx export round-trip: OK")

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

if (plotter_profile_enabled()) {
  message("--- Profile baseline (typical RUS fixture) ---")
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
    update_plot_reps = 5L
  )
  print_plotter_profile_baseline(timing)
  stopifnot(nrow(timing) >= 4L)
  message("Profile baseline recorded ", nrow(timing), " step(s).")
}

message("All smoke tests passed.")
