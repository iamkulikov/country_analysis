here::i_am("download_script/owid_tool.R")
suppressPackageStartupMessages({ library(httr); library(jsonlite); library(rlang) })
source(here::here("download_script", "owid_tool.R"))

# Common OWID energy share charts
cands <- c(
  "grapher/share-elec-by-source",
  "grapher/elec-mix-bar",
  "grapher/electricity-prod-source-stacked",
  "grapher/share-of-electricity-production-by-source",
  "grapher/primary-energy-consumption-by-source",
  "grapher/energy-consumption-by-source-and-country",
  "grapher/per-capita-energy-stacked",
  "grapher/share-of-primary-energy-consumption-by-source",
  "grapher/primary-energy-source-shares",
  # relative mode on mix charts
  "grapher/energy-mix?source=total&metric=by_source&relative=true",
  "grapher/energy-mix?source=total&metric=by_source&relative=Relative",
  "grapher/electricity-mix?source=total&metric=by_source&frequency=annual&relative=true",
  "grapher/electricity-mix?source=total&metric=by_source&frequency=annual&unit=pct"
)
for (rc in cands) {
  cat("\n-- ", rc, "\n", sep = "")
  p <- tryCatch(owid_parse_code(rc), error = function(e) e)
  if (inherits(p, "error")) next
  meta <- tryCatch(owid_fetch_metadata(p), error = function(e) e)
  if (inherits(meta, "error")) { cat("fail: ", meta$message, "\n", sep = ""); next }
  cols <- owid_metadata_columns(meta)
  cat("n=", length(cols), ": ", paste(head(cols, 12), collapse = " | "), "\n", sep = "")
}
