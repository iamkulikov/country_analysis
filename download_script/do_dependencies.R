library(here)

##### Where are we?
here::i_am("download_script/do_dependencies.R")
source(here("download_script","dependencies.R"))

# Get formulas
param_fname   <- here::here("assets", "_DB", "0_database_params.xlsx")
update_mode   <- 0L 
import_params <- readImportParams(param_fname = param_fname, update_mode = update_mode)
impplan  <- import_params$impplan
fillplan <- readFillParams(param_fname, sheet = "fill")

# Вынести в файл параметров?
formula_words <- c("lag", "lead", "rollsum", "rollavg", "rollvol", "mean", "last", "first", "min", "pmin", "max", "pmax", 
                   "sum", "coalesce", "share", "exp", "fromto", "year", "na_if", "cummax", "cummin", "cumsum", "ceiling",
                   "letterize", "if_else", "ifelse", "sqrt", "log", "abs", "rollapply")

# Dependency graph construction
dep <- build_dependency_graph(impplan = impplan, fillplan = fillplan, formula_words = formula_words)

# Drawing dependencies
plot_indicator_graph(dep, indicator_code = "policy_rate_av", frequency = "y", direction = "upstream", show_edge_formula = TRUE)
