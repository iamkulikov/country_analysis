# Append OECD CIT + WoRLD rows to 0_database_params.xlsx (import sheet).
suppressPackageStartupMessages({
  library(openxlsx)
  library(readxl)
})

param_path <- "assets/_DB/0_database_params.xlsx"
staging_path <- "assets/_DB/_0_database_params_oecd_world.xlsx"

file.copy(param_path, staging_path, overwrite = TRUE)
wb <- loadWorkbook(staging_path)
imp <- read_excel(staging_path, sheet = "import", skip = 1)

existing_codes <- imp$indicator_code
new_rows <- list(
  data.frame(
    indicator = "Combined statutory CIT, %",
    theme = "budget: revenue",
    indicator_code = "oecd_cit_combined",
    source_frequency = "y",
    retrieve_type = "API",
    source_name = "OECD",
    database_name = "CTS",
    file_name = "oecd_cts_cit.csv",
    sheet_name = NA_character_,
    retrieve_code = "COMBINED",
    active = 1,
    keep = 1,
    update = 0,
    comment = "OECD Corporate Tax Statistics; combined central+sub-central statutory rate",
    stringsAsFactors = FALSE
  )
)

world_specs <- list(
  c("Total revenue, % GDP (WoRLD)", "world_totrev", "TotRev"),
  c("Tax revenue, % GDP (WoRLD)", "world_taxrev", "TaxRev"),
  c("Taxes on income, % GDP (WoRLD)", "world_taxinc", "TaxInc"),
  c("Taxes on income: individuals, % GDP (WoRLD)", "world_taxinci", "TaxIncI"),
  c("Taxes on income: corporations, % GDP (WoRLD)", "world_taxincc", "TaxIncC"),
  c("Property taxes, % GDP (WoRLD)", "world_taxpro", "TaxPro"),
  c("Taxes on goods and services, % GDP (WoRLD)", "world_taxsal", "TaxSal"),
  c("General sales/VAT, % GDP (WoRLD)", "world_taxsalg", "TaxSalG"),
  c("Taxes on international trade, % GDP (WoRLD)", "world_taxtra", "TaxTra"),
  c("Social contributions, % GDP (WoRLD)", "world_socialcon", "SocialCon"),
  c("Grants, % GDP (WoRLD)", "world_grants", "Grants"),
  c("Other revenue, % GDP (WoRLD)", "world_revoth", "RevOth"),
  c("Non-tax resource revenue, % GDP (WoRLD)", "world_nontaxres", "NonTaxRes")
)

for (spec in world_specs) {
  new_rows[[length(new_rows) + 1]] <- data.frame(
    indicator = spec[[1]],
    theme = "budget: revenue",
    indicator_code = spec[[2]],
    source_frequency = "y",
    retrieve_type = "API",
    source_name = "IMF",
    database_name = "IMF.FAD/WORLD",
    file_name = NA_character_,
    sheet_name = NA_character_,
    retrieve_code = spec[[3]],
    active = 1,
    keep = 1,
    update = 0,
    comment = "IMF WoRLD revenue structure (% of GDP)",
    stringsAsFactors = FALSE
  )
}

to_add <- do.call(rbind, new_rows)
dup <- to_add$indicator_code[to_add$indicator_code %in% existing_codes]
if (length(dup) > 0) {
  stop("Indicator codes already exist: ", paste(dup, collapse = ", "))
}

start_row <- nrow(imp) + 3L
for (i in seq_len(nrow(to_add))) {
  writeData(
    wb,
    sheet = "import",
    x = as.data.frame(lapply(to_add[i, ], function(x) if (is.na(x)) "" else x), stringsAsFactors = FALSE),
    startRow = start_row + i - 1L,
    colNames = FALSE
  )
}

saveWorkbook(wb, staging_path, overwrite = TRUE)

promoted <- FALSE
if (file.exists(param_path)) {
  promoted <- tryCatch({
    file.remove(param_path)
    file.copy(staging_path, param_path, overwrite = TRUE)
  }, error = function(e) FALSE)
  if (isTRUE(promoted)) {
    file.remove(staging_path)
  }
} else {
  file.copy(staging_path, param_path, overwrite = TRUE)
  promoted <- TRUE
}

if (!isTRUE(promoted)) {
  message(
    "Could not overwrite ", param_path,
    " (file may be open). Staged workbook: ", staging_path,
    ". Close Excel and run: file.copy('", staging_path, "', '", param_path, "', overwrite=TRUE)"
  )
} else {
  cat("Added", nrow(to_add), "rows to", param_path, "starting at row", start_row, "\n")
}
