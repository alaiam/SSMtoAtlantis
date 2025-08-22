#' join_dailyfiles: Merge the files created in step B into one file
#'
#' @param year Year to process.
#' @param variable Variable name (e.g., "temperature", "NO3").
#' @param scenario Scenario name (e.g., "Hist", "RCP85").
#'
#' @returns Nothing. Side effects: saves merged file.
#' @export
#'
#' @examples
#' join_dailyfiles(2011, "temperature", "BAU")
join_dailyfiles <- function(year, variable, scenario) {

  # Global variables (used by sourced scripts)
  Nyear <<- year
  scenario <<- tolower(scenario)
  variable <<- variable

  # Define valid variables and their associated folders
  var_map <- data.frame(
    variable = c("salinity", "temperature",
                 "U", "V", "W",
                 "NO3", "NH4",
                 "SZ", "LZ", "MZ", "SP", "LP",
                 "O2", "LPON", "RPON", "RDON"),
    folder = c("TS", "TS",
               "uv", "uv", "uv",
               "N", "N",
               "Z", "Z", "Z", "B", "B",
               "O2", "PON", "PON", "DON"))

  # Validation
  if (!variable %in% var_map$variable) {
    stop("The variable is not in SSM. Please try one of:\n", paste(var_map$variable, collapse = ", "))
  }

  folder_name <- var_map$folder[var_map$variable == variable]

  output_path <- here::here(paste0("intermediate output archive/output_", scenario, "_", year, "_", folder_name))
  final_path <- here::here(paste0("Final outputs/", scenario, "/", year))

  message("Checking files for variable: ", variable, " | year: ", year, " | scenario: ", scenario)
  message("→ Output path: ", output_path)

  if (!dir.exists(final_path)) dir.create(final_path, recursive = TRUE)

  # Special case: U, V, W need a different folder check
  if (variable %in% c("U", "V", "W")) {
    output_path <- here::here(paste0("intermediate output archive/output_", scenario, "_", year, "_ww"))
  }

  # If not enough daily files, re-run StepB and recall
  if (length(list.files(output_path)) < 730) {
    message("Missing daily files (<730). Running StepB...")
    StepB(year, variable, scenario)
    return(join_dailyfiles(year, variable, scenario))  # recursive call
  }

  # Source appropriate script depending on variable
  message("All daily files found. Sourcing join script...")

  if (variable %in% c("salinity", "temperature")) {
    source("R/code/Step 4 - Join TS daily files.R")
  } else if (variable %in% c("U", "V", "W")) {
    source("R/code/Step 5 - Join_daily_files_uv.R")
  } else if (variable %in% c("NO3", "NH4")) {
    source("R/code/Step 9 - Join_daily_files_N.R")
  } else if (variable %in% c("SZ", "LZ", "MZ")) {
    source("R/code/Step 9 - Join_daily_files_Z.R")
  } else if (variable %in% c("SP", "LP")) {
    source("R/code/Step 9 - Join_daily_files_B.R")
  } else if (variable == "O2") {
    source("R/code/Step 11 - Join_daily_files_O2.R")
  } else if (variable %in% c("LPON", "RPON")) {
    source("R/code/Step 13 - Join_daily_files_PON.R")
  } else if (variable %in% c("RDON", "DON")) {
    source("R/code/Step 15 - Join_daily_files_DON.R")
  }
}
