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

  output_path <- here::here("Atlantis_daily_files", scenario, year, variable)
  final_path <- here::here("Atlantis_inputs", scenario, year)

  message("Checking files for variable: ", variable, " | year: ", year, " | scenario: ", scenario)
  message("→ Output path: ", output_path)

  if (!dir.exists(final_path)) dir.create(final_path, recursive = TRUE)

  # If not enough daily files, re-run StepB and recall
  if (length(list.files(output_path)) < 730) {
    message("Missing daily files (<730). Running StepB...")
    StepB(year, variable, scenario)
    return(join_dailyfiles(year, variable, scenario))  # recursive call
  }

  # Source appropriate script depending on variable
  message("All daily files found. Sourcing join script...")

  if (variable %in% c("salinity", "temperature")) {
    source(system.file("code/Step 4 - Join TS daily files.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("U", "V", "W")) {
    source(system.file("code/Step 5 - Join_daily_files_uv.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("NO3", "NH4")) {
    source(system.file("code/Step 9 - Join_daily_files_N.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("SZ", "LZ", "MZ")) {
    source(system.file("code/Step 9 - Join_daily_files_Z.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("SP", "LP")) {
    source(system.file("code/Step 9 - Join_daily_files_B.R", package = "SSMtoAtlantis"))
  } else if (variable == "O2") {
    source(system.file("code/Step 11 - Join_daily_files_O2.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("LPON", "RPON")) {
    source(system.file("code/Step 13 - Join_daily_files_PON.R", package = "SSMtoAtlantis"))
  } else if (variable %in% c("RDON", "DON")) {
    source(system.file("code/Step 15 - Join_daily_files_DON.R", package = "SSMtoAtlantis"))
  } else if (variable == "PCB") {
    source(system.file("code/Step 17 - Join_daily_files_PCB_B.R", package = "SSMtoAtlantis"))
    source(system.file("code/Step 19 - Join_daily_files_PCB.R", package = "SSMtoAtlantis"))
  }
}
