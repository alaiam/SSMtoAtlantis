
#' StepA: From FVCOM grid to regular (ROMS like) grid for SSM outputs
#'
#' @param year The year of the SSM outputs (numeric or string).
#' @param variable The variable name to extract.
#' @param scenario The name of the scenario used to produced SSM output
#' @param filename The input filename, a NetCDF file which include SSM outputs on FVCOM grid.
#'
#' @return Path to the generated NetCDF file with SSM regular output for variable (invisible).
#' @export
#'
#' @examples
#' year = 2095
#' variable = "temperature"
#' scenario = "BAU"
#' filename = "path/to/SSM_FVCOM_grid_file"
#' StepA(year, variable, scenario, filename)
StepA <- function(year, variable, scenario, filename) {
  list.var <- c("salinity","temperature", "U", "V", "W", "NO3", "NH4",
                "SZ", "LZ", "MZ", "SP", "LP", "Oxygen", "LPON", "RPON", "RDON")
  if (!variable %in% list.var) {
    stop("The variable is not in SSM, please try: ",
         paste(list.var, collapse = ", "))
  }

  scenario <- tolower(scenario)
  path <- here::here("Atlantis_inputs", scenario, year)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  script_map <- list(
    TS = c("salinity", "temperature"),
    UVW = c("U", "V", "W"),
    N = c("NO3", "NH4"),
    Z = c("SZ", "LZ", "MZ"),
    B = c("SP", "LP"),
    Oxygen = "Oxygen",
    PON = c("LPON", "RPON"),
    DON = "RDON"
  )

  group <- names(script_map)[sapply(script_map, function(vars) variable %in% vars)]
  if (length(group) == 0) stop("Invalid variable group.")

  suffix <- group
  script <- paste0("python/StepA_", suffix, ".py")
  file_output <- file.path(path, paste0("regular_grid_", suffix, "_", scenario, "_", year, ".nc"))

  filename <- r_to_py(filename)
  file_name_output <- r_to_py(file_output)

  print(filename)
  print(file_output)

  py_run_file(script)

  invisible(file_output)
}
