#' Title
#'
#' @param year The year of the SSM outputs (numeric or string).
#' @param variable The variable name to extract.
#' @param scenario The name of the scenario used to produced SSM output
#' @param filename The input filename, a NetCDF file which include SSM outputs on FVCOM grid.
#'
#' @returns
#' @export
#'
#' @examples
var.SSMtoAtlantis <- function(year, variable, scenario, filename){

  list.var = c("salinity","temperature",
               "U", "V", "W",
               "NO3", "NH4",
               "SZ", "LZ", "MZ", "SP", "LP",
               "Oxygen","LPON","RPON",
               "RDON")

  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON")
  print("var1")
  StepA(year = year, variable = variable, scenario = scenario, filename = filename)
  print("var2")
  StepB(year = year, variable = variable, scenario = scenario)
  print("var3")
  Joindailyfile(year = year, variable = variable, scenario = scenario)

}
