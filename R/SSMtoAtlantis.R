#' SSMtoAtlantis: Function that creates Atlantis inputs from SSM outputs (for all
#' the variables and in the correct Atlantis grid). It expect yearly files.
#'
#' @param year The year of the SSM outputs (numeric or string).
#' @param scenario The name of the scenario used to produced SSM output
#' @param filename.hyd The input filename of the file with physical variables.
#' It is a NetCDF file which include SSM outputs on FVCOM grid (variables are be
#' currents (U,V and W) temperature or salinity).
#' @param filename.wq The input filename of the file with water quality variables.
#' It is a NetCDF file which include SSM outputs on FVCOM grid (variables are be
#' nutrients (NO3, NH4), phytoplankton (B1, B2), zooplankton (SZ, LZ)
#' organic matters (LPON, LDON, RPON, RDON)))
#'
#' @returns
#' @export
#'
#' @examples
SSMtoAtlantis <- function(year, scenario, filename.hyd, filename.wq){
  .year <<- year
  .scenario <<- scenario
  .filename.hyd <<- filename.hyd
  .filename.wq <<- filename.wq

  start.time <- Sys.time()
  write.csv(start.time, "starttime.csv")
  step.time <- c()
  # hyd
  print("TS")
  var.SSMtoAtlantis(year = .year, variable = "temperature", scenario = .scenario, filename = .filename.hyd)
  print("UVW")
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  var.SSMtoAtlantis(year = .year, variable = "U", scenario = .scenario, filename = .filename.hyd)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  #wq
  print("Z")
  var.SSMtoAtlantis(year = .year, variable = "SZ", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("P")
  var.SSMtoAtlantis(year = .year, variable = "SP", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("N")
  var.SSMtoAtlantis(year = .year, variable = "NO3", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("DON")
  var.SSMtoAtlantis(year = .year, variable = "RDON", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("O2")
  var.SSMtoAtlantis(year = .year, variable = "Oxygen", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("PON")
  var.SSMtoAtlantis(year = .year, variable = "LPON", scenario = .scenario, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  end.time <- Sys.time()
  write.csv(end.time, "endtime.csv")
}
