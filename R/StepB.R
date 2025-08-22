#' StepB: From regular (ROMS like) grid to Atlantis intputs
#'
#' @param year The year of the SSM outputs (numeric or string).
#' @param variable The variable name to extract. Choice are:
#' salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, O2, LPON, RPON, RDON, PCB
#' @param scenario The name of the scenario used to produced SSM output
#' @param PCB_congener When variable = PCB, PCB_congener informs the PCB congener number.
#'
#'
#' @returns Nothing. Creates daily variable files.
#' @export
#' @importFrom ncdf4 nc_create ncvar_put ncvar_def ncatt_put nc_close
#'
#' @examples
#' StepB(2011, "temperature", "status_quo")
#'
StepB <- function(year, variable, scenario, PCB_congener = "0"){

  select <- dplyr::select
  map <- purrr::map
  options(dplyr.summarise.inform=FALSE)

  Nyear <<- year
  year <<- year
  scenario <<- scenario
  variable <<- variable
  PCB_congener <<- PCB_congener

  list.var = c("salinity","temperature",
               "U", "V", "W",
               "NO3", "NH4",
               "SZ", "LZ", "MZ", "SP", "LP",
               "O2","LPON","RPON",
               "RDON", "PCB")
  scenario <- tolower(scenario)
  path <- here::here("Atlantis_daily_files", scenario, year, variable)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, O2, LPON, RPON, RDON, PCB.
For a pdf sum up, try all")

  if(variable== "PCB" & PCB_congener == "0") stop("PCB_congener needs to be between 118, 138 or 153")


  if (variable == "salinity"|| variable =="temperature")   {source(
    system.file("code/Step 1 - SSM-ROMS_to_Atlantis_T_S.R", package = "SSMtoAtlantis"))}

  if (variable == "U"|| variable =="V"|| variable =="W")   {source(
    system.file("code/Step 2 - SSM-ROMS_to_Atlantis_w.R", package = "SSMtoAtlantis"))
                                                            source(
    system.file("code/Step 3 - SSM-ROMS_to_Atlantis_uv.R", package = "SSMtoAtlantis"))}

  if (variable == "SP"|| variable =="LP")                  {source(
    system.file("code/Step 6 - SSM-ROMS_to_Atlantis_B.R", package = "SSMtoAtlantis"))}

  if (variable == "NO3"|| variable =="NH4")                {source(
    system.file("code/Step 7 - SSM-ROMS_to_Atlantis_N.R", package = "SSMtoAtlantis"))}

  if (variable == "SZ"|| variable =="LZ"|| variable =="MZ"){source(
    system.file("code/Step 8 - SSM-ROMS_to_Atlantis_Z.R", package = "SSMtoAtlantis"))}

  if (variable == "O2")                                   {source(
    system.file("code/Step 10 - SSM-ROMS_to_Atlantis_O2.R", package = "SSMtoAtlantis"))}

  if (variable == "LPON"|| variable =="RPON")              {source(
    system.file("code/Step 12 - SSM-ROMS_to_Atlantis_PON.R", package = "SSMtoAtlantis"))}

  if (variable == "RDON"|| variable =="DON")               {source(
    system.file("code/Step 14 - SSM-ROMS_to_Atlantis_DON.R", package = "SSMtoAtlantis"))}

  if (variable == "PCB")                                   {source(
    system.file("code/Step 16 - SSM-ROMS_to_Atlantis_PCB_B.R", package = "SSMtoAtlantis"))
                                                            source(
    system.file("code/Step 18 - SSM-ROMS_to_Atlantis_PCB_WC.R", package = "SSMtoAtlantis"))}
}




