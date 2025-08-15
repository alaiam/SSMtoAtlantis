#' StepB: From regular (ROMS like) grid to Atlantis intputs
#'
#' @param year The year of the SSM outputs (numeric or string).
#' @param variable The variable name to extract. Choice are:
#' salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON, PCB
#' @param scenario The name of the scenario used to produced SSM output
#' @param PCB_congener When variable = PCB, PCB_congener informs the PCB congener number.
#'
#'
#' @returns Nothing. Creates daily variable files.
#' @export
#'
#' @examples
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
               "Oxygen","LPON","RPON",
               "RDON", "PCB")
  scenario <- tolower(scenario)
  path <- here::here("Atlantis_daily_files", scenario, year, variable)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON, PCB.
For a pdf sum up, try all")

  if(!variable== "PCB" & PCB_congener == "0") stop("PCB_congener needs to be between 118, 138 or 153")


  if (variable == "salinity"|| variable =="temperature")   {source("Workflow/Step B/code/Step 1 - SSM-ROMS_to_Atlantis_T_S.R")}
  if (variable == "U"|| variable =="V"|| variable =="W")   {source("Workflow/Step B/code/Step 2 - SSM-ROMS_to_Atlantis_w.R")
                                                            source("Workflow/Step B/code/Step 3 - SSM-ROMS_to_Atlantis_uv.R")}
  if (variable == "SP"|| variable =="LP")                  {source("Workflow/Step B/code/Step 6 - SSM-ROMS_to_Atlantis_B.R")}
  if (variable == "NO3"|| variable =="NH4")                {source("Workflow/Step B/code/Step 7 - SSM-ROMS_to_Atlantis_N.R")}
  if (variable == "SZ"|| variable =="LZ"|| variable =="MZ"){source("Workflow/Step B/code/Step 8 - SSM-ROMS_to_Atlantis_Z.R")}
  if (variable == "Oxygen")                                {source("Workflow/Step B/code/Step 10 - SSM-ROMS_to_Atlantis_O2.R")}
  if (variable == "LPON"|| variable =="RPON")              {source("Workflow/Step B/code/Step 12 - SSM-ROMS_to_Atlantis_PON.R")}
  if (variable == "RDON"|| variable =="DON")               {source("Workflow/Step B/code/Step 14 - SSM-ROMS_to_Atlantis_DON.R")}
  if (variable == "PCB")                                   {source("Workflow/Step B/code/Step 16 - SSM-ROMS_to_Atlantis_PCB_B.R")
                                                            source("Workflow/Step B/code/Step 18 - SSM-ROMS_to_Atlantis_PCB_WC.R")}
}




