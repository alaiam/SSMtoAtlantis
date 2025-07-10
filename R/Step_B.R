

select <- dplyr::select
map <- purrr::map
options(dplyr.summarise.inform=FALSE)


#' Preprocessing Check and Environment Initialization
#'
#' Before using the main functions of the SSMtoAtlantis package, this helper function:
#' - Ensures that the Python environment `Salish_Sea_env` is activated (via R script)
#' - Verifies that key preprocessing files are present
#' - Guides the user to run the necessary equivalence functions if any files are missing
#'
#' @return No return value. Messages are printed to the console.
#' @export
preprocess <- function() {
  message("Before using the functions `SSMtoAtlantis`, `var.SSMtoAtlantis`, `Step A`, or `Step B`, make sure that:")
  message("1) You have created and activated your Python environment `Salish_Sea_env`.")
  message("2) You have run the preprocessing steps for Step B, which generate the following files:")
  message("     - `box_composition_ww.csv`")
  message("     - `face_composition_uv.csv`")
  message("     - `box_composition.csv`")
  message("     - `layer_max_ww.csv`")
  message("     - `uxy2_uv_code`")

  if (file.exists("python/Start_Salish_Sea_env.R")) {
    source("python/Start_Salish_Sea_env.R")
    message("✓ Python environment `Salish_Sea_env` started.")
  } else {
    warning("⚠ File 'Start_Salish_Sea_env.R' not found in /python/")
  }

  if (file.exists("R/code/box_composition_ww.csv")) {
    message("✓ `box_composition_ww.csv` found.")
  } else {
    message("→ Missing: Run `face_horizontal_equivalence()`.")
  }

  if (file.exists("R/code/box_composition.csv")) {
    message("✓ `box_composition.csv` found.")
  } else {
    message("→ Missing: Run `box_equivalence()`.")
  }

  if (file.exists("R/code/face_composition_uv.csv")) {
    message("✓ `face_composition_uv.csv` found.")
  } else {
    message("→ Missing: Run `face_vertical_equivalence()`.")
  }

  if (file.exists("R/code/layer_max_ww.csv")) {
    message("✓ `layer_max_ww.csv` found.")
  } else {
    message("→ Missing: `layer_max_ww.csv`. Run `face_horizontal_equivalence()`.")
  }

  if (file.exists("R/code/uxy_uv_code.csv")) {
    message("✓ `uxy_uv_code.csv` found.")
  } else {
    message("→ Missing: `uxy2_uv_code.csv` must be generated before Step B.")
  }
}




SSMtoAtlantis <- function(year, velma, filename.hyd, filename.wq){
  .year <<- year
  .velma <<- velma
  .filename.hyd <<- filename.hyd
  .filename.wq <<- filename.wq

  start.time <- Sys.time()
  write.csv(start.time, "starttime.csv")
  step.time <- c()
   # hyd
  print("TS")
  var.SSMtoAtlantis(year = .year, variable = "temperature", velma = .velma, filename = .filename.hyd)
  print("UVW")
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  var.SSMtoAtlantis(year = .year, variable = "U", velma = .velma, filename = .filename.hyd)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  #wq
  print("Z")
  var.SSMtoAtlantis(year = .year, variable = "SZ", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("P")
  var.SSMtoAtlantis(year = .year, variable = "SP", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("N")
  var.SSMtoAtlantis(year = .year, variable = "NO3", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("DON")
  var.SSMtoAtlantis(year = .year, variable = "RDON", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("O2")
  var.SSMtoAtlantis(year = .year, variable = "Oxygen", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  print("PON")
  var.SSMtoAtlantis(year = .year, variable = "LPON", velma = .velma, filename = .filename.wq)
  step.time <- c(step.time,Sys.time())
  write.csv(step.time, "steptime.csv")

  end.time <- Sys.time()
  write.csv(end.time, "endtime.csv")

}

var.SSMtoAtlantis <- function(year, variable, velma, filename){

  list.var = c("salinity","temperature",
               "U", "V", "W",
               "NO3", "NH4",
               "SZ", "LZ", "MZ", "SP", "LP",
               "Oxygen","LPON","RPON",
               "RDON")

  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON")
  print("var1")
  StepA(year = year, variable = variable, velma = velma, filename = filename)
  print("var2")
  StepB(year = year, variable = variable, velma = velma)
  print("var3")
  Joindailyfile(year = year, variable = variable, velma = velma)

}


# Step B
StepB <- function(year, variable, velma){
  Nyear <<- year
  velma <<- velma
  variable <<- variable
  list.var = c("salinity","temperature",
               "U", "V", "W",
               "NO3", "NH4",
               "SZ", "LZ", "MZ", "SP", "LP",
               "Oxygen","LPON","RPON",
               "RDON")

  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON.
For a pdf sum up, try all ")


  if (variable == "salinity"|| variable =="temperature")   {source("Workflow/Step B/code/Step 1 - SSM-ROMS_to_Atlantis_T_S.R")}
  if (variable == "U"|| variable =="V"|| variable =="W")   {source("Workflow/Step B/code/Step 2 - SSM-ROMS_to_Atlantis_w.R")
                                                            source("Workflow/Step B/code/Step 3 - SSM-ROMS_to_Atlantis_uv.R")}
  if (variable == "SP"|| variable =="LP")                  {source("Workflow/Step B/code/Step 6 - SSM-ROMS_to_Atlantis_B.R")}
  if (variable == "NO3"|| variable =="NH4")                {source("Workflow/Step B/code/Step 7 - SSM-ROMS_to_Atlantis_N.R")}
  if (variable == "SZ"|| variable =="LZ"|| variable =="MZ"){source("Workflow/Step B/code/Step 8 - SSM-ROMS_to_Atlantis_Z.R")}
  if (variable == "Oxygen")                                {source("Workflow/Step B/code/Step 10 - SSM-ROMS_to_Atlantis_O2.R")}
  if (variable == "LPON"|| variable =="RPON")              {source("Workflow/Step B/code/Step 12 - SSM-ROMS_to_Atlantis_PON.R")}
  if (variable == "RDON"|| variable =="DON")               {source("Workflow/Step B/code/Step 14 - SSM-ROMS_to_Atlantis_DON.R")}


}
# Join daily files
Joindailyfile <- function(year, variable, velma){

  Nyear <<- year
  velma <<- velma
  variable <<- variable

  list.var = c("salinity","temperature",
               "U", "V", "W",
               "NO3", "NH4",
               "SZ", "LZ", "MZ", "SP", "LP",
               "Oxygen","LPON","RPON",
               "RDON")


  if(!variable%in%list.var) stop("The variable is not in SSM, please try:
salinity, temperature, U, V, W, NO3, NH4, SZ, LZ, MZ, SP, LP, Oxygen, LPON, RPON, RDON")

  list.folder.names = c("TS","TS",
                        "uv", "uv", "uv",
                        "N", "N",
                        "Z", "Z", "Z", "B", "B",
                        "O2","PON","PON",
                        "DON")

  pos <- match(variable, list.var)
  if (velma){
    output_path = paste0("Workflow/Step B/intermediate output archive/output_VELMA_",year, "_", list.folder.names[pos])
    final_path = paste0("Workflow/Step B/Final outputs/VELMA/",year)
  }else{
    output_path = paste0("Workflow/Step B/intermediate output archive/output_No_VELMA_",year, "_", list.folder.names[pos])
    final_path = paste0("Workflow/Step B/Final outputs/No_VELMA/",year)

  }
  print(output_path)
  if (!file.exists(final_path)){dir.create(final_path)}


  if (length(list.files(output_path))<730){
    print("toto")
    StepB(year, variable, velma)
    Joindailyfile(year, variable, velma)
  }else{
    if (variable == "salinity"|| variable =="temperature"){
      source("Workflow/Step B/code/Step 4 - Join TS daily files.R")
    }
    if (variable == "U"|| variable =="V"|| variable =="W"){
      output_path = paste0("Workflow/Step B/intermediate output archive/output_VELMA_",year, "_ww")
      if (length(list.files(output_path))<730){
        print("totou")
        StepB(year, variable, velma)
        Joindailyfile(year, variable, velma)
      }else{
      source("Workflow/Step B/code/Step 5 - Join_daily_files_uv.R")}
    }
    if (variable == "NO3"|| variable =="NH4"){
      print("oy")
      source("Workflow/Step B/code/Step 9 - Join_daily_files_N.R")
    }
    if (variable == "SZ"|| variable =="LZ"|| variable =="MZ"){
      source("Workflow/Step B/code/Step 9 - Join_daily_files_Z.R")
    }
    if (variable == "SP"|| variable =="LP"){
      source("Workflow/Step B/code/Step 9 - Join_daily_files_B.R")
    }
    if (variable == "Oxygen"){
      source("Workflow/Step B/code/Step 11 - Join_daily_files_O2.R")
    }
    if (variable == "LPON"|| variable =="RPON"){
      source("Workflow/Step B/code/Step 13 - Join_daily_files_PON.R")
    }
    if (variable == "RDON"|| variable =="DON"){
      source("Workflow/Step B/code/Step 15 - Join_daily_files_DON.R")
    }
    }}
#test


