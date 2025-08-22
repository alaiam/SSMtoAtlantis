#' setup_python_env : Create and configure a conda environment for the SSM to Atlantis translation
#'
#' This function sets up a Python conda environment for the SSM to Atlantis translation
#' developped in the SSMtoAtlantis package.
#' It adds the specified `conda_path` (path to bin the de anaconda or miniconda folder)
#' to the system `PATH`. Then, it checks if the conda environment `env_name`
#' already exists, and if not, creates it using a list of required packages from a file.
#' It also configures reticulate to use the new environment.
#'
#' @param env_name Character. Name of the conda environment to create or use. Default is `"Salish_sea_env"`.
#' @param conda_path Character. Full path to the `bin` directory in the anaconda or miniconda folder
#' used for your conda installation.
#' @returns Nothing. Creates python environment needed for stepA.
#' @export
#' @examples
#' setup_python_env(env_name = "Salish_sea_env",
#'   conda_path = "/home/atlantis/anaconda3/bin")

setup_python_env <- function(env_name = "Salish_sea_env",
                             conda_path) {
     Sys.setenv(PATH = paste(conda_path, Sys.getenv("PATH"), sep = ":"))
     envs <- stringr::word(system("conda env list", intern = TRUE))
  if (env_name %in% envs) {
    message(paste0("conda env '", env_name, "' already exist"))
  }else{
    # Create the conda env with the package defined in Salish_sea_env_explicit.txt
    message(paste0("Creation of the conda env '", env_name, "'"))
    cmd <- paste0(
      "conda create -n ",
      env_name,
      " --file " ,
      system.file("Salish_sea_env_explicit.txt", package = "SSMtoAtlantis"))
    status <- system(cmd)
    if (status != 0) {
      stop("Environment creation failed. Check your conda installation")
    }else{
      system(paste("conda install -y -n", env_name, "pyproj netCDF4 openpyxl"))
    }

    message("The env was successfully created")
    reticulate::use_condaenv(env_name, required = TRUE)
  }
}

