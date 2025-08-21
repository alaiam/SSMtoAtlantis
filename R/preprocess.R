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
    warning("⚠ File 'Start_Salish_Sea_env.R' not found in python/")
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

