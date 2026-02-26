

#' write_interpoled_data
#'
#' Write 3D array after interpolation between two or more SSM year into
#' an Atlantis input NetCDF file. In a case where variable = uvw, it write a
#' list of 3 3D array: exchange, dest_b, dest_k
#'
#' This function writes an interpolated 3D array (z × box × time)
#' into a NetCDF file formatted for the Atlantis model forcing files.
#' The output file is created in the `Atlantis_inputs/<scenario>/interannual`
#' directory and follows the naming convention used for the
#' Puget Sound Atlantis configuration.
#'
#' The variable name, long name, and units are automatically mapped
#' to the corresponding Atlantis conventions.
#'
#' @param data A 3D numeric array with dimensions (or a list of 3 D numeric array with dimensions for uvw)
#' `layer × box × time`, containing interpolated values
#' for the selected variable.
#' @param start_year Numeric. First year of the forcing period.
#' Used to define the NetCDF time origin.
#' @param end_year Numeric. Last year of the forcing period.
#' Used in the output file name.
#' @param variable Character string. Name of the variable to write.
#' Must be one of:
#' `"salinity"`, `"temperature"`, `"U"`, `"V"`, `"W"`,
#' `"NO3"`, `"NH4"`, `"SZ"`, `"LZ"`, `"MZ"`, `"SP"`, `"LP"`,
#' `"O2"`, `"LPON"`, `"RPON"`, `"RDON"`.
#' @param scenario Character string. Scenario name used to define
#' the output directory structure.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Creates vertical (`z`), box (`b`), and time (`t`) dimensions
#'   compatible with Atlantis.
#'   \item Assigns variable metadata (long name, units) based on
#'   internal correspondence with Atlantis variable names.
#'   \item Writes valid minimum and maximum attributes.
#'   \item Adds standard global attributes required for Atlantis forcing.
#' }
#'
#' The time dimension is defined in seconds since
#' `start_year-01-01`, with a forced default time step (`dt`)
#' of 43200 seconds (12 hours).
#'
#' @return
#' Nothing returned. .
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with a dummy array
#' dummy_array <- data(runif(10 * 5 * 20),
#'                      dim = c(10, 5, 20))
#'
#' write_interpoled_data(
#'   data = dummy_array,
#'   start_year = 2011,
#'   end_year = 2045,
#'   variable = "temperature",
#'   scenario = "status_quo"
#' )
#' }

write_interpoled_data <- function(data, start_year, end_year, variable, scenario){

  var_map <- data.frame(
    variable = c("salinity", "temperature",
                 "uvw",
                 "NO3", "NH4",
                 "SZ", "LZ", "MZ", "SP", "LP",
                 "oxygen", "LPON", "RPON", "RDON"),
    atlantis.name = c("salinity", "temperature",
                      "uvw",
                      "NO3", "NH3",
                      "Sm_Zooplankton_N", "Lrg_Zoo_N", "Meso_Zoo_N", "Sm_Phyto_N", "Lrg_Phyto_N",
                      "Oxygen", "Lab_Det_N", "Ref_Det_N", "RDON"),

    atlantis.longname = c("Salinity", "Temperature",
                          "uvw",
                          "NO3", "NH3",
                          "SmallZooplankton", "LargeZooplankton", "MesoZooplankton", "Small phytoplankton (dinoflagellates)", "Large phytoplankton (diatoms)",
                          "Dissolved oxygen", "Labile particulate organic nitrogen", "Refractory particulate organic nitrogen", "Refractory dissolved organic nitrogen"),
    atlantis.unit = c("g.L-1", "°C",
                          "m^3",
                          "mg N m-3", "mg N m-3",
                          "mg N m-3", "mg N m-3", "mg N m-3", "mg N m-3", "mg N m-3",
                          "mg O2 m-3", "mg N m-3", "mg N m-3", "mg N m-3"))

  # Validation
  if (!variable %in% var_map$variable) {
    stop("The variable is not in SSM. Please try one of:\n", paste(var_map$variable, collapse = ", "))
  }

  folder <- here::here("Atlantis_inputs", scenario, "interannual")
  if(!dir.exists(folder)){dir.create(folder, recursive = T)}
  nc_filename <- paste0(folder, "/pugetsound_SSM_Atlantis_", variable, "_", scenario, "_", start_year,"_", end_year,".nc")

  ###################################################################################
  # Variable file
  ###################################################################################


  if (variable %in% c("uvw")){
    time = time = seq(0,dim(data$exchange)[4]*12*60*60-1, 12*60*60)

    # Define dimensions
    z_dim <- ncdf4::ncdim_def("z","layerNum", 1:dim(data$exchange)[1]) # 7 layers
    b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(dim(data$exchange)[3]-1)) # 89 polygons
    t_dim <- ncdf4::ncdim_def("t",units = paste0("seconds since ",start_year,"-01-01"), time, unlim = T)
    dest_dim <- ncdf4::ncdim_def("dest", "Nb max of destinaions", 1:dim(data$exchange)[2])

    # Define variables
    z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
    b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
    t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = paste0("seconds since ",start_year,"-01-01"), longname = "t")
    dest_var <- ncdf4::ncvar_def("dest", "int", dim = list(dest_dim), units = "dest", longname = "dest")


    exchange_var = ncdf4::ncvar_def("exchange", prec = "double", dim = list(dest_dim, z_dim,b_dim,t_dim),
                             units = "m^3", missval = 0, longname = "Change in volume in this time step")
    dest_b_var =  ncdf4::ncvar_def("dest_b", prec = "integer", dim = list(dest_dim, z_dim,b_dim,t_dim),
                            missval = -1, longname = "", units = "#")
    dest_k_var =  ncdf4::ncvar_def("dest_k", prec = "integer", dim = list(dest_dim, z_dim,b_dim,t_dim),
                            missval = -1, longname = "", units = "#")


    # Create a NetCDF file
    nc <- ncdf4::nc_create(nc_filename, vars = list(exchange = exchange_var, dest_b = dest_b_var, dest_k = dest_k_var))

    # Put dimensions and variables in the NetCDF file

    ncdf4::ncvar_put(nc, z_var, 1:(dim(data$exchange)[1]))
    ncdf4::ncvar_put(nc, b_var, 0:(dim(data$exchange)[3]-1))
    ncdf4::ncvar_put(nc, exchange_var, data$exchange, start = c(1,1,1,1), count = c(dim(data$exchange)[2], dim(data$exchange)[1],dim(data$exchange)[3], dim(data$exchange)[4]))
    ncdf4::ncvar_put(nc, dest_b_var,   data$dest_b, start   = c(1,1,1,1), count = c(dim(data$exchange)[2], dim(data$exchange)[1],dim(data$exchange)[3], dim(data$exchange)[4]))
    ncdf4::ncvar_put(nc, dest_k_var,   data$dest_k, start   = c(1,1,1,1), count = c(dim(data$exchange)[2],dim(data$exchange)[1],dim(data$exchange)[3], dim(data$exchange)[4]))

    # Add dt attribute to t variable
    ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

    # Global attributes
    ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
    ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
    ncdf4::ncatt_put(nc, 0, "parameters", "")

    # Close the NetCDF file
    ncdf4::nc_close(nc)

  }else{
    time = time = seq(0,dim(data)[3]*12*60*60-1, 12*60*60)

  # Define dimensions
  z_dim <- ncdf4::ncdim_def("z","layerNum", 1:dim(data)[1]) # 7 layers
  b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(dim(data)[2]-1)) # 89 polygons
  t_dim <- ncdf4::ncdim_def("t",units = paste0("seconds since ",start_year,"-01-01"), time, unlim = T)


  # Define variables
  z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
  b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units =  paste0("seconds since ",start_year,"-01-01"), longname = "t")
  variable.Atlantis <- ncdf4::ncvar_def(var_map$atlantis.name[var_map$variable==variable], "double", dim = list( z_dim,b_dim, t_dim),
                           units = var_map$atlantis.unit[var_map$variable==variable], missval = 0, longname = var_map$atlantis.longname[var_map$variable==variable])

  # Create a NetCDF file
  nc <- ncdf4::nc_create(nc_filename, vars = list(variable.Atlantis = variable.Atlantis))

  # Put dimensions and variables in the NetCDF file

  ncdf4::ncvar_put(nc, z_var, 1:dim(data)[1])
  ncdf4::ncvar_put(nc, b_var, 0:(dim(data)[2]-1))
  ncdf4::ncvar_put(nc, t_var, time)
  ncdf4::ncvar_put(nc, variable.Atlantis, data, start = c(1,1,1),count = c(dim(data)[1],dim(data)[2], length(time)))

  # Add minimum and maximum values to variable.Atlantis variable attributes
  ncdf4::ncatt_put(nc, var_map$atlantis.name[var_map$variable==variable], "valid_min", -50)
  ncdf4::ncatt_put(nc, var_map$atlantis.name[var_map$variable==variable], "valid_max", 1e12)

  # Add dt attribute to t variable
  ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

  # Global attributes
  ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

  # Close the NetCDF file
  ncdf4::nc_close(nc)
  }
}
