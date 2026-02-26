
###########################################################################
# Path and names definition

input_path <- here::here("File_regular_grid", scenario, year)
filename <- paste0("/regular_grid_POM_sed_", scenario , "_",year, ".nc")
output_path <- here::here("Atlantis_daily_files", scenario, year, variable)

###########################################################################
# Read data ROMS data
roms <- tidync::tidync(paste0(input_path,filename))
box_composition <- utils::read.csv(system.file("code/box_composition.csv", package = "SSMtoAtlantis"))
box_composition <- box_composition[box_composition$roms_layer==1,c(1,11,12)]

###########################################################################

# get list of ROMS variables
roms_vars <- tidync::hyper_grids(roms) %>% # all available grids in the ROMS ncdf
  purrr::pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
      dplyr::mutate(grd=x)
  })

############################################################################################
############################################################################################
############################################################################################
step_file <- 1:730 #Days to divide the total files

files <- sub("PON_sed_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

PON_dim <- roms_vars %>% dplyr::filter(name==c("PON")) %>% purrr::pluck('grd')


variable_before_Atlantis2 <- roms %>%
  tidync::activate(PON_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(PON, longitude, latitude,time)%>%
  dplyr::rename(
    PON=PON,
    longitude = longitude,
    latitude = latitude, time = time)



gc() #free unused memory before parallelization
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

foreach::foreach(days = step_file,
                 .packages = c("dplyr","tidync","ncdf4")) %dopar%{


  variable_before_Atlantis<- variable_before_Atlantis2 %>% dplyr::filter(time== days)
  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude"))

  ###################################################################
  time = as.numeric(sort(unique(variables_polygons$time)))
  box = 89
  atlantis_input_PON <- array(rep(NA,box*length(time)), dim = c(box,length(time)))

  for (i in 0:(box-1)){
    for (t in 1:length(time)){
        subset <-variables_polygons %>%
          filter(.bx0 == i, time == time[t])


          atlantis_input_PON[i+1,t] <- (mean(subset$PON, na.rm = T)*1000)[[1]] #gN meters-3 to mgN meters-3
    }
  }

  ###################################################################################
  # Define nc file
  ###################################################################################
  # Define dimensions
  b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
  t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", (time-1)*60*60)
  # Define variables
  b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
  PON <- ncdf4::ncvar_def("PON", "double", dim = list(b_dim, t_dim),
                   units = "mg.m-3", missval = NA, longname = "LPON")
  output_filename = paste0("/PON_sed_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- ncdf4::nc_create(nc_filename, vars = list(PON = PON))

  # Put dimensions and variables in the NetCDF file

  ncdf4::ncvar_put(nc, b_var, 0:(box-1))
  ncdf4::ncvar_put(nc, t_var, (time-1)*60*60)
  ncdf4::ncvar_put(nc, PON, atlantis_input_PON, start = c(1,1),count = c(box, length(time)))

  # Add minimum and maximum values to LPON variable attributes
  ncdf4::ncatt_put(nc, "PON", "valid_min", 0)
  ncdf4::ncatt_put(nc, "PON", "valid_max", 2000000)

  # Add dt attribute to t variable
  ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

  # Global attributes
  ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
  ncdf4::ncatt_put(nc, 0, "parameters", "")

  # Close the NetCDF file
  ncdf4::nc_close(nc)

}
parallel::stopCluster(cl)
foreach::registerDoSEQ()
gc()

