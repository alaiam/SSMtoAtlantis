
###########################################################################
# Path and names definition

input_path <- here::here("File_regular_grid", scenario, year)
filename <- paste0("/regular_grid_N_", scenario , "_",year, ".nc")
output_path <- here::here("Atlantis_daily_files", scenario, year, variable)

###########################################################################
# Read data ROMS data
roms <- tidync::tidync(paste0(input_path,filename))
box_composition <- utils::read.csv(system.file("code/box_composition.csv", package = "SSMtoAtlantis"))

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

files <- sub("N_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

NH4_dim <- roms_vars %>% dplyr::filter(name==c("NH4")) %>% purrr::pluck('grd')


variable_before_Atlantis2 <- roms %>%
  tidync::activate(NH4_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(NH4, NO3, longitude, latitude, sigma_layer,time)%>%
  dplyr::rename(
    NH4=NH4,
    NO3=NO3,
    longitude = longitude,
    latitude = latitude,
    roms_layer = sigma_layer, time = time)

gc()
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

foreach::foreach(days = step_file,
                 .packages = c("dplyr","tidync","ncdf4"))%dopar%{

  variable_before_Atlantis<- variable_before_Atlantis2 %>% dplyr::filter(time == days)
  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude", "roms_layer"))

  ###################################################################
  time = as.numeric(sort(unique(variables_polygons$time)))
  box = 89
  layer = 6
  N_var = 2

  atlantis_input_NH4 <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  atlantis_input_NO3 <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))

  for (i in 0:(box-1)){
    for (t in 1:length(time)){
      all.layers_NH4 = rep(NA,6)   # define an empty vector to receive the values of the 6 layers for NH4
      all.layers_NO3 = rep(NA,6) # define an empty vector to receive the values of the 6 layers for NO3
      # Calculate the layer
      for (j in 1:layer){
        subset <-variables_polygons %>%
          dplyr::filter(.bx0 == i, atlantis_layer == j, time == time[t])

        if (dim(subset)[1] == 0){
          all.layers_NH4[j] = NA
          all.layers_NO3[j] = NA
        }else{
          all.layers_NH4[j] <- (mean(subset$NH4, na.rm = T)*1000)[[1]] #unit conversion
          all.layers_NO3[j] <- (mean(subset$NO3, na.rm = T)*1000)[[1]] #unit conversion
        }
      }

      keep <- all.layers_NH4[is.na(all.layers_NH4)]
      all.layers_NH4 <- c(rev(all.layers_NH4[!is.na(all.layers_NH4)]),keep,NA)
      atlantis_input_NH4[,i+1,t] <- all.layers_NH4

      keep <- all.layers_NO3[is.na(all.layers_NO3)]
      all.layers_NO3 <- c(rev(all.layers_NO3[!is.na(all.layers_NO3)]),keep,NA)
      atlantis_input_NO3[,i+1,t] <- all.layers_NO3

    }
  }

  ###################################################################################
  # Define nc file
  ###################################################################################
  # Define dimensions
  z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
  b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
  t_dim <- ncdf4::ncdim_def("t",paste0("seconds since ",2095,"-01-01"), (time-1)*60*60)
  # Define variables
  z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
  b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = paste0("seconds since ",2095,"-01-01"), longname = "t")
  NH4 <- ncdf4::ncvar_def("NH4", "double", dim = list( z_dim,b_dim, t_dim),
                   units = "mgN.m-3", missval = NA, longname = "NH4")
  NO3 <- ncdf4::ncvar_def("NO3", "double", dim = list( z_dim,b_dim, t_dim),
                   units = "mgN.m-3", missval = NA, longname = "NO3")
  output_filename = paste0("/N_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- ncdf4::nc_create(nc_filename, vars = list(NH4 = NH4, NO3 = NO3))

  # Put dimensions and variables in the NetCDF file

  ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
  ncdf4::ncvar_put(nc, b_var, 0:(box-1))
  ncdf4::ncvar_put(nc, t_var, (time-1)*60*60)
  ncdf4::ncvar_put(nc, NH4, atlantis_input_NH4, start = c(1,1,1),count = c( layer+1,box, length(time)))
  ncdf4::ncvar_put(nc, NO3, atlantis_input_NO3, start = c(1,1,1),count = c( layer+1,box, length(time)))

  # Add minimum and maximum values to NH4 variable attributes
  ncdf4::ncatt_put(nc, "NH4", "valid_min", -50)
  ncdf4::ncatt_put(nc, "NH4", "valid_max", 200)

  # Add minimum and maximum values to NO3 variable attributes
  ncdf4::ncatt_put(nc, "NO3", "valid_min", 0)
  ncdf4::ncatt_put(nc, "NO3", "valid_max", 2000)

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
