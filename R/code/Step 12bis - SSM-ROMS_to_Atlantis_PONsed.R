
###########################################################################
# Path and names definition

input_path <- here::here("File_regular_grid", scenario, year)
filename <- paste0("/regular_grid_POM_sed_", scenario , "_",year, ".nc")
output_path <- here::here("Atlantis_daily_files", scenario, year, variable)

###########################################################################
# Read data ROMS data
roms <- tidync(paste0(input_path,filename))
box_composition <- read.csv(here("R/code/box_composition.csv"))
box_composition <- box_composition[box_composition$roms_layer==1,c(1,11,12)]

###########################################################################

# get list of ROMS variables
roms_vars <- tidync::hyper_grids(roms) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
      dplyr::mutate(grd=x)
  })

####
atlantis_bgm <- read_bgm(paste(path,"PugetSound_89b_070116.bgm", sep = ""))
atlantis_sf <- atlantis_bgm %>% box_sf()
area  <- (atlantis_sf %>% ungroup %>%
            st_drop_geometry()%>% select(area,box_id ))

layer_thickness <- c(5,20,25,50,50,200)

############################################################################################
############################################################################################
############################################################################################
step_file <- 1:730 #Days to divide the total files

files <- sub("PON_sed_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

PON_dim <- roms_vars %>% dplyr::filter(name==c("PON")) %>% pluck('grd')


variable_before_Atlantis2 <- roms %>%
  tidync::activate(PON_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(PON, longitude, latitude,time)%>%
  dplyr::rename(
    PON=PON,
    longitude = longitude,
    latitude = latitude, time = time)



gc() #free unused memory before parallelization
cores=detectCores()
cl <- cores -1 #not to overload your computer
cl <- 4 #not to overload your computer
registerDoParallel(cl)

foreach(days = step_file) %dopar%{
  # for (days in 1:length(step_file)){




  variable_before_Atlantis<- variable_before_Atlantis2 %>% filter(time== days)



  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude"))

  ###################################################################
  time = sort(unique(variables_polygons$time))
  box = 89
  atlantis_input_PON <- array(rep(NA,box*(layer+1)*length(time)), dim = c(box,length(time)))

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
  b_dim <- ncdim_def("b","boxNum", 0:(box-1))
  t_dim <- ncdim_def("t","seconds since 2011-01-01", (time-1)*60*60)
  # Define variables
  b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
  PON <- ncvar_def("PON", "double", dim = list(b_dim, t_dim),
                   units = "mg.m-3", missval = NA, longname = "LPON")
  output_filename = paste0("PON_sed_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- nc_create(nc_filename, vars = list(PON = PON))

  # Put dimensions and variables in the NetCDF file

  ncvar_put(nc, b_var, 0:(box-1))
  ncvar_put(nc, t_var, (time-1)*60*60)
  ncvar_put(nc, PON, atlantis_input_PON, start = c(1,1),count = c(box, length(time)))

  # Add minimum and maximum values to LPON variable attributes
  ncatt_put(nc, "PON", "valid_min", 0)
  ncatt_put(nc, "PON", "valid_max", 2000000)

  # Add dt attribute to t variable
  ncatt_put(nc, "t", "dt", 43200.0)

  # Global attributes
  ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
  ncatt_put(nc, 0, "parameters", "")

  # Close the NetCDF file
  nc_close(nc)

}


