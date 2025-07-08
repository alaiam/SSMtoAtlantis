


###########################################################################
# Path and names definition

path        <- paste0(here(), "/Workflow/Step B/")
input_path <- paste0(here(),"/Workflow/Step A/File_regular_grid/")

# Velma?
Velma = T
if (Velma){
  filename <- paste0("VELMA/",Nyear,"/regular_grid_Oxygen_velma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_VELMA_",Nyear,"_O2/")
  
}else{
  filename <- paste0("No_VELMA/",Nyear,"/regular_grid_Oxygen_novelma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_No_VELMA_",Nyear,"_O2/")
}

if (!file.exists(output_path)){dir.create(output_path)}

###########################################################################
# Read data ROMS data
roms <- tidync(paste(input_path,filename, sep = ""))
box_composition <- read.csv("Workflow/Step B/code/box_composition.csv")

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

files <- sub("O2_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

O2_dim <- roms_vars %>% dplyr::filter(name==c("O2")) %>% pluck('grd')


variable_before_Atlantis2 <- roms %>%
  tidync::activate(O2_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(O2, longitude, latitude, sigma_layer,time)%>%
  dplyr::rename(
    O2=O2, 
    longitude = longitude,  
    latitude = latitude,  
    roms_layer = sigma_layer, time = time)



gc()
# cores=detectCores()
# cl <- cores -1 #not to overload your computer
cl <- 4 #not to overload your computer
registerDoParallel(cl)

foreach(days = step_file) %dopar%{
  # for (days in 1:length(step_file)){
  
  
  
  
  variable_before_Atlantis<- variable_before_Atlantis2 %>% filter(time== days)
  
  
  
  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude", "roms_layer"))
  
  ###################################################################
  time = sort(unique(variables_polygons$time)) 
  box = 89
  layer = 6
  N_var = 1
  
  atlantis_input_O2 <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))

  for (i in 0:(box-1)){
    for (t in 1:length(time)){
      all.layers_O2 = rep(NA,6)   # define an empty vector to receive the values of the 6 layers for O2
      # Calculate the layer
      for (j in 1:layer){ 
        subset <-variables_polygons %>%
          filter(.bx0 == i, atlantis_layer == j, time == time[t])
        
        
        if (dim(subset)[1] == 0){
          all.layers_O2[j] = NA
        }else{
          all.layers_O2[j] <- (mean(subset$O2, na.rm = T))[[1]] 
        }
      }
      
      keep <- all.layers_O2[is.na(all.layers_O2)]
      all.layers_O2 <- c(rev(all.layers_O2[!is.na(all.layers_O2)]),keep,NA)
      atlantis_input_O2[,i+1,t] <- all.layers_O2
    }
  }
  
  
  ###################################################################################
  # Define nc file
  ###################################################################################
  # Define dimensions
  z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
  b_dim <- ncdim_def("b","boxNum", 0:(box-1))
  t_dim <- ncdim_def("t","seconds since 2095-01-01", (time-1)*60*60)
  # Define variables
  z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
  b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2095-01-01", longname = "t")
  O2 <- ncvar_def("O2", "double", dim = list( z_dim,b_dim, t_dim),
                   units = "mgN", missval = NA, longname = "O2")
  output_filename = paste0("O2_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- nc_create(nc_filename, vars = list(O2 = O2))
  
  # Put dimensions and variables in the NetCDF file
  
  ncvar_put(nc, z_var, 1:(layer+1))
  ncvar_put(nc, b_var, 0:(box-1))
  ncvar_put(nc, t_var, (time-1)*60*60)
  ncvar_put(nc, O2, atlantis_input_O2, start = c(1,1,1),count = c( layer+1,box, length(time)))
  # Add minimum and maximum values to O2 variable attributes
  ncatt_put(nc, "O2", "valid_min", -50)
  ncatt_put(nc, "O2", "valid_max", 200)
  
  
  # Add dt attribute to t variable
  ncatt_put(nc, "t", "dt", 43200.0)
  
  # Global attributes
  ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
  ncatt_put(nc, 0, "parameters", "")
  
  # Close the NetCDF file
  nc_close(nc)
  
}

