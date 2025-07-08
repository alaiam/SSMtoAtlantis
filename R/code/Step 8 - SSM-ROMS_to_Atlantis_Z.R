


###########################################################################
# Path and names definition

path        <- paste0(here(), "/Workflow/Step B/")
input_path <- paste0(here(),"/Workflow/Step A/File_regular_grid/")

# Velma?
Velma = T
if (Velma){
  filename <- paste0("VELMA/",Nyear,"/regular_grid_Z_velma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_VELMA_",Nyear,"_Z/")
  
}else{
  filename <- paste0("No_VELMA/",Nyear,"/regular_grid_Z_novelma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_No_VELMA_",Nyear,"_Z/")
}

if (!file.exists(output_path)){dir.create(output_path)}


###########################################################################
# Read data ROMS data
roms <- tidync(paste0(input_path,filename))
box_composition <- read.csv(paste0(path,"code/box_composition.csv"))

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

files <- sub("Zoo_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out


SZ_dim <- roms_vars %>% dplyr::filter(name==c("SZ")) %>% pluck('grd')


variable_before_Atlantis2 <- roms %>%
  tidync::activate(SZ_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(SZ, LZ, longitude, latitude, sigma_layer,time)%>%
  dplyr::rename(
    SZ=SZ, 
    LZ=LZ, 
    longitude = longitude,  
    latitude = latitude,  
    roms_layer = sigma_layer, time = time)


gc()
cores=detectCores()
cl <- cores -1 #not to overload your computer
cl <- makeCluster(4) #not to overload your computer
registerDoParallel(4)

foreach(days = step_file) %dopar%{
  # for (days in 1:length(step_file)){
  
  
  
  
  variable_before_Atlantis<- variable_before_Atlantis2 %>% filter(time== days)
  
  
  
  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude", "roms_layer"))
  
  ###################################################################
  time = sort(unique(variables_polygons$time)) 
  box = 89
  layer = 6
  N_var = 2
  
  atlantis_input_SZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  atlantis_input_LZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  
  for (i in 0:(box-1)){
    for (t in 1:length(time)){
      all.layers_SZ = rep(NA,6)   # define an empty vector to receive the values of the 6 layers for SZ
      all.layers_LZ = rep(NA,6) # define an empty vector to receive the values of the 6 layers for LZ
      # Calculate the layer
      for (j in 1:layer){ 
        subset <-variables_polygons %>%
          filter(.bx0 == i, atlantis_layer == j, time == time[t])
        
        
        if (dim(subset)[1] == 0){
          all.layers_SZ[j] = NA
          all.layers_LZ[j] = NA
        }else{
          all.layers_SZ[j] <- (mean(subset$SZ, na.rm = T)*0.176*1000)[[1]] #redfield ratio
          all.layers_LZ[j] <- (mean(subset$LZ, na.rm = T)*0.176*1000)[[1]] #redfield ratio
          # all.layers_SZ[j] <- (mean(subset$SZ, na.rm = T)*area[i+1,1]*layer_thickness[j]*0.176*1000)[[1]] #redfield ratio
          # all.layers_LZ[j] <- (mean(subset$LZ, na.rm = T)*area[i+1,1]*layer_thickness[j]*0.176*1000)[[1]] #redfield ratio
        }
      }
      
      keep <- all.layers_SZ[is.na(all.layers_SZ)]
      all.layers_SZ <- c(rev(all.layers_SZ[!is.na(all.layers_SZ)]),keep,NA)
      atlantis_input_SZ[,i+1,t] <- all.layers_SZ
      
      keep <- all.layers_LZ[is.na(all.layers_LZ)]
      all.layers_LZ <- c(rev(all.layers_LZ[!is.na(all.layers_LZ)]),keep,NA)
      atlantis_input_LZ[,i+1,t] <- all.layers_LZ
      
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
  SZ <- ncvar_def("SZ", "double", dim = list( z_dim,b_dim, t_dim),
                  units = "mgN.m-3", missval = NA, longname = "SZ")
  LZ <- ncvar_def("LZ", "double", dim = list( z_dim,b_dim, t_dim),
                  units = "mgN.m-3", missval = NA, longname = "LZ")
  output_filename = paste0("Zoo_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- nc_create(nc_filename, vars = list(SZ = SZ, LZ = LZ))
  
  # Put dimensions and variaBbles in the NetCDF file
  
  ncvar_put(nc, z_var, 1:(layer+1))
  ncvar_put(nc, b_var, 0:(box-1))
  ncvar_put(nc, t_var, (time-1)*60*60)
  ncvar_put(nc, SZ, atlantis_input_SZ, start = c(1,1,1),count = c( layer+1,box, length(time)))
  ncvar_put(nc, LZ, atlantis_input_LZ, start = c(1,1,1),count = c( layer+1,box, length(time)))
  
  # Add minimum and maximum values to SZ variable attributes
  ncatt_put(nc, "SZ", "valid_min", -50)
  ncatt_put(nc, "SZ", "valid_max", 200)
  
  # Add minimum and maximum values to LZ variable attributes
  ncatt_put(nc, "LZ", "valid_min", 0)
  ncatt_put(nc, "LZ", "valid_max", 2000)
  
  # Add dt attribute to t variable
  ncatt_put(nc, "t", "dt", 43200.0)
  
  # Global attributes
  ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
  ncatt_put(nc, 0, "parameters", "")
  
  # Close the NetCDF file
  nc_close(nc)
  
}

