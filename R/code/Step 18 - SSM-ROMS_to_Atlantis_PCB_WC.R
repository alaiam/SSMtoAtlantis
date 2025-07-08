


###########################################################################
# Path and names definition

path        <- paste0(here(), "/Workflow/Step B/")
input_path <- paste0(here(),"/Workflow/Step A/File_regular_grid/")

# Velma?
Velma = F
Nyear = 2011
pcb_n = 153
if (Velma){
  filename <- paste0("VELMA/",Nyear,"/regular_grid_PCB",pcb_n,"_velma_",Nyear,".nc")
  filename_sed <- paste0("VELMA/",Nyear,"/regular_grid_PCB",pcb_n,"_sed_velma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_VELMA_",Nyear,"_PCB",pcb_n,"/")
  
}else{
  filename <- paste0("No_VELMA/",Nyear,"/regular_grid_PCB",pcb_n,"_novelma_",Nyear,".nc")
  filename_sed <- paste0("No_VELMA/",Nyear,"/regular_grid_PCB",pcb_n,"_sed_novelma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_No_VELMA_",Nyear,"_PCB",pcb_n,"/")
}

if (!file.exists(output_path)){dir.create(output_path)}


###########################################################################
# Read data ROMS data
roms <- tidync(paste(input_path,filename, sep = ""))
roms_sed <- tidync(paste(input_path,filename_sed, sep = ""))
box_composition <- read.csv("Step B/code/box_composition.csv")

###########################################################################

# get list of ROMS variables
roms_vars <- tidync::hyper_grids(roms) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms %>% tidync::activate(x) %>% tidync::hyper_vars() %>% 
      dplyr::mutate(grd=x)
  })

roms_sed_vars <- tidync::hyper_grids(roms_sed) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms_sed  %>% tidync::activate(x) %>% tidync::hyper_vars() %>% 
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

files <- sub("PCB_Atlantis_", "", list.files(output_path))
files <- sort(as.numeric(sub(".nc", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

PCB_WC_dim <- roms_vars %>% dplyr::filter(name==c("WC")) %>% pluck('grd')
PCB_POC_dim <-roms_vars %>% dplyr::filter(name==c("POC")) %>% pluck('grd')
PCB_DOC_dim <-roms_vars %>% dplyr::filter(name==c("DOC")) %>% pluck('grd')

PCB_sed_dim <- roms_sed_vars %>% dplyr::filter(name==c("PCBsed")) %>% pluck('grd')
PCB_POCsed_dim <-roms_sed_vars %>% dplyr::filter(name==c("POC")) %>% pluck('grd')
PCB_DOCsed_dim <-roms_sed_vars %>% dplyr::filter(name==c("DOC")) %>% pluck('grd')

variable_before_Atlantis_sed2 <- roms_sed %>%
  tidync::activate(PCB_sed_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(PCBsed, POC, DOC, longitude, latitude,time)%>%
  dplyr::rename(
    PCB_sed=PCBsed, 
    PCB_sed_POC=POC, 
    PCB_sed_DOC=DOC, 
    longitude = longitude,  
    latitude = latitude,  
    time = time)
gc()
variable_before_Atlantis2 <- roms %>%
  tidync::activate(PCB_WC_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(WC, POC, DOC, longitude, latitude, sigma_layer,time)%>%
  dplyr::rename(
    PCB_WC=WC, 
    PCB_POC=POC, 
    PCB_DOC=DOC, 
    longitude = longitude,  
    latitude = latitude,  
    roms_layer = sigma_layer, time = time)



gc() #free unused memory before parallelization
cores=detectCores()
cl <- cores -1 #not to overload your computer
cl <- 4 #not to overload your computer
registerDoParallel(cl)

foreach(days = step_file) %dopar%{
  # for (days in 1:length(step_file)){
  
  
  
  
  variable_before_Atlantis<- variable_before_Atlantis2 %>% filter(time== days)
  variable_before_Atlantis_sed<- variable_before_Atlantis_sed2 %>% filter(time== days)
  
  
  
  variables_polygons <- merge(box_composition, variable_before_Atlantis, by = c("latitude", "longitude", "roms_layer"))
  variables_sed <- merge(box_composition, variable_before_Atlantis_sed, by = c("latitude", "longitude"))
  ###################################################################
  time = sort(unique(variables_polygons$time)) 
  box = 89
  layer = 6
  N_var = 2
  
  atlantis_input_PCB_WC <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  atlantis_input_PCB_POC <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  atlantis_input_PCB_DOC <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  
  for (i in 0:(box-1)){
    for (t in 1:length(time)){
      all.layers_PCB_WC = rep(NA,6)   # define an empty vector to receive the values of the 6 layers for PCB_WC
      all.layers_PCB_DOC = rep(NA,6) # define an empty vector to receive the values of the 6 layers for PCB_DOC
      all.layers_PCB_POC = rep(NA,6) # define an empty vector to receive the values of the 6 layers for PCB_DOC
      # Calculate the layer
      for (j in 1:layer){ 
        subset <-variables_polygons %>%
          filter(.bx0 == i, atlantis_layer == j, time == time[t])
        
        
        if (dim(subset)[1] == 0){
          all.layers_PCB_WC[j] = NA
          all.layers_PCB_DOC[j] = NA
          all.layers_PCB_POC[j] = NA
        }else{
          all.layers_PCB_WC[j] <- (mean(subset$PCB_WC, na.rm = T)*1000)[[1]] #g to mg
          all.layers_PCB_POC[j] <- (mean(subset$PCB_POC, na.rm = T)*1000)[[1]] #g to mg
          all.layers_PCB_DOC[j] <- (mean(subset$PCB_DOC, na.rm = T)*1000)[[1]] #g to mg
        }
      }
      subset <-variables_sed %>%
        filter(.bx0 == i, atlantis_layer == j)
      
      sed_value <- (mean(subset$PCB_sed, na.rm = T)*1000)[[1]] #g to mg
      keep <- all.layers_PCB_WC[is.na(all.layers_PCB_WC)]
      all.layers_PCB_WC <- c(rev(all.layers_PCB_WC[!is.na(all.layers_PCB_WC)]),keep,    sed_value)
      atlantis_input_PCB_WC[,i+1,t] <- all.layers_PCB_WC
      
      sed_value <- (mean(subset$PCB_sed_DOC, na.rm = T)*1000)[[1]]  #g to mg
      keep <- all.layers_PCB_DOC[is.na(all.layers_PCB_DOC)]
      all.layers_PCB_DOC <- c(rev(all.layers_PCB_DOC[!is.na(all.layers_PCB_DOC)]),keep, sed_value)
      atlantis_input_PCB_DOC[,i+1,t] <- all.layers_PCB_DOC
      
      sed_value <- (mean(subset$PCB_sed_POC, na.rm = T)*1000)[[1]]  #g to mg
      keep <- all.layers_PCB_POC[is.na(all.layers_PCB_POC)]
      all.layers_PCB_POC <- c(rev(all.layers_PCB_POC[!is.na(all.layers_PCB_POC)]),keep, sed_value)
      atlantis_input_PCB_POC[,i+1,t] <- all.layers_PCB_POC
      
    }
  }
  
  
  ###################################################################################
  # Define nc file
  ###################################################################################
  # Define dimensions
  z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
  b_dim <- ncdim_def("b","boxNum", 0:(box-1))
  t_dim <- ncdim_def("t","seconds since 2011-01-01", (time-1)*60*60)
  # Define variables
  z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
  b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
  t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
  PCB_WC <- ncvar_def("PCB_WC", "double", dim = list( z_dim,b_dim, t_dim),
                   units = "mg.m-3", missval = 0, longname = "PCB_WC")
  PCB_POC <- ncvar_def("PCB_POC", "double", dim = list( z_dim,b_dim, t_dim),
                       units = "mg.m-3", missval = 0, longname = "PCB_POC")
  PCB_DOC <- ncvar_def("PCB_DOC", "double", dim = list( z_dim,b_dim, t_dim),
                       units = "mg.m-3", missval = 0, longname = "PCB_DOC")
  output_filename = paste0("PCB_Atlantis_", days, ".nc")
  # Create a NetCDF file
  nc_filename <- paste0(output_path, output_filename)
  nc <- nc_create(nc_filename, vars = list(PCB_WC = PCB_WC, PCB_POC = PCB_POC, PCB_DOC = PCB_DOC))
  
  # Put dimensions and variables in the NetCDF file
  
  ncvar_put(nc, z_var, 1:(layer+1))
  ncvar_put(nc, b_var, 0:(box-1))
  ncvar_put(nc, t_var, (time-1)*60*60)
  ncvar_put(nc, PCB_WC, atlantis_input_PCB_WC, start = c(1,1,1),count = c( layer+1,box, length(time)))
  ncvar_put(nc, PCB_POC, atlantis_input_PCB_POC, start = c(1,1,1),count = c( layer+1,box, length(time)))
  ncvar_put(nc, PCB_DOC, atlantis_input_PCB_DOC, start = c(1,1,1),count = c( layer+1,box, length(time)))
  
  # Add minimum and maximum values to PCB_WC variable attributes
  ncatt_put(nc, "PCB_WC", "valid_min", -50)
  ncatt_put(nc, "PCB_WC", "valid_max", 200)
  
  # Add minimum and maximum values to PCB_POC variable attributes
  ncatt_put(nc, "PCB_POC", "valid_min", 0)
  ncatt_put(nc, "PCB_POC", "valid_max", 2000)
  
  # Add minimum and maximum values to PCB_DOC variable attributes
  ncatt_put(nc, "PCB_DOC", "valid_min", 0)
  ncatt_put(nc, "PCB_DOC", "valid_max", 2000)
  
  # Add dt attribute to t variable
  ncatt_put(nc, "t", "dt", 43200.0)
  
  # Global attributes
  ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
  ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
  ncatt_put(nc, 0, "parameters", "")
  
  # Close the NetCDF file
  nc_close(nc)
  
}


