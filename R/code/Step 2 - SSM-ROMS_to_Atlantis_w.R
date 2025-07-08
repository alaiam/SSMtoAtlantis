library(reshape2)
###########################################################################
# Path and names definition

path        <- paste0(here(), "/Workflow/Step B/")
input_path <- paste0(here(),"/Workflow/Step A/File_regular_grid/")

# Velma?
velma = T
if (velma){
  filename <- paste0("VELMA/",Nyear,"/regular_grid_UVW_velma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_VELMA_",Nyear,"_ww/")
  
}else{
  filename <- paste0("No_VELMA/",Nyear,"/regular_grid_UVW_novelma_",Nyear,".nc")
  output_path <- paste0(path, "intermediate output archive/output_No_VELMA_",Nyear,"_ww/")
}

if (!file.exists(output_path)){dir.create(output_path)}


###########################################################################
# Read data ROMS data
roms <- tidync(paste(input_path,filename, sep = ""))
box_composition <- read.csv(paste0(path, "code/box_composition_ww.csv"))
layer_max <- read.csv(paste0(path, "code/layer_max_ww.csv"))


# get list of ROMS variables
roms_vars <- tidync::hyper_grids(roms) %>% # all available grids in the ROMS ncdf
  pluck("grid") %>% # for each grid, pull out all the variables associated with that grid and make a reference table
  purrr::map_df(function(x){
    roms %>% tidync::activate(x) %>% tidync::hyper_vars() %>% 
      dplyr::mutate(grd=x)
  })



names(layer_max)[1] <- "Polygon #"
layer_depth <-box_composition %>%
  select("atlantis_layer", "dz.x")%>% 
  mutate(Layer = atlantis_layer-1, Layer_dest = atlantis_layer-1)%>%
  distinct()%>%
  select("dz.x", Layer, Layer_dest)

############################################################################################
############################################################################################
############################################################################################
step_file <- 1:730 #Days to divide the total files

files <- sub("flux_ww_", "", list.files(output_path))
files <- sort(as.numeric(sub(".csv", "", files)))
out <- (1:730)[!1:730 %in% files]
step_file <- out

ww_dim <- roms_vars %>% dplyr::filter(name==c("ww")) %>% pluck('grd')

cores=detectCores()
cl <- cores -1 #not to overload your computer
cl <- 4 #not to overload your computer
registerDoParallel(cl)

variable_before_Atlantis <- roms %>%
  tidync::activate(ww_dim) %>%
  tidync::hyper_tibble(force = TRUE) %>%
  dplyr::select(ww, longitude, latitude, sigma_layer, time)%>%
  dplyr::rename(
    ww=ww, 
    longitude = longitude,  
    latitude = latitude,  
    roms_layer = sigma_layer, time = time)
gc()
foreach(days = step_file) %dopar%{

  
  variable_before_Atlantis2<- variable_before_Atlantis %>% filter(time==days)
  print("1")
  merge_test <- merge(box_composition, variable_before_Atlantis2, by = c("latitude", "longitude", "roms_layer"))
  print("2")
  
  ###################################################################
  time = sort(unique(merge_test$time))          # To adapt after time intergration
  box = 89
  layer = 6
  
  atlantis_input_ww <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
  
  for (i in 0:(box-1)){
    
    for (t in 1:length(time)){
      all.layers_ww = rep(NA,7)   # define an empty vector to receive the values of the 6 layers for ww
      # Calculate the layer
      for (j in 1:layer){ 
        subset <-merge_test %>%
          filter(.bx0 == i, atlantis_layer == j, time == time[t])
        
        
        if (dim(subset)[1] == 0){
          all.layers_ww[j] = NA
        }else{
          all.layers_ww[j] <- mean(subset$ww, na.rm = T)
        }
      }
      
      atlantis_input_ww[,i+1,t] <- all.layers_ww
      
    }
  }
  atlantis_input_ww <- atlantis_input_ww*12*60*60
  
  print("3")
  
  
  table_flux_ww <- melt(atlantis_input_ww, varnames = c("Layer", "Polygon #", "time"), value.name = "water.exchange") %>%
    mutate(`Polygon #` = `Polygon #` - 1,
           `adjacent box` = `Polygon #`,
           Layer = Layer - 1,
           Layer_dest = Layer + 1, 
           time = min(merge_test$time) - 1 + time)
  print("4")
  
  table_flux_ww <- table_flux_ww %>% left_join(layer_max, by = "Polygon #") 
  
  table_flux_ww<- table_flux_ww %>%
    filter(Layer_dest <= layer_max) %>%
    select(Layer, `Polygon #` ,time, water.exchange, `adjacent box`, Layer_dest)
  
  
  output_filename <- paste0("flux_ww_", days,".csv")
  csv_filename <- paste0(output_path, output_filename)
  write.csv(table_flux_ww, csv_filename, row.names = F)
  print("5")
  
}

