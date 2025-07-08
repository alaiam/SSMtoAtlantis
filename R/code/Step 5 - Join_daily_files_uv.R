
# Set path

path        <- here()
outdir <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear)
if (!file.exists(outdir)){dir.create(outdir)}

# Set path
if (velma){
  uv_dir <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_VELMA_",Nyear,"_uv")
  ww_dir <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_VELMA_",Nyear,"_ww")
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_uvw_velma_",Nyear,".nc")
  
}else{
  uv_dir <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_No_VELMA_",Nyear,"_uv")
  ww_dir <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_No_VELMA_",Nyear,"_ww")
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_uvw_novelma_",Nyear,".nc")
  
}



###########################################################################
# matrix dest_k, dest_b, 


table_flux_ww <- read.csv(paste0(ww_dir,"/flux_ww_1.csv"))
table_flux_uv <- read.csv(paste0(uv_dir,"/uv_1.csv"))


table_flux_ww1 <- table_flux_ww %>% 
  filter(time == 1)%>% 
  select("Layer", "Polygon..","adjacent.box", "water.exchange" , "Layer_dest") %>%
  rename(atlantis_layer = "Layer", 
         `Polygon #` = "Polygon..", 
         `adjacent box` = "adjacent.box", 
         Corr.water.transfert = "water.exchange", 
         Layer_dest = "Layer_dest")

table_flux_uv1 <- table_flux_uv %>% 
  filter(time == 1)%>% 
  select("atlantis_layer", "Polygon..","adjacent.box", "Corr.water.transfert" , "Layer_dest") %>%
  rename(`Polygon #` = "Polygon..", 
         `adjacent box` = "adjacent.box")

flux_all_table <- rbind(table_flux_uv1, table_flux_ww1)


##############################  
##### File definition 

# Var
ts = 730 
layer = 6
Nbox = 89
Nmaxdest = max(table(flux_all_table$atlantis_layer, flux_all_table$`Polygon #`))

# Table
dest_b =  array(rep(-1,layer*Nbox*Nmaxdest), dim = c(Nmaxdest, layer,Nbox, ts))
dest_k =  array(rep(-1,layer*Nbox*Nmaxdest), dim = c(Nmaxdest, layer,Nbox, ts))
exchange= array(rep(0,layer*Nbox*Nmaxdest), dim = c(Nmaxdest, layer,Nbox, ts))


# Aggregation
for (days in (1:ts)){

  table_flux_ww <- read.csv(paste0(ww_dir,"/flux_ww_",days,".csv"))

  table_flux_uv <- read.csv(paste0(uv_dir,"/uv_",days,".csv"))
  
  table_flux_ww1 <- table_flux_ww %>% 
    filter(time == days)%>% 
    select("Layer", "Polygon..","adjacent.box", "water.exchange" , "Layer_dest") %>%
    rename(atlantis_layer = "Layer", 
           `Polygon #` = "Polygon..", 
           `adjacent box` = "adjacent.box", 
           Corr.water.transfert = "water.exchange", 
           Layer_dest = "Layer_dest")
  
  table_flux_uv1 <- table_flux_uv %>% 
    filter(time == days)%>% 
    select("atlantis_layer", "Polygon..","adjacent.box", "Corr.water.transfert" , "Layer_dest") %>%
    rename(`Polygon #` = "Polygon..", 
           `adjacent box` = "adjacent.box")
  
  flux_all_table <- rbind(table_flux_uv1, table_flux_ww1)
  

  for (i in (1:length(flux_all_table$atlantis_layer))){
    box_i = flux_all_table$`Polygon #`[i]+1
    layer_i = flux_all_table$atlantis_layer[i]+1
    
    vector <- exchange[,layer_i,box_i,days] 
    Nrangement <- match("0", vector)
    
    dest_b[Nrangement,layer_i,box_i,days]   = flux_all_table$`adjacent box`[i]          # box de destination
    dest_k[Nrangement,layer_i,box_i,days]   = flux_all_table$Layer_dest[i]                                            # layer de destination
    exchange[Nrangement,layer_i,box_i,days] = flux_all_table$Corr.water.transfert[i]
    
  }
  
  }




###################################################################################
# Define nc file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 0:(layer-1))
b_dim <- ncdim_def("b","boxNum", 0:(Nbox-1))
t_dim <- ncdim_def("t","seconds since 2095-01-01", (seq(1,ts)-1)*60*60*12, unlim = T)
dest_dim <- ncdim_def("dest", "Nb max of destinaions", 1:Nmaxdest)

# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2095-01-01", longname = "t")
dest_var <- ncvar_def("dest", "int", dim = list(dest_dim), units = "dest", longname = "dest")


exchange_var = ncvar_def("exchange", prec = "double", dim = list(dest_dim, z_dim,b_dim,t_dim),
                         units = "m^3", missval = 0, longname = "Change in volume in this time step")
dest_b_var =  ncvar_def("dest_b", prec = "integer", dim = list(dest_dim, z_dim,b_dim,t_dim),
                        missval = -1, longname = "", units = "#")
dest_k_var =  ncvar_def("dest_k", prec = "integer", dim = list(dest_dim, z_dim,b_dim,t_dim),
                        missval = -1, longname = "", units = "#")


# Create a NetCDF file
nc <- nc_create(nc_filename, vars = list(exchange =exchange_var, dest_b = dest_b_var, dest_k = dest_k_var))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer))
ncvar_put(nc, b_var, 0:(Nbox-1))
ncvar_put(nc, exchange_var, exchange, start = c(1,1,1,1),count = c( Nmaxdest, layer,Nbox, ts))
ncvar_put(nc, dest_b_var, dest_b, start = c(1,1,1,1),count = c(Nmaxdest, layer,Nbox, ts))
ncvar_put(nc, dest_k_var, dest_k, start = c(1,1,1,1),count = c( Nmaxdest,layer,Nbox, ts))

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")
ncatt_put(nc, 0, "parameters", "")

# Close the NetCDF file
nc_close(nc)
setwd(here())
