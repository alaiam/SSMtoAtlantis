
# Set path
if (velma){
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_VELMA_",Nyear,"_TS")
  nc_filenameT <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_temperature_velma_",Nyear,".nc")
  nc_filenameS <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_salinity_velma_",Nyear,".nc")
  
}else{
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_No_VELMA_",Nyear,"_TS")
  nc_filenameT <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_temperature_","2011.nc")
  nc_filenameS <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_salinity_","2011.nc")
  
}

setwd(path)


list.file <- sort(list.files(path))


##############################  
##### File definition 

# Var
time = seq(0,730*12*60*60-1, 12*60*60) 
Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

# Table
atlantis_input_Temp <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_salinity <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)

print("yo")
# Aggregation
for (i in 1:length(list.file)){
  nc <- nc_open(paste0("Physical_var_AtlantisTS_",i,".nc"))
  pdt <- ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_Temp[,,i]      <- ncvar_get(nc, varid = "temperature")
  atlantis_input_salinity[,,i]  <- ncvar_get(nc, varid = "salinity")
  nc_close(nc)
}
print("ya")


###################################################################################
# Temperature file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t",units = paste0("seconds since ",Nyear,"-01-01"), time, unlim = T)


# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units =  paste0("seconds since ",Nyear,"-01-01"), longname = "t")
temperature <- ncvar_def("temperature", "double", dim = list( z_dim,b_dim, t_dim),
                         units = "°C", missval = 0, longname = "Temperature")

print("yre")

# Create a NetCDF file
nc <- nc_create(nc_filenameT, vars = list(temperature = temperature))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, temperature, atlantis_input_Temp, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to temperature variable attributes
ncatt_put(nc, "temperature", "valid_min", -50)
ncatt_put(nc, "temperature", "valid_max", 200)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)


print("ytt")

###################################################################################
# Salinity file
###################################################################################
# Define dimensions
t_dim <- ncdim_def("t", paste0("seconds since ",Nyear,"-01-01"), time, unlim = T)
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units =  paste0("seconds since ",Nyear,"-01-01"), longname = "t")
salinity <- ncvar_def("salinity", "double", dim = list( z_dim,b_dim, t_dim),
                      units = "g.L-1", missval = 0, longname = "Salinity")


# Create a NetCDF file
nc <- nc_create(nc_filenameS, vars = list(salinity = salinity))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, salinity, atlantis_input_salinity, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to salinity variable attributes
ncatt_put(nc, "salinity", "valid_min", 0)
ncatt_put(nc, "salinity", "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)

print("yrezgf")

setwd(here())

