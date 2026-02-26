

input_path <- here::here("Atlantis_daily_files",scenario,year,"temperature")
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"salinity")
}
if(!dir.exists(input_path)){
  stop("The daily files were not created.")
}

output_path <- here::here("Atlantis_inputs",scenario,year)
  nc_filenameT <- paste0(output_path, "/pugetsound_SSM_Atlantis_temperature_",scenario,"_",year,".nc")
  nc_filenameS <- paste0(output_path, "/pugetsound_SSM_Atlantis_salinity_",scenario,"_",year,".nc")


list.file <- sort(list.files(input_path))

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

# Aggregation
for (i in 1:length(list.file)){
  nc <- ncdf4::nc_open(paste0(input_path, "/Physical_var_AtlantisTS_",i,".nc"))
  pdt <- ncdf4::ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_Temp[,,i]      <- ncdf4::ncvar_get(nc, varid = "temperature")
  atlantis_input_salinity[,,i]  <- ncdf4::ncvar_get(nc, varid = "salinity")
  ncdf4::nc_close(nc)
}

###################################################################################
# Temperature file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t",units = paste0("seconds since ",year,"-01-01"), time, unlim = T)


# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units =  paste0("seconds since ",year,"-01-01"), longname = "t")
temperature <- ncdf4::ncvar_def("temperature", "double", dim = list( z_dim,b_dim, t_dim),
                         units = "°C", missval = 0, longname = "Temperature")

# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameT, vars = list(temperature = temperature))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, temperature, atlantis_input_Temp, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to temperature variable attributes
ncdf4::ncatt_put(nc, "temperature", "valid_min", -50)
ncdf4::ncatt_put(nc, "temperature", "valid_max", 200)

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)


###################################################################################
# Salinity file
###################################################################################
# Define dimensions
t_dim <- ncdf4::ncdim_def("t", paste0("seconds since ",year,"-01-01"), time, unlim = T)
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units =  paste0("seconds since ",year,"-01-01"), longname = "t")
salinity <- ncdf4::ncvar_def("salinity", "double", dim = list( z_dim,b_dim, t_dim),
                      units = "g.L-1", missval = 0, longname = "Salinity")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameS, vars = list(salinity = salinity))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, salinity, atlantis_input_salinity, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to salinity variable attributes
ncdf4::ncatt_put(nc, "salinity", "valid_min", 0)
ncdf4::ncatt_put(nc, "salinity", "valid_max", 2000)

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)
