
input_path <- here::here("Atlantis_daily_files",scenario,year,"NO3")
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"NH4")
}
if(!dir.exists(input_path)){
  stop("The daily files were not created.")
}
output_path <- here::here("Atlantis_inputs",scenario,year)
nc_filenameNO3 <- paste0(output_path, "/pugetsound_SSM_Atlantis_NO3_",scenario,"_",year,".nc")
nc_filenameNH4 <- paste0(output_path, "/pugetsound_SSM_Atlantis_NH4_",scenario,"_",year,".nc")
list.file <- sort(list.files(input_path))

time = seq(0,730*12*60*60-1, 12*60*60)


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_NO3 <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_NH4 <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc <- ncdf4::nc_open(paste0(input_path, "/N_Atlantis_",i,".nc"))
  pdt <- ncdf4::ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_NO3[,,i]      <- ncdf4::ncvar_get(nc, varid = "NO3")
  atlantis_input_NH4[,,i]      <- ncdf4::ncvar_get(nc, varid = "NH4")
  ncdf4::nc_close(nc)
}

apply(X = is.na(atlantis_input_NH4),  FUN = sum, MARGIN = c(3))
apply(X = is.na(atlantis_input_NO3),  FUN = sum, MARGIN = c(3))
###################################################################################
# NO3 file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
NO3 <- ncdf4::ncvar_def("NO3", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "NO3")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameNO3, vars = list(NO3 = NO3))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, NO3, atlantis_input_NO3, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to NO3 variable attributes
ncdf4::ncatt_put(nc, "NO3", "valid_min", -50)
ncdf4::ncatt_put(nc, "NO3", "valid_max", max(atlantis_input_NO3, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)



###################################################################################
# NH4 file
###################################################################################
# Define dimensions
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
NH4 <- ncdf4::ncvar_def("NH3", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "NH3)")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameNH4, vars = list(NH4 = NH4))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, NH4, atlantis_input_NH4, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to NH4 variable attributes
ncdf4::ncatt_put(nc, "NH3", "valid_min", -1)
ncdf4::ncatt_put(nc, "NH3", "valid_max", max(atlantis_input_NH4, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)
