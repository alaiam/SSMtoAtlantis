
input_path <- here::here("Atlantis_daily_files",scenario,year,"LPON")
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"RPON")
}
if(!dir.exists(input_path)){
  stop("The daily files were not created.")
}
output_path <- here::here("Atlantis_inputs",scenario,year)
nc_filenameLPON <- paste0(output_path, "/pugetsound_SSM_Atlantis_LPON_",scenario,"_",year,".nc")
nc_filenameRPON <- paste0(output_path, "/pugetsound_SSM_Atlantis_RPON_",scenario,"_",year,".nc")


list.file <- sort(list.files(input_path))
time = seq(0,730*12*60*60-1, 12*60*60)


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_LPON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_RPON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:(length(list.file)/2)){
  nc  <- ncdf4::nc_open(paste0(input_path,"/PON_Atlantis_",i,".nc"))
  pdt <- ncdf4::ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_LPON[,,i]      <- ncdf4::ncvar_get(nc, varid = "LPON")
  atlantis_input_RPON[,,i]      <- ncdf4::ncvar_get(nc, varid = "RPON")
  ncdf4::nc_close(nc)
}

for (i in 1:(length(list.file)/2)){
  nc <- nc_open(paste0(input_path,"/PON_sed_Atlantis_",i,".nc"))
  pdt <- ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_LPON[7,,i]      <- ncvar_get(nc, varid = "PON")*(1/9)
  atlantis_input_RPON[7,,i]      <- ncvar_get(nc, varid = "PON")*(8/9)
  nc_close(nc)
}
plot(apply(atlantis_input_RPON[7,,], FUN = function (x) mean(x, na.rm = T), MARGIN = 2))
plot(apply(atlantis_input_RPON[1:6,,]/atlantis_input_LPON[1:6,,], FUN = function (x) mean(x, na.rm = T), MARGIN = 3))
apply(X = is.na(atlantis_input_RPON),  FUN = sum, MARGIN = c(3))
apply(X = is.na(atlantis_input_LPON),  FUN = sum, MARGIN = c(3))
###################################################################################
# LPON file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
LPON <- ncdf4::ncvar_def("Lab_Det_N", "double", dim = list( z_dim,b_dim, t_dim),
                         units = "mg N m-3", missval = 0, longname = "Labile particulate organic nitrogen")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameLPON, vars = list(LPON = LPON))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, LPON, atlantis_input_LPON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to LPON variable attributes
ncdf4::ncatt_put(nc, "Lab_Det_N", "valid_min", -50)
ncdf4::ncatt_put(nc, "Lab_Det_N", "valid_max", max(atlantis_input_LPON, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)



###################################################################################
# RPON file
###################################################################################
# Define dimensions
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
RPON <- ncdf4::ncvar_def("Ref_Det_N", "double", dim = list( z_dim,b_dim, t_dim),
                      units = "mg N m-3", missval = 0, longname = "Refractory particulate organic nitrogen")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameRPON, vars = list(RPON = RPON))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, RPON, atlantis_input_RPON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to RPON variable attributes
ncdf4::ncatt_put(nc, "Ref_Det_N", "valid_min", -1)
ncdf4::ncatt_put(nc, "Ref_Det_N", "valid_max", max(atlantis_input_RPON, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)
