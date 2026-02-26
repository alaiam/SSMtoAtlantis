
input_path <- here::here("Atlantis_daily_files",scenario,year,"SZ")
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"MZ")
}
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"LZ")
}
if(!dir.exists(input_path)){
  stop("The daily files were not created.")
}

output_path <- here::here("Atlantis_inputs",scenario,year)
nc_filenameSZ <- paste0(output_path, "/pugetsound_SSM_Atlantis_SZ_",scenario,"_",year,".nc")
nc_filenameMZ <- paste0(output_path, "/pugetsound_SSM_Atlantis_MZ_",scenario,"_",year,".nc")
nc_filenameLZ <- paste0(output_path, "/pugetsound_SSM_Atlantis_LZ_",scenario,"_",year,".nc")

list.file <- sort(list.files(input_path))


time = seq(0,730*12*60*60-1, 12*60*60)
Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_SZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_LZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc <- ncdf4::nc_open(paste0(input_path,"/Zoo_Atlantis_",i,".nc"))
  pdt <- ncdf4::ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_SZ[,,i]      <- ncdf4::ncvar_get(nc, varid = "SZ")
  atlantis_input_LZ[,,i]      <- ncdf4::ncvar_get(nc, varid = "LZ")
  ncdf4::nc_close(nc)
}

atlantis_input_MZ = (atlantis_input_SZ + atlantis_input_LZ)/3
atlantis_input_SZ = atlantis_input_SZ * 2/3
atlantis_input_LZ = atlantis_input_LZ * 2/3

apply(X = is.na(atlantis_input_LZ),  FUN = sum, MARGIN = c(3))
apply(X = is.na(atlantis_input_SZ),  FUN = sum, MARGIN = c(3))

###################################################################################
# SZ file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
SZ <- ncdf4::ncvar_def("Micro_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "SmallZooplankton")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameSZ, vars = list(SZ = SZ))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, SZ, atlantis_input_SZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to SZ variable attributes
ncdf4::ncatt_put(nc, "Micro_Zoo_N", "valid_min", -50)
ncdf4::ncatt_put(nc, "Micro_Zoo_N", "valid_max", max(atlantis_input_SZ, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)


###################################################################################
# MZ file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
MZ <- ncdf4::ncvar_def("Meso_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "MZ")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameMZ, vars = list(MZ = MZ))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, MZ, atlantis_input_MZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to MZ variable attributes
ncdf4::ncatt_put(nc, "Meso_Zoo_N", "valid_min", -50)
ncdf4::ncatt_put(nc, "Meso_Zoo_N", "valid_max", max(atlantis_input_MZ, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)


###################################################################################
# LZ file
###################################################################################
# Define dimensions
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
LZ <- ncdf4::ncvar_def("Lrg_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                 units = "mg N m-3", missval = -1, longname = "LargeZooplankton)")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameLZ, vars = list(LZ = LZ))

# Put dimensions and variables in the NetCDF file

ncdf4::ncvar_put(nc, z_var, 1:(layer+1))
ncdf4::ncvar_put(nc, b_var, 0:(box-1))
ncdf4::ncvar_put(nc, t_var, time)
ncdf4::ncvar_put(nc, LZ, atlantis_input_LZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to LZ variable attributes
ncdf4::ncatt_put(nc, "Lrg_Zoo_N", "valid_min", -1)
ncdf4::ncatt_put(nc, "Lrg_Zoo_N", "valid_max", max(atlantis_input_LZ, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)

