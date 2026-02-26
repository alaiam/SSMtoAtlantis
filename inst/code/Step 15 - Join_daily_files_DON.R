
input_path <- here::here("Atlantis_daily_files",scenario,year,"LDON")
if(!dir.exists(input_path)){
  input_path <- here::here("Atlantis_daily_files",scenario,year,"RDON")
}
if(!dir.exists(input_path)){
  stop("The daily files were not created.")
}
output_path <- here::here("Atlantis_inputs",scenario,year)
nc_filenameLDON <- paste0(output_path, "/pugetsound_SSM_Atlantis_LDON_",scenario,"_",year,".nc")
nc_filenameRDON <- paste0(output_path, "/pugetsound_SSM_Atlantis_RDON_",scenario,"_",year,".nc")

list.file <- sort(list.files(input_path))
time = seq(0,730*12*60*60-1, 12*60*60)


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_LDON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_RDON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc  <- ncdf4::nc_open(paste0(input_path,"/DON_Atlantis_",i,".nc"))
  pdt <- ncdf4::ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_LDON[,,i]      <- ncdf4::ncvar_get(nc, varid = "LDON")
  atlantis_input_RDON[,,i]      <- ncdf4::ncvar_get(nc, varid = "RDON")
  ncdf4::nc_close(nc)
}

apply(X = is.na(atlantis_input_RDON),  FUN = sum, MARGIN = c(3))
apply(X = is.na(atlantis_input_LDON),  FUN = sum, MARGIN = c(3))
###################################################################################
# LDON file
###################################################################################
# Define dimensions
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
LDON <- ncdf4::ncvar_def("LDON", "double", dim = list( z_dim,b_dim, t_dim),
                         units = "mg N m-3", missval = 0, longname = "Labile dissolved organic nitrogen")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameLDON, vars = list(LDON = LDON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, LDON, atlantis_input_LDON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to LDON variable attributes
ncdf4::ncatt_put(nc, "LDON", "valid_min", -50)
ncdf4::ncatt_put(nc, "LDON", "valid_max", max(atlantis_input_LDON, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)



###################################################################################
# RDON file
###################################################################################
# Define dimensions
t_dim <- ncdf4::ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdf4::ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdf4::ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncdf4::ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncdf4::ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncdf4::ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
RDON <- ncdf4::ncvar_def("DON", "double", dim = list( z_dim,b_dim, t_dim),
                      units = "mg N m-3", missval = 0, longname = "Refractory dissolved organic nitrogen")


# Create a NetCDF file
nc <- ncdf4::nc_create(nc_filenameRDON, vars = list(RDON = RDON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, RDON, atlantis_input_RDON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to RDON variable attributes
ncdf4::ncatt_put(nc, "DON", "valid_min", -1)
ncdf4::ncatt_put(nc, "DON", "valid_max", max(atlantis_input_RDON, na.rm = T))

# Add dt attribute to t variable
ncdf4::ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncdf4::ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncdf4::ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
ncdf4::nc_close(nc)
