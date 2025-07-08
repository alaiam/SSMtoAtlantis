
# Set path
if (velma){
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_VELMA_",Nyear,"_Z")
}else{
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_No_VELMA_",Nyear,"_Z")
}
setwd(path)


list.file <- sort(list.files(path))


time = seq(0,730*12*60*60-1, 12*60*60) 


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_SZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_LZ <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc <- nc_open(paste0("Zoo_Atlantis_",i,".nc"))
  pdt <- ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_SZ[,,i]      <- ncvar_get(nc, varid = "SZ")
  atlantis_input_LZ[,,i]      <- ncvar_get(nc, varid = "LZ")
  nc_close(nc)
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
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
SZ <- ncvar_def("Micro_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "SmallZooplankton")


# Create a NetCDF file
if (velma){
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_SZ_velma_",Nyear,".nc")
}else{
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_SZ_","2011.nc")
}

nc <- nc_create(nc_filename, vars = list(SZ = SZ))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, SZ, atlantis_input_SZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to SZ variable attributes
ncatt_put(nc, "Micro_Zoo_N", "valid_min", -50)
ncatt_put(nc, "Micro_Zoo_N", "valid_max", 1000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)






###################################################################################
# MZ file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
MZ <- ncvar_def("Meso_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "MZ")


# Create a NetCDF file
if (velma){
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_MZ_velma_",Nyear,".nc")
}else{
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_MZ_","2011.nc")
}

nc <- nc_create(nc_filename, vars = list(MZ = MZ))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, MZ, atlantis_input_MZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to MZ variable attributes
ncatt_put(nc, "Meso_Zoo_N", "valid_min", -50)
ncatt_put(nc, "Meso_Zoo_N", "valid_max", 1000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)








###################################################################################
# LZ file
###################################################################################
# Define dimensions
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
LZ <- ncvar_def("Lrg_Zoo_N", "double", dim = list( z_dim,b_dim, t_dim),
                 units = "mg N m-3", missval = -1, longname = "LargeZooplankton)")


# Create a NetCDF file
if (velma){
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_LZ_velma_",Nyear,".nc")
}else{
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_LZ_","2011.nc")
}

nc <- nc_create(nc_filename, vars = list(LZ = LZ))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, LZ, atlantis_input_LZ, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to LZ variable attributes
ncatt_put(nc, "Lrg_Zoo_N", "valid_min", -1)
ncatt_put(nc, "Lrg_Zoo_N", "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)
setwd(here())

