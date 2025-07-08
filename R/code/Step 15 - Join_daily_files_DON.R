# Set path
if (velma){
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_VELMA_",Nyear,"_DON")
}else{
  path <- paste0(here(), "/Workflow/Step B/intermediate output archive/output_No_VELMA_",Nyear,"_DON")
}
setwd(path)

list.file <- sort(list.files(path))
time = seq(0,730*12*60*60-1, 12*60*60) 


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_LDON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_RDON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc <- nc_open(paste0("DON_Atlantis_",i,".nc"))
  pdt <- ncvar_get(nc, varid = "t")/60/60+1
  atlantis_input_LDON[,,i]      <- ncvar_get(nc, varid = "LDON")
  atlantis_input_RDON[,,i]      <- ncvar_get(nc, varid = "RDON")
  nc_close(nc)
}

apply(X = is.na(atlantis_input_RDON),  FUN = sum, MARGIN = c(3))
apply(X = is.na(atlantis_input_LDON),  FUN = sum, MARGIN = c(3))
###################################################################################
# LDON file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
LDON <- ncvar_def("LDON", "double", dim = list( z_dim,b_dim, t_dim),
                         units = "mg N m-3", missval = 0, longname = "Labile dissolved organic nitrogen")


# Create a NetCDF file
if (velma){
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_LDON_velma_",Nyear,".nc")
}else{
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_LDON_","2011.nc")
}
nc <- nc_create(nc_filename, vars = list(LDON = LDON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, LDON, atlantis_input_LDON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to LDON variable attributes
ncatt_put(nc, "LDON", "valid_min", -50)
ncatt_put(nc, "LDON", "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)



###################################################################################
# RDON file
###################################################################################
# Define dimensions
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
RDON <- ncvar_def("DON", "double", dim = list( z_dim,b_dim, t_dim),
                      units = "mg N m-3", missval = 0, longname = "Refractory dissolved organic nitrogen")


# Create a NetCDF file
if (velma){
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/VELMA/",Nyear,"/pugetsound_SSM_Atlantis_RDON_velma_",Nyear,".nc")
}else{
  nc_filename <- paste0(here(), "/Workflow/Step B/Final outputs/No_VELMA/",Nyear,"/pugetsound_SSM_Atlantis_RDON_","2011.nc")
}
nc <- nc_create(nc_filename, vars = list(RDON = RDON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, RDON, atlantis_input_RDON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to RDON variable attributes
ncatt_put(nc, "DON", "valid_min", -1)
ncatt_put(nc, "DON", "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)
setwd(here())
