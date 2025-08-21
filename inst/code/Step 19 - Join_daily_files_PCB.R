
input_path <- here::here("Atlantis_daily_files",scenario,year,"PCB")
output_path <- here::here("Atlantis_inputs",scenario,year)
nc_filenameWC  <- paste0(output_path, "/pugetsound_SSM_Atlantis_PCB", PCB_congener, "_",scenario,"_",year,".nc")
nc_filenamePON <- paste0(output_path, "/pugetsound_SSM_Atlantis_PCB", PCB_congener, "_PON_",scenario,"_",year,".nc")
nc_filenameDON <- paste0(output_path, "/pugetsound_SSM_Atlantis_PCB", PCB_congener, "_DON_",scenario,"_",year,".nc")
list.file <- sort(list.files(input_path))


time = seq(0,730*12*60*60-1, 12*60*60)


Ndt = 1:length(time)
box = 89
layer = 6
N_var = 2

atlantis_input_WC <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))
atlantis_input_PON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))/0.176 # Concentration in N higher to take into account all the material
atlantis_input_DON <- array(rep(NA,box*(layer+1)*length(time)), dim = c((layer+1),box,length(time)))/0.176 # Concentration in N higher to take into account all the material

liste <- sort(list.file)
for (i in 1:length(list.file)){
  nc <- nc_open(paste0(input_path,"/PCB_Atlantis_",i,".nc"))
  pdt <- ncvar_get(nc, varid = "t")/60/60+1

  atlantis_input_WC[,,pdt] <- ncvar_get(nc, varid = "PCB_WC")
  # Division by 2 is a rough way to split the PCB between refractory and labile
  # In can be improve using the [] of each group as a proxy
  atlantis_input_PON[,,pdt] <- ncvar_get(nc, varid = "PCB_POC")   # Redfield ratio?? Do we need it ? How do we translate into
  atlantis_input_DON[,,pdt] <- ncvar_get(nc, varid = "PCB_DOC")    # Redfield ratio
  nc_close(nc)
}



###################################################################################
# PCB_habitat file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
WC <- ncvar_def(paste0("PCB",pcb_n), "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "PCB in the WC")


# Create a NetCDF file
nc <- nc_create(nc_filenameWC, vars = list(WC = WC))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, WC, atlantis_input_WC, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to PCB_habitat variable attributes
ncatt_put(nc, paste0("PCB",pcb_n), "valid_min", -50)
ncatt_put(nc, paste0("PCB",pcb_n), "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)



###################################################################################
# PON file
###################################################################################
# Define dimensions
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
PON <- ncvar_def(paste0("PCB",pcb_n, "_PON"), "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "PCB in PON")


# Create a NetCDF file
nc <- nc_create(nc_filenamePON, vars = list(PON = PON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, PON, atlantis_input_PON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to PON variable attributes
ncatt_put(nc, paste0("PCB",pcb_n,"_PON"), "valid_min", -50)
ncatt_put(nc, paste0("PCB",pcb_n,"_PON"), "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)



###################################################################################
# DON file
###################################################################################
# Define dimensions
t_dim <- ncdim_def("t","seconds since 2011-01-01", time, unlim = T)
z_dim <- ncdim_def("z","layerNum", 1:(layer+1))
b_dim <- ncdim_def("b","boxNum", 0:(box-1))

# Define variables
z_var <- ncvar_def("z", "int", dim = list(z_dim), units = "depthBin", longname = "z")
b_var <- ncvar_def("b", "int", dim = list(b_dim), units = "boxNum", longname = "b")
t_var <- ncvar_def("t", "double", dim = list(t_dim), units = "seconds since 2011-01-01", longname = "t")
DON <- ncvar_def(paste0("PCB",pcb_n,"_DON"), "double", dim = list( z_dim,b_dim, t_dim),
                units = "mg N m-3", missval = 0, longname = "PCB_DON")


# Create a NetCDF file
nc <- nc_create(nc_filenameDON, vars = list(DON = DON))

# Put dimensions and variables in the NetCDF file

ncvar_put(nc, z_var, 1:(layer+1))
ncvar_put(nc, b_var, 0:(box-1))
ncvar_put(nc, t_var, time)
ncvar_put(nc, DON, atlantis_input_DON, start = c(1,1,1),count = c( layer+1,box, length(time)))

# Add minimum and maximum values to DON variable attributes
ncatt_put(nc, paste0("PCB",pcb_n,"_DON"), "valid_min", -1)
ncatt_put(nc, paste0("PCB",pcb_n,"_DON"), "valid_max", 2000)

# Add dt attribute to t variable
ncatt_put(nc, "t", "dt", 43200.0)

# Global attributes
ncatt_put(nc, 0, "title", "PSIMF Atlantis forcing")
ncatt_put(nc, 0, "geometry", "PugetSound_89b_070116.bgm")

# Close the NetCDF file
nc_close(nc)
