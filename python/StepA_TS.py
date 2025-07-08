#!/bin/bash
# File name: StepA_croppedfiles.py

###########
# import packages
import numpy as np
import xarray as xr
import netCDF4
import datetime as dt
import os
import pyinterp
import pyproj
import sys

###########
# Step 1a: Retrieve filename and file_name_output from r env

filename = r.file_name_input
print(filename)

file_name_output = r.file_name_output
print(file_name_output)

# Step 1b: define kriging function

# Create an RTree instance for spatial indexing using pyinterp
mesh = pyinterp.RTree()
#(original_values=org_temp, original_lon=original_lon, original_lat=original_lat, new_lon=my, new_lat=mx)

def kriging_universal(original_values, original_lon, original_lat, new_lat, new_lon):
    # Pack the original data into the RTree for spatial indexing (erases previous data)
    mesh.packing(np.vstack((original_lon, original_lat)).T, original_values)
    # Perform universal kriging to interpolate new_lon, new_lat points
    kriging, neighbors = mesh.universal_kriging(np.vstack(( new_lon.ravel(), new_lat.ravel())).T, within=True, k=3*3,
                                                covariance='matern_12', alpha=1_000_000, num_threads=0)
    return kriging.reshape(new_lon.shape)

###########
# Step 2b: open netCDF file
# Open the NetCDF file and read the original Salish Sea Model data
ssm_solution = xr.open_dataset(filename, decode_cf=True, decode_times=False)
print('NetCDF file read!')

xssmvarb = ssm_solution.x.data 
yssmvarb = ssm_solution.y.data 

# Su Kyong's UTM to Lat/Lon conversion code
transformer_xy_latlon = pyproj.Transformer.from_crs('epsg:26910', 'epsg:4326', always_xy=True)
lon, lat = transformer_xy_latlon.transform(xssmvarb, yssmvarb)
print('Transformed to lat/lon!')

###########
# Step 2c: Start section 1 of interpolation code
# NEW grid set up in SSM files
# this will not be used for the interpolation step, but necessary to unpack netCDF file.

# ~~~~~~~~~~~~~~
# Define the regular grid with a step of 0.01 for the new latitude and longitude
min_lat = np.array(lat.min())
max_lat = np.array(lat.max())
min_lon = np.array(lon.min())
max_lon = np.array(lon.max())

STEP = 0.01
reg_lat = np.arange(min_lat - STEP, max_lat + STEP, STEP)
reg_lon = np.arange(min_lon - STEP, max_lon + STEP, STEP)

# Meshgrid for the regular grid
mx, my = np.meshgrid(reg_lon, reg_lat, indexing='ij') 
print('Defined meshgrid!')

# Global Values

original_siglay = np.array([-0.01581139, -0.06053274, -0.12687974, -0.20864949, -0.30326778, -0.40915567, -0.52520996, -0.65060186, -0.78467834, -0.9269075 ])
original_siglev = np.array([-0., -0.03162277, -0.08944271, -0.16431676, -0.2529822 , -0.35355335, -0.46475798, -0.58566195, -0.7155418 , -0.85381496, -1.])
original_time = np.arange(0, 365, 0.5)

#######
# Step 2d: Start interpolation of variables like temperature, salinity and sigma layer values
# Define the dimensions of the data
siglay_size = len(original_siglay)
time_size = len(original_time)

# Variables
# Extract the original latitude, longitude, sigma layer, and time values
original_lat = lat
original_lon = lon 

# Create empty arrays to store the interpolated temperature, salinity, and sigma layer values
new_regular_temp = np.full((len(original_time), len(
    original_siglay), len(reg_lon), len(reg_lat)), np.nan)
new_regular_salt = np.full((len(original_time), len(
    original_siglay), len(reg_lon), len(reg_lat)), np.nan)


# Loop over each depth layer and interpolate the data onto the regular grid
for d in range(0, siglay_size):
  for t in range(0, time_size):  # Loop over time steps
        org_temp = ssm_solution.temp[t][d].values  # Extract temperature values
        org_salt = ssm_solution.salinity[t][d].values # Extract salinity values
        
        #Krigging
        new_regular_temp[t][d][:] = kriging_universal(
            org_temp, original_lon, original_lat, my, mx)
        new_regular_salt[t][d][:] = kriging_universal(
            org_salt, original_lon, original_lat, my, mx)

print('Interpolation variables done!')

# org_salt = ssm_solution.salinity[1][4].values
# new_regular_salt_test_l = kriging_universal(
# org_salt, original_lon, original_lat, my, mx)

import matplotlib.pyplot as plt
plt.figure(figsize=(10, 10))
plt.pcolormesh(mx, my, new_regular_temp[350][1], cmap='viridis')
plt.colorbar(label='Temperature')
plt.title('Interpolated Temperature')
plt.xlabel('Longitude')
plt.ylabel('Latitude')
plt.show()

# Create a new NetCDF file with the interpolated data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save the interpolated data to a new NetCDF file

nc = netCDF4.Dataset(file_name_output, 'w')

# Define NetCDF global attributes
nc.title = 'Regular grid for the Salish Sea Model'
nc.Conventions = 'CF-1.6'
nc.history = '{0} creation of regular grid NetCDF file by Javier Porobic'.format(
    dt.datetime.now().strftime("%Y-%m-%d"))


# Create NetCDF variables and set attributes
lat_dim = nc.createDimension('latitude', len(reg_lat))
lon_dim = nc.createDimension('longitude', len(reg_lon))
siglev_dim = nc.createDimension('sigma_layer', siglay_size)
time_dim = nc.createDimension('time', time_size)

lat_var = nc.createVariable('latitude', np.single, ('latitude'))
lat_var.units = 'degrees_north'
lat_var.standard_name = 'latitude'
lat_var.axis = 'Y'
lat_var[:] = reg_lat.astype('float')

lon_var = nc.createVariable('longitude', np.single, ('longitude'))
lon_var.units = 'degrees_east'
lon_var.standard_name = 'longitude'
lon_var.axis = 'X'
lon_var[:] = reg_lon.astype('float')

time_var = nc.createVariable('time_vector', np.intc, ('time'))
time_var.units = 'days since 1858-11-17 00:00:00'
time_var.standard_name = 'time'
time_var.format = 'modified julian day (MJD)'
time_var.time_zone = 'UTC'
time_var[:] = original_time.astype('int')

siglay_var = nc.createVariable('siglay', np.single, ('sigma_layer'))
siglay_var.units = 'sigma_layers'
siglay_var.standard_name = 'ocean_sigma/general_coordinate'
siglay_var[:] = original_siglay.astype('float') 

temp_var = nc.createVariable(
    'temp', np.single, ('time', 'sigma_layer', 'longitude','latitude' ))
temp_var.units = 'degrees_C'
temp_var.standard_name = 'sea_water_temperature'
temp_var[:] = new_regular_temp.astype('float')


salt_var = nc.createVariable(
    'salinity', np.single, ('time', 'sigma_layer', 'longitude', 'latitude'))
salt_var.units = '1e-3'
salt_var.standard_name = 'sea_water_salinity'
salt_var[:] = new_regular_salt.astype('float')

nc.close()
print('New ROMSgrid NetCDF file created!')


del new_regular_temp
del new_regular_salt
print('Clean var!')
