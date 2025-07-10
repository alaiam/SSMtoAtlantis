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
# Step 1a: Check filename and file_name_output sent from r 

print(filename)
print(file_name_output)

# Step 1b: define kriging function

# Create an RTree instance for spatial indexing using pyinterp
mesh = pyinterp.RTree()
#(original_values=org_WC, original_lon=original_lon, original_lat=original_lat, new_lon=my, new_lat=mx)

def kriging_universal(original_values, original_lon, original_lat, new_lat, new_lon):
    # Pack the original data into the RTree for spatial indexing (erases previous data)
    mesh.packing(np.vstack((original_lon, original_lat)).T, original_values)
    # Perform universal kriging to interpolate new_lon, new_lat points
    kriging, neighbors = mesh.universal_kriging(np.vstack(( new_lon.ravel(), new_lat.ravel())).T, within=True, k=3*3,
                                                covariance='matern_12', alpha=1_000_000, num_threads=0)
    return kriging.reshape(new_lon.shape)
print('Function defined!')
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
# Step 2d: Start interpolation of variables like PCB in phyto 1 and 2
# Define the dimensions of the data
siglay_size = len(original_siglay)
time_size = len(original_time)

# Variables
# Extract the original latitude, longitude, sigma layer, and time values

original_lat = lat
original_lon = lon 

# Create empty arrays to store the interpolated PBC1, PBC2
new_regular_WC = np.full((len(original_time), len(
    original_siglay), len(reg_lon), len(reg_lat)), np.nan)
new_regular_DOC = np.full((len(original_time), len(
    original_siglay), len(reg_lon), len(reg_lat)), np.nan)
new_regular_POC = np.full((len(original_time), len(
    original_siglay), len(reg_lon), len(reg_lat)), np.nan)
    
    
# np.min(ssm_solution.PCBCONW.values)
# np.max(ssm_solution.PCBCONW.values)
# Loop over each depth layer and interpolate the data onto the regular grid
for d in range(0, siglay_size):
  for t in range(0, time_size):  # Loop over time steps
        org_WC = ssm_solution.PCBCONW[t][0][d].values  # Extract WC values
        org_POC = ssm_solution.PCBCONPOCW[t][0][d].values # Extract POC values
        org_DOC = ssm_solution.PCBCONDOCW[t][0][d].values # Extract DOC values
        
        #Krigging
        new_regular_WC[t][d][:] = kriging_universal(
            org_WC, original_lon, original_lat, my, mx)
        new_regular_POC[t][d][:] = kriging_universal(
            org_POC, original_lon, original_lat, my, mx)
        new_regular_DOC[t][d][:] = kriging_universal(
            org_DOC, original_lon, original_lat, my, mx)

print('Interpolation variables done!')

import matplotlib.pyplot as plt
plt.figure(figsize=(10, 10))
plt.pcolormesh(mx, my, new_regular_POC[100][0], cmap='viridis')
np.min(org_WC)
plt.colorbar(label='PCB WC')
plt.title('Interpolated PCB in POM in WC')
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

WC_var = nc.createVariable(
    'WC', np.single, ('time', 'sigma_layer', 'longitude','latitude' ))
WC_var.units = 'g meters-3'
WC_var.standard_name = 'PCB in WC'
WC_var[:] = new_regular_WC.astype('float')


POC_var = nc.createVariable(
    'POC', np.single, ('time', 'sigma_layer', 'longitude', 'latitude'))
POC_var.units = 'g meters-3'
POC_var.standard_name = 'PCB in POC'
POC_var[:] = new_regular_POC.astype('float')

DOC_var = nc.createVariable(
    'DOC', np.single, ('time', 'sigma_layer', 'longitude', 'latitude'))
DOC_var.units = 'g meters-3'
DOC_var.standard_name = 'PCB in DOC'
DOC_var[:] = new_regular_DOC.astype('float')

nc.close()
print('New ROMSgrid NetCDF file created!')


