# SSMtoAtlantis

**SSMtoAtlantis** is an R package for converting Salish Sea Models outputs (irregular grid FVCOM like)) into forcing files compatible with the Atlantis model for Puget Sound.

Atlantis requires specific NetCDF input files describing physical (e.g., temperature, salinity, fluxes) (mandatory) and biogeochemical forcings (e.g., plankton, oxygen, nutrients, detritus) (optional). 
This package automates the creation of those forcing files from irregular grid, to regular grid (ROMS-like) in Step A
and from regular grid to Atlantis polygons (Step B).

## Features

- Aggregate physical and biological SSM variable into Atlantis boxes and layers  
- Generate yearly forcing files netcdf for Atlantis  
- Support for variables such as temperature, salinity, nutrients, zooplankton, and fluxes  
- Parallelized processing for speed on large datasets  

## Installation

To install the development version of **SSMtoAtlantis** from GitHub:

```r
# install.packages("devtools") # if not already installed
devtools::install_github("alaiam/SSMtoAtlantis")
