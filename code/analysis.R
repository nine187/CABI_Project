rm(list=ls())
graphics.off()

#package for loading the github functions
library(remotes)

#function for dealing with CLIMEX data
#remotes::install_github("nowosad/ffipm") 

#download the package for dealing with CLIMEX data
library(ffipm)

#package for mapping the output
library(tmap)

#define the path for the files

path <- "C:/Users/Pasith/Documents/Dymex/Aflatoxin/"
irr_annual <- paste0(path, "NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
mod_irr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Annual.nc")
mod_irr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_Irr_Weekly.nc")
mod_noirr_annual <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Annual.nc")
mod_noirr_week <- paste0(path, "NetCDF/A.flavus_CM30_1995H_V2_modified-param-file_NoIrr_Weekly.nc")

#extract the data from the NetCDF file using the extract_data_list function
AllYears <- extract_data_list(irr_annual,"CS",n = 1, years = 2000, step = "Year")

#create a raster stack from the previous output
AllYears_raster <- create_raster_stack(AllYears,years = 2000, step = "Year")

tmap_mode("plot")


#alternative approach#
library(ncdf4)
nc <- nc_open("Dymex/Aflatoxin/NetCDF/A.flavus_modified-param-file_Irr_Annual_1970-2019.nc")
var <- ncvar_get(nc, "CS")