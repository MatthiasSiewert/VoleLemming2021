###################################################################################
# author: Matthias Siewert
# matthias.siewert@umu.se
########
###
# This script is to collect drone flight raster maps for further analysis
###

# Load libraries

######### GIS libraries
library(raster)  # raster data
library(rgdal)  # input/output, projections


# Check if all the files are present:
Rastlist <- list.files("Raster/2_NDVI/",pattern = ".tif$", full.names=T, recursive =T)
Rastlist


# Resample peak season 2018 into 2019 extent
NFrast <- list.files("Raster/2_NDVI/NF/",pattern = ".tif$", full.names=F, recursive =T)
NTrast <- list.files("Raster/2_NDVI/NT/",pattern = ".tif$", full.names=F, recursive =T)
VJrast <- list.files("Raster/2_NDVI/VJ/",pattern = ".tif$", full.names=F, recursive =T)
KJrast <- list.files("Raster/2_NDVI/KJ/",pattern = ".tif$", full.names=F, recursive =T)

beginCluster()
raster::resample(raster(paste0('Raster/2_NDVI/NF/',NFrast[[1]])), raster(paste0('Raster/2_NDVI/NF/', NFrast[[2]])),method="ngb", paste0('Raster/3_Resamp/',NFrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/2_NDVI/NT/',NTrast[[1]])), raster(paste0('Raster/2_NDVI/NT/', NTrast[[2]])),method="ngb", paste0('Raster/3_Resamp/',NTrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/2_NDVI/VJ/',VJrast[[1]])), raster(paste0('Raster/2_NDVI/VJ/', VJrast[[2]])),method="ngb", paste0('Raster/3_Resamp/',VJrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/2_NDVI/KJ/',KJrast[[1]])), raster(paste0('Raster/2_NDVI/KJ/', KJrast[[2]])),method="ngb", paste0('Raster/3_Resamp/',KJrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
endCluster()

# Load vector data
NFarea <-readOGR(dsn="Vector/AreaOutlines/NFarea.shp")
NTarea <-readOGR(dsn="Vector/AreaOutlines/NTarea.shp")
VJarea <-readOGR(dsn="Vector/AreaOutlines/VJarea.shp")
KJarea <-readOGR(dsn="Vector/AreaOutlines/KJarea.shp")


#### Crop original to core areas 
NFrast <- list.files("Raster/2_NDVI/NF/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in NFrast) {writeRaster(mask(crop(raster(paste0('Raster/2_NDVI/NF/', i)),NFarea), NFarea), paste0('Raster/4_Cropped/',i,'crp.tif'),overwrite =T)}

NTrast <- list.files("Raster/2_NDVI/NT/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in NTrast) {writeRaster(mask(crop(raster(paste0('Raster/2_NDVI/NT/', i)),NTarea), NTarea), paste0('Raster/4_Cropped/',i,'crp.tif'),overwrite =T)}

VJrast <- list.files("Raster/2_NDVI/VJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in VJrast) {writeRaster(mask(crop(raster(paste0('Raster/2_NDVI/VJ/', i)),VJarea), VJarea), paste0('Raster/4_Cropped/',i,'crp.tif'),overwrite =T)}

KJrast <- list.files("Raster/2_NDVI/KJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in KJrast) {writeRaster(mask(crop(raster(paste0('Raster/2_NDVI/KJ/', i)),KJarea), KJarea), paste0('Raster/4_Cropped/',i,'crp.tif'),overwrite =T)}

