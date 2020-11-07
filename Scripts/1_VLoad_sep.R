######
#author: Matthias Siewert
# matthias.siewert@umu.se
########
## 2019-11-23
###
# This script is to collect drone flight raster maps for further timeseries analysis
# For Vole imagery
###

######### GIS libraries
library(raster)  # raster data
#library(rgdal)  # input/output, projections
#library(maptools)# autoloads sp
#library(sp)  # vector data
#library(rasterVis)
#library(snow)
#library(fs) # system file manipulation

#library(ggplot2)


##### list of all sites
ABDsites <- c("NF", "NT", "VJ", "KJ")
ABDsites

### Find all the .tif folder of NDVI
getwd()
temp <- list.files("~/Data2/DronesLocal/Flights/Processed/2017/2017_Seq/.",pattern = "index_ndvi.tif$", full.names=T, recursive =T)
temp2 <- list.files("~/Process/Flights/2018_Seq/.",pattern = "index_ndvi.tif$", full.names=T, recursive =T)
temp3 <- list.files("~/Process/Flights/2019_Seq/.",pattern = "index_ndvi.tif$", full.names=T, recursive =T)

temp <- c(temp, temp2, temp3)

for (i in ABDsites){
  assign(paste("a",i,"list", sep=""),temp[grepl(i, temp)])
}

aNFlist <- aNFlist[grep("17-06-10|17-08-02|17-09-12|18-06-06|18-07-30|18-09-09|19-06-12|19-07-27|19-09-08", aNFlist)]   
aNTlist <- aNTlist[grep("17-06-19|17-08-02|17-09-12|18-06-07|18-07-28|18-09-09|19-06-11|19-07-31_NT_seq_276|19-09-08", aNTlist)]   
aVJlist <- aVJlist[grep("17-06-11|17-08-03|17-09-11|18-06-12|2018-07-29_VJ_seq_msp/|18-09-11|19-06-13|19-08-03_VJ_seq_msp|19-09-10", aVJlist)]   
aKJlist <- aKJlist[grep("17-06-11|17-08-03|17-09-11|18-06-17|18-07-27|18-09-09|19-06-13|19-08-04_KJ_seq_msp|19-09-10", aKJlist)]   


print(aNFlist)
print(aNTlist)
print(aVJlist)
print(aKJlist)

file.copy(aNFlist, "Raster/NDVI/1_Org/NF")
file.copy(aNTlist, "Raster/NDVI/1_Org/NT")
file.copy(aVJlist, "Raster/NDVI/1_Org/VJ")
file.copy(aKJlist, "Raster/NDVI/1_Org/KJ")

bNFrast <- list.files("Raster/NDVI/1_Org/NF",pattern = ".tif$", full.names=T, recursive =T)
bNTrast <- list.files("Raster/NDVI/1_Org/NT",pattern = ".tif$", full.names=T, recursive =T)
bVJrast <- list.files("Raster/NDVI/1_Org/VJ",pattern = ".tif$", full.names=T, recursive =T)
bKJrast <- list.files("Raster/NDVI/1_Org/KJ",pattern = ".tif$", full.names=T, recursive =T)


#Create a lists of raster objects for each site
bNFrast <- sapply(bNFrast, function(filename) {filename = raster(filename);return(filename)})
bNTrast <- sapply(bNTrast, function(filename) {filename = raster(filename);return(filename)})
bVJrast <- sapply(bVJrast, function(filename) {filename = raster(filename);return(filename)})
bKJrast <- sapply(bKJrast, function(filename) {filename = raster(filename);return(filename)})

# Subset to flights we want!
bNFrast <- bNFrast[grep("18-07-30|19-07-27", names(bNFrast))]   
bNTrast <- bNTrast[grep("18-07-28|2019-07-31_NT_seq_276", names(bNTrast))]   
bVJrast <- bVJrast[grep("2018-07-29|2019-08-03", names(bVJrast))]   
bKJrast <- bKJrast[grep("2018-07-27|2019-08-04", names(bKJrast))]   


#test if it worked
plot(bNFrast[[1]])
inMemory(bNFrast[[1]])


# print resolutions of product
for (i in 1:length(bNFrast)) print(res(bNFrast[[i]]))
for (i in 1:length(bNTrast)) print(res(bNTrast[[i]]))
for (i in 1:length(bVJrast)) print(res(bVJrast[[i]]))
for (i in 1:length(bKJrast)) print(res(bKJrast[[i]]))

################################
plot(bNFrast[[2]])

i=1
########### Replace no value data with snow default value
for(i in 1:length(bNFrast)) {
  temp <- bNFrast[[i]]
  temp[is.na(temp[])] <- -0.042
  writeRaster(temp, paste0("Raster/NDVI/2_Snow/NF/", names(bNFrast[[i]]),"_snow.tif"),overwrite=T)}

for(i in 1:length(bNTrast)) {
  temp <- bNTrast[[i]]
  temp[is.na(temp[])] <- -0.042
  writeRaster(temp, paste0("Raster/NDVI/2_Snow/NT/", names(bNTrast[[i]]),"_snow.tif"),overwrite=T)}

for(i in 1:length(bVJrast)) {
  temp <- bVJrast[[i]]
  temp[is.na(temp[])] <- -0.042
  writeRaster(temp, paste0("Raster/NDVI/2_Snow/VJ/", names(bVJrast[[i]]),"_snow.tif"),overwrite=T)}

for(i in 1:length(bKJrast)) {
  temp <- bKJrast[[i]]
  temp[is.na(temp[])] <- -0.042
  writeRaster(temp, paste0("Raster/NDVI/2_Snow/KJ/", names(bKJrast[[i]]),"_snow.tif"),overwrite=T)}


### double check the following:
# Resample to 2018 into 2019 extent

NFrast <- list.files("Raster/NDVI/2_Snow/NF/",pattern = "_snow.tif$", full.names=F, recursive =T)
NTrast <- list.files("Raster/NDVI/2_Snow/NT/",pattern = "_snow.tif$", full.names=F, recursive =T)
VJrast <- list.files("Raster/NDVI/2_Snow/VJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
KJrast <- list.files("Raster/NDVI/2_Snow/KJ/",pattern = "_snow.tif$", full.names=F, recursive =T)

# Resample peak season to 2019
beginCluster()
raster::resample(raster(paste0('Raster/NDVI/2_Snow/NF/',NFrast[[1]])), raster(paste0('Raster/NDVI/2_Snow/NF/', NFrast[[2]])),method="ngb", paste0('Raster/NDVI/3_Resamp/NF/',NFrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/NDVI/2_Snow/NT/',NTrast[[1]])), raster(paste0('Raster/NDVI/2_Snow/NT/', NTrast[[2]])),method="ngb", paste0('Raster/NDVI/3_Resamp/NT/',NTrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/NDVI/2_Snow/VJ/',VJrast[[1]])), raster(paste0('Raster/NDVI/2_Snow/VJ/', VJrast[[2]])),method="ngb", paste0('Raster/NDVI/3_Resamp/VJ/',VJrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
raster::resample(raster(paste0('Raster/NDVI/2_Snow/KJ/',KJrast[[1]])), raster(paste0('Raster/NDVI/2_Snow/KJ/', KJrast[[2]])),method="ngb", paste0('Raster/NDVI/3_Resamp/KJ/',KJrast[[1]],'2019_resamp.tif'),overwrite=TRUE)
endCluster()


# # move the snow corrected files
# bNFrast <- list.files("NDVI/NF/",pattern = "_snow.tif$", full.names=F, recursive =T)
# for (i in bNFrast) {file_move(paste0('NDVI/NF/',i),paste0('NDVI/NFsnow/',i))}
# 
# bNTrast <- list.files("NDVI/NT/",pattern = "_snow.tif$", full.names=F, recursive =T)
# for (i in bNTrast) {file_move(paste0('NDVI/NT/',i),paste0('NDVI/NTsnow/',i))}
# 
# bVJrast <- list.files("NDVI/VJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
# for (i in bVJrast) {file_move(paste0('NDVI/VJ/',i),paste0('NDVI/VJsnow/',i))}
# 
# bKJrast <- list.files("NDVI/KJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
# for (i in bKJrast) {file_move(paste0('NDVI/KJ/',i),paste0('NDVI/KJsnow/',i))}

# Load vector  data
NFarea <-readOGR(dsn="/home/masi/Data/Drones/GIS_data/2017_Abisko_mission_areas/2017_Abisko_mission_areas_shape/Nissunjokka_forest_mission_area2017.shp")
NTarea <-readOGR(dsn="/home/masi/Data/Drones/GIS_data/2017_Abisko_mission_areas/2017_Abisko_mission_areas_shape/Nissunjokka_tundra_mission_area2017.shp")
VJarea <-readOGR(dsn="/home/masi/Data/Drones/GIS_data/2017_Abisko_mission_areas/2017_Abisko_mission_areas_shape/Vassijaure_mission_area2017.shp")
KJarea <-readOGR(dsn="/home/masi/Data/Drones/GIS_data/2017_Abisko_mission_areas/2017_Abisko_mission_areas_shape/Katterjokk_mission_area2017.shp")

temp <- crs(raster('Raster/NDVI/1_Org/NF/2017-06-10_NF_seq_msp_index_ndvi.tif'))

# reproject from wgas 84 to UTM 34 (using info from raster stack)
NFarea <- spTransform(NFarea, temp)#CRS("+init=epsg:32634"))
NTarea <- spTransform(NTarea, temp)#CRS("+init=epsg:32634"))
VJarea <- spTransform(VJarea, temp)#CRS("+init=epsg:32634"))
KJarea <- spTransform(KJarea, temp)#CRS("+init=epsg:32634"))


#### Cropping to core areas 
NFrast <- list.files("Raster/NDVI/2_Snow/NF/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in NFrast) {writeRaster(mask(crop(raster(paste0('Raster/NDVI/2_Snow/NF/', i)),NFarea), NFarea), paste0('Raster/NDVI/3_SnowCrp/NF/',i,'crp.tif'),overwrite =T)}

NTrast <- list.files("Raster/NDVI/2_Snow/NT/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in NTrast) {writeRaster(mask(crop(raster(paste0('Raster/NDVI/2_Snow/NT/', i)),NTarea), NTarea), paste0('Raster/NDVI/3_SnowCrp/NT/',i,'crp.tif'),overwrite =T)}

VJrast <- list.files("Raster/NDVI/2_Snow/VJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in VJrast) {writeRaster(mask(crop(raster(paste0('Raster/NDVI/2_Snow/VJ/', i)),VJarea), VJarea), paste0('Raster/NDVI/3_SnowCrp/VJ/',i,'crp.tif'),overwrite =T)}

KJrast <- list.files("Raster/NDVI/2_Snow/KJ/",pattern = "_snow.tif$", full.names=F, recursive =T)
for (i in KJrast) {writeRaster(mask(crop(raster(paste0('Raster/NDVI/2_Snow/KJ/', i)),KJarea), KJarea), paste0('Raster/NDVI/3_SnowCrp/KJ/',i,'crp.tif'),overwrite =T)}



