###################################################################################
# author: Matthias Siewert
# matthias.siewert@umu.se
########
###
# This script is to extract rodent impact from the UAV at the location of ground plots
# then combines this with ground inventory
###

# Load libraries
library(raster)
library(tidyverse)
library(tidyr)

#######################################################
## calculate GPP and biomass following the same method as described in Siewert & Olofsson (2020):

# get peak season NDVI
Rastlist <- list.files("Raster/4_Cropped/", full.names=F, recursive =T)
Rastlist

#For GPP
for (i in Rastlist){
  system(paste0("gdal_calc.py -A Raster/4_Cropped/" , i ,
                " --outfile=Raster/9_GPPBiomass/", i, "_GPP.tif --A_band=1",
                " --allBands=A --overwrite --type='Float32' --NoDataValue='-3.4e+38' ",
                "--calc='0.205564*numpy.exp(4.861*A)'"))}

#For biomass
for (i in Rastlist){
  system(paste0("gdal_calc.py -A Raster/4_Cropped/" , i ,
                " --outfile=Raster/9_GPPBiomass/", i, "_BIO.tif --A_band=1",
                " --allBands=A --overwrite --type='Float32' --NoDataValue='-3.4e+38' ",
                "--calc='4.32754*numpy.exp(6.421*A)'"))}

# extract GPP and biomass mean values
Rastlist <- list.files("Raster/9_GPPBiomass", full.names=T, recursive =T)
Rastlist
templist <- list()

Rastlist2 <-  Rastlist[grepl("NF", Rastlist)]
for (i in Rastlist2) {
  temp <- mask(crop(raster(i),NFarea),NFarea)
  temp2 <- mask(temp, NFlccDis[NFlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  templist[[i]] <- cellStats(temp2, 'mean')}

Rastlist2 <-  Rastlist[grepl("NT", Rastlist)]
for (i in Rastlist2) {
  temp <- mask(crop(raster(i),NTarea),NTarea)
  # no water
  templist[[i]] <- cellStats(temp, 'mean')}

Rastlist2 <-  Rastlist[grepl("VJ", Rastlist)]
for (i in Rastlist2) {
  temp <- mask(crop(raster(i),VJarea),VJarea)
  temp2 <- mask(temp, VJlccDis[VJlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  templist[[i]] <- cellStats(temp2, 'mean')}

Rastlist2 <-  Rastlist[grepl("KJ", Rastlist)]
for (i in Rastlist2) {
  temp <- mask(crop(raster(i),KJarea),KJarea)
  temp2 <- mask(temp, KJlccDis[KJlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  templist[[i]] <- cellStats(temp2, 'mean')}

templist

templist <- unlist(templist, recursive = F)
templist <- as.data.frame(templist)
#templist <- subset(templist, !is.na(templist))
templist$area <-  substr(rownames(templist),33,34)
templist$year <-  substr(rownames(templist),22,25)

#dcast()
templist$BIOGPP <-  substr(rownames(templist),(nchar(rownames(templist))+1)-7,nchar(rownames(templist))-4)

temp <- templist %>%
  #filter(area == "NF") %>%
  pivot_wider(names_from =  "BIOGPP", values_from = "templist")

temp <- as.data.frame(pivot_wider(temp, names_from =  "year", values_from = c("BIO", "GPP")))

# add to summary table
SummaryTable <- merge (SummaryTable, temp, by = "area")
# add % change
SummaryTable$BIOchange <- SummaryTable$BIO_2019 /SummaryTable$BIO_2018 *100 -100

##############
# View
SummaryTable

#SummaryTablebackup <- SummaryTable
