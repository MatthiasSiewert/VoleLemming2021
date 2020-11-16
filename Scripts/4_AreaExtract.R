###################################################################################
# author: Matthias Siewert
# matthias.siewert@umu.se
########
## 2020-07-02
###
# This script is to extract detected rodent impact per habitat
# provides figure 4 and gathers data for Table 1. 
###

# Load libraries
library(tidyverse)
library(rgdal)
library(sf) # Simple Features for R
library(raster)
library(exactextractr) 
library(ggplot2)
library(cowplot)

beginCluster()


options(scipen=999)

#################################################################

####   This script extracts measures relevant for the entire study areas

# Mean NDVI
# Mean Damage
# Spatial autocorrelation of damage

#################################################################

###################################################################33
## Load land covers
# load land cover classification:
NFlccDisCrp <- readOGR("Vector/LCCcrop/NFlccDisCrp.shp")
NTlccDisCrp <- readOGR("Vector/LCCcrop/NTlccDisCrp.shp")
VJlccDisCrp <- readOGR("Vector/LCCcrop/VJlccDisCrp.shp")
KJlccDisCrp <- readOGR("Vector/LCCcrop/KJlccDisCrp.shp")
##############################################3
#### NDVI

# List all files generated in script 1VLoad_sep
RastList <- list.files("Raster/2_NDVI/", full.names=T, recursive =T)
## Subset for summer peak season flights only:
RastList <- RastList[grep("18.07.27|2018.07.28|2018.07.29|18.07.30|19.07.27|19.07.31|19.08.03|19.08.04", RastList)]    #, names(RastList))]   
RastList



# Mask water from NDVI images
temp <-  RastList[grepl("NF", RastList)]
for (i in (temp)) {
  temp2 <- mask(raster(i), NFlccDis[NFlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  writeRaster(temp2, paste0('Raster/4_CrpMasked/',names(temp2),'crp.tif'),overwrite =T)}

#### No water in NT - > skip
temp <-  RastList[grepl("NT", RastList)]
for (i in (temp)) {
  file.copy(i,'Raster/4_CrpMasked' )}


temp <-  RastList[grepl("VJ", RastList)]
for (i in (temp)) {
  temp2 <- mask(raster(i), VJlccDis[VJlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  writeRaster(temp2, paste0('Raster/4_CrpMasked/',names(temp2),'crp.tif'),overwrite =T)}

temp <-  RastList[grepl("KJ", RastList)]
for (i in (temp)) {
  temp2 <- mask(raster(i), KJlccDis[KJlccDis$class %in% c("water"),], inverse =T, updatevalue = NA)
  writeRaster(temp2, paste0('Raster/4_CrpMasked/',names(temp2),'crp.tif'),overwrite =T)}



# use the masked files to generate a table
RastList <- list.files("Raster/4_CrpMasked", full.names=T, recursive =T)
RastList

tempTable <- as.data.frame(RastList)

for (i in 1:length(RastList)) {
  temp <- mean(values(raster(RastList[i])), na.rm =T)
  print(temp)
  tempTable$meanNDVI[i] <- temp
  temp <- sd(values(raster(RastList[i])), na.rm =T)
  print(temp)
  tempTable$sdNDVI[i] <- temp}

SummaryTable <- tempTable

# Define nice date and area column plus cleanup
SummaryTable$area <- substr(SummaryTable$RastList, 32, 33) # add area column
SummaryTable$date <- as.Date(substr(SummaryTable$RastList, 21, 30),"%Y.%m.%d")  # add date
SummaryTable$year <- substr(SummaryTable$RastList, 21, 24)  # add date
SummaryTable$RastList <- NULL  # remove file names
SummaryTable <- transform(SummaryTable,area=factor(area,levels=c("NF","NT","VJ","KJ"))) # sort factors
SummaryTable <- SummaryTable[,c(3,5,1,2)]  # reorder 

SummaryTable <- SummaryTable %>% pivot_wider(names_from = year, values_from = c(meanNDVI,sdNDVI))

# change from 2018 to 2019
SummaryTable$dif20182019 <-  SummaryTable$meanNDVI_2019 -SummaryTable$meanNDVI_2018
SummaryTable

####################################################################################################
# Affected area

NF2018minus19damBin

#### Cropping to core areas 
NF2018minus19damBinBestCrp <- writeRaster(mask(crop(NF2018minus19damBinBest, NFarea),NFarea), 'Raster/8_ImpactCutoffCrp/NFareaNF2018minus19damBinBestCrp.tif',overwrite =T)
NF2018minus19damBinMinCrp  <- writeRaster(mask(crop(NF2018minus19damBinMin,  NFarea),NFarea), 'Raster/8_ImpactCutoffCrp/NFareaNF2018minus19damBinMinCrp.tif',overwrite =T)
NF2018minus19damBinMaxCrp  <- writeRaster(mask(crop(NF2018minus19damBinMax,  NFarea),NFarea), 'Raster/8_ImpactCutoffCrp/NFareaNF2018minus19damBinMaxCrp.tif',overwrite =T)


NT2018minus19damBinBestCrp <- writeRaster(mask(crop(NT2018minus19damBinBest, NTarea),NTarea), 'Raster/8_ImpactCutoffCrp/NTareaNT2018minus19damBinBestCrp.tif',overwrite =T)
NT2018minus19damBinMinCrp  <- writeRaster(mask(crop(NT2018minus19damBinMin,  NTarea),NTarea), 'Raster/8_ImpactCutoffCrp/NTareaNT2018minus19damBinMinCrp.tif',overwrite =T)
NT2018minus19damBinMaxCrp  <- writeRaster(mask(crop(NT2018minus19damBinMax,  NTarea),NTarea), 'Raster/8_ImpactCutoffCrp/NTareaNT2018minus19damBinMaxCrp.tif',overwrite =T)

VJ2018minus19damBinBestCrp <- writeRaster(mask(crop(VJ2018minus19damBinBest, VJarea),VJarea), 'Raster/8_ImpactCutoffCrp/VJareaVJ2018minus19damBinBestCrp.tif',overwrite =T)
VJ2018minus19damBinMinCrp  <- writeRaster(mask(crop(VJ2018minus19damBinMin,  VJarea),VJarea), 'Raster/8_ImpactCutoffCrp/VJareaVJ2018minus19damBinMinCrp.tif',overwrite =T)
VJ2018minus19damBinMaxCrp  <- writeRaster(mask(crop(VJ2018minus19damBinMax,  VJarea),VJarea), 'Raster/8_ImpactCutoffCrp/VJareaVJ2018minus19damBinMaxCrp.tif',overwrite =T)

KJ2018minus19damBinBestCrp <- writeRaster(mask(crop(KJ2018minus19damBinBest, KJarea),KJarea), 'Raster/8_ImpactCutoffCrp/KJareaKJ2018minus19damBinBestCrp.tif',overwrite =T)
KJ2018minus19damBinMinCrp  <- writeRaster(mask(crop(KJ2018minus19damBinMin,  KJarea),KJarea), 'Raster/8_ImpactCutoffCrp/KJareaKJ2018minus19damBinMinCrp.tif',overwrite =T)
KJ2018minus19damBinMaxCrp  <- writeRaster(mask(crop(KJ2018minus19damBinMax,  KJarea),KJarea), 'Raster/8_ImpactCutoffCrp/KJareaKJ2018minus19damBinMaxCrp.tif',overwrite =T)

########### Impact counts 
# Get the count of values best case scenario
impactExt <-  freq(NF2018minus19damBinBestCrp, useNA = 'no')
impactExt <- as.data.frame(impactExt)
impactExt <- rbind(impactExt,as.data.frame(freq(NT2018minus19damBinBestCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(VJ2018minus19damBinBestCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(KJ2018minus19damBinBestCrp, useNA = 'no')))
impactExt$area <- c('NF', 'NF', 'NT', 'NT', 'VJ', 'VJ', 'KJ', 'KJ')
impactExt

impactExtBest <- impactExt %>% 
  group_by(area) %>% 
  mutate(imp_percBest=count/sum(count)*100)

# Get the count of values Min scenario
impactExt <-  freq(NF2018minus19damBinMinCrp, useNA = 'no')
impactExt <- as.data.frame(impactExt)
impactExt <- rbind(impactExt,as.data.frame(freq(NT2018minus19damBinMinCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(VJ2018minus19damBinMinCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(KJ2018minus19damBinMinCrp, useNA = 'no')))
impactExt$area <- c('NF', 'NF', 'NT', 'NT', 'VJ', 'VJ', 'KJ', 'KJ')
impactExt

impactExtMin <- impactExt %>% 
  group_by(area) %>% 
  mutate(imp_percMin=count/sum(count)*100)

# Get the count of values Max scenario
impactExt <-  freq(NF2018minus19damBinMaxCrp, useNA = 'no')
impactExt <- as.data.frame(impactExt)
impactExt <- rbind(impactExt,as.data.frame(freq(NT2018minus19damBinMaxCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(VJ2018minus19damBinMaxCrp, useNA = 'no')))
impactExt <- rbind(impactExt,as.data.frame(freq(KJ2018minus19damBinMaxCrp, useNA = 'no')))
impactExt$area <- c('NF', 'NF', 'NT', 'NT', 'VJ', 'VJ', 'KJ', 'KJ')
impactExt

impactExtMax <- impactExt %>% 
  group_by(area) %>% 
  mutate(imp_percMax=count/sum(count)*100)

SummaryTable <- merge (SummaryTable, subset(impactExtBest, value ==1), by = 'area')
SummaryTable <- merge (SummaryTable, subset(impactExtMin, value ==1), by = 'area')
SummaryTable <- merge (SummaryTable, subset(impactExtMax, value ==1), by = 'area')


################# Spatial autocorrelation ##############
# calculate Moran's I as an indicator of spatial autocorrelation

# takes forever
MoranTable <-       as.data.frame(cbind( 'NF',Moran(NF2018minus19damBinBestCrp)))
MoranTable <- rbind(as.data.frame(cbind( 'NT',Moran(NT2018minus19damBinBestCrp))), MoranTable)
MoranTable <- rbind(as.data.frame(cbind( 'VJ',Moran(VJ2018minus19damBinBestCrp))), MoranTable)
MoranTable <- rbind(as.data.frame(cbind( 'KJ',Moran(KJ2018minus19damBinBestCrp))), MoranTable)

# Merge into summary table:
colnames(MoranTable) <- c('area', 'moran')
SummaryTable <- merge (SummaryTable, MoranTable, by = 'area')

SummaryTable

######################################33
## add area to summary table:

SummaryTable$TotalAream2 <- rbind(sum(NFlccDisCrp$aream2),
                                  sum(NTlccDisCrp$aream2),
                                  sum(VJlccDisCrp$aream2),
                                  sum(KJlccDisCrp$aream2))

SummaryTable

###################################################################33
## extract by land cover
# load land cover classification:
NFlccDisCrp <- read_sf("~/Data/Drones/LCC/Data/AllAreas/NFlccDisCrp.shp")
NTlccDisCrp <- read_sf("~/Data/Drones/LCC/Data/AllAreas/NTlccDisCrp.shp")
VJlccDisCrp <- read_sf("~/Data/Drones/LCC/Data/AllAreas/VJlccDisCrp.shp")
KJlccDisCrp <- read_sf("~/Data/Drones/LCC/Data/AllAreas/KJlccDisCrp.shp")

# extract fraction of damage affected areas per class:
# use package 'exactextractr' for precise extraction

# for best estimate
# transfer count into m2 by multiplying with the squared resolution res(NF2018minus19damBinBestCrp)[1] ^ 2
NFlccDisCrp <- NFlccDisCrp %>% add_column(damFractBest = exact_extract(NF2018minus19damBinBestCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
NTlccDisCrp <- NTlccDisCrp %>% add_column(damFractBest = exact_extract(NT2018minus19damBinBestCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
VJlccDisCrp <- VJlccDisCrp %>% add_column(damFractBest = exact_extract(VJ2018minus19damBinBestCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
KJlccDisCrp <- KJlccDisCrp %>% add_column(damFractBest = exact_extract(KJ2018minus19damBinBestCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))

NFlccDisCrp <- NFlccDisCrp %>% add_column(damCountBest = exact_extract(NF2018minus19damBinBestCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NF2018minus19damBinBestCrp)[1] ^2) )
NTlccDisCrp <- NTlccDisCrp %>% add_column(damCountBest = exact_extract(NT2018minus19damBinBestCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NT2018minus19damBinBestCrp)[1] ^2) )
VJlccDisCrp <- VJlccDisCrp %>% add_column(damCountBest = exact_extract(VJ2018minus19damBinBestCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(VJ2018minus19damBinBestCrp)[1] ^2) )
KJlccDisCrp <- KJlccDisCrp %>% add_column(damCountBest = exact_extract(KJ2018minus19damBinBestCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(KJ2018minus19damBinBestCrp)[1] ^2) )

# for min estimate
NFlccDisCrp <- NFlccDisCrp %>% add_column(damFractMin = exact_extract(NF2018minus19damBinMinCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
NTlccDisCrp <- NTlccDisCrp %>% add_column(damFractMin = exact_extract(NT2018minus19damBinMinCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
VJlccDisCrp <- VJlccDisCrp %>% add_column(damFractMin = exact_extract(VJ2018minus19damBinMinCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
KJlccDisCrp <- KJlccDisCrp %>% add_column(damFractMin = exact_extract(KJ2018minus19damBinMinCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))


NFlccDisCrp <- NFlccDisCrp %>% add_column(damCountMin = exact_extract(NF2018minus19damBinMinCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NF2018minus19damBinBestCrp)[1] ^2) )
NTlccDisCrp <- NTlccDisCrp %>% add_column(damCountMin = exact_extract(NT2018minus19damBinMinCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NT2018minus19damBinBestCrp)[1] ^2) )
VJlccDisCrp <- VJlccDisCrp %>% add_column(damCountMin = exact_extract(VJ2018minus19damBinMinCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(VJ2018minus19damBinBestCrp)[1] ^2) )
KJlccDisCrp <- KJlccDisCrp %>% add_column(damCountMin = exact_extract(KJ2018minus19damBinMinCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(KJ2018minus19damBinBestCrp)[1] ^2) )


# for Max estimate
NFlccDisCrp <- NFlccDisCrp %>% add_column(damFractMax = exact_extract(NF2018minus19damBinMaxCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
NTlccDisCrp <- NTlccDisCrp %>% add_column(damFractMax = exact_extract(NT2018minus19damBinMaxCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
VJlccDisCrp <- VJlccDisCrp %>% add_column(damFractMax = exact_extract(VJ2018minus19damBinMaxCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))
KJlccDisCrp <- KJlccDisCrp %>% add_column(damFractMax = exact_extract(KJ2018minus19damBinMaxCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))

NFlccDisCrp <- NFlccDisCrp %>% add_column(damCountMax = exact_extract(NF2018minus19damBinMaxCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NF2018minus19damBinBestCrp)[1] ^2) )
NTlccDisCrp <- NTlccDisCrp %>% add_column(damCountMax = exact_extract(NT2018minus19damBinMaxCrp,NTlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(NT2018minus19damBinBestCrp)[1] ^2) )
VJlccDisCrp <- VJlccDisCrp %>% add_column(damCountMax = exact_extract(VJ2018minus19damBinMaxCrp,VJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(VJ2018minus19damBinBestCrp)[1] ^2) )
KJlccDisCrp <- KJlccDisCrp %>% add_column(damCountMax = exact_extract(KJ2018minus19damBinMaxCrp,KJlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T)}) * (res(KJ2018minus19damBinBestCrp)[1] ^2) )

################## at total count per class abd 
#NFlccDisCrp %>% add_column(totalCover = exact_extract(NF2018minus19damBinBestCrp,NFlccDisCrp, function(value, coverage_fraction)  {sum(coverage_fraction[value == 1],na.rm = T) / sum(coverage_fraction,na.rm = T)*100}))


NFlccDisCrp$geometry <- NULL
NTlccDisCrp$geometry <- NULL
VJlccDisCrp$geometry <- NULL
KJlccDisCrp$geometry <- NULL


SummaryLCC <- rbind(NFlccDisCrp, NTlccDisCrp, VJlccDisCrp,KJlccDisCrp)
SummaryLCC

# tests:
sum(subset(NFlccDisCrp, area == "NF")$damCountBest)
freq(NF2018minus19damBinBestCrp)



##################################
## Plot of the relative damage
options(scipen=999)

# Relative damage for each land cover class
temp <- subset(SummaryLCC , class != "water") # remove water
temp <- transform(temp,area=factor(area,levels=c("NF","NT","VJ","KJ")))

unique(temp$class)

temp <- 
  transform(temp,class=factor(class,levels=c( "barren", "dry_heath","mesic_heath",
                                              "nana_heath", "snowbed", "willow",
                                              "birch_forest",
                                              "semiwetland", "gram_meadow","wetland")))


temp2 <- temp %>%
  group_by(area) %>%
  summarize(sum = sum(aream2))
temp <- merge(temp,temp2, by = "area")

temp$aream2perc <- temp$aream2 /temp$sum *100
temp$damFractTotBest <- temp$damCountBest /temp$sum *100
temp$damFractTotMin <- temp$damCountMin /temp$sum *100
temp$damFractTotMax <- temp$damCountMax /temp$sum *100


# add classes with 0 values that don't exist in specific area
temp[nrow(temp) + 1,] = c("KJ","birch_forest","0","0","0","0","0","0","0","0","0","0","0","0")
temp[nrow(temp) + 1,] = c("NF","snowbed",     "0","0","0","0","0","0","0","0","0","0","0","0")
temp[nrow(temp) + 1,] = c("VJ","snowbed",     "0","0","0","0","0","0","0","0","0","0","0","0")
temp[nrow(temp) + 1,] = c("NT", "wetland",    "0","0","0","0","0","0","0","0","0","0","0","0")

# order by class
temp <- temp[order(temp$class),]
temp

# change formats
cols = c(3:14) #c(4, 5,6,7,8,9);
temp[,cols] = apply(temp[,cols], 2, function(x) as.numeric(as.character(x)));
sapply(temp, typeof)
temp

# Colors same as in map
unique(temp$class)

temp2 <- c("#b1b1b1","#c9e2dc","#9eca50", #  "barren", "dry_heath","mesic_heath",
           "#58d4d4","#406f89","#7fc2ff", # "nana_heath", "snowbed", "willow"
           "#00a087", "#ebd248", # , "birch_forest", "semiwetland",
           "#dc0000","#ffff13") # "gram_meadow","wetland"

# # # damage in m2 per area and land cover fraction
# tempPlot1 <- ggplot(temp) +
#   geom_bar(aes(area,aream2,group = class, fill = class),stat="identity", position="dodge", alpha = 0.3, width=.9) +
#   geom_bar(aes(area,damCountBest,group = class, fill = class),stat="identity", position="dodge", width=.9) +
#   geom_errorbar(aes(x= area, ymin=damCountMin,ymax=damCountMax),stat="identity",position="dodge2",col = "grey30", width=.9) +
#   theme_classic() +
#   scale_fill_npg() +
#   labs(y= "Land cover & \n Rodent impact (m²)", x = "Continental   low –    high altitude          –                    Oceanic low    –    high altitude")
# #  scale_y_continuous(limits=c(0, 10000), breaks=c(0,20000, 40000, 60000, 80000))
# tempPlot1

# damage in % per area and land cover fraction
tempPlot1 <- ggplot(temp) +
  geom_bar(aes(area,aream2perc,group = class, fill = class),stat="identity", position="dodge", alpha = 0.3, width=.9) +
  geom_bar(aes(area,damFractTotBest,group = class, fill = class),stat="identity", position="dodge", width=.9) +
  geom_errorbar(aes(x= area, ymin=damFractTotMin,ymax=damFractTotMax),stat="identity",position="dodge2",col = "grey30", width=.9) +
  theme_classic() +
  scale_fill_manual(values = temp2) + #  scale_fill_npg() +
  labs(y= "Land cover class & rodent impact \n (% of areal coverage)", x = "Continental   low –    high altitude                            Oceanic low    –    high altitude") 
#  scale_y_continuous(limits=c(0, 10000), breaks=c(0,20000, 40000, 60000, 80000))
tempPlot1

# damage in per class per area and land cover fraction
tempPlot2 <- ggplot(temp) +
  # geom_bar(aes(area,class100,group = class, fill = class),stat="identity", position="dodge", alpha = 0.3, width=.9) +
  geom_bar(aes(area,damFractBest,group = class, fill = class),stat="identity", position="dodge", width=.9) +
  geom_errorbar(aes(x= area, ymin=damFractMin,ymax=damFractMax),stat="identity",position="dodge2",col = "grey30", width=.9) +
  theme_classic() +
  #  theme(legend.position="bottom") +
  #ylim(0,50) +
  scale_fill_manual(values = temp2) + #  scale_fill_npg() +
  labs(y= "Rodent impact \n (% of Land cover class)", x = "Continental   low –    high altitude                             Oceanic low    –    high altitude") 
tempPlot2

# combine both plots
Fig_habitat <- plot_grid(tempPlot1, tempPlot2, nrow = 2, align = "v", labels = c("a","b"))
Fig_habitat

# Save the figures
ggsave("Plots/Fig_4_Habitat/Fig_HabitatsPerc.pdf", Fig_habitat,  width = 25, height = 12, units = "cm")
ggsave("Plots/Fig_4_Habitat/Fig_HabitatsPerc.png", Fig_habitat,  width = 25, height = 12, units = "cm")

SummaryTablebackup <- SummaryTable
