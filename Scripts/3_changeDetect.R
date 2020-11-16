###################################################################################
# author: Matthias Siewert
# matthias.siewert@umu.se
########
## 2020-05-02
###
# Script for change detection. Detected reduction in NDVI reflects rodent impact on vegetation:
###

# Load libraries
# library(rgdal)
# library(doParallel)
# library(raster)
# library(snow)
library(ggplot2)
library(cowplot)
library(ggsci)

###############################################
# Load land cover classification:
NFlccDis <- readOGR("Vector/LCC/NFlccDis.shp")
NTlccDis <- readOGR("Vector/LCC/NTlccDis.shp")
VJlccDis <- readOGR("Vector/LCC/VJlccDis.shp")
KJlccDis <- readOGR("Vector/LCC/KJlccDis.shp")

####################################################

##### Copy the pairs in a folder for peak season
#NF
file.copy('Raster/2_NDVI/NF/X2019.07.27_NF_seq_266_msp_index_ndvi_snow.tif','Raster/5_PeakPairs/NF_ndvi_2019_____snow.tif', overwrite = T)
file.copy('Raster/3_Resamp/X2018.07.30_NF_seq_msp_index_ndvi_snow.tif2019_resamp.tif','Raster/5_PeakPairs/NF_ndvi_2018to19_snow.tif', overwrite = T)

#NT
file.copy('Raster/2_NDVI/NT/X2019.07.31_NT_seq_276_msp_index_ndvi_snow.tif','Raster/5_PeakPairs/NT_ndvi_2019_____snow.tif', overwrite = T)
file.copy('Raster/3_Resamp/X2018.07.28_NT_seq_msp_index_ndvi_snow.tif2019_resamp.tif','Raster/5_PeakPairs/NT_ndvi_2018to19_snow.tif', overwrite = T)

#VJ
file.copy('Raster/2_NDVI/VJ/X2019.08.03_VJ_seq_msp_index_ndvi_snow.tif','Raster/5_PeakPairs/VJ_ndvi_2019_____snow.tif', overwrite = T)
file.copy('Raster/3_Resamp/X2018.07.29_VJ_seq_msp_index_ndvi_snow.tif2019_resamp.tif','Raster/5_PeakPairs/VJ_ndvi_2018to19_snow.tif', overwrite = T)

#KJ
file.copy('Raster/2_NDVI/KJ/X2019.08.04_KJ_seq_msp_index_ndvi_snow.tif','Raster/5_PeakPairs/KJ_ndvi_2019_____snow.tif', overwrite = T)
file.copy('Raster/3_Resamp/X2018.07.27_KJ_seq_msp_index_ndvi_snow.tif2019_resamp.tif','Raster/5_PeakPairs/KJ_ndvi_2018to19_snow.tif', overwrite = T)


# Check resolutions, there should always be a pair
Rastlist <- list.files("Raster/5_PeakPairs",pattern = ".tif$", full.names=T, recursive =T)
Rastlist <- sapply(Rastlist, function(filename) {filename = raster(filename);return(filename)})
for (i in 1:length(Rastlist)) print(res(Rastlist[[i]]))

names(Rastlist)

##### Subtraction:
# Subtract 2018 and 2019 raster imagery 
NF2018minus19 <- overlay(Rastlist[[4]], Rastlist[[3]], fun=function(x,y){return(x-y)})
NF2018minus19 <- writeRaster(NF2018minus19, 'Raster/6_2018minus2019/NF2018minus19.tif', overwrite =T)

NT2018minus19 <- overlay(Rastlist[[6]], Rastlist[[5]], fun=function(x,y){return(x-y)})
NT2018minus19 <- writeRaster(NT2018minus19, 'Raster/6_2018minus2019/NT2018minus19.tif', overwrite =T)

VJ2018minus19 <- overlay(Rastlist[[8]], Rastlist[[7]], fun=function(x,y){return(x-y)})
VJ2018minus19 <- writeRaster(VJ2018minus19, 'Raster/6_2018minus2019/VJ2018minus19.tif', overwrite =T)

KJ2018minus19 <- overlay(Rastlist[[2]], Rastlist[[1]], fun=function(x,y){return(x-y)})
KJ2018minus19 <- writeRaster(KJ2018minus19, 'Raster/6_2018minus2019/KJ2018minus19.tif', overwrite =T)

#################################
# Change detection:
# Set a threshold to the NDVI as a mask.
# And mask individual land cover classes that are potential under- or overestimations.

############################# NF
#Define Threshold function best 0.07
NF2018minus19damBin <- NF2018minus19
fun <- function(x) {ifelse(x  > -0.08, 0,1)}
NF2018minus19damBin <- raster::calc(NF2018minus19damBin, fun)
# LCC exclude: water, barren
NF2018minus19damBin <- raster::mask(NF2018minus19damBin, NFlccDis[NFlccDis$class %in% c("water", "barren","gram_meadow"),], inverse =T, updatevalue = 0)
NF2018minus19damBinBest <- writeRaster(NF2018minus19damBin, 'Raster/7_ImpactCutoff/NF2018minus19damBinBest.tif',overwrite=TRUE)


#Define Threshold function Min 0.11
NF2018minus19damBin <- NF2018minus19
fun <- function(x) {ifelse(x  > -0.11, 0,1)}
NF2018minus19damBin <- calc(NF2018minus19damBin, fun)
# LCC exclude: water, barren, semiwetland, gram_meadow
NF2018minus19damBin <- raster::mask(NF2018minus19damBin, NFlccDis[NFlccDis$class %in% c("water", "barren","gram_meadow", "semiwetland"),], inverse =T, updatevalue = 0)
NF2018minus19damBinMin <- writeRaster(NF2018minus19damBin, 'Raster/7_ImpactCutoff/NF2018minus19damBinMin.tif',overwrite=TRUE)

#Define Threshold function 0.05
NF2018minus19damBin <- NF2018minus19
fun <- function(x) {ifelse(x  > -0.05, 0,1)}
NF2018minus19damBin <- calc(NF2018minus19damBin, fun)
# LCC exclude: water; barren
NF2018minus19damBin <- raster::mask(NF2018minus19damBin, NFlccDis[NFlccDis$class %in% c("water", "barren"),], inverse =T, updatevalue = 0)
NF2018minus19damBinMax <- writeRaster(NF2018minus19damBin, 'Raster/7_ImpactCutoff/NF2018minus19damBinMax.tif',overwrite=TRUE)


############################# NT
#Define Threshold function best 0.07
NT2018minus19damBin <- NT2018minus19
fun <- function(x) {ifelse(x  > -0.07, 0,1)}  #seems ok
NT2018minus19damBin <- calc(NT2018minus19damBin, fun)
# LCC exclude: water; Gram Meadow, barren
NT2018minus19damBin <- raster::mask(NT2018minus19damBin, NTlccDis[NTlccDis$class %in% c("water", "gram_meadow","barren"),], inverse =T, updatevalue = 0)
NT2018minus19damBinBest <- writeRaster(NT2018minus19damBin, 'Raster/7_ImpactCutoff/NT2018minus19damBinBest.tif',overwrite=TRUE)

#Define Threshold function max 0.04
NT2018minus19damBin <- NT2018minus19
fun <- function(x) {ifelse(x  > -0.04, 0,1)}  #seems ok
NT2018minus19damBin <- calc(NT2018minus19damBin, fun)
# LCC exclude: water;
NT2018minus19damBin <- raster::mask(NT2018minus19damBin, NTlccDis[NTlccDis$class %in% c("water"),], inverse =T, updatevalue = 0)
NT2018minus19damBinMax <- writeRaster(NT2018minus19damBin, 'Raster/7_ImpactCutoff/NT2018minus19damBinMax.tif',overwrite=TRUE)

#Define Threshold function min 0.09
NT2018minus19damBin <- NT2018minus19
fun <- function(x) {ifelse(x  > -0.09, 0,1)}  #seems ok
NT2018minus19damBin <- calc(NT2018minus19damBin, fun)
# LCC exclude: water; barren, dry_heath; Gram Meadow
NT2018minus19damBin <- raster::mask(NT2018minus19damBin, NTlccDis[NTlccDis$class %in% c("water", "gram_meadow", "dry_heath","barren"),], inverse =T, updatevalue = 0)
NT2018minus19damBinMin <- writeRaster(NT2018minus19damBin, 'Raster/7_ImpactCutoff/NT2018minus19damBinMin.tif',overwrite=TRUE)




############################# VJ best 0.8
VJ2018minus19damBin <- VJ2018minus19
fun <- function(x) {ifelse(x  > -0.07, 0,1)} # maybe even 0.8
VJ2018minus19damBin <- calc(VJ2018minus19damBin, fun)
# LCC exclude: water; barren; wetland; gram_meadow;
VJ2018minus19damBin <- raster::mask(VJ2018minus19damBin, VJlccDis[VJlccDis$class %in% c("water", "barren","wetland","gram_meadow"),], inverse =T, updatevalue = 0)
VJ2018minus19damBinBest <- writeRaster(VJ2018minus19damBin, 'Raster/7_ImpactCutoff/VJ2018minus19damBinBest.tif',overwrite=TRUE)


#Define Threshold function max 0.07
VJ2018minus19damBin <- VJ2018minus19
fun <- function(x) {ifelse(x  > -0.04, 0,1)} # maybe even 0.8
VJ2018minus19damBin <- calc(VJ2018minus19damBin, fun)
# LCC exclude: water; barren
VJ2018minus19damBin <- raster::mask(VJ2018minus19damBin, VJlccDis[VJlccDis$class %in% c("water", "barren"),], inverse =T, updatevalue = 0)
VJ2018minus19damBinMax <- writeRaster(VJ2018minus19damBin, 'Raster/7_ImpactCutoff/VJ2018minus19damBinMax.tif',overwrite=TRUE)

#Define Threshold function min 0.11
VJ2018minus19damBin <- VJ2018minus19
fun <- function(x) {ifelse(x  > -0.11, 0,1)} # maybe even 0.8
VJ2018minus19damBin <- calc(VJ2018minus19damBin, fun)
# LCC exclude: water; barren; dry Heath; wetland; gram_meadow
VJ2018minus19damBin <- raster::mask(VJ2018minus19damBin, VJlccDis[VJlccDis$class %in% c("water", "barren","dry tundra","wetland","gram_meadow"),], inverse =T, updatevalue = 0)
VJ2018minus19damBinMin <- writeRaster(VJ2018minus19damBin, 'Raster/7_ImpactCutoff/VJ2018minus19damBinMin.tif',overwrite=TRUE)


############################# KJ best 0.6
KJ2018minus19damBin <- KJ2018minus19
fun <- function(x) {ifelse(x  > -0.06, 0,1)} #
KJ2018minus19damBin <- calc(KJ2018minus19damBin, fun)
# LCC exclude: water; barren; gram_meadow
KJ2018minus19damBin <- raster::mask(KJ2018minus19damBin, KJlccDis[KJlccDis$class %in% c("water", "barren","gram_meadow"),], inverse =T, updatevalue = 0)
KJ2018minus19damBinBest <- writeRaster(KJ2018minus19damBin, 'Raster/7_ImpactCutoff/KJ2018minus19damBinBest.tif',overwrite=TRUE)

#Define Threshold function max 0.04
KJ2018minus19damBin <- KJ2018minus19
fun <- function(x) {ifelse(x  > -0.04, 0,1)} #
KJ2018minus19damBin <- calc(KJ2018minus19damBin, fun)
# LCC exclude: water; barren;
KJ2018minus19damBin <- raster::mask(KJ2018minus19damBin, KJlccDis[KJlccDis$class %in% c("water", "barren"),], inverse =T, updatevalue = 0)
KJ2018minus19damBinMax <- writeRaster(KJ2018minus19damBin, 'Raster/7_ImpactCutoff/KJ2018minus19damBinMax.tif',overwrite=TRUE)

#Define Threshold function min 0.08
KJ2018minus19damBin <- KJ2018minus19
fun <- function(x) {ifelse(x  > -0.08, 0,1)} #
KJ2018minus19damBin <- calc(KJ2018minus19damBin, fun)
# LCC exclude: water; barren; wetland; gram_meadow
KJ2018minus19damBin <- raster::mask(KJ2018minus19damBin, KJlccDis[KJlccDis$class %in% c("water", "barren","wetland","gram_meadow"),], inverse =T, updatevalue = 0)
KJ2018minus19damBinMin <- writeRaster(KJ2018minus19damBin, 'Raster/7_ImpactCutoff/KJ2018minus19damBinMin.tif',overwrite=TRUE)


###############################################################################################

## Load impact polygons
DamPlots <- readOGR("Vector/PlotData/RodentPlots_2019_polyg.gpkg", "DamagePlots_2019_polyg")
DamPlots
# Plot to see if they loaded 
# should be UTM 34 too
plot(NF2018minus19damBin)
plot(DamPlots, add = T)


DamRastList <- list(NF2018minus19damBinBest,NT2018minus19damBinBest,VJ2018minus19damBinBest,KJ2018minus19damBinBest,
                    NF2018minus19damBinMin,NT2018minus19damBinMin,VJ2018minus19damBinMin,KJ2018minus19damBinMin,
                    NF2018minus19damBinMax,NT2018minus19damBinMax,VJ2018minus19damBinMax,KJ2018minus19damBinMax)

DamRastList

DamList <- list()
endCluster()
for (i in 1:length(DamRastList)){
  temp <- raster::extract(DamRastList[[i]],DamPlots)
  names(temp) <- DamPlots$name
  name <- names(DamRastList[[i]])
  temp <- Filter(Negate(is.null), temp)
  DamList[[name]] <- temp
  print(name)
}

# Collapse into one main list
DamList <- unlist(DamList, recursive =F)

##################################################
# extract values

# caluclate sum:
templist <- list()
for (i in 1:length(DamList)){
  print(names(DamList[i]))
  temp <- sum(DamList[[i]]) / length(DamList[[i]]) * 100
  name <- names(DamList[i])
  templist[[name]]<- temp
}
templist

# create  tibble to work with
damPercTable <- tibble(names = names(unlist(templist)),
                       damPerc = unlist(templist))
damPercTable$names
damPercTable$area <- as.character(substr(damPercTable$names,1,2))

damPercTable$plot <- substr(damPercTable$names, nchar(damPercTable$names)-4, nchar(damPercTable$names))
damPercTable$type <- substr(damPercTable$names, nchar(damPercTable$names)-1, nchar(damPercTable$names)-1)

damPercTable


ggplot(damPercTable, aes(area, damPerc )) +
  geom_point( aes(color = type))
#  geom_bar( stat = "identity", aes(fill = type), position = position_dodge())


###############3
# Load ground control data
# use rules 3; 1 per rodent impact, max 1
sumTable <- read_csv('~/Data/Drones/Damage/DamageR/sumTablerules4_eachRodentcounts1nomaxbut3.csv')
sumTable$damPercField <-  sumTable$damCount / 400 *100
sumTable

temp2 <- merge(damPercTable,sumTable, by = 'plot')#, all.x = all)
temp <- as.data.frame(cbind(plot = damageTable$plot, habitat = as.character(damageTable$habitat)))
temp$habitat = factor(temp$habitat,levels = c("Dry heath & Barren",
                                              "Betula nana heath",
                                              "Mesic heath & Snowbed",
                                              "Wetlands & meadows",
                                              "Birch forest",
                                              "Willow"))
temp2 <- merge(temp2,temp, by = 'plot')

temp2$error <- ifelse(grepl("Best",temp2$names), "best", NA)
temp2$error <- ifelse(grepl("Min",temp2$names), "min", temp2$error)
temp2$error <- ifelse(grepl("Max",temp2$names), "max", temp2$error)
temp2$error

Fig_S1_ImpactCount <- ggplot(data=temp2[temp2$error %in% c("best"),], aes(x=damCount, y = damPerc)) +
  #geom_smooth(aes(x=damCount, y = damPerc,colour = area.x), method=lm, se=F) +
  geom_errorbar(aes(ymin=temp2[temp2$error %in% c("min"),]$damPerc,ymax=temp2[temp2$error %in% c("max"),]$damPerc),col = "grey30",width = 0.5,) +# makes it unreadable
  #geom_point()
  geom_point(aes(color = habitat)) + #, shape = area.x)) + #, shape = type.x) ) +
  #geom_point(data=function(temp2){temp2[temp2$error %in% c("best"),]}, aes(color = area.x, shape = type.x) ) 
  geom_smooth(aes(x=damCount, y = damPerc), col ="black", method=lm, se=F) +
  #theme_cowplot(10) +
  theme_classic() +
  scale_color_npg() +
  theme(legend.position=c(0.2, 0.85),
        legend.title=element_blank()) +
  labs(y= "Mapped impact areas in % of 2m x 2m plots", x = "Field inventory impact count in 2m x 2m plots") 
#xlim( c(0, 800)) + ylim( c(0, 80))
Fig_S1_ImpactCount

ggsave("Plots/Fig_S1_ImpactCount.pdf", Fig_dam_count_comp,  width = 12, height = 12, units = "cm")
ggsave("Plots/Fig_S1_ImpactCount.png", Fig_dam_count_comp,  width = 12, height = 12, units = "cm")

