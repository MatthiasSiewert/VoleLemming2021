###################################################################################
#author: Matthias Siewert
# matthias.siewert@umu.se
########
## 2020-05-02
###
# This script is to extract rodent damage from the UAV at the location of ground plots
# Then combines this with ground inventory
###

library(rgdal)
# library(tidyverse)
 library(lubridate)
# library(raster)
# library(readODS)
# library(cowplot)
# library(wesanderson)
# library(ggsci)
library(doBy)
 library(ggpmisc)



#########################################################
# Load digitized Rodent damage polygons
###############################

## Load damage polygons
DamPlots <- readOGR("Vector/PlotData/RodentPlots_2019_polyg.gpkg", "DamagePlots_2019_polyg")


#####
# extract data from uncroped snow replaced files .. (because some plots are outside of core area)

Rastlist <- list.files("Raster/2_NDVI/",pattern = ".tif$", full.names=T, recursive =T)
Rastlist

# have a look at an example:
plot(raster(Rastlist[[4]]))
plot(NFarea, add = T)
plot(DamPlots, add = T)


# # example extraction
# endCluster()  # wont work with cluster on
# test <- raster::extract(raster(Rastlist[[14]]),DamPlots)
# DamPlots$name
# names(test) <- DamPlots$name
# test

########################
# The following loops through all rasters and extract NDVI pixel values (not using weights)
# and stores them in a temporary list with a sublist for each raster

# create empty list for results
DamList <- list()
# loops through each raster and then extracts all damageplots
for (i in 1:length(Rastlist)){
  temp <- raster::extract(raster(Rastlist[[i]]),DamPlots)
  names(temp) <- DamPlots$name
  name <- substr(Rastlist[[i]],24,150)
  temp <- Filter(Negate(is.null), temp)
  DamList[[name]] <- temp
  print(name)}

# Collapse into one main list
DamList <- unlist(DamList, recursive =F)
#DamList # Takes long to load
##################################################
# extract values

# calculate means:
templist <- list()
i =1
for (i in 1:length(DamList)){
  print(names(DamList[i]))
  temp <- lapply(DamList[i], mean)
  name <- names(DamList[i])
  templist[[name]]<- temp}
templist

# create  tibble to work with
# head(meanTable)
meanTable <- reshape2::melt(templist)

colnames(meanTable) <- c('meanNDVI', 'count', 'names')
meanTable$count <- NULL
meanTable$area <- as.character(substr(meanTable$names,12,13))
meanTable$date <- as.Date(as.character(substr(meanTable$names,1,10)), "%Y.%m.%d")

meanTable$plot <- substr(meanTable$names, nchar(meanTable$names)-4, nchar(meanTable$names))
meanTable$type <- substr(meanTable$names, nchar(meanTable$names)-1, nchar(meanTable$names)-1)

#meanTable$season <- ifelse(month(meanTable$date) == 6, 'spring', ifelse(month(meanTable$date) == 9, 'autumn', 'peak')) 
meanTable$year   <- ifelse(year(meanTable$date) == 2018, 2018, ifelse(year(meanTable$date) == 2019, 2019, 2017)) 

# only use 2018 and 2019
meanTable <- subset(meanTable, date > as.Date("2018-01-01"))
meanTable
write_csv(meanTable, 'meanTable.csv')


##### extract the mean of all Damage and all control plots per study area
meanTable2 <- meanTable %>% 
  group_by(area, date, type) %>%
  summarise(mean_impactNDVI = mean(meanNDVI))
meanTable2

meanTable2$season <- ifelse(month(meanTable2$date) == 6, 'spring', ifelse(month(meanTable2$date) == 9, 'autumn', 'peak')) 
meanTable2$year   <- ifelse(year(meanTable2$date) == 2018, 2018, ifelse(year(meanTable2$date) == 2019, 2019, 2017)) 
meanTable2 <- subset(meanTable2, date > as.Date("2018-01-01"))
meanTable2

############################################################## combine with damage information
## Load ground inventory data
#sumTable <- read_csv('~/Data/Drones/Damage/DamageR/sumTablerules3_eachRodentcounts1max1.csv')
sumTable <- read_csv('~/Data/Drones/Damage/DamageR/sumTablerules4_eachRodentcounts1nomaxbut3.csv')

# also merge class names
temp <- read_ods('~/Data/Drones/Damage/DamageR/Excel/PlotTable.ods')
temp[, c('plot', 'habitat')]
temp$habitat = factor(temp$habitat,levels = c("Dry heath & Barren",
                                              "Betula nana heath",
                                              "Mesic heath & Snowbed",
                                              "Willow",
                                              "Birch forest",
                                              "Wetlands & meadows"
))


sumTable <- merge(sumTable, temp[, c('plot', 'habitat')], by = 'plot')

sumTable
meanTable

meanTable3 <- meanTable

# clean up
meanTable3$names <- NULL
meanTable3$season = factor(meanTable3$season,levels = c("spring", "peak", "autumn"))

# Need to delete date, otherwise pivot wont work. 
meanTable3$date <- NULL
temp <-meanTable3 %>% pivot_wider(names_from = year, values_from = meanNDVI)
temp

# combine with previous results
temp2 <- merge(temp,sumTable, by = c( 'area', 'plot','type'))
temp2$X1 <- NULL

# Subtract NDVI 2019 minus 2018
colnames(temp2) <- c("area","plot","type", "season","y2018","y2019","damCount","habitat")
temp2$y2018 <- as.numeric(as.character(temp2$y2018))
temp2$y2019 <- as.numeric(as.character(temp2$y2019))

temp2$diff19minus18 <- temp2$y2019 - temp2$y2018
temp2

# use only peak season
temp <- subset(temp2 , season == 'peak')
#temp <- subset(temp, plot != c('KJ-C1' , 'KJ-D1'))

damageTable <- temp
damageTable
summary(lm(damCount ~ diff19minus18 , data = damageTable))
summary(lm(damCount ~ diff19minus18:area , data = damageTable))

summary(lm(damCount ~ diff19minus18:habitat , data = damageTable))

summary(lm(damCount ~ diff19minus18 , data = subset(damageTable, habitat %in% c("Dry tundra & Barren" )) ))
summary(lm(damCount ~ diff19minus18 , data = subset(damageTable, habitat %in% c("Mesic heath" )) ))




Fig2_damPlot_all <- ggplot(damageTable, aes(x=damCount, y = diff19minus18)) +
  geom_smooth(method=lm, se=T, color = 'black') +
  geom_point(aes(colour = factor(type))) +
  #  geom_point(aes(shape = type), size =3) +
  #  geom_point(aes(col =area)) +
  #  geom_text(aes(label = plot, size =6)) +
  theme_classic() +
  scale_color_npg() +
  theme(legend.position=c(0.8, 0.8)) +
  labs(y= "UAV NDVI change 2018 to 2019", x = "Rodent impact field inventory count",fill = "Plot type") +
  stat_poly_eq(formula =  y ~ x, rr.digits = 3,
               aes(label = paste(..rr.label..,..p.value.label.., sep = "~~~")), 
               parse = TRUE, size =2.5)   
Fig2_damPlot_all

ggsave("Plots/Fig_2_Damage/Fig2_damPlot_all.pdf", Fig2_damPlot_all,  width = 8, height = 8, units = "cm")

Fig2_damPlot_sep <- ggplot(damageTable, aes(x=damCount, y = diff19minus18)) +
  geom_smooth(method=lm, se=T, color = 'black') +
  #   geom_point() +
  geom_point(aes(col =habitat)) +
  #  geom_text(aes(label = plot, size =6)) +
  theme_classic() +
  scale_color_npg() +
  theme(legend.position="none") +
  labs(y= "UAV NDVI change 2018 to 2019", x = "Rodent impact field inventory count") +
  facet_wrap(~habitat) +#, scales = "free_y") +
  stat_poly_eq(formula =  y ~ x, rr.digits = 3,
               aes(label = paste(..rr.label..,..p.value.label.., sep = "~~~")), 
               parse = TRUE, size =2.5)   
Fig2_damPlot_sep

tempPlot <- ggplot(damageTable, aes(x=damCount, y = diff19minus18)) +
  geom_smooth(method=lm, se=T, color = 'black') +
  geom_point(aes(col =habitat)) +
  #  geom_point(aes(col =area)) +
  #  geom_text(aes(label = plot, size =6)) +
  theme_classic() +
  #  scale_color_npg() +
  theme(legend.position="none") +
  labs(y= "UAV NDVI change 2019 -  2018", x = "Field impact inventory count") +
  facet_wrap(~area)#, scales = "free_y")
tempPlot



#plotRule1 <- tempPlot
#plotRule3 <- tempPlot

Fig2_damPlot_comp <- plot_grid(Fig2_damPlot_all,Fig2_damPlot_sep,rel_widths = c(1, 1), labels = c("a","b"))
Fig2_damPlot_comp

ggsave("Plots/Fig_2_Damage/Fig2_damPlot_comp.pdf", Fig2_damPlot_comp,  width = 25, height = 12, units = "cm")
ggsave("Plots/Fig_2_Damage/Fig2_damPlot_comp.png", Fig2_damPlot_comp,  width = 25, height = 12, units = "cm")
