###################################################################################
# author: Matthias Siewert
# matthias.siewert@umu.se
########
###
# This script provides variograms of rodent damage and adds the spatial range to the summary table.
###

# Load libraries
library(gstat)
library(rgdal)
library(reshape2)
library(ggplot2)
library(ggsci)
library(tidyverse)



#################################################################
####   This script extracts analyzes spatial autocorrelation of the rodent damage
# NF
temp <- as(NF2018minus19damBinBestCrp, 'SpatialPointsDataFrame')
set.seed(1)
temp <- temp[sample(1:length(temp),100000),]   # subsample to reduce processing time
VarioNF <- variogram(temp@data$NFareaNF2018minus19damBinBestCrp ~ 1, data = temp,width = .3,cutoff = 20)
#VarioNF.m <- fit.variogram(VarioNF, vgm(1,"Exp",1,0))
set.seed(1)
VarioNF.m <- fit.variogram(VarioNF,  vgm(.4, "Exp", 0.4, 0, add.to=vgm(.1, "Exp", 3)))

# NT
temp <- as(NT2018minus19damBinBestCrp, 'SpatialPointsDataFrame')
set.seed(1)
temp <- temp[sample(1:length(temp),100000),]   # subsample to reduce processing time
VarioNT <- variogram(temp@data$NTareaNT2018minus19damBinBestCrp ~ 1, data = temp,width = .3,cutoff = 20)
#VarioNT.m <- fit.variogram(VarioNT, vgm(1,"Exp",1,0))
set.seed(1)
VarioNT.m <- fit.variogram(VarioNT,  vgm(.4, "Exp", 0.5, 0, add.to=vgm(1, "Exp", 3)))

# VJ
temp <- as(VJ2018minus19damBinBestCrp, 'SpatialPointsDataFrame')
set.seed(1)
temp <- temp[sample(1:length(temp),100000),]   # subsample to reduce processing time
VarioVJ <- variogram(temp@data$VJareaVJ2018minus19damBinBestCrp ~ 1, data = temp,width = .3,cutoff = 20)
#VarioVJ.m <- fit.variogram(VarioVJ, vgm(1,"Exp",1,0))
set.seed(1)
VarioVJ.m <- fit.variogram(VarioVJ, vgm(.4, "Exp", 0.5, 0, add.to=vgm(1, "Exp", 2)))

# KJ
temp <- as(KJ2018minus19damBinBestCrp, 'SpatialPointsDataFrame')
set.seed(1)
temp <- temp[sample(1:length(temp),100000),]   # subsample to reduce processing time
VarioKJ <- variogram(temp@data$KJareaKJ2018minus19damBinBestCrp ~ 1, data = temp,width = .3,cutoff = 20)
#VarioKJ.m <- fit.variogram(VarioKJ, vgm(1,"Exp",1,0))
set.seed(1)
VarioKJ.m <- fit.variogram(VarioKJ, vgm(.15, "Exp",2.5, 0, add.to=vgm(1, "Exp", 2)))

plot(VarioNF,VarioNF.m)
plot(VarioNT,VarioNT.m)
plot(VarioVJ,VarioVJ.m)
plot(VarioKJ,VarioKJ.m)

### Bind dots
VarioNF$area    <- c("NF")
VarioNT$area    <- c("NT")
VarioVJ$area    <- c("VJ")
VarioKJ$area    <- c("KJ")
## merge them into one for ggplot


VarioALL <- data.frame(rbind(VarioNF,VarioNT, VarioVJ, VarioKJ))

VarioALL <- transform(VarioALL,area=factor(area,levels=c("NF","NT","VJ","KJ")))


# Bind models:
### Bind the models
VarioLine <- rbind(  cbind(variogramLine(VarioNF.m, maxdist = max(VarioNF$dist)), area =  "NF"), 
                     cbind(variogramLine(VarioNT.m, maxdist = max(VarioNT$dist)), area =  "NT"),
                     cbind(variogramLine(VarioVJ.m, maxdist = max(VarioVJ$dist)), area =  "VJ"),
                     cbind(variogramLine(VarioKJ.m, maxdist = max(VarioKJ$dist)), area =  "KJ")) 
VarioLine <- transform(VarioLine,area=factor(area,levels=c("NF","NT","VJ","KJ")))


VariogrPl <- ggplot(VarioALL, aes(x = dist, y = gamma, colour = area)) +
  geom_point(size =1,alpha = 1/3) +  #ylim(0,1.15) +xlim(0,25) +geom_line(data = vgLine)+
  xlim(0,10) +
  geom_line(data = VarioLine)+
  xlab('Distance in m') + ylab("Semivariance") +
  theme_classic() +
  scale_color_npg() +
  labs(color='area') +
  theme(legend.position= "right")
VariogrPl

ggsave("Plots/Fig_S2_variogram.pdf", VariogrPl,  width = 12, height = 8, units = "cm")
ggsave("Plots/Fig_S2_variogram.png", VariogrPl,  width = 12, height = 8, units = "cm")


# Get variogram parameters:
VarioNF.m$area <-  "NF"
VarioNT.m$area <-  "NT"
VarioVJ.m$area <-  "VJ"
VarioKJ.m$area <-  "KJ"

VarioTable <- as.data.frame(rbind (VarioNF.m,VarioNT.m,VarioVJ.m,VarioKJ.m))
VarioTable

VarioTable <- VarioTable[,c(3,10)]

SummaryTable <- merge(SummaryTable, filter(VarioTable, between(range, 0.01, 2)), by = "area")
SummaryTable <- merge(SummaryTable, filter(VarioTable, range > 2), by = "area")

SummaryTable

#SummaryTablebackup <- SummaryTable
