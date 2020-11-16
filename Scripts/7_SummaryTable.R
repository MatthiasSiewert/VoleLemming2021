# Summarytable

#############################
# Summary table
library(xtable)

# UAV Mean NDVI 2018, Mean NDVI 2918
# Modis Modis /sentinel NDVI 2018, 2019
# reduction in NDVI in mean for entire study area, as percentage
# mean N rodent damage hits in 20x20 raster ( total, percent)
# mean N control plots hits in 20x20 raster (total, percent)
# global MoranI 
# variogram ranges

SummaryTable



################# create an output tabe for print
SummaryTableOut <- as.data.frame(SummaryTable$area)
colnames(SummaryTableOut) <- c('Study area')

SummaryTableOut$'Type' <- c("Oceanic tundra","Continental forest line","Continental tundra","Oceanic forest line")

SummaryTableOut$'Mapped rodent impact 2019 (%) - Best estimate (Min - Max)' <-
  paste0(round(SummaryTable$imp_percBest,1)," (", round(SummaryTable$imp_percMin,1),"–", round(SummaryTable$imp_percMax,1),")") 


SummaryTableOut$'Mean NDVI 2018' <- round(SummaryTable$meanNDVI_2018, 3)
SummaryTableOut$'Mean NDVI 2019' <- round(SummaryTable$meanNDVI_2019, 3)
SummaryTableOut$'Change in NDVI 2018 to 2019' <- round(SummaryTable$dif20182019, 3)

SummaryTableOut$'Change in NDVI 2018 to 2019 in %' <- round(SummaryTable$dif20182019 / SummaryTable$meanNDVI_2018 *100 , 1)

SummaryTableOut$'Change in GPP 2018 to 2019 in %'     <- round(SummaryTable$GPP_2019 /SummaryTable$GPP_2018 *100 -100 , 1)
SummaryTableOut$'Change in biomass 2018 to 2019 in %' <- round(SummaryTable$BIO_2019 /SummaryTable$BIO_2018 *100 -100 , 1)

SummaryTableOut$"Spatial autocorrelation of rodent impact (Moran's I)" <- round(as.numeric(as.character(SummaryTable$moran)), 2)
SummaryTableOut$"Maximum range of spatial autocorrelation (m)" <- round(as.numeric(as.character(SummaryTable$range.y)), 2)


# reorder
SummaryTableOut <- SummaryTableOut[c(2,3,4,1),]

SummaryTableOut


temp <- xtable(SummaryTableOut,digits =3)   
print.xtable(temp, type="html", file = "Table/SummaryTableOut.html", floating.environment='sidewaystable')


