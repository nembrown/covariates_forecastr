library(here)
library(sf)
library(raster)
library(spData)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)
library(rgdal) # spatial/shp reading
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)
require(sp)
#devtools::install_github("ropensci/rerddap")
library(rerddap)
library(tidyverse)
library(dplyr)

#from erdapploading:
dfo_meds_buoys_stations


stocks_loc<-read.csv("data-raw/stocks.csv") 
stocks_loc<- stocks_loc %>%  as_tibble() %>% dplyr::select(Stock_ERA, StockName, SiteName, MapRegion,Lat, Long) %>% 
                                             rename(long=Long, lat=Lat)

coordinates(dfo_meds_buoys_stations) <- c("long", "lat")
coordinates(stocks_loc) <- c("long", "lat")             


# Define these vectors, used in the loop.
closestStock_ERAVec <- vector(mode = "numeric",length = nrow(stocks_loc))
minDistVec     <- vector(mode = "numeric",length = nrow(stocks_loc))

for (i in 1 : nrow(stocks_loc))
{
   distVec <- spDistsN1(dfo_meds_buoys_stations,stocks_loc[i,],longlat = TRUE)
   minDistVec[i] <- min(distVec)
   closestStock_ERAVec[i] <- which.min(distVec)
}

PointAssignTemps <- as(dfo_meds_buoys_stations[closestStock_ERAVec,]$STN_ID,"character")

loc_matching = data.frame(coordinates(stocks_loc),stocks_loc$Stock_ERA,stocks_loc$MapRegion,closestStock_ERAVec,minDistVec,PointAssignTemps)
names(loc_matching) <- c("Stock_long","Stock_lat","Stock_ERA","Region","CloseTempIndex","Distance","STN_ID")



loc_matching<- loc_matching %>% arrange(desc(Distance)) %>% as_tibble()
loc_matching

loc_matching_BC<- loc_matching %>% filter(Region == "BC")
loc_matching_BC

