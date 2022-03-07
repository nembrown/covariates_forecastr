
#load mapping libraries
library(sf)
library(raster)
library(spData)
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(maps)
library(mapdata)
require(sp)
library(tidyverse)
library(lubridate)
library(here)


# Map variables for stocks ------------------------------------------------

stocks_loc_map<-stocks_loc %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA) %>% add_column(site_type = "ERA Stock")
stocks_loc_map_BC<-stocks_loc %>% filter(MapRegion == "BC") %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA)  %>% add_column(site_type = "ERA Stock")



# Map boxes ---------------------------------------------------------------
bbox_marine<- make_bbox(stocks_loc_map$long, stocks_loc_map$lat, f = 0.1)
map_marine<- get_stamenmap(bbox_marine, source="stamen", maptype= "terrain", crop=FALSE, zoom=7)

bbox_marine_BC<- make_bbox(stocks_loc_map_BC$long, stocks_loc_map_BC$lat, f = 0.1)
map_marine_BC<- get_stamenmap(bbox_marine_BC, source="stamen", maptype= "terrain", crop=FALSE, zoom=8)


colorset_map = c("ERA Stock"="#FF007F" , 
                 "Lighthouse" ="#0000FF", 
                 "MEDS buoy" = "#3399FF", 
                 "IOS Zooplankton Station" = "#66CCCC")

# Map Lightstations and stocks ------------------------------------------------------
light_stock<-bind_rows(Data_Lightstations_locations, stocks_loc_map)

ggmap(map_marine) + geom_point(data=light_stock, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)


# Map MEDS buoys and stocks -----------------------------------------------

dfo_meds_buoys_locations<- dfo_meds_buoys_locations %>% rename(Site_name = STN_ID)
dfo_meds_buoys_era_stocks<- bind_rows(stocks_loc_map, dfo_meds_buoys_locations) 
dfo_meds_buoys_era_stocks

ggmap(map_marine) + geom_point(data=dfo_meds_buoys_era_stocks, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)


# Map IOS Zooplankton stations and stocks ---------------------------------
ios_zoop_stations
ios_zoop_stations_simple<- ios_zoop_stations %>% rename(Site_name = region_station) %>% add_column(site_type = "IOS Zooplankton Station")

ios_zoop_era_stocks<- bind_rows(stocks_loc_map, ios_zoop_stations_simple) 
ios_zoop_era_stocks

ggmap(map_marine) + geom_point(data=ios_zoop_era_stocks, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)

# Map all stations and stocks ---------------------------------
all_stations_stocks<- bind_rows(ios_zoop_stations_simple,dfo_meds_buoys_locations, Data_Lightstations_locations, stocks_loc_map)

ggmap(map_marine) + geom_point(data=all_stations_stocks, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)


