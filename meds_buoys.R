library(rerddap)
library("akima")
library("dplyr")
library("ggplot2")
library("mapdata")
library("ncdf4")
library("plot3D")
library(lubridate)
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
library(tidyverse)
library(dplyr)

#searching erddap for sea surface temperature
whichSST <- ed_search(query = "SST")


cioos_pacific_datasets<-ed_datasets(which = "tabledap", url = "https://data.cioospacific.ca/erddap/")
#View(cioos_pacific_datasets)

rerddap::info('DFO_MEDS_BUOYS', url = "https://data.cioospacific.ca/erddap/")

dfo_meds_buoys<- tabledap('DFO_MEDS_BUOYS',  url = "https://data.cioospacific.ca/erddap/", 
                          fields = c('latitude', 'longitude', 'time', 'STN_ID', 'SSTP', 'SSTP_flags', 'Q_FLAG', 'SSTP_UQL'), 
                          'time>=1980-01-01', 'SSTP!=NaN', 'SSTP_flags!=1')

dfo_meds_buoys_small <- dfo_meds_buoys %>% as_tibble %>% 
                                           filter(SSTP_UQL %in% c(1,2)) %>% 
                                           filter(Q_FLAG != 4) %>% 
                                           mutate(year = lubridate::year(time), 
                                                  month = lubridate::month(time), 
                                                  day = lubridate::day(time), 
                                                  SSTP = as.numeric(SSTP))

dfo_meds_buoys_month<- dfo_meds_buoys_small %>%  group_by(STN_ID, year, month) %>% 
                                                 dplyr::summarise(mean_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_year_summer<- dfo_meds_buoys_small %>%  filter(month %in% c(5,6,7,8,9)) %>% 
                                                       group_by(STN_ID, year) %>% 
                                                       dplyr::summarise(mean_summer_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_year<- dfo_meds_buoys_small  %>% group_by(STN_ID, year) %>% 
                                                       dplyr::summarise(mean_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_combined<-merge(dfo_meds_buoys_year, dfo_meds_buoys_year_summer)

dfo_meds_buoys_stations <- dfo_meds_buoys_small %>% dplyr::select(STN_ID, latitude, longitude) %>% 
                                                    rename(lat= latitude, long= longitude) %>% 
                                                    mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
                                                    filter(STN_ID != "C46134") %>% 
                                                    rowwise() %>% 
                                                    distinct()


###### 
dfo_meds_buoys_small$time<- as_date(dfo_meds_buoys_small$time)
ggplot(dfo_meds_buoys_year, aes(x=year, y=mean_SSTP, col=STN_ID, group=STN_ID))+geom_point()+geom_line()

ggplot(dfo_meds_buoys_year , aes(x=STN_ID, y=year, col=STN_ID, group=STN_ID))+geom_point()




####Mapping 
stocks_loc<-read.csv("data-raw/stocks.csv") 
#note terminal - I manually mapped these, so that for eg. the fraser river stocks would map to the mouth of the fraser and not interior
stocks_loc<- stocks_loc %>%  as_tibble() %>% dplyr::select(Stock_ERA, StockName, SiteName, MapRegion,Lat, Long, Lat_terminal, Long_terminal) %>% 
  rename(long=Long_terminal, lat=Lat_terminal)

stocks_loc_simple_BC <- stocks_loc %>% filter(MapRegion == "BC") %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA)
stocks_loc_simple_BC$site_type <- "ERA Stock"

stocks_loc_simple <- stocks_loc %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA)
stocks_loc_simple$site_type <- "ERA Stock"
dfo_meds_buoys_stations_simple<- dfo_meds_buoys_stations %>% rename(Site_name = STN_ID)
dfo_meds_buoys_stations_simple$site_type <- "MERS buoy"


dfo_meds_buoys_era_stocks<- bind_rows(stocks_loc_simple_BC , dfo_meds_buoys_stations_simple) 
dfo_meds_buoys_era_stocks


###Make map
bbox_marine <- make_bbox(stocks_loc_simple$long, stocks_loc_simple$lat, f = 0.001)
map_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "terrain", crop=FALSE, zoom=8)

bbox_marine_BC <- make_bbox(dfo_meds_buoys_stations_simple$long, dfo_meds_buoys_stations_simple$lat, f = 0.1)
map_marine_BC <- get_stamenmap(bbox_marine_BC, source="stamen", maptype= "terrain", crop=FALSE, zoom=8)

colorset_map = c("ERA Stock"="#FF007F" , "MERS buoy" ="#0000FF" )
ggmap(map_marine_BC) + geom_point(data=dfo_meds_buoys_era_stocks, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)


ggmap(map_marine) + geom_point(data=stocks_loc_simple, aes(x = long, y = lat))

ggmap(map_marine_BC) + geom_point(data=dfo_meds_buoys_stations_simple, aes(x = long, y = lat))+geom_text(data=dfo_meds_buoys_stations_simple, aes(x = long, y = lat, label=Site_name))

ggmap(map_marine_BC) + geom_point(data=stocks_loc_simple_BC, aes(x = long, y = lat, col=Site_name), size=3)


#+
 # geom_text(data=dfo_meds_buoys_era_stocks, aes(x = long, y = lat,  label=Site_name))


####Matching

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

loc_matching_BC_terminal<-loc_matching_BC %>%  select(Stock_ERA, STN_ID)
loc_matching_BC_offshore<-loc_matching_BC_terminal %>% 
                          mutate(STN_ID = case_when(
                                          Stock_ERA == "KLM/KLY" ~ "C46184",
                                          Stock_ERA == "ATN/ATS" ~ "C46004",
                                          TRUE ~ "C46036"))

#Terminal
dfo_meds_buoys_matched_terminal<-left_join(loc_matching_BC_terminal, dfo_meds_buoys_combined)
dfo_meds_buoys_matched_terminal<-dfo_meds_buoys_matched_terminal %>% 
                                 rename(cov_SSTP_terminal_summer = mean_summer_SSTP, 
                                        cov_SSTP_terminal_year = mean_SSTP) %>% rename(buoy_ID_terminal=STN_ID)
#Offshore
dfo_meds_buoys_matched_offshore<-left_join(loc_matching_BC_offshore, dfo_meds_buoys_combined)
dfo_meds_buoys_matched_offshore<-dfo_meds_buoys_matched_offshore %>% 
                                 rename(cov_SSTP_offshore_summer = mean_summer_SSTP, 
                                        cov_SSTP_offshore_year = mean_SSTP) %>% rename(buoy_ID_offshore=STN_ID)

dfo_meds_buoys_matched_combined<-merge(dfo_meds_buoys_matched_terminal, dfo_meds_buoys_matched_offshore)
dfo_meds_buoys_matched_combined

