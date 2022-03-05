library(curl)
library(tidyverse)
library(lubridate)
library(sf)
library(ggplot2)
library(here)
library(raster)
library(spData)
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(rgdal) # spatial/shp reading
library(viridis) # nice color palette
library(ggmap) # ggplot functionality for maps ---> dplyr, purr is dependency
library(ggsn) # for scale bars/north arrows in ggplots
library(maps)
library(mapdata)
require(sp)


# Load zooplankton data ---------------------------------------------------

ios_zoop_base<-read.csv(curl('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Pacific_Zooplankton/IOS_zoop_vnh_biomass_major_taxa_1980_2018_V1.csv')) %>%  as_tibble()

ios_zoop <- ios_zoop_base  %>% mutate(Euphausiacea = case_when(Twilight == "Daylight" ~ Euphausiacea*3), 
                                total_zoop_biomass = rowSums(across(Polychaeta:Animalia), na.rm=TRUE), 
                                              Date = as_date(Date),
                                              year = lubridate::year(Date), 
                                             month = lubridate::month(Date), 
                                               day = lubridate::day(Date), 
                                            season = case_when(month %in% c(12, 1, 2) ~ "winter", 
                                                               month %in% c(3, 4, 5) ~ "spring", 
                                                               month %in% c(6, 7, 8) ~ "summer", 
                                                               month %in% c(9, 10, 11) ~ "fall"), 
                                         calc_year = case_when(month == 12 ~ (year + 1), TRUE ~ year)) %>%                                         
                                mutate(region_station = paste(Region_name, Station, sep="-"))


ios_zoop_stations<- ios_zoop %>% dplyr::select(region_station, Longitude, Latitude) %>% 
                                        rename(lat= Latitude, long= Longitude) %>% 
                                        mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
                                        rowwise() %>% 
                                        distinct() %>% 
                                        na.omit
ios_zoop_stations


# Load stock locations ----------------------------------------------------
stocks_loc<-read.csv("data-raw/stocks.csv") %>%  as_tibble()

#note terminal - I manually mapped these, so that for eg. the fraser river stocks would map to the mouth of the fraser and not interior
stocks_loc<- stocks_loc %>% dplyr::select(Stock_ERA, StockName, SiteName, MapRegion,Lat, Long, Lat_terminal, Long_terminal) %>% 
                                   rename(long=Long_terminal, lat=Lat_terminal)

stocks_loc_simple_1 <- stocks_loc %>% dplyr::select(Stock_ERA, lat, long)
stocks_loc_simple_1


# Match zooplankton to stock ----------------------------------------------

#transforming into sf objects
ios_zoop_stations.sf <- st_as_sf(ios_zoop_stations, coords = c("long", "lat"), crs = 4326)
ios_zoop_stations.sf<-ios_zoop_stations.sf%>% st_transform(3035)
ios_zoop_stations.sf

stocks_loc_simple.sf <- st_as_sf(stocks_loc_simple_1, coords = c("long", "lat"), crs = 4326)
stocks_loc_simple.sf<-stocks_loc_simple.sf %>% st_transform(3035)
stocks_loc_simple.sf

# Buffer circles of 100000m, i.e. 100km  -- creates polygons around the stocks
dat_circles <- st_buffer(stocks_loc_simple.sf, dist = 100000)

#which of the region_stations fall within 50km radius of each stock
ios_zoop_era_stocks<- st_join(ios_zoop_stations.sf, dat_circles, left=FALSE) %>% st_set_geometry(NULL) %>% as_tibble()
ios_zoop_era_stocks

# Apply matching to data --------------------------------------------------
ios_zoop_matched <- left_join(ios_zoop_era_stocks, ios_zoop)
ios_zoop_matched 

#Instead of by Station, need to do by region
ios_zoop_summer<- ios_zoop_matched %>%  filter(season == "summer") %>% 
                                        group_by(Stock_ERA, calc_year) %>% 
                                        summarise(summer_zoop = mean(total_zoop_biomass, na.rm=TRUE)) %>% 
                                        mutate(log_summ_zoop = log10(summer_zoop))
                                
  
ios_zoop_winter<-ios_zoop_matched %>% filter(season == "winter") %>% 
                                      group_by(Stock_ERA, calc_year) %>% 
                                      summarise(winter_zoop = mean(total_zoop_biomass, na.rm=TRUE)) %>% 
                                      mutate(log_wint_zoop = log10(winter_zoop))

ios_zoop_spring<-ios_zoop_matched %>% filter(season == "spring") %>% 
                                      group_by(Stock_ERA, calc_year) %>% 
                                      summarise(spring_zoop = mean(total_zoop_biomass, na.rm=TRUE))%>% 
                                      mutate(log_spring_zoop = log10(spring_zoop))

ios_zoop_fall<- ios_zoop_matched %>% filter(season == "fall") %>% 
                                     group_by(Stock_ERA, calc_year) %>% 
                                     summarise(fall_zoop = mean(total_zoop_biomass, na.rm=TRUE))%>% 
                                     mutate(log_fall_zoop = log10(fall_zoop))

ios_zoop_allseasons<-merge(ios_zoop_winter, ios_zoop_spring, all=TRUE)
ios_zoop_allseasons<-merge(ios_zoop_allseasons, ios_zoop_summer, all=TRUE)
ios_zoop_allseasons<-merge(ios_zoop_allseasons, ios_zoop_fall, all=TRUE) %>% as_tibble()
ios_zoop_allseasons

ios_zoop_alltime_av <- ios_zoop_allseasons %>% dplyr::select(Stock_ERA, log_wint_zoop, log_spring_zoop, log_summ_zoop, log_fall_zoop) %>% 
                                               group_by(Stock_ERA) %>% 
                                               summarise_at(vars(log_wint_zoop:log_fall_zoop), funs(mean(., na.rm=TRUE), n()))

ios_zoop_allseasons<-left_join(ios_zoop_allseasons, ios_zoop_alltime_av)

ios_zoop_allseasons<- ios_zoop_allseasons %>% mutate(cov_zoop_winter_anomaly = log_wint_zoop - log_wint_zoop_mean, 
                                                     cov_zoop_spring_anomaly = log_spring_zoop - log_spring_zoop_mean, 
                                                     cov_zoop_summer_anomaly = log_summ_zoop - log_summ_zoop_mean, 
                                                     cov_zoop_fall_anomaly = log_fall_zoop - log_fall_zoop_mean)
ios_zoop_anomalies<-ios_zoop_allseasons %>% dplyr::select(Stock_ERA, calc_year, cov_zoop_winter_anomaly, cov_zoop_spring_anomaly, cov_zoop_summer_anomaly, cov_zoop_fall_anomaly) %>% 
                                          mutate(cov_zoop_yearly_anomaly = rowMeans(across(cov_zoop_winter_anomaly:cov_zoop_fall_anomaly), na.rm=TRUE))
ios_zoop_anomalies


View(ios_zoop_anomalies)  

ggplot(ios_zoop_anomalies, aes(x=Stock_ERA, y=calc_year, col=Stock_ERA))+geom_point()+coord_flip()

 
##### Mapping and matching


####Mapping 
stocks_loc<-read.csv("data-raw/stocks.csv") 
#note terminal - I manually mapped these, so that for eg. the fraser river stocks would map to the mouth of the fraser and not interior
stocks_loc<- stocks_loc %>%  as_tibble() %>% dplyr::select(Stock_ERA, StockName, SiteName, MapRegion,Lat, Long, Lat_terminal, Long_terminal) %>% 
  rename(long=Long_terminal, lat=Lat_terminal)

stocks_loc_simple_BC <- stocks_loc %>% filter(MapRegion == "BC") %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA)
stocks_loc_simple_BC$site_type <- "ERA Stock"

stocks_loc_simple <- stocks_loc %>% dplyr::select(Stock_ERA, lat, long) %>% rename(Site_name = Stock_ERA)
stocks_loc_simple$site_type <- "ERA Stock"


ios_zoop_regions_simple<- ios_zoop_regions %>% rename(Site_name = Region_name)
ios_zoop_regions_simple$site_type <- "IOS Zooplankton Station"


ios_zoop_era_stocks<- bind_rows(stocks_loc_simple, ios_zoop_regions_simple) 
ios_zoop_era_stocks

View(ios_zoop_stations)

###Make map
bbox_marine <- make_bbox(ios_zoop_era_stocks$long, ios_zoop_era_stocks$lat, f = 0.001)
map_marine <- get_stamenmap(bbox_marine, source="stamen", maptype= "terrain", crop=FALSE, zoom=7)

 bbox_marine_BC <- make_bbox(stocks_loc_simple_BC$long,stocks_loc_simple_BC$lat, f = 0.1)
 map_marine_BC <- get_stamenmap(bbox_marine_BC, source="stamen", maptype= "terrain", crop=FALSE, zoom=7)

colorset_map = c("ERA Stock"="#FF007F" , "IOS Zooplankton Station" ="#0000FF" )
ggmap(map_marine) + geom_point(data=ios_zoop_era_stocks, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map)


ggmap(map_marine_BC) + geom_point(data=ios_zoop_stations, aes(x = long, y = lat, col=Region_name), size=3) 

#+
 #                   geom_text(data=ios_zoop_regions_simple, aes(x = long, y = lat, label=Site_name, col=Site_name))



