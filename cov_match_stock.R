library(tidyverse)
library(sf)
library(sp)
library(tidyhydat)

"%notin%"<- Negate("%in%")

#This code is only applied to spatially explicit data, like zooplankton, temperature. 
#PDO and ONI are North-Pacific Ocean wide. 

# ERA Stock Locations -----------------------------------------------------
stocks_loc<-read.csv("data-raw/stocks.csv") 
#note terminal - I manually mapped these, so that for eg. the fraser river stocks would map to the mouth of the fraser and not interior
stocks_loc<- stocks_loc %>%  as_tibble() %>% 
                             dplyr::select(Stock_ERA,StockName, SiteName, MapRegion,Lat, Long, Lat_terminal, Long_terminal) %>% 
                             rename(long=Long_terminal, lat=Lat_terminal) 

#new dataframe that can be changed into a spatial object for distance matching 
stocks_loc_simple<-stocks_loc
coordinates(stocks_loc_simple) <- c("long", "lat")   
stocks_loc_simple

#new dataframe that can be changed into spatial for radius matching 
stocks_loc_simple_1 <- stocks_loc %>% dplyr::select(Stock_ERA,  lat, long)
stocks_loc_simple_1

#new dataframe that can be changed into spatial for radius matching - using non terminal locations
stocks_loc_simple_2 <- stocks_loc %>% dplyr::select(Stock_ERA,  Lat, Long) %>% rename(lat=Lat, long=Long)
stocks_loc_simple_2

# Match Light stations to ERA Stocks --------------------------------------------
#Needs Data_Lightstations from cov_fetch 
Data_Lightstations_locations<-Lightstations_location %>% dplyr::select(Station_ID, lat, long) %>% 
                                                         rowwise() %>% distinct() %>% 
                                                         rename(Site_name=Station_ID) %>% 
                                                         add_column(site_type = "Lighthouse")

#new dataframe that can be changed into a spatial object
Data_Lightstations_loc<-Data_Lightstations_locations
coordinates(Data_Lightstations_loc) <- c("long", "lat")

# Define these vectors, used in the loop.
closestStock_ERAVec_light <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
minDistVec_light     <- vector(mode = "numeric",length = nrow(stocks_loc_simple))

for (i in 1 : nrow(stocks_loc_simple))
{
  distVec_light <- spDistsN1(Data_Lightstations_loc,stocks_loc_simple[i,],longlat = TRUE)
  minDistVec_light[i] <- min(distVec_light)
  closestStock_ERAVec_light[i] <- which.min(distVec_light)
}

PointAssignTemps_light <- as(Data_Lightstations_loc[closestStock_ERAVec_light,]$Site_name,"character")

loc_matching_light = data.frame(coordinates(stocks_loc_simple),stocks_loc_simple$Stock_ERA, stocks_loc_simple$MapRegion,closestStock_ERAVec_light,minDistVec_light,PointAssignTemps_light)
names(loc_matching_light) <- c("Stock_long","Stock_lat","Stock_ERA","Region","closestStock_ERAVec_light","Distance","Site_name")

#All stocks including American
loc_matching_light<- loc_matching_light %>% arrange(desc(Distance)) %>% as_tibble()

#only BC stocks
loc_matching_light_BC<- loc_matching_light %>% filter(Region == "BC")

#All stocks, including American within 200km 
loc_matching_light_simple<-loc_matching_light %>% filter(Distance<200) %>% dplyr::select(Stock_ERA, Site_name) %>% rename(Location = Site_name)
loc_matching_light_simple

# Combing the match file to the data file (from cov_fetch)
Data_Lightstations_matched<-left_join(loc_matching_light_simple, Data_Lightstations_combined)
Data_Lightstations_matched<- Data_Lightstations_matched %>% rename(Lightstation = Location)
Data_Lightstations_matched



# Match MEDS buoys to ERA Stocks ------------------------------------------------
# #needs dfo_meds_buoy_small and dfo_meds_buoys_combined from cov_fetch 
# dfo_meds_buoys_locations <- dfo_meds_buoys_small %>% dplyr::select(STN_ID, latitude, longitude) %>% 
#                                                     rename(lat= latitude, long= longitude) %>% 
#                                                     mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
#                                                     filter(STN_ID != "C46134") %>% 
#                                                     group_by(STN_ID) %>% 
#                                                     summarise_if(is.numeric, mean) %>% 
#                                                     rowwise() %>% 
#                                                     distinct() %>% 
#                                                     add_column(site_type = "MEDS buoy")
# 
# #New dataframe that can be changed into  a spatial object
# dfo_meds_buoys_stations <- dfo_meds_buoys_locations
# coordinates(dfo_meds_buoys_stations) <- c("long", "lat")
# 
# # Define these vectors, used in the loop.
# closestStock_ERAVec <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
# minDistVec     <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
# 
# for (i in 1 : nrow(stocks_loc_simple))
# {
#   distVec <- spDistsN1(dfo_meds_buoys_stations,stocks_loc_simple[i,],longlat = TRUE)
#   minDistVec[i] <- min(distVec)
#   closestStock_ERAVec[i] <- which.min(distVec)
# }
# 
# PointAssignTemps <- as(dfo_meds_buoys_stations[closestStock_ERAVec,]$STN_ID,"character")
# 
# loc_matching = data.frame(coordinates(stocks_loc_simple),stocks_loc_simple$Stock_ERA,stocks_loc_simple$MapRegion,closestStock_ERAVec,minDistVec,PointAssignTemps)
# names(loc_matching) <- c("Stock_long","Stock_lat","Stock_ERA","Region","closestStock_ERAVec","Distance","STN_ID")
# 
# loc_matching<- loc_matching %>% arrange(desc(Distance)) %>% as_tibble()
# #loc_matching_BC<- loc_matching %>% filter(Region == "BC")
# 
# #All locations match, Within 200km 
# loc_matching_terminal<-loc_matching %>%  filter(Distance < 200) %>% dplyr::select(Stock_ERA, STN_ID) 
# loc_matching_terminal
# 
# 
# ##Offshore matching 
# dfo_meds_buoys_stations_offshore <- dfo_meds_buoys_locations %>% filter(STN_ID %in% c("C46184", "C46004","C46036"))
# coordinates(dfo_meds_buoys_stations_offshore) <- c("long", "lat")
# 
# # Define these vectors, used in the loop.
# closestStock_ERAVec_Offshore <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
# minDistVec_Offshore     <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
# 
# for (i in 1 : nrow(stocks_loc_simple))
# {
#   distVec_Offshore <- spDistsN1(dfo_meds_buoys_stations_offshore,stocks_loc_simple[i,],longlat = TRUE)
#   minDistVec_Offshore[i] <- min(distVec_Offshore)
#   closestStock_ERAVec_Offshore[i] <- which.min(distVec_Offshore)
# }
# 
# PointAssignTemps_Offshore <- as(dfo_meds_buoys_stations_offshore[closestStock_ERAVec_Offshore,]$STN_ID,"character")
# 
# loc_matching_offshore_1 = data.frame(coordinates(stocks_loc_simple),stocks_loc_simple$Stock_ERA,stocks_loc_simple$MapRegion,closestStock_ERAVec_Offshore,minDistVec_Offshore,PointAssignTemps_Offshore)
# names(loc_matching_offshore_1) <- c("Stock_long","Stock_lat","Stock_ERA","Region","closestStock_ERAVec_Offshore","Distance","STN_ID")
# 
# #All locations match, since offshore, okay if they are far away
# loc_matching_offshore<- loc_matching_offshore_1  %>% dplyr::select(Stock_ERA, STN_ID) %>% as_tibble
# loc_matching_offshore
# 
# 
# # Combing the match file to the data file (from cov_fetch)
# #Terminal
# dfo_meds_buoys_matched_terminal<-left_join(loc_matching_terminal, dfo_meds_buoys_combined)
# dfo_meds_buoys_matched_terminal<-dfo_meds_buoys_matched_terminal %>%  rename(cov_SST_MEDS_terminal_summer_mean = mean_summer_SSTP, 
#                                                                              cov_SST_MEDS_terminal_yearly_mean = mean_SSTP,
#                                                                              buoy_ID_terminal=STN_ID)
# 
# #Offshore
# dfo_meds_buoys_matched_offshore<-left_join(loc_matching_offshore, dfo_meds_buoys_combined)
# dfo_meds_buoys_matched_offshore<-dfo_meds_buoys_matched_offshore %>% rename(cov_SST_MEDS_offshore_summer_mean = mean_summer_SSTP, 
#                                                                             cov_SST_MEDS_offshore_yearly_mean = mean_SSTP, 
#                                                                             buoy_ID_offshore=STN_ID)
# 
# dfo_meds_buoys_matched_combined<-merge(dfo_meds_buoys_matched_terminal, dfo_meds_buoys_matched_offshore) %>% as_tibble()
# dfo_meds_buoys_matched_combined
# 

# Match IOS Zooplankton to stock ---------------------------------------------------------
#needs ios_zoop from cov_fetch
#zooplankton locations
ios_zoop_stations<- ios_zoop %>% dplyr::select(region_station, Longitude, Latitude) %>% 
                                 rename(lat= Latitude, long= Longitude) %>% 
                                 mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
                                 rowwise() %>% 
                                 distinct() %>% 
                                 na.omit
ios_zoop_stations

#transforming into sf objects
ios_zoop_stations.sf <- st_as_sf(ios_zoop_stations, coords = c("long", "lat"), crs = 4326)
ios_zoop_stations.sf<-ios_zoop_stations.sf%>% st_transform(3035)
ios_zoop_stations.sf

stocks_loc_simple.sf <- st_as_sf(stocks_loc_simple_1, coords = c("long", "lat"), crs = 4326)
stocks_loc_simple.sf<-stocks_loc_simple.sf %>% st_transform(3035)
stocks_loc_simple.sf

#Buffer circles of 200000m, i.e. 200km  -- creates polygons around the stocks
dat_circles <- st_buffer(stocks_loc_simple.sf, dist = 200000)

#which of the region_stations fall within 200km radius of each stock
ios_zoop_era_stocks<- st_join(ios_zoop_stations.sf, dat_circles, left=FALSE) %>% st_set_geometry(NULL) %>% as_tibble()
ios_zoop_era_stocks

#Apply matching to data
ios_zoop_matched <- left_join(ios_zoop_era_stocks, ios_zoop)

#Fixing problems with zeros
ios_zoop_matched<-ios_zoop_matched %>% mutate(total_zoop_biomass = case_when(total_zoop_biomass == 0 ~ runif(1, min=0, max=0.15087), TRUE ~ total_zoop_biomass))

#Transformation of the data by Stock
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
ios_zoop_anomalies<-ios_zoop_allseasons %>% dplyr::select(Stock_ERA,  calc_year, cov_zoop_winter_anomaly, cov_zoop_spring_anomaly, cov_zoop_summer_anomaly, cov_zoop_fall_anomaly) %>% 
                                            mutate(cov_zoop_yearly_anomaly = rowMeans(across(cov_zoop_winter_anomaly:cov_zoop_fall_anomaly), na.rm=TRUE))
ios_zoop_anomalies



# Herring -----------------------------------------------------------------
herring_spawn_location<- herring_spawn %>% select(LocationName, Longitude, Latitude) %>% 
  rename(lat= Latitude, long= Longitude) %>% 
  mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
  distinct() %>% 
  drop_na()
herring_spawn_location

#transforming into sf objects
herring_spawn_location.sf <- st_as_sf(herring_spawn_location, coords = c("long", "lat"), crs = 4326)
herring_spawn_location.sf<-herring_spawn_location.sf%>% st_transform(3035)
herring_spawn_location.sf

stocks_loc_simple.sf <- st_as_sf(stocks_loc_simple_1, coords = c("long", "lat"), crs = 4326)
stocks_loc_simple.sf<-stocks_loc_simple.sf %>% st_transform(3035)
stocks_loc_simple.sf

#Buffer circles of 500000m, i.e.500km  -- creates polygons around the stocks
dat_circles <- st_buffer(stocks_loc_simple.sf, dist = 500000)

#which of the region_stations fall within 100km radius of each stock
herring_spawn_stocks<- st_join(herring_spawn_location.sf, dat_circles, left=FALSE) %>% st_set_geometry(NULL) %>% as_tibble()
herring_spawn_stocks

#Apply matching to data
herring_spawn_matched <- left_join(herring_spawn_stocks, herring_spawn)

herring_spawn_matched<- herring_spawn_matched %>% group_by(year, Stock_ERA) %>% summarize(cov_herring_spawn_index_mean = mean(Surface, na.rm=TRUE))


# River discharge ---------------------------------------------------------
pacific_stations_locations<-hy_stations(prov_terr_state_loc = c("BC", "YT", "AK", "WA", "ID")) %>% dplyr::select(STATION_NUMBER, LATITUDE, LONGITUDE) %>% 
  rename(lat = LATITUDE, 
         long = LONGITUDE)


#transforming into sf objects
pacific_stations_locations.sf <- st_as_sf(pacific_stations_locations, coords = c("long", "lat"), crs = 4326)
pacific_stations_locations.sf<-pacific_stations_locations.sf%>% st_transform(3035)
pacific_stations_locations.sf


stocks_loc_simple.sf <- st_as_sf(stocks_loc_simple_1, coords = c("long", "lat"), crs = 4326)
stocks_loc_simple.sf<-stocks_loc_simple.sf %>% st_transform(3035)
stocks_loc_simple.sf

#Buffer circles of 5000m, i.e.100km  -- creates polygons around the stocks
dat_circles <- st_buffer(stocks_loc_simple.sf, dist = 100000)

#which of the region_stations fall within 100km radius of each stock
pacific_stations_stocks<- st_join(pacific_stations_locations.sf, dat_circles, left=FALSE) %>% st_set_geometry(NULL) %>% as_tibble()
pacific_stations_stocks

#Apply matching to data
pacific_stations_matched <- left_join(pacific_stations_stocks, hy_annual_wide) %>% 
                           group_by(Year, Stock_ERA) %>% 
                           summarize(cov_water_flow_yearly_max=max(cov_water_flow_yearly_max, na.rm=TRUE),  
                                     cov_water_flow_yearly_mean=max(cov_water_flow_yearly_mean, na.rm=TRUE))

water_office_matched<- left_join(pacific_stations_stocks, get_water_office_flow_2021_2022) %>% 
                       group_by(Year, Stock_ERA) %>% 
                       summarize(cov_water_flow_yearly_max=max(cov_water_flow_yearly_max, na.rm=TRUE),  
                                 cov_water_flow_yearly_mean=max(cov_water_flow_yearly_mean, na.rm=TRUE))


#08BB001,08BB002,08BB005,08CE001,08CF001,08CF003,08CG001,08CG004,08DA005,08DB001,08DC006,08DD001,08EF001,08FA002,08FB011,08FC003,08GA022,08GD004,08GE002,08HA001,08HA010,08HA011,08HA034,08HA037,08HA039,08HA047,08HA059,08HA065,08HB006,08HB010,08HB014,08HB017,08HB034,08HB092,08HC001,08HD003,08HD035,08KA004,08KH001,08LC002,08LE031,08LF051,08MB012,08MC018,08MD013,08MF005,08MF035,08MH001,08MH024,08MH103,08MH126,08ND011,08ND025,08NH118,08NH119,08NL022,08NL038,08NL071,08NM127,08NN012,08NN026,09AA012,09AA013,09AA014,09AA015


# Combing the match file to the data file (from cov_fetch)
hydro_annual_wide_matched <-  pacific_stations_matched %>% 
                              filter(Year %notin% c(2021, 2022)) %>% 
                              full_join(water_office_matched) %>% drop_na()


