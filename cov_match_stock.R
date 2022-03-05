library(tidyverse)
library(sf)


# ERA Stock Locations -----------------------------------------------------
stocks_loc<-read.csv("data-raw/stocks.csv") 
#note terminal - I manually mapped these, so that for eg. the fraser river stocks would map to the mouth of the fraser and not interior
stocks_loc<- stocks_loc %>%  as_tibble() %>% 
                             dplyr::select(Stock_ERA, StockName, SiteName, MapRegion,Lat, Long, Lat_terminal, Long_terminal) %>% 
                             rename(long=Long_terminal, lat=Lat_terminal)

#new dataframe that can be changed into a spatial object for distance matching 
stocks_loc_simple<-stocks_loc
coordinates(stocks_loc_simple) <- c("long", "lat")   
stocks_loc_simple

#new dataframe that can be changed into spatial for radius matching 
stocks_loc_simple_1 <- stocks_loc %>% dplyr::select(Stock_ERA, lat, long)
stocks_loc_simple_1

# Match Light stations to ERA Stocks --------------------------------------------
#Needs Data_Lightstations from cov_fetch 
Data_Lightstations_locations<-Data_Lightstations %>% dplyr::select(Location, Longitude, Latitude) %>% 
                                                     rowwise() %>% distinct() %>% 
                                                     rename(long=Longitude, lat = Latitude, Site_name=Location) %>% 
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

loc_matching_light = data.frame(coordinates(stocks_loc_simple),stocks_loc_simple$Stock_ERA,stocks_loc_simple$MapRegion,closestStock_ERAVec_light,minDistVec_light,PointAssignTemps_light)
names(loc_matching_light) <- c("Stock_long","Stock_lat","Stock_ERA","Region","CloseTempIndex","Distance","Site_name")

loc_matching_light<- loc_matching_light %>% arrange(desc(Distance)) %>% as_tibble()
loc_matching_light

loc_matching_light_BC<- loc_matching_light %>% filter(Region == "BC") %>% rename(Location = Site_name)
loc_matching_light_BC

loc_matching_light_BC_simple<-loc_matching_light_BC %>% dplyr::select(Stock_ERA, Location)
loc_matching_light_BC_simple

# Combing the match file to the data file (from cov_fetch)
Data_Lightstations_matched<-left_join(loc_matching_light_BC_simple, Data_Lightstations_combined)
Data_Lightstations_matched<- Data_Lightstations_matched %>% rename(Lightstation = Location)
Data_Lightstations_matched



# Match MEDS buoys to ERA Stocks ------------------------------------------------
#needs dfo_meds_buoy_small and dfo_meds_buoys_combined from cov_fetch 
dfo_meds_buoys_locations <- dfo_meds_buoys_small %>% dplyr::select(STN_ID, latitude, longitude) %>% 
                                                    rename(lat= latitude, long= longitude) %>% 
                                                    mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
                                                    filter(STN_ID != "C46134") %>% 
                                                    rowwise() %>% 
                                                    distinct() %>% 
                                                    add_column(site_type = "MEDS buoy")

#New dataframe that can be changed into  a spatial object
dfo_meds_buoys_stations <- dfo_meds_buoys_locations
coordinates(dfo_meds_buoys_stations) <- c("long", "lat")

# Define these vectors, used in the loop.
closestStock_ERAVec <- vector(mode = "numeric",length = nrow(stocks_loc_simple))
minDistVec     <- vector(mode = "numeric",length = nrow(stocks_loc_simple))

for (i in 1 : nrow(stocks_loc_simple))
{
  distVec <- spDistsN1(dfo_meds_buoys_stations,stocks_loc_simple[i,],longlat = TRUE)
  minDistVec[i] <- min(distVec)
  closestStock_ERAVec[i] <- which.min(distVec)
}

PointAssignTemps <- as(dfo_meds_buoys_stations[closestStock_ERAVec,]$STN_ID,"character")

loc_matching = data.frame(coordinates(stocks_loc_simple),stocks_loc_simple$Stock_ERA,stocks_loc_simple$MapRegion,closestStock_ERAVec,minDistVec,PointAssignTemps)
names(loc_matching) <- c("Stock_long","Stock_lat","Stock_ERA","Region","CloseTempIndex","Distance","STN_ID")

loc_matching<- loc_matching %>% arrange(desc(Distance)) %>% as_tibble()
loc_matching_BC<- loc_matching %>% filter(Region == "BC")
loc_matching_BC_terminal<-loc_matching_BC %>%  dplyr::select(Stock_ERA, STN_ID)
loc_matching_BC_offshore<-loc_matching_BC_terminal %>% mutate(STN_ID = case_when(
                                                       Stock_ERA == "KLM/KLY" ~ "C46184",
                                                       Stock_ERA == "ATN/ATS" ~ "C46004",
                                                       TRUE ~ "C46036"))

# Combing the match file to the data file (from cov_fetch)
#Terminal
dfo_meds_buoys_matched_terminal<-left_join(loc_matching_BC_terminal, dfo_meds_buoys_combined)
dfo_meds_buoys_matched_terminal<-dfo_meds_buoys_matched_terminal %>% 
  rename(cov_SSTP_terminal_summer = mean_summer_SSTP, 
         cov_SSTP_terminal_year = mean_SSTP) %>% rename(buoy_ID_terminal=STN_ID)
#Offshore
dfo_meds_buoys_matched_offshore<-left_join(loc_matching_BC_offshore, dfo_meds_buoys_combined)
dfo_meds_buoys_matched_offshore<-dfo_meds_buoys_matched_offshore %>% rename(cov_SSTP_offshore_summer = mean_summer_SSTP, 
                                                                     cov_SSTP_offshore_year = mean_SSTP) %>% rename(buoy_ID_offshore=STN_ID)

dfo_meds_buoys_matched_combined<-merge(dfo_meds_buoys_matched_terminal, dfo_meds_buoys_matched_offshore) %>% as_tibble()
dfo_meds_buoys_matched_combined


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

#Buffer circles of 100000m, i.e. 100km  -- creates polygons around the stocks
dat_circles <- st_buffer(stocks_loc_simple.sf, dist = 100000)

#which of the region_stations fall within 100km radius of each stock
ios_zoop_era_stocks<- st_join(ios_zoop_stations.sf, dat_circles, left=FALSE) %>% st_set_geometry(NULL) %>% as_tibble()
ios_zoop_era_stocks

#Apply matching to data
ios_zoop_matched <- left_join(ios_zoop_era_stocks, ios_zoop)

#Fixing problems with zeros
ios_zoop_matched<-ios_zoop_matched %>% mutate(total_zoop_biomass = case_when(total_zoop_biomass == 0 ~ 0.07, TRUE ~ total_zoop_biomass))

View(ios_zoop_matched)

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
ios_zoop_anomalies<-ios_zoop_allseasons %>% dplyr::select(Stock_ERA, calc_year, cov_zoop_winter_anomaly, cov_zoop_spring_anomaly, cov_zoop_summer_anomaly, cov_zoop_fall_anomaly) %>% 
                                            mutate(cov_zoop_yearly_anomaly = rowMeans(across(cov_zoop_winter_anomaly:cov_zoop_fall_anomaly), na.rm=TRUE))
ios_zoop_anomalies
