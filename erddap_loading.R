#devtools::install_github("ropensci/rerddap")
library(rerddap)
library("akima")
library("dplyr")
library("ggplot2")
library("mapdata")
library("ncdf4")
library("plot3D")
library(lubridate)

#searching erddap for sea surface temperature
whichSST <- ed_search(query = "SST")


cioos_pacific_datasets<-ed_datasets(which = "tabledap", url = "https://data.cioospacific.ca/erddap/")
#View(cioos_pacific_datasets)

rerddap::info('DFO_MEDS_BUOYS', url = "https://data.cioospacific.ca/erddap/")

dfo_meds_buoys<- tabledap('DFO_MEDS_BUOYS',  url = "https://data.cioospacific.ca/erddap/")

dfo_meds_buoys_small <- dfo_meds_buoys %>% as_tibble %>% 
                                           select(STN_ID, time, Q_FLAG, latitude, longitude, SSTP, SSTP_flags) %>%
                                           filter(SSTP != NaN) %>% 
                                           mutate(year = lubridate::year(time), 
                                                  month = lubridate::month(time), 
                                                  day = lubridate::day(time), 
                                                  SSTP = as.numeric(SSTP))

dfo_meds_buoys_month<- dfo_meds_buoys_small %>%  group_by(STN_ID, year, month) %>% 
                                                 dplyr::summarise(mean_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_year_summer<- dfo_meds_buoys_small %>%  filter(month %in% c(5,6,7,8,9)) %>% 
                                                       group_by(STN_ID, year) %>% 
                                                       dplyr::summarise(mean_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_year<- dfo_meds_buoys_small  %>% group_by(STN_ID, year) %>% 
                                                       dplyr::summarise(mean_SSTP = mean(SSTP, na.rm=TRUE))

dfo_meds_buoys_stations <- dfo_meds_buoys_small %>% dplyr::select(STN_ID, latitude, longitude) %>% 
                                                    rename(lat= latitude, long= longitude) %>% 
                                                    mutate(lat = as.numeric(lat), long= as.numeric(long)) %>% 
                                                    rowwise() %>% 
                                                    distinct()

#from assign closest point to points
dfo_meds_buoys_matched<-left_join(loc_matching_BC, dfo_meds_buoys_stations)
