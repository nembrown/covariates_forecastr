library(tidyverse)
library(ggplot2)
library(tsibble)


# Check each of the datasets have been calculated properly ----------------

#Pacific basin-wide variables: pdo, oni, soi, npgo, npi, alpi
pdo_simple
oni_simple
soi_simple
#npi_simple
npgo_simple
epnp_simple
#alpi_simple


#take out the station ID parts
#temperature from MEDS buoys #464, since 1989
#dfo_meds_buoys_matched_combined<-dfo_meds_buoys_matched_combined %>% dplyr::select(-c(buoy_ID_terminal, buoy_ID_offshore)) %>% filter(year!=2023)

#temp and salinity from lightstations #752, since 1900 but active lighthouses since 1976
Data_Lightstations_matched<-Data_Lightstations_matched %>% dplyr::select(-Lightstation) %>% filter(year!=2023)

#zooplankton from ios, since 1980
ios_zoop_anomalies<-ios_zoop_anomalies %>% rename(year = calc_year) %>% filter(year!=2023)

#Hydro stations, take out 
hydro_stations_matched<-hydro_annual_wide_matched %>% rename(year = Year) 


#model_EVs_stocks
#model_EVs_stocks

#herring
herring_spawn_matched

# Join all together -------------------------------------------------------

#start with atmospheric bc longer time series usually and no stocks
fcs_covariates_atm<- merge(oni_simple, soi_simple, by=c("year"), all=TRUE) %>% as_tibble()
#fcs_covariates_atm<- merge(fcs_covariates_atm, npi_simple, by=c("year"), all=TRUE) %>% as_tibble()
fcs_covariates_atm<- merge(fcs_covariates_atm, pdo_simple, by=c("year"), all=TRUE) %>% as_tibble()
fcs_covariates_atm<- merge(fcs_covariates_atm, npgo_simple, by=c("year"), all=TRUE) %>% as_tibble()
fcs_covariates_atm<- merge(fcs_covariates_atm, epnp_simple, by=c("year"), all=TRUE) %>% as_tibble()
#fcs_covariates_atm<- merge(fcs_covariates_atm, alpi_simple, by=c("year"), all=TRUE) %>% as_tibble()
fcs_covariates_atm

#Then join stock-wise
fcs_covariates<- merge(Data_Lightstations_matched, ios_zoop_anomalies, by=c("Stock_ERA", "year"), all=TRUE) %>% as_tibble
fcs_covariates<- merge(fcs_covariates,herring_spawn_matched, by=c("Stock_ERA", "year"), all=TRUE) %>% as_tibble
fcs_covariates<- merge(fcs_covariates,hydro_stations_matched, by=c("Stock_ERA", "year"), all=TRUE) %>% as_tibble
fcs_covariates

#then combine atmospheric and by-stock - data frame year and stock 
stocks_year <- stocks_loc_simple_1 %>% dplyr::select(-c(lat, long)) %>% add_column(year = c(1970:2024))
stocks_year <- stocks_year %>% expand(Stock_ERA, year) %>% filter(year<2023)
stocks_year 



fcs_covariates_combined<-merge(stocks_year, fcs_covariates, all=TRUE) %>% as_tibble()
fcs_covariates_combined<-merge(fcs_covariates_combined, fcs_covariates_atm , by=c("year"), all=TRUE) %>% as_tibble()
fcs_covariates_combined

fcs_covariates_combined<- fcs_covariates_combined %>% relocate(Stock_ERA, year) %>% 
                          arrange(Stock_ERA, year) %>% 
                          filter(year>1978)

write.csv(fcs_covariates_combined, "fcs_covariates.csv", row.names = FALSE)


#### Interpolation of missing values

# fcs_covariates_combined_ts<-fcs_covariates_combined %>% as_tsibble(key=c(Stock_ERA), index=year)
# fcs_covariates_combined_ts_interpolated<- fcs_covariates_combined_ts %>% model(arima = MEAN(cov_herring_spawn_index_mean ~ trend())) %>% interpolate(fcs_covariates_combined_ts)

fcs_covariates_combined_interpolated<- fcs_covariates_combined %>% group_by(Stock_ERA) %>% arrange(Stock_ERA, year) %>% mutate_at(vars(starts_with("cov")), funs(na.approx(., maxgap=3, rule=2)))


write.csv(fcs_covariates_combined_interpolated, "fcs_covariates_interpolated.csv", row.names = FALSE)


# Add in metadata------------------------------------------------------

#Summary table
cov_meta <- data.frame(cov_name=character(), 
                       cov_type=character(), 
                       cov_source_station_type=character(), 
                       cov_source_method=character(), 
                       cov_source=character(), 
                       cov_temporal=character(),
                       cov_unit=character(),
                       match_type=character(), 
                       match_spatial=character(), 
                       date_range=character(), stringsAsFactors=FALSE)

cov_meta <- cov_meta  %>% 
            add_row(cov_name="cov_SST_lighthouse_yearly_mean",  cov_type="Temperature", cov_source_station_type="Lighthouse", cov_source="DFO", cov_temporal="Year", cov_unit="Sea surface temperature",match_type="Point", match_spatial="terminal", date_range= "1914-present") %>% 
            add_row(cov_name="cov_PPT_lighthouse_yearly_mean",  cov_type="Salinity", cov_source_station_type="Lighthouse", cov_source="DFO", cov_temporal="Year", cov_unit="Salinity parts per thousand",match_type="Point", match_spatial="terminal", date_range= "1914-present") %>% 
            add_row(cov_name="cov_SST_lighthouse_summer_mean",  cov_type="Temperature", cov_source_station_type="Lighthouse", cov_source="DFO", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Sea surface temperature",match_type="Point", match_spatial="terminal", date_range= "1914-present") %>% 
            add_row(cov_name="cov_PPT_lighthouse_summer_mean",  cov_type="Salinity", cov_source_station_type="Lighthouse", cov_source="DFO", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Salinity parts per thousand",match_type="Point", match_spatial="terminal", date_range= "1914-present")  %>% 
            add_row(cov_name="cov_SST_MEDS_terminal_yearly_mean",  cov_type="Temperature", cov_source_station_type="MEDS buoys", cov_source="DFO", cov_temporal="Year", cov_unit="Sea surface temperature",match_type="Point", match_spatial="terminal", date_range= "1989-present")  %>% 
            add_row(cov_name="cov_SST_MEDS_terminal_summer_mean",  cov_type="Temperature", cov_source_station_type="MEDS buoys", cov_source="DFO", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Sea surface temperature",match_type="Point", match_spatial="terminal", date_range= "1989-present")  %>% 
            add_row(cov_name="cov_SST_MEDS_offshore_yearly_mean",  cov_type="Temperature", cov_source_station_type="MEDS buoys", cov_source="DFO", cov_temporal="Year", cov_unit="Sea surface temperature",match_type="Point", match_spatial="offshore", date_range= "1989-present")  %>%
            add_row(cov_name="cov_SST_MEDS_offshore_summer_mean",  cov_type="Temperature", cov_source_station_type="MEDS buoys", cov_source="DFO", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Sea surface temperature",match_type="Point", match_spatial="offshore", date_range= "1989-present")  %>%
            add_row(cov_name="cov_zoop_winter_anomaly",  cov_type="Zooplankton", cov_source_station_type="IOS Stations", cov_source="DFO", cov_temporal="Winter (Dec - Feb inclusive)", cov_unit="Zooplankton biomass anomaly",match_type="Radius (500km)", match_spatial="terminal", date_range= "1980-present")  %>%
            add_row(cov_name="cov_zoop_spring_anomaly",  cov_type="Zooplankton", cov_source_station_type="IOS Stations", cov_source="DFO", cov_temporal="Spring (March - May inclusive)", cov_unit="Zooplankton biomass anomaly",match_type="Radius (500km)", match_spatial="terminal", date_range= "1980-present")  %>%
            add_row(cov_name="cov_zoop_summer_anomaly",  cov_type="Zooplankton", cov_source_station_type="IOS Stations", cov_source="DFO", cov_temporal="Summer (June - Aug inclusive)", cov_unit="Zooplankton biomass anomaly",match_type="Radius (500km)", match_spatial="terminal", date_range= "1980-present")  %>%
            add_row(cov_name="cov_zoop_fall_anomaly",  cov_type="Zooplankton", cov_source_station_type="IOS Stations", cov_source="DFO", cov_temporal="Fall (Sept - Nov inclusive)", cov_unit="Zooplankton biomass anomaly",match_type="Radius (500km)", match_spatial="terminal", date_range= "1980-present")  %>%
            add_row(cov_name="cov_zoop_year_anomaly",  cov_type="Zooplankton", cov_source_station_type="IOS Stations", cov_source="DFO", cov_temporal="Year", cov_unit="Zooplankton biomass anomaly",match_type="Radius (100km)", match_spatial="terminal", date_range= "1980-present")  %>%
            add_row(cov_name="cov_SOI_summer_mean",  cov_type="Atmospheric Index", cov_source_method="Tahiti and Darwin", cov_source="NOAA", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Southern Oscillation Index",match_type="none", match_spatial="basin", date_range= "1951-present")  %>%
            add_row(cov_name="cov_SOI_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="Tahiti and Darwin", cov_source="NOAA", cov_temporal="Year", cov_unit="Southern Oscillation Index",match_type="none", match_spatial="basin", date_range= "1951-present")  %>%
            add_row(cov_name="cov_ONI_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="Nino 3.4", cov_source="NOAA", cov_temporal="Year", cov_unit="Ocean Nino Index",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            add_row(cov_name="cov_ONI_yearly_anomaly",  cov_type="Atmospheric Index", cov_source_method="Nino 3.4", cov_source="NOAA", cov_temporal="Year", cov_unit="Ocean Nino Index anomaly",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            add_row(cov_name="cov_ONI_summer_mean",  cov_type="Atmospheric Index", cov_source_method="Nino 3.4", cov_source="NOAA", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Ocean Nino Index",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            add_row(cov_name="cov_ONI_summer_anomaly",  cov_type="Atmospheric Index", cov_source_method="Nino 3.4", cov_source="NOAA", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Ocean Nino Index anomaly",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            #add_row(cov_name="cov_NPI_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="Trenberth and Hurrell", cov_source="NCAR", cov_temporal="Year", cov_unit="North Pacific Index",match_type="none", match_spatial="basin", date_range= "1899-present")  %>%
            #add_row(cov_name="cov_NPI_summer_mean",  cov_type="Atmospheric Index", cov_source_method="Trenberth and Hurrell", cov_source="NCAR", cov_temporal="Summer (May - Sept inclusive)", cov_unit="North Pacific Index",match_type="none", match_spatial="basin", date_range= "1899-present")  %>%
            #add_row(cov_name="cov_NPI_yearly_anomaly",  cov_type="Atmospheric Index", cov_source_method="Trenberth and Hurrell", cov_source="NCAR", cov_temporal="Year", cov_unit="North Pacific Index anomaly",match_type="none", match_spatial="basin", date_range= "1899-present")  %>%
            add_row(cov_name="cov_PDO_summer_mean",  cov_type="Atmospheric Index", cov_source_method="ERSST Version 5", cov_source="NOAA", cov_temporal="Summer (May - Sept inclusive)", cov_unit="Pacific Decadal Oscillation",match_type="none", match_spatial="basin", date_range= "1854-present")  %>%
            add_row(cov_name="cov_PDO_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="ERSST Version 5", cov_source="NOAA", cov_temporal="Year", cov_unit="Pacific Decadal Oscillation",match_type="none", match_spatial="basin", date_range= "1854-present")  %>%
            add_row(cov_name="cov_NPGO_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="AVISO Satellite SSHa", cov_source="E. di Loranzo", cov_temporal="Year", cov_unit="North Pacific Gyre Oscillation",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            add_row(cov_name="cov_NPGO_summer_mean",  cov_type="Atmospheric Index", cov_source_method="AVISO Satellite SSHa", cov_source="E. di Loranzo", cov_temporal="Summer (May - Sept inclusive)", cov_unit="North Pacific Gyre Oscillation",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            #add_row(cov_name="cov_ALPI_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="Surry and King", cov_source="DFO", cov_temporal="Year", cov_unit="Aleutian Low Pressure Index",match_type="none", match_spatial="basin", date_range= "1900-2015")  %>%
            add_row(cov_name="cov_EPNP_yearly_mean",  cov_type="Atmospheric Index", cov_source_method="Bell and Janowiak", cov_source="NOAA", cov_temporal="Year", cov_unit="East Pacific - North Pacific Index",match_type="none", match_spatial="basin", date_range= "1950-present")  %>%
            add_row(cov_name="cov_EPNP_summer_mean",  cov_type="Atmospheric Index", cov_source_method="Bell and Janowiak", cov_source="NOAA", cov_temporal="Summer (May - Sept inclusive)", cov_unit="East Pacific - North Pacific Index",match_type="none", match_spatial="basin", date_range= "1950-present") %>% 
            add_row(cov_name="cov_herring_spawn_index_mean",  cov_type="Herring Spawn Index", cov_source_method="Spawn Index R package", cov_source="DFO", cov_temporal="Year", cov_unit="Herring Spawn Index",match_type="Radius (500km)", match_spatial="terminal", date_range= "1950-present") %>% 
            add_row(cov_name="cov_water_level_yearly_mean",  cov_type="Hydrographic", cov_source_method="tidyhydat package", cov_source="Water Survey of Canada", cov_temporal="Year", cov_unit="Water level",match_type="Point", match_spatial="terminal", date_range= "1900-present") %>% 
            add_row(cov_name="cov_water_flow_yearly_mean",  cov_type="Hydrographic", cov_source_method="tidyhydat package", cov_source="Water Survey of Canada", cov_temporal="Year", cov_unit="Water level",match_type="Point", match_spatial="terminal", date_range= "1900-present") 

# Metadata locations ------------------------------------------------------
#Lightstations
loc_matching_light_meta<- merge(loc_matching_light, Data_Lightstations_locations) %>% dplyr::select(-closestStock_ERAVec_light) %>% as_tibble
loc_matching_light_meta

#Offshore meds buoys
dfo_meds_buoys_locations_offshore<-dfo_meds_buoys_locations %>% filter(STN_ID %in% c("C46184", "C46004","C46036"))
loc_matching_offshore_meta <- merge(loc_matching_offshore_1, dfo_meds_buoys_locations_offshore) %>%  dplyr::select(-closestStock_ERAVec_Offshore)  %>% mutate(site_type= "Offshore MEDS buoy") %>%  rename(Site_name = STN_ID)%>% as_tibble
loc_matching_offshore_meta

#Terminal meds buoys
loc_matching_meta <- merge(loc_matching, dfo_meds_buoys_locations) %>% dplyr::select(-closestStock_ERAVec)  %>%  rename(Site_name = STN_ID) %>% as_tibble
loc_matching_meta

stations_meta<-bind_rows(loc_matching_light_meta, loc_matching_offshore_meta, loc_matching_meta)
stations_meta<- stations_meta %>% relocate(Region, Stock_ERA, Stock_long, Stock_lat, site_type, Site_name, long, lat, Distance ) %>% arrange(Region, Stock_ERA)
stations_meta

# Writing to excel file --------------------------------------------------------


cov_sheet_list<-list("Covariate Metadata"=cov_meta,
                     "Station Metadata"=stations_meta,
                     "fcs_covariates"=fcs_covariates_combined
                     )



writexl::write_xlsx(cov_sheet_list, "fcs_covariates_meta.xlsx")

