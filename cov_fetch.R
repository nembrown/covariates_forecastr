library(tidyverse)
#library(PACea)
library(rerddap)
library(curl)
library(lubridate)
library(sf)
library(data.table)
library(tidyhydat)
library(sjmisc)


# California current data -------------------------------------------------
#this is from the stoplight data found here: https://www.fisheries.noaa.gov/west-coast/science-data/ocean-conditions-indicators-trends
#add new data to the csv each year
calicur_1998_present<-read.csv("Inputs/OEI-stoplight-cvs-2023-NWFSC.csv", check.names = FALSE)
calicur_1998_present_long<- rotate_df(calicur_1998_present, rn="year", cn=TRUE) %>% as_tibble()
calicur_1998_present_long

# PDO (Pacific Decadal Oscillation) from NOAA ---------------------------------------------------------------------

pdo_1854_present<-read.table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.data",  header=FALSE, skip=1, fill=TRUE) %>% as_tibble()
pdo_1854_present<-pdo_1854_present %>% rename(year = V1, Jan = V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, Jul = V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13) %>% 
                                       filter(year!= 2025) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(cov_PDO_summer_mean= rowMeans(dplyr::select(.,May, Jun, Jul, Aug, Sep)))  %>% 
                                       mutate(cov_PDO_yearly_mean = rowMeans(dplyr::select(.,Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)))

pdo_simple<- pdo_1854_present %>% dplyr::select(year, cov_PDO_summer_mean, cov_PDO_yearly_mean) %>% 
                                  filter(year>1969)
pdo_simple


# ONI (Oceanic Nino Index) from NOAA ---------------------------------------------------------------------

oni_1950_present<-read.table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",  header=TRUE,  fill=TRUE) %>% as_tibble()
oni_simple1<-oni_1950_present %>% rename(year = YR) %>% 
                                       filter(year!= 2025) %>% 
                                       group_by(year) %>% 
                                       summarise_if(is.numeric, mean) %>% 
                                       rename(cov_ONI_yearly_mean= TOTAL, 
                                              cov_ONI_yearly_anomaly = ANOM)%>% 
                                       filter(year>1969)
oni_simple2<-oni_1950_present %>% rename(year = YR) %>% 
                                  filter(year!= 2025, SEAS %in% c("MJJ", "JJA", "JAS")) %>% 
                                  group_by(year) %>% 
                                  summarise_if(is.numeric, mean) %>% 
                                  rename(cov_ONI_summer_mean= TOTAL, 
                                         cov_ONI_summer_anomaly = ANOM)%>% 
                                  filter(year>1969)

oni_simple<- merge(oni_simple1, oni_simple2) %>% as_tibble()
oni_simple


# SOI (Southern Oscillation Index) from NOAA ---------------------------------------------------------------------

soi_1951_present<-read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/soi.long.data",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
names(soi_1951_present) <- c("year","JAN","FEB","MAR","APR","MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

#Last updated february 2024, so took out 2024, check website to see if updated
soi_1951_present<-soi_1951_present %>% filter(year < 2024) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(NOV=ifelse(NOV==-99.99, NA, NOV)) %>% 
                                       mutate(DEC=ifelse(DEC==-99.99, NA, DEC)) %>% 
                                       mutate(cov_SOI_summer_mean= rowMeans(dplyr::select(.,MAY, JUN, JUL, AUG, SEP)))  %>% 
                                       mutate(cov_SOI_yearly_mean = rowMeans(dplyr::select(.,JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)))

soi_simple<- soi_1951_present %>% dplyr::select(year, cov_SOI_summer_mean, cov_SOI_yearly_mean) %>% 
                                  filter(year>1969)

# NPI (North Pacific Index) from NOAA ---------------------------------------------------------------------
#note 2024: can't find this - it's a broken link 
# npi_1899_present<-read.table("https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
# #This is only updated to 2021 April, so cut out 2022
# names(npi_1899_present)<-c("year_month", "NPI")
# npi_1899_present<- npi_1899_present %>% separate(year_month, c("year", "month"), sep=4) %>% 
#                                         mutate_if(is.character, as.numeric) 
# 
# npi_simple1<-npi_1899_present %>% filter(year < 2021) %>% 
#                                   group_by(year) %>% 
#                                   summarise_if(is.numeric, mean) %>% 
#                                   rename(cov_NPI_yearly_mean= NPI)%>% 
#                                   filter(year>1969) %>% 
#                                   dplyr::select(-month)
# 
# npi_simple2<-npi_1899_present %>% filter(year < 2021, month %in% c(5,6,7,8, 9)) %>% 
#                                   group_by(year) %>% 
#                                   summarise_if(is.numeric, mean) %>% 
#                                   rename(cov_NPI_summer_mean= NPI)%>% 
#                                   filter(year>1969) %>% 
#                                   dplyr::select(-month)
# 
# npi_simple<-merge(npi_simple1, npi_simple2) %>% as_tibble
# 
# #anomaly data only available by year
# npi_anomaly_1899_present<-read.table("https://climatedataguide.ucar.edu/sites/default/files/npindex_anom_ndjfm.txt",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
# names(npi_anomaly_1899_present)<-c("year", "cov_NPI_yearly_anomaly")
# npi_anomaly_simple<- npi_anomaly_1899_present %>% filter(year>1969)
# 
# npi_simple<-merge(npi_simple, npi_anomaly_simple) %>% as_tibble
# npi_simple  

# NPGO (North Pacific Gyre Oscillation) from E. Di Lorenzo ---------------------------------------------------------------------
npgo_1950_present<-read.table("https://o3d.org/npgo/data/NPGO.txt",  header=FALSE, skip = 22, fill=TRUE) %>% as_tibble()
names(npgo_1950_present)<-c("year", "month", "cov_NPGO_yearly_mean")
npgo_1950_present<-npgo_1950_present %>% mutate(year = as.numeric(year)) 

#updated to sept 2024 
npgo_simple1<- npgo_1950_present %>% filter(year!= 2025) %>% 
                                     group_by(year) %>% 
                                     summarise_if(is.numeric, mean) %>% 
                                     dplyr::select(-month) %>% 
                                     filter(year>1969)

npgo_simple2<- npgo_1950_present %>% filter(year!= 2025, month %in% c(5,6,7,8,9)) %>% 
                                     group_by(year) %>% 
                                     summarise_if(is.numeric, mean) %>% 
                                     rename(cov_NPGO_summer_mean= cov_NPGO_yearly_mean)%>% 
                                     dplyr::select(-month) %>% 
                                     filter(year>1969)

npgo_simple<-merge(npgo_simple1, npgo_simple2, all=TRUE) %>% as_tibble
npgo_simple


# EPNP (East Pacific - North Pacific Index) from NOAA ---------------------------------------------------------------------
epnp_1950_present<-read.table("https://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/epnp_index.tim",  header=TRUE, skip = 8, fill=TRUE) %>% as_tibble()

#take out the -99.90 values
epnp_simple1<-epnp_1950_present %>% filter(YEAR!= 2025) %>% 
                                    mutate(INDEX=ifelse(INDEX==-99.90, NA, INDEX)) %>% 
                                    rename(year = YEAR, cov_EPNP_yearly_mean=INDEX) %>% 
                                    group_by(year) %>% 
                                    summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
                                    dplyr::select(-MONTH) %>% 
                                    filter(year>1969)

epnp_simple2<-epnp_1950_present %>% filter(YEAR!= 2025, MONTH %in% c(5,6,7,8, 9)) %>% 
                                    mutate(INDEX=ifelse(INDEX==-99.90, NA, INDEX)) %>% 
                                    rename(year = YEAR, cov_EPNP_summer_mean=INDEX) %>% 
                                    group_by(year) %>% 
                                    summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
                                    dplyr::select(-MONTH) %>% 
                                    filter(year>1969)

epnp_simple<- merge(epnp_simple1,epnp_simple2) %>% as_tibble
epnp_simple


# ALPI (Aleutian Low Pressure Index) from DFO -----------------------------
#data only available by year, only up to 2015, took out until we can get more up to date data
# alpi_1900_2015<-read.csv(curl('https://open.canada.ca/data/dataset/4bb821ce-bef7-46d3-95d2-064065f1bda4/resource/d7406b43-7e64-4dbe-9cf1-b932e88a3a14/download/alpi_1900_2015_en.csv')) %>%  as_tibble()
# alpi_simple <- alpi_1900_2015 %>% rename(year = YEAR, 
#                                             cov_ALPI_yearly_mean = ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.) %>% 
#                                             filter(year>1969)
# 
# alpi_simple 

# Temp and salinity from Lightstations -----------------------------------------------------------

#Extracts and unzips folder into your working directory
Lightstations_data_temp<-tempfile()
download.file('https://api-proxy.edh.azure.cloud.dfo-mpo.gc.ca/catalogue/records/719955f2-bf8e-44f7-bc26-6bd623e82884/attachments/Data_Active_Sites_20240620.zip', Lightstations_data_temp)
unzip(Lightstations_data_temp)

Lightstations_shp_temp<-tempfile()
download.file('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/BClightstations/BC_Lightstation_Data_SHP_Files.zip', Lightstations_shp_temp, mode="wb")
unzip(Lightstations_shp_temp)

Lightstations <- sf::st_read(dsn="BC_Lightstation_Data_SHP Files/BC_Lighthouse_DATA.shp")
Lightstations <-Lightstations %>% dplyr::mutate(LIGHSTATIO = recode(LIGHSTATIO,'LANGARA POINT LIGHTSTATION'='LANGARA ISLAND LIGHTSTATION')) %>% st_drop_geometry()
Lightstations_location<-Lightstations %>% rename(Station_ID = LIGHSTATIO, lat = LATITUDE__, long= LONGITUDE) %>% 
                                          filter(DATA_COLLE == "ACTIVE") %>% 
                                          dplyr::select(Station_ID, lat, long)  %>% as_tibble

list_of_files<-list.files('Data_Active_Sites_20240620', pattern=c('Sea_Surface_Temperature_and_Salinity.+2024.csv'), recursive=TRUE, full.names = T)
station_names<-list(Lightstations_location$Station_ID)


###If Chrome island is still broken: 
names(list_of_files)[1]<-station_names[[1]][1]
names(list_of_files)[2]<-station_names[[1]][2]
names(list_of_files)[3]<-station_names[[1]][4]
names(list_of_files)[4]<-station_names[[1]][5]
names(list_of_files)[5]<-station_names[[1]][6]
names(list_of_files)[6]<-station_names[[1]][7]
names(list_of_files)[7]<-station_names[[1]][8]
names(list_of_files)[8]<-station_names[[1]][9]
names(list_of_files)[9]<-station_names[[1]][10]
names(list_of_files)[10]<-station_names[[1]][11]
names(list_of_files)[11]<-station_names[[1]][12]


#If Chrome island is fixed: 
# names(list_of_files)[1]<-station_names[[1]][1]
# names(list_of_files)[2]<-station_names[[1]][2]
# names(list_of_files)[3]<-station_names[[1]][3]
# names(list_of_files)[4]<-station_names[[1]][4]
# names(list_of_files)[5]<-station_names[[1]][5]
# names(list_of_files)[6]<-station_names[[1]][6]
# names(list_of_files)[7]<-station_names[[1]][7]
# names(list_of_files)[8]<-station_names[[1]][8]
# names(list_of_files)[9]<-station_names[[1]][9]
# names(list_of_files)[10]<-station_names[[1]][10]
# names(list_of_files)[11]<-station_names[[1]][11]
# names(list_of_files)[12]<-station_names[[1]][12]

Lightstation_data <- list_of_files %>% map_dfr(~fread(., skip=1), .id = "Location") %>% as_tibble 
Lightstation_data<- Lightstation_data %>% rename(Date = `DATE (YYYY-MM-DD)`, 
                                                 Salinity = `SALINITY (PSS)`, 
                                                 SST = `TEMPERATURE ( C )`, 
                                                 Latitude = `LATITUDE (DECIMAL DEGREES)`, 
                                                 Longitude = `LONGITUDE (DECIMAL DEGREES)`) %>% 
                                          dplyr::select(Location, Date, Salinity, SST, Latitude, Longitude) %>% 
                                          mutate(Date = as_date(Date),
                                                 Year = lubridate::year(Date), 
                                                 Month = lubridate::month(Date), 
                                                 Day = lubridate::day(Date)) 

# Set all 999.99 values to NA
Data_Lightstations <- Lightstation_data %>% dplyr::mutate(SST=ifelse(SST >99, NA, SST),
                                                          Salinity=ifelse(Salinity>99, NA, Salinity),
                                                          Longitude=ifelse(Longitude>999.9, NA, Longitude),
                                                          Latitude=ifelse(Latitude==999.9, NA, Latitude)) %>% 
                                            rename(year = Year) 

### Yearly and summer data - summarized by month first then only use complete months 

Data_Lightstations_month<-  Data_Lightstations %>%  group_by(Location, year, Month) %>% 
                                                    dplyr::summarise(monthly_SST = mean(SST, na.rm=TRUE), 
                                                                     monthly_PPT = mean(Salinity, na.rm=TRUE))

Data_Lightstations_month_PPT<-  Data_Lightstations_month %>%  dplyr::select(Location, year, Month, monthly_PPT) %>% drop_na()
Data_Lightstations_year_summer_PPT<-  Data_Lightstations_month_PPT %>%  
                                      filter(Month %in% c(5,6,7,8,9)) %>% 
                                      dplyr::select(-Month) %>% 
                                      group_by(Location, year) %>% 
                                      dplyr::summarise(cov_PPT_lighthouse_summer_mean = mean(monthly_PPT, na.rm=TRUE), n_month = n()) %>% 
                                     # filter(n_month == 5)  %>% 
                                      dplyr::select(-n_month)

Data_Lightstations_year_PPT<- Data_Lightstations_month_PPT %>%  
                              dplyr::select(-Month) %>% 
                              group_by(Location, year) %>% 
                              dplyr::summarise(cov_PPT_lighthouse_yearly_mean = mean(monthly_PPT, na.rm=TRUE), n_month = n()) %>% 
                              #filter(n_month == 12)  %>% 
                              dplyr::select(-n_month)
                                      
Data_Lightstations_month_SST<-  Data_Lightstations_month %>%  dplyr::select(Location, year, Month, monthly_SST) %>% drop_na()
Data_Lightstations_year_summer_SST<-  Data_Lightstations_month_SST %>%  
                                      filter(Month %in% c(5,6,7,8,9)) %>% 
                                      dplyr::select(-Month) %>% 
                                      group_by(Location, year) %>% 
                                      dplyr::summarise(cov_SST_lighthouse_summer_mean = mean(monthly_SST, na.rm=TRUE), n_month = n()) %>% 
                                     # filter(n_month == 5)  %>% 
                                      dplyr::select(-n_month)

Data_Lightstations_year_SST<- Data_Lightstations_month_SST %>%  
                              dplyr::select(-Month) %>% 
                              group_by(Location, year) %>% 
                              dplyr::summarise(cov_SST_lighthouse_yearly_mean = mean(monthly_SST, na.rm=TRUE), n_month = n()) %>% 
                             # filter(n_month == 12)  %>% 
                              dplyr::select(-n_month)



Data_Lightstations_SST_combined<-merge(Data_Lightstations_year_SST, Data_Lightstations_year_summer_SST, all=TRUE)
Data_Lightstations_PPT_combined<-merge(Data_Lightstations_year_PPT, Data_Lightstations_year_summer_PPT, all=TRUE)

Data_Lightstations_combined<-merge(Data_Lightstations_PPT_combined, Data_Lightstations_SST_combined, all=TRUE)
Data_Lightstations_combined<- Data_Lightstations_combined %>% filter(year>1969) %>% as_tibble()
Data_Lightstations_combined


# Temp from MEDS_buoys took out bc better coverage from lighthouse data ----------------------------------------------------
# dfo_meds_buoys<- tabledap('DFO_MEDS_BUOYS',  url = "https://data.cioospacific.ca/erddap/", 
#                           fields = c('latitude', 'longitude', 'time', 'STN_ID', 'SSTP', 'SSTP_flags', 'Q_FLAG', 'SSTP_UQL'), 
#                           'time>=1970-01-01', 'SSTP!=NaN', 'SSTP_flags!=1')
# 
# dfo_meds_buoys_small <- dfo_meds_buoys %>% as_tibble %>% 
#                                            filter(SSTP_UQL %in% c(1,2)) %>% 
#                                            filter(Q_FLAG != 4) %>% 
#                                            mutate(year = lubridate::year(time), 
#                                                   month = lubridate::month(time), 
#                                                   day = lubridate::day(time), 
#                                                   SSTP = as.numeric(SSTP))
# 
# #want to do yearly sums of only complete monthly data, so first summarize by month
# dfo_meds_buoys_month<- dfo_meds_buoys_small %>%  group_by(STN_ID, year, month) %>% 
#                                                  dplyr::summarise(monthly_SSTP = mean(SSTP, na.rm=TRUE))  %>% 
#                                                  drop_na()
# 
# dfo_meds_buoys_year_summer<- dfo_meds_buoys_month %>%  filter(month %in% c(5,6,7,8,9)) %>% 
#                                                        group_by(STN_ID, year) %>% 
#                                                        dplyr::summarise(mean_summer_SSTP = mean(monthly_SSTP), n_month=n()) %>% 
#                                                       # filter(n_month == 5) %>% 
#                                                        dplyr::select(-n_month)
# 
# dfo_meds_buoys_year<- dfo_meds_buoys_month  %>% group_by(STN_ID, year) %>% 
#                                                 dplyr::summarise(mean_SSTP = mean(monthly_SSTP), n_month=n()) %>% 
#                                                # filter(n_month == 12) %>% 
#                                                 dplyr::select(-n_month)
# 
# dfo_meds_buoys_combined<-merge(dfo_meds_buoys_year, dfo_meds_buoys_year_summer) %>% as_tibble()
# dfo_meds_buoys_combined

# Zooplankton biomass from IOS --------------------------------------------
#only updated to 2018 # get file from Akash Sastri
#ios_zoop_base<-read.csv(curl('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Pacific_Zooplankton/IOS_zoop_vnh_biomass_major_taxa_1980_2018_V1.csv')) %>%  as_tibble()
ios_zoop_base<-read.csv("Inputs/IOS Zoop biomass major taxa 1980-2023.csv", fileEncoding = "Latin1")

ios_zoop <- ios_zoop_base  %>% filter(Mesh..µm. <300, Net != "Bongo ONH") %>% 
                               mutate(Euphausiacea = case_when(Twilight == "Daylight" ~ Euphausiacea*3), 
                                      total_zoop_biomass = rowSums(across(Polychaeta:Animalia), na.rm=TRUE), 
                                      Date = as_date(Date, format = "%m/%d/%Y"),
                                      year = lubridate::year(Date), 
                                      month = lubridate::month(Date), 
                                      day = lubridate::day(Date), 
                                      season = case_when(month %in% c(12, 1, 2) ~ "winter", 
                                                         month %in% c(3, 4, 5) ~ "spring", 
                                                         month %in% c(6, 7, 8) ~ "summer", 
                                                         month %in% c(9, 10, 11) ~ "fall"), 
                                      calc_year = case_when(month == 12 ~ (year + 1), TRUE ~ year)) %>%                                         
                                mutate(region_station = paste(Region_name, Station, sep="-")) %>% 
                                as_tibble()

ios_zoop


# Herring Spawn Index --------------------------------------------

#online is only updated to 2022
herring_spawn<-read.csv(curl('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Pacific_Herring_Spawn_Index_Data/Pacific_herring_spawn_index_data_EN.csv')) %>%  as_tibble()

herring_spawn <- herring_spawn %>% filter(Method == "Surface") %>% 
  mutate(Date = as_date(StartDate, format = "%Y-%m-%d"),
         year = lubridate::year(Date), 
         month = lubridate::month(Date), 
         day = lubridate::day(Date)) %>% 
         filter(year > 1969) 

herring_spawn 



# Hydat -------------------------------------------------------------------


#download_hydat()

hy_annual<-hy_annual_stats(prov_terr_state_loc = c("BC", "YT", "AK", "WA", "ID"))
hy_annual_wide<- hy_annual %>% filter(Sum_stat %in%  c("MEAN", "MAX"), Parameter %in%  c("Flow")) %>% 
                               select(-Date, -Symbol) %>% 
                               pivot_wider(names_from = c(Parameter, Sum_stat), values_from = Value) %>% 
                               rename(cov_water_flow_yearly_mean = Flow_MEAN, 
                                      cov_water_flow_yearly_max = Flow_MAX) %>% 
                              filter(Year>1969) %>% 
                              drop_na()



#generated from: https://wateroffice.ec.gc.ca/services/real_time_links_e.html
#just for certain stations that do have 2022 data in hydat, so they will be current/continue the time series

get_water_office_flow_2023_2024<-read.csv(curl('https://wateroffice.ec.gc.ca/services/real_time_data/csv/inline?stations[]=08BB001&stations[]=08BB002&stations[]=08BB005&stations[]=08CE001&stations[]=08CF001&stations[]=08CF003&stations[]=08CG001&stations[]=08CG004&stations[]=08DA005&stations[]=08DB001&stations[]=08DC006&stations[]=08DD001&stations[]=08EF001&stations[]=08FA002&stations[]=08FB011&stations[]=08FC003&stations[]=08GA022&stations[]=08GD004&stations[]=08GE002&stations[]=08HA001&stations[]=08HA010&stations[]=08HA011&stations[]=08HA034&stations[]=08HA037&stations[]=08HA039&stations[]=08HA047&stations[]=08HA059&stations[]=08HA065&stations[]=08HB006&stations[]=08HB010&stations[]=08HB014&stations[]=08HB017&stations[]=08HB034&stations[]=08HB092&stations[]=08HC001&stations[]=08HD003&stations[]=08HD035&stations[]=08KA004&stations[]=08KH001&stations[]=08LC002&stations[]=08LE031&stations[]=08LF051&stations[]=08MB012&stations[]=08MC018&stations[]=08MD013&stations[]=08MF005&stations[]=08MF035&stations[]=08MH001&stations[]=08MH024&stations[]=08MH103&stations[]=08MH126&stations[]=08ND011&stations[]=08ND025&stations[]=08NH118&stations[]=08NH119&stations[]=08NL022&stations[]=08NL038&stations[]=08NL071&stations[]=08NM127&stations[]=08NN012&stations[]=08NN026&stations[]=09AA012&stations[]=09AA013&stations[]=09AA014&stations[]=09AA015&parameters[]=47&start_date=2023-01-01%2000:00:00&end_date=2024-12-31%2023:59:59')) %>% as_tibble 


get_water_office_flow_2023_2024<-get_water_office_flow_2023_2024 %>%                           
                                 rename(STATION_NUMBER = X.ID) %>% 
                                 mutate(Year=lubridate::year(Date)) %>% 
                                 group_by(STATION_NUMBER, Year) %>% 
                                 summarise(cov_water_flow_yearly_mean = mean(Value.Valeur, na.rm=TRUE), 
                                           cov_water_flow_yearly_max = max(Value.Valeur, na.rm=TRUE)) %>% 
                                drop_na()



### adding model EVs back in 
# Model EVs ---------------------------------------------------------------
col_names_list<-c("stock", paste(1974:2026))
model_EVs<-read.table("Inputs/2304B.EVO", skip=2, col.names= col_names_list, check.names = FALSE) %>% as_tibble()
model_stocks<-read.csv("Inputs/stockCodes.csv") 
model_EVs <- model_EVs %>% pivot_longer(cols = 2:52, names_to = "year", values_to = "cov_model_EVs") %>% mutate(year= as.numeric(year))
model_EVs_stocks<-merge(model_EVs, model_stocks) %>% dplyr::select(Stock_ERA, Stock_model, year, cov_model_EVs) %>% filter(year< 2025) %>% as_tibble
model_EVs_stocks<- model_EVs_stocks %>% complete(year=1970:2023, nesting(Stock_ERA, Stock_model))
model_EVs_stocks