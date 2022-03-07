library(tidyverse)
library(PACea)
library(rerddap)
library(curl)
library(lubridate)



# PDO (Pacific Decadal Oscillation) from NOAA ---------------------------------------------------------------------

pdo_1854_present<-read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",  header=TRUE, skip=1, fill=TRUE) %>% as_tibble()

#Note, the last year of data gets imported with -99 as the values missing appended to the previous month's value
#Makes for eg. November a character
#best to exclude Year 2022 first or if just doing summer and have full data for summer then can ignore


pdo_1854_present<-pdo_1854_present %>% rename(year = Year) %>% 
                                       filter(year!= 2022) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(cov_PDO_pacific_summer= rowMeans(dplyr::select(.,May, Jun, Jul, Aug, Sep)))  %>% 
                                       mutate(cov_PDO_pacific_year = rowMeans(dplyr::select(.,Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)))

pdo_simple<- pdo_1854_present %>% dplyr::select(year, cov_PDO_pacific_summer, cov_PDO_pacific_year) %>% 
                                  filter(year>1969)
pdo_simple



# ONI (Oceanic Nino Index) from NOAA ---------------------------------------------------------------------

oni_1950_present<-read.table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",  header=TRUE,  fill=TRUE) %>% as_tibble()

oni_1950_present<-oni_1950_present %>% rename(year = YR) %>% 
                                       filter(year!= 2022) %>% 
                                       group_by(year) %>% 
                                       summarise_if(is.numeric, mean) %>% 
                                       rename(cov_ONI_yearly_mean= TOTAL, 
                                              cov_ONI_yearly_anomaly = ANOM)


oni_simple<-oni_1950_present %>% filter(year>1969)

# SOI (Southern Oscillation Index) from NOAA ---------------------------------------------------------------------

soi_1951_present<-read.table("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/soi.long.data",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
names(soi_1951_present) <- c("year","JAN","FEB","MAR","APR","MAY","JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")

soi_1951_present<-soi_1951_present %>% filter(year!= 2022) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(cov_soi_pacific_summer= rowMeans(dplyr::select(.,MAY, JUN, JUL, AUG, SEP)))  %>% 
                                       mutate(cov_soi_pacific_year = rowMeans(dplyr::select(.,JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)))

soi_simple<- soi_1951_present %>% dplyr::select(year, cov_soi_pacific_summer, cov_soi_pacific_year) %>% 
                                  filter(year>1969)
soi_simple


# Temp and salinity from Lightstations -----------------------------------------------------------
dataset_commonname <-
  c('SST_Monthly_BC_Lightstation',
    'Salinity_Monthly_BC_Lightstation')

Lightstations <-
  sf::st_read(
    dsn=here::here('data-raw/BC_Lightstation_Data_SHP_Files/BC_Lighthouse_DATA.shp')
  )

Lightstations <-
  Lightstations %>%
  dplyr::mutate(LIGHSTATIO = recode(LIGHSTATIO,'LANGARA POINT LIGHTSTATION'='LANGARA ISLAND LIGHTSTATION'))
#plot(Lightstations)


nstations <- 12
nmonths <- 12
# 1914 is the year the first 'active' buoy went online
nyears <- length(1914:lubridate::year(Sys.Date()))

# Create a 'complete' data.frame skeleton to fill in 
Data_Lightstations <- data.frame(Location=rep(NA,nstations*nyears*nmonths),
                                 Year=rep(rep(1914:lubridate::year(Sys.Date()),
                                              each=nmonths),times=nstations), 
                                 Month=rep(1:12, times=nyears*nstations), 
                                 SST=rep(NA, nstations*nyears*nmonths),
                                 Salinity=rep(NA, nstations*nyears*nmonths),
                                 Longitude=NA,
                                 Latitude=NA)

# Loop through the currently active stations and obtain SST and salinity
count_SST <- 1
count_salinity <- 1
for(filename in list.files('data-raw/DATA_Active_lightstations',pattern = '.csv') )
{
  if(grepl(filename, pattern = 'Temperatures'))
  {
    Data_Lightstations$Location[
      (count_SST-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <-
      substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1)
    
    # Extract the lat/lon coords of the buoy
    Data_Lightstations$Longitude[
      (count_SST-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <- as.numeric(
      Lightstations$LONGITUDE[grepl(
        pattern=substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1),
        gsub(Lightstations$LIGHSTATIO, pattern = ' ', replacement = '_'),
        ignore.case = T)])
    Data_Lightstations$Latitude[
      (count_SST-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <- as.numeric(
      Lightstations$LATITUDE__[grepl(
        pattern=substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1),
        gsub(Lightstations$LIGHSTATIO, pattern = ' ', replacement = '_'),
        ignore.case = T)])
    
    # read the csv data and merge by Location Year Month
    tmp <- read.csv(paste0('data-raw/DATA_Active_Lightstations/', filename), 
                    header = T, skip = 1)
    tmp <- tmp %>%
      dplyr::select(YEAR, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC) %>%
      tidyr::pivot_longer(
        cols=c('JAN','FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
        names_to = 'Month',
        values_to = 'SST'
      ) %>%
      dplyr::mutate(Month=recode(Month, JAN=1, FEB=2, MAR=3,
                                 APR=4, MAY=5, JUN=6, JUL=7, AUG=8,
                                 SEP=9, OCT=10, NOV=11, DEC=12),
                    Location=substr(filename, start=1, 
                                    stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1)) %>%
      dplyr::rename(Year=YEAR)
    
    Data_Lightstations[(count_SST-1)*nyears*nmonths + (1:(nyears*nmonths)),] <-
      dplyr::left_join(Data_Lightstations[(count_SST-1)*nyears*nmonths + (1:(nyears*nmonths)),
                                          c('Location','Year','Month','Salinity','Longitude','Latitude')], 
                       tmp) %>%
      dplyr::select(Location,Year,Month,SST,Salinity,Longitude,Latitude)
    
    count_SST <- count_SST + 1
  }
  if(grepl(filename, pattern = 'Salinities'))
  {
    Data_Lightstations$Location[
      (count_salinity-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <-
      substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1)
    
    # Extract the lat/lon coords of the buoy
    Data_Lightstations$Longitude[
      (count_SST-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <- as.numeric(
      Lightstations$LONGITUDE[grepl(
        pattern=substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1),
        gsub(Lightstations$LIGHSTATIO, pattern = ' ', replacement = '_'),
        ignore.case = T)])
    Data_Lightstations$Latitude[
      (count_SST-1)*nyears*nmonths + (1:(nyears*nmonths))
    ] <- as.numeric(
      Lightstations$LATITUDE__[grepl(
        pattern=substr(filename, start=1, stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1),
        gsub(Lightstations$LIGHSTATIO, pattern = ' ', replacement = '_'),
        ignore.case = T)])
    
    # read the data and merge by Location, Year and Month
    tmp <- read.csv(paste0('data-raw/DATA_Active_Lightstations/', filename), 
                    header = T, skip = 1)
    tmp <- tmp %>%
      dplyr::select(YEAR, JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC) %>%
      tidyr::pivot_longer(
        cols=c('JAN','FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'),
        names_to = 'Month',
        values_to = 'Salinity'
      ) %>%
      dplyr::mutate(Month=recode(Month, JAN=1, FEB=2, MAR=3,
                                 APR=4, MAY=5, JUN=6, JUL=7, AUG=8,
                                 SEP=9, OCT=10, NOV=11, DEC=12),
                    Location=substr(filename, start=1, 
                                    stop = stringr::str_locate_all(filename,'_')[[1]][2,]-1)) %>%
      rename(Year=YEAR)
    
    Data_Lightstations[(count_salinity-1)*nyears*nmonths + (1:(nyears*nmonths)),] <-
      dplyr::left_join(Data_Lightstations[(count_salinity-1)*nyears*nmonths + (1:(nyears*nmonths)),
                                          c('Location','Year','Month','SST','Longitude','Latitude')], 
                       tmp) %>%
      dplyr::select(Location,Year,Month,SST,Salinity,Longitude,Latitude)
    
    count_salinity <- count_salinity + 1
  }
}

# Set all 999.99 values to NA
Data_Lightstations <-
  Data_Lightstations %>%
  dplyr::mutate(SST=ifelse(SST==999.99, NA, SST),
                Salinity=ifelse(Salinity==999.99, NA, Salinity),
                Longitude=ifelse(Longitude==999.99, NA, Longitude),
                Latitude=ifelse(Latitude==999.99, NA, Latitude))

### Yearly and summer data
Data_Lightstations_year_summer<- Data_Lightstations %>%  filter(Month %in% c(5,6,7,8,9)) %>% 
  rename(year = Year) %>% 
  group_by(Location, year) %>% 
  dplyr::summarise(cov_SST_lighthouse_summer = mean(SST, na.rm=TRUE), 
                   cov_PPT_lighthouse_summer = mean(Salinity, na.rm=TRUE))

Data_Lightstations_year<- Data_Lightstations  %>% 
  rename(year = Year) %>%   
  group_by(Location, year) %>% 
  dplyr::summarise(cov_SST_lighthouse_year = mean(SST, na.rm=TRUE), 
                   cov_PPT_lighthouse_year = mean(Salinity, na.rm=TRUE))

Data_Lightstations_combined<-merge(Data_Lightstations_year, Data_Lightstations_year_summer)
Data_Lightstations_combined<- Data_Lightstations_combined %>% filter(year>1975) %>% as_tibble()
Data_Lightstations_combined


# Temp from MEDS_buoys ----------------------------------------------------
dfo_meds_buoys<- tabledap('DFO_MEDS_BUOYS',  url = "https://data.cioospacific.ca/erddap/", 
                          fields = c('latitude', 'longitude', 'time', 'STN_ID', 'SSTP', 'SSTP_flags', 'Q_FLAG', 'SSTP_UQL'), 
                          'time>=1970-01-01', 'SSTP!=NaN', 'SSTP_flags!=1')

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

dfo_meds_buoys_combined<-merge(dfo_meds_buoys_year, dfo_meds_buoys_year_summer) %>% as_tibble()
dfo_meds_buoys_combined



# Zooplankton biomass from IOS --------------------------------------------

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

ios_zoop






