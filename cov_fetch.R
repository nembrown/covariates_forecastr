library(tidyverse)
library(PACea)
library(rerddap)
library(curl)
library(lubridate)
library(sf)
devtools::install_github("r-lib/usethis")
library(usethis)


# PDO (Pacific Decadal Oscillation) from NOAA ---------------------------------------------------------------------

pdo_1854_present<-read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",  header=TRUE, skip=1, fill=TRUE) %>% as_tibble()
pdo_1854_present<-pdo_1854_present %>% rename(year = Year) %>% 
                                       filter(year!= 2022) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(cov_PDO_summer_mean= rowMeans(dplyr::select(.,May, Jun, Jul, Aug, Sep)))  %>% 
                                       mutate(cov_PDO_yearly_mean = rowMeans(dplyr::select(.,Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)))

pdo_simple<- pdo_1854_present %>% dplyr::select(year, cov_PDO_summer_mean, cov_PDO_yearly_mean) %>% 
                                  filter(year>1969)
pdo_simple



# ONI (Oceanic Nino Index) from NOAA ---------------------------------------------------------------------

oni_1950_present<-read.table("https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt",  header=TRUE,  fill=TRUE) %>% as_tibble()
oni_simple1<-oni_1950_present %>% rename(year = YR) %>% 
                                       filter(year!= 2022) %>% 
                                       group_by(year) %>% 
                                       summarise_if(is.numeric, mean) %>% 
                                       rename(cov_ONI_yearly_mean= TOTAL, 
                                              cov_ONI_yearly_anomaly = ANOM)%>% 
                                       filter(year>1969)
oni_simple2<-oni_1950_present %>% rename(year = YR) %>% 
                                  filter(year!= 2022, SEAS %in% c("MJJ", "JJA", "JAS")) %>% 
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

soi_1951_present<-soi_1951_present %>% filter(year!= 2022) %>% 
                                       mutate_if(is.character, as.numeric) %>% 
                                       mutate(cov_SOI_summer_mean= rowMeans(dplyr::select(.,MAY, JUN, JUL, AUG, SEP)))  %>% 
                                       mutate(cov_SOI_yearly_mean = rowMeans(dplyr::select(.,JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC)))

soi_simple<- soi_1951_present %>% dplyr::select(year, cov_SOI_summer_mean, cov_SOI_yearly_mean) %>% 
                                  filter(year>1969)
soi_simple

# NPI (North Pacific Index) from NOAA ---------------------------------------------------------------------
npi_1899_present<-read.table("https://climatedataguide.ucar.edu/sites/default/files/npindex_monthly.txt",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
names(npi_1899_present)<-c("year_month", "NPI")
npi_1899_present<- npi_1899_present %>% separate(year_month, c("year", "month"), sep=4) %>% 
                                        mutate_if(is.character, as.numeric) 

npi_simple1<-npi_1899_present %>% filter(year!= 2022) %>% 
                                  group_by(year) %>% 
                                  summarise_if(is.numeric, mean) %>% 
                                  rename(cov_NPI_yearly_mean= NPI)%>% 
                                  filter(year>1969) %>% 
                                  dplyr::select(-month)

npi_simple2<-npi_1899_present %>% filter(year!= 2022, month %in% c(5,6,7,8, 9)) %>% 
                                  group_by(year) %>% 
                                  summarise_if(is.numeric, mean) %>% 
                                  rename(cov_NPI_summer_mean= NPI)%>% 
                                  filter(year>1969) %>% 
                                  dplyr::select(-month)

npi_simple<-merge(npi_simple1, npi_simple2) %>% as_tibble

#anomaly data only available by year
npi_anomaly_1899_present<-read.table("https://climatedataguide.ucar.edu/sites/default/files/npindex_anom_ndjfm.txt",  header=FALSE, skip = 1, fill=TRUE) %>% as_tibble()
names(npi_anomaly_1899_present)<-c("year", "cov_NPI_yearly_anomaly")
npi_anomaly_simple<- npi_anomaly_1899_present %>% filter(year>1969)

npi_simple<-merge(npi_simple, npi_anomaly_simple) %>% as_tibble
npi_simple  

# NPGO (North Pacific Gyre Oscillation) from E. Di Lorenzo ---------------------------------------------------------------------
npgo_1950_present<-read.table("http://www.o3d.org/npgo/npgo.php",  header=FALSE, skip = 22, fill=TRUE) %>% as_tibble()
names(npgo_1950_present)<-c("year", "month", "cov_NPGO_yearly_mean")
npgo_1950_present<-npgo_1950_present %>% mutate(year = as.numeric(year)) 

npgo_simple1<- npgo_1950_present %>% filter(year!= 2022) %>% 
                                     group_by(year) %>% 
                                     summarise_if(is.numeric, mean) %>% 
                                     dplyr::select(-month) %>% 
                                     filter(year>1969)

npgo_simple2<- npgo_1950_present %>% filter(year!= 2022, month %in% c(5,6,7,8)) %>% 
                                     group_by(year) %>% 
                                     summarise_if(is.numeric, mean) %>% 
                                     rename(cov_NPGO_summer_mean= cov_NPGO_yearly_mean)%>% 
                                     dplyr::select(-month) %>% 
                                     filter(year>1969)

npgo_simple<-merge(npgo_simple1, npgo_simple2) %>% as_tibble
npgo_simple


# EPNP (East Pacific - North Pacific Index) from NOAA ---------------------------------------------------------------------
epnp_1950_present<-read.table("https://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/epnp_index.tim",  header=TRUE, skip = 8, fill=TRUE) %>% as_tibble()

epnp_simple1<-epnp_1950_present %>% filter(YEAR!= 2022) %>% 
                                    rename(year = YEAR, cov_EPNP_yearly_mean=INDEX) %>% 
                                    group_by(year) %>% 
                                    summarise_if(is.numeric, mean) %>% 
                                    dplyr::select(-MONTH) %>% 
                                    filter(year>1969)

epnp_simple2<-epnp_1950_present %>% filter(YEAR!= 2022, MONTH %in% c(5,6,7,8, 9)) %>% 
                                    rename(year = YEAR, cov_EPNP_summer_mean=INDEX) %>% 
                                    group_by(year) %>% 
                                    summarise_if(is.numeric, mean) %>% 
                                    dplyr::select(-MONTH) %>% 
                                    filter(year>1969)

epnp_simple<- merge(epnp_simple1,epnp_simple2) %>% as_tibble
epnp_simple


# ALPI (Aleutian Low Pressure Index) from DFO -----------------------------
#data only available by year
alpi_1900_2015<-read.csv(curl('https://open.canada.ca/data/dataset/4bb821ce-bef7-46d3-95d2-064065f1bda4/resource/d7406b43-7e64-4dbe-9cf1-b932e88a3a14/download/alpi_1900_2015_en.csv')) %>%  as_tibble()
alpi_simple <- alpi_1900_2015 %>% rename(year = YEAR, 
                                            cov_ALPI_yearly_mean = ALEUTIAN.LOW.PRESSURE.INDEX..ALPI.) %>% 
                                            filter(year>1969)

alpi_simple 

# Temp and salinity from Lightstations -----------------------------------------------------------

Lightstations_data_temp<-tempfile()
curl_download('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/BCLightstations/DATA_-_Active_Sites.zip', Lightstations_data_temp)

Lightstations_shp_temp<-tempfile()
download.file('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/BClightstations/BC_Lightstation_Data_SHP_Files.zip', Lightstations_shp_temp, mode="wb")
Lightstations_shp_temp_2<-tempfile()
Lightstations_shp_temp_2<-unzip(Lightstations_shp_temp, files= "BC_Lightstation_Data_SHP Files/BC_Lighthouse_DATA.shp")


list.files(Lightstations_shp_temp)

?getURL
?unz
?unzip
?download.file
Lightstations_shp<-

dataset_commonname <-
  c('SST_Monthly_BC_Lightstation',
    'Salinity_Monthly_BC_Lightstation')
?st_read

Lightstations <- sf::st_read(dsn=Lightstations_shp_temp_2)

list.files(Lightstations_shp_temp)


file.path(Lightstations_shp_temp, "BC_Lighthouse_DATA.shp")
?list.files
#"BC_Lightstation_DATA_SHP Files",

paste(Lightstations_shp_temp,"BC_Lightstation_DATA_SHP Files", "BC_Lighthouse_DATA.shp", sep="/")

paste(Lightstations_shp_temp)
Lightstations <-
  Lightstations %>%
  dplyr::mutate(LIGHSTATIO = recode(LIGHSTATIO,'LANGARA POINT LIGHTSTATION'='LANGARA ISLAND LIGHTSTATION'))

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
for(filename in list.files(Lightstations_data_temp,pattern = '.csv') )
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
  dplyr::summarise(cov_SST_lighthouse_summer_mean = mean(SST, na.rm=TRUE), 
                   cov_PPT_lighthouse_summer_mean = mean(Salinity, na.rm=TRUE))

Data_Lightstations_year<- Data_Lightstations  %>% 
  rename(year = Year) %>%   
  group_by(Location, year) %>% 
  dplyr::summarise(cov_SST_lighthouse_yearly_mean = mean(SST, na.rm=TRUE), 
                   cov_PPT_lighthouse_yearly_mean = mean(Salinity, na.rm=TRUE))

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






