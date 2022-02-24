
# loading libraries
library(dplyr)
library(PACea)


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
plot(Lightstations)


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
for(filename in list.files('./DATA_Active_lightstations',pattern = '.csv') )
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
    tmp <- read.csv(paste0('./DATA_Active_Lightstations/', filename), 
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
      select(Location,Year,Month,SST,Salinity,Longitude,Latitude)
    
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
    tmp <- read.csv(paste0('./DATA_Active_Lightstations/', filename), 
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
      select(Location,Year,Month,SST,Salinity,Longitude,Latitude)
    
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

# Check for missing values
summary(Data_Lightstations)

# Make it a spatial object in sf package with lat/lon CRS
Data_Lightstations <-
  sf::st_as_sf(Data_Lightstations,
               coords=c('Longitude','Latitude'),
               crs=sf::st_crs('EPSG:4326'))

# Convert to the same CRS as the data
Data_Lightstations <-
  sf::st_transform(Data_Lightstations,
                   sf::st_crs(PACea::Coastline@proj4string))
