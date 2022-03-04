
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

# Check for missing values
summary(Data_Lightstations)

unique(Data_Lightstations$Location)
#only 12 active lightstations

###
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



####Matching
Data_Lightstations_loc<-Data_Lightstations %>% dplyr::select(Location, Longitude, Latitude) %>% 
  rowwise() %>% distinct() %>% 
  rename(long=Longitude, lat = Latitude, Site_name=Location)
Data_Lightstations_loc$site_type<- "Lighthouse"


coordinates(Data_Lightstations_loc) <- c("long", "lat")
coordinates(stocks_loc) <- c("long", "lat")             


# Define these vectors, used in the loop.
closestStock_ERAVec_light <- vector(mode = "numeric",length = nrow(stocks_loc))
minDistVec_light     <- vector(mode = "numeric",length = nrow(stocks_loc))

for (i in 1 : nrow(stocks_loc))
{
  distVec_light <- spDistsN1(Data_Lightstations_loc,stocks_loc[i,],longlat = TRUE)
  minDistVec_light[i] <- min(distVec_light)
  closestStock_ERAVec_light[i] <- which.min(distVec_light)
}

PointAssignTemps_light <- as(Data_Lightstations_loc[closestStock_ERAVec_light,]$Site_name,"character")

loc_matching_light = data.frame(coordinates(stocks_loc),stocks_loc$Stock_ERA,stocks_loc$MapRegion,closestStock_ERAVec_light,minDistVec_light,PointAssignTemps_light)
names(loc_matching_light) <- c("Stock_long","Stock_lat","Stock_ERA","Region","CloseTempIndex","Distance","Site_name")

loc_matching_light<- loc_matching_light %>% arrange(desc(Distance)) %>% as_tibble()
loc_matching_light

loc_matching_light_BC<- loc_matching_light %>% filter(Region == "BC") %>% rename(Location = Site_name)
loc_matching_light_BC

loc_matching_light_BC_simple<-loc_matching_light_BC %>% dplyr::select(Stock_ERA, Location)


## Combing the match file to the data file
Data_Lightstations_matched<-left_join(loc_matching_light_BC_simple, Data_Lightstations_combined)
Data_Lightstations_matched<- Data_Lightstations_matched %>% rename(Lightstation = Location)
Data_Lightstations_matched








### Mapping



light_stock<-bind_rows(Data_Lightstations_loc, stocks_loc_simple_BC)
###Make map
bbox_marine_light <- make_bbox(stocks_loc_simple_BC$long, stocks_loc_simple_BC$lat, f = 0.1)
map_marine_light <- get_stamenmap(bbox_marine_light, source="stamen", maptype= "terrain", crop=FALSE, zoom=8)

colorset_map_light = c("ERA Stock"="#FF007F" , "Lighthouse" ="#0000FF" )
ggmap(map_marine_light) + geom_point(data=light_stock, aes(x = long, y = lat, col=site_type), size=3) +  scale_colour_manual(values=colorset_map_light)





 
# # Make it a spatial object in sf package with lat/lon CRS
# Data_Lightstations <-
#   sf::st_as_sf(Data_Lightstations,
#                coords=c('Longitude','Latitude'),
#                crs=sf::st_crs('EPSG:4326'))
# 
# # Convert to the same CRS as the data
# Data_Lightstations <-
#   sf::st_transform(Data_Lightstations,
#                    sf::st_crs(PACea::Coastline@proj4string))
