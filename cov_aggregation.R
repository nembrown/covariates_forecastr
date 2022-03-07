library(tidyverse)
library(ggplot2)


# Check each of the datasets have been calculated properly ----------------

#Pacific basin-wide variables: pdo, oni, soi, npgo, npi, alpi
pdo_simple
oni_simple
soi_simple
npi_simple
npgo_simple
alpi_simple
epnp_simple

#temperature from MEDS buoys #464, since 1989
dfo_meds_buoys_matched_combined

#temp and salinity from lightstations #752, since 1900 but active lighthouses since 1976
Data_Lightstations_matched

#zooplankton from ios, since 1980
ios_zoop_anomalies<-ios_zoop_anomalies %>% rename(year = calc_year)


# Join all together -------------------------------------------------------


fcs_covariates<- merge(Data_Lightstations_matched, dfo_meds_buoys_matched_combined, by=c("Stock_ERA", "year"), all=TRUE) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates,ios_zoop_anomalies, by=c("Stock_ERA", "year"), all=TRUE)
fcs_covariates<- merge(fcs_covariates, soi_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, oni_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, npi_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, pdo_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, npgo_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, alpi_simple, by=c("year")) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates, epnp_simple, by=c("year")) %>% as_tibble()


fcs_covariates<- fcs_covariates %>% relocate(where(is.numeric), .after = where(is.character)) %>% 
                                    arrange(Stock_ERA, year) %>% 
                                    dplyr::select(- c(Lightstation,buoy_ID_terminal, Region, buoy_ID_offshore))
fcs_covariates

write.csv(fcs_covariates, "fcs_covariates.csv")



# Plotting covariates by stock --------------------------------------------


fcs_covariates_long<- fcs_covariates %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
                                         mutate(var_cat = case_when(
                                           str_detect(Covariate, "SST") ~ "Temperature", 
                                           str_detect(Covariate, "PPT") ~ "Salinity",
                                           str_detect(Covariate, "PDO") ~ "PDO", 
                                           str_detect(Covariate, "zoop") ~ "Zooplankton"
                                          )) %>% 
                                        mutate(var_timing = case_when(
                                          str_detect(Covariate, "year") ~ "Year", 
                                          str_detect(Covariate, "summer") ~ "Summer"
                                           )) %>% 
                                         mutate(var_location = case_when(
                                          str_detect(Covariate, "offshore") ~ "offshore", 
                                          str_detect(Covariate, "terminal") ~ "terminal", 
                                          TRUE ~ "terminal"
                                         ))
unique(fcs_covariates_long$Stock_ERA)

ggplot(fcs_covariates_long %>% filter(var_cat== "Zooplankton", var_timing=="Summer", var_location=="terminal"), 
       aes(x=year, y=value, col=Covariate, group=Covariate))+
        geom_point()+geom_line()+facet_wrap(~Stock_ERA, scales="free")

