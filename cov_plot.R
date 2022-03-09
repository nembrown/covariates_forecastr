
library(ggplot2)
library(tidyverse)


# Plotting by stock -------------------------------------------------------
fcs_covariates_combined

fcs_covariates_long<- fcs_covariates_combined %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
  mutate(var_cat = case_when(
    str_detect(Covariate, "SST") ~ "Temperature", 
    str_detect(Covariate, "PPT") ~ "Salinity",
    str_detect(Covariate, "zoop") ~ "Zooplankton", 
    str_detect(Covariate, "ALPI") ~ "ALPI", 
    str_detect(Covariate, "PDO") ~ "PDO", 
    str_detect(Covariate, "SOI") ~ "SOI", 
    str_detect(Covariate, "ONI") ~ "ONI", 
    str_detect(Covariate, "NPI") ~ "NPI", 
    str_detect(Covariate, "NPGO") ~ "NPGO", 
    str_detect(Covariate, "EPNP") ~ "EPNP"  )) %>% 
  mutate(var_timing = case_when(
    str_detect(Covariate, "year") ~ "Year", 
    str_detect(Covariate, "summer") ~ "Summer")) %>% 
  mutate(var_location = case_when(
    str_detect(Covariate, "offshore") ~ "offshore", 
    str_detect(Covariate, "terminal") ~ "terminal", 
    TRUE ~ "terminal")) 


unique(fcs_covariates_long$Stock_ERA)

ggplot(fcs_covariates_long %>% filter(var_cat== "Zooplankton", var_timing=="Summer", var_location=="terminal"), 
       aes(x=year, y=value, col=Covariate, group=Covariate))+
       geom_point()+geom_line()+facet_wrap(~Stock_ERA, scales="free")

View(stations_meta)

fcs_covariates_long_meta<- merge(fcs_covariates_long, cov_meta, by.x=c("Covariate"), by.y=c("cov_name")) %>% as_tibble
fcs_covariates_long_meta<- merge(fcs_covariates_long_meta, stations_meta %>% dplyr::select(Region, Stock_ERA)) %>% as_tibble
fcs_covariates_long_meta<- fcs_covariates_long_meta %>%  fct_relevel(var_cat, "Zooplankton", "Temperature", "Salinity", "PDO", "ONI", "SOI", "NPI", "EPNP", "NPGO", "ALPI")
fcs_covariates_long_meta

#BC
ggplot(fcs_covariates_long_meta %>% filter(Region == "BC"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("BC")

#AK
ggplot(fcs_covariates_long_meta %>% filter(Region == "AK"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("AK")

#WA
ggplot(fcs_covariates_long_meta %>% filter(Region == "WA"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("WA")

#OR
ggplot(fcs_covariates_long_meta %>% filter(Region == "OR"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free")+ ggtitle("OR")

#CR
ggplot(fcs_covariates_long_meta %>% filter(Region == "CR"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free")+ ggtitle("CR")





