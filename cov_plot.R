
library(ggplot2)
library(tidyverse)


# Plotting by stock -------------------------------------------------------
fcs_covariates

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

