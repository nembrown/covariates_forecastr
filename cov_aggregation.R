



#pdo #168
pdo_simple

#temperature from MEDS buoys #464
dfo_meds_buoys_matched_combined %>% as_tibble()

#temp and salinity from lightstations #752
Data_Lightstations_matched

#combining PDO and temperature

fcs_covariates<- merge(Data_Lightstations_matched, dfo_meds_buoys_matched_combined, by=c("Stock_ERA", "year")) %>% as_tibble()
fcs_covariates<-merge(fcs_covariates, pdo_simple, by=c("year")) %>% as_tibble()
fcs_covariates





fcs_covariates_long<- fcs_covariates %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
                                         mutate(var_cat = case_when(
                                           str_detect(Covariate, "SST") ~ "Temperature", 
                                           str_detect(Covariate, "PPT") ~ "Salinity",
                                           str_detect(Covariate, "PDO") ~ "PDO"
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
fcs_covariates_long

ggplot(fcs_covariates_long %>% filter(Stock_ERA == "RBT", var_cat== "Temperature", var_location=="terminal"), 
       aes(x=year, y=value, col=Covariate, group=Covariate))+
        geom_point()+geom_line()+ggtitle("RBT")
