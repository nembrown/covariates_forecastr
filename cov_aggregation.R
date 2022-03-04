

#pdo #52 lines, all years sing 1896
pdo_simple

#temperature from MEDS buoys #463, since 1989
dfo_meds_buoys_matched_combined

#temp and salinity from lightstations #752, since 1900
Data_Lightstations_matched

#zooplankton from ios, since 1980
ios_zoop_anomalies<-ios_zoop_anomalies %>% rename(year = calc_year)

#join all together 

fcs_covariates<- merge(Data_Lightstations_matched, dfo_meds_buoys_matched_combined, by=c("Stock_ERA", "year"), all=TRUE) %>% as_tibble()
fcs_covariates<- merge(fcs_covariates,ios_zoop_anomalies, by=c("Stock_ERA", "year"), all=TRUE)
fcs_covariates<- merge(fcs_covariates, pdo_simple, by=c("year")) %>% as_tibble()

fcs_covariates<- fcs_covariates %>% relocate(where(is.numeric), .after = where(is.character))
fcs_covariates
View(fcs_covariates)


write.csv(fcs_covariates, "fcs_covariates.csv")



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
