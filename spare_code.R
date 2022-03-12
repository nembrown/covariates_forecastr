####
ios_zoop_winter_COW<-ios_zoop_matched %>% filter(season == "winter", Stock_ERA=="COW", year==2014)
View(ios_zoop_winter_COW)

View(ios_zoop_winter)
unique(ios_zoop_matched$Net)
unique(ios_zoop_matched$Mesh...U.00B5.m.)


ios_zoop__matched_zero<-ios_zoop_matched %>% filter(total_zoop_biomass != 0.07)
min(ios_zoop__matched_zero$total_zoop_biomass)


hist(ios_zoop_matched$Mesh...U.00B5.m.)
count(ios_zoop_matched$Net)

barplot(prop.table(table(ios_zoop_matched$Net)))





###Trying to make the plots in a list
var_temp<- GST_sample_age_covariates_run_year %>% filter(var_cat== "Temperature") %>% select(Covariate) %>% distinct() 
var_temp_list<-as.list(var_temp$Covariate)
var_cats<-unique(GST_sample_age_covariates_run_year$var_cat)


Covariate_plots_temp<-map(Covariate_plots, var_temp_list)

pdo_yearly_plot<-pluck(Covariate_plots, "cov_PDO_yearly_mean")
pluck(Covariate_plots, "cov_PDO_summer_mean")
pluck(Covariate_plots, "cov_ONI_yearly_mean")
pluck(Covariate_plots, "cov_ONI_yearly_anomaly")

oceanic_list<-c("cov_PDO_yearly_mean", "cov_PDO_summer_mean", 
                "cov_ONI_yearly_mean", "cov_ONI_yearly_anomaly", 
                "cov_ONI_summer_mean", "cov_ONI_summer_anomaly")

plots_oceanic<-pluck(Covariate_plots, !!!oceanic_list)


###
View(GST_sample_age_covariates_wide)
ggplot(GST_sample_age_covariates_wide, aes(y=cov_zoop_winter_anomaly, x=Run_Year))+geom_point()+geom_line()

fcs_covariates_long_meta

fcs_covariates_long_meta_temp_comparison <- fcs_covariates_long_meta %>% filter(Covariate %in% c("cov_SST_MEDS_terminal_yearly_mean", "cov_SST_lighthouse_yearly_mean")) %>% drop_na(value)

ggplot(fcs_covariates_long_meta_temp_comparison,aes(y=value, x=year, col=Covariate))+geom_point()+geom_line() + facet_wrap(~Stock_ERA, drop=TRUE, scales="free")

ggplot(fcs_covariates_long_meta_temp_comparison %>% filter(Stock_ERA=="COW"),aes(y=value, x=year, col=Covariate))+geom_point()+geom_line() 
