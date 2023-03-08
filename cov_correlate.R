
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)
library(rstatix)
library(lares)
library(purrr)


# Read in cov_data for covariate and Av escapement  ------------------------------------------------------------

correlate_covs(cov_data="fcs_covariates_interpolated.csv",
               escapement_data = "Inputs/PPS_Esc_no_age_upto2022.csv",
               modelstock = "PPS",
               stock= "PPS",
               year_match="Brood_Year_Lag2", 
               escapement_type="Average Escapement",
               age_specific=FALSE, 
               age_combine=FALSE,
               age_class=NA)


correlate_covs<-function(cov_data,
                         escapement_data,
                         modelstock = NA_character_, 
                         stock= NA_character_, 
                         year_match= NA_character_,
                         escapement_type=NA_character_,
                         age_specific=FALSE,
                         age_combine=FALSE,
                         age_class = NA_integer_){

  cov_data<-read.csv(cov_data) %>% as_tibble()
  escapement_data<-read.csv(escapement_data) %>% as_tibble()
  
  rel_file_dir <- normalizePath(paste0("Plots/", modelstock), mustWork = FALSE)
  
  if(dir.exists(rel_file_dir) == FALSE) {
    cat(paste0("Plots directory does not exist, creating:\n", rel_file_dir))
    dir.create(rel_file_dir)
  }
  
cov_data_long<- cov_data %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
  mutate(var_cat = case_when(
    str_detect(Covariate, "SST") ~ "Temperature", 
    str_detect(Covariate, "PPT") ~ "Salinity",
    str_detect(Covariate, "zoop") ~ "Zooplankton", 
    str_detect(Covariate, "herring") ~ "Herring",
    str_detect(Covariate, "water") ~ "Hydrographic", 
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

# Restrict to only stock
cov_data_long_stock<-cov_data_long %>% filter(Stock_ERA == stock)
cov_data_stock<-cov_data %>% filter(Stock_ERA == stock)

if(escapement_type=="Terminal Run"){
  escapement_data<-escapement_data %>% rename(Escapement_type=Terminal_Run)
}else if (escapement_type=="Average Escapement"){
  escapement_data<-escapement_data %>% rename(Escapement_type=Average_Escapement)
}

if(age_specific==FALSE){

escapement_data<-escapement_data %>%  select(Run_Year, Escapement_type) %>% 
                                      mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
                                      mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
                                      mutate(Brood_Year = Run_Year - 4) %>% 
                                      mutate(Brood_Year_Lag1 = Run_Year - 3) %>% 
                                      mutate(Brood_Year_Lag2 = Run_Year - 2) %>% 
                                      select(Escapement_type, all_of(c(year_match)))

cov_data_stock_roll<- zoo::rollmean(cov_data_stock %>% select(-Stock_ERA), k=3) %>% as_tibble()
#Brood year
cov_data_stock_roll_long<- cov_data_stock_roll %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
  mutate(var_cat = case_when(
    str_detect(Covariate, "SST") ~ "Temperature", 
    str_detect(Covariate, "PPT") ~ "Salinity",
    str_detect(Covariate, "zoop") ~ "Zooplankton", 
    str_detect(Covariate, "herring") ~ "Herring",
    str_detect(Covariate, "water") ~ "Hydrographic", 
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
} else if (age_combine==TRUE  & year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
           escapement_data<-escapement_data %>%  group_by(Brood_Year) %>%
           summarise(Escapement_type = sum(Escapement_type, na.rm=TRUE)) %>% 
           mutate(Brood_Year_Lag1 = Brood_Year + 1) %>% 
           mutate(Brood_Year_Lag2 = Brood_Year + 2) %>% 
           select(Escapement_type, all_of(c(year_match)))
} else if (age_combine==TRUE){
           escapement_data<-escapement_data %>%  group_by(Run_Year) %>%
           summarise(Escapement_type = sum(Escapement_type, na.rm=TRUE)) %>% 
           mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
           mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
           select(Escapement_type, all_of(c(year_match)))
} else {
  escapement_data<-escapement_data %>%  select(Brood_Year, Run_Year, Escapement_type, Age_Class) %>% 
  mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
  mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
  mutate(Brood_Year_Lag1 = Brood_Year + 1) %>% 
  mutate(Brood_Year_Lag2 = Brood_Year + 2) %>% 
  select(Escapement_type, Age_Class, all_of(c(year_match)))
}

# Matching escapement to covariates ---------------------------------------

#1. Long files - for use in visualization plotting 
#matching to Run Year + time lags
if(age_specific==FALSE & year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
  escapement_data_covariates<-merge(escapement_data ,cov_data_stock_roll_long, by.x=year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock_roll, by.x=year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble
} else  {
  escapement_data_covariates<-merge(escapement_data, cov_data_long_stock, by.x=year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock, by.x=year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble
}


#Filter for age class
if(age_specific==TRUE & age_combine==FALSE){
  escapement_data_covariates<- escapement_data_covariates %>% filter(Age_Class == age_class)
  escapement_data_covariates_wide<- escapement_data_covariates_wide %>% filter(Age_Class == age_class)
  }

# Correlation with Escapement type -------------------------------------

corr_stock<-escapement_data_covariates_wide %>% select(Escapement_type, starts_with("cov")) 

#Full correlations
corr_var(corr_stock,Escapement_type, plot=TRUE, top=40) %>% ggsave(file=paste0("Plots/", modelstock, "/corr_", year_match, "_", modelstock, "_Age_",age_class, ".tiff"))

#Significant p-values only - need to save the plots
corr_var(corr_stock,Escapement_type, plot=TRUE, top=40, max_pvalue=0.05,SAVE=TRUE)%>% ggsave(file=paste0("Plots/", modelstock, "/corr_psig_", year_match,"_", modelstock, "_Age_",age_class,".tiff"))


# Plotting ----------------------------------------------------------------
#setting up
escapement_data_covariates$Covariate<-as.factor(escapement_data_covariates$Covariate)
matches_stock = unique(escapement_data_covariates$Covariate)
Covariate_plots_stock = list()

#For loops

for(Covariate_ in matches_stock) {
  Covariate_plots_stock[[Covariate_]] = ggplot(escapement_data_covariates %>% filter(Covariate == Covariate_), aes( x=value, y=Escapement_type)) + 
    geom_point() + geom_smooth(method="lm")+
    ylab(paste0(escapement_type))+
    xlab(paste0(escapement_data_covariates$Covariate[escapement_data_covariates$Covariate== Covariate_]))+
    ggtitle(paste0("Age = ", age_class))
}

########### Covariate_plots_stock
#Oceanic indices by year
oceanic_yearly_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_stock, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_stock, "cov_SOI_yearly_mean") +
  # pluck(Covariate_plots_stock, "cov_NPI_yearly_mean") +
  # pluck(Covariate_plots_stock, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_stock, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_stock, "cov_EPNP_yearly_mean") +
 # pluck(Covariate_plots_stock, "cov_ALPI_yearly_mean") +
 # pluck(Covariate_plots_stock, "cov_model_EVs") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_stock 
ggsave(oceanic_yearly_Covariate_plots_stock , file=paste0("Plots/", modelstock, "/oceanic_yearly_", year_match,"_", modelstock, "_Age_",age_class, ".tiff"))

#oceanic summer plot
oceanic_summer_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_stock, "cov_SOI_summer_mean") + 
  # pluck(Covariate_plots_stock, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_stock, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=3)
ggsave(oceanic_summer_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/oceanic_summer_", year_match,"_", modelstock,  "_Age_",age_class,".tiff"))

##Zooplankton
zoop_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_stock, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/zoop_",  year_match,"_", modelstock, "_Age_",age_class, ".tiff"))

#Temperature
temp_Covariate_plots_stock <- 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_yearly_mean") + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_summer_mean") + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_yearly_mean") + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(temp_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/temperature_",  year_match,"_", modelstock, "_Age_",age_class, ".tiff"))

#Salinity
salinity_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/salinity_",  year_match,"_", modelstock, "_Age_",age_class, ".tiff"))

#Herring
herring_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_herring_spawn_index_mean")
ggsave(herring_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/herring_", year_match,"_", modelstock,  "_Age_",age_class,".tiff"))

#Hydrographic variables
hydro_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_water_flow_yearly_mean")+
  pluck(Covariate_plots_stock, "cov_water_flow_yearly_max")+ 
  plot_layout(guides = 'collect', ncol=2)
ggsave(hydro_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/hydro_",  year_match,"_", modelstock, "_Age_",age_class, ".tiff"))
}
