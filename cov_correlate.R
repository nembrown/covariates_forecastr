
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)
library(rstatix)
library(lares)
library(purrr)


## create


# Read in cov_data for covariate and Av escapement  ------------------------------------------------------------

correlate_covs(cov_data="fcs_covariates.csv",
               escapement_data = "Inputs/UGS_Esc_upto2021.csv",
               modelstock = "UGS",
               stock= "QUI",
               year_match="Brood_Year_Lag1")


correlate_covs<-function(cov_data,
                         escapement_data,
                         modelstock = NA_character_, 
                         stock= NA_character_, 
                         year_match= NA_character_,
                         age_specific=FALSE){

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


escapement_data<-escapement_data %>%  select(Run_Year, Average_Escapement) %>% 
                                      mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
                                      mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
                                      mutate(Brood_Year = Run_Year - 4) %>% 
                                      mutate(Brood_Year_Lag1 = Run_Year - 3) %>% 
                                      mutate(Brood_Year_Lag2 = Run_Year - 2) 

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

# Matching escapement to covariates ---------------------------------------

#1. Long files - for use in visualization plotting 
#matching to Run Year + time lags
if(year_match %in%  c("Run_Year", "Run_Year_Lead1", "Run_Year_Lead2")) {
  escapement_data_covariates<-merge(escapement_data, cov_data_long_stock, by.x=year_match, by.y=c("year")) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock, by.x=year_match, by.y=c("year")) %>% as_tibble
  
  } else if (year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
  escapement_data_covariates<-merge(escapement_data,cov_data_stock_roll_long, by.x=year_match, by.y=c("year")) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock_roll, by.x=year_match, by.y=c("year")) %>% as_tibble
  }


# Correlation with Average Escapement -------------------------------------

corr_stock<-escapement_data_covariates_wide %>% select(Average_Escapement, starts_with("cov")) 

#Full correlations
corr_var(corr_stock,Average_Escapement, plot=TRUE, top=40) %>% ggsave(file=paste0("Plots/", modelstock, "/corr_", year_match, "_", modelstock, ".tiff"))

#Significant p-values only - need to save the plots
corr_var(corr_stock,Average_Escapement, plot=TRUE, top=40, max_pvalue=0.05,SAVE=TRUE)%>% ggsave(file=paste0("Plots/", modelstock, "/corr_psig_", year_match,"_", modelstock, ".tiff"))

# Plotting ----------------------------------------------------------------
#setting up
escapement_data_covariates$Covariate<-as.factor(escapement_data_covariates$Covariate)
matches_stock = unique(escapement_data_covariates$Covariate)
Covariate_plots_stock = list()

#For loops

#Run Year
for(Covariate_ in matches_stock) {
  Covariate_plots_stock[[Covariate_]] = ggplot(escapement_data_covariates %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement)) + 
    geom_point() + geom_smooth(method="lm")+
    ggtitle(escapement_data_covariates$Covariate[escapement_data_covariates$Covariate== Covariate_])
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
  pluck(Covariate_plots_stock, "cov_ALPI_yearly_mean") +
  pluck(Covariate_plots_stock, "cov_model_EVs") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)

oceanic_yearly_Covariate_plots_stock 
ggsave(oceanic_yearly_Covariate_plots_stock , file=paste0("Plots/", modelstock, "/oceanic_yearly_", year_match,"_", modelstock, ".tiff"))

#oceanic summer plot
oceanic_summer_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_stock, "cov_SOI_summer_mean") + 
  # pluck(Covariate_plots_stock, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_stock, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/oceanic_summer_", year_match,"_", modelstock, ".tiff"))

##Zooplankton
zoop_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_stock, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_stock, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/zoop_",  year_match,"_", modelstock, ".tiff"))

#Temperature
temp_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/temperature_",  year_match,"_", modelstock, ".tiff"))

#Salinity
salinity_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/salinity_",  year_match,"_", modelstock, ".tiff"))

#Herring
herring_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_herring_spawn_index")
ggsave(herring_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/herring_", year_match,"_", modelstock, ".tiff"))

#Hydrographic variables
hydro_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_water_flow_yearly_mean")+
  pluck(Covariate_plots_stock, "cov_water_level_yearly_mean")+ 
  plot_layout(guides = 'collect', ncol=2)
ggsave(hydro_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/hydro_",  year_match,"_", modelstock, ".tiff"))
}
