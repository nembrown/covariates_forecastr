
# Load libraries ----------------------------------------------------------

library(tidyverse)
# install.packages("devtools")
library(devtools)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)
library(rstatix)
library(lares)
library(purrr)


# Read in data for covariate and Av escapement  ------------------------------------------------------------


fcs_covariates_combined<-read.csv("fcs_covariates.csv") %>% as_tibble()

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

# Restrict to only SRH stock
fcs_covariates_long_SRH<-fcs_covariates_long %>% filter(Stock_ERA == "SRH")
fcs_covariates_combined_SRH<-fcs_covariates_combined %>% filter(Stock_ERA == "SRH")

#Load in Escapement data
ALSEA_sample_age1<-read.csv("Inputs/ALSEA_cov.csv") %>% as_tibble()
ALSEA_sample_age<-ALSEA_sample_age1 %>%  dplyr::select(Run_Year, Brood_Year, Age_Class, Average_Escapement) %>% 
                                        mutate(Run_Year_Lead_1 = Run_Year - 1) %>% 
                                        mutate(Run_Year_Lead_2 = Run_Year - 2) %>% 
                                        mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
                                        mutate(Brood_Year_Lag_2 = Brood_Year + 2)

ALSEA_sample_age_cov<-ALSEA_sample_age1 %>%  dplyr::select(Run_Year, cov_ONI_JAN_to_JUN, cov_Var_BY_Age_2_surv_transformed, cov_Copepod_richness_SUM) %>% 
                                             rename(year = Run_Year) %>% 
                                             pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
                                             add_column(Stock_ERA = "SRH") %>% 
                                             add_column(var_cat = "Ethan variables") %>% 
                                             add_column(var_timing = "NA") %>% 
                                             add_column(var_location = "NA") 
  
fcs_covariates_long_SRH<-bind_rows(fcs_covariates_long_SRH, ALSEA_sample_age_cov)

# Matching escapement to covariates ---------------------------------------

#1. Wide files - for use in correlation plots
#matching to run year + time lags
ALSEA_sample_age_covariates_wide_run_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_wide_run_lead1_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Run_Year_Lead_1"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_wide_run_lead2_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Run_Year_Lead_2"), by.y=c("year")) %>% as_tibble

#matching to Brood Year + time lags
ALSEA_sample_age_covariates_wide_brood_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_wide_brood_lag1_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_wide_brood_lag2_year<-merge(ALSEA_sample_age, fcs_covariates_combined_SRH, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#2. Long files - for use in visualization plotting 
#matching to run year + time lags
ALSEA_sample_age_covariates_run_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_run_lead1_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Run_Year_Lead_1"), by.y=c("year"), all = TRUE) %>% as_tibble
ALSEA_sample_age_covariates_run_lead2_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Run_Year_Lead_2"), by.y=c("year"), all=TRUE) %>% as_tibble

#matching to Brood Year + time lags
ALSEA_sample_age_covariates_brood_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_brood_lag1_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
ALSEA_sample_age_covariates_brood_lag2_year<-merge(ALSEA_sample_age, fcs_covariates_long_SRH, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble

# Correlation with Average Escapement -------------------------------------

corr_ALSEA_run_year<-ALSEA_sample_age_covariates_wide_run_year %>% select(Average_Escapement, starts_with("cov")) 
corr_ALSEA_run_lead1_year<-ALSEA_sample_age_covariates_wide_run_lead1_year %>% select(Average_Escapement, starts_with("cov")) 
corr_ALSEA_run_lead2_year<-ALSEA_sample_age_covariates_wide_run_lead2_year %>% select(Average_Escapement, starts_with("cov")) 
corr_ALSEA_brood_year<-ALSEA_sample_age_covariates_wide_brood_year %>% select(Average_Escapement, starts_with("cov"))
corr_ALSEA_brood_lag1_year<-ALSEA_sample_age_covariates_wide_brood_lag1_year %>% select(Average_Escapement, starts_with("cov"))  
corr_ALSEA_brood_lag2_year<-ALSEA_sample_age_covariates_wide_brood_lag2_year %>% select(Average_Escapement, starts_with("cov")) 

#Full correlations
corr_var(corr_ALSEA_run_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Run year/Sync/corr_ALSEA_run_year.tiff")
corr_var(corr_ALSEA_run_lead1_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Run year/Lead1/corr_ALSEA_run_year_lead1.tiff")
corr_var(corr_ALSEA_run_lead2_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Run year/Lead2/corr_ALSEA_run_year_lead2.tiff")
corr_var(corr_ALSEA_brood_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Brood Year/Sync/corr_ALSEA_brood_year.tiff")
corr_var(corr_ALSEA_brood_lag1_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Brood Year/Lag1/corr_ALSEA_brood_year_lag1.tiff")
corr_var(corr_ALSEA_brood_lag2_year,Average_Escapement, plot=TRUE, top=32)
ggsave(file="Plots/ALSEA/Brood Year/Lag2/corr_ALSEA_brood_year_lag2.tiff")

#Significant p-values only 
corr_var(corr_ALSEA_run_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Run year/Sync/corr_ALSEA_run_year_sigp.tiff")
corr_var(corr_ALSEA_run_lead1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Run year/Lead1/corr_ALSEA_run_year_lead1_sigp.tiff")
corr_var(corr_ALSEA_run_lead2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Run year/Lead2/corr_ALSEA_run_year_lead2_sigp.tiff")
corr_var(corr_ALSEA_brood_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Brood Year/Sync/corr_ALSEA_brood_year_sigp.tiff")
corr_var(corr_ALSEA_brood_lag1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Brood Year/Lag1/corr_ALSEA_brood_year_lag1_sigp.tiff")
corr_var(corr_ALSEA_brood_lag2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/ALSEA/Brood Year/Lag2/corr_ALSEA_brood_year_lag2_sigp.tiff")



# Plotting ----------------------------------------------------------------
##Adding in Ethan's covariate

#setting up
ALSEA_sample_age_covariates_run_year$Covariate<-as.factor(ALSEA_sample_age_covariates_run_year$Covariate)
ALSEA_sample_age_covariates_run_lead1_year$Covariate<-as.factor(ALSEA_sample_age_covariates_run_lead1_year$Covariate)
ALSEA_sample_age_covariates_run_lead2_year$Covariate<-as.factor(ALSEA_sample_age_covariates_run_lead2_year$Covariate)
ALSEA_sample_age_covariates_brood_year$Covariate<-as.factor(ALSEA_sample_age_covariates_brood_year$Covariate)
ALSEA_sample_age_covariates_brood_lag1_year$Covariate<-as.factor(ALSEA_sample_age_covariates_brood_lag1_year$Covariate)
ALSEA_sample_age_covariates_brood_lag2_year$Covariate<-as.factor(ALSEA_sample_age_covariates_brood_lag2_year$Covariate)

macthes_ALSEA_run_year = unique(ALSEA_sample_age_covariates_run_year$Covariate)
macthes_ALSEA_run_lead1_year = unique(ALSEA_sample_age_covariates_run_lead1_year$Covariate)
macthes_ALSEA_run_lead2_year = unique(ALSEA_sample_age_covariates_run_lead2_year$Covariate)
macthes_ALSEA_brood_year =unique(ALSEA_sample_age_covariates_brood_year$Covariate)
macthes_ALSEA_brood_lag1_year = unique(ALSEA_sample_age_covariates_brood_lag1_year$Covariate)
macthes_ALSEA_brood_lag2_year = unique(ALSEA_sample_age_covariates_brood_lag2_year$Covariate)

Covariate_plots_ALSEA_run_year = list()
Covariate_plots_ALSEA_run_lead1_year = list()
Covariate_plots_ALSEA_run_lead2_year = list()
Covariate_plots_ALSEA_brood_year = list()
Covariate_plots_ALSEA_brood_lag1_year = list()
Covariate_plots_ALSEA_brood_lag2_year = list()

#For loops

#Run year
for(Covariate_ in macthes_ALSEA_run_year) {
  Covariate_plots_ALSEA_run_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_run_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_run_year$Covariate[ALSEA_sample_age_covariates_run_year$Covariate== Covariate_])
}

#Run year lead 1
for(Covariate_ in macthes_ALSEA_run_lead1_year) {
  Covariate_plots_ALSEA_run_lead1_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_run_lead1_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_run_lead1_year$Covariate[ALSEA_sample_age_covariates_run_lead1_year$Covariate== Covariate_])
}

#Run year lead 2
for(Covariate_ in macthes_ALSEA_run_lead2_year) {
  Covariate_plots_ALSEA_run_lead2_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_run_lead2_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_run_lead2_year$Covariate[ALSEA_sample_age_covariates_run_lead2_year$Covariate== Covariate_])
}

#Brood Year
for(Covariate_ in macthes_ALSEA_brood_year) {
  Covariate_plots_ALSEA_brood_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_brood_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_brood_year$Covariate[ALSEA_sample_age_covariates_brood_year$Covariate== Covariate_])
}

#Brood Year lag 1
for(Covariate_ in macthes_ALSEA_brood_lag1_year) {
  Covariate_plots_ALSEA_brood_lag1_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_brood_lag1_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_brood_lag1_year$Covariate[ALSEA_sample_age_covariates_brood_lag1_year$Covariate== Covariate_])
}

#Brood Year lag 2
for(Covariate_ in macthes_ALSEA_brood_lag2_year) {
  Covariate_plots_ALSEA_brood_lag2_year[[Covariate_]] = ggplot(ALSEA_sample_age_covariates_brood_lag2_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(ALSEA_sample_age_covariates_brood_lag2_year$Covariate[ALSEA_sample_age_covariates_brood_lag2_year$Covariate== Covariate_])
}


########### Covariate_plots_ALSEA_run_year
#Ethan's covariates
Ethans_Covariate_plots_ALSEA_run_year <- pluck(Covariate_plots_ALSEA_run_year, "cov_ONI_JAN_to_JUN") + 
                                                 pluck(Covariate_plots_ALSEA_run_year, "cov_Var_BY_Age_2_surv_transformed")+
                                                 pluck(Covariate_plots_ALSEA_run_year, "cov_Copepod_richness_SUM") +
                                                 guide_area()+                                               
                                                 plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_run_year 
ggsave(Ethans_Covariate_plots_ALSEA_run_year , file="Plots/ALSEA/Run Year/Sync/Ethans_Covariate_plots_ALSEA_run_year.tiff")

#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_run_year <- pluck(Covariate_plots_ALSEA_run_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_run_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_run_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_run_year , file="Plots/ALSEA/Run Year/Sync/oceanic_yearly_Covariate_plots_ALSEA_run_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_run_year <-  pluck(Covariate_plots_ALSEA_run_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_run_year, file="Plots/ALSEA/Run Year/Sync/oceanic_summer_Covariate_plots_ALSEA_run_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_run_year <-  pluck(Covariate_plots_ALSEA_run_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_run_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_run_year, file="Plots/ALSEA/Run Year/Sync/zoop_Covariate_plots_ALSEA_run_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_run_year <-  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_run_year, file="Plots/ALSEA/Run Year/Sync/temp_Covariate_plots_ALSEA_run_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_run_year <-  pluck(Covariate_plots_ALSEA_run_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_run_year, file="Plots/ALSEA/Run Year/Sync/salinity_Covariate_plots_ALSEA_run_year.tiff")


# Covariate_plots_ALSEA_run_lead1_year
#Ethan's covariates
Ethans_Covariate_plots_ALSEA_run_lead1_year <- pluck(Covariate_plots_ALSEA_run_lead1_year , "cov_ONI_JAN_to_JUN") + 
                                         pluck(Covariate_plots_ALSEA_run_lead1_year , "cov_Var_BY_Age_2_surv_transformed")+
                                         pluck(Covariate_plots_ALSEA_run_lead1_year , "cov_Copepod_richness_SUM") +
                                         guide_area()+                                               
                                         plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_run_lead1_year 
ggsave(Ethans_Covariate_plots_ALSEA_run_lead1_year , file="Plots/ALSEA/Run Year/Lead1/Ethans_Covariate_plots_ALSEA_run_lead1_year.tiff")



#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_run_lead1_year <- pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_run_lead1_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_run_lead1_year , file="Plots/ALSEA/Run Year/Lead1/oceanic_yearly_Covariate_plots_ALSEA_run_lead1_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_run_lead1_year <-  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_run_lead1_year, file="Plots/ALSEA/Run Year/Lead1/oceanic_summer_Covariate_plots_ALSEA_run_lead1_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_run_lead1_year <-  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_run_lead1_year, file="Plots/ALSEA/Run Year/Lead1/zoop_Covariate_plots_ALSEA_run_lead1_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_run_lead1_year <-  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_run_lead1_year, file="Plots/ALSEA/Run Year/Lead1/temp_Covariate_plots_ALSEA_run_lead1_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_run_lead1_year <-  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead1_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_run_lead1_year, file="Plots/ALSEA/Run Year/Lead1/salinity_Covariate_plots_ALSEA_run_lead1_year.tiff")

####

# Covariate_plots_ALSEA_run_lead2_year
#Ethan's covariates
Ethans_Covariate_plots_ALSEA_run_lead2_year <- pluck(Covariate_plots_ALSEA_run_lead2_year , "cov_ONI_JAN_to_JUN") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year , "cov_Var_BY_Age_2_surv_transformed")+
  pluck(Covariate_plots_ALSEA_run_lead2_year , "cov_Copepod_richness_SUM") +
  guide_area()+                                               
  plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_run_lead2_year 
ggsave(Ethans_Covariate_plots_ALSEA_run_lead2_year , file="Plots/ALSEA/Run Year/Lead2/Ethans_Covariate_plots_ALSEA_run_lead2_year.tiff")



#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_run_lead2_year <- pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_run_lead2_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_run_lead2_year , file="Plots/ALSEA/Run Year/Lead2/oceanic_yearly_Covariate_plots_ALSEA_run_lead2_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_run_lead2_year <-  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_run_lead2_year, file="Plots/ALSEA/Run Year/Lead2/oceanic_summer_Covariate_plots_ALSEA_run_lead2_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_run_lead2_year <-  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_run_lead2_year, file="Plots/ALSEA/Run Year/Lead2/zoop_Covariate_plots_ALSEA_run_lead2_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_run_lead2_year <-  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_run_lead2_year, file="Plots/ALSEA/Run Year/Lead2/temp_Covariate_plots_ALSEA_run_lead2_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_run_lead2_year <-  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_run_lead2_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_run_lead2_year, file="Plots/ALSEA/Run Year/Lead2/salinity_Covariate_plots_ALSEA_run_lead2_year.tiff")



############# Brood
########### Covariate_plots_ALSEA_brood_year
Ethans_Covariate_plots_ALSEA_brood_year <- pluck(Covariate_plots_ALSEA_brood_year, "cov_ONI_JAN_to_JUN") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_Var_BY_Age_2_surv_transformed")+
  pluck(Covariate_plots_ALSEA_brood_year, "cov_Copepod_richness_SUM") +
  guide_area()+                                               
  plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_brood_year 
ggsave(Ethans_Covariate_plots_ALSEA_brood_year , file="Plots/ALSEA/Brood Year/Sync/Ethans_Covariate_plots_ALSEA_brood_year.tiff")


#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_brood_year <- pluck(Covariate_plots_ALSEA_brood_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_brood_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_brood_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_brood_year , file="Plots/ALSEA/Brood Year/Sync/oceanic_yearly_Covariate_plots_ALSEA_brood_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_brood_year <-  pluck(Covariate_plots_ALSEA_brood_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_brood_year, file="Plots/ALSEA/Brood Year/Sync/oceanic_summer_Covariate_plots_ALSEA_brood_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_brood_year <-  pluck(Covariate_plots_ALSEA_brood_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_brood_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_brood_year, file="Plots/ALSEA/Brood Year/Sync/zoop_Covariate_plots_ALSEA_brood_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_brood_year <-  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_brood_year, file="Plots/ALSEA/Brood Year/Sync/temp_Covariate_plots_ALSEA_brood_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_brood_year <-  pluck(Covariate_plots_ALSEA_brood_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_brood_year, file="Plots/ALSEA/Brood Year/Sync/salinity_Covariate_plots_ALSEA_brood_year.tiff")


# Covariate_plots_ALSEA_brood_lag1_year
Ethans_Covariate_plots_ALSEA_brood_lag1_year <- pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ONI_JAN_to_JUN") + 
                                                pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_Var_BY_Age_2_surv_transformed")+
                                                pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_Copepod_richness_SUM") +
                                                guide_area()+                                               
                                                plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_brood_lag1_year 
ggsave(Ethans_Covariate_plots_ALSEA_brood_lag1_year , file="Plots/ALSEA/Brood Year/Lag1/Ethans_Covariate_plots_ALSEA_brood_lag1_year.tiff")


#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_brood_lag1_year <- pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_brood_lag1_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_brood_lag1_year , file="Plots/ALSEA/Brood Year/Lag1/oceanic_yearly_Covariate_plots_ALSEA_brood_lag1_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_brood_lag1_year <-  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_brood_lag1_year, file="Plots/ALSEA/Brood Year/Lag1/oceanic_summer_Covariate_plots_ALSEA_brood_lag1_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_brood_lag1_year <-  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_brood_lag1_year, file="Plots/ALSEA/Brood Year/Lag1/zoop_Covariate_plots_ALSEA_brood_lag1_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_brood_lag1_year <-  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_brood_lag1_year, file="Plots/ALSEA/Brood Year/Lag1/temp_Covariate_plots_ALSEA_brood_lag1_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_brood_lag1_year <-  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag1_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_brood_lag1_year, file="Plots/ALSEA/Brood Year/Lag1/salinity_Covariate_plots_ALSEA_brood_lag1_year.tiff")

####

# Covariate_plots_ALSEA_brood_lag2_year
# Ethans
Ethans_Covariate_plots_ALSEA_brood_lag2_year <- pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ONI_JAN_to_JUN") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_Var_BY_Age_2_surv_transformed")+
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_Copepod_richness_SUM") +
  guide_area()+                                               
  plot_layout(guides = 'collect', ncol=2)
Ethans_Covariate_plots_ALSEA_brood_lag2_year 
ggsave(Ethans_Covariate_plots_ALSEA_brood_lag2_year , file="Plots/ALSEA/Brood Year/Lag2/Ethans_Covariate_plots_ALSEA_brood_lag2_year.tiff")



#Oceanic indices by year
oceanic_yearly_Covariate_plots_ALSEA_brood_lag2_year <- pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_ALSEA_brood_lag2_year 
ggsave(oceanic_yearly_Covariate_plots_ALSEA_brood_lag2_year , file="Plots/ALSEA/Brood Year/Lag2/oceanic_yearly_Covariate_plots_ALSEA_brood_lag2_year.tiff")

#oceanic summer plot
oceanic_summer_Covariate_plots_ALSEA_brood_lag2_year <-  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)
ggsave(oceanic_summer_Covariate_plots_ALSEA_brood_lag2_year, file="Plots/ALSEA/Brood Year/Lag2/oceanic_summer_Covariate_plots_ALSEA_brood_lag2_year.tiff")

##Zooplankton
zoop_Covariate_plots_ALSEA_brood_lag2_year <-  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(zoop_Covariate_plots_ALSEA_brood_lag2_year, file="Plots/ALSEA/Brood Year/Lag2/zoop_Covariate_plots_ALSEA_brood_lag2_year.tiff")

#Temperature
temp_Covariate_plots_ALSEA_brood_lag2_year <-  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)
ggsave(temp_Covariate_plots_ALSEA_brood_lag2_year, file="Plots/ALSEA/Brood Year/Lag2/temp_Covariate_plots_ALSEA_brood_lag2_year.tiff")

#Salinity
salinity_Covariate_plots_ALSEA_brood_lag2_year <-  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots_ALSEA_brood_lag2_year, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)
ggsave(salinity_Covariate_plots_ALSEA_brood_lag2_year, file="Plots/ALSEA/Brood Year/Lag2/salinity_Covariate_plots_ALSEA_brood_lag2_year.tiff")
