
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

unique(fcs_covariates_combined$Stock_ERA)

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

# Restrict to only BQR stock
fcs_covariates_long_BQR<-fcs_covariates_long %>% filter(Stock_ERA == "BQR")
fcs_covariates_combined_BQR<-fcs_covariates_combined %>% filter(Stock_ERA == "BQR")

#Load in Escapement data
MGS_sample_age<-read.csv("Inputs/MGS_ESc_upto2021.csv") %>% as_tibble()
MGS_sample_age<-MGS_sample_age %>%  dplyr::select(Run_Year, Brood_Year, Age_Class, Average_Escapement) %>% 
  mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
  mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
  mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
  mutate(Brood_Year_Lag_2 = Brood_Year + 2) 

# Matching escapement to covariates ---------------------------------------

#1. Wide files - for use in correlation plots
#matching to run year + time lags
MGS_sample_age_covariates_wide_run_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_wide_run_lag1_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Run_Year_Lag_1"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_wide_run_lag2_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Run_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#matching to brood year + time lags
MGS_sample_age_covariates_wide_brood_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_wide_brood_lag1_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_wide_brood_lag2_year<-merge(MGS_sample_age, fcs_covariates_combined_BQR, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#2. Long files - for use in visualization plotting 
#matching to run year + time lags
MGS_sample_age_covariates_run_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_run_lag1_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Run_Year_Lag_1"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_run_lag2_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Run_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#matching to brood year + time lags
MGS_sample_age_covariates_brood_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_brood_lag1_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
MGS_sample_age_covariates_brood_lag2_year<-merge(MGS_sample_age, fcs_covariates_long_BQR, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble


# Correlation with Average Escapement -------------------------------------

corr_MGS_run_year<-MGS_sample_age_covariates_wide_run_year %>% select(Average_Escapement, starts_with("cov")) 
corr_MGS_run_lag1_year<-MGS_sample_age_covariates_wide_run_lag1_year %>% select(Average_Escapement, starts_with("cov")) 
corr_MGS_run_lag2_year<-MGS_sample_age_covariates_wide_run_lag2_year %>% select(Average_Escapement, starts_with("cov")) 
corr_MGS_brood_year<-MGS_sample_age_covariates_wide_brood_year %>% select(Average_Escapement, starts_with("cov"))
corr_MGS_brood_lag1_year<-MGS_sample_age_covariates_wide_brood_lag1_year %>% select(Average_Escapement, starts_with("cov"))  
corr_MGS_brood_lag2_year<-MGS_sample_age_covariates_wide_brood_lag2_year %>% select(Average_Escapement, starts_with("cov")) 

#Full correlations
corr_var(corr_MGS_run_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_run_year.tiff")
corr_var(corr_MGS_run_lag1_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_run_year_lag1.tiff")
corr_var(corr_MGS_run_lag2_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_run_year_lag2.tiff")
corr_var(corr_MGS_brood_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_brood_year.tiff")
corr_var(corr_MGS_brood_lag1_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_brood_year_lag1.tiff")
corr_var(corr_MGS_brood_lag2_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/MGS/corr_MGS_brood_year_lag2.tiff")

#Significant p-values only 
corr_var(corr_MGS_run_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_run_year_sigp.tiff")
corr_var(corr_MGS_run_lag1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_run_year_lag1_sigp.tiff")
corr_var(corr_MGS_run_lag2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_run_year_lag2_sigp.tiff")
corr_var(corr_MGS_brood_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_brood_year_sigp.tiff")
corr_var(corr_MGS_brood_lag1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_brood_year_lag1_sigp.tiff")
corr_var(corr_MGS_brood_lag2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/MGS/corr_MGS_brood_year_lag2_sigp.tiff")



# Plotting ----------------------------------------------------------------
#setting up
MGS_sample_age_covariates_run_year$Covariate<-as.factor(MGS_sample_age_covariates_run_year$Covariate)
MGS_sample_age_covariates_run_lag1_year$Covariate<-as.factor(MGS_sample_age_covariates_run_lag1_year$Covariate)
MGS_sample_age_covariates_run_lag2_year$Covariate<-as.factor(MGS_sample_age_covariates_run_lag2_year$Covariate)
MGS_sample_age_covariates_brood_year$Covariate<-as.factor(MGS_sample_age_covariates_brood_year$Covariate)
MGS_sample_age_covariates_brood_lag1_year$Covariate<-as.factor(MGS_sample_age_covariates_brood_lag1_year$Covariate)
MGS_sample_age_covariates_brood_lag2_year$Covariate<-as.factor(MGS_sample_age_covariates_brood_lag2_year$Covariate)

macthes_MGS_run_year = unique(MGS_sample_age_covariates_run_year$Covariate)
macthes_MGS_run_lag1_year = unique(MGS_sample_age_covariates_run_lag1_year$Covariate)
macthes_MGS_run_lag2_year = unique(MGS_sample_age_covariates_run_lag2_year$Covariate)
macthes_MGS_brood_year =unique(MGS_sample_age_covariates_brood_year$Covariate)
macthes_MGS_brood_lag1_year = unique(MGS_sample_age_covariates_brood_lag1_year$Covariate)
macthes_MGS_brood_lag2_year = unique(MGS_sample_age_covariates_brood_lag2_year$Covariate)

Covariate_plots_MGS_run_year = list()
Covariate_plots_MGS_run_lag1_year = list()
Covariate_plots_MGS_run_lag2_year = list()
Covariate_plots_MGS_brood_year = list()
Covariate_plots_MGS_brood_lag1_year = list()
Covariate_plots_MGS_brood_lag2_year = list()

#For loops

#Run year
for(Covariate_ in macthes_MGS_run_year) {
  Covariate_plots_MGS_run_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_run_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_run_year$Covariate[MGS_sample_age_covariates_run_year$Covariate== Covariate_])
}

#Run year lag 1
for(Covariate_ in macthes_MGS_run_lag1_year) {
  Covariate_plots_MGS_run_lag1_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_run_lag1_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_run_lag1_year$Covariate[MGS_sample_age_covariates_run_lag1_year$Covariate== Covariate_])
}

#Run year lag 2
for(Covariate_ in macthes_MGS_run_lag2_year) {
  Covariate_plots_MGS_run_lag2_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_run_lag2_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_run_lag2_year$Covariate[MGS_sample_age_covariates_run_lag2_year$Covariate== Covariate_])
}

#brood year
for(Covariate_ in macthes_MGS_brood_year) {
  Covariate_plots_MGS_brood_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_brood_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_brood_year$Covariate[MGS_sample_age_covariates_brood_year$Covariate== Covariate_])
}

#brood year lag 1
for(Covariate_ in macthes_MGS_brood_lag1_year) {
  Covariate_plots_MGS_brood_lag1_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_brood_lag1_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_brood_lag1_year$Covariate[MGS_sample_age_covariates_brood_lag1_year$Covariate== Covariate_])
}

#brood year lag 2
for(Covariate_ in macthes_MGS_brood_lag2_year) {
  Covariate_plots_MGS_brood_lag2_year[[Covariate_]] = ggplot(MGS_sample_age_covariates_brood_lag2_year %>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(MGS_sample_age_covariates_brood_lag2_year$Covariate[MGS_sample_age_covariates_brood_lag2_year$Covariate== Covariate_])
}


# check plots with 
Covariate_plots_MGS_run_year[["cov_PDO_yearly_mean"]]
Covariate_plots_MGS_run_lag1_year[["cov_PDO_yearly_mean"]]
Covariate_plots_MGS_run_lag2_year[["cov_PDO_yearly_mean"]]
Covariate_plots_MGS_brood_year[["cov_PDO_yearly_mean"]]
Covariate_plots_MGS_brood_lag1_year[["cov_PDO_yearly_mean"]]
Covariate_plots_MGS_brood_lag2_year[["cov_PDO_yearly_mean"]]


#Group plots


#could do this in a list but somehow can't figure that out
#save
#all oceanic
oceanicCovariate_plot <-  pluck(Covariate_plots, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots, "cov_EPNP_summer_mean") +
  pluck(Covariate_plots, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=4)

oceanicCovariate_plot
ggsave(oceanicCovariate_plot, file="Plots/BQR_oceanic_Indices_Covariate.tiff")

#year plot
oceanicCovariate_plot_year <- pluck(Covariate_plots, "cov_PDO_yearly_mean") + 
  pluck(Covariate_plots, "cov_ONI_yearly_mean")+
  pluck(Covariate_plots, "cov_ONI_yearly_anomaly") + 
  pluck(Covariate_plots, "cov_SOI_yearly_mean") +
  pluck(Covariate_plots, "cov_NPI_yearly_mean") +
  pluck(Covariate_plots, "cov_NPI_yearly_anomaly") +
  pluck(Covariate_plots, "cov_NPGO_yearly_mean") +
  pluck(Covariate_plots, "cov_EPNP_yearly_mean") +
  pluck(Covariate_plots, "cov_ALPI_yearly_mean") +
  plot_layout(guides = 'collect', ncol=3)

oceanicCovariate_plot_year
ggsave(oceanicCovariate_plot_year, file="Plots/BQR_oceanic_Indices_year_Covariate.tiff")

#oceanic summer plot
oceanicCovariate_plot_summer <-  pluck(Covariate_plots, "cov_PDO_summer_mean") + 
  pluck(Covariate_plots, "cov_ONI_summer_mean") + 
  pluck(Covariate_plots, "cov_ONI_summer_anomaly") + 
  pluck(Covariate_plots, "cov_SOI_summer_mean") + 
  pluck(Covariate_plots, "cov_NPI_summer_mean") + 
  pluck(Covariate_plots, "cov_NPGO_summer_mean") +
  pluck(Covariate_plots, "cov_EPNP_summer_mean") +
  guide_area()+
  plot_layout(guides = 'collect', ncol=4)

oceanicCovariate_plot_summer
ggsave(oceanicCovariate_plot_summer, file="Plots/BQR_oceanic_Indices_Summer_Covariate.tiff")

##Zooplankton
zoopCovariate_plot <-  pluck(Covariate_plots, "cov_zoop_yearly_anomaly") + 
  pluck(Covariate_plots, "cov_zoop_winter_anomaly") + 
  pluck(Covariate_plots, "cov_zoop_summer_anomaly")+
  pluck(Covariate_plots, "cov_zoop_spring_anomaly") + 
  pluck(Covariate_plots, "cov_zoop_fall_anomaly") + 
  plot_layout(guides = 'collect', ncol=3)

zoopCovariate_plot
ggsave(zoopCovariate_plot, file="Plots/BQR_zoop_Covariate.tiff")

#Temperature
tempCovariate_plot <-  pluck(Covariate_plots, "cov_SST_MEDS_terminal_yearly_mean") + 
  pluck(Covariate_plots, "cov_SST_MEDS_terminal_summer_mean") + 
  pluck(Covariate_plots, "cov_SST_MEDS_offshore_yearly_mean") + 
  pluck(Covariate_plots, "cov_SST_MEDS_offshore_summer_mean") +
  pluck(Covariate_plots, "cov_SST_lighthouse_yearly_mean") + 
  pluck(Covariate_plots, "cov_SST_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=3)

tempCovariate_plot
ggsave(tempCovariate_plot, file="Plots/BQR_temp_Covariate.tiff")

#Salinity
salCovariate_plot <-  pluck(Covariate_plots, "cov_PPT_lighthouse_yearly_mean") + 
  pluck(Covariate_plots, "cov_PPT_lighthouse_summer_mean") + 
  plot_layout(guides = 'collect', ncol=2)

salCovariate_plot
ggsave(salCovariate_plot, file="Plots/BQR_sal_Covariate.tiff")

