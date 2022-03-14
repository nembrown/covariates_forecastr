
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

# Restrict to only COW stock
fcs_covariates_long_COW<-fcs_covariates_long %>% filter(Stock_ERA == "COW")
fcs_covariates_combined_COW<-fcs_covariates_combined %>% filter(Stock_ERA == "COW")

#Load in Escapement data
LGS_sample_age<-read.csv("Inputs/LGS_ESc_upto2021.csv") %>% as_tibble()
LGS_sample_age<-LGS_sample_age %>%  dplyr::select(-c(Cov_Variable3, Cov_Variable2, cov_zoop_winter_anomaly)) %>% 
                                    mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
                                    mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
                                    mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
                                    mutate(Brood_Year_Lag_2 = Brood_Year + 2) 


# Matching escapement to covariates ---------------------------------------

#1. Long files - for use in visualization plotting 
#matching to run year + time lags
LGS_sample_age_covariates_run_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_run_lag1_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Run_Year_Lag_1"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_run_lag2_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Run_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#matching to brood year + time lags
LGS_sample_age_covariates_brood_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_brood_lag1_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_brood_lag2_year<-merge(LGS_sample_age, fcs_covariates_long_COW, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#2. Wide files - for use in correlation plots
#matching to run year + time lags
LGS_sample_age_covariates_wide_run_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_wide_run_lag1_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Run_Year_Lag_1"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_wide_run_lag2_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Run_Year_Lag_2"), by.y=c("year")) %>% as_tibble

#matching to brood year + time lags
LGS_sample_age_covariates_wide_brood_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Brood_Year"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_wide_brood_lag1_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Brood_Year_Lag_1"), by.y=c("year")) %>% as_tibble
LGS_sample_age_covariates_wide_brood_lag2_year<-merge(LGS_sample_age, fcs_covariates_combined_COW, by.x=c("Brood_Year_Lag_2"), by.y=c("year")) %>% as_tibble


# Correlation with Average Escapement -------------------------------------


corr_LGS_run_year<-LGS_sample_age_covariates_wide_run_year %>% select(Average_Escapement, starts_with("cov")) 
corr_LGS_run_lag1_year<-LGS_sample_age_covariates_wide_run_lag1_year %>% select(Average_Escapement, starts_with("cov")) 
corr_LGS_run_lag2_year<-LGS_sample_age_covariates_wide_run_lag2_year %>% select(Average_Escapement, starts_with("cov")) 
corr_LGS_brood_year<-LGS_sample_age_covariates_wide_brood_year %>% select(Average_Escapement, starts_with("cov"))
corr_LGS_brood_lag1_year<-LGS_sample_age_covariates_wide_brood_lag1_year %>% select(Average_Escapement, starts_with("cov"))  
corr_LGS_brood_lag2_year<-LGS_sample_age_covariates_wide_brood_lag2_year %>% select(Average_Escapement, starts_with("cov")) 

#Full correlations
corr_var(corr_LGS_run_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_run_year.tiff")
corr_var(corr_LGS_run_lag1_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_run_year_lag1.tiff")
corr_var(corr_LGS_run_lag2_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_run_year_lag2.tiff")
corr_var(corr_LGS_brood_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_brood_year.tiff")
corr_var(corr_LGS_brood_lag1_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_brood_year_lag1.tiff")
corr_var(corr_LGS_brood_lag2_year,Average_Escapement, plot=TRUE, top=29)
ggsave(file="Plots/corr_LGS_brood_year_lag2.tiff")

#Significant p-values only 
corr_var(corr_LGS_run_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_run_year_sigp.tiff")
corr_var(corr_LGS_run_lag1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_run_year_lag1_sigp.tiff")
corr_var(corr_LGS_run_lag2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_run_year_lag2_sigp.tiff")
corr_var(corr_LGS_brood_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_brood_year_sigp.tiff")
corr_var(corr_LGS_brood_lag1_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_brood_year_lag1_sigp.tiff")
corr_var(corr_LGS_brood_lag2_year,Average_Escapement, plot=TRUE, top=29, max_pvalue=0.05, pvalue=TRUE)
ggsave(file="Plots/corr_LGS_brood_year_lag2_sigp.tiff")




### but want p values - so will use the lares package: 
corr_pdo<-corr_var(corr_LGS_run_year,Average_Escapement, plot=TRUE)
corr_pdo

corr_var(corr_LGS_sample_age_covariates_wide_run_year,Average_Escapement, top = 29, pvalue=TRUE, plot=TRUE)
corr_pdo_plot
ggsave(corr_pdo_plot, file="Plots/corr_Covariate_plot_2.tiff")

corr_pdo_plot_p<-corr_var(LGS_sample_Covariate_correlation,Average_Escapement, max_pvalue=0.05, pvalue=TRUE, plot=TRUE)
corr_pdo_plot_p
ggsave(corr_pdo_plot_p, file="Plots/corr_Covariate_plot_sig_p.tiff")


#using Performance Analytics
chart.Correlation(LGS_sample_Covariate_correlation , histogram=TRUE, pch=19)

# Correlation plot of Average escapement vs. all other variables
ggplot(corr_LGS_run_year, aes(x = term, y = Average_Escapement)) +
  geom_bar(stat = "identity") +   geom_col(aes(fill = Average_Escapement >= 0)) + 
  ylab("Correlation with Average_Escapement") + 
  xlab("Variable") + coord_flip()
ggsave(file="Plots/corr_Covariate_plot.tiff")


####Unused

#the sample data is only 1990 to present for run year, just use the summer average
Covariate_run_sync<-Covariate_1854_present  %>% select(Year, value) %>% mutate(PDO_run_sync=value, .keep="unused")
Covariate_run_lag1<-Covariate_1854_present  %>% select(Year, value) %>% mutate(PDO_run_lag1=value, .keep="unused")
Covariate_run_lag2<-Covariate_1854_present  %>% select(Year, value) %>% mutate(PDO_run_lag2=value, .keep="unused")

#1987 to present for brood year just use the summer average
Covariate_brood_sync<-Covariate_1854_present %>% select(Year, value)%>% mutate(PDO_brood_sync=value, .keep="unused")
Covariate_brood_lag1<-Covariate_1854_present %>% select(Year, value)%>% mutate(PDO_brood_lag1=value, .keep="unused")
Covariate_brood_lag2<-Covariate_1854_present %>% select(Year, value)%>% mutate(PDO_brood_lag2=value, .keep="unused")

#Make one file with different joins
LGS_sample_age_pdo<-left_join(LGS_sample_age, Covariate_run_sync, by=c("Run_Year" = "Year"))
LGS_sample_age_pdo<-left_join(LGS_sample_age_pdo, Covariate_run_lag1, by=c("Run_Year_Lag_1" = "Year"))
LGS_sample_age_pdo<-left_join(LGS_sample_age_pdo, Covariate_run_lag2, by=c("Run_Year_Lag_2" = "Year"))
LGS_sample_age_pdo<-left_join(LGS_sample_age_pdo, Covariate_brood_sync,  by=c("Brood_Year" = "Year"))
LGS_sample_age_pdo<-left_join(LGS_sample_age_pdo, Covariate_brood_lag1,  by=c("Brood_Year_Lag_1" = "Year"))
LGS_sample_age_pdo<-left_join(LGS_sample_age_pdo, Covariate_brood_lag2,  by=c("Brood_Year_Lag_2" = "Year"))

LGS_sample_age_covariates_run_year<-LGS_sample_age_pdo %>% pivot_longer(names_to="Covariate", values_to="value", cols=c(PDO_run_sync,  PDO_run_lag1,PDO_run_lag2,  PDO_brood_sync, PDO_brood_lag1, PDO_brood_lag2)) %>% 
  mutate(Temperature_pattern = ifelse(value > 0, "Hot years", "Cold Years"))




# Plotting ----------------------------------------------------------------
LGS_sample_age_covariates_run_year$Covariate<-as.factor(LGS_sample_age_covariates_run_year$Covariate)
levels(LGS_sample_age_covariates_run_year$Covariate)
Covariate_matches = unique(LGS_sample_age_covariates_run_year$Covariate)
Covariate_plots = list()
Covariate_plots_age3_6 = list()


for(Covariate_ in Covariate_matches) {
  Covariate_plots[[Covariate_]] = ggplot(LGS_sample_age_covariates_run_year%>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(LGS_sample_age_covariates_run_year$Covariate[LGS_sample_age_covariates_run_year$Covariate== Covariate_])
}

# check plots with 
Covariate_plots[["cov_PDO_yearly_mean"]]

#Group plots


#could do this ina list but somehow can't figure that out
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
ggsave(oceanicCovariate_plot, file="Plots/COW_oceanic_Indices_Covariate.tiff")

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
ggsave(oceanicCovariate_plot_year, file="Plots/COW_oceanic_Indices_year_Covariate.tiff")

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
ggsave(oceanicCovariate_plot_summer, file="Plots/COW_oceanic_Indices_Summer_Covariate.tiff")

##Zooplankton
zoopCovariate_plot <-  pluck(Covariate_plots, "cov_zoop_yearly_anomaly") + 
                       pluck(Covariate_plots, "cov_zoop_winter_anomaly") + 
                       pluck(Covariate_plots, "cov_zoop_summer_anomaly")+
                       pluck(Covariate_plots, "cov_zoop_spring_anomaly") + 
                       pluck(Covariate_plots, "cov_zoop_fall_anomaly") + 
                       plot_layout(guides = 'collect', ncol=3)
                      
zoopCovariate_plot
ggsave(zoopCovariate_plot, file="Plots/COW_zoop_Covariate.tiff")

#Temperature
tempCovariate_plot <-  pluck(Covariate_plots, "cov_SST_MEDS_terminal_yearly_mean") + 
                       pluck(Covariate_plots, "cov_SST_MEDS_terminal_summer_mean") + 
                       pluck(Covariate_plots, "cov_SST_MEDS_offshore_yearly_mean") + 
                       pluck(Covariate_plots, "cov_SST_MEDS_offshore_summer_mean") +
                       pluck(Covariate_plots, "cov_SST_lighthouse_yearly_mean") + 
                       pluck(Covariate_plots, "cov_SST_lighthouse_summer_mean") + 
                       plot_layout(guides = 'collect', ncol=3)

tempCovariate_plot
ggsave(tempCovariate_plot, file="Plots/COW_temp_Covariate.tiff")

#Salinity
salCovariate_plot <-  pluck(Covariate_plots, "cov_PPT_lighthouse_yearly_mean") + 
                       pluck(Covariate_plots, "cov_PPT_lighthouse_summer_mean") + 
                       plot_layout(guides = 'collect', ncol=2)

salCovariate_plot
ggsave(salCovariate_plot, file="Plots/COW_sal_Covariate.tiff")


