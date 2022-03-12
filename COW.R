
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



# Read in data for covariate and Av escapement example ------------------------------------------------------------
View(fcs_covariates_long_COW)
fcs_covariates_long_COW<-fcs_covariates_long %>% filter(Stock_ERA == "COW")
fcs_covariates_combined_COW<-fcs_covariates_combined %>% filter(Stock_ERA == "COW")

####


GST_sample_age<-read.csv("Inputs/LGS_ESc_upto2021.csv") %>% as_tibble()
GST_sample_age<-GST_sample_age %>%  select(-c(Cov_Variable3, Cov_Variable2, cov_zoop_winter_anomaly))

# mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
#   mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
#   mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
#   mutate(Brood_Year_Lag_2 = Brood_Year + 2)%>% 


GST_sample_age_covariates_run_year<-merge(GST_sample_age, fcs_covariates_long_COW, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble

# Plotting ----------------------------------------------------------------
# Plot pdo vs. Average escapement
GST_sample_age_covariates_run_year$Covariate<-as.factor(GST_sample_age_covariates_run_year$Covariate)
levels(GST_sample_age_covariates_run_year$Covariate)
Covariate_matches = unique(GST_sample_age_covariates_run_year$Covariate)
Covariate_plots = list()
Covariate_plots_age3_6 = list()


for(Covariate_ in Covariate_matches) {
  Covariate_plots[[Covariate_]] = ggplot(GST_sample_age_covariates_run_year%>% filter(Covariate == Covariate_), aes( x=value, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point() + geom_smooth()+
    ggtitle(GST_sample_age_covariates_run_year$Covariate[GST_sample_age_covariates_run_year$Covariate== Covariate_])
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


# Correlation with Average Escapement -------------------------------------

# Correlation plotting, don't use the gathered file use the spread file 
GST_sample_age_covariates_wide<-merge(GST_sample_age, fcs_covariates_combined_COW, by.x=c("Run_Year"), by.y=c("year")) %>% as_tibble

GST_sample_Covariate_correlation<-GST_sample_age_covariates_wide %>% select(Average_Escapement, starts_with("cov"))


# Correlation plot of Average escapement vs. all other variables
corr_GST_sample_Covariate <- GST_sample_Covariate_correlation %>% correlate() %>% focus(Average_Escapement)
ggplot(corr_GST_sample_Covariate, aes(x = term, y = Average_Escapement)) +
  geom_bar(stat = "identity") +   geom_col(aes(fill = Average_Escapement >= 0)) + 
  ylab("Correlation with Average_Escapement") + 
  xlab("Variable") + coord_flip()
ggsave(file="Plots/corr_Covariate_plot.tiff")

### but want p values - so will use the lares package: 
corr_pdo<-corr_var(GST_sample_Covariate_correlation,Average_Escapement, top = 29, pvalue=TRUE, plot=FALSE)
corr_pdo
corr_pdo_plot<-corr_var(GST_sample_Covariate_correlation,Average_Escapement, top = 29, pvalue=TRUE, plot=TRUE)
corr_pdo_plot
ggsave(corr_pdo_plot, file="Plots/corr_Covariate_plot_2.tiff")

corr_pdo_plot_p<-corr_var(GST_sample_Covariate_correlation,Average_Escapement, max_pvalue=0.05, pvalue=TRUE, plot=TRUE)
corr_pdo_plot_p
ggsave(corr_pdo_plot_p, file="Plots/corr_Covariate_plot_sig_p.tiff")


#using Performance Analytics
chart.Correlation(GST_sample_Covariate_correlation , histogram=TRUE, pch=19)



# Making forecastR/shiny app input files --------------------------------------------

# Choose the PDO match that best correlates with the data (can pick up to 3 covariates)
#rename the variable "Cov_" so forecastR will read it automatically as a covariate
#Cov can't have any nas or will not work - so any "lag" variables need to be trimmed to the correct years
GST_sample_age_forecastr<- GST_sample_age_covariates_run_year %>% 
  select(-c(Run_Year_Lag_1, Run_Year_Lag_2, Brood_Year_Lag_1, Brood_Year_Lag_2, Temperature_pattern)) %>% 
  filter(Covariate %in% c("PDO_run_sync", "PDO_run_lag1")) %>% 
  pivot_wider(names_from=Covariate, values_from=value) %>% 
  rename(Cov_PDO_sync = PDO_run_sync, Cov_PDO_lag1 = PDO_run_lag1)


#The Forecasting year needs to be the last year with data, keep as 2021 if there is 2021 data
# GST_sample_age_forecastr$Forecasting_Year[GST_sample_age_forecastr$Forecasting_Year==2021]<-2018
# GST_sample_age_forecastr$Forecasting_Year<- as.integer(GST_sample_age_forecastr$Forecasting_Year)


write.csv(GST_sample_age_forecastr, file="Outputs/GST_sample_age_forecastr.csv", row.names=FALSE)



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
GST_sample_age_pdo<-left_join(GST_sample_age, Covariate_run_sync, by=c("Run_Year" = "Year"))
GST_sample_age_pdo<-left_join(GST_sample_age_pdo, Covariate_run_lag1, by=c("Run_Year_Lag_1" = "Year"))
GST_sample_age_pdo<-left_join(GST_sample_age_pdo, Covariate_run_lag2, by=c("Run_Year_Lag_2" = "Year"))
GST_sample_age_pdo<-left_join(GST_sample_age_pdo, Covariate_brood_sync,  by=c("Brood_Year" = "Year"))
GST_sample_age_pdo<-left_join(GST_sample_age_pdo, Covariate_brood_lag1,  by=c("Brood_Year_Lag_1" = "Year"))
GST_sample_age_pdo<-left_join(GST_sample_age_pdo, Covariate_brood_lag2,  by=c("Brood_Year_Lag_2" = "Year"))

GST_sample_age_covariates_run_year<-GST_sample_age_pdo %>% pivot_longer(names_to="Covariate", values_to="value", cols=c(PDO_run_sync,  PDO_run_lag1,PDO_run_lag2,  PDO_brood_sync, PDO_brood_lag1, PDO_brood_lag2)) %>% 
  mutate(Temperature_pattern = ifelse(value > 0, "Hot years", "Cold Years"))

