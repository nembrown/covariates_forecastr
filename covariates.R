
# Load libraries ----------------------------------------------------------

library(tidyverse)
# install.packages("devtools")
library(devtools)
# devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(ggcorrplot)
library(corrr)
library(PerformanceAnalytics)


# Read in data for PDO and Atnarko example ------------------------------------------------------------

pdo_1854_present<-read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",  header=TRUE, skip=1, fill=TRUE)
#Note, the last year of data gets imported with -99 as the values missing appended to the previous month's value
#Makes for eg. November a character
#best to exclude these first or if just doing summer and have full data for summer then can ignore
pdo_1854_present<-pdo_1854_present %>% as_tibble() %>% 
                  mutate(PDO.summer.av= rowMeans(select(.,May, Jun, Jul, Aug, Sep)))  

Atnarko_sample_age<-read.csv("Inputs/Sample File_with age-specific data.csv") %>% as_tibble()
Atnarko_sample_age<-Atnarko_sample_age %>% mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
                                           mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
                                           mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
                                           mutate(Brood_Year_Lag_2 = Brood_Year + 2)%>% 
                                           select(-c(Cov_Variable3, Cov_Hatch_Rel, Cov_Variable2))


#the sample data is only 1990 to present for run year, just use the summer average
pdo_run_sync<-pdo_1854_present  %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_sync=PDO.summer.av, .keep="unused")
pdo_run_lag1<-pdo_1854_present  %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_lag1=PDO.summer.av, .keep="unused")
pdo_run_lag2<-pdo_1854_present  %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_lag2=PDO.summer.av, .keep="unused")

#1987 to present for brood year just use the summer average
pdo_brood_sync<-pdo_1854_present %>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_sync=PDO.summer.av, .keep="unused")
pdo_brood_lag1<-pdo_1854_present %>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_lag1=PDO.summer.av, .keep="unused")
pdo_brood_lag2<-pdo_1854_present %>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_lag2=PDO.summer.av, .keep="unused")

#Make one file with different joins
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age, pdo_run_sync, by=c("Run_Year" = "Year"))
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age_pdo, pdo_run_lag1, by=c("Run_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age_pdo, pdo_run_lag2, by=c("Run_Year_Lag_2" = "Year"))
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age_pdo, pdo_brood_sync,  by=c("Brood_Year" = "Year"))
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age_pdo, pdo_brood_lag1,  by=c("Brood_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-left_join(Atnarko_sample_age_pdo, pdo_brood_lag2,  by=c("Brood_Year_Lag_2" = "Year"))

Atnarko_sample_age_pdo_gathered<-Atnarko_sample_age_pdo %>% pivot_longer(names_to="PDO_match", values_to="PDO.summer.av", cols=c(PDO_run_sync,  PDO_run_lag1,PDO_run_lag2,  PDO_brood_sync, PDO_brood_lag1, PDO_brood_lag2)) %>% 
                                                   mutate(Temperature_pattern = ifelse(PDO.summer.av > 0, "Hot years", "Cold Years"))
  

# Plotting ----------------------------------------------------------------

# Plot pdo vs. Average escapement
Atnarko_sample_age_pdo_gathered$PDO_match<-as.factor(Atnarko_sample_age_pdo_gathered$PDO_match)
levels(Atnarko_sample_age_pdo_gathered$PDO_match)
pdo_matches = unique(Atnarko_sample_age_pdo_gathered$PDO_match)
pdo_plots = list()
pdo_plots_age3_6 = list()

for(pdo_ in pdo_matches) {
  pdo_plots[[pdo_]] = ggplot(Atnarko_sample_age_pdo_gathered%>% filter(PDO_match == pdo_), aes( x=PDO.summer.av, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point(aes(shape=Temperature_pattern)) + geom_smooth()+
    ggtitle(Atnarko_sample_age_pdo_gathered$PDO_match[Atnarko_sample_age_pdo_gathered$PDO_match== pdo_])
}

# check plots with 
pdo_plots[["PDO_brood_sync"]]

#save
allpdo_plot <-  wrap_plots( pdo_plots, ncol = 3) + plot_layout(guides = 'collect') 
allpdo_plot
ggsave(allpdo_plot, file="Plots/PDO.tiff")

#looks like PDO run_sync and run_lag1 are important for age 4 and 5 fish. 

# Do again with just 3 and 6 year old fish to see if zoomed in the pattern is there
for(pdo_ in pdo_matches) {
  pdo_plots_age3_6[[pdo_]] = ggplot(Atnarko_sample_age_pdo_gathered%>% filter(PDO_match == pdo_, Age_Class %in% c(3,6)), aes( x=PDO.summer.av, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point(aes(shape=Temperature_pattern)) + geom_smooth()+
    ggtitle(Atnarko_sample_age_pdo_gathered$PDO_match[Atnarko_sample_age_pdo_gathered$PDO_match== pdo_])
}

allpdo_plot_3_6 <-  wrap_plots( pdo_plots_age3_6, ncol = 3) + plot_layout(guides = 'collect') 
allpdo_plot_3_6
ggsave(allpdo_plot_3_6, file="Plots/PDO_3_6.tiff")


# Correlation plotting, don't use the gathered file use the spread file 
Atnarko_sample_pdo_correlation<-Atnarko_sample_age_pdo %>% select(Average_Escapement, PDO_run_sync, PDO_run_lag1, PDO_run_lag2, 
                                                                                                                                 PDO_brood_sync, PDO_brood_lag1, PDO_brood_lag2)

# Correlation plot
corr_Atnarko_sample_pdo <- round(cor(Atnarko_sample_pdo_correlation, use="pairwise.complete.obs"), 1)
corr_pmat_Atnarko_sample_pdo <- cor_pmat(corr_Atnarko_sample_pdo)
ggcorrplot(corr_Atnarko_sample_pdo, lab=TRUE,  type = "lower", p.mat = corr_pmat_Atnarko_sample_pdo)

#Network plot
# Atnarko_sample_pdo_correlation %>% correlate() %>%  network_plot()

#using Performance Analytics
chart.Correlation(Atnarko_sample_pdo_correlation , histogram=TRUE, pch=19)



# Making forecastR/shiny app input files --------------------------------------------

# Choose the PDO match that best correlates with the data (can pick up to 3 covariates)
#rename the variable "Cov_" so forecastR will read it automatically as a covariate
#Cov can't have any nas or will not work - so any "lag" variables need to be trimmed to the correct years
Atnarko_sample_age_forecastr<- Atnarko_sample_age_pdo_gathered %>% 
                               select(-c(Run_Year_Lag_1, Run_Year_Lag_2, Brood_Year_Lag_1, Brood_Year_Lag_2, Temperature_pattern)) %>% 
                               filter(PDO_match %in% c("PDO_run_sync", "PDO_run_lag1")) %>% 
                               pivot_wider(names_from=PDO_match, values_from=PDO.summer.av) %>% 
                               rename(Cov_PDO_sync = PDO_run_sync, Cov_PDO_lag1 = PDO_run_lag1)


#The Forecasting year needs to be the last year with data, keep as 2021 if there is 2021 data
# Atnarko_sample_age_forecastr$Forecasting_Year[Atnarko_sample_age_forecastr$Forecasting_Year==2021]<-2018
# Atnarko_sample_age_forecastr$Forecasting_Year<- as.integer(Atnarko_sample_age_forecastr$Forecasting_Year)


write.csv(Atnarko_sample_age_forecastr, file="Outputs/Atnarko_sample_age_forecastr.csv", row.names=FALSE)

