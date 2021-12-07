
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

pdo_1854_present<-read.csv("Inputs/ersst.v5.pdo.csv") %>% as_tibble()
Atnarko_sample_age<-read.csv("Inputs/Sample File_with age-specific data.csv") %>% as_tibble()
Atnarko_sample_age<-Atnarko_sample_age %>% mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
                                           mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
                                           mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
                                           mutate(Brood_Year_Lag_2 = Brood_Year + 2)%>% 
                                           select(-c(Cov_Variable3, Cov_Hatch_Rel, Cov_Variable2))



#the sample data is only 1990 to present for run year, just use the summer average
pdo_run_sync<-pdo_1854_present %>% filter(Year >1989) %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_sync=PDO.summer.av, .keep="unused")
pdo_run_lag1<-pdo_1854_present %>% filter(Year >1989) %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_lag1=PDO.summer.av, .keep="unused")
pdo_run_lag2<-pdo_1854_present %>% filter(Year >1989) %>% select(Year, PDO.summer.av) %>% mutate(PDO_run_lag2=PDO.summer.av, .keep="unused")

#1987 to present for brood year just use the summer average
pdo_brood_sync<-pdo_1854_present %>% filter(Year >1986)%>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_sync=PDO.summer.av, .keep="unused")
pdo_brood_lag1<-pdo_1854_present %>% filter(Year >1986)%>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_lag1=PDO.summer.av, .keep="unused")
pdo_brood_lag2<-pdo_1854_present %>% filter(Year >1986)%>% select(Year, PDO.summer.av)%>% mutate(PDO_brood_lag2=PDO.summer.av, .keep="unused")

#Make one file with different joins
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age, pdo_run_sync, by=c("Run_Year" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_run_lag1, by=c("Run_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_run_lag2, by=c("Run_Year_Lag_2" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_sync,  by=c("Brood_Year" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_lag1,  by=c("Brood_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_lag2,  by=c("Brood_Year_Lag_2" = "Year"))

Atnarko_sample_age_pdo_gathered<-Atnarko_sample_age_pdo %>% gather(key="PDO_match", value="PDO.summer.av", PDO_run_sync,  PDO_run_lag1,PDO_run_lag2,  PDO_brood_sync, PDO_brood_lag1, PDO_brood_lag2) %>% 
                                                   mutate(Temperature_pattern = ifelse(PDO.summer.av > 0, "Hot years", "Cold Years"))
  

Atnarko_sample_age_pdo_gathered$PDO_match<-as.factor(Atnarko_sample_age_pdo_gathered$PDO_match)
levels(Atnarko_sample_age_pdo_gathered$PDO_match)


# Plotting ----------------------------------------------------------------

# Plot pdo vs. Average escapement
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
  pdo_plots_age3_6[[pdo_]] = ggplot(Atnarko_sample_age_pdo_gathered%>% filter(PDO_match == pdo_, Age_Class==c(3,6)), aes( x=PDO.summer.av, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
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
Atnarko_sample_pdo_correlation %>% correlate() %>%  network_plot()

#using Performance Analytics
chart.Correlation(Atnarko_sample_pdo_correlation , histogram=TRUE, pch=19)



# Making forecastR input files --------------------------------------------

# Choose the PDO match that best correlates with the data (can pick up to 3 covariates)
#rename the variable "Cov_" so forecastR will read it automatically as a covariate
Atnarko_sample_age_forecastr<- Atnarko_sample_age_pdo_gathered %>% filter(PDO_match == "PDO_run_sync") %>% 
                               rename(Cov_PDO = PDO.summer.av) %>% select(-c(Run_Year_Lag_1, Run_Year_Lag_2, Brood_Year_Lag_1, Brood_Year_Lag_2, PDO_match, Temperature_pattern))
# Atnarko_sample_age_forecastr$Stock_Name[1,]<- "Atnarko"
# Atnarko_sample_age_forecastr$Stock_Species<- "Chinook"
# Atnarko_sample_age_forecastr$Stock_Abundance<- "Escapement"
Atnarko_sample_age_forecastr$Forecasting_Year<-2019
Atnarko_sample_age_forecastr$Forecasting_Year<- as.integer(Atnarko_sample_age_forecastr$Forecasting_Year)
# 

write.csv(Atnarko_sample_age_forecastr, file="Outputs/Atnarko_sample_age_forecastr.csv")

View(Atnarko_sample_age_forecastr)
