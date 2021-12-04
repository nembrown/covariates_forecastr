library(tidyverse)
install.packages("devtools")
library(devtools)
devtools::install_github("thomasp85/patchwork")
library(patchwork)

pdo_1854_present<-read.csv("ersst.v5.pdo.csv") %>% as_tibble()
Atnarko_sample_age<-read.csv("Sample File_with age-specific data.csv") %>% as_tibble()
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
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_sync,  by=c("Brood_Year" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_run_lag1, by=c("Run_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_lag1,  by=c("Brood_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_run_lag2, by=c("Run_Year_Lag_2" = "Year"))
Atnarko_sample_age_pdo<-inner_join(Atnarko_sample_age_pdo, pdo_brood_lag2,  by=c("Brood_Year_Lag_2" = "Year"))

Atnarko_sample_age_pdo_gathered<-Atnarko_sample_age_pdo %>% gather(key="PDO_match", value="PDO.summer.av", PDO_run_sync, PDO_brood_sync, PDO_run_lag1, PDO_brood_lag1, PDO_run_lag2, PDO_brood_lag2) %>% 
                                                   mutate(Temperature_pattern = ifelse(PDO.summer.av > 0, "Hot years", "Cold Years"))
  

Atnarko_sample_age_pdo_gathered$PDO_match<-as.factor(Atnarko_sample_age_pdo_gathered$PDO_match)
levels(Atnarko_sample_age_pdo_gathered$PDO_match)

#### Ggplotting before going into forecast to see which ones make sense to use
levels(Atnarko_sample_age_pdo_gathered$PDO_match)
pdo_matches = unique(Atnarko_sample_age_pdo_gathered$PDO_match)
pdo_plots = list()

for(pdo_ in pdo_matches) {
  pdo_plots[[pdo_]] = ggplot(Atnarko_sample_age_pdo_gathered%>% filter(PDO_match == pdo_), aes( x=PDO.summer.av, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point(aes(shape=Temperature_pattern)) + geom_smooth()+
    ggtitle(Atnarko_sample_age_pdo_gathered$PDO_match[Atnarko_sample_age_pdo_gathered$PDO_match== pdo_])
}

# check plots with 
pdo_plots[["PDO_brood_sync"]]

#
allpdo_plot <-  wrap_plots( pdo_plots, ncol = 3) + plot_layout(guides = 'collect') 
allpdo_plot

#looks like PDO run_sync and run_lag1 are important for age 4 and 5 fish.  

#Altering it for forecastr
# Atnarko_sample_age_forecastr<- Atnarko_sample_age_pdo_gathered %>% filter(PDO_match == )


