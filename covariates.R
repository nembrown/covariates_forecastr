library(tidyverse)


pdo_1854_present<-read.csv("ersst.v5.pdo.csv") %>% as_tibble()
Atnarko_sample_age<-read.csv("Sample File_with age-specific data.csv") %>% as_tibble()
Atnarko_sample_age<-Atnarko_sample_age %>% mutate(Run_Year_Lag_1 = Run_Year + 1) %>% 
                                           mutate(Run_Year_Lag_2 = Run_Year + 2) %>% 
                                           mutate(Brood_Year_Lag_1 = Brood_Year + 1) %>% 
                                           mutate(Brood_Year_Lag_2 = Brood_Year + 2)%>% 
                                           select(-c(Cov_Variable3, Cov_Hatch_Rel, Cov_Variable2))



#the sample data is only 1990 to present for run year, just use the summer average
pdo_1990_present<-pdo_1854_present %>% filter(Year >1989) %>% 
                                       select(Year, PDO.year.av, PDO.summer.av) %>% 
                                       rename(Cov_PDO_year_run=PDO.year.av,Cov_PDO_summer= PDO.summer.av) %>% 
                                       select(-c(Cov_PDO_year_run)) %>% 
                                       mutate(Cov_cat = ifelse(Cov_PDO_summer > 0, "Hot years", "Cold Years"))

#1987 to present for brood year just use the summer average
pdo_1987_present<-pdo_1854_present %>% filter(Year >1986)%>% 
                                       select(Year, PDO.year.av, PDO.summer.av) %>% 
                                       rename(Cov_PDO_year_brood=PDO.year.av, Cov_PDO_summer=PDO.summer.av) %>% 
                                       select(-c(Cov_PDO_year_brood)) %>% 
                                       mutate(Cov_cat = ifelse(Cov_PDO_summer > 0, "Hot years", "Cold Years"))



Atnarko_sample_age_pdo_run_sync<-inner_join(Atnarko_sample_age, pdo_1990_present, by=c("Run_Year" = "Year"))
Atnarko_sample_age_pdo_brood_sync<-inner_join(Atnarko_sample_age, pdo_1987_present,  by=c("Brood_Year" = "Year"))
Atnarko_sample_age_pdo_run_lag1<-inner_join(Atnarko_sample_age, pdo_1990_present, by=c("Run_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo_brood_lag1<-inner_join(Atnarko_sample_age, pdo_1987_present,  by=c("Brood_Year_Lag_1" = "Year"))
Atnarko_sample_age_pdo_run_lag2<-inner_join(Atnarko_sample_age, pdo_1990_present, by=c("Run_Year_Lag_2" = "Year"))
Atnarko_sample_age_pdo_brood_lag2<-inner_join(Atnarko_sample_age, pdo_1987_present,  by=c("Brood_Year_Lag_2" = "Year"))


write.csv(Atnarko_sample_age_pdo_run_sync, file="Atnarko_sample_age_pdo_run_sync.csv")
write.csv(Atnarko_sample_age_pdo_brood_sync, file="Atnarko_sample_age_pdo_brood_sync.csv")
write.csv(Atnarko_sample_age_pdo_run_lag1, file="Atnarko_sample_age_pdo_run_lag1.csv")
write.csv(Atnarko_sample_age_pdo_brood_lag1, file="Atnarko_sample_age_pdo_brood_lag1.csv")
write.csv(Atnarko_sample_age_pdo_run_lag2, file="Atnarko_sample_age_pdo_run_lag2.csv")
write.csv(Atnarko_sample_age_pdo_brood_lag2, file="Atnarko_sample_age_pdo_brood_lag2.csv")



#### Ggplotting before going into forecast to see which ones make sense to use
theme_classic()

#sync run summer pdo
sync_run<-ggplot(Atnarko_sample_age_pdo_run_sync, aes( x=Cov_PDO_summer_run, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
  geom_point(aes(size=Cov_PDO_summer_run, shape=Cov_cat)) + geom_smooth()+ggtitle("Synced to run year")

#sync brood summer pdo
sync_brood<-ggplot(Atnarko_sample_age_pdo_brood_sync, aes( x=Cov_PDO_summer_brood, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
  geom_point(aes(size=Cov_PDO_summer_brood, shape=Cov_cat)) + geom_smooth()+ggtitle("Synced to brood year")


#Lag 1 year
#lag1 run summer pdo
lag1_run<-ggplot(Atnarko_sample_age_pdo_run_lag1, aes( x=Cov_PDO_summer_run, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
  geom_point(aes(size=Cov_PDO_summer_run, shape=Cov_cat)) + geom_smooth()+ggtitle("Lag of 1 year to run year")

#lag1 brood summer pdo
lag1_brood<-ggplot(Atnarko_sample_age_pdo_brood_lag1, aes( x=Cov_PDO_summer_brood, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
  geom_point(aes(size=Cov_PDO_summer_brood, shape=Cov_cat)) + geom_smooth()+ggtitle("Lag of 1 year to brood year")

dataset_list<-list("Synced to run year"= Atnarko_sample_age_pdo_run_sync, 
                 "Synced to brood year" = Atnarko_sample_age_pdo_brood_sync, 
                 "Lag of 1 year to run year" = Atnarko_sample_age_pdo_run_lag1, 
                 "Lag of 1 year to brood year" = Atnarko_sample_age_pdo_brood_lag1, 
                 "Lag of 2 years to run year" = Atnarko_sample_age_pdo_run_lag2, 
                 "Lag of 2 years to brood year" = Atnarko_sample_age_pdo_brood_lag2)

dataset_plots = list()

for(dataset_ in dataset_list) {
  dataset_plots[[dataset_]] = ggplot(dataset_ , aes( x=Cov_PDO_summer, y=Average_Escapement, col=as.factor(Age_Class), fill=as.factor(Age_Class))) + 
    geom_point(aes(size=Cov_PDO_summer, shape=Cov_cat)) + geom_smooth()
}

# check region with 116
region_plots[["116"]]

#This is figure 3
allregions_plot <-  wrap_plots( region_plots, ncol = 7) + plot_layout(guides = 'collect') 
allregions_plot
ggsave(allregions_plot, file="Plots/fig3.tiff", unit="cm",width=18.2,height=23.7, dpi=600)


