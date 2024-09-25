
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

#Run this second
correlate_covs(cov_data_file="fcs_covariates_interpolated.csv",
               escapement_data_file = "Inputs/MGS_Esc_Age5_upto2023.csv",
               modelstock = "MGS",
               stock= "BQR",
               year_match=c("Run_Year_Lead1", "Run_Year_Lead2", "Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2"),
               escapement_type="Escapement",
               truncate_ts=1998,
               age_specific=TRUE,
               age_combine=TRUE,
               age_class=NA)

#Run this first
correlate_covs<-function(cov_data_file,
                         escapement_data_file,
                         modelstock = NA_character_, 
                         stock= NA_character_, 
                         year_match= NA_character_,
                         escapement_type=NA_character_,
                         truncate_ts=NA_integer_,
                         age_specific=FALSE,
                         age_combine=FALSE,
                         age_class = NA_integer_){
  
  for (i in 1:length(year_match)){
    
  cov_data<-read.csv(cov_data_file) %>% as_tibble()
  escapement_data<-read.csv(escapement_data_file) %>% as_tibble()
  
  if (!is.na(truncate_ts)){
    escapement_data<-  escapement_data %>% filter(Brood_Year >= truncate_ts)
  }
  
  
  rel_file_dir <- normalizePath(paste0("Plots/", modelstock), mustWork = FALSE)
  
  if(dir.exists(rel_file_dir) == FALSE) {
    cat(paste0("Plots directory does not exist, creating:\n", rel_file_dir))
    dir.create(rel_file_dir)
  }
  
cov_data_long<- cov_data %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
  mutate(var_cat = case_when(
    str_detect(Covariate, "SST|_T_|temp") ~ "Temperature", 
    str_detect(Covariate, "PPT|salinity") ~ "Salinity",
    str_detect(Covariate, "zoop|Ichthyoplankton") ~ "Zooplankton", 
    str_detect(Covariate, "juvenile") ~ "Juvenile Salmon", 
    str_detect(Covariate, "cope|Copepod") ~ "Copepod", 
    str_detect(Covariate, "PC") ~ "Principal Components", 
    str_detect(Covariate, "herring") ~ "Herring",
    str_detect(Covariate, "water") ~ "Hydrographic", 
    str_detect(Covariate, "Upwelling") ~ "Upwelling", 
    str_detect(Covariate, "trans|Trans") ~ "Transition", 
    str_detect(Covariate, "ALPI") ~ "ALPI", 
    str_detect(Covariate, "PDO") ~ "PDO", 
    str_detect(Covariate, "SOI") ~ "SOI", 
    str_detect(Covariate, "ONI") ~ "ONI", 
    str_detect(Covariate, "NPI") ~ "NPI", 
    str_detect(Covariate, "NPGO") ~ "NPGO", 
    str_detect(Covariate, "EPNP") ~ "EPNP", 
   str_detect(Covariate, "EV") ~ "EVs")) %>% 
  mutate(var_timing = case_when(
    str_detect(Covariate, "year") ~ "Year", 
    str_detect(Covariate, "summer") ~ "Summer")) %>% 
  mutate(var_location = case_when(
    str_detect(Covariate, "offshore") ~ "offshore", 
    str_detect(Covariate, "terminal") ~ "terminal", 
    TRUE ~ "terminal")) 

# Restrict to only stock
cov_data_long_stock<-cov_data_long %>% filter(Stock_ERA == stock, Stock_model == modelstock) %>% select(-Stock_model)
cov_data_stock<-cov_data %>% filter(Stock_ERA == stock, Stock_model == modelstock) %>% select(-Stock_model)

if(escapement_type=="Terminal Run"){
  escapement_data<-escapement_data %>% rename(Escapement_type=Average_Terminal_Run)
}else if (escapement_type=="Escapement"){
  escapement_data<-escapement_data %>% rename(Escapement_type=Average_Escapement)
}


if(age_specific==FALSE){

escapement_data<-escapement_data %>%  select(Run_Year, Escapement_type) %>% 
                                      mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
                                      mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
                                      mutate(Brood_Year = Run_Year - 4) %>% 
                                      mutate(Brood_Year_Lag1 = Run_Year - 3) %>% 
                                      mutate(Brood_Year_Lag2 = Run_Year - 2) %>% 
                                      select(Escapement_type, all_of(c(year_match[[i]])))

cov_data_stock_roll<- zoo::rollmean(cov_data_stock %>% select(-Stock_ERA), k=3) %>% as_tibble()
#Brood year
cov_data_stock_roll_long<- cov_data_stock_roll %>% pivot_longer(cols = starts_with("cov"), names_to = "Covariate", values_to = "value") %>% 
  mutate(var_cat = case_when(
    str_detect(Covariate, "SST|_T_|temp") ~ "Temperature", 
    str_detect(Covariate, "PPT|salinity") ~ "Salinity",
    str_detect(Covariate, "zoop|Ichthyoplankton") ~ "Zooplankton", 
    str_detect(Covariate, "juvenile") ~ "Juvenile Salmon", 
    str_detect(Covariate, "cope|Cope") ~ "Copepod", 
    str_detect(Covariate, "PC") ~ "Principal Components", 
    str_detect(Covariate, "herring") ~ "Herring",
    str_detect(Covariate, "water") ~ "Hydrographic", 
    str_detect(Covariate, "Upwelling") ~ "Upwelling", 
    str_detect(Covariate, "trans|Trans") ~ "Transition", 
    str_detect(Covariate, "ALPI") ~ "ALPI", 
    str_detect(Covariate, "PDO") ~ "PDO", 
    str_detect(Covariate, "SOI") ~ "SOI", 
    str_detect(Covariate, "ONI") ~ "ONI", 
    str_detect(Covariate, "NPI") ~ "NPI", 
    str_detect(Covariate, "NPGO") ~ "NPGO", 
    str_detect(Covariate, "EPNP") ~ "EPNP", 
    str_detect(Covariate, "EV") ~ "EVs")) %>% 
  mutate(var_timing = case_when(
    str_detect(Covariate, "year") ~ "Year", 
    str_detect(Covariate, "summer") ~ "Summer")) %>% 
  mutate(var_location = case_when(
    str_detect(Covariate, "offshore") ~ "offshore", 
    str_detect(Covariate, "terminal") ~ "terminal", 
    TRUE ~ "terminal")) 
} else if (age_combine==TRUE  & year_match[[i]] %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
           escapement_data<-escapement_data %>%  group_by(Brood_Year) %>%
           summarise(Escapement_type = sum(Escapement_type, na.rm=TRUE)) %>% 
           mutate(Brood_Year_Lag1 = Brood_Year + 1) %>% 
           mutate(Brood_Year_Lag2 = Brood_Year + 2) %>% 
           select(Escapement_type, all_of(c(year_match[[i]])))
           
} else if (age_combine==TRUE){
           escapement_data<-escapement_data %>%  group_by(Run_Year) %>%
           summarise(Escapement_type = sum(Escapement_type, na.rm=TRUE)) %>% 
           mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
           mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
           select(Escapement_type, all_of(c(year_match[[i]])))
} else {
  escapement_data<-escapement_data %>%  select(Brood_Year, Run_Year, Escapement_type, Age_Class) %>% 
  mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
  mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
  mutate(Brood_Year_Lag1 = Brood_Year + 1) %>% 
  mutate(Brood_Year_Lag2 = Brood_Year + 2) %>% 
  select(Escapement_type, Age_Class, all_of(c(year_match[[i]])))
}

# Matching escapement to covariates ---------------------------------------

#1. Long files - for use in visualization plotting 
#matching to Run Year + time lags
if(age_specific==FALSE & year_match[[i]] %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
  escapement_data_covariates<-merge(escapement_data ,cov_data_stock_roll_long, by.x=year_match[[i]], by.y=c("year"), all.x=TRUE) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock_roll, by.x=year_match[[i]], by.y=c("year"), all.x=TRUE) %>% as_tibble
} else  {
  escapement_data_covariates<-merge(escapement_data, cov_data_long_stock, by.x=year_match[[i]], by.y=c("year"), all.x=TRUE) %>% as_tibble
  escapement_data_covariates_wide<-merge(escapement_data, cov_data_stock, by.x=year_match[[i]], by.y=c("year"), all.x=TRUE) %>% as_tibble
}


#Filter for age class
if(age_specific==TRUE & age_combine==FALSE){
  escapement_data_covariates<- escapement_data_covariates %>% filter(Age_Class == age_class)
  escapement_data_covariates_wide<- escapement_data_covariates_wide %>% filter(Age_Class == age_class)
  }


### drop columns with NAs 
escapement_data_covariates_wide<-  escapement_data_covariates_wide %>% select(Escapement_type, contains("Brood"), contains("Run"), (contains("cov") & where(~! any(is.na(.)))))
escapement_data_covariates<-  escapement_data_covariates %>% group_by(Covariate) %>% filter(!is.na(sum(value))) %>% ungroup()


# Correlation with Escapement type -------------------------------------

corr_stock<-escapement_data_covariates_wide %>% select(Escapement_type, starts_with("cov")) 

#Full correlations
corr_var(corr_stock,Escapement_type, plot=TRUE, top=40) %>% ggsave(file=paste0("Plots/", modelstock, "/corr_", year_match[[i]], "_", modelstock, "_Age_",age_class, ".tiff"))

#Significant p-values only - need to save the plots
corr_var(corr_stock,Escapement_type, plot=TRUE, top=40, max_pvalue=0.05,SAVE=TRUE)%>% ggsave(file=paste0("Plots/", modelstock, "/corr_psig_", year_match[[i]],"_", modelstock, "_Age_",age_class,".tiff"))


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
#California_current_ocean indices
california_current_oceanic_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_PDO_Sum_Dec_March", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_PDO_Sum_May_Sept", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_ONI_Average_Jan_June", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_Physical_Spring_Trans", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Physical_Spring_Trans_Hydro", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Upwelling_Anomaly_Sum_April_May", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Length_Upwelling_season", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_PC1", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_PC2", .default=plot_spacer()) +
    guide_area()+
  plot_layout(guides = 'collect', ncol=3)

if (!is.na(truncate_ts) & truncate_ts >=1998) {
ggsave(california_current_oceanic_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/cali_current_oceanic_", year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#cali current temperature/salinity
california_current_temp_salinity_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_SST_NDBC_buoys_May_Sept", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_Upper_20_m_T_Nov_Mar", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_Upper_20_m_T_May_Sept", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_Deep_temperature_May_Sept", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Deep_salinity_May_Sept", .default=plot_spacer()) +
  guide_area()+
  plot_layout(guides = 'collect', ncol=3)

if (!is.na(truncate_ts) & truncate_ts >=1998) {
  ggsave(california_current_temp_salinity_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/cali_current_temp_sal_", year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#cali current zooplankton
california_current_zoop_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_Copepod_richness_anom_May_Sept", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_N_copepod_biomass_anom_May_Sept", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_S_copepod_biomass_anom_May_Sept", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_Nearshore_Ichthyoplankton_Jan_Mar", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Near_off_community_Ichthyoplankton_Jan_Mar", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_Copepod_Community_Index_May_Sept", .default=plot_spacer()) +
  
    guide_area()+
  plot_layout(guides = 'collect', ncol=3)

if (!is.na(truncate_ts) & truncate_ts >=1998) {
  ggsave(california_current_zoop_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/cali_current_zoop", year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#cali current juvenile salmon
california_current_salmon_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_Chinook_salmon_juvenile_catches_June", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_Coho_salmon_juvenile_catches_June", .default=plot_spacer())+
  guide_area()+
  plot_layout(guides = 'collect', ncol=2)

if (!is.na(truncate_ts) & truncate_ts >=1998) {
  ggsave(california_current_salmon_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/cali_current_salmon", year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}


#Oceanic indices by year
oceanic_yearly_Covariate_plots_stock <- pluck(Covariate_plots_stock, "cov_PDO_yearly_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_ONI_yearly_mean", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_ONI_yearly_anomaly", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_SOI_yearly_mean", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_NPGO_yearly_mean", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_EPNP_yearly_mean", .default=plot_spacer()) +
  guide_area()+
  plot_layout(guides = 'collect', ncol=3)

oceanic_yearly_Covariate_plots_stock 
ggsave(oceanic_yearly_Covariate_plots_stock , file=paste0("Plots/", modelstock, "/oceanic_yearly_", year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))

#oceanic summer plot
oceanic_summer_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PDO_summer_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_ONI_summer_anomaly", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_SOI_summer_mean", .default=plot_spacer()) + 
  # pluck(Covariate_plots_stock, "cov_NPI_summer_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_NPGO_summer_mean", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_EPNP_summer_mean", .default=plot_spacer()) +
  guide_area()+
  plot_layout(guides = 'collect', ncol=3)
ggsave(oceanic_summer_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/oceanic_summer_", year_match[[i]],"_", modelstock,  "_Age_",age_class,".tiff"))

##Zooplankton
zoop_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_zoop_yearly_anomaly", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_zoop_winter_anomaly", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_zoop_summer_anomaly", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_zoop_spring_anomaly", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_zoop_fall_anomaly", .default=plot_spacer()) + 
  plot_layout(guides = 'collect', ncol=3)

if (pluck_exists(Covariate_plots_stock, "cov_zoop_yearly_anomaly") == TRUE | 
    pluck_exists(Covariate_plots_stock, "cov_zoop_winter_anomaly") == TRUE |
    pluck_exists(Covariate_plots_stock, "cov_zoop_summer_anomaly")== TRUE |
    pluck_exists(Covariate_plots_stock, "cov_zoop_spring_anomaly") == TRUE |
    pluck_exists(Covariate_plots_stock, "cov_zoop_fall_anomaly")== TRUE) {
ggsave(zoop_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/zoop_",  year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#Temperature
temp_Covariate_plots_stock <- 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_yearly_mean", .default=plot_spacer()) + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_terminal_summer_mean", .default=plot_spacer()) + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_yearly_mean", .default=plot_spacer()) + 
  #pluck(Covariate_plots_stock, "cov_SST_MEDS_offshore_summer_mean", .default=plot_spacer()) +
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_yearly_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_SST_lighthouse_summer_mean", .default=plot_spacer()) + 
  plot_layout(guides = 'collect', ncol=2)
if (pluck_exists(Covariate_plots_stock, "cov_SST_lighthouse_yearly_mean")==TRUE |
    pluck_exists(Covariate_plots_stock, "cov_SST_lighthouse_summer_mean")==TRUE ){
ggsave(temp_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/temperature_",  year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#Salinity
salinity_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_yearly_mean", .default=plot_spacer()) + 
  pluck(Covariate_plots_stock, "cov_PPT_lighthouse_summer_mean", .default=plot_spacer()) + 
  plot_layout(guides = 'collect', ncol=2)
if (pluck_exists(Covariate_plots_stock, "cov_PPT_lighthouse_yearly_mean")==TRUE |
    pluck_exists(Covariate_plots_stock, "cov_PPT_lighthouse_summer_mean")==TRUE ){
ggsave(salinity_Covariate_plots_stock, file=paste0("Plots/", modelstock, "/salinity_",  year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

#Herring
herring_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_herring_spawn_index_mean", .default=plot_spacer())
if (pluck_exists(Covariate_plots_stock, "cov_herring_spawn_index_mean")==TRUE){
ggsave(herring_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/herring_", year_match[[i]],"_", modelstock,  "_Age_",age_class,".tiff"))
}

#Hydrographic variables
hydro_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_water_flow_yearly_mean", .default=plot_spacer())+
  pluck(Covariate_plots_stock, "cov_water_flow_yearly_max", .default=plot_spacer())+ 
  plot_layout(guides = 'collect', ncol=2)
if (pluck_exists(Covariate_plots_stock, "cov_water_flow_yearly_mean")==TRUE |
    pluck_exists(Covariate_plots_stock, "cov_water_flow_yearly_max")==TRUE ){
ggsave(hydro_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/hydro_",  year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
}

  #Evs
 EV_Covariate_plots_stock <-  pluck(Covariate_plots_stock, "cov_model_EVs", .default=plot_spacer())
  if (pluck_exists(Covariate_plots_stock, "cov_model_EVs")==TRUE) {
    ggsave(EV_Covariate_plots_stock,  file=paste0("Plots/", modelstock, "/evs_",  year_match[[i]],"_", modelstock, "_Age_",age_class, ".tiff"))
    }
    
}
}
