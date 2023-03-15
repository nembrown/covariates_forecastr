
# Load libraries ----------------------------------------------------------

library(tidyverse)

# Read in cov_data for covariate and Av escapement  ------------------------------------------------------------


select_covs(cov_data_file ="fcs_covariates_interpolated.csv",
            escapement_data_file = "Inputs/MOC for Norah use ELK cov/Elknewbooks2023.csv",
            output_file_name = "Outputs/MOC/Elknewbooks2023_cov.csv" ,
            modelstock = "MOC",
            stock= "ELK",
            stock_name="Elk",
            escapement_type="Escapement",
            age_specific=TRUE,
            truncate_ts = 1998,
            cov1 = "cov_ONI_yearly_mean",
            cov1_year_match = "Brood_Year_Lag1",
            cov2 = "cov_ONI_yearly_anomaly",
            cov2_year_match = "Brood_Year_Lag1",
            cov3 = "cov_NPGO_yearly_mean",
            cov3_year_match = "Brood_Year_Lag2"
)
            


#Laura don't touch
select_covs<-function(cov_data_file,
                         escapement_data_file,
                         output_file_name,
                         modelstock = NA_character_, 
                         stock= NA_character_, 
                         stock_name= NA_character_,
                         escapement_type=NA_character_,
                         truncate_ts=NA_integer_,
                         age_specific=FALSE,
                         cov1 = NA_character_, 
                         cov1_year_match = NA_character_, 
                         cov2 = NA_character_, 
                         cov2_year_match = NA_character_, 
                         cov3 = NA_character_, 
                         cov3_year_match = NA_character_){
  cly<- as.integer(format(Sys.Date(), "%Y"))
  cov_data<-read.csv(cov_data_file) %>% as_tibble()
  escapement_data_original_format<-read.csv(escapement_data_file) %>% as_tibble() %>% select(-contains("Cov"))

  if (max(escapement_data_original_format$Run_Year)!= cly) {
    escapement_data_original_format<- escapement_data_original_format %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-3, Age_Class=3, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-4, Age_Class=4, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-5, Age_Class=5, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-6, Age_Class=6, Average_Escapement=NA)
  }
  
  escapement_data<-read.csv(escapement_data_file) %>% as_tibble()
  
  if (max(escapement_data$Run_Year)!= cly) {
    escapement_data<- escapement_data %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-3, Age_Class=3, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-4, Age_Class=4, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-5, Age_Class=5, Average_Escapement=NA) %>% 
      add_row(Stock_Name = "", Stock_Species = "", Stock_Abundance = "", Forecasting_Year = NA, Run_Year=cly, Brood_Year=cly-6, Age_Class=6, Average_Escapement=NA)
  }
  
  
  rel_file_dir <- normalizePath(paste0("Outputs/", modelstock), mustWork = FALSE)
  
  if(dir.exists(rel_file_dir) == FALSE) {
    cat(paste0("Outputs directory does not exist, creating:\n", rel_file_dir))
    dir.create(rel_file_dir)
  }
  
  # Restrict to only stock
  cov_data_stock<-cov_data %>% filter(Stock_ERA == stock & Stock_model== modelstock) %>% select(-Stock_model)
  cov_data_stock_select<- cov_data_stock %>% select(year, all_of(c(cov1, cov2, cov3)))

    if(escapement_type=="Terminal Run"){
    escapement_data<-escapement_data %>% rename(Escapement_type=Average_Terminal_Run)
  } else if (escapement_type=="Escapement"){
    escapement_data<-escapement_data %>% rename(Escapement_type=Average_Escapement)
  }
  
  if(age_specific==FALSE){
    
    escapement_data<- escapement_data %>%  select(Run_Year, Escapement_type) %>% 
      mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
      mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
      mutate(Brood_Year = Run_Year - 4) %>% 
      mutate(Brood_Year_Lag1 = Run_Year - 3) %>% 
      mutate(Brood_Year_Lag2 = Run_Year - 2) 
    
    cov_data_stock_roll<- zoo::rollmean(cov_data_stock %>% select(-Stock_ERA), k=3) %>% as_tibble()
    cov_data_stock_roll_select<- cov_data_stock_roll %>% select(year, all_of(c(cov1, cov2, cov3)))
    
  }else {
    escapement_data<-escapement_data %>%  select(Brood_Year, Run_Year, Escapement_type, Age_Class) %>% 
    mutate(Run_Year_Lead1 = Run_Year - 1) %>% 
    mutate(Run_Year_Lead2 = Run_Year - 2) %>% 
    mutate(Brood_Year_Lag1 = Brood_Year + 1) %>% 
    mutate(Brood_Year_Lag2 = Brood_Year + 2) 
  }
  
  # Matching escapement to covariates ---------------------------------------
  
#cov1
  if(age_specific==FALSE & cov1_year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
      escapement_data_cov1_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov1_year_match))), cov_data_stock_roll_select %>% select(year, all_of(c(cov1))), by.x=cov1_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov1_year_match ), starts_with("cov"))
    } else if (age_specific==TRUE) {
      escapement_data_cov1_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, Brood_Year, Age_Class, all_of(c(cov1_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov1))), by.x=cov1_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov1_year_match ), starts_with("cov"))
  } else {
    escapement_data_cov1_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov1_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov1))), by.x=cov1_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov1_year_match ), starts_with("cov"))
  } 
 
#cov 2
  if(age_specific==FALSE & cov2_year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
    escapement_data_cov2_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov2_year_match))), cov_data_stock_roll_select %>% select(year, all_of(c(cov2))), by.x=cov2_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov2_year_match ), starts_with("cov"))
  } else if (age_specific==TRUE) {
    escapement_data_cov2_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, Brood_Year, Age_Class, all_of(c(cov2_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov2))), by.x=cov2_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov2_year_match ), starts_with("cov"))
  } else {
    escapement_data_cov2_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov2_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov2))), by.x=cov2_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov2_year_match ), starts_with("cov"))
  } 
  
#cov 3
  if(age_specific==FALSE & cov3_year_match %in%  c("Brood_Year", "Brood_Year_Lag1", "Brood_Year_Lag2")){
    escapement_data_cov3_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov3_year_match))), cov_data_stock_roll_select %>% select(year, all_of(c(cov3))), by.x=cov3_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov3_year_match ), starts_with("cov"))
  } else if (age_specific==TRUE) {
    escapement_data_cov3_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, Brood_Year, Age_Class, all_of(c(cov3_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov3))), by.x=cov3_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov3_year_match ), starts_with("cov"))
  } else {
    escapement_data_cov3_wide<-merge(escapement_data %>% select(Escapement_type, Run_Year, all_of(c(cov3_year_match))), cov_data_stock_select %>% select(year, all_of(c(cov3))), by.x=cov3_year_match, by.y=c("year"), all.x=TRUE) %>% as_tibble %>% rename_with(~paste0(., "_",cov3_year_match ), starts_with("cov"))
  } 
    
  
  if(escapement_type=="Terminal Run"){
    escapement_data_cov1_wide<-escapement_data_cov1_wide %>% rename(Average_Terminal_Run=Escapement_type)
    escapement_data_cov2_wide<-escapement_data_cov2_wide %>% rename(Average_Terminal_Run=Escapement_type)
    escapement_data_cov3_wide<-escapement_data_cov3_wide %>% rename(Average_Terminal_Run=Escapement_type)
    
      }else if (escapement_type=="Escapement"){
    escapement_data_cov1_wide<-escapement_data_cov1_wide %>% rename(Average_Escapement=Escapement_type)
    escapement_data_cov2_wide<-escapement_data_cov2_wide %>% rename(Average_Escapement=Escapement_type)
    escapement_data_cov3_wide<-escapement_data_cov3_wide %>% rename(Average_Escapement=Escapement_type)
      }

    escapement_data_original_format_covs<- escapement_data_original_format %>% 
                                           left_join(escapement_data_cov1_wide %>% select(-all_of(c(cov1_year_match)))) %>% 
                                           left_join(escapement_data_cov2_wide %>% select(-all_of(c(cov2_year_match)))) %>% 
                                           left_join(escapement_data_cov3_wide %>% select(-all_of(c(cov3_year_match)))) %>% 
                                           rename_with(str_to_title, starts_with("cov"))
  
    if (!is.na(truncate_ts)){
      escapement_data_original_format_covs<-  escapement_data_original_format_covs %>% filter(Brood_Year >= truncate_ts) %>% 
        mutate(Stock_Name = case_when(
          Brood_Year == truncate_ts & Age_Class == 3 ~ stock_name,
          TRUE ~ Stock_Name), 
          Stock_Species = case_when(
            Brood_Year == truncate_ts & Age_Class == 3 ~ "Chinook",
            TRUE ~ Stock_Species), 
          Stock_Abundance = case_when(
            Brood_Year == truncate_ts & Age_Class == 3 ~ escapement_type, 
            TRUE ~ Stock_Abundance), 
          Forecasting_Year = case_when(
            Brood_Year == truncate_ts & Age_Class == 3 ~ cly, 
            TRUE ~ NA_integer_)) 
    }
    
    
write.csv(escapement_data_original_format_covs, file=output_file_name, row.names = FALSE)
}
