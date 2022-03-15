
library(ggplot2)
library(tidyverse)


# Plotting by stock -------------------------------------------------------
fcs_covariates_combined

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


unique(fcs_covariates_long$Stock_ERA)

ggplot(fcs_covariates_long %>% filter(var_cat== "Zooplankton", var_timing=="Summer", var_location=="terminal"), 
       aes(x=year, y=value, col=Covariate, group=Covariate))+
       geom_point()+geom_line()+facet_wrap(~Stock_ERA, scales="free")



fcs_covariates_long_meta<- merge(fcs_covariates_long, cov_meta, by.x=c("Covariate"), by.y=c("cov_name")) %>% as_tibble
#fcs_covariates_long_meta<- merge(fcs_covariates_long_meta, stations_meta %>% dplyr::select(Region, Stock_ERA))  %>% as_tibble
fcs_covariates_long_meta$var_cat <- factor(fcs_covariates_long_meta$var_cat, levels = c("Zooplankton", "Temperature", "Salinity", "PDO", "ONI", "SOI", "NPI", "EPNP", "NPGO", "ALPI"))
fcs_covariates_long_meta

#BC
ggplot(fcs_covariates_long_meta %>% filter(Region == "BC"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("BC")
ggsave("Plots/BC_coverage.tiff")

#AK
ggplot(fcs_covariates_long_meta %>% filter(Region == "AK"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("AK")
ggsave("Plots/AK_coverage.tiff")

#WA
ggplot(fcs_covariates_long_meta %>% filter(Region == "WA"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free") + ggtitle("WA")
ggsave("Plots/WA_coverage.tiff")

#OR
ggplot(fcs_covariates_long_meta %>% filter(Region == "OR"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free")+ ggtitle("OR")
ggsave("Plots/OR_coverage.tiff")

#CR
ggplot(fcs_covariates_long_meta %>% filter(Region == "CR"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free")+ ggtitle("CR")
ggsave("Plots/CR_coverage.tiff")

#PS
ggplot(fcs_covariates_long_meta %>% filter(Region == "PS"), aes(x=year, y=var_cat, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none")+facet_wrap(~Stock_ERA, scales="free")+ ggtitle("PS")
ggsave("Plots/PS_coverage.tiff")

##COW,BQR
ggplot(fcs_covariates_long_meta %>% filter(Stock_ERA == "COW"), aes(x=year, y=Covariate, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none") + ggtitle("COW") + theme_bw()
ggsave(file="Plots/LGS_NAT/COW_coverage.tiff")

ggplot(fcs_covariates_long_meta %>% filter(Stock_ERA == "BQR"), aes(x=year, y=Covariate, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none") + ggtitle("BQR") + theme_bw()
ggsave("Plots/MGS/BQR_coverage.tiff")

ggplot(fcs_covariates_long_meta %>% filter(Stock_ERA == "ATN/ATS"), aes(x=year, y=Covariate, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none") + ggtitle("ATN") + theme_bw()
ggsave("Plots/ATN/ATN_coverage.tiff")

ggplot(fcs_covariates_long_meta %>% filter(Stock_ERA == "RBT"), aes(x=year, y=Covariate, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none") + ggtitle("RBT") + theme_bw()
ggsave("Plots/WCVI_HATCH/RBT_coverage.tiff")
ggsave("Plots/WCVI_NAT/RBT_coverage.tiff")


ggplot(fcs_covariates_long_meta %>% filter(Stock_ERA == "SRH"), aes(x=year, y=Covariate, size=value, col=var_cat))+ geom_point() +
  scale_size(range = c(1,1)) + theme(legend.position = "none") + ggtitle("SRH") + theme_bw()
ggsave("Plots/ALSEA/SHR_coverage.tiff")

