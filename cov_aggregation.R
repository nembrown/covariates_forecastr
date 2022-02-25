



#pdo
pdo_simple

#temperature from MEDS buoys
dfo_meds_buoys_combined



#combining PDO and temperature

fcs_covariates<- left_join(dfo_meds_buoys_combined, pdo_simple) %>% as_tibble()
fcs_covariates