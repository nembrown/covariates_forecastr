# covariates_forecastr
Repository of code for gathering environmental and biological covariates to feed into ForecastR to predict salmon escapement.

Use the scripts in this order:
1. [cov_fetch.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_fetch.R) - Use this code to grab covariate data (so far temperature, salinity, and zooplankton biomass) from online sources. Where possible, I have linked to the online source so that we have the most up to date data each time the query is run. 

2. [cov_match_stock.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_match_stock.R) - Use this code to match up spatially explicit data (e.g. from buoys or sampling stations) to a given ERA stock location and summarize the data at the correct scale (i.e. one measurement per year). 

3. [cov_aggregation.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_aggregation.R) - Use this code to merge all covariates by ERA Stock and year into a long-format file. This file (fcs_covariates.csv) can be filtered by stock for use in forecastR. Match the covariate to either brood year or run year with a 0, 1, or 2 year time lag. This is demonstrated with the PDO data in [shiny_formatting.R](https://github.com/nembrown/covariates_forecastr/blob/main/shiny_formatting.R). 

4. [cov_map.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_map.R) - use this code to map the coordinates of stations and ERA stocks. 


Some of the code used here is borrowed from the [PACea](https://github.com/pbs-assess/PACea) initiative. 