# covariates_forecastr

Repository of code for gathering environmental and biological covariates to feed into ForecastR to predict salmon escapement. Some of the code used here is inspired by the [PACea](https://github.com/pbs-assess/PACea) initiative. Visit [ForecastR](https://github.com/SalmonForecastR/ForecastR-Releases) for more information on using covariates in forecasting via Complex Sibling Regression. 

#### To gather and collate covariate data, follow these scripts in the following order: 

1. [cov_fetch.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_fetch.R) - Use this code to grab covariate data from online sources. so far we have salinity, temperature, zooplankton, herring spawn, river flow, and various atmospheric/oceanic indices. Where possible, I have linked to the online source so that we have the most up to date data each time the query is run. Make sure to check sources for updated data and alter the code as necessary. 

2. [cov_match_stock.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_match_stock.R) - Use this code to match up spatially explicit data (e.g. from buoys or sampling stations) to a given ERA stock location and summarize the data at the correct scale (i.e. one measurement per year). 

3. [cov_aggregation.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_aggregation.R) - Use this code to merge all covariates by ERA Stock and year into a long-format file. This file (fcs_covariates.csv) can be filtered by stock for use in forecastR. 


#### Next, map covariate station locations and plot data coverage by covariates:

[cov_map.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_map.R) - use this code to map the coordinates of stations and ERA stocks.

[cov_plot.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_plot.R) - use this code to plot the yearly data coverage of given stock or a series of stocks in a given region. 

#### Next, match and plot average escapement of a model stock to covariates from an indicator stock:

[cov_correlate.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_correlate.R) - Run the function and then specify the inputs. The script will then spit out plots in corresponding folders. These plots are intended for visualization and to help select a suite of 3 correlated covariates to use in ForecastR. 

These plots are of two types: 

1. Correlation plots - calculate a correlation coefficient (between 0 and 1) for each covariate against average escapement (or terminal run). These correlation plots can be either the full suite of covariates or a subset that have a significant (p<0.05) correlation to average escapement. 

2. Regression plots - Using geom_smooth and grouping covariates by category and plot average escapement on the y axis and the covariate on the x axis to investigate the relationship. 

#### After selecting covariates for forecasting, you can create and save a ForecastR-shiny-ready file:

[cov_select.R](https://github.com/nembrown/covariates_forecastr/blob/main/cov_select.R). Select and rename covariates as desired. Remember to include if the covariate is matched to run or brood year, and any lead or lag of years. 
