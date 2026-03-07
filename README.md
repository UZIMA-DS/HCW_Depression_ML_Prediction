# HCW_Depression_ML_Prediction
This prct description Here
# 1. Data Preprocessing
This /script/data_prepare.R file  contains codes for preprocessing, imputing, and visualizing intensive longitudinal data from the UZIMA-DS study, which collected daily activity, sleep, and mood data from healthcare workers using Fitbit devices and surveys. The workflow cleans and merges Fitbit daily and sleep logs with mood and baseline survey data, handles missing values using sequential multiple imputation (mice), and generates longitudinal datasets  Key outputs include imputed and non-imputed datasets, as well as visualizations of data completeness and the time between baseline and follow-up surveys. Required R packages include dplyr, tidyr, purrr, pROC, tidyverse, mice, lubridate, and ggplot2.
