# Machine Learning Pipeline for Predicting Depression and Suicidal Ideation

## Overview

This repository contains code for preparing longitudinal data, building machine learning models, and evaluating their performance in predicting:

- **Depression (PHQ-9)**
- **Suicidal ideation (SI)**

The workflow is divided into two main components:
- `/script/data_prepare.R` → Helper functions (feature engineering + modeling utilities) 
- `/R/model_utils.R` → Helper functions (feature engineering + modeling utilities)  
- `/script/_results.R` → Main analysis pipeline and visualization  
# HCW_Depression_ML_Prediction
This prct description Here
# 1. Data Preprocessing
This /script/data_prepare.R file  contains codes for preprocessing, imputing, and visualizing intensive longitudinal data from the UZIMA-DS study, which collected daily activity, sleep, and mood data from healthcare workers using Fitbit devices and surveys. The workflow cleans and merges Fitbit daily and sleep logs with mood and baseline survey data, handles missing values using sequential multiple imputation (mice), and generates longitudinal datasets  Key outputs include imputed and non-imputed datasets, as well as visualizations of data completeness and the time between baseline and follow-up surveys. Required R packages include dplyr, tidyr, purrr, pROC, tidyverse, mice, lubridate, and ggplot2.

# 2. Modelling
The workflow begins by transforming daily participant-level data into summary features using two approaches:

- **Stage 1 (cumulative windows):** aggregates data from the start of observation up to each day  
- **Stage 2 (rolling windows):** aggregates data within fixed time windows (e.g., 11 days for depression, 30 days for SI)  

For each time window, the script computes statistical features (mean and standard deviation) for variables such as mood, sleep, activity levels, heart rate, and steps.

These features are then used to train and evaluate machine learning models, specifically:

- Logistic Regression  
- Random Forest  

Two modeling strategies are compared:

- Mood-only models  
- Combined models (Mood + Fitbit data)  

Model performance is evaluated using cross-validated **ROC AUC**, and results are tracked across different time windows to identify optimal prediction periods.

Additionally, the script performs a **sensitivity analysis** comparing results from:

- Imputed datasets  
- Non-imputed datasets  

Finally, the results are visualized using line plots with confidence intervals, illustrating how predictive performance changes over time and across different modeling approaches.
## 2. Analysis Pipeline (Integrated Workflow)

This section describes the full workflow implemented in the analysis script.


### 2.1 Data Loading

Two datasets are used:

- `imp.csv` → imputed dataset  
- `noimp.csv` → non-imputed dataset  

These datasets contain:

- Daily observations per participant  
- Behavioral and physiological variables  
- Binary outcomes:
  - `phq9_cat`
  - `si`


### 2.2 Feature Selection

The following predictors are used:

**Activity:**
- sedentary minutes  
- light activity  
- moderate activity  
- vigorous activity  

**Physiological:**
- resting heart rate  
- steps  

**Sleep:**
- total sleep  
- sleep efficiency  
- sleep duration  

**Psychological:**
- mood  

**Derived features:**
- Mean (`_mean`)  
- Standard deviation (`_sd`)  


### 2.3 Feature Engineering

Feature datasets are generated as follows:

**Stage 1 (Cumulative):**
- Full dataset (mood + Fitbit)  
- Mood-only dataset  

**Stage 2 (Rolling Windows):**
- PHQ-9 prediction → **11-day window**  
- SI prediction → **30-day window**  


### 2.4 Model Training

Models are trained across **time windows (1–90 days)**.

**Logistic Regression Models:**
- Mood-only  
- Mood + Fitbit  

**Random Forest Models:**
- Mood-only  
- Mood + Fitbit  

Each model is trained and evaluated independently for each window.


### 2.5 Model Evaluation

For each model and window:

- Cross-validated **ROC AUC** is computed  
- Confidence intervals are generated  

This allows identification of:

- Optimal prediction window length  
- Performance differences between feature sets  


### 2.6 Sensitivity Analysis

The pipeline compares:

- Imputed dataset  
- Non-imputed dataset  

This evaluates robustness of:

- Mood-only models  
- Model performance stability  


### 2.7 Visualization

The pipeline generates plots showing:

- AUC vs time window  

**Model comparisons:**
- Mood vs Mood + Fitbit  
- Logistic vs Random Forest  
- Imputed vs Non-imputed  

**Plot features:**
- Line plots  
- Confidence intervals (ribbons)  
- Multiple model comparisons  


## 3. Workflow Summary

1. Load datasets (`imp.csv`, `noimp.csv`)  
2. Generate features using:
   - Stage 1 (cumulative)  
   - Stage 2 (rolling windows)  
3. Train models:
   - Logistic regression  
   - Random forest  
4. Evaluate performance:
   - ROC AUC  
   - Confidence intervals  
5. Perform sensitivity analysis  
6. Visualize results  

## 4. Output

The pipeline produces:

### Data Outputs
- AUC values per model and window  
- Confidence intervals  
- Window index (Day)  

### Visual Outputs
- Performance curves over time  
- Optimal window identification  
- Model comparison plots  


## 5. Key Insights Enabled

This pipeline allows you to:

- Identify optimal time windows for prediction  
- Compare mood-only vs multimodal models  
- Evaluate impact of wearable data (Fitbit)  
- Assess robustness to missing data (imputation)  
- Understand temporal dynamics of mental health prediction  

## 6. Notes

- Dataset must be in **long format**  
- Outcomes must be **binary (0/1)**  
- Missing values are handled during preprocessing  
- Ensure correct working directory is set before running scripts  
