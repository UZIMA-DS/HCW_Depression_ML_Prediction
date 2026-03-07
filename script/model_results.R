library(dplyr)
library(tidyr)
library(purrr)
library(pROC)
library(tidyverse)
library(glmnet)
source("model_utils.R")

#================================
#         Data Prep
#================================

setwd("~/Kevin")
imp_dataset = read.csv("imp.csv")
noimp_dataset = read.csv("noimp.csv")

all_vars <- c("minutessedentary", "minuteslightlyactive",
              "minutesfairlyactive", "minutesveryactive",
              "restingheartrate", "steps",
              "sleep_total", "sleep_eff",
              "Sleep_time", "mood")

continuous_vars <- c(
  "mood", "steps", "sleep_eff","Sleep_time","minutessedentary",
  "sleep_total", "restingheartrate", "minutesveryactive"
)

covariates_full <- c(
  as.vector(rbind(paste0(continuous_vars, "_mean")))
)

covariates_full_sd <- c(
  as.vector(rbind(paste0(continuous_vars, "_mean"),
                  paste0(continuous_vars, "_sd")))
)

covariates_mood <- c("mood_mean")

covariates_mood_sd <- c("mood_mean", "mood_sd")

imp_s1 <- stage1(all_vars, imp_dataset)
noimp_s1_full <- stage1(all_vars, noimp_dataset)
noimp_s1_mood <- stage1(c("mood"), noimp_dataset)

imp_s2_phq9 <- stage2(all_vars, imp_dataset, 11)
noimp_s2_full_phq9 <- stage2(all_vars, noimp_dataset, 11)
noimp_s2_mood_phq9 <- stage2(c("mood"), noimp_dataset, 11)

imp_s2_si <- stage2(all_vars, imp_dataset, 30)
noimp_s2_full_si <- stage2(all_vars, noimp_dataset, 30)
noimp_s2_mood_si <- stage2(c("mood"), noimp_dataset, 30)

#================================
#     Logistic Regression
#================================

lr_phq9_mood_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


lr_phq9_full_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = covariates_full,
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  #model_fun = enr_fun,
  #pred_fun = enr_pred)
  metrics$Day <- d
  metrics
})

lr_si_mood_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "si",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


lr_si_full_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "si",
                     covariates = covariates_full,
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  #model_fun = enr_fun,
  #pred_fun = enr_pred)
  metrics$Day <- d
  metrics
})


lr_phq9_mood_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_phq9[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


lr_phq9_full_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_phq9[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = covariates_full,
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  #model_fun = enr_fun,
  #pred_fun = enr_pred)
  metrics$Day <- d
  metrics
})

lr_si_mood_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_si[[d]]
  metrics <- get_roc(df, "si",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


lr_si_full_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_si[[d]]
  metrics <- get_roc(df, "si",
                     covariates = covariates_full,
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  #model_fun = enr_fun,
  #pred_fun = enr_pred)
  metrics$Day <- d
  metrics
})

#================================
#        Random Forest
#================================

rf_phq9_mood_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean", "mood_sd"),
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})


rf_phq9_full_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = covariates_full,
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})

rf_si_mood_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "si",
                     covariates = c("mood_mean", "mood_sd"),
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})


rf_si_full_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "si",
                     covariates = covariates_full,
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})

rf_phq9_mood_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_phq9[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})


rf_phq9_full_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_phq9[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = covariates_full,
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})

rf_si_mood_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_si[[d]]
  metrics <- get_roc(df, "si",
                     covariates = c("mood_mean"),
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})


rf_si_full_s2 <- map_dfr(1:90, function(d) {
  df <- imp_s2_si[[d]]
  metrics <- get_roc(df, "si",
                     covariates = covariates_full,
                     model_fun = rf_fun,
                     pred_fun = rf_pred)
  metrics$Day <- d
  metrics
})

#================================
#     Sensitivity Analysis
#================================


lr_si_mood_s1_raw <- map_dfr(1:90, function(d) {
  df <- noimp_s1_mood[[d]]
  metrics <- get_roc(df, "si",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})

lr_phq9_mood_s2_raw <- map_dfr(1:90, function(d) {
  df <- noimp_s2_mood_phq9[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


#================================
#       Visualization
#================================

lr_phq9_mood_s1$model <- "Mood"
lr_phq9_full_s1$model <- "Mood and Fitbit"

s1_phq9 <- bind_rows(lr_phq9_mood_s1, lr_phq9_full_s1)
s1_phq9 <- s1_phq9 |> dplyr::filter(!is.na(ROC))


ggplot(s1_phq9, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )


lr_si_mood_s1$model <- "Mood"
lr_si_full_s1$model <- "Mood and Fitbit"

s1_si <- bind_rows(lr_si_mood_s1, lr_si_full_s1)
s1_si <- s1_si |> dplyr::filter(!is.na(ROC))


ggplot(s1_si, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )




lr_phq9_mood_s2$model <- "Mood"
lr_phq9_full_s2$model <- "Mood and Fitbit"

s2_phq9 <- bind_rows(lr_phq9_mood_s2, lr_phq9_full_s2)
s2_phq9 <- s2_phq9 |> dplyr::filter(!is.na(ROC))

ggplot(s2_phq9, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 79),
    breaks = seq(0, 79, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )




lr_si_mood_s2$model <- "Mood"
lr_si_full_s2$model <- "Mood and Fitbit"

s2_si <- bind_rows(lr_si_mood_s2, lr_si_full_s2)
s2_si <- s2_si |> dplyr::filter(!is.na(ROC))

ggplot(s2_si, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )

lr_phq9_mood_s1_raw <- map_dfr(1:90, function(d) {
  df <- noimp_s1_mood[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})

lr_phq9_mood_s1$imp <- "Mood (Imputed)"
lr_phq9_mood_s1_raw$imp <- "Mood (Non imputed)"

s1_phq9_imp <- bind_rows(lr_phq9_mood_s1, lr_phq9_mood_s1_raw)
s1_phq9_imp <- s1_phq9_imp |> dplyr::filter(!is.na(ROC))

ggplot(s1_phq9_imp, aes(x = Day, y = ROC, color = imp, fill = imp, linetype = imp)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood (Imputed)" = "#E41A1C", 
    "Mood (Non imputed)"  = "#4DAF4A" 
  )) +
  scale_fill_manual(values = c(
    "Mood (Imputed)" = "#E41A1C",   
    "Mood (Non imputed)"  = "#4DAF4A"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )




lr_si_mood_s1$imp <- "Mood (Imputed)"
lr_si_mood_s1_raw$imp <- "Mood (Non imputed)"

s1_si_imp <- bind_rows(lr_si_mood_s1, lr_si_mood_s1_raw)
s1_si_imp <- s1_si_imp |> dplyr::filter(!is.na(ROC))

ggplot(s1_si_imp, aes(x = Day, y = ROC, color = imp, fill = imp, linetype = imp)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood (Imputed)" = "#E41A1C", 
    "Mood (Non imputed)"  = "#4DAF4A" 
  )) +
  scale_fill_manual(values = c(
    "Mood (Imputed)" = "#E41A1C",   
    "Mood (Non imputed)"  = "#4DAF4A"
  ))+
  scale_y_continuous(limits = c(0.40, 0.92), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )

lr_phq9_mood_s2$imp <- "Mood (Imputed)"
lr_phq9_mood_s2_raw$imp <- "Mood (Non imputed)"

s2_phq9_imp <- bind_rows(lr_phq9_mood_s2, lr_phq9_mood_s2_raw)
s2_phq9_imp <- s2_phq9_imp |> dplyr::filter(!is.na(ROC))


ggplot(s2_phq9_imp, aes(x = Day, y = ROC, color = imp, fill = imp, linetype = imp)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood (Imputed)" = "#E41A1C", 
    "Mood (Non imputed)"  = "#4DAF4A" 
  )) +
  scale_fill_manual(values = c(
    "Mood (Imputed)" = "#E41A1C",   
    "Mood (Non imputed)"  = "#4DAF4A"
  ))+
  scale_y_continuous(limits = c(0.30, 0.92), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 79),
    breaks = seq(0, 79, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )


lr_si_mood_s2$imp <- "Mood (Imputed)"
lr_si_mood_s2_raw$imp <- "Mood (Non imputed)"

s2_si_imp <- bind_rows(lr_si_mood_s2, lr_si_mood_s2_raw)
s2_si_imp <- s2_si_imp |> dplyr::filter(!is.na(ROC))


ggplot(s2_si_imp, aes(x = Day, y = ROC, color = imp, fill = imp, linetype = imp)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood (Imputed)" = "#E41A1C", 
    "Mood (Non imputed)"  = "#4DAF4A" 
  )) +
  scale_fill_manual(values = c(
    "Mood (Imputed)" = "#E41A1C",   
    "Mood (Non imputed)"  = "#4DAF4A"
  ))+
  scale_y_continuous(limits = c(0.20, 1), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )


lr_phq9_mood_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = c("mood_mean"),
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  metrics$Day <- d
  metrics
})


lr_phq9_full_s1 <- map_dfr(1:90, function(d) {
  df <- imp_s1[[d]]
  metrics <- get_roc(df, "phq9_cat",
                     covariates = covariates_full,
                     model_fun = glm_fun,
                     pred_fun = glm_pred)
  #model_fun = enr_fun,
  #pred_fun = enr_pred)
  metrics$Day <- d
  metrics
})


rf_phq9_mood_s1$model <- "Mood"
rf_phq9_full_s1$model <- "Mood and Fitbit"

s1_phq9_rf <- bind_rows(rf_phq9_full_s1, rf_phq9_mood_s1)
s1_phq9_rf <- s1_phq9_rf |> dplyr::filter(!is.na(ROC))


ggplot(s1_phq9_rf, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.3, 0.9), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )

rf_si_mood_s1$model <- "Mood"
rf_si_full_s1$model <- "Mood and Fitbit"

s1_si_rf <- bind_rows(rf_si_mood_s1, rf_si_full_s1)
s1_si_rf <- s1_si_rf |> dplyr::filter(!is.na(ROC))


ggplot(s1_si_rf, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.3, 0.9), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window length " ~ Delta), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )

rf_phq9_mood_s2$model <- "Mood"
rf_phq9_full_s2$model <- "Mood and Fitbit"

s2_phq9_rf <- bind_rows(rf_phq9_mood_s2, rf_phq9_full_s2)
s2_phq9_rf <- s2_phq9_rf |> dplyr::filter(!is.na(ROC))

ggplot(s2_phq9_rf, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.4, 0.8), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 79),
    breaks = seq(0, 79, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )




rf_si_mood_s2$model <- "Mood"
rf_si_full_s2$model <- "Mood and Fitbit"

s2_si_rf <- bind_rows(rf_si_mood_s2, rf_si_full_s2)
s2_si_rf <- s2_si_rf |> dplyr::filter(!is.na(ROC))

ggplot(s2_si_rf, aes(x = Day, y = ROC, color = model, fill = model, linetype = model)) +
  geom_line(size = 1) +
  geom_point(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
  scale_color_manual(values = c(
    "Mood" = "#E41A1C", 
    "Mood and Fitbit"  = "#377EB8" 
  )) +
  scale_fill_manual(values = c(
    "Mood" = "#E41A1C",   
    "Mood and Fitbit"  = "#377EB8"
  ))+
  scale_y_continuous(limits = c(0.40, 0.85), expand = c(0, 0)) +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    expand = c(0, 0)
  )+
  labs(x = expression("Window ID"), y = "AUC") +
  theme_bw(base_size = 14) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid = element_blank()
  )

