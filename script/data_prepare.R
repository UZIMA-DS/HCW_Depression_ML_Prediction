library(dplyr)
library(tidyr)
library(purrr)
library(pROC)
library(tidyverse)

setwd("//uzima/CleanedData")
daily =  read.csv("./FitbitData/FitbitDailyData_09292025.csv")
sleep =  read.csv("./FitbitData/FitbitSleepLogs_09292025.csv")
mood =  read.csv("./MoodData/MoodData_20251017.csv")
survey = read.csv("./SurveyData/Q1_Var_20251017.csv")
baseline = read.csv("./SurveyData/Baseline_Cleaned_09292025.csv")

#================================
#        Preprocessing
#================================

fitbit_daily = daily %>% 
  select(participantidentifier, minutessedentary,
         minuteslightlyactive, minutesfairlyactive, 
         minutesveryactive, restingheartrate,
         steps, date)%>%
  mutate(
    steps = ifelse(steps == 0, NA, steps),
    across(
      c("minutessedentary", "minuteslightlyactive", "minutesfairlyactive", "minutesveryactive"),
      ~ ifelse(minutessedentary == 1440 |
                 (minutesfairlyactive + minuteslightlyactive + 
                    minutesveryactive + minutessedentary != 1440), NA, .)
    )
  ) %>%
  distinct() %>%
  filter(
    !(is.na(steps) &
        is.na(minutesveryactive) &
        is.na(minuteslightlyactive) &
        is.na(minutesfairlyactive) &
        is.na(minutessedentary) &
        is.na(restingheartrate))
  ) %>%
  mutate(date = as.Date(ymd_hms(date)))

sleep_daily <- sleep %>%
  mutate(
    start_datetime = ymd_hms(startdate),
    date = as.Date(start_datetime)
  ) %>%
  
  group_by(participantidentifier, date) %>%
  
  summarise(
    sleep_total = sum(minutesasleep, na.rm = TRUE),
    sleep_eff = sum(minutesasleep, na.rm = TRUE)/sum(timeinbed, na.rm = TRUE),
    
    latest_sleep = max(start_datetime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    latest_hour = hour(latest_sleep),
    latest_minute = minute(latest_sleep),
    
    minutes_since_midnight = latest_hour * 60 + latest_minute,
    
    Sleep_time = case_when(
      latest_hour >= 0 & latest_hour < 12 ~ -((latest_hour * 60) + latest_minute),
      latest_hour >= 12 ~ 1440 - ((latest_hour * 60) + latest_minute)
    )
  ) %>%
  select(participantidentifier, date, sleep_total, sleep_eff, Sleep_time) %>%
  mutate(date = as.Date(date))

mood_daily <- mood %>%
  mutate(answers = as.numeric(answers)) %>%
  filter(!is.na(answers)) %>%
  group_by(participantidentifier, date) %>%
  summarize(mood = mean(answers), .groups = "drop") %>%
  mutate(date = as.Date(date))

full_daily <- fitbit_daily %>%
  full_join(sleep_daily, by = c("participantidentifier", "date")) %>%
  full_join(mood_daily,  by = c("participantidentifier", "date")) %>%
  semi_join(survey, by = "participantidentifier")

full_daily_clean <- full_daily %>%
  left_join(survey %>% select(participantidentifier, enroll_date),
            by = "participantidentifier") %>%
  mutate(
    date = as.Date(date),                   # <- drop time
    enroll_date = as.Date(enroll_date),     # <- ensure same class
    day = as.numeric(difftime(date, enroll_date, units = "days"))
  ) %>%
  filter(day >= 0 & day <= 285)%>%
  group_by(participantidentifier) %>%
  complete(day = 0:285, fill = list(
    minutessedentary = NA_real_,
    minuteslightlyactive = NA_real_,
    minutesfairlyactive = NA_real_,
    minutesveryactive = NA_real_,
    restingheartrate = NA_real_,
    steps = NA_real_,
    sleep_total = NA_real_,
    sleep_eff = NA_real_,
    Sleep_time = NA_real_,
    mood = NA_real_
  )) %>%
  ungroup() %>%
  select(-date, -enroll_date)

baseline = baseline %>%
  select(participantidentifier,efe_score, neuroticsm_score, phq_score,Sex,Relation) %>%
  semi_join(survey, by = "participantidentifier")

survey = survey %>%
  mutate(si = as.numeric(si>0)) %>%
  mutate(daydiff = as.Date(survey_date) - as.Date(enroll_date))

#================================
#          Imputation
#================================

library(mice)
vars <- c("minutessedentary", "minuteslightlyactive",
          "minutesfairlyactive", "minutesveryactive",
          "restingheartrate", "steps",
          "sleep_total", "sleep_eff",
          "Sleep_time", "mood")
full_daily_imp <- full_daily_clean

all_imps <- vector("list", length = 5)

for (r in 1:10) {
  
  cat("Replication:", r, "\n")
  full_daily_temp <- full_daily_imp
  
  for (d in 0:285) {
    cat("  Imputing day:", d, "\n")
    dat_day <- full_daily_temp %>%
      filter(day == d) %>%
      left_join(baseline, by = "participantidentifier")
    if (d > 0) {
      for (lag in 1:min(2, d)) {
        lagged <- full_daily_temp %>%
          filter(day == d + 1 - lag) %>%
          select(participantidentifier, all_of(vars)) %>%
          rename_with(~ paste0("lag", lag, "_", .x), -participantidentifier)
        dat_day <- left_join(dat_day, lagged, by = "participantidentifier")
      }
    }
    imp_input <- dat_day %>%
      select(all_of(vars),
             efe_score, neuroticsm_score, phq_score, Sex,
             starts_with("lag"))
    imp <- mice(imp_input, m = 1, method = "pmm", maxit = 5, printFlag = FALSE)
    dat_day_completed <- complete(imp, 1)
    full_daily_temp[full_daily_temp$day == d, vars] <- dat_day_completed[, vars]
  }
  all_imps[[r]] <- full_daily_temp
}

full_daily_imp <- all_imps[[1]]

for (v in vars) {
  mat <- sapply(all_imps, function(x) x[[v]])
  full_daily_imp[[v]] <- rowMeans(mat, na.rm = TRUE)
}

imp_dataset = full_daily_imp %>% left_join(survey, by = "participantidentifier")
imp_dataset <- imp_dataset %>%
  group_by(participantidentifier) %>%
  filter(day >= (max(daydiff) - 89), day <= max(daydiff)) %>%
  mutate(day = day - min(day) + 1) %>%
  ungroup()
noimp_dataset = full_daily_clean %>% left_join(survey, by = "participantidentifier")
noimp_dataset <- noimp_dataset %>%
  group_by(participantidentifier) %>%
  filter(day >= (max(daydiff) - 89), day <= max(daydiff)) %>%
  mutate(day = day - min(day) + 1) %>%
  ungroup()


#================================
#        Visualization
#================================

duration <- imp_dataset %>%
  mutate(
    enroll_date = as.Date(enroll_date),
    survey_date = as.Date(survey_date)
  )

duration <- duration %>%
  distinct(participantidentifier, .keep_all = TRUE) %>%
  mutate(
    duration_days = as.numeric(survey_date - enroll_date)
  ) %>%
  filter(!is.na(duration_days))

ggplot(duration, aes(x = duration_days)) +
  geom_histogram(bins = 25, color = "black", fill = "steelblue") +
  labs(
    x = "Days from baseline to first follow-up survey",
    y = "Number of participants",
    title = "Time Between Baseline and First Follow-Up Survey"
  ) + theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


vars_to_plot <- c(
  "minutessedentary",
  "sleep_eff",
  "steps",
  "restingheartrate",
  "mood"
)

nonmiss_by_day <- noimp_dataset %>%
  filter(day <= 90) %>%
  select(day,
         minutessedentary,
         sleep_eff,
         steps,
         restingheartrate,
         mood) %>%
  pivot_longer(
    cols = -day,
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    days_prior = 90 - day,
    label = case_when(
      variable == "minutessedentary" ~ "Active time variables",
      variable == "sleep_eff" ~ "Sleep variables",
      variable == "steps" ~ "Steps",
      variable == "restingheartrate" ~ "Resting heart rate",
      variable == "mood" ~ "Mood"
    ),
    label = factor(
      label,
      levels = c(
        "Active time variables",
        "Sleep variables",
        "Steps",
        "Resting heart rate",
        "Mood"
      )
    )
  ) %>%
  group_by(days_prior, label) %>%
  summarise(
    non_missing_rate = mean(!is.na(value)),
    .groups = "drop"
  )

ggplot(nonmiss_by_day,
       aes(x = days_prior, y = non_missing_rate, color = label)) +
  geom_line(linewidth = 1) +
  scale_x_reverse(
    limits = c(90, 0),
    breaks = c(90, 60, 30)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Days prior to the follow-up survey",
    y = "Non-missing rate",
    title = "Daily Non-Missing Rates Prior to Survey Date",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )


vars_to_plot <- all_vars#
c(
  "minutessedentary",
  "sleep_eff",
  "steps",
  "restingheartrate",
  "mood"
)

# Ensure Date types and unique enroll_date per participant
survey_enroll <- survey %>%
  mutate(enroll_date = as.Date(enroll_date)) %>%
  group_by(participantidentifier) %>%
  summarise(enroll_date = min(enroll_date, na.rm = TRUE), .groups = "drop")

full_daily2 <- full_daily %>%
  mutate(date = as.Date(date))

daily_with_day <- full_daily2 %>%
  left_join(survey_enroll, by = "participantidentifier") %>%
  mutate(day = as.integer(date - enroll_date)) %>%
  filter(!is.na(day), day >= 0, day <= 285) %>%
  select(participantidentifier, day, all_of(vars_to_plot))


daily_complete <- daily_with_day %>%
  complete(
    participantidentifier = unique(survey_enroll$participantidentifier),
    day = 0:285
  )


nonmiss_by_day <- daily_complete %>%
  pivot_longer(
    cols = all_of(vars_to_plot),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    label = case_when(
      variable == "minutessedentary" ~ "Active time variables",
      variable == "sleep_eff" ~ "Sleep variables",
      variable == "steps" ~ "Steps",
      variable == "restingheartrate" ~ "Resting heart rate",
      variable == "mood" ~ "Mood"
    )
  ) %>%
  group_by(day, label) %>%
  summarise(
    non_missing_rate = mean(!is.na(value)),
    .groups = "drop"
  ) %>%
  mutate(label = factor(
    label,
    levels = c("Active time variables", "Sleep variables",
               "Steps", "Resting heart rate", "Mood")
  ))

ggplot(nonmiss_by_day, aes(x = day, y = non_missing_rate, color = label)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(0, 285), breaks = seq(0, 285, by = 30)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Days since enrollment",
    y = "Non-missing rate",
    title = "Daily Non-Missing Rates Since Enrollment",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  )
