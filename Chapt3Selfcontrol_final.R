# Packages
library(readr)
library(tidyverse)
library(janitor)
library(here)
library(dplyr)
library(tidyr)
library(writexl)
library(readxl)
library(moments)
library(fitdistrplus)
library(gamlss)
library(glmmTMB)
library(performance)
library(loo)
library(DHARMa)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggplot2)

file_path <- here::here("..", "selfcontrolrawdataCERZORIGINAL.xlsx")
if (!file.exists(file_path)) {
  stop("File not found. Put selfcontrolrawdataCERZORIGINAL.xlsx in C:/Users/User/Documents/publication stats/self-control/")
}

CERZ_selfcontrol <- readxl::read_excel(
  file_path,
  na = c("NA", ""),
  col_types = "text"
)

CERZ_selfcontrol <- CERZ_selfcontrol %>% clean_names()

CERZ_selfcontrol <- CERZ_selfcontrol %>%
  mutate(
    pull_dr_5_l = factor(pull_dr_5_l, levels = "Y"),
    pull_dr_10_l = factor(pull_dr_10_l, levels = "Y"),
    pull_dr_20_l = factor(pull_dr_20_l, levels = "Y"),
    pull_dr_40_l = factor(pull_dr_40_l, levels = "Y"),
    pull_dr_80_l = factor(pull_dr_80_l, levels = "Y"),
    pull_dr_5_r = factor(pull_dr_5_r, levels = "Y"),
    pull_dr_10_r = factor(pull_dr_10_r, levels = "Y"),
    pull_dr_20_r = factor(pull_dr_20_r, levels = "Y"),
    pull_dr_40_r = factor(pull_dr_40_r, levels = "Y"),
    pull_dr_80_r = factor(pull_dr_80_r, levels = "Y"),
    collect_dr_5_l = factor(collect_dr_5_l, levels = "Y"),
    collect_dr_10_l = factor(collect_dr_10_l, levels = "Y"),
    collect_dr_20_l = factor(collect_dr_20_l, levels = "Y"),
    collect_dr_40_l = factor(collect_dr_40_l, levels = "Y"),
    collect_dr_80_l = factor(collect_dr_80_l, levels = "Y"),
    collect_dr_5_r = factor(collect_dr_5_r, levels = "Y"),
    collect_dr_10_r = factor(collect_dr_10_r, levels = "Y"),
    collect_dr_20_r = factor(collect_dr_20_r, levels = "Y"),
    collect_dr_40_r = factor(collect_dr_40_r, levels = "Y"),
    collect_dr_80_r = factor(collect_dr_80_r, levels = "Y"),
    pull_ir_l = factor(pull_ir_l, levels = "Y"),
    pull_ir_r = factor(pull_ir_r, levels = "Y"),
    collect_ir_l = factor(collect_ir_l, levels = "Y"),
    collect_ir_r = factor(collect_ir_r, levels = "Y"),
    no_collect_ir = factor(no_collect_ir, levels = "Y"),
    no_collect_dr = factor(no_collect_dr, levels = "Y"),
    look_left_d = factor(look_left_d, levels = "Y"),
    look_left_i = factor(look_left_i, levels = "Y"),
    look_right_d = factor(look_right_d, levels = "Y"),
    look_right_i = factor(look_right_i, levels = "Y")
  )

convert_to_seconds_SC <- function(time_string) {
  if (is.na(time_string) || !grepl("^\\d+:\\d+\\.\\d{3}$", time_string)) {
    return(NA_real_)
  }
  parts <- unlist(strsplit(time_string, "[:.]"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  milliseconds <- as.numeric(parts[3]) / 1000
  total_seconds <- minutes * 60 + seconds + milliseconds
  return(total_seconds)
}

CERZ_selfcontrol <- CERZ_selfcontrol %>%
  mutate(
    session_start = sapply(session_start, convert_to_seconds_SC),
    trial_start = sapply(trial_start, convert_to_seconds_SC),
    time_dr_5_l = sapply(time_dr_5_l, convert_to_seconds_SC),
    time_dr_10_l = sapply(time_dr_10_l, convert_to_seconds_SC),
    time_dr_20_l = sapply(time_dr_20_l, convert_to_seconds_SC),
    time_dr_40_l = sapply(time_dr_40_l, convert_to_seconds_SC),
    time_dr_80_l = sapply(time_dr_80_l, convert_to_seconds_SC),
    time_dr_5_r = sapply(time_dr_5_r, convert_to_seconds_SC),
    time_dr_10_r = sapply(time_dr_10_r, convert_to_seconds_SC),
    time_dr_20_r = sapply(time_dr_20_r, convert_to_seconds_SC),
    time_dr_40_r = sapply(time_dr_40_r, convert_to_seconds_SC),
    time_dr_80_r = sapply(time_dr_80_r, convert_to_seconds_SC),
    timec_dr_5_l = sapply(timec_dr_5_l, convert_to_seconds_SC),
    timec_dr_10_l = sapply(timec_dr_10_l, convert_to_seconds_SC),
    timec_dr_20_l = sapply(timec_dr_20_l, convert_to_seconds_SC),
    timec_dr_40_l = sapply(timec_dr_40_l, convert_to_seconds_SC),
    timec_dr_80_l = sapply(timec_dr_80_l, convert_to_seconds_SC),
    timec_dr_5_r = sapply(timec_dr_5_r, convert_to_seconds_SC),
    timec_dr_10_r = sapply(timec_dr_10_r, convert_to_seconds_SC),
    timec_dr_20_r = sapply(timec_dr_20_r, convert_to_seconds_SC),
    timec_dr_40_r = sapply(timec_dr_40_r, convert_to_seconds_SC),
    timec_dr_80_r = sapply(timec_dr_80_r, convert_to_seconds_SC),
    pull_ir_l_time = sapply(pull_ir_l_time, convert_to_seconds_SC),
    pull_ir_r_time = sapply(pull_ir_r_time, convert_to_seconds_SC),
    collect_ir_l_time = sapply(collect_ir_l_time, convert_to_seconds_SC),
    collect_ir_r_time = sapply(collect_ir_r_time, convert_to_seconds_SC),
    no_collect_ir_time = sapply(no_collect_ir_time, convert_to_seconds_SC),
    no_collect_dr_time = sapply(no_collect_dr_time, convert_to_seconds_SC),
    look_left_d_time = sapply(look_left_d_time, convert_to_seconds_SC),
    look_left_i_time = sapply(look_left_i_time, convert_to_seconds_SC),
    look_right_d_time = sapply(look_right_d_time, convert_to_seconds_SC),
    look_right_i_time = sapply(look_right_i_time, convert_to_seconds_SC),
    swap_reward_position_start = sapply(swap_reward_position_start, convert_to_seconds_SC),
    swap_reward_position_end = sapply(swap_reward_position_end, convert_to_seconds_SC),
    walk_away_start = sapply(walk_away_start, convert_to_seconds_SC),
    walk_away_end = sapply(walk_away_end, convert_to_seconds_SC),
    walk_away_mp_start = sapply(walk_away_mp_start, convert_to_seconds_SC),
    walk_away_mp_end = sapply(walk_away_mp_end, convert_to_seconds_SC),
    monkey_present_start = sapply(monkey_present_start, convert_to_seconds_SC),
    monkey_present_end = sapply(monkey_present_end, convert_to_seconds_SC)
  )

file_path <- here::here("..", "selfcontrolrawdataEMERORIGINAL.xlsx")
if (!file.exists(file_path)) {
  stop("File not found. Put selfcontrolrawdataCERZORIGINAL.xlsx in C:/Users/User/Documents/publication stats/self-control/")
}

EMER_selfcontrol <- readxl::read_excel(
  file_path,
  na = c("NA", ""),
  col_types = "text"
)

EMER_selfcontrol <- EMER_selfcontrol %>% clean_names()

EMER_selfcontrol <- EMER_selfcontrol %>%
  mutate(
    pull_dr_5_l = factor(pull_dr_5_l, levels = "Y"),
    pull_dr_10_l = factor(pull_dr_10_l, levels = "Y"),
    pull_dr_20_l = factor(pull_dr_20_l, levels = "Y"),
    pull_dr_40_l = factor(pull_dr_40_l, levels = "Y"),
    pull_dr_80_l = factor(pull_dr_80_l, levels = "Y"),
    pull_dr_5_r = factor(pull_dr_5_r, levels = "Y"),
    pull_dr_10_r = factor(pull_dr_10_r, levels = "Y"),
    pull_dr_20_r = factor(pull_dr_20_r, levels = "Y"),
    pull_dr_40_r = factor(pull_dr_40_r, levels = "Y"),
    pull_dr_80_r = factor(pull_dr_80_r, levels = "Y"),
    collect_dr_5_l = factor(collect_dr_5_l, levels = "Y"),
    collect_dr_10_l = factor(collect_dr_10_l, levels = "Y"),
    collect_dr_20_l = factor(collect_dr_20_l, levels = "Y"),
    collect_dr_40_l = factor(collect_dr_40_l, levels = "Y"),
    collect_dr_80_l = factor(collect_dr_80_l, levels = "Y"),
    collect_dr_5_r = factor(collect_dr_5_r, levels = "Y"),
    collect_dr_10_r = factor(collect_dr_10_r, levels = "Y"),
    collect_dr_20_r = factor(collect_dr_20_r, levels = "Y"),
    collect_dr_40_r = factor(collect_dr_40_r, levels = "Y"),
    collect_dr_80_r = factor(collect_dr_80_r, levels = "Y"),
    pull_ir_l = factor(pull_ir_l, levels = "Y"),
    pull_ir_r = factor(pull_ir_r, levels = "Y"),
    collect_ir_l = factor(collect_ir_l, levels = "Y"),
    collect_ir_r = factor(collect_ir_r, levels = "Y"),
    no_collect_ir = factor(no_collect_ir, levels = "Y"),
    no_collect_dr = factor(no_collect_dr, levels = "Y"),
    look_left_d = factor(look_left_d, levels = "Y"),
    look_left_i = factor(look_left_i, levels = "Y"),
    look_right_d = factor(look_right_d, levels = "Y"),
    look_right_i = factor(look_right_i, levels = "Y")
  )

convert_to_seconds_SC <- function(time_string) {
  if (is.na(time_string) || !grepl("^\\d+:\\d+\\.\\d{3}$", time_string)) {
    return(NA_real_)
  }
  parts <- unlist(strsplit(time_string, "[:.]"))
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  milliseconds <- as.numeric(parts[3]) / 1000
  total_seconds <- minutes * 60 + seconds + milliseconds
  return(total_seconds)
}

EMER_selfcontrol <- EMER_selfcontrol %>%
  mutate(
    session_start = sapply(session_start, convert_to_seconds_SC),
    trial_start = sapply(trial_start, convert_to_seconds_SC),
    time_dr_5_l = sapply(time_dr_5_l, convert_to_seconds_SC),
    time_dr_10_l = sapply(time_dr_10_l, convert_to_seconds_SC),
    time_dr_20_l = sapply(time_dr_20_l, convert_to_seconds_SC),
    time_dr_40_l = sapply(time_dr_40_l, convert_to_seconds_SC),
    time_dr_80_l = sapply(time_dr_80_l, convert_to_seconds_SC),
    time_dr_5_r = sapply(time_dr_5_r, convert_to_seconds_SC),
    time_dr_10_r = sapply(time_dr_10_r, convert_to_seconds_SC),
    time_dr_20_r = sapply(time_dr_20_r, convert_to_seconds_SC),
    time_dr_40_r = sapply(time_dr_40_r, convert_to_seconds_SC),
    time_dr_80_r = sapply(time_dr_80_r, convert_to_seconds_SC),
    timec_dr_5_l = sapply(timec_dr_5_l, convert_to_seconds_SC),
    timec_dr_10_l = sapply(timec_dr_10_l, convert_to_seconds_SC),
    timec_dr_20_l = sapply(timec_dr_20_l, convert_to_seconds_SC),
    timec_dr_40_l = sapply(timec_dr_40_l, convert_to_seconds_SC),
    timec_dr_80_l = sapply(timec_dr_80_l, convert_to_seconds_SC),
    timec_dr_5_r = sapply(timec_dr_5_r, convert_to_seconds_SC),
    timec_dr_10_r = sapply(timec_dr_10_r, convert_to_seconds_SC),
    timec_dr_20_r = sapply(timec_dr_20_r, convert_to_seconds_SC),
    timec_dr_40_r = sapply(timec_dr_40_r, convert_to_seconds_SC),
    timec_dr_80_r = sapply(timec_dr_80_r, convert_to_seconds_SC),
    pull_ir_l_time = sapply(pull_ir_l_time, convert_to_seconds_SC),
    pull_ir_r_time = sapply(pull_ir_r_time, convert_to_seconds_SC),
    collect_ir_l_time = sapply(collect_ir_l_time, convert_to_seconds_SC),
    collect_ir_r_time = sapply(collect_ir_r_time, convert_to_seconds_SC),
    no_collect_ir_time = sapply(no_collect_ir_time, convert_to_seconds_SC),
    no_collect_dr_time = sapply(no_collect_dr_time, convert_to_seconds_SC),
    look_left_d_time = sapply(look_left_d_time, convert_to_seconds_SC),
    look_left_i_time = sapply(look_left_i_time, convert_to_seconds_SC),
    look_right_d_time = sapply(look_right_d_time, convert_to_seconds_SC),
    look_right_i_time = sapply(look_right_i_time, convert_to_seconds_SC),
    swap_reward_position_start = sapply(swap_reward_position_start, convert_to_seconds_SC),
    swap_reward_position_end = sapply(swap_reward_position_end, convert_to_seconds_SC),
    walk_away_start = sapply(walk_away_start, convert_to_seconds_SC),
    walk_away_end = sapply(walk_away_end, convert_to_seconds_SC),
    walk_away_mp_start = sapply(walk_away_mp_start, convert_to_seconds_SC),
    walk_away_mp_end = sapply(walk_away_mp_end, convert_to_seconds_SC),
    monkey_present_start = sapply(monkey_present_start, convert_to_seconds_SC),
    monkey_present_end = sapply(monkey_present_end, convert_to_seconds_SC)
  )

pull_cols <- c(
  "pull_dr_5_l", "pull_dr_10_l", "pull_dr_20_l", "pull_dr_40_l", "pull_dr_80_l",
  "pull_dr_5_r", "pull_dr_10_r", "pull_dr_20_r", "pull_dr_40_r", "pull_dr_80_r"
)
pull_counts <- CERZ_selfcontrol %>%
  summarise(across(all_of(pull_cols), ~ sum(.x == "Y", na.rm = TRUE)))
total_pulls <- sum(pull_counts)
print(total_pulls)

pull_ir_cols <- c("pull_ir_l", "pull_ir_r")
pull_ir_counts <- CERZ_selfcontrol %>%
  summarise(across(all_of(pull_ir_cols), ~ sum(.x == "Y", na.rm = TRUE)))
total_ir_pulls <- sum(pull_ir_counts)
print(total_ir_pulls)

collect_cols <- c(
  "collect_dr_5_l", "collect_dr_10_l", "collect_dr_20_l", "collect_dr_40_l", "collect_dr_80_l",
  "collect_dr_5_r", "collect_dr_10_r", "collect_dr_20_r", "collect_dr_40_r", "collect_dr_80_r"
)
collect_counts <- CERZ_selfcontrol %>%
  summarise(across(all_of(collect_cols), ~ sum(.x == "Y", na.rm = TRUE)))
total_collects <- sum(collect_counts)
print(total_collects)

collect_counts_per_duration <- CERZ_selfcontrol %>%
  summarise(
    latency_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),
    latency_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),
    latency_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),
    latency_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),
    latency_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  )

print(collect_counts_per_duration)

collect_counts_per_monkey_CERZ <- CERZ_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    latency_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),
    latency_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),
    latency_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),
    latency_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),
    latency_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  )

View(collect_counts_per_monkey_CERZ)

left_cols <- c(
  "collect_dr_5_l", "collect_dr_10_l", "collect_dr_20_l", "collect_dr_40_l", "collect_dr_80_l"
)

right_cols <- c(
  "collect_dr_5_r", "collect_dr_10_r", "collect_dr_20_r", "collect_dr_40_r", "collect_dr_80_r"
)

side_counts <- CERZ_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    left_total = sum(across(all_of(left_cols), ~ sum(.x == "Y", na.rm = TRUE))),
    right_total = sum(across(all_of(right_cols), ~ sum(.x == "Y", na.rm = TRUE)))
  )

print(side_counts)

preference_tests <- side_counts %>%
  rowwise() %>%
  mutate(
    p_value = chisq.test(c(left_total, right_total), p = c(0.5, 0.5))$p.value
  )
print(preference_tests)

combined_p <- sum(-2 * log(preference_tests$p_value)) > qchisq(0.95, df = 2 * nrow(preference_tests))
combined_p_value <- pchisq(sum(-2 * log(preference_tests$p_value)), df = 2 * nrow(preference_tests), lower.tail = FALSE)

print(combined_p_value)

pull_cols_EMER <- c(
  "pull_dr_5_l", "pull_dr_10_l", "pull_dr_20_l", "pull_dr_40_l", "pull_dr_80_l",
  "pull_dr_5_r", "pull_dr_10_r", "pull_dr_20_r", "pull_dr_40_r", "pull_dr_80_r"
)

pull_counts_EMER <- EMER_selfcontrol %>%
  summarise(across(all_of(pull_cols_EMER), ~ sum(.x == "Y", na.rm = TRUE)))
total_pulls_EMER <- sum(pull_counts_EMER)

print(total_pulls_EMER)

pull_ir_cols_EMER <- c("pull_ir_l", "pull_ir_r")

pull_ir_counts_EMER <- EMER_selfcontrol %>%
  summarise(across(all_of(pull_ir_cols_EMER), ~ sum(.x == "Y", na.rm = TRUE)))
total_ir_pulls_EMER <- sum(pull_ir_counts_EMER)

print(total_ir_pulls_EMER)

collect_cols_EMER <- c(
  "collect_dr_5_l", "collect_dr_10_l", "collect_dr_20_l", "collect_dr_40_l", "collect_dr_80_l",
  "collect_dr_5_r", "collect_dr_10_r", "collect_dr_20_r", "collect_dr_40_r", "collect_dr_80_r"
)

collect_counts_EMER <- EMER_selfcontrol %>%
  summarise(across(all_of(collect_cols_EMER), ~ sum(.x == "Y", na.rm = TRUE)))
total_collects_EMER <- sum(collect_counts_EMER)

print(total_collects_EMER)

collect_counts_per_duration_EMER <- EMER_selfcontrol %>%
  summarise(
    latency_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),
    latency_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),
    latency_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),
    latency_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),
    latency_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  )

print(collect_counts_per_duration_EMER)
View(collect_counts_per_duration_EMER)

collect_counts_per_monkey_EMER <- EMER_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    latency_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),
    latency_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),
    latency_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),
    latency_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),
    latency_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  )

View(collect_counts_per_monkey_EMER)

left_cols_EMER <- c(
  "collect_dr_5_l", "collect_dr_10_l", "collect_dr_20_l", "collect_dr_40_l", "collect_dr_80_l"
)

right_cols_EMER <- c(
  "collect_dr_5_r", "collect_dr_10_r", "collect_dr_20_r", "collect_dr_40_r", "collect_dr_80_r"
)

side_counts_EMER <- EMER_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    left_total = sum(across(all_of(left_cols_EMER), ~ sum(.x == "Y", na.rm = TRUE))),
    right_total = sum(across(all_of(right_cols_EMER), ~ sum(.x == "Y", na.rm = TRUE)))
  )

print(side_counts_EMER)

preference_tests_EMER <- side_counts_EMER %>%
  rowwise() %>%
  mutate(
    p_value = chisq.test(c(left_total, right_total), p = c(0.5, 0.5))$p.value
  )

print(preference_tests_EMER)

combined_p_EMER <- sum(-2 * log(preference_tests_EMER$p_value)) > qchisq(0.95, df = 2 * nrow(preference_tests_EMER))
combined_p_value_EMER <- pchisq(sum(-2 * log(preference_tests_EMER$p_value)), df = 2 * nrow(preference_tests_EMER), lower.tail = FALSE)

print(combined_p_value_EMER)

collect_counts_per_monkey_combined <- bind_rows(
  collect_counts_per_monkey_CERZ     %>% mutate(group = "CERZ"),
  collect_counts_per_monkey_EMER %>% mutate(group = "EMER")
) %>%
  arrange(group, monkey_id)

delay_success_combined_percent <- collect_counts_per_monkey_combined %>%
  summarise(
    total_monkeys = n(),
    `5 seconds`  = sum(latency_5  > 0),
    `10 seconds` = sum(latency_10 > 0),
    `20 seconds` = sum(latency_20 > 0),
    `40 seconds` = sum(latency_40 > 0),
    `80 seconds` = sum(latency_80 > 0)
  ) %>%
  pivot_longer(
    cols = -total_monkeys,
    names_to = "Delay_Window",
    values_to = "n_successful_monkeys"
  ) %>%
  mutate(
    Percentage_Successful = round(n_successful_monkeys / total_monkeys * 100, 1),
    Individuals = paste0(n_successful_monkeys, "/", total_monkeys)
  ) %>%
  select(Delay_Window, Individuals, Percentage_Successful, n_successful_monkeys)

View(delay_success_combined_percent)

no_collect_ir_CERZ <- CERZ_selfcontrol %>%
  summarise(n_no_collect_ir = sum(no_collect_ir == "Y", na.rm = TRUE)) %>%
  mutate(group = "CERZ")

no_collect_ir_EMER <- EMER_selfcontrol %>%
  summarise(n_no_collect_ir = sum(no_collect_ir == "Y", na.rm = TRUE)) %>%
  mutate(group = "EMER")

no_collect_ir_combined <- bind_rows(no_collect_ir_CERZ, no_collect_ir_EMER)
print(no_collect_ir_combined)

total_no_collect_ir <- sum(no_collect_ir_combined$n_no_collect_ir)
print(paste("Total no_collect_ir across both groups:", total_no_collect_ir))

reward_collection_speed <- CERZ_selfcontrol %>%
  filter(if_any(all_of(collect_cols), ~ .x == "Y")) %>%
  select(
    monkey_id, session, session_start, trial, trial_start,
    collect_dr_5_l, timec_dr_5_l,
    collect_dr_10_l, timec_dr_10_l,
    collect_dr_20_l, timec_dr_20_l,
    collect_dr_40_l, timec_dr_40_l,
    collect_dr_80_l, timec_dr_80_l,
    collect_dr_5_r, timec_dr_5_r,
    collect_dr_10_r, timec_dr_10_r,
    collect_dr_20_r, timec_dr_20_r,
    collect_dr_40_r, timec_dr_40_r,
    collect_dr_80_r, timec_dr_80_r
  )

reward_collection_speed_EMER <- EMER_selfcontrol %>%
  filter(if_any(all_of(collect_cols_EMER), ~ .x == "Y")) %>%
  select(
    monkey_id, session, session_start, trial, trial_start,
    collect_dr_5_l, timec_dr_5_l,
    collect_dr_10_l, timec_dr_10_l,
    collect_dr_20_l, timec_dr_20_l,
    collect_dr_40_l, timec_dr_40_l,
    collect_dr_80_l, timec_dr_80_l,
    collect_dr_5_r, timec_dr_5_r,
    collect_dr_10_r, timec_dr_10_r,
    collect_dr_20_r, timec_dr_20_r,
    collect_dr_40_r, timec_dr_40_r,
    collect_dr_80_r, timec_dr_80_r
  )

reward_collection_speed_5 <- reward_collection_speed %>%
  filter(collect_dr_5_l == "Y" | collect_dr_5_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_5_l, collect_dr_5_r),
    names_to = "side",
    names_pattern = "collect_dr_5_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_5_l, timec_dr_5_r),
    names_to = "time_side",
    names_pattern = "timec_dr_5_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

reward_collection_speed_5 <- reward_collection_speed_5 %>%
  mutate(group = "CERZ")

View(reward_collection_speed_5)

reward_collection_speed_5_EMER <- reward_collection_speed_EMER %>%
  filter(collect_dr_5_l == "Y" | collect_dr_5_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_5_l, collect_dr_5_r),
    names_to = "side",
    names_pattern = "collect_dr_5_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_5_l, timec_dr_5_r),
    names_to = "time_side",
    names_pattern = "timec_dr_5_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

reward_collection_speed_5_EMER <- reward_collection_speed_5_EMER %>%
  mutate(group = "EMER")

View(reward_collection_speed_5_EMER)

CERZ_EMER_faster_collection_5 <- bind_rows(
  reward_collection_speed_5,
  reward_collection_speed_5_EMER
)
print(CERZ_EMER_faster_collection_5)

max(CERZ_EMER_faster_collection_5$collection_latency)
mean(CERZ_EMER_faster_collection_5$collection_latency)
sd(CERZ_EMER_faster_collection_5$collection_latency)

summary(CERZ_EMER_faster_collection_5$collection_latency)
table(is.na(CERZ_EMER_faster_collection_5$collection_latency))

boxplot(collection_latency ~ group, data = CERZ_EMER_faster_collection_5,
        main = "Latency by Group", ylab = "Seconds")

boxplot(collection_latency ~ monkey_id, data = CERZ_EMER_faster_collection_5,
        main = "Latency by Monkey", ylab = "Seconds", las = 2)

boxplot(collection_latency ~ session, data = CERZ_EMER_faster_collection_5,
        main = "Latency by Session", ylab = "Seconds")

hist(CERZ_EMER_faster_collection_5$collection_latency, breaks = 50,
     main = "Raw Latency Distribution", xlab = "Collection Latency (seconds)")

CERZ_EMER_faster_collection_5_allsessions <- CERZ_EMER_faster_collection_5 %>%
  distinct(monkey_id, group) %>%
  crossing(session = 1:4)

lat_per_session5 <- CERZ_EMER_faster_collection_5 %>%
  mutate(session = as.integer(session)) %>%
  group_by(monkey_id, session) %>%
  summarise(collection_latency = median(collection_latency, na.rm = TRUE), .groups = "drop")

lat_per_session5 <- CERZ_EMER_faster_collection_5_allsessions %>%
  left_join(lat_per_session5, by = c("monkey_id", "session"))

lat_per_session5 <- lat_per_session5 %>%
  mutate(collection_latency = ifelse(is.na(collection_latency), 190, collection_latency))

friedman.test(collection_latency ~ session | monkey_id, data = lat_per_session5)

reward_collection_speed_10 <- reward_collection_speed %>%
  filter(collect_dr_10_l == "Y" | collect_dr_10_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_10_l, collect_dr_10_r),
    names_to = "side",
    names_pattern = "collect_dr_10_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_10_l, timec_dr_10_r),
    names_to = "time_side",
    names_pattern = "timec_dr_10_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

reward_collection_speed_10 <- reward_collection_speed_10 %>%
  mutate(group = "CERZ")

reward_collection_speed_10_EMER <- reward_collection_speed_EMER %>%
  filter(collect_dr_10_l == "Y" | collect_dr_10_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_10_l, collect_dr_10_r),
    names_to = "side",
    names_pattern = "collect_dr_10_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_10_l, timec_dr_10_r),
    names_to = "time_side",
    names_pattern = "timec_dr_10_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

print(reward_collection_speed_10_EMER)

reward_collection_speed_10_EMER <- reward_collection_speed_10_EMER %>%
  mutate(group = "EMER")

CERZ_EMER_faster_collection_10 <- bind_rows(
  reward_collection_speed_10,
  reward_collection_speed_10_EMER
)

mean(CERZ_EMER_faster_collection_10$collection_latency)
sd(CERZ_EMER_faster_collection_10$collection_latency)
summary(CERZ_EMER_faster_collection_10$collection_latency)

summary(CERZ_EMER_faster_collection_10$collection_latency)
table(is.na(CERZ_EMER_faster_collection_10$collection_latency))

boxplot(collection_latency ~ group, data = CERZ_EMER_faster_collection_10,
        main = "Latency by Group", ylab = "Seconds")

boxplot(collection_latency ~ monkey_id, data = CERZ_EMER_faster_collection_10,
        main = "Latency by Monkey", ylab = "Seconds", las = 2)

boxplot(collection_latency ~ session, data = CERZ_EMER_faster_collection_10,
        main = "Latency by Session", ylab = "Seconds")

CERZ_EMER_faster_collection_10_allsessions <- CERZ_EMER_faster_collection_10 %>%
  distinct(monkey_id, group) %>%
  crossing(session = 1:4)

lat_per_session10 <- CERZ_EMER_faster_collection_10 %>%
  mutate(session = as.integer(session)) %>%
  group_by(monkey_id, session) %>%
  summarise(collection_latency = median(collection_latency, na.rm = TRUE), .groups = "drop")

lat_per_session10 <- CERZ_EMER_faster_collection_10_allsessions %>%
  left_join(lat_per_session10, by = c("monkey_id", "session"))

lat_per_session10 <- lat_per_session10 %>%
  mutate(collection_latency = ifelse(is.na(collection_latency), 295, collection_latency))

friedman.test(collection_latency ~ session | monkey_id, data = lat_per_session10)

reward_collection_speed_20 <- reward_collection_speed %>%
  filter(collect_dr_20_l == "Y" | collect_dr_20_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_20_l, collect_dr_20_r),
    names_to = "side",
    names_pattern = "collect_dr_20_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_20_l, timec_dr_20_r),
    names_to = "time_side",
    names_pattern = "timec_dr_20_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

reward_collection_speed_20 <- reward_collection_speed_20 %>%
  mutate(group = "CERZ")

reward_collection_speed_20_EMER <- reward_collection_speed_EMER %>%
  filter(collect_dr_20_l == "Y" | collect_dr_20_r == "Y") %>%
  pivot_longer(
    cols = c(collect_dr_20_l, collect_dr_20_r),
    names_to = "side",
    names_pattern = "collect_dr_20_([lr])",
    values_to = "collected"
  ) %>%
  pivot_longer(
    cols = c(timec_dr_20_l, timec_dr_20_r),
    names_to = "time_side",
    names_pattern = "timec_dr_20_([lr])",
    values_to = "collection_time"
  ) %>%
  filter(side == time_side, collected == "Y") %>%
  mutate(
    collection_latency = collection_time - trial_start
  ) %>%
  select(monkey_id, session, trial, trial_start, side, collection_time, collection_latency)

print(reward_collection_speed_20_EMER)

reward_collection_speed_20_EMER <- reward_collection_speed_20_EMER %>%
  mutate(group = "EMER")

CERZ_EMER_faster_collection_20 <- bind_rows(
  reward_collection_speed_20,
  reward_collection_speed_20_EMER
)

mean(CERZ_EMER_faster_collection_20$collection_latency)
sd(CERZ_EMER_faster_collection_20$collection_latency)
summary(CERZ_EMER_faster_collection_20$collection_latency)

CERZ_EMER_faster_collection_20_allsessions <- CERZ_EMER_faster_collection_20 %>%
  distinct(monkey_id, group) %>%
  crossing(session = 1:4)

lat_per_session20 <- CERZ_EMER_faster_collection_20 %>%
  mutate(session = as.integer(session)) %>%
  group_by(monkey_id, session) %>%
  summarise(collection_latency = median(collection_latency, na.rm = TRUE), .groups = "drop")

lat_per_session20 <- CERZ_EMER_faster_collection_20_allsessions %>%
  left_join(lat_per_session20, by = c("monkey_id", "session"))

lat_per_session20 <- lat_per_session20 %>%
  mutate(collection_latency = ifelse(is.na(collection_latency), 208, collection_latency))

friedman.test(collection_latency ~ session | monkey_id, data = lat_per_session20)

collect_per_monkey_rates <- CERZ_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    attempts_5  = sum(pull_dr_5_l == "Y", na.rm = TRUE) + sum(pull_dr_5_r == "Y", na.rm = TRUE),
    successes_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),

    attempts_10 = sum(pull_dr_10_l == "Y", na.rm = TRUE) + sum(pull_dr_10_r == "Y", na.rm = TRUE),
    successes_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),

    attempts_20 = sum(pull_dr_20_l == "Y", na.rm = TRUE) + sum(pull_dr_20_r == "Y", na.rm = TRUE),
    successes_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),

    attempts_40 = sum(pull_dr_40_l == "Y", na.rm = TRUE) + sum(pull_dr_40_r == "Y", na.rm = TRUE),
    successes_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),

    attempts_80 = sum(pull_dr_80_l == "Y", na.rm = TRUE) + sum(pull_dr_80_r == "Y", na.rm = TRUE),
    successes_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  ) %>%
  mutate(group = "CERZ") %>%
  rowwise() %>%
  mutate(
    success_5 = successes_5 / attempts_5 * 100,
    success_10 = successes_10 / attempts_10 * 100,
    success_20 = successes_20 / attempts_20 * 100,
    success_40 = successes_40 / attempts_40 * 100,
    success_80 = successes_80 / attempts_80 * 100
  ) %>%
  select(monkey_id, group, success_5, success_10, success_20, success_40, success_80) %>%
  ungroup() %>%
  arrange(monkey_id)

view(collect_per_monkey_rates)

collective_mean_rates <- collect_per_monkey_rates %>%
  summarise(
    mean_5 = mean(success_5, na.rm = TRUE),
    mean_10 = mean(success_10, na.rm = TRUE),
    mean_20 = mean(success_20, na.rm = TRUE),
    mean_40 = mean(success_40, na.rm = TRUE),
    mean_80 = mean(success_80, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "delay", values_to = "mean_success_rate_pct") %>%
  arrange(delay)
view(collective_mean_rates)

collect_per_monkey_rates_EMER <- EMER_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    attempts_5 = sum(pull_dr_5_l == "Y", na.rm = TRUE) + sum(pull_dr_5_r == "Y", na.rm = TRUE),
    successes_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),

    attempts_10 = sum(pull_dr_10_l == "Y", na.rm = TRUE) + sum(pull_dr_10_r == "Y", na.rm = TRUE),
    successes_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),

    attempts_20 = sum(pull_dr_20_l == "Y", na.rm = TRUE) + sum(pull_dr_20_r == "Y", na.rm = TRUE),
    successes_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),

    attempts_40 = sum(pull_dr_40_l == "Y", na.rm = TRUE) + sum(pull_dr_40_r == "Y", na.rm = TRUE),
    successes_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),

    attempts_80 = sum(pull_dr_80_l == "Y", na.rm = TRUE) + sum(pull_dr_80_r == "Y", na.rm = TRUE),
    successes_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  ) %>%
  mutate(group = "EMER") %>%
  rowwise() %>%
  mutate(
    success_5 = successes_5 / attempts_5 * 100,
    success_10 = successes_10 / attempts_10 * 100,
    success_20 = successes_20 / attempts_20 * 100,
    success_40 = successes_40 / attempts_40 * 100,
    success_80 = successes_80 / attempts_80 * 100
  ) %>%
  select(monkey_id, group, success_5, success_10, success_20, success_40, success_80) %>%
  ungroup() %>%
  arrange(monkey_id)

View(collect_per_monkey_rates_EMER)

collective_mean_rates_EMER <- collect_per_monkey_rates_EMER %>%
  summarise(
    mean_5 = mean(success_5, na.rm = TRUE),
    mean_10 = mean(success_10, na.rm = TRUE),
    mean_20 = mean(success_20, na.rm = TRUE),
    mean_40 = mean(success_40, na.rm = TRUE),
    mean_80 = mean(success_80, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "delay", values_to = "mean_success_rate_pct") %>%
  arrange(delay)

View(collective_mean_rates_EMER)

collect_per_monkey_rates_combined <- bind_rows(
  collect_per_monkey_rates,
  collect_per_monkey_rates_EMER
) %>%
  arrange(monkey_id, group)

collective_mean_rates_combined <- collect_per_monkey_rates_combined %>%
  summarise(
    mean_5  = mean(success_5, na.rm = TRUE),
    mean_10 = mean(success_10, na.rm = TRUE),
    mean_20 = mean(success_20, na.rm = TRUE),
    mean_40 = mean(success_40, na.rm = TRUE),
    mean_80 = mean(success_80, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "delay", values_to = "mean_success_rate_pct") %>%
  mutate(
    delay = gsub("mean_", "", delay),
    mean_success_rate_pct = round(mean_success_rate_pct, 1)
  ) %>%
  arrange(delay)

View(collective_mean_rates_combined)

collect_per_monkey_rates_raw <- CERZ_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    attempts_5  = sum(pull_dr_5_l == "Y", na.rm = TRUE) + sum(pull_dr_5_r == "Y", na.rm = TRUE),
    successes_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),

    attempts_10 = sum(pull_dr_10_l == "Y", na.rm = TRUE) + sum(pull_dr_10_r == "Y", na.rm = TRUE),
    successes_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),

    attempts_20 = sum(pull_dr_20_l == "Y", na.rm = TRUE) + sum(pull_dr_20_r == "Y", na.rm = TRUE),
    successes_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),

    attempts_40 = sum(pull_dr_40_l == "Y", na.rm = TRUE) + sum(pull_dr_40_r == "Y", na.rm = TRUE),
    successes_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),

    attempts_80 = sum(pull_dr_80_l == "Y", na.rm = TRUE) + sum(pull_dr_80_r == "Y", na.rm = TRUE),
    successes_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  ) %>%
  mutate(group = "CERZ")

collect_per_monkey_rates_raw_EMER <- EMER_selfcontrol %>%
  group_by(monkey_id) %>%
  summarise(
    attempts_5  = sum(pull_dr_5_l == "Y", na.rm = TRUE) + sum(pull_dr_5_r == "Y", na.rm = TRUE),
    successes_5 = sum(collect_dr_5_l == "Y", na.rm = TRUE) + sum(collect_dr_5_r == "Y", na.rm = TRUE),

    attempts_10 = sum(pull_dr_10_l == "Y", na.rm = TRUE) + sum(pull_dr_10_r == "Y", na.rm = TRUE),
    successes_10 = sum(collect_dr_10_l == "Y", na.rm = TRUE) + sum(collect_dr_10_r == "Y", na.rm = TRUE),

    attempts_20 = sum(pull_dr_20_l == "Y", na.rm = TRUE) + sum(pull_dr_20_r == "Y", na.rm = TRUE),
    successes_20 = sum(collect_dr_20_l == "Y", na.rm = TRUE) + sum(collect_dr_20_r == "Y", na.rm = TRUE),

    attempts_40 = sum(pull_dr_40_l == "Y", na.rm = TRUE) + sum(pull_dr_40_r == "Y", na.rm = TRUE),
    successes_40 = sum(collect_dr_40_l == "Y", na.rm = TRUE) + sum(collect_dr_40_r == "Y", na.rm = TRUE),

    attempts_80 = sum(pull_dr_80_l == "Y", na.rm = TRUE) + sum(pull_dr_80_r == "Y", na.rm = TRUE),
    successes_80 = sum(collect_dr_80_l == "Y", na.rm = TRUE) + sum(collect_dr_80_r == "Y", na.rm = TRUE)
  ) %>%
  mutate(group = "EMER")

CERZ_EMER_rates_raw <- bind_rows(
  collect_per_monkey_rates_raw,
  collect_per_monkey_rates_raw_EMER
)

CERZ_EMER_rates_long <- CERZ_EMER_rates_raw %>%
  pivot_longer(
    cols = c(attempts_5, attempts_10, attempts_20, attempts_40, attempts_80,
             successes_5, successes_10, successes_20, successes_40, successes_80),
    names_to = c(".value", "delay_window"),
    names_pattern = "(attempts|successes)_(.*)"
  ) %>%
  filter(attempts > 0) %>%
  mutate(
    success_rate = successes / attempts * 100,
    delay_window = factor(delay_window, levels = c("5", "10", "20", "40", "80"))
  )

CERZ_EMER_rates_long_5_20 <- CERZ_EMER_rates_long %>%
  filter(delay_window %in% c("5", "10", "20")) %>%
  mutate(delay_window = factor(delay_window, levels = c("5", "10", "20")))

kerana_20 <- tibble(
  monkey_id = "KERANA",
  group = "EMER",
  delay_window = factor("20", levels = c("5", "10", "20")),
  attempts = 0,
  successes = 0,
  success_rate = 0
)

CERZ_EMER_rates_long_5_20 <- bind_rows(CERZ_EMER_rates_long_5_20, kerana_20) %>%
  mutate(delay_window = factor(delay_window, levels = c("5", "10", "20")))

CERZ_EMER_rates_long_5_20 %>%
  group_by(monkey_id) %>%
  summarise(n_windows = n()) %>%
  print(n = 14)

friedman.test(success_rate ~ delay_window | monkey_id, data = CERZ_EMER_rates_long_5_20)

rates_5  <- CERZ_EMER_rates_long_5_20 %>% filter(delay_window == "5")  %>% pull(success_rate)
rates_10 <- CERZ_EMER_rates_long_5_20 %>% filter(delay_window == "10") %>% pull(success_rate)
rates_20 <- CERZ_EMER_rates_long_5_20 %>% filter(delay_window == "20") %>% pull(success_rate)

wilcox_5_10  <- wilcox.test(rates_5, rates_10, paired = TRUE)
wilcox_5_20  <- wilcox.test(rates_5, rates_20, paired = TRUE)
wilcox_10_20 <- wilcox.test(rates_10, rates_20, paired = TRUE)

p_values <- c(wilcox_5_10$p.value, wilcox_5_20$p.value, wilcox_10_20$p.value)
p_adjusted <- p.adjust(p_values, method = "bonferroni")

data.frame(
  comparison = c("5s vs 10s", "5s vs 20s", "10s vs 20s"),
  p_raw = round(p_values, 4),
  p_bonferroni = round(p_adjusted, 4)
)

p_values <- c(wilcox_5_10$p.value, wilcox_5_20$p.value, wilcox_10_20$p.value)
p_adjusted <- p.adjust(p_values, method = "bonferroni")

data.frame(
  comparison = c("5s vs 10s", "5s vs 20s", "10s vs 20s"),
  p_raw = round(p_values, 4),
  p_bonferroni = round(p_adjusted, 4)
)

plot_delay_window_dif <- CERZ_EMER_rates_long_5_20 %>%
  group_by(delay_window) %>%
  summarise(
    mean_success = mean(success_rate, na.rm = TRUE),
    se = sd(success_rate, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(plot_delay_window_dif, aes(x = delay_window, y = mean_success)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_success - se, ymax = mean_success + se),
                width = 0.15, linewidth = 0.7) +
  annotate("segment", x = 1, xend = 2, y = 48, yend = 48) +
  annotate("text", x = 1.5, y = 49.5, label = "**", size = 5) +
  annotate("segment", x = 1, xend = 3, y = 54, yend = 54) +
  annotate("text", x = 2, y = 55.5, label = "**", size = 5) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  labs(
    title = "Delayed Reward Collection Success Rate Across Delay Windows",
    x = "Delay Duration (seconds)",
    y = "Mean Collection Success Rate (%)"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5)
  )

delay_window_plot <- ggplot(plot_delay_window_dif, aes(x = delay_window, y = mean_success)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_success - se, ymax = mean_success + se),
                width = 0.15, linewidth = 0.7) +
  annotate("segment", x = 1, xend = 2, y = 48, yend = 48) +
  annotate("text", x = 1.5, y = 49.5, label = "**", size = 5) +
  annotate("segment", x = 1, xend = 3, y = 54, yend = 54) +
  annotate("text", x = 2, y = 55.5, label = "**", size = 5) +
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  labs(
    title = "Delayed Reward Collection Success Rate Across Delay Windows",
    x = "Delay Duration (seconds)",
    y = "Mean Collection Success Rate (%)"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 13, hjust = 0.5)
  )

ggsave("delay_window_success_rate.png", plot = delay_window_plot, width = 8, height = 6, dpi = 300, units = "in")

collect_cols <- c(
  "collect_dr_5_l",  "collect_dr_10_l", "collect_dr_20_l", "collect_dr_40_l", "collect_dr_80_l",
  "collect_dr_5_r",  "collect_dr_10_r", "collect_dr_20_r", "collect_dr_40_r", "collect_dr_80_r"
)

timec_cols <- c(
  "timec_dr_5_l",  "timec_dr_10_l", "timec_dr_20_l", "timec_dr_40_l", "timec_dr_80_l",
  "timec_dr_5_r",  "timec_dr_10_r", "timec_dr_20_r", "timec_dr_40_r", "timec_dr_80_r"
)

CERZ_collectperform_CSI <- CERZ_selfcontrol %>%
  select(
    file_id, monkey_id, sex, age, session, trial, trial_start,
    invalid_trial,
    all_of(collect_cols),
    all_of(timec_cols)
  ) %>%
  mutate(
    collected_dr = if_else(
      rowSums(across(all_of(collect_cols), ~ .x == "Y"), na.rm = TRUE) > 0,
      "Y", NA_character_
    ),
    delay_duration = case_when(
      collect_dr_5_l  == "Y" | collect_dr_5_r  == "Y" ~ 5,
      collect_dr_10_l == "Y" | collect_dr_10_r == "Y" ~ 10,
      collect_dr_20_l == "Y" | collect_dr_20_r == "Y" ~ 20,
      collect_dr_40_l == "Y" | collect_dr_40_r == "Y" ~ 40,
      collect_dr_80_l == "Y" | collect_dr_80_r == "Y" ~ 80,
      TRUE ~ NA_real_
    )
  )
CERZ_collectperform_CSI <- CERZ_collectperform_CSI %>%
  filter(collected_dr == "Y")

CERZ_collectperform_CSI <- CERZ_collectperform_CSI %>%
  filter(collected_dr == "Y") %>%
  mutate(
    sex = as.factor(sex),
    trial_start = as.numeric(trial_start),
    across(all_of(timec_cols), ~ as.numeric(.x))
  )

CERZ_CSI <- read_excel("CERZ_final_CSI.xlsx")

cerz_csi_long <- CERZ_CSI %>%
  select(focal_id1, focal_id2, z_CSI) %>%
  pivot_longer(cols = c(focal_id1, focal_id2),
               names_to = "position",
               values_to = "monkey_id") %>%
  group_by(monkey_id) %>%
  summarise(count_above_zero_zCSI = sum(z_CSI > 0, na.rm = TRUE))

print(cerz_csi_long)

tested_CERZ <- c("QUANNY", "BUNTA", "BROW", "KAOS", "MAKASSAR", "MALI", "MONK")

cerz_csi_predictor <- cerz_csi_long %>%
  filter(monkey_id %in% tested_CERZ)

print(cerz_csi_predictor)

CERZ_collectperform_CSI <- CERZ_collectperform_CSI %>%
  left_join(cerz_csi_predictor, by = "monkey_id")

CERZ_ELO <- read_excel("testtable.xlsx")

CERZ_ELO <- CERZ_ELO %>%
  rename(monkey_id = Var1, ELO = Freq) %>%
  mutate(ELO_z = as.numeric(scale(ELO)))

print(CERZ_ELO)

CERZ_collectperform_CSI <- CERZ_collectperform_CSI %>%
  left_join(CERZ_ELO %>% select(monkey_id, ELO_z), by = "monkey_id")

CERZ_collectperform_CSI <- CERZ_collectperform_CSI %>%
  group_by(monkey_id) %>%
  summarise(
    n_success = n(),
    n_trials = 40,
    sex = first(sex),
    age = first(age),
    count_above_zero_zCSI = first(count_above_zero_zCSI),
    ELO_z = first(ELO_z)
  )

EMER_collectperform_CSI <- EMER_selfcontrol %>%
  select(
    file_id, monkey_id, sex, age, session, trial, trial_start,
    invalid_trial,
    all_of(collect_cols),
    all_of(timec_cols)
  ) %>%
  mutate(
    collected_dr = if_else(
      rowSums(across(all_of(collect_cols), ~ .x == "Y"), na.rm = TRUE) > 0,
      "Y", NA_character_
    ),
    delay_duration = case_when(
      collect_dr_5_l  == "Y" | collect_dr_5_r  == "Y" ~ 5,
      collect_dr_10_l == "Y" | collect_dr_10_r == "Y" ~ 10,
      collect_dr_20_l == "Y" | collect_dr_20_r == "Y" ~ 20,
      collect_dr_40_l == "Y" | collect_dr_40_r == "Y" ~ 40,
      collect_dr_80_l == "Y" | collect_dr_80_r == "Y" ~ 80,
      TRUE ~ NA_real_
    )
  )

EMER_collectperform_CSI <- EMER_collectperform_CSI %>%
  filter(collected_dr == "Y") %>%
  mutate(
    sex = as.factor(sex),
    trial_start = as.numeric(trial_start),
    across(all_of(timec_cols), ~ as.numeric(.x))
  )

EMER_CSI <- read_excel("EMER_final_CSI.xlsx")

emer_csi_long <- EMER_CSI %>%
  select(focal_id1, focal_id2, z_CSI) %>%
  pivot_longer(cols = c(focal_id1, focal_id2),
               names_to = "position",
               values_to = "monkey_id") %>%
  group_by(monkey_id) %>%
  summarise(count_above_zero_zCSI = sum(z_CSI > 0, na.rm = TRUE))

print(emer_csi_long)

tested_EMER <- c("BASUKI", "DOUGIE", "DRUSILLA", "EKAH", "INDAH", "MASAMBA", "KERANA")

emer_csi_predictor <- emer_csi_long %>%
  filter(monkey_id %in% tested_EMER)

print(emer_csi_predictor)

EMER_collectperform_CSI <- EMER_collectperform_CSI %>%
  left_join(emer_csi_predictor, by = "monkey_id")

EMER_ELO <- read_excel("testtableEMER.xlsx")

EMER_ELO <- EMER_ELO %>%
  rename(monkey_id = Var1, ELO = Freq) %>%
  mutate(ELO_z = as.numeric(scale(ELO)))

print(EMER_ELO)

EMER_collectperform_CSI <- EMER_collectperform_CSI %>%
  left_join(EMER_ELO %>% select(monkey_id, ELO_z), by = "monkey_id")

EMER_collectperform_CSI <- EMER_collectperform_CSI %>%
  group_by(monkey_id) %>%
  summarise(
    n_success = n(),
    n_trials = if_else(first(monkey_id) == "MASAMBA", 20, 40),
    sex = first(sex),
    age = first(age),
    count_above_zero_zCSI = first(count_above_zero_zCSI),
    ELO_z = first(ELO_z)
  )

CERZ_EMER_collectperform_CSI <- bind_rows(
  CERZ_collectperform_CSI %>% mutate(site = "CERZ"),
  EMER_collectperform_CSI %>% mutate(site = "EMER")
) %>%
  mutate(site = as.factor(site))

CERZ_EMER_collectperform_CSI <- CERZ_EMER_collectperform_CSI %>%
  rename(group = site)

cat("Total rows:", nrow(CERZ_EMER_collectperform_CSI), "\n")
cat("Total successes:", sum(CERZ_EMER_collectperform_CSI$n_success), "\n")
cat("Sites:", levels(CERZ_EMER_collectperform_CSI$site), "\n")

CERZ_centrality <- read_excel("CERZ_centrality_scores.xlsx")

EMER_centrality <- read_excel("EMER_centrality_scores.xlsx")

cerz_eigen_lookup <- setNames(CERZ_centrality$eigenvector, CERZ_centrality$individual)
emer_eigen_lookup <- setNames(EMER_centrality$eigenvector, EMER_centrality$individual)

CERZ_EMER_CSI_eigen <- CERZ_EMER_collectperform_CSI %>%
  mutate(
    eigenvector = case_when(
      group == "CERZ" ~ cerz_eigen_lookup[monkey_id],
      group == "EMER" ~ emer_eigen_lookup[monkey_id],
      TRUE ~ NA_real_
    ),
    z_eigenvector = as.numeric(scale(eigenvector))
  )

CERZ_EMER_CSI_eigen %>%
  summarise(r = cor(count_above_zero_zCSI, z_eigenvector))

collectionsuccess_model_combined <- brm(
  n_success | trials(n_trials) ~ count_above_zero_zCSI + z_eigenvector + (1 | monkey_id),
  data = CERZ_EMER_CSI_eigen,
  family = binomial(link = "logit"),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 0.5), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  save_pars = save_pars(all = TRUE),
  control = list(adapt_delta = 0.95)
)

summary(collectionsuccess_model_combined)

collectionsuccess_combined_null <- brm(
  n_success | trials(n_trials) ~ 1 + (1 | monkey_id),
  data = CERZ_EMER_CSI_eigen,
  family = binomial(link = "logit"),
  prior = c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  save_pars = save_pars(all = TRUE)
)

loo_combined <- loo(collectionsuccess_model_combined, moment_match = TRUE)
loo_combined_null <- loo(collectionsuccess_combined_null, moment_match = TRUE)

loo_compare(loo_combined_null, loo_combined)

pp_check(collectionsuccess_model_combined, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated (Combined Model)",
       x = "Collection Success", y = "Density") +
  theme_gray()

dens_plot <- pp_check(collectionsuccess_model_combined, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated (Combined Model)",
       x = "Collection Success", y = "Density") +
  theme_gray()
ggsave("PPC_dens_overlay_combined.png", plot = dens_plot, width = 8, height = 6, dpi = 300, units = "in")

pp_check(collectionsuccess_model_combined, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Collection Success (Combined Model)",
       x = "Mean", y = "Frequency") +
  theme_gray()

mean_plot <- pp_check(collectionsuccess_model_combined, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Collection Success (Combined Model)",
       x = "Mean", y = "Frequency") +
  theme_gray()
ggsave("PPC_mean_combined.png", plot = mean_plot, width = 8, height = 6, dpi = 300, units = "in")

random_effects_combined <- collectionsuccess_model_combined %>%
  spread_draws(r_monkey_id[monkey_id, term]) %>%
  filter(term == "Intercept") %>%
  median_qi(.width = c(0.95))

group_lookup <- CERZ_EMER_CSI_eigen %>%
  select(monkey_id, group)

random_effects_combined <- random_effects_combined %>%
  left_join(group_lookup, by = "monkey_id")

ggplot(random_effects_combined,
       aes(x = r_monkey_id,
           y = reorder(monkey_id, r_monkey_id),
           colour = group)) +
  geom_pointrange(aes(xmin = .lower, xmax = .upper), size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("CERZ" = "steelblue", "EMER" = "darkorange")) +
  labs(
    title = "Individual Variation in Delayed Reward Collection Success",
    subtitle = "Random effects from combined CSI + eigenvector model",
    x = "Deviation from Group Mean (log-odds)",
    y = "Individual",
    colour = "Group"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  )

forest_plot_success_collection <- ggplot(random_effects_combined,
       aes(x = r_monkey_id,
           y = reorder(monkey_id, r_monkey_id),
           colour = group)) +
  geom_pointrange(aes(xmin = .lower, xmax = .upper), size = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = c("CERZ" = "steelblue", "EMER" = "darkorange")) +
  labs(
    title = "Individual Variation in Delayed Reward Collection Success",
    subtitle = "Random effects from combined CSI + eigenvector model",
    x = "Deviation from Group Mean (log-odds)",
    y = "Individual",
    colour = "Group"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right"
  )

ggsave("forest_plot_success_collection.png", plot = forest_plot_success_collection, width = 8, height = 6, dpi = 300, units = "in")

CERZ_abandon_latency <- CERZ_selfcontrol %>%
  filter(is.na(invalid_trial)) %>%
  mutate(row_idx = row_number()) %>%
  mutate(
    event_type = case_when(
      !is.na(pull_dr_5_l)  | !is.na(pull_dr_10_l) | !is.na(pull_dr_20_l) |
      !is.na(pull_dr_5_r)  | !is.na(pull_dr_10_r) | !is.na(pull_dr_20_r)  ~ "pull_dr",
      !is.na(pull_ir_l)    | !is.na(pull_ir_r)                             ~ "pull_ir",
      !is.na(monkey_present_start)                                          ~ "monkey_present",
      !is.na(walk_away_start)                                               ~ "walk_away",
      !is.na(walk_away_mp_start)                                            ~ "walk_away_mp",
      TRUE ~ "other"
    ),
    event_time = case_when(
      event_type == "pull_dr" ~ coalesce(
        time_dr_5_l, time_dr_10_l, time_dr_20_l,
        time_dr_5_r, time_dr_10_r, time_dr_20_r
      ),
      event_type == "pull_ir"        ~ coalesce(pull_ir_l_time, pull_ir_r_time),
      event_type == "monkey_present" ~ monkey_present_start,
      event_type == "walk_away"      ~ walk_away_start,
      event_type == "walk_away_mp"   ~ walk_away_mp_start
    ),
    delay_window_raw = case_when(
      !is.na(pull_dr_5_l)  | !is.na(pull_dr_5_r)  ~ 5,
      !is.na(pull_dr_10_l) | !is.na(pull_dr_10_r) ~ 10,
      !is.na(pull_dr_20_l) | !is.na(pull_dr_20_r) ~ 20
    ),
    was_collected = case_when(
      !is.na(collect_dr_5_l)  | !is.na(collect_dr_5_r)  ~ TRUE,
      !is.na(collect_dr_10_l) | !is.na(collect_dr_10_r) ~ TRUE,
      !is.na(collect_dr_20_l) | !is.na(collect_dr_20_r) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(event_type %in% c("pull_dr", "pull_ir", "monkey_present",
                            "walk_away", "walk_away_mp")) %>%
  filter(!is.na(event_time)) %>%
  arrange(monkey_id, session, trial, event_time) %>%
  group_by(monkey_id, session, trial) %>%
  mutate(
    is_interruption = event_type %in% c("monkey_present", "walk_away", "walk_away_mp"),
    is_pull_dr      = event_type == "pull_dr" & !was_collected,
    is_pull_ir      = event_type == "pull_ir"
  ) %>%
  filter(any(is_pull_dr) & any(is_pull_ir)) %>%
  mutate(pull_ir_time = min(event_time[is_pull_ir])) %>%
  filter(event_time <= pull_ir_time) %>%
  mutate(last_pull_dr_time = max(event_time[is_pull_dr], na.rm = TRUE)) %>%
  mutate(
    interruption_after_last_dr = any(
      is_interruption &
      event_time > last_pull_dr_time &
      event_time < pull_ir_time
    )
  ) %>%
  filter(!interruption_after_last_dr) %>%
  filter(is_pull_dr & event_time == last_pull_dr_time) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    abandonment_latency = pull_ir_time - last_pull_dr_time,
    session = as.numeric(session)
  ) %>%
  filter(abandonment_latency > 0) %>%
  select(
    file_id, monkey_id, sex, session, trial,
    delay_window = delay_window_raw,
    last_pull_dr_time, pull_ir_time, abandonment_latency
  )

CERZ_abandon_latency <- CERZ_abandon_latency %>%
  mutate(abandonment_latency = ifelse(
    monkey_id == "BROW" & session == 2 & trial == 10,
    10,
    abandonment_latency
  ))

EMER_abandon_latency <- EMER_selfcontrol %>%
  filter(is.na(invalid_trial)) %>%
  mutate(row_idx = row_number()) %>%
  mutate(
    event_type = case_when(
      !is.na(pull_dr_5_l)  | !is.na(pull_dr_10_l) | !is.na(pull_dr_20_l) |
      !is.na(pull_dr_5_r)  | !is.na(pull_dr_10_r) | !is.na(pull_dr_20_r)  ~ "pull_dr",
      !is.na(pull_ir_l)    | !is.na(pull_ir_r)                             ~ "pull_ir",
      !is.na(monkey_present_start)                                          ~ "monkey_present",
      !is.na(walk_away_start)                                               ~ "walk_away",
      !is.na(walk_away_mp_start)                                            ~ "walk_away_mp",
      TRUE ~ "other"
    ),
    event_time = case_when(
      event_type == "pull_dr" ~ coalesce(
        time_dr_5_l, time_dr_10_l, time_dr_20_l,
        time_dr_5_r, time_dr_10_r, time_dr_20_r
      ),
      event_type == "pull_ir"        ~ coalesce(pull_ir_l_time, pull_ir_r_time),
      event_type == "monkey_present" ~ monkey_present_start,
      event_type == "walk_away"      ~ walk_away_start,
      event_type == "walk_away_mp"   ~ walk_away_mp_start
    ),
    delay_window_raw = case_when(
      !is.na(pull_dr_5_l)  | !is.na(pull_dr_5_r)  ~ 5,
      !is.na(pull_dr_10_l) | !is.na(pull_dr_10_r) ~ 10,
      !is.na(pull_dr_20_l) | !is.na(pull_dr_20_r) ~ 20
    ),
    was_collected = case_when(
      !is.na(collect_dr_5_l)  | !is.na(collect_dr_5_r)  ~ TRUE,
      !is.na(collect_dr_10_l) | !is.na(collect_dr_10_r) ~ TRUE,
      !is.na(collect_dr_20_l) | !is.na(collect_dr_20_r) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(event_type %in% c("pull_dr", "pull_ir", "monkey_present",
                            "walk_away", "walk_away_mp")) %>%
  filter(!is.na(event_time)) %>%
  arrange(monkey_id, session, trial, event_time) %>%
  group_by(monkey_id, session, trial) %>%
  mutate(
    is_interruption = event_type %in% c("monkey_present", "walk_away", "walk_away_mp"),
    is_pull_dr      = event_type == "pull_dr" & !was_collected,
    is_pull_ir      = event_type == "pull_ir"
  ) %>%
  filter(any(is_pull_dr) & any(is_pull_ir)) %>%
  mutate(pull_ir_time = min(event_time[is_pull_ir])) %>%
  filter(event_time <= pull_ir_time) %>%
  mutate(last_pull_dr_time = max(event_time[is_pull_dr], na.rm = TRUE)) %>%
  mutate(
    interruption_after_last_dr = any(
      is_interruption &
      event_time > last_pull_dr_time &
      event_time < pull_ir_time
    )
  ) %>%
  filter(!interruption_after_last_dr) %>%
  filter(is_pull_dr & event_time == last_pull_dr_time) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    abandonment_latency = pull_ir_time - last_pull_dr_time,
    session = as.numeric(session)
  ) %>%
  filter(abandonment_latency > 0) %>%
  select(
    file_id, monkey_id, sex, session, trial,
    delay_window = delay_window_raw,
    last_pull_dr_time, pull_ir_time, abandonment_latency
  )

EMER_abandon_latency <- EMER_abandon_latency %>%
  mutate(abandonment_latency = case_when(
    monkey_id == "DOUGIE" & session == 1 & trial == 7  ~ 10,
    monkey_id == "INDAH"  & session == 4 & trial == 7  ~ 10,
    monkey_id == "EKAH"   & session == 2 & trial == 5  ~ 20,
    TRUE ~ abandonment_latency
  ))

CERZ_EMER_abandon_latency_comb <- bind_rows(CERZ_abandon_latency, EMER_abandon_latency)

CERZ_EMER_abandon_latency_comb %>%
  mutate(delay_window = factor(delay_window,
                                levels = c(5, 10, 20),
                                labels = c("5s", "10s", "20s"))) %>%
  ggplot(aes(x = abandonment_latency)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "white") +
  geom_density(aes(y = after_stat(count)), colour = "darkred", linewidth = 0.8) +
  facet_wrap(~ delay_window, scales = "free_x") +
  labs(
    x = "Abandonment Latency (seconds)",
    y = "Count",
    title = "Distribution of Abandonment Latency by Delay Window"
  ) +
  theme_minimal()

CERZ_EMER_abandon_latency_comb %>%
  mutate(log_latency = log(abandonment_latency),
         delay_window = factor(delay_window, levels = c(5, 10, 20),
                               labels = c("5s", "10s", "20s"))) %>%
  ggplot(aes(x = log_latency)) +
  geom_histogram(binwidth = 0.3, fill = "steelblue", colour = "white") +
  facet_wrap(~ delay_window, scales = "free_x") +
  labs(x = "Log Abandonment Latency", y = "Count",
       title = "Log-transformed Distribution by Delay Window") +
  theme_minimal()

CERZ_EMER_abandon_latency_comb %>%
  summarise(
    total = n(),
    under_1s = sum(abandonment_latency < 1),
    prop_under_1s = mean(abandonment_latency < 1)
  )

CERZ_EMER_abandon_latency_comb %>%
  group_by(delay_window) %>%
  summarise(
    n = n(),
    mean = mean(abandonment_latency),
    sd = sd(abandonment_latency),
    median = median(abandonment_latency),
    min = min(abandonment_latency),
    max = max(abandonment_latency)
  )

CERZ_EMER_abandon_latency_comb <- CERZ_EMER_abandon_latency_comb %>%
  mutate(delay_window = factor(delay_window, levels = c(5, 10, 20)))

model_abandon <- brm(
  abandonment_latency ~ session + delay_window + session:delay_window + (1 | monkey_id),
  data = CERZ_EMER_abandon_latency_comb,
  family = Gamma(link = "log"),
  prior = c(
    prior(normal(0, 1.5), class = Intercept),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sd)
  ),
  chains = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE),
  seed = 123
)

summary(model_abandon)

session_coef <- -0.27
exp(session_coef)
(exp(session_coef) - 1) * 100

model_abandon_null <- brm(
  abandonment_latency ~ 1 + (1 | monkey_id),
  data = CERZ_EMER_abandon_latency_comb,
  family = Gamma(link = "log"),
  prior = c(
    prior(normal(0, 1.5), class = Intercept),
    prior(exponential(1), class = sd)
  ),
  chains = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE),
  seed = 123
)

loo_full_abandon <- loo(model_abandon, moment_match = TRUE)
loo_null_abandon <- loo(model_abandon_null, moment_match = TRUE)
loo_compare(loo_full_abandon, loo_null_abandon)

pp_density_abandon <- pp_check(model_abandon, ndraws = 100, type = "dens_overlay") +
  coord_cartesian(xlim = c(0, 25)) +
  scale_color_manual(
    values = c("y" = "#2C3E50", "yrep" = "#3498DB"),
    labels = c("y" = "Observed", "yrep" = "Predicted")
  ) +
  labs(
    title = "Posterior Predictive Check: Density Overlay",
    x = "Abandonment Latency (seconds)",
    y = "Density",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

pp_density_abandon

ggsave(
  filename = "pp_density_abandon.png",
  plot = pp_density_abandon,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

pp_mean_abandon <- pp_check(model_abandon, ndraws = 100, type = "stat", stat = "mean") +
  scale_color_manual(
    values = c("y" = "#2C3E50", "yrep" = "#3498DB"),
    labels = c("y" = "Observed", "yrep" = "Predicted")
  ) +
  labs(
    title = "Posterior Predictive Check: Mean",
    x = "Mean Abandonment Latency (seconds)",
    y = "Count",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

pp_mean_abandon

ggsave(
  filename = "pp_mean_abandon.png",
  plot = pp_mean_abandon,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

interaction_effects_abandon <- conditional_effects(
  model_abandon,
  effects = "session:delay_window",
  int_conditions = list(session = c(1, 2, 3, 4))
)

interaction_df <- as.data.frame(interaction_effects_abandon$`session:delay_window`)

interaction_plot <- ggplot(interaction_df, aes(x = session, y = estimate__,
                            colour = delay_window,
                            fill = delay_window)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__),
              alpha = 0.15, colour = NA) +
  scale_x_continuous(breaks = c(1, 2, 3, 4),
                     labels = c("1", "2", "3", "4")) +
  scale_colour_manual(
    values = c("5" = "#2C3E50", "10" = "#2980B9", "20" = "#27AE60"),
    labels = c("5" = "5 seconds", "10" = "10 seconds", "20" = "20 seconds")
  ) +
  scale_fill_manual(
    values = c("5" = "#2C3E50", "10" = "#2980B9", "20" = "#27AE60"),
    labels = c("5" = "5 seconds", "10" = "10 seconds", "20" = "20 seconds")
  ) +
  labs(
    x = "Session",
    y = "Predicted Abandonment Latency (seconds)",
    colour = "Delay Window",
    fill = "Delay Window",
    title = "Predicted Abandonment Latency Across Sessions by Delay Window"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )
interaction_plot

ggsave(
  filename = "interaction_plot_abandonment.png",
  plot = interaction_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

ranef_df <- ranef(model_abandon, summary = TRUE)$monkey_id %>%
  as.data.frame() %>%
  rownames_to_column("monkey_id") %>%
  rename(
    estimate = Estimate.Intercept,
    lower = Q2.5.Intercept,
    upper = Q97.5.Intercept
  )

ranef_plot_abandon <- ggplot(ranef_df, aes(x = reorder(monkey_id, estimate),
                     y = estimate,
                     ymin = lower,
                     ymax = upper)) +
  geom_pointrange(colour = "#2980B9", size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "#E74C3C", linewidth = 0.8) +
  coord_flip() +
  labs(
    x = "Individual",
    y = "Random Effect Estimate (log scale)",
    title = "Individual Variation in Baseline Abandonment Latency"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ranef_plot_abandon

ggsave(
  filename = "ranef_plot_abandon.png",
  plot = ranef_plot_abandon,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

CERZ_CSI_abandon <- CERZ_CSI %>%
  select(focal_id1, focal_id2, z_CSI) %>%
  pivot_longer(cols = c(focal_id1, focal_id2),
               names_to = "position",
               values_to = "monkey_id") %>%
  group_by(monkey_id) %>%
  summarise(count_above_zero_zCSI = sum(z_CSI > 0, na.rm = TRUE))

EMER_CSI_abandon <- EMER_CSI %>%
  select(focal_id1, focal_id2, z_CSI) %>%
  pivot_longer(cols = c(focal_id1, focal_id2),
               names_to = "position",
               values_to = "monkey_id") %>%
  group_by(monkey_id) %>%
  summarise(count_above_zero_zCSI = sum(z_CSI > 0, na.rm = TRUE))

CSI_abandon_combined <- bind_rows(CERZ_CSI_abandon, EMER_CSI_abandon)

CERZ_EMER_abandon_latency_comb <- CERZ_EMER_abandon_latency_comb %>%
  left_join(CSI_abandon_combined, by = "monkey_id")

CERZ_EMER_abandon_latency_comb <- CERZ_EMER_abandon_latency_comb %>%
  mutate(
    z_bond_count = as.numeric(scale(count_above_zero_zCSI)),
    sex = factor(sex)
  )

model2_abandon_sex_csi <- brm(
  abandonment_latency ~ session + delay_window + session:delay_window +
    z_bond_count + sex + (1 | monkey_id),
  data = CERZ_EMER_abandon_latency_comb,
  family = Gamma(link = "log"),
  prior = c(
    prior(normal(0, 1.5), class = Intercept),
    prior(normal(0, 0.5), class = b),
    prior(exponential(1), class = sd)
  ),
  chains = 4, iter = 4000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE),
  seed = 123
)

summary(model2_abandon_sex_csi)

loo_full_abandon2 <- loo(model2_abandon_sex_csi, moment_match = TRUE)

loo_compare(loo_full_abandon, loo_full_abandon2)
