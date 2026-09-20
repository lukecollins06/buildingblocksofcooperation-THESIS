# Packages
library(dplyr)
library(tidyverse)
library(writexl)
library(readxl)
library(openxlsx)
library(EloRating)
library(performance)
library(DHARMa)
library(glmmTMB)
library(survival)
library(ggplot2)
library(survminer)
library(muhaz)
library(flexsurv)
library(moments)
library(fitdistrplus)
library(brms)

socialbondrawdataNEW <- read_excel(
  path = "socialbondrawdataNEW.xlsx",
  sheet = 1,
  skip = 0,
  col_types = "guess"
)
saveRDS(socialbondrawdataNEW, "socialbondrawdataNEW.rds")

CSI <- socialbondrawdataNEW[, c("file_id", "date", "focal_id", "sex", "age",
                          "groom_give_id", "groom_give_start", "groom_give_end", "groom_give_dur_total",
                          "groom_recv_id", "groom_recv_start", "groom_recv_end", "groom_recv_dur_total",
                          "affcont_give_id", "affcont_give_type", "affcont_give_time",
                          "affcont_recv_id", "affcont_recv_type", "recv_affcont_time",
                          "proximity_id", "proximity_time", "proximity_bc", "proximity_1", "proximity_5",
                          "out_view_start", "out_view_end", "out_view_total")]
CSI_CERZ <- CSI %>% filter(grepl("CERZ", file_id))
str(CSI_CERZ)

CSI_CERZ$groom_give_dur_total[CSI_CERZ$groom_give_dur_total == "NA"] <- NA
str(CSI_CERZ$groom_give_dur_total)
summary(CSI_CERZ$groom_give_dur_total)
convert_time_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)
  parts <- strsplit(time_str, ":")[[1]]
  if(length(parts) != 2) return(NA)
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  if(is.na(minutes) || is.na(seconds)) return(NA)
  return(minutes * 60 + seconds)
}
CSI_CERZ$groom_give_dur_total_sec <- sapply(CSI_CERZ$groom_give_dur_total, convert_time_to_seconds)

mean(CSI_CERZ$groom_give_dur_total_sec, na.rm = TRUE)
sd(CSI_CERZ$groom_give_dur_total_sec, na.rm = TRUE)
max(CSI_CERZ$groom_give_dur_total_sec, na.rm = TRUE)
min(CSI_CERZ$groom_give_dur_total_sec, na.rm = TRUE)

hist(CSI_CERZ$groom_give_dur_total_sec,
     main = "Histogram of Groom Give Duration (seconds)",
     xlab = "Duration (seconds)",
     col = "skyblue",
     breaks = 50)

CSI_CERZ$groom_recv_dur_total[CSI_CERZ$groom_recv_dur_total == "NA"] <- NA
CSI_CERZ$groom_recv_dur_total_sec <- sapply(CSI_CERZ$groom_recv_dur_total, convert_time_to_seconds)

mean(CSI_CERZ$groom_recv_dur_total_sec, na.rm = TRUE)
sd(CSI_CERZ$groom_recv_dur_total_sec, na.rm = TRUE)
max(CSI_CERZ$groom_recv_dur_total_sec, na.rm = TRUE)
min(CSI_CERZ$groom_recv_dur_total_sec, na.rm = TRUE)

hist(CSI_CERZ$groom_recv_dur_total_sec,
     main = "Histogram of Groom Recv Duration (seconds)",
     xlab = "Duration (seconds)",
     col = "skyblue",
     breaks = 50)

focal_obs <- CSI_CERZ %>%
  distinct(focal_id, file_id) %>%
  group_by(focal_id) %>%
  summarise(obs_count = n(), .groups = "drop") %>%
  mutate(
    total_seconds = obs_count * 30 * 60,
    total_time = sprintf("%02d:%02d:%02d",
                         total_seconds %/% 3600,
                         (total_seconds %% 3600) %/% 60,
                         total_seconds %% 60)
  )
print(focal_obs)

dyads <- expand.grid(focal_id1 = focal_obs$focal_id, focal_id2 = focal_obs$focal_id,
                     stringsAsFactors = FALSE) %>%
  filter(focal_id1 < focal_id2)

dyads <- dyads %>%
  left_join(focal_obs %>% dplyr::select(focal_id, total_seconds), by = c("focal_id1" = "focal_id")) %>%
  rename(time1 = total_seconds) %>%
  left_join(focal_obs %>% dplyr::select(focal_id, total_seconds), by = c("focal_id2" = "focal_id")) %>%
  rename(time2 = total_seconds) %>%
  mutate(
    total_seconds = time1 + time2,
    total_time = sprintf("%02d:%02d:%02d",
                         total_seconds %/% 3600,
                         (total_seconds %% 3600) %/% 60,
                         total_seconds %% 60),
    dyad = paste(focal_id1, focal_id2, sep = "_")
  ) %>%
  arrange(focal_id1, focal_id2)

print(dyads)

focal_obs_summary_CERZ <- focal_obs %>%
  summarise(
    n_individuals     = n(),
    total_obs_count   = sum(obs_count),
    total_hours       = sum(total_seconds) / 3600,
    avg_obs_per_indiv = mean(obs_count),
    avg_hours_per_indiv = mean(total_seconds / 3600)
  )

cat("=== CERZ ===\n")
print(focal_obs_summary_CERZ)
print(focal_obs[, c("focal_id", "obs_count", "total_time")])

time_columns <- c("groom_give_start", "groom_give_end", "groom_give_dur_total",
                  "groom_recv_start", "groom_recv_end", "groom_recv_dur_total",
                  "affcont_give_time", "recv_affcont_time", "proximity_time",
                  "out_view_start", "out_view_end", "out_view_total")
CSI_CERZ <- CSI_CERZ[!grepl("24", format(CSI_CERZ$date, "%y")), ]

CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  as.character(col)
})
convert_to_time_format <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  if(grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    return(time_str)
  }

  return(NA)
}
CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  sapply(col, convert_to_time_format, USE.NAMES = FALSE)
})

head(CSI_CERZ[time_columns], 20)

CSI_CERZ$date <- as.Date(CSI_CERZ$date, format = "%Y-%m-%d")
str(CSI_CERZ$date)

valid_names <- c("MALI", "TALIA", "FIDGET", "KAOS", "SULA", "QUANNY", "SUNDA", "BUNTA", "BROW", "MAKASSAR", "MONK")
valid_types <- c("T", "E", "H", "L")
valid_proximity <- c("Y")

clean_column <- function(column, valid_values) {
  return(ifelse(column %in% valid_values, column, NA))
}

CSI_CERZ$groom_give_id <- clean_column(CSI_CERZ$groom_give_id, valid_names)
CSI_CERZ$groom_recv_id <- clean_column(CSI_CERZ$groom_recv_id, valid_names)
CSI_CERZ$affcont_give_id <- clean_column(CSI_CERZ$affcont_give_id, valid_names)
CSI_CERZ$affcont_give_type <- clean_column(CSI_CERZ$affcont_give_type, valid_types)
CSI_CERZ$affcont_recv_id <- clean_column(CSI_CERZ$affcont_recv_id, valid_names)
CSI_CERZ$affcont_recv_type <- clean_column(CSI_CERZ$affcont_recv_type, valid_types)
CSI_CERZ$proximity_id <- clean_column(CSI_CERZ$proximity_id, valid_names)
CSI_CERZ$proximity_bc <- clean_column(CSI_CERZ$proximity_bc, valid_proximity)
CSI_CERZ$proximity_1 <- clean_column(CSI_CERZ$proximity_1, valid_proximity)
CSI_CERZ$proximity_5 <- clean_column(CSI_CERZ$proximity_5, valid_proximity)

groom_freq_long <- CSI_CERZ %>%
  pivot_longer(
    cols = c(groom_give_id, groom_recv_id),
    names_to = "direction",
    values_to = "partner"
  ) %>%
  filter(!is.na(partner))

groom_freq_summary <- groom_freq_long %>%
  group_by(focal_id, partner) %>%
  summarise(freq = n(), .groups = "drop")

groom_freq_matrix <- xtabs(freq ~ focal_id + partner, data = groom_freq_summary)

cat("\nTotal Grooming Interactions per Focal:\n")
print(rowSums(groom_freq_matrix))

cat("Grooming Interaction Frequency Matrix:\n")
print(groom_freq_matrix)

quanny_sula_groom_count <- sum(
  CSI_CERZ$focal_id == "QUANNY" &
    (CSI_CERZ$groom_give_id == "SULA" | CSI_CERZ$groom_recv_id == "SULA"),
  na.rm = TRUE
)
print(quanny_sula_groom_count)

grooming_data <- CSI_CERZ %>%
  select(focal_id, groom_give_id, groom_recv_id) %>%
  filter(!is.na(focal_id) & (!is.na(groom_give_id) | !is.na(groom_recv_id)))

groom_given <- grooming_data %>%
  filter(!is.na(groom_give_id)) %>%
  group_by(focal_id, partner = groom_give_id) %>%
  summarise(Groom_Given = n(), .groups = "drop")

groom_received <- grooming_data %>%
  filter(!is.na(groom_recv_id)) %>%
  group_by(focal_id, partner = groom_recv_id) %>%
  summarise(Groom_Received = n(), .groups = "drop")

grooming_breakdown <- full_join(groom_given, groom_received, by = c("focal_id", "partner")) %>%
  mutate(
    Groom_Given = replace_na(Groom_Given, 0),
    Groom_Received = replace_na(Groom_Received, 0)
  ) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "-")) %>%
  ungroup() %>%
  arrange(dyad, focal_id) %>%
  select(-dyad)

print(grooming_breakdown, n = Inf)
View(grooming_breakdown)

grooming_breakdown_aggregated <- grooming_breakdown %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner),
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    Groom_Given    = sum(Groom_Given, na.rm = TRUE),
    Groom_Received = sum(Groom_Received, na.rm = TRUE),
    Total_Groom    = Groom_Given + Groom_Received,
    .groups = "drop"
  ) %>%
  tidyr::separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  select(dyad_id, indiv1, indiv2, Groom_Given, Groom_Received, Total_Groom) %>%
  arrange(dyad_id)

View(grooming_breakdown_aggregated)

sum(grooming_breakdown_aggregated$Total_Groom)

grooming_breakdown_aggregated_episperhour <- grooming_breakdown_aggregated %>%
  left_join(
    dyads %>% select(dyad, total_seconds),
    by = c("dyad_id" = "dyad")
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Total_Groom / total_hours
  )

grooming_breakdown_aggregated_episperhour %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Total_Groom),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 2)
  )

grooming_breakdown_aggregated %>%
  filter(Total_Groom <= 1) %>%
  select(dyad_id, Total_Groom) %>%
  arrange(Total_Groom, dyad_id) %>%
  print(n = Inf)

groom_given_received_summary <- grooming_data %>%
  filter(!is.na(groom_give_id)) %>%
  count(focal_id, name = "Groom_Given") %>%

  full_join(
    grooming_data %>%
      filter(!is.na(groom_recv_id)) %>%
      count(focal_id, name = "Groom_Received"),
    by = "focal_id"
  ) %>%

  mutate(
    Groom_Given     = replace_na(Groom_Given, 0),
    Groom_Received  = replace_na(Groom_Received, 0),
    Total_Grooming  = Groom_Given + Groom_Received
  ) %>%

  arrange(desc(Total_Grooming)) %>%

  select(monkey_id = focal_id, Groom_Given, Groom_Received, Total_Grooming)

View(groom_given_received_summary)

write_xlsx(groom_given_received_summary,
           path = "groom_given_received_summary_CERZ.xlsx")

time_columns <- c("groom_give_dur_total", "groom_recv_dur_total")

CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  as.character(col)
})

convert_to_time_format <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    return(time_str)
  }

  return(NA)
}

CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  sapply(col, convert_to_time_format, USE.NAMES = FALSE)
})

invalid_times <- CSI_CERZ$groom_give_dur_total[!is.na(CSI_CERZ$groom_give_dur_total) &
                                                 !grepl("^\\d{2}:\\d{2}\\.\\d{3}$", CSI_CERZ$groom_give_dur_total)]
print(invalid_times)

convert_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  time_str <- trimws(time_str)

  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    parts <- strsplit(time_str, "[:\\.]")[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 1000)
  }

  return(NA)
}

groom_dur_given <- CSI_CERZ %>%
  filter(!is.na(groom_give_id)) %>%
  mutate(duration_sec = sapply(groom_give_dur_total, convert_to_seconds)) %>%
  select(focal_id, partner = groom_give_id, duration_sec)

groom_dur_recv <- CSI_CERZ %>%
  filter(!is.na(groom_recv_id)) %>%
  mutate(duration_sec = sapply(groom_recv_dur_total, convert_to_seconds)) %>%
  select(focal_id, partner = groom_recv_id, duration_sec)

groom_dur_combined <- bind_rows(groom_dur_given, groom_dur_recv)

groom_dur_summary <- groom_dur_combined %>%
  group_by(focal_id, partner) %>%
  summarise(total_duration_sec = sum(duration_sec, na.rm = TRUE), .groups = "drop")

groom_dur_summary %>%
  mutate(
    dyad_id = map2_chr(
      focal_id, partner,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  summarise(
    total_rows        = n(),
    unique_dyads      = n_distinct(dyad_id)
  ) %>%
  print()

groom_dur_matrix <- xtabs(total_duration_sec ~ focal_id + partner, data = groom_dur_summary)

groom_dur_matrix <- groom_dur_matrix + t(groom_dur_matrix)

cat("\nGrooming Duration Matrix (seconds):\n")
print(groom_dur_matrix)

cat("\nTotal Grooming Duration (seconds) per Focal:\n")
print(rowSums(groom_dur_matrix))
View(groom_dur_matrix)

write_xlsx(
  groom_dur_summary,
  path = "CERZ_groom_dur_summary_directed.xlsx"
)

grooming_dur_pfocal <- rowSums(groom_dur_matrix)

grooming_stats <- data.frame(
  mean_min = mean(grooming_dur_pfocal / 60),
  sd_min   = sd(grooming_dur_pfocal / 60),
  n_focals = length(grooming_dur_pfocal)
)

cat("Grooming duration per focal (CERZ):\n")
cat("Mean =", round(grooming_stats$mean_min, 1),
    "±", round(grooming_stats$sd_min, 1),
    "min (N =", grooming_stats$n_focals, "focals)\n")
cat("Grand total =", round(sum(grooming_dur_pfocal)/60, 1), "minutes\n")

format_duration <- function(total_seconds) {
  minutes <- floor(total_seconds / 60)
  secs <- floor(total_seconds %% 60)
  ms <- round((total_seconds %% 1) * 1000)
  sprintf("%02d:%02d:%03d", minutes, secs, ms)
}

formatted_dur_matrix <- apply(groom_dur_matrix, c(1,2), format_duration)

cat("\nGrooming Duration Matrix (formatted mm:ss:ms):\n")
print(formatted_dur_matrix)

formatted_total <- sapply(rowSums(groom_dur_matrix), format_duration)

cat("\nTotal Grooming Duration (formatted mm:ss:ms) per Focal:\n")
print(formatted_total)

total_sec <- sum(sapply(formatted_total, function(t) {
  if (!nzchar(t)) return(0)
  p <- as.numeric(strsplit(t, "[:.]")[[1]])
  p[1] * 60 + p[2] + p[3]/1000
}))

hours   <- total_sec %/% 3600
minutes <- (total_sec %% 3600) %/% 60
seconds <- round(total_sec %% 60)

cat("\nTotal grooming duration across ALL focals (hh:mm:ss):",
    sprintf("%02d:%02d:%02d", hours, minutes, seconds), "\n")

time_columns <- c("groom_give_dur_total", "groom_recv_dur_total")

CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  as.character(col)
})

convert_to_time_format <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    return(time_str)
  }
  return(NA)
}

CSI_CERZ[time_columns] <- lapply(CSI_CERZ[time_columns], function(col) {
  sapply(col, convert_to_time_format, USE.NAMES = FALSE)
})

invalid_times <- CSI_CERZ$groom_give_dur_total[!is.na(CSI_CERZ$groom_give_dur_total) &
                                               !grepl("^\\d{2}:\\d{2}\\.\\d{3}$", CSI_CERZ$groom_give_dur_total)]
print(invalid_times)

convert_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  time_str <- trimws(time_str)
  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    parts <- strsplit(time_str, "[:\\.]")[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 1000)
  }
  return(NA)
}

summary_given <- groom_dur_given %>%
  summarise(
    min_duration = min(duration_sec, na.rm = TRUE),
    max_duration = max(duration_sec, na.rm = TRUE),
    median_duration = median(duration_sec, na.rm = TRUE),
    mean_duration = mean(duration_sec, na.rm = TRUE),
    sd_duration = sd(duration_sec, na.rm = TRUE)
  )

cat("\nGrooming GIVEN - Summary (seconds):\n")
print(summary_given)

summary_received <- groom_dur_recv %>%
  summarise(
    min_duration = min(duration_sec, na.rm = TRUE),
    max_duration = max(duration_sec, na.rm = TRUE),
    median_duration = median(duration_sec, na.rm = TRUE),
    mean_duration = mean(duration_sec, na.rm = TRUE),
    sd_duration = sd(duration_sec, na.rm = TRUE)
  )

cat("\nGrooming RECEIVED - Summary (seconds):\n")
print(summary_received)

given_by_focal <- groom_dur_given %>%
  group_by(giver = focal_id) %>%
  summarise(total_given_sec = sum(duration_sec, na.rm = TRUE))
given_by_focal_ranked <- given_by_focal %>%
  arrange(desc(total_given_sec))
print(given_by_focal_ranked)
View(given_by_focal_ranked)

affil_given <- CSI_CERZ %>%
  filter(!is.na(affcont_give_id), affcont_give_id != "",
         !is.na(affcont_give_type), affcont_give_type != "") %>%
  transmute(
    focal_id = focal_id,
    partner = affcont_give_id,
    type = affcont_give_type,
    event = "given"
  ) %>%
  filter(focal_id != partner)

affil_given_mirror <- affil_given %>%
  mutate(temp_focal = focal_id, temp_partner = partner) %>%
  transmute(
    focal_id = temp_partner,
    partner = temp_focal,
    type = type,
    event = "received"
  ) %>%
  filter(focal_id != partner)

affil_received <- CSI_CERZ %>%
  filter(!is.na(affcont_recv_id), affcont_recv_id != "",
         !is.na(affcont_recv_type), affcont_recv_type != "") %>%
  transmute(
    focal_id = focal_id,
    partner = affcont_recv_id,
    type = affcont_recv_type,
    event = "received"
  ) %>%
  filter(focal_id != partner)

affil_received_mirror <- affil_received %>%
  mutate(temp_focal = focal_id, temp_partner = partner) %>%
  transmute(
    focal_id = temp_partner,
    partner = temp_focal,
    type = type,
    event = "given"
  ) %>%
  filter(focal_id != partner)

affil_combined <- bind_rows(affil_given, affil_given_mirror,
                            affil_received, affil_received_mirror) %>%
  filter(!is.na(focal_id), focal_id != "",
         !is.na(partner), partner != "")

affil_overall_summary <- affil_combined %>%
  group_by(focal_id, event, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(event, type),
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  arrange(focal_id)

cat("Overall Affiliative Behavior Summary by Focal:\n")
print(affil_overall_summary, n = Inf)

View(affil_overall_summary)

affil_dyad_summary <- affil_combined %>%
  group_by(focal_id, partner, event, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(event, type),
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  arrange(focal_id, partner)

cat("\nAffiliative Behavior Summary by Dyad (Focal and Partner):\n")
View(affil_dyad_summary)

affil_dyad_summary_aggregated <- affil_combined %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = type,
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  arrange(dyad)

View(affil_dyad_summary_aggregated)

missing_affil_dyads <- dyads %>%
  anti_join(affil_dyad_summary_aggregated, by = "dyad")

print(missing_affil_dyads)
View(missing_affil_dyads)

affil_breakdown_aggregated_episperhour <- affil_combined %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner),
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    Total_Affil = n(),
    .groups = "drop"
  ) %>%
  left_join(
    dyads %>% select(dyad, total_seconds),
    by = c("dyad_id" = "dyad")
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Total_Affil / total_hours
  )

affil_breakdown_aggregated_episperhour %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Total_Affil),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 2)
  )

affil_given_individual <- affil_combined %>%
  filter(event == "given") %>%
  group_by(focal_id) %>%
  summarise(total_affil_given = n())
affil_given_individual_sorted <- affil_given_individual %>%
  arrange(desc(total_affil_given))
print(affil_given_individual_sorted)
View(affil_given_individual_sorted)

affil_given_received_summary_CERZ <- affil_overall_summary %>%
  mutate(
    given = given_H + given_L + given_T + given_E,
    received = received_H + received_L + received_T + received_E
  ) %>%
  select(focal_id, given, received) %>%
  arrange(focal_id)

write_xlsx(affil_given_received_summary_CERZ, "affil_given_received_summary_CERZ.xlsx")

write_xlsx(
  affil_dyad_summary,
  path = "CERZ_affil_dyad_summary_directed.xlsx"
)

prox_summary <- CSI_CERZ %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(prox_count = n(), .groups = "drop")

print(prox_summary, n = Inf)

prox_matrix <- xtabs(prox_count ~ focal_id + partner, data = prox_summary)
cat("\nProximity Interaction Matrix:\n")
print(prox_matrix)

total_prox_CERZ <- sum(CSI_CERZ$proximity_bc == "Y", na.rm = TRUE) +
                   sum(CSI_CERZ$proximity_1 == "Y", na.rm = TRUE) +
                   sum(CSI_CERZ$proximity_5 == "Y", na.rm = TRUE)

cat("Total proximity interactions across all CERZ focals and partners (all lengths):",
    total_prox_CERZ, "\n")

proximity_breakdown_aggregated_episperhour <- prox_summary %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner),
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    Total_Prox = sum(prox_count),
    .groups = "drop"
  ) %>%
  left_join(
    dyads %>% select(dyad, total_seconds),
    by = c("dyad_id" = "dyad")
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Total_Prox / total_hours
  )

proximity_breakdown_aggregated_episperhour %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Total_Prox),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 2)
  )

CSI_CERZ %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id) %>%
  summarise(total_proximity = sum(proximity_bc == "Y", na.rm = TRUE) +
              sum(proximity_1  == "Y", na.rm = TRUE) +
              sum(proximity_5  == "Y", na.rm = TRUE)) %>%
  { print(., n = Inf) }

prox_length_summary <- CSI_CERZ %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(
    count_bc = sum(proximity_bc == "Y", na.rm = TRUE),
    count_1  = sum(proximity_1  == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(focal_id, partner)
cat("Proximity Interactions by Focal and Partner (BC and 1m only):\n")
print(prox_length_summary, n = Inf)

prox_length_summary <- CSI_CERZ %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(
    count_bc = sum(proximity_bc == "Y", na.rm = TRUE),
    count_1  = sum(proximity_1  == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(focal_id, partner) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  mutate(total_prox = count_bc + count_1) %>%
  filter(total_prox >= 2)

prox_length_summary <- prox_length_summary %>%
  group_by(dyad) %>%
  summarise(
    count_bc   = sum(count_bc, na.rm = TRUE),
    count_1    = sum(count_1, na.rm = TRUE),
    total_prox = sum(total_prox, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dyad)

print(prox_length_summary, n = Inf)
View(prox_length_summary)

missing_prox_dyads <- dyads %>%
  anti_join(prox_length_summary, by = "dyad")
print(missing_prox_dyads)

proximity_interactions_per_focal <- prox_per_focal

write.xlsx(
  proximity_interactions_per_focal,
  file = "proximity_interactions_per_focal.xlsx",
  overwrite = TRUE,
  asTable = TRUE,
  rowNames = FALSE,
  colWidths = "auto",
  sheetName = "Prox_per_Focal"
)

write_xlsx(
  prox_length_summary,
  path = "CERZ_prox_summary.xlsx"
)

dyad_groom_counts <- groom_dur_combined %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad) %>%
  summarise(
    n_groom_events = n(),
    total_groom_dur = sum(duration_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_groom_events >= 2)

dyad_data_dur <- inner_join(dyads, dyad_groom_counts, by = "dyad")

dyad_data_dur <- dyad_data_dur %>%
  mutate(
    grooming_minutes = total_groom_dur / 60,
    observation_hours = total_seconds / 3600,
    groom_dur_rate = grooming_minutes / observation_hours
  )

mean_dyadic_grooming_rate <- mean(dyad_data_dur$groom_dur_rate, na.rm = TRUE)
print(mean_dyadic_grooming_rate)
cat("Mean Grooming Duration Rate across Dyads (min/hr):", mean_dyadic_grooming_rate, "\n")

dyad_data_dur <- dyad_data_dur %>%
  mutate(CSI_groom_dur = groom_dur_rate / mean_dyadic_grooming_rate)

final_CSI_groom_dur <- dyad_data_dur %>%
  select(focal_id1, focal_id2, dyad, total_groom_dur, total_seconds, groom_dur_rate, CSI_groom_dur)
print(final_CSI_groom_dur)
View(final_CSI_groom_dur)

affil_total <- affil_dyad_summary %>%
  mutate(total_affil = rowSums(select(., starts_with("given_"), starts_with("received_")), na.rm = TRUE)) %>%
  select(focal_id, partner, total_affil) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad) %>%
  summarise(total_affil = mean(total_affil, na.rm = TRUE), .groups = "drop") %>%
  filter(total_affil >= 2)

dyad_affil <- dyads %>%
  select(focal_id1, focal_id2, dyad, total_seconds) %>%
  left_join(affil_total, by = "dyad") %>%
  mutate(
    observation_hours = total_seconds / 3600,
    affil_rate        = total_affil / observation_hours
  )

overall_mean_affil <- mean(dyad_affil$affil_rate, na.rm = TRUE)
cat("Overall Mean Affiliative Rate (interactions per hour):", overall_mean_affil, "\n")

dyad_affil <- dyad_affil %>%
  mutate(CSI_affil = affil_rate / overall_mean_affil)

final_affil_CSI <- dyad_affil %>%
  select(focal_id1, focal_id2, dyad, total_affil, total_seconds, observation_hours, affil_rate, CSI_affil)
print(final_affil_CSI, n = Inf)
View(final_affil_CSI)

dyad_prox <- dyads %>%
  select(focal_id1, focal_id2, dyad, total_seconds) %>%
  left_join(prox_length_summary, by = "dyad") %>%
  mutate(
    observation_hours = total_seconds / 3600,
    prox_rate         = total_prox / observation_hours
  )

overall_mean_prox <- mean(dyad_prox$prox_rate, na.rm = TRUE)
cat("Overall Mean Proximity Rate (interactions per hour):", overall_mean_prox, "\n")

dyad_prox <- dyad_prox %>%
  mutate(CSI_prox = prox_rate / overall_mean_prox)

final_prox_CSI <- dyad_prox %>%
  select(focal_id1, focal_id2, dyad, count_bc, count_1, total_prox, total_seconds, observation_hours, prox_rate, CSI_prox)
print(final_prox_CSI, n = Inf)
View(final_prox_CSI)

final_CSI <- final_affil_CSI %>%
  select(focal_id1, focal_id2, dyad, CSI_affil) %>%
  left_join(final_CSI_groom_dur %>% select(dyad, CSI_groom_dur), by = "dyad") %>%
  left_join(final_prox_CSI %>% select(dyad, CSI_prox), by = "dyad") %>%
  mutate(
    CSI_affil     = coalesce(CSI_affil, 0),
    CSI_groom_dur = coalesce(CSI_groom_dur, 0),
    CSI_prox      = coalesce(CSI_prox, 0)
  ) %>%
  mutate(CSI_total = (CSI_affil + CSI_groom_dur + CSI_prox) / 3) %>%
  mutate(z_CSI = as.vector(scale(CSI_total))) %>%
  arrange(dyad)

final_CSI %>%
  summarise(
    n_dyads = n(),
    min_CSI = round(min(CSI_total, na.rm = TRUE), 2),
    max_CSI = round(max(CSI_total, na.rm = TRUE), 2),
    mean_CSI = round(mean(CSI_total, na.rm = TRUE), 2),
    sd_CSI = round(sd(CSI_total, na.rm = TRUE), 2),
    median_CSI = round(median(CSI_total, na.rm = TRUE), 2)
  )

write.xlsx(
  final_CSI,
  file      = "CERZ_final_CSI.xlsx",
  overwrite = TRUE,
  asTable   = TRUE,
  rowNames  = FALSE,
  colWidths = "auto"
)

min_CSI_total <- min(final_CSI$CSI_total, na.rm = TRUE)
max_CSI_total <- max(final_CSI$CSI_total, na.rm = TRUE)
mean_CSI_total <- mean(final_CSI$CSI_total, na.rm = TRUE)
median_CSI_total <- median(final_CSI$CSI_total, na.rm = TRUE)
cat("Minimum CSI score across all dyads:", min_CSI_total, "\n")
cat("Maximum CSI score across all dyads:", max_CSI_total, "\n")
cat("Mean CSI score across all dyads:", mean_CSI_total, "\n")
cat("Median CSI score across all dyads:", median_CSI_total, "\n")

CSI_EMER <- socialbondrawdataNEW %>%
  filter(grepl("EMER", file_id)) %>%
  dplyr::select(file_id, date, focal_id, sex, age,
         groom_give_id, groom_give_start, groom_give_end, groom_give_dur_total,
         groom_recv_id, groom_recv_start, groom_recv_end, groom_recv_dur_total,
         affcont_give_id, affcont_give_type, affcont_give_time,
         affcont_recv_id, affcont_recv_type, recv_affcont_time,
         proximity_id, proximity_time, proximity_bc, proximity_1, proximity_5,
         out_view_start, out_view_end, out_view_total)

CSI_EMER$groom_give_dur_total[CSI_CERZ$groom_give_dur_total == "NA"] <- NA
str(CSI_EMER$groom_give_dur_total)
summary(CSI_EMER$groom_give_dur_total)
convert_time_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)
  parts <- strsplit(time_str, ":")[[1]]
  if(length(parts) != 2) return(NA)
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  if(is.na(minutes) || is.na(seconds)) return(NA)
  return(minutes * 60 + seconds)
}

CSI_EMER$groom_give_dur_total_sec <- sapply(CSI_EMER$groom_give_dur_total, convert_time_to_seconds)
mean(CSI_EMER$groom_give_dur_total_sec, na.rm = TRUE)
sd(CSI_EMER$groom_give_dur_total_sec, na.rm = TRUE)
max(CSI_EMER$groom_give_dur_total_sec, na.rm = TRUE)
min(CSI_EMER$groom_give_dur_total_sec, na.rm = TRUE)

hist(CSI_EMER$groom_give_dur_total_sec,
     main = "Histogram of Groom Give Duration (seconds) (0-100s)",
     xlab = "Duration (seconds)",
     col = "skyblue",
     breaks = 800,
     xlim = c(0, 100))

hist(CSI_EMER$groom_give_dur_total_sec,
     main = "Histogram of Groom Give Duration (seconds)",
     xlab = "Duration (seconds)",
     col = "skyblue",
     breaks = 50)

CSI_EMER$groom_recv_dur_total[CSI_EMER$groom_recv_dur_total == "NA"] <- NA
CSI_EMER$groom_recv_dur_total_sec <- sapply(CSI_EMER$groom_recv_dur_total, convert_time_to_seconds)
mean(CSI_EMER$groom_recv_dur_total_sec, na.rm = TRUE)
sd(CSI_EMER$groom_recv_dur_total_sec, na.rm = TRUE)
max(CSI_EMER$groom_recv_dur_total_sec, na.rm = TRUE)
min(CSI_EMER$groom_recv_dur_total_sec, na.rm = TRUE)

hist(CSI_EMER$groom_recv_dur_total_sec,
     main = "Histogram of Groom Recv Duration (seconds)",
     xlab = "Duration (seconds)",
     col = "skyblue",
     breaks = 50)

focal_obs_EMER <- CSI_EMER %>%
  distinct(focal_id, file_id) %>%
  group_by(focal_id) %>%
  summarise(obs_count = n(), .groups = "drop") %>%
  mutate(
    total_seconds = obs_count * 30 * 60,
    total_time = sprintf("%02d:%02d:%02d",
                         total_seconds %/% 3600,
                         (total_seconds %% 3600) %/% 60,
                         total_seconds %% 60)
  )

print(focal_obs_EMER)

dyads_EMER <- expand.grid(focal_id1 = focal_obs_EMER$focal_id, focal_id2 = focal_obs_EMER$focal_id,
                          stringsAsFactors = FALSE) %>%
  filter(focal_id1 < focal_id2)

dyads_EMER <- dyads_EMER %>%
  left_join(focal_obs_EMER %>% dplyr::select(focal_id, total_seconds), by = c("focal_id1" = "focal_id")) %>%
  rename(time1 = total_seconds) %>%
  left_join(focal_obs_EMER %>% dplyr::select(focal_id, total_seconds), by = c("focal_id2" = "focal_id")) %>%
  rename(time2 = total_seconds) %>%
  mutate(
    total_seconds = time1 + time2,
    total_time = sprintf("%02d:%02d:%02d",
                         total_seconds %/% 3600,
                         (total_seconds %% 3600) %/% 60,
                         total_seconds %% 60),
    dyad = paste(focal_id1, focal_id2, sep = "_")
  ) %>%
  arrange(focal_id1, focal_id2)

print(dyads_EMER)

focal_obs_summary_EMER <- focal_obs_EMER %>%
  summarise(
    n_individuals       = n(),
    total_obs_count     = sum(obs_count),
    total_hours         = sum(total_seconds) / 3600,
    avg_obs_per_indiv   = mean(obs_count),
    avg_hours_per_indiv = mean(total_seconds / 3600)
  )

cat("\n=== EMER ===\n")
print(focal_obs_summary_EMER)
print(focal_obs_EMER[, c("focal_id", "obs_count", "total_time")])

valid_names_EMER <- c("BASUKI", "BUMI", "DOUGIE", "DRUSILLA", "EKAH",
                      "INDAH", "KERANA", "MASAMBA", "SETANA")

valid_types <- c("T", "E", "H", "L")

valid_proximity <- c("Y")

clean_column <- function(column, valid_values) {
  return(ifelse(column %in% valid_values, column, NA))
}

CSI_EMER$groom_give_id <- clean_column(CSI_EMER$groom_give_id, valid_names_EMER)
CSI_EMER$groom_recv_id <- clean_column(CSI_EMER$groom_recv_id, valid_names_EMER)
CSI_EMER$affcont_give_id <- clean_column(CSI_EMER$affcont_give_id, valid_names_EMER)
CSI_EMER$affcont_give_type <- clean_column(CSI_EMER$affcont_give_type, valid_types)
CSI_EMER$affcont_recv_id <- clean_column(CSI_EMER$affcont_recv_id, valid_names_EMER)
CSI_EMER$affcont_recv_type <- clean_column(CSI_EMER$affcont_recv_type, valid_types)
CSI_EMER$proximity_id <- clean_column(CSI_EMER$proximity_id, valid_names_EMER)
CSI_EMER$proximity_bc <- clean_column(CSI_EMER$proximity_bc, valid_proximity)
CSI_EMER$proximity_1 <- clean_column(CSI_EMER$proximity_1, valid_proximity)
CSI_EMER$proximity_5 <- clean_column(CSI_EMER$proximity_5, valid_proximity)

groom_freq_long_EMER <- CSI_EMER %>%
  pivot_longer(
    cols = c(groom_give_id, groom_recv_id),
    names_to = "direction",
    values_to = "partner"
  ) %>%
  filter(!is.na(partner))

groom_freq_summary_EMER <- groom_freq_long_EMER %>%
  group_by(focal_id, partner) %>%
  summarise(freq = n(), .groups = "drop")

groom_freq_matrix_EMER <- xtabs(freq ~ focal_id + partner, data = groom_freq_summary_EMER)

cat("\nTotal Grooming Interactions per Focal (EMER):\n")
print(rowSums(groom_freq_matrix_EMER))
cat("\nGrooming Interaction Frequency Matrix (EMER):\n")
print(groom_freq_matrix_EMER)
View(groom_freq_matrix_EMER)

grooming_data_EMER <- CSI_EMER %>%
  select(focal_id, groom_give_id, groom_recv_id) %>%
  filter(!is.na(focal_id) & (!is.na(groom_give_id) | !is.na(groom_recv_id)))

groom_given_EMER <- grooming_data_EMER %>%
  filter(!is.na(groom_give_id)) %>%
  group_by(focal_id, partner = groom_give_id) %>%
  summarise(Groom_Given = n(), .groups = "drop")

groom_received_EMER <- grooming_data_EMER %>%
  filter(!is.na(groom_recv_id)) %>%
  group_by(focal_id, partner = groom_recv_id) %>%
  summarise(Groom_Received = n(), .groups = "drop")

grooming_breakdown_EMER <- full_join(groom_given_EMER, groom_received_EMER,
                                     by = c("focal_id", "partner")) %>%
  mutate(
    Groom_Given    = replace_na(Groom_Given, 0),
    Groom_Received = replace_na(Groom_Received, 0)
  ) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "-")) %>%
  ungroup() %>%
  arrange(dyad, focal_id) %>%
  select(-dyad)

View(grooming_breakdown_EMER)

grooming_breakdown_EMER_totals <- grooming_breakdown_EMER %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad) %>%
  summarise(
    Groom_Given    = sum(Groom_Given, na.rm = TRUE),
    Groom_Received = sum(Groom_Received, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Groom_Total = Groom_Given + Groom_Received) %>%
  arrange(dyad)

print(grooming_breakdown_EMER_totals, n = Inf)
View(grooming_breakdown_EMER_totals)

sum(grooming_breakdown_EMER_totals$Groom_Total)

cat("Total grooming interactions (sum of Groom_Total across all dyads):",
    sum(grooming_breakdown_EMER_totals$Groom_Total, na.rm = TRUE), "\n")

grooming_breakdown_aggregated_episperhour_EMER <- grooming_breakdown_EMER_totals %>%
  left_join(
    dyads_EMER %>% select(dyad, total_seconds),
    by = "dyad"
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Groom_Total / total_hours
  )

grooming_breakdown_aggregated_episperhour_EMER %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Groom_Total),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 2)
  )

groom_given_received_summary_EMER <- grooming_data_EMER %>%
  filter(!is.na(groom_give_id)) %>%
  count(focal_id, name = "Groom_Given") %>%

  full_join(
    grooming_data_EMER %>%
      filter(!is.na(groom_recv_id)) %>%
      count(focal_id, name = "Groom_Received"),
    by = "focal_id"
  ) %>%

  mutate(
    Groom_Given     = replace_na(Groom_Given, 0),
    Groom_Received  = replace_na(Groom_Received, 0),
    Total_Grooming  = Groom_Given + Groom_Received
  ) %>%

  arrange(desc(Total_Grooming)) %>%

  select(
    monkey_id = focal_id,
    Groom_Given,
    Groom_Received,
    Total_Grooming
  )

View(groom_given_received_summary_EMER)

write_xlsx(groom_given_received_summary_EMER,
           path = "groom_given_received_summary_EMER.xlsx")

time_columns <- c("groom_give_dur_total", "groom_recv_dur_total")

CSI_EMER[time_columns] <- lapply(CSI_EMER[time_columns], function(col) {
  as.character(col)
})

convert_to_time_format <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    return(time_str)
  }

  return(NA)
}

CSI_EMER[time_columns] <- lapply(CSI_EMER[time_columns], function(col) {
  sapply(col, convert_to_time_format, USE.NAMES = FALSE)
})

invalid_times <- CSI_EMER$groom_give_dur_total[!is.na(CSI_EMER$groom_give_dur_total) &
                                                 !grepl("^\\d{2}:\\d{2}\\.\\d{3}$", CSI_EMER$groom_give_dur_total)]
print(invalid_times)

convert_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") return(NA)

  time_str <- trimws(time_str)

  if (grepl("^\\d{2}:\\d{2}\\.\\d{3}$", time_str)) {
    parts <- strsplit(time_str, "[:\\.]")[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]) + as.numeric(parts[3]) / 1000)
  }

  return(NA)
}

groom_dur_given_EMER <- CSI_EMER %>%
  filter(!is.na(groom_give_id)) %>%
  mutate(
    duration_sec = as.numeric(unlist(lapply(groom_give_dur_total, convert_to_seconds)))
  ) %>%
  dplyr::select(focal_id, partner = groom_give_id, duration_sec)

groom_dur_recv_EMER <- CSI_EMER %>%
  filter(!is.na(groom_recv_id)) %>%
  mutate(
    duration_sec = as.numeric(unlist(lapply(groom_recv_dur_total, convert_to_seconds)))
  ) %>%
  dplyr::select(focal_id, partner = groom_recv_id, duration_sec)

groom_dur_combined_EMER <- bind_rows(groom_dur_given_EMER, groom_dur_recv_EMER)

groom_dur_summary_EMER <- groom_dur_combined_EMER %>%
  group_by(focal_id, partner) %>%
  summarise(total_duration_sec = sum(duration_sec, na.rm = TRUE), .groups = "drop")

groom_dur_summary_EMER %>%
  mutate(
    dyad_id = map2_chr(
      focal_id, partner,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  summarise(
    total_rows   = n(),
    unique_dyads = n_distinct(dyad_id)
  ) %>%
  print()

write_xlsx(
  groom_dur_summary_EMER,
  path = "EMER_groom_dur_summary_directed.xlsx"
)

groom_dur_matrix_EMER <- xtabs(total_duration_sec ~ focal_id + partner, data = groom_dur_summary_EMER)

groom_dur_matrix_EMER <- groom_dur_matrix_EMER + t(groom_dur_matrix_EMER)

cat("\nGrooming Duration Matrix (seconds) for CSI_EMER:\n")
print(groom_dur_matrix_EMER)
cat("\nTotal Grooming Duration (seconds) per Focal for CSI_EMER:\n")
print(rowSums(groom_dur_matrix_EMER))

format_duration <- function(total_seconds) {
  minutes <- floor(total_seconds / 60)
  secs <- floor(total_seconds %% 60)
  ms <- round((total_seconds %% 1) * 1000)
  sprintf("%02d:%02d:%03d", minutes, secs, ms)
}

formatted_dur_matrix_EMER <- apply(groom_dur_matrix_EMER, c(1,2), format_duration)

cat("\nGrooming Duration Matrix (formatted mm:ss:ms) for CSI_EMER:\n")
print(formatted_dur_matrix_EMER)

formatted_total_EMER <- sapply(rowSums(groom_dur_matrix_EMER), format_duration)

cat("\nTotal Grooming Duration (formatted mm:ss:ms) per Focal for CSI_EMER:\n")
print(formatted_total_EMER)

total_sec_EMER <- sum(sapply(formatted_total_EMER, function(t) {
  if (!nzchar(t)) return(0)
  p <- as.numeric(strsplit(t, "[:.]")[[1]])
  p[1] * 60 + p[2]
}))

hours   <- total_sec_EMER %/% 3600
minutes <- (total_sec_EMER %% 3600) %/% 60
seconds <- total_sec_EMER %% 60

groom_per_focal_EMER <- data.frame(
  focal_id = rownames(groom_dur_matrix_EMER),
  total_duration_sec = rowSums(groom_dur_matrix_EMER)
)

groom_stats_EMER <- groom_per_focal_EMER %>%
  summarise(
    N_focals      = n(),
    Mean_duration = mean(total_duration_sec) / 60,
    SD_duration   = sd(total_duration_sec) / 60
  )

cat("Grooming duration per focal (EMER): mean ± SD =",
    round(groom_stats_EMER$Mean_duration, 1), "±",
    round(groom_stats_EMER$SD_duration, 1),
    "minutes (N =", groom_stats_EMER$N_focals, "focals)\n")
cat("\nTotal grooming duration across ALL EMER focals (hh:mm:ss):",
    sprintf("%02d:%02d:%02d", hours, minutes, seconds), "\n")

summary_given_EMER <- groom_dur_given_EMER %>%
  summarise(
    min_duration = min(duration_sec, na.rm = TRUE),
    max_duration = max(duration_sec, na.rm = TRUE),
    median_duration = median(duration_sec, na.rm = TRUE),
    mean_duration = mean(duration_sec, na.rm = TRUE),
    sd_duration = sd(duration_sec, na.rm = TRUE)
  )

cat("\nGrooming GIVEN - Summary (seconds) - EMER:\n")
print(summary_given_EMER)

summary_received_EMER <- groom_dur_recv_EMER %>%
  summarise(
    min_duration = min(duration_sec, na.rm = TRUE),
    max_duration = max(duration_sec, na.rm = TRUE),
    median_duration = median(duration_sec, na.rm = TRUE),
    mean_duration = mean(duration_sec, na.rm = TRUE),
    sd_duration = sd(duration_sec, na.rm = TRUE)
  )

cat("\nGrooming RECEIVED - Summary (seconds) - EMER:\n")
print(summary_received_EMER)

affil_given_EMER <- CSI_EMER %>%
  filter(!is.na(affcont_give_id), affcont_give_id != "",
         !is.na(affcont_give_type), affcont_give_type != "") %>%
  transmute(
    focal_id = focal_id,
    partner = affcont_give_id,
    type = affcont_give_type,
    event = "given"
  ) %>%
  filter(focal_id != partner)

affil_given_mirror_EMER <- affil_given_EMER %>%
  mutate(temp_focal = focal_id, temp_partner = partner) %>%
  transmute(
    focal_id = temp_partner,
    partner = temp_focal,
    type = type,
    event = "received"
  ) %>%
  filter(focal_id != partner)

affil_received_EMER <- CSI_EMER %>%
  filter(!is.na(affcont_recv_id), affcont_recv_id != "",
         !is.na(affcont_recv_type), affcont_recv_type != "") %>%
  transmute(
    focal_id = focal_id,
    partner = affcont_recv_id,
    type = affcont_recv_type,
    event = "received"
  ) %>%
  filter(focal_id != partner)

affil_received_mirror_EMER <- affil_received_EMER %>%
  mutate(temp_focal = focal_id, temp_partner = partner) %>%
  transmute(
    focal_id = temp_partner,
    partner = temp_focal,
    type = type,
    event = "given"
  ) %>%
  filter(focal_id != partner)

affil_combined_EMER <- bind_rows(affil_given_EMER, affil_given_mirror_EMER,
                                 affil_received_EMER, affil_received_mirror_EMER) %>%
  filter(!is.na(focal_id), focal_id != "",
         !is.na(partner), partner != "")

affil_breakdown_aggregated_episperhour_EMER <- affil_combined_EMER %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner),
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    Total_Affil = n(),
    .groups = "drop"
  ) %>%
  left_join(
    dyads_EMER %>% select(dyad, total_seconds),
    by = c("dyad_id" = "dyad")
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Total_Affil / total_hours
  )

affil_breakdown_aggregated_episperhour_EMER %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Total_Affil),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 2)
  )

affil_overall_summary_EMER <- affil_combined_EMER %>%
  group_by(focal_id, event, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(event, type),
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  arrange(focal_id)

cat("Overall Affiliative Behavior Summary by Focal for EMER:\n")
print(affil_overall_summary_EMER, n = Inf)
View(affil_overall_summary_EMER)

affil_dyad_summary_EMER <- affil_combined_EMER %>%
  group_by(focal_id, partner, event, type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(event, type),
    values_from = count,
    values_fill = list(count = 0)
  ) %>%
  arrange(focal_id, partner)

cat("\nAffiliative Behavior Summary by Dyad for EMER (Focal and Partner):\n")
print(affil_dyad_summary_EMER, n = Inf)
View(affil_dyad_summary_EMER)

write_xlsx(
  affil_dyad_summary_EMER,
  path = "EMER_affil_dyad_summary_directed.xlsx"
)

affil_given_received_summary_EMER <- affil_overall_summary_EMER %>%
  mutate(
    given = given_H + given_L + given_T + given_E,
    received = received_H + received_L + received_T + received_E
  ) %>%
  select(focal_id, given, received) %>%
  arrange(focal_id)

write_xlsx(affil_given_received_summary_EMER, "affil_given_received_summary_EMER.xlsx")

prox_summary_EMER <- CSI_EMER %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(prox_count = n(), .groups = "drop")

cat("Proximity Interaction Summary by Focal and Partner for EMER:\n")
print(prox_summary_EMER, n = Inf)

prox_matrix_EMER <- xtabs(prox_count ~ focal_id + partner, data = prox_summary_EMER)
cat("\nProximity Interaction Matrix for EMER:\n")
print(prox_matrix_EMER)

CSI_EMER %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id) %>%
  summarise(total_proximity = sum(proximity_bc == "Y", na.rm = TRUE) +
              sum(proximity_1  == "Y", na.rm = TRUE) +
              sum(proximity_5  == "Y", na.rm = TRUE)) %>%
  { print(., n = Inf) }

prox_length_summary_EMER <- CSI_EMER %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(
    count_bc = sum(proximity_bc == "Y", na.rm = TRUE),
    count_1  = sum(proximity_1  == "Y", na.rm = TRUE),
    count_5  = sum(proximity_5  == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(focal_id, partner)

cat("Proximity Interactions by Focal and Partner (per proximity length) for EMER:\n")
print(prox_length_summary_EMER, n = Inf)

cat("Total proximity interactions for SETANA:",
    sum(with(prox_length_summary_EMER, ifelse(focal_id == "SETANA", count_bc + count_1 + count_5, 0))), "\n")

total_prox_EMER <- sum(CSI_EMER$proximity_bc == "Y", na.rm = TRUE) +
                   sum(CSI_EMER$proximity_1 == "Y", na.rm = TRUE) +
                   sum(CSI_EMER$proximity_5 == "Y", na.rm = TRUE)

cat("\nTotal proximity interactions across ALL EMER (direct from raw data):",
    total_prox_EMER, "\n")

proximity_breakdown_aggregated_episperhour_EMER <- prox_summary_EMER %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner),
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    Total_Prox = sum(prox_count),
    .groups = "drop"
  ) %>%
  left_join(
    dyads_EMER %>% select(dyad, total_seconds),
    by = c("dyad_id" = "dyad")
  ) %>%
  mutate(
    total_hours = total_seconds / 3600,
    episodes_per_hour = Total_Prox / total_hours
  )

proximity_breakdown_aggregated_episperhour_EMER %>%
  summarise(
    n_dyads = n(),
    total_interactions = sum(Total_Prox),
    mean_episodes_per_hour = round(mean(episodes_per_hour), 2),
    sd_episodes_per_hour = round(sd(episodes_per_hour), 2),
    min_episodes_per_hour = round(min(episodes_per_hour), 2),
    max_episodes_per_hour = round(max(episodes_per_hour), 9)
  )

write.xlsx(
  prox_per_focal_EMER,
  file = "prox_per_focal_EMER.xlsx",
  overwrite = TRUE,
  asTable = TRUE,
  rowNames = FALSE,
  colWidths = "auto",
  sheetName = "Prox_per_Focal_EMER"
)

write_xlsx(
  prox_length_summary_EMER,
  path = "EMER_prox_length_summary_directed.xlsx"
)

dyad_groom_counts_EMER <- groom_dur_combined_EMER %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad) %>%
  summarise(
    n_groom_events  = n(),
    total_groom_dur = sum(duration_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_groom_events >= 2)

missing_groom_dyads_EMER <- dyads_EMER %>%
  anti_join(dyad_groom_counts_EMER, by = "dyad")
print(missing_groom_dyads_EMER)
View(missing_groom_dyads_EMER)

dyad_data_dur_EMER <- dyads_EMER %>%
  left_join(dyad_groom_counts_EMER, by = "dyad")

dyad_data_dur_EMER <- dyad_data_dur_EMER %>%
  mutate(
    grooming_minutes  = total_groom_dur / 60,
    observation_hours = total_seconds / 3600,
    groom_dur_rate    = grooming_minutes / observation_hours
  )

mean_dyadic_grooming_rate_EMER <- mean(dyad_data_dur_EMER$groom_dur_rate, na.rm = TRUE)
cat("Mean Grooming Duration Rate across Dyads (min/hr) for EMER:", mean_dyadic_grooming_rate_EMER, "\n")

dyad_data_dur_EMER <- dyad_data_dur_EMER %>%
  mutate(CSI_groom_dur = groom_dur_rate / mean_dyadic_grooming_rate_EMER)

final_CSI_groom_dur_EMER <- dyad_data_dur_EMER %>%
  select(focal_id1, focal_id2, dyad, total_groom_dur, total_seconds, groom_dur_rate, CSI_groom_dur)
print(final_CSI_groom_dur_EMER)
View(final_CSI_groom_dur_EMER)

affil_total_EMER <- affil_dyad_summary_EMER %>%
  mutate(total_affil = rowSums(select(., starts_with("given_"), starts_with("received_")), na.rm = TRUE)) %>%
  select(focal_id, partner, total_affil) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  group_by(dyad) %>%
  summarise(total_affil = sum(total_affil, na.rm = TRUE), .groups = "drop") %>%
  filter(total_affil >= 2)

missing_affil_dyads_EMER <- dyads_EMER %>%
  anti_join(affil_total_EMER, by = "dyad")
View(missing_affil_dyads_EMER)

dyad_affil_EMER <- dyads_EMER %>%
  select(focal_id1, focal_id2, dyad, total_seconds) %>%
  left_join(affil_total_EMER, by = "dyad") %>%
  mutate(
    observation_hours = total_seconds / 3600,
    affil_rate        = total_affil / observation_hours
  )

overall_mean_affil_EMER <- mean(dyad_affil_EMER$affil_rate, na.rm = TRUE)
cat("Overall Mean Affiliative Rate for EMER (interactions per hour):", overall_mean_affil_EMER, "\n")

dyad_affil_EMER <- dyad_affil_EMER %>%
  mutate(CSI_affil = affil_rate / overall_mean_affil_EMER)

final_affil_CSI_EMER <- dyad_affil_EMER %>%
  select(focal_id1, focal_id2, dyad, total_affil, total_seconds, observation_hours, affil_rate, CSI_affil)
print(final_affil_CSI_EMER, n = Inf)
View(final_affil_CSI_EMER)

prox_length_summary_EMER_BC1 <- CSI_EMER %>%
  filter(!is.na(proximity_id)) %>%
  group_by(focal_id, partner = proximity_id) %>%
  summarise(
    count_bc = sum(proximity_bc == "Y", na.rm = TRUE),
    count_1  = sum(proximity_1  == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(dyad = paste(sort(c(focal_id, partner)), collapse = "_")) %>%
  ungroup() %>%
  mutate(total_prox = count_bc + count_1) %>%
  filter(total_prox >= 2)

prox_length_summary_EMER_BC1 <- prox_length_summary_EMER_BC1 %>%
  group_by(dyad) %>%
  summarise(
    count_bc   = sum(count_bc, na.rm = TRUE),
    count_1    = sum(count_1, na.rm = TRUE),
    total_prox = sum(total_prox, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(dyad)

missing_prox_dyads_EMER <- dyads_EMER %>%
  anti_join(prox_length_summary_EMER_BC1, by = "dyad")
print(missing_prox_dyads_EMER)
View(missing_prox_dyads_EMER)

dyad_prox_EMER <- dyads_EMER %>%
  select(focal_id1, focal_id2, dyad, total_seconds) %>%
  left_join(prox_length_summary_EMER_BC1, by = "dyad") %>%
  mutate(
    observation_hours = total_seconds / 3600,
    prox_rate         = total_prox / observation_hours
  )

overall_mean_prox_EMER <- mean(dyad_prox_EMER$prox_rate, na.rm = TRUE)
cat("Overall Mean Proximity Rate for EMER (interactions per hour):", overall_mean_prox_EMER, "\n")

dyad_prox_EMER <- dyad_prox_EMER %>%
  mutate(CSI_prox = prox_rate / overall_mean_prox_EMER)

final_prox_CSI_EMER <- dyad_prox_EMER %>%
  select(focal_id1, focal_id2, dyad, count_bc, count_1, total_prox,
         total_seconds, observation_hours, prox_rate, CSI_prox)
print(final_prox_CSI_EMER, n = Inf)
View(final_prox_CSI_EMER)

final_CSI_EMER <- final_affil_CSI_EMER %>%
  select(focal_id1, focal_id2, dyad, CSI_affil) %>%
  left_join(final_CSI_groom_dur_EMER %>% select(dyad, CSI_groom_dur), by = "dyad") %>%
  left_join(final_prox_CSI_EMER %>% select(dyad, CSI_prox), by = "dyad") %>%
  mutate(
    CSI_affil     = coalesce(CSI_affil, 0),
    CSI_groom_dur = coalesce(CSI_groom_dur, 0),
    CSI_prox      = coalesce(CSI_prox, 0)
  ) %>%
  mutate(CSI_total = (CSI_affil + CSI_groom_dur + CSI_prox) / 3) %>%
  mutate(z_CSI = as.vector(scale(CSI_total))) %>%
  arrange(dyad)

print(final_CSI_EMER, n = Inf)
View(final_CSI_EMER)

final_CSI_EMER %>%
  summarise(
    n_dyads = n(),
    min_CSI = round(min(CSI_total, na.rm = TRUE), 2),
    max_CSI = round(max(CSI_total, na.rm = TRUE), 2),
    mean_CSI = round(mean(CSI_total, na.rm = TRUE), 2),
    sd_CSI = round(sd(CSI_total, na.rm = TRUE), 2),
    median_CSI = round(median(CSI_total, na.rm = TRUE), 2)
  )

write.xlsx(
  final_CSI_EMER,
  file      = "EMER_final_CSI.xlsx",
  overwrite = TRUE,
  asTable   = TRUE,
  rowNames  = FALSE,
  colWidths = "auto"
)

df_cerz <- socialbondrawdataNEW %>%
  filter(grepl("cerz", file_id, ignore.case = TRUE))
df_cerz_elo <- df_cerz %>% select(file_id, date, focal_id, sex, age,
                                   displ_give_id, displ_give_time,
                                   displ_recv_id, displ_recv_time)

df_cerz_elo <- df_cerz_elo %>%
  mutate(
    displ_give_id = na_if(displ_give_id, "NA"),
    displ_give_time = na_if(displ_give_time, "NA"),
    displ_recv_id = na_if(displ_recv_id, "NA"),
    displ_recv_time = na_if(displ_recv_time, "NA")
  )
df_cerz_elo %>%
  summarise(
    displ_give_id_count = sum(!is.na(displ_give_id)),
    displ_give_time_count = sum(!is.na(displ_give_time)),
    displ_recv_id_count = sum(!is.na(displ_recv_id)),
    displ_recv_time_count = sum(!is.na(displ_recv_time))
  )

df_cerz_elo <- df_cerz_elo %>%
  mutate(
    winner = case_when(
      displ_give_id != focal_id ~ focal_id,
      displ_recv_id != focal_id ~ displ_recv_id,
      TRUE ~ NA_character_
    ),
    loser = case_when(
      displ_give_id != focal_id ~ displ_give_id,
      displ_recv_id != focal_id ~ focal_id,
      TRUE ~ NA_character_
    )
  )
df_cerz_elo <- df_cerz_elo %>%
  mutate(
    time_selected = coalesce(displ_give_time, displ_recv_time),
    time_seconds = period_to_seconds(ms(time_selected)),
    merged_datetime = as.POSIXct(date) + time_seconds,
    merged_datetime = format(merged_datetime, "%Y-%m-%d %M:%OS3")
  ) %>%
  select(-time_selected, -time_seconds)

df_cerz_elo <- df_cerz_elo %>%
  filter(!is.na(winner) & !is.na(loser))

str(df_cerz_elo)

df_cerz_elo <- df_cerz_elo %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
str(df_cerz_elo$date)

df_cerz_elo <- df_cerz_elo %>%
  filter(!is.na(winner), !is.na(loser), !is.na(date)) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

elo_ratings <- elo.seq(
  winner = df_cerz_elo$winner,
  loser = df_cerz_elo$loser,
  Date = df_cerz_elo$date,
  progressbar = TRUE
)

elo_ratings_over_time <- extract_elo(elo_ratings)
print(elo_ratings_over_time)

df_cerz_elo_filtered <- df_cerz_elo %>%
  slice(-181:-185)

elo_ratings_filtered <- elo.seq(
  winner = df_cerz_elo_filtered$winner,
  loser = df_cerz_elo_filtered$loser,
  Date = df_cerz_elo_filtered$date,
  progressbar = TRUE
)

elo_ratings_over_time_filtered <- extract_elo(elo_ratings_filtered)
print(elo_ratings_over_time_filtered)
view(elo_ratings_over_time)

testtable <- data.frame(Var1 = names(elo_ratings_over_time_filtered), Freq = elo_ratings_over_time_filtered)

write.xlsx(testtable, file = "testtable.xlsx", overwrite = TRUE, asTable = TRUE, rowNames = FALSE, colWidths = "auto")

df_emerald <- socialbondrawdataNEW %>% filter(grepl("emer", file_id, ignore.case = TRUE))

df_emerald_elo <- df_emerald %>%
  dplyr::select(file_id, date, focal_id, sex, age,
                displ_give_id, displ_give_time,
                displ_recv_id, displ_recv_time)

df_emerald_elo <- df_emerald_elo %>%
  mutate(
    displ_give_id = na_if(displ_give_id, "NA"),
    displ_give_time = na_if(displ_give_time, "NA"),
    displ_recv_id = na_if(displ_recv_id, "NA"),
    displ_recv_time = na_if(displ_recv_time, "NA")
  )
df_emerald_elo %>%
  summarise(
    displ_give_id_count = sum(!is.na(displ_give_id)),
    displ_give_time_count = sum(!is.na(displ_give_time)),
    displ_recv_id_count = sum(!is.na(displ_recv_id)),
    displ_recv_time_count = sum(!is.na(displ_recv_time))
  )

df_emerald_elo <- df_emerald_elo %>%
  mutate(
    winner = case_when(
      displ_give_id != focal_id ~ focal_id,
      displ_recv_id != focal_id ~ displ_recv_id,
      TRUE ~ NA_character_
    ),
    loser = case_when(
      displ_give_id != focal_id ~ displ_give_id,
      displ_recv_id != focal_id ~ focal_id,
      TRUE ~ NA_character_
    )
  )
df_emerald_elo <- df_emerald_elo %>%
  mutate(
    time_selected = coalesce(displ_give_time, displ_recv_time),
    time_seconds = period_to_seconds(ms(time_selected)),
    merged_datetime = as.POSIXct(date) + time_seconds,
    merged_datetime = format(merged_datetime, "%Y-%m-%d %M:%OS3")
  ) %>%
  dplyr::select(-time_selected, -time_seconds)

df_emerald_elo <- df_emerald_elo %>%
  filter(!is.na(winner) & !is.na(loser))

df_cerz_elo <- df_cerz_elo %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))
str(df_cerz_elo$date)

df_emerald_elo <- df_emerald_elo %>%
  filter(!is.na(winner), !is.na(loser), !is.na(date)) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

str(df_emerald_elo$date)

elo_ratings_emerald <- elo.seq(
  winner = df_emerald_elo$winner,
  loser = df_emerald_elo$loser,
  Date = df_emerald_elo$date,
  progressbar = TRUE
)

elo_ratings_emerald <- extract_elo(elo_ratings_emerald)
print(elo_ratings_emerald)
view(elo_ratings_emerald)

testtableEMER <- data.frame(Var1 = names(elo_ratings_emerald), Freq = elo_ratings_emerald)
view(testtableEMER)

write.xlsx(testtableEMER, file = "testtableEMER.xlsx", overwrite = TRUE, asTable = TRUE, rowNames = FALSE, colWidths = "auto")

CSI_CERZ_individuals <- toupper(c("Mali", "Talia", "Fidget", "Kaos", "Sula", "Quanny",
                                  "Sunda", "Bunta", "Brow", "Makassar", "Monk"))

CSI_CERZ_relatedness_matrix <- matrix(0,
                                      nrow = length(CSI_CERZ_individuals),
                                      ncol = length(CSI_CERZ_individuals),
                                      dimnames = list(CSI_CERZ_individuals, CSI_CERZ_individuals))

CSI_CERZ_related_pairs <- data.frame(
  indiv_CERZ_1 = toupper(c("Brow", "Brow", "Bunta", "Bunta", "Bunta", "Makassar", "Mali", "Mali", "Mali", "Mali", "Sula", "Sula", "Sula", "Sula", "Sula", "Sunda", "Sunda", "Sunda", "Sunda", "Talia", "Talia")),
  indiv_CERZ_2 = toupper(c("Makassar", "Monk", "Brow", "Makassar","Monk", "Monk", "Brow", "Makassar", "Sula", "Sunda", "Brow", "Bunta", "Makassar", "Monk", "Sunda", "Brow", "Bunta", "Makassar", "Monk", "Bunta", "Monk")),
  relatedness_CERZ = c(0.5, 0.25, 0.25, 0.25, 0.5, 0.25, 0.5, 0.5, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.5, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5)
)

for (i in 1:nrow(CSI_CERZ_related_pairs)) {
  ind1 <- CSI_CERZ_related_pairs$indiv_CERZ_1[i]
  ind2 <- CSI_CERZ_related_pairs$indiv_CERZ_2[i]
  rel_value <- CSI_CERZ_related_pairs$relatedness_CERZ[i]

  CSI_CERZ_relatedness_matrix[ind1, ind2] <- rel_value
  CSI_CERZ_relatedness_matrix[ind2, ind1] <- rel_value
}

CSI_CERZ_relatedness_df <- as.data.frame(as.table(CSI_CERZ_relatedness_matrix)) %>%
  rename(focal_CERZ = Var1, partner_CERZ = Var2, relatedness_CERZ = Freq) %>%
  filter(focal_CERZ != partner_CERZ)

CSI_CERZ_relatedness_modeling <- CSI_CERZ_relatedness_df %>%
  mutate(
    dyad = map2_chr(focal_CERZ, partner_CERZ, ~ paste(sort(c(.x, .y)), collapse = "_"))
  ) %>%
  group_by(dyad) %>%
  summarize(relatedness_score = first(relatedness_CERZ), .groups = "drop")

print("CSI_CERZ Relatedness Matrix:")
print(CSI_CERZ_relatedness_matrix)
print(format(CSI_CERZ_relatedness_matrix, nsmall = 2))

print("CSI_CERZ Tidy Relatedness DataFrame:")
print(CSI_CERZ_relatedness_df)

print("CSI_CERZ Aggregated Relatedness DataFrame for Modeling:")
print(CSI_CERZ_relatedness_modeling)

write_xlsx(
  CSI_CERZ_relatedness_modeling,
  path = "CSI_CERZ_relatedness_modeling.xlsx"
)

CSI_EMER_individuals <- c("DRUSILLA", "DOUGIE", "SETANA", "EKAH", "KERANA",
                          "BASUKI", "BUMI", "MASAMBA", "INDAH")

CSI_EMER_relatedness_matrix <- matrix(0,
                                      nrow = length(CSI_EMER_individuals),
                                      ncol = length(CSI_EMER_individuals),
                                      dimnames = list(CSI_EMER_individuals, CSI_EMER_individuals))
CSI_EMER_related_pairs <- data.frame(
  indiv_EMER_1 = c("SETANA", "SETANA", "SETANA", "DRUSILLA", "DRUSILLA", "DRUSILLA",
                   "MASAMBA", "MASAMBA", "BUMI", "MASAMBA", "MASAMBA", "MASAMBA",
                   "EKAH", "EKAH", "INDAH", "BUMI", "BUMI", "BUMI",
                   "BASUKI", "BASUKI", "BASUKI", "DOUGIE", "DOUGIE", "DOUGIE",
                   "DOUGIE", "DOUGIE", "DOUGIE"),
  indiv_EMER_2 = c("MASAMBA", "BUMI", "BASUKI", "EKAH", "KERANA", "INDAH",
                   "BUMI", "BASUKI", "BASUKI", "EKAH", "KERANA", "INDAH",
                   "KERANA", "INDAH", "KERANA", "EKAH", "KERANA", "INDAH",
                   "EKAH", "KERANA", "INDAH", "MASAMBA", "BUMI", "BASUKI",
                   "EKAH", "KERANA", "INDAH"),
  relatedness_EMER = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5, 0.25, 0.25, 0.25,
                       0.5, 0.5, 0.5, 0.25, 0.25, 0.25,
                       0.25, 0.25, 0.25, 0.5, 0.5, 0.5,
                       0.5, 0.5, 0.5)
)

for (i in 1:nrow(CSI_EMER_related_pairs)) {
  ind1 <- CSI_EMER_related_pairs$indiv_EMER_1[i]
  ind2 <- CSI_EMER_related_pairs$indiv_EMER_2[i]
  rel_value <- CSI_EMER_related_pairs$relatedness_EMER[i]

  CSI_EMER_relatedness_matrix[ind1, ind2] <- rel_value
  CSI_EMER_relatedness_matrix[ind2, ind1] <- rel_value
}

CSI_EMER_relatedness_df <- as.data.frame(as.table(CSI_EMER_relatedness_matrix)) %>%
  rename(focal_EMER = Var1, partner_EMER = Var2, relatedness_EMER = Freq) %>%
  filter(focal_EMER != partner_EMER)

CSI_EMER_relatedness_matrix <- format(CSI_EMER_relatedness_matrix, nsmall = 2)

print("CSI_EMER Relatedness Matrix:")
print(CSI_EMER_relatedness_matrix)

print("CSI_EMER Tidy Relatedness DataFrame:")
print(CSI_EMER_relatedness_df)

CSI_EMER_relatedness_df <- CSI_EMER_relatedness_df %>%
  mutate(
    dyad_EMER = paste(focal_EMER, partner_EMER, sep = "_")
  )

CSI_EMER_relatedness_df <- CSI_EMER_relatedness_df %>%
  mutate(
    parts = str_split(dyad_EMER, "_"),
    normalized_dyad = map_chr(parts, ~ paste(sort(.), collapse = "_"))
  ) %>%
  group_by(normalized_dyad) %>%
  summarize(relatedness_EMER = first(relatedness_EMER), .groups = "drop")

print("CSI_EMER Aggregated Tidy Relatedness DataFrame:")
print(CSI_EMER_relatedness_df)

write_xlsx(
  CSI_EMER_relatedness_df,
  path = "CSI_EMER_relatedness_df.xlsx"
)

groom_given <- CSI_CERZ %>%
  filter(!is.na(groom_give_id)) %>%
  mutate(duration_sec = sapply(groom_give_dur_total, convert_to_seconds),
         direction = "given",
         partner = groom_give_id)

groom_recv <- CSI_CERZ %>%
  filter(!is.na(groom_recv_id)) %>%
  mutate(duration_sec = sapply(groom_recv_dur_total, convert_to_seconds),
         direction = "received",
         partner = groom_recv_id)

groom_long <- bind_rows(groom_given, groom_recv) %>%
  filter(!is.na(duration_sec)) %>%
  mutate(dyad_id = ifelse(focal_id < partner,
                          paste(focal_id, partner, sep = "-"),
                          paste(partner, focal_id, sep = "-")),
         direction = ifelse(direction == "given", "given", "received"))

groom_paired <- groom_long %>%
  pivot_wider(names_from = direction, values_from = duration_sec, values_fill = 0)

View(groom_paired)

n_distinct(groom_paired$dyad_id)

cat("CERZ - Total Groom Given events:", sum(groom_paired$given > 0), "\n")
cat("CERZ - Total Groom Received events:", sum(groom_paired$received > 0), "\n")
cat("CERZ - Total grooming events:", nrow(groom_paired), "\n")

nrow(groom_paired)
n_distinct(groom_paired$dyad_id)

cat("Rows with duration_sec < 1 second:\n")
groom_long %>%
  filter(duration_sec < 1 & !is.na(duration_sec)) %>%
  select(focal_id, partner, dyad_id, direction, duration_sec) %>%
  arrange(direction, duration_sec) %>%
  print(n = Inf)

groom_given_EMER <- CSI_EMER %>%
  filter(!is.na(groom_give_id)) %>%
  mutate(duration_sec = sapply(groom_give_dur_total, convert_to_seconds),
         direction = "given",
         partner = groom_give_id)

groom_recv_EMER <- CSI_EMER %>%
  filter(!is.na(groom_recv_id)) %>%
  mutate(duration_sec = sapply(groom_recv_dur_total, convert_to_seconds),
         direction = "received",
         partner = groom_recv_id)

groom_long_EMER <- bind_rows(groom_given_EMER, groom_recv_EMER) %>%
  filter(!is.na(duration_sec)) %>%
  mutate(dyad_id = ifelse(focal_id < partner,
                          paste(focal_id, partner, sep = "-"),
                          paste(partner, focal_id, sep = "-")),
         direction = ifelse(direction == "given", "given", "received"))

groom_long_EMER <- groom_long_EMER %>%
  dplyr::select(-groom_give_dur_total_sec)

groom_paired_EMER <- groom_long_EMER %>%
  pivot_wider(names_from = direction, values_from = duration_sec, values_fill = 0)

View(groom_paired_EMER)

n_distinct(groom_paired_EMER$dyad_id)

cat("EMER - Total Groom Given events:", sum(groom_paired_EMER$given > 0), "\n")
cat("EMER - Total Groom Received events:", sum(groom_paired_EMER$received > 0), "\n")
cat("EMER - Total grooming events:", nrow(groom_paired_EMER), "\n")

cat("Rows with duration_sec < 1 second:\n")
groom_long_EMER %>%
  filter(duration_sec < 1 & !is.na(duration_sec)) %>%
  select(focal_id, partner, dyad_id, direction, duration_sec) %>%
  arrange(direction, duration_sec) %>%
  print(n = Inf)

groom_paired_aggregated <- bind_rows(
  groom_paired %>% mutate(Group = "CERZ"),
  groom_paired_EMER %>% mutate(Group = "EMER")
) %>%
  group_by(dyad_id, Group) %>%
  summarise(
    Groom_Given    = sum(given, na.rm = TRUE),
    Groom_Received = sum(received, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Group, dyad_id)

groom_long_combined <- bind_rows(
  groom_long %>% mutate(Group = "CERZ"),
  groom_long_EMER %>% mutate(Group = "EMER")
)

groom_paired_focal <- groom_long_combined %>%
  group_by(dyad_id, file_id, Group, direction) %>%
 summarise(duration_sec = sum(duration_sec, na.rm = TRUE), .groups = "drop") %>%
pivot_wider(names_from = direction, values_from = duration_sec, values_fill = 0)

groom_paired_combined <- bind_rows(
  groom_paired %>% mutate(Group = "CERZ"),
  groom_paired_EMER %>% mutate(Group = "EMER")
) %>%
  arrange(Group, dyad_id)

groom_paired_combined <- groom_paired_combined %>%
  group_by(dyad_id, Group) %>%
  summarise(
    given    = sum(given, na.rm = TRUE),
    received = sum(received, na.rm = TRUE),
    .groups  = "drop"
  )

dyads_combined_obs <- bind_rows(
  dyads %>% select(dyad, total_seconds) %>% mutate(Group = "CERZ"),
  dyads_EMER %>% select(dyad, total_seconds) %>% mutate(Group = "EMER")
)

groom_paired_combined <- groom_paired_combined %>%
  mutate(dyad_id = str_replace_all(dyad_id, "-", "_"))

groom_paired_combined <- groom_paired_combined %>%
  left_join(dyads_combined_obs, by = c("dyad_id" = "dyad", "Group" = "Group"))

groom_paired_combined <- groom_paired_combined %>%
  mutate(
    observation_hours = total_seconds / 3600,
    given_rate        = given / observation_hours,
    received_rate     = received / observation_hours
  )

summary(groom_paired_aggregated$Groom_Given)
summary(groom_paired_aggregated$Groom_Received)

sum(groom_paired_aggregated$Groom_Given == 0)
sum(groom_paired_aggregated$Groom_Received == 0)

par(mfrow = c(1, 2))
hist(groom_paired_aggregated$Groom_Given,
     breaks = 20, main = "Groom Given", xlab = "Duration (sec)", col = "lightblue")
hist(groom_paired_aggregated$Groom_Received,
     breaks = 20, main = "Groom Received", xlab = "Duration (sec)", col = "lightpink")

cat("Given - Mean:", mean(groom_paired_aggregated$Groom_Given),
    "Variance:", var(groom_paired_aggregated$Groom_Given), "\n")
cat("Received - Mean:", mean(groom_paired_aggregated$Groom_Received),
    "Variance:", var(groom_paired_aggregated$Groom_Received), "\n")

cor(groom_paired_aggregated$Groom_Given, groom_paired_aggregated$Groom_Received,
    method = "spearman")

cat("Zeros in given:", sum(groom_paired_focal$given == 0),
    "out of", nrow(groom_paired_focal),
    "(", round(mean(groom_paired_focal$given == 0) * 100, 1), "%)\n")

cat("Zeros in received:", sum(groom_paired_focal$received == 0),
    "out of", nrow(groom_paired_focal),
    "(", round(mean(groom_paired_focal$received == 0) * 100, 1), "%)\n")

summary(groom_paired_focal$given)
summary(groom_paired_focal$received)

cat("Given - Mean:", mean(groom_paired_focal$given),
    "Variance:", var(groom_paired_focal$given), "\n")
cat("Received - Mean:", mean(groom_paired_focal$received),
    "Variance:", var(groom_paired_focal$received), "\n")

summary(groom_paired_combined$given)
summary(groom_paired_combined$given_rate)

cat("Zeros in given_rate:", sum(groom_paired_combined$given_rate == 0), "\n")
cat("Zeros in received_rate:", sum(groom_paired_combined$received_rate == 0), "\n")

p1rate <- ggplot(groom_paired_combined, aes(x = given)) +
  geom_histogram(bins = 30, fill = "#2c7bb6", colour = "white", alpha = 0.8) +
  labs(title = "Distribution of Grooming Given (seconds)",
       x = "Grooming Given", y = "Count") +
  theme_classic()

p2rate <- ggplot(groom_paired_combined, aes(x = given_rate)) +
  geom_histogram(bins = 30, fill = "#2c7bb6", colour = "white", alpha = 0.8) +
  labs(title = "Distribution of Grooming Given (rate)",
       x = "Grooming Given Rate", y = "Count") +
  theme_classic()

print(p1rate)
print(p2rate)

groom_paired_combined %>%
  group_by(Group) %>%
  summarise(
    n_dyads = n(),
    total_given = sum(given, na.rm = TRUE),
    total_received = sum(received, na.rm = TRUE),
    mean_given_rate = mean(given_rate, na.rm = TRUE),
    sd_given_rate = sd(given_rate, na.rm = TRUE),
    mean_received_rate = mean(received_rate, na.rm = TRUE),
    sd_received_rate = sd(received_rate, na.rm = TRUE),
    min_given_rate = min(given_rate, na.rm = TRUE),
    max_given_rate = max(given_rate, na.rm = TRUE)
  )

groom_paired_combined %>%
  summarise(
    n_dyads = n(),
    total_given = sum(given, na.rm = TRUE),
    total_received = sum(received, na.rm = TRUE),
    mean_given_rate = mean(given_rate, na.rm = TRUE),
    sd_given_rate = sd(given_rate, na.rm = TRUE),
    mean_received_rate = mean(received_rate, na.rm = TRUE),
    sd_received_rate = sd(received_rate, na.rm = TRUE)
  )

groom_give_receive_model <- glmmTMB(
  Groom_Given ~ Groom_Received + Group + (1 | dyad_id),
  family = tweedie(link = "log"),
  data = groom_paired_aggregated
)
summary(groom_give_receive_model)

groom_give_receive_null <- glmmTMB(
  Groom_Given ~ 1 + (1 | dyad_id),
  family = tweedie(link = "log"),
  data = groom_paired_aggregated
)

groom_give_receive_model_interaction <- glmmTMB(
  Groom_Given ~ Groom_Received * Group + (1 | dyad_id),
  family = tweedie(link = "log"),
  data = groom_paired_aggregated
)
summary(groom_give_receive_model_interaction)

groom_give_receive_model_focal <- glmmTMB(
  given ~ received + Group + (1 | dyad_id),
  family = tweedie(link = "log"),
  ziformula = ~1,
  data = groom_paired_focal
)

summary(groom_give_receive_model_focal)

groom_rate_model <- glmmTMB(
  given_rate ~ received_rate + Group + (1 | dyad_id),
  family = tweedie(link = "log"),
  data = groom_paired_combined
)

summary(groom_rate_model)

groom_rate_model_zi <- glmmTMB(
  given_rate ~ received_rate + Group + (1 | dyad_id),
  family = tweedie(link = "log"),
  ziformula = ~1,
  data = groom_paired_combined
)

summary(groom_rate_model_zi)

groom_rate_null <- glmmTMB(
  given_rate ~ 1 + (1 | dyad_id),
  family = tweedie(link = "log"),
  data = groom_paired_combined
)

summary(groom_rate_null)

anova(groom_give_receive_null, groom_give_receive_model)

simulationOutput <- simulateResiduals(fittedModel = groom_give_receive_model, n = 1000)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)

png("DHARMa_simulation_plot.png", width = 10, height = 6, units = "in", res = 300)
plot(simulationOutput)
dev.off()

pdf("DHARMa_tests.pdf", width = 10, height = 6)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
dev.off()

anova(groom_give_receive_model, groom_give_receive_model_interaction)

simulationOutput_interaction <- simulateResiduals(
  fittedModel = groom_give_receive_model_interaction, n = 1000)
plot(simulationOutput_interaction)

png("DHARMa_simulation_plot_interaction.png", width = 10, height = 6, units = "in", res = 300)
plot(simulationOutput_interaction)
dev.off()

plot(residuals(groom_give_receive_model_focal) ~ fitted(groom_give_receive_model_focal),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

png("residuals_vs_fitted_focal.png", width = 8, height = 6, units = "in", res = 300)
plot(residuals(groom_give_receive_model_focal) ~ fitted(groom_give_receive_model_focal),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)
dev.off()

qqnorm(residuals(groom_give_receive_model_focal))
qqline(residuals(groom_give_receive_model_focal), col = "red")

png("qq_plot_focal.png", width = 8, height = 6, units = "in", res = 300)
qqnorm(residuals(groom_give_receive_model_focal))
qqline(residuals(groom_give_receive_model_focal), col = "red")
dev.off()

sum(residuals(groom_give_receive_model_focal, type = "pearson")^2) /
  df.residual(groom_give_receive_model_focal)

obs_zeros <- sum(groom_paired_focal$given == 0)
pred_zeros <- sum(predict(groom_give_receive_model_focal, type = "response") < 0.5)
cat("Observed zeros:", obs_zeros, "\n")
cat("Predicted zeros:", pred_zeros, "\n")

AIC(groom_rate_model, groom_rate_model_zi)

anova(groom_rate_null, groom_rate_model)

AIC(groom_rate_model, groom_give_receive_model)

simulationOutput_rate <- simulateResiduals(
  fittedModel = groom_rate_model, n = 1000)
plot(simulationOutput_rate)

png("DHARMa_simulation_plot_rate.png", width = 10, height = 6, units = "in", res = 300)
plot(simulationOutput_rate)
dev.off()

testDispersion(simulationOutput_rate)
testZeroInflation(simulationOutput_rate)

setwd("C:/Users/User/Documents/publication stats/social bond")
source("convert_to_seconds.R")

CERZ_grooming_data_BASELINE <- CSI_CERZ %>%
  filter(!is.na(groom_give_id) | !is.na(groom_recv_id)) %>%
  select(file_id, date, focal_id, sex, age, groom_give_id, groom_give_start, groom_give_end, groom_give_dur_total,
         groom_recv_id, groom_recv_start, groom_recv_end, groom_recv_dur_total)

individual_obs <- CSI_CERZ %>%
  group_by(focal_id) %>%
  summarise(
    obs_count = n_distinct(file_id),
    obs_time_hours = obs_count * 0.5
  ) %>%
  ungroup()

write_xlsx(
  individual_obs,
  path = "CERZ_individual_observation_time.xlsx"
)

all_dyads_index <- bind_rows(
  CSI_CERZ %>%
    filter(!is.na(groom_give_id)) %>%
    transmute(
      focal_id,
      partner_id = groom_give_id,
      dyad_str = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
    ),
  CSI_CERZ %>%
    filter(!is.na(groom_recv_id)) %>%
    transmute(
      focal_id,
      partner_id = groom_recv_id,
      dyad_str = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
    )
) %>%
  distinct(dyad_str) %>%
  mutate(dyad_index_baseline = row_number())

CERZ_grooming_data_BASELINE <- CERZ_grooming_data_BASELINE %>%
  mutate(
    partner_id = coalesce(groom_give_id, groom_recv_id),
    dyad_str = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
  ) %>%
  left_join(all_dyads_index, by = "dyad_str") %>%
  group_by(dyad_index_baseline) %>%
  mutate(dyadic_grooming_eps = n()) %>%
  ungroup() %>%
  left_join(
    individual_obs %>% rename(focal_obs_time = obs_time_hours),
    by = "focal_id"
  ) %>%
  left_join(
    individual_obs %>% rename(partner_obs_time = obs_time_hours),
    by = c("partner_id" = "focal_id")
  ) %>%
  mutate(total_obs_time_hours = focal_obs_time + partner_obs_time) %>%
  group_by(dyad_index_baseline) %>%
  mutate(
    grooming_rate_pm_CERZ = dyadic_grooming_eps / total_obs_time_hours / 60,
    weighted_rate = grooming_rate_pm_CERZ * dyadic_grooming_eps
  )

CERZ_grooming_data_BASELINE %>%
  filter(dyad_index_baseline == 12) %>%
  summarise(total_grooming_interactions = n())

baseline_grooming_rate <- CERZ_grooming_data_BASELINE %>%
  group_by(dyad_index_baseline) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(
    total_weighted_rate = sum(weighted_rate, na.rm = TRUE),
    total_interactions  = sum(dyadic_grooming_eps, na.rm = TRUE),
    baseline_rate       = total_weighted_rate / total_interactions
  )

cat("Baseline Grooming Rate (grooming episodes per hour per minute):",
    baseline_grooming_rate$baseline_rate, "\n")

print(CERZ_grooming_data_BASELINE)

EMER_grooming_data_BASELINE <- CSI_EMER %>%
  filter(!is.na(groom_give_id) | !is.na(groom_recv_id)) %>%
  select(file_id, date, focal_id, sex, age, groom_give_id, groom_give_start, groom_give_end, groom_give_dur_total,
         groom_recv_id, groom_recv_start, groom_recv_end, groom_recv_dur_total)

individual_obs_EMER <- CSI_EMER %>%
  group_by(focal_id) %>%
  summarise(
    obs_count      = n_distinct(file_id),
    obs_time_hours = obs_count * 0.5
  ) %>%
  ungroup()

all_dyads_index_EMER <- bind_rows(
  CSI_EMER %>%
    filter(!is.na(groom_give_id)) %>%
    transmute(
      focal_id,
      partner_id = groom_give_id,
      dyad_str   = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
    ),
  CSI_EMER %>%
    filter(!is.na(groom_recv_id)) %>%
    transmute(
      focal_id,
      partner_id = groom_recv_id,
      dyad_str   = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
    )
) %>%
  distinct(dyad_str) %>%
  mutate(dyad_index_baseline = row_number())

EMER_grooming_data_BASELINE <- EMER_grooming_data_BASELINE %>%
  mutate(
    partner_id = coalesce(groom_give_id, groom_recv_id),
    dyad_str   = map2_chr(focal_id, partner_id, ~ paste(sort(c(.x, .y)), collapse = "_"))
  ) %>%
  left_join(all_dyads_index_EMER, by = "dyad_str") %>%
  group_by(dyad_index_baseline) %>%
  mutate(dyadic_grooming_eps = n()) %>%
  ungroup() %>%
  left_join(
    individual_obs_EMER %>% rename(focal_obs_time = obs_time_hours),
    by = "focal_id"
  ) %>%
  left_join(
    individual_obs_EMER %>% rename(partner_obs_time = obs_time_hours),
    by = c("partner_id" = "focal_id")
  ) %>%
  mutate(total_obs_time_hours = focal_obs_time + partner_obs_time) %>%
  group_by(dyad_index_baseline) %>%
  mutate(
    grooming_rate_pm_EMER = dyadic_grooming_eps / total_obs_time_hours / 60,
    weighted_rate         = grooming_rate_pm_EMER * dyadic_grooming_eps
  )

baseline_grooming_rate_EMER <- EMER_grooming_data_BASELINE %>%
  group_by(dyad_index_baseline) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(
    total_weighted_rate = sum(weighted_rate, na.rm = TRUE),
    total_interactions  = sum(dyadic_grooming_eps, na.rm = TRUE),
    baseline_rate       = total_weighted_rate / total_interactions
  )

cat("Baseline Grooming Rate for EMER (grooming episodes per hour per minute):",
    baseline_grooming_rate_EMER$baseline_rate, "\n")

write_xlsx(
  individual_obsEMER,
  path = "EMER_individual_observation_time.xlsx"
)

CERZ_grooming_data_dyadsFINALcsv1 <- read_csv(
  "CERZ_grooming_data_dyadsFINALcsv1.csv",
  show_col_types = FALSE
)

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  rowwise() %>%
  mutate(dyad = paste(sort(str_split(dyad, "_")[[1]]), collapse = "_")) %>%
  ungroup()

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  left_join(
    final_CSI %>% select(dyad, CSI_total, z_CSI),
    by = "dyad"
  )

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  group_by(dyad) %>%
  mutate(dyad_index = cur_group_id()) %>%
  ungroup()

testtable <- data.frame(Var1 = names(elo_ratings_over_time_filtered), Freq = elo_ratings_over_time_filtered)
CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1  %>%
      mutate(
        ELO = NA_character_
      )

    for (i in 1:nrow(testtable)) {
      keyword <- testtable$Var1[i]
      value <- testtable$Freq[i]
      CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
        mutate(
          ELO = ifelse(grepl(keyword, file_id, ignore.case = TRUE), value, ELO)
        )
    }

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  mutate(
    ELO_numeric = as.numeric(ELO),
    ELO_z       = as.vector(scale(ELO_numeric))
  ) %>%
  dplyr::select(-ELO_numeric)

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  dplyr::select(-any_of("relatedness")) %>%
  left_join(
    CSI_CERZ_relatedness_modeling %>% select(dyad, relatedness = relatedness_score),
    by = "dyad"
  ) %>%
  mutate(relatedness = replace_na(relatedness, 0))

CERZ_grooming_data_dyadsFINALcsv1$sex <- factor(CERZ_grooming_data_dyadsFINALcsv1$sex, levels = c("F", "M"))
class(CERZ_grooming_data_dyadsFINALcsv1$sex)

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  mutate(age_z = as.vector(scale(age)))

CERZ_grooming_data_dyadsFINALcsv1 <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  mutate(
    time = case_when(
      event == 1 ~ time_to_event,
      event == 0 ~ censored_time,
      TRUE ~ NA_real_
    )
  )

EMER_grooming_data_bayes <- read_csv(
  "EMER_grooming_data_bayes.csv",
  show_col_types = FALSE
)

EMER_grooming_data_bayes <- EMER_grooming_data_bayes[-c(1030, 1031, 1032), ]

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  rowwise() %>%
  mutate(dyad = paste(sort(str_split(dyad, "_")[[1]]), collapse = "_")) %>%
  ungroup()

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  left_join(
    final_CSI_EMER %>% select(dyad, CSI_total, z_CSI),
    by = "dyad"
  )

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  mutate(ELO = NA_character_)

for (i in 1:nrow(testtableEMER)) {
  keyword <- testtableEMER$Var1[i]
  value   <- testtableEMER$Freq[i]
  EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
    mutate(ELO = ifelse(grepl(keyword, file_id, ignore.case = TRUE), value, ELO))
}

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  mutate(
    ELO_numeric = as.numeric(ELO),
    ELO_z       = as.vector(scale(ELO_numeric))
  ) %>%
  dplyr::select(-ELO_numeric)

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  dplyr::select(-any_of("relatedness")) %>%
  left_join(
    CSI_EMER_relatedness_df %>% select(normalized_dyad, relatedness = relatedness_EMER),
    by = c("dyad" = "normalized_dyad")
  ) %>%
  mutate(relatedness = replace_na(relatedness, 0))

sum(is.na(EMER_grooming_data_bayes$relatedness))

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  mutate(age_z = as.vector(scale(age)))

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  mutate(
    time = case_when(
      event == 1 ~ time_to_event,
      event == 0 ~ censored_time,
      TRUE ~ NA_real_
    )
  )

EMER_grooming_data_bayes <- EMER_grooming_data_bayes %>%
  filter(!is.na(time))

sum(CERZ_grooming_data_dyadsFINALcsv1$event == 1, na.rm = TRUE)

sum(CERZ_grooming_data_dyadsFINALcsv1$censored == 1, na.rm = TRUE)

min(CERZ_grooming_data_dyadsFINALcsv1$time_to_event, na.rm = TRUE)

max(CERZ_grooming_data_dyadsFINALcsv1$time_to_event, na.rm = TRUE)

mean(CERZ_grooming_data_dyadsFINALcsv1$time_to_event, na.rm = TRUE)

min(CERZ_grooming_data_dyadsFINALcsv1$censored_time, na.rm = TRUE)
max(CERZ_grooming_data_dyadsFINALcsv1$censored_time, na.rm = TRUE)
mean(CERZ_grooming_data_dyadsFINALcsv1$censored_time, na.rm = TRUE)

CERZ_grooming_data_dyads %>%
  summarise(n_dyads_raw = n_distinct(dyad),
            n_dyads_sorted = n_distinct(map_chr(dyad, ~ paste(sort(str_split(.x, "_")[[1]]), collapse = "_"))))

sum(EMER_grooming_data_bayes$event == 1, na.rm = TRUE)

sum(EMER_grooming_data_bayes$censored == 1, na.rm = TRUE)

min(EMER_grooming_data_bayes$time_to_event, na.rm = TRUE)

max(EMER_grooming_data_bayes$time_to_event, na.rm = TRUE)

mean(EMER_grooming_data_bayes$time_to_event, na.rm = TRUE)

min(EMER_grooming_data_bayes$censored_time, na.rm = TRUE)

max(EMER_grooming_data_bayes$censored_time, na.rm = TRUE)

mean(EMER_grooming_data_bayes$censored_time, na.rm = TRUE)

EMER_grooming_data_bayes %>%
  summarise(n_dyads_raw = n_distinct(dyad),
            n_dyads_sorted = n_distinct(map_chr(dyad, ~ paste(sort(str_split(.x, "_")[[1]]), collapse = "_"))))

EMER_grooming_data_bayes %>%
  filter(event == 1) %>%
  arrange(desc(time_to_event)) %>%
  select(file_id, dyad, time_to_event, event) %>%
  mutate(time_to_event_min = time_to_event / 60) %>%
  head(10)

CERZ_grooming_data_dyadsFINALcsv1 %>%
  filter(event == 1) %>%
  arrange(desc(time_to_event)) %>%
  select(file_id, dyad, time_to_event, event) %>%
  mutate(time_to_event_min = time_to_event / 60) %>%
  head(10)

censoring_comparison <- bind_rows(
  CERZ_grooming_data_dyadsFINALcsv1 %>%
    filter(event == 0) %>%
    select(censored_time) %>%
    mutate(group = "CERZ"),
  EMER_grooming_data_bayes %>%
    filter(event == 0) %>%
    select(censored_time) %>%
    mutate(group = "EMER")
)

print(censoring_comparison)

censoring_comparison %>%
  group_by(group) %>%
  summarise(
    n_censored   = n(),
    min_sec      = min(censored_time, na.rm = TRUE),
    max_sec      = max(censored_time, na.rm = TRUE),
    mean_sec     = mean(censored_time, na.rm = TRUE),
    sd_sec       = sd(censored_time, na.rm = TRUE),
    q25_sec      = quantile(censored_time, 0.25, na.rm = TRUE),
    q75_sec      = quantile(censored_time, 0.75, na.rm = TRUE),
    q95_sec      = quantile(censored_time, 0.95, na.rm = TRUE),
    min_min      = min_sec / 60,
    max_min      = max_sec / 60,
    mean_min     = mean_sec / 60,
    q95_min      = q95_sec / 60
  )

bind_rows(
  CERZ_grooming_data_dyadsFINALcsv1 %>%
    filter(event == 0) %>%
    select(censored_time) %>%
    mutate(group = "CERZ",
           censored_min = censored_time / 60),
  EMER_grooming_data_bayes %>%
    filter(event == 0) %>%
    select(censored_time) %>%
    mutate(group = "EMER",
           censored_min = censored_time / 60)
) %>%
  group_by(group) %>%
  slice_max(censored_time, n = 20) %>%
  arrange(group, desc(censored_time)) %>%
  print(n = 40)

cat("CERZ max censored time:", max(CERZ_grooming_data_dyadsFINALcsv1$censored_time[CERZ_grooming_data_dyadsFINALcsv1$event == 0], na.rm = TRUE) / 60, "minutes\n")
cat("EMER max censored time:", max(EMER_grooming_data_bayes$censored_time[EMER_grooming_data_bayes$event == 0], na.rm = TRUE) / 60, "minutes\n")

EMER_grooming_data_bayes %>%
  filter(event == 0) %>%
  summarise(
    total_censored = n(),
    above_180 = sum(censored_time > 180 * 60, na.rm = TRUE),
    pct_lost = round(above_180 / total_censored * 100, 1)
  )

CERZ_grooming_data_dyadsFINALcsv1_combined <- CERZ_grooming_data_dyadsFINALcsv1 %>%
  filter(
    (event == 1 & time_to_event <= 180 * 60) |
    (event == 0 & censored_time <= 180 * 60)
  )
str(CERZ_grooming_data_dyadsFINALcsv1_combined)
CERZ_grooming_data_dyadsFINALcsv1_combined %>%
  filter(event == 1) %>%
  slice_max(time_to_event, n = 10) %>%
  select(file_id, dyad, time_to_event, event) %>%
  mutate(time_to_event_min = round(time_to_event / 60, 2))

CERZ_grooming_data_dyadsFINALcsv1_combined %>%
  filter(event == 0) %>%
  slice_max(censored_time, n = 10) %>%
  select(file_id, dyad, censored_time, event) %>%
  mutate(censored_min = round(censored_time / 60, 2))

cat("Total rows:", nrow(CERZ_grooming_data_dyadsFINALcsv1_combined), "\n")
cat("Total dyads:", n_distinct(CERZ_grooming_data_dyadsFINALcsv1_combined$dyad), "\n")
cat("Reciprocation events (event = 1):", sum(CERZ_grooming_data_dyadsFINALcsv1_combined$event == 1), "\n")
cat("Censored observations (event = 0):", sum(CERZ_grooming_data_dyadsFINALcsv1_combined$event == 0), "\n")
cat("Mean reciprocation latency (sec):", round(mean(CERZ_grooming_data_dyadsFINALcsv1_combined$time_to_event, na.rm = TRUE), 2), "\n")
cat("Mean reciprocation latency (min):", round(mean(CERZ_grooming_data_dyadsFINALcsv1_combined$time_to_event, na.rm = TRUE) / 60, 2), "\n")
sd(CERZ_grooming_data_dyadsFINALcsv1_combined$time_to_event, na.rm = TRUE) / 60

surv_obj <- Surv(time = CERZ_grooming_data_dyadsFINALcsv1_combined$time,
                 event = CERZ_grooming_data_dyadsFINALcsv1_combined$event)

plot(surv_obj)

km_fit <- survfit(surv_obj ~ 1)
summary(km_fit)

km_fit_sex <- survfit(surv_obj ~ sex,
                      data = CERZ_grooming_data_dyadsFINALcsv1_combined)

ggsurvplot(km_fit_sex,
           data          = CERZ_grooming_data_dyadsFINALcsv1_combined,
           conf.int      = TRUE,
           pval          = TRUE,
           risk.table    = TRUE,
           title         = "CERZ Kaplan-Meier Survival Curve by Sex",
           xlab          = "Time after end of received grooming (sec)",
           ylab          = "Probability of no reciprocation",
           break.time.by = 1000,
           legend.labs   = c("Female", "Male"),
           palette       = c("red", "blue"))

png("CERZ_KM_Sex.png", width = 3000, height = 2400, res = 300)
print(ggsurvplot(km_fit_sex,
                 data          = CERZ_grooming_data_dyadsFINALcsv1_combined,
                 conf.int      = TRUE,
                 pval          = TRUE,
                 risk.table    = TRUE,
                 title         = "CERZ Kaplan-Meier Survival Curve by Sex",
                 xlab          = "Time after end of received grooming (sec)",
                 ylab          = "Probability of no reciprocation",
                 break.time.by = 1000,
                 legend.labs   = c("Female", "Male"),
                 palette       = c("red", "blue")))
dev.off()

km_fit_hazard <- km_fit
plot(km_fit_hazard, fun = "cumhaz",
     main = "Cumulative Hazard Plot (from Kaplan-Meier)",
     xlab = "Time",
     ylab = "Cumulative Hazard H(t)",
     conf.int = TRUE)

smooth_hazard <- muhaz(CERZ_grooming_data_dyadsFINALcsv1_combined$time / 60,
                       CERZ_grooming_data_dyadsFINALcsv1_combined$event,
                       bw.method = "global")

hazard_times  <- smooth_hazard$est.grid
hazard_values <- smooth_hazard$haz.est
n_obs         <- smooth_hazard$pin$nobs
hazard_se     <- sqrt(hazard_values / (n_obs * smooth_hazard$bw.glob))
ci_upper      <- hazard_values + 1.96 * hazard_se
ci_lower      <- pmax(hazard_values - 1.96 * hazard_se, 0)

crossing_index    <- which(diff(sign(hazard_values - 0.04)) != 0)
crossing_time_min <- hazard_times[crossing_index] +
  (0.04 - hazard_values[crossing_index]) /
  (hazard_values[crossing_index + 1] - hazard_values[crossing_index]) *
  (hazard_times[crossing_index + 1] - hazard_times[crossing_index])
crossing_minutes  <- floor(crossing_time_min)
crossing_seconds  <- round((crossing_time_min - crossing_minutes) * 60)
cat("Crossing point:", crossing_time_min, "minutes\n")
cat("Crossing point:", crossing_minutes, "min", crossing_seconds, "sec\n")

y_max <- max(ci_upper) * 1.1

plot(smooth_hazard,
     main     = "CERZ Smooth Hazard Rate of Grooming Reciprocation",
     xlab     = "Time after end of received grooming (min)",
     ylab     = "Rate of reciprocation (eps/min)",
     col      = "red",
     lwd      = 2,
     ylim     = c(0, y_max),
     xlim     = c(0, max(hazard_times)),
     xaxt     = "n",
     cex.main = 0.9,
     cex.lab  = 0.85,
     cex.axis = 0.85)

x_breaks <- seq(0, max(hazard_times), by = 10)
axis(1, at = x_breaks, cex.axis = 0.85)

lines(hazard_times, ci_upper, lty = 2, col = "black", lwd = 1.5)
lines(hazard_times, ci_lower, lty = 2, col = "black", lwd = 1.5)

abline(h = 0.04, lty = 3, lwd = 1.5, col = "black")

legend("topright",
       legend = c("Smooth hazard function", "95% Confidence Interval", "Baseline rate"),
       col    = c("red", "black", "black"),
       lty    = c(1, 2, 3),
       lwd    = c(2, 1.5, 1.5),
       bty    = "n",
       cex    = 0.8)

data.frame(
  time_min = round(hazard_times[1:20], 3),
  hazard   = round(hazard_values[1:20], 4),
  baseline = 0.04
)

short_term_threshold_CERZ <- (3 * 60) + 7

short_term_CERZ <- sum(CERZ_grooming_data_dyadsFINALcsv1_combined$event == 1 &
                       CERZ_grooming_data_dyadsFINALcsv1_combined$time_to_event <= short_term_threshold_CERZ, na.rm = TRUE)

long_term_CERZ  <- sum(CERZ_grooming_data_dyadsFINALcsv1_combined$event == 1 &
                       CERZ_grooming_data_dyadsFINALcsv1_combined$time_to_event > short_term_threshold_CERZ, na.rm = TRUE)

total_events_CERZ <- short_term_CERZ + long_term_CERZ

cat("Total reciprocation events:", total_events_CERZ, "\n")
cat("Short term (<=3min 7sec):", short_term_CERZ, "(", round(short_term_CERZ / total_events_CERZ * 100, 1), "%)\n")
cat("Long term (>3min 7sec):", long_term_CERZ, "(", round(long_term_CERZ / total_events_CERZ * 100, 1), "%)\n")

data_female <- CERZ_grooming_data_dyadsFINALcsv1_combined %>% filter(sex == "F")
data_male   <- CERZ_grooming_data_dyadsFINALcsv1_combined %>% filter(sex == "M")

smooth_hazard_F <- muhaz(data_female$time / 60, data_female$event, bw.method = "global")
smooth_hazard_M <- muhaz(data_male$time / 60,   data_male$event,   bw.method = "global")

hazard_times_F  <- smooth_hazard_F$est.grid
hazard_values_F <- smooth_hazard_F$haz.est
hazard_se_F     <- sqrt(hazard_values_F / (smooth_hazard_F$pin$nobs * smooth_hazard_F$bw.glob))
ci_upper_F      <- hazard_values_F + 1.96 * hazard_se_F
ci_lower_F      <- pmax(hazard_values_F - 1.96 * hazard_se_F, 0)

hazard_times_M  <- smooth_hazard_M$est.grid
hazard_values_M <- smooth_hazard_M$haz.est
hazard_se_M     <- sqrt(hazard_values_M / (smooth_hazard_M$pin$nobs * smooth_hazard_M$bw.glob))
ci_upper_M      <- hazard_values_M + 1.96 * hazard_se_M
ci_lower_M      <- pmax(hazard_values_M - 1.96 * hazard_se_M, 0)

y_max_sex <- max(c(ci_upper_F, ci_upper_M), na.rm = TRUE) * 1.1

png("CERZ_Smooth_Hazard_Plot_bySex.png", width = 3000, height = 2000, res = 300)

plot(smooth_hazard_F,
     main     = "CERZ Smooth Hazard Rate of Grooming Reciprocation by Sex",
     xlab     = "Time after end of received grooming (min)",
     ylab     = "Rate of reciprocation (eps/min)",
     col      = "red", lwd = 2,
     ylim     = c(0, y_max_sex),
     xlim     = c(0, max(c(hazard_times_F, hazard_times_M))),
     xaxt     = "n",
     cex.main = 0.9,
     cex.lab  = 0.85,
     cex.axis = 0.85)

x_breaks_sex <- seq(0, max(c(hazard_times_F, hazard_times_M)), by = 10)
axis(1, at = x_breaks_sex, cex.axis = 0.85)

lines(hazard_times_F, ci_upper_F, lty = 2, col = "red",  lwd = 1.5)
lines(hazard_times_F, ci_lower_F, lty = 2, col = "red",  lwd = 1.5)

lines(hazard_times_M, hazard_values_M, col = "blue", lwd = 2)
lines(hazard_times_M, ci_upper_M,      lty = 2, col = "blue", lwd = 1.5)
lines(hazard_times_M, ci_lower_M,      lty = 2, col = "blue", lwd = 1.5)

abline(h = 0.04, lty = 3, lwd = 1.5, col = "black")

legend("topright",
       legend = c("Female", "Female 95% CI", "Male", "Male 95% CI", "Baseline (0.04)"),
       col    = c("red", "red", "blue", "blue", "black"),
       lwd    = c(2, 1.5, 2, 1.5, 1.5),
       lty    = c(1, 2, 1, 2, 3),
       bty    = "n",
       cex    = 0.8)

dev.off()

EMER_grooming_data_bayes_recip_cens_edited <- EMER_grooming_data_bayes %>%
  filter(
    (event == 1 & time_to_event <= 180 * 60) |
    (event == 0 & censored_time <= 180 * 60)
  )

cat("Total rows:", nrow(EMER_grooming_data_bayes_recip_cens_edited), "\n")
cat("Total dyads:", n_distinct(EMER_grooming_data_bayes_recip_cens_edited$dyad), "\n")
cat("Reciprocation events (event = 1):", sum(EMER_grooming_data_bayes_recip_cens_edited$event == 1), "\n")
cat("Censored observations (event = 0):", sum(EMER_grooming_data_bayes_recip_cens_edited$event == 0), "\n")
cat("Mean reciprocation latency (sec):", round(mean(EMER_grooming_data_bayes_recip_cens_edited$time_to_event, na.rm = TRUE), 2), "\n")
cat("Mean reciprocation latency (min):", round(mean(EMER_grooming_data_bayes_recip_cens_edited$time_to_event, na.rm = TRUE) / 60, 2), "\n")
sd(EMER_grooming_data_bayes_recip_cens_edited$time_to_event, na.rm = TRUE) / 60

EMER_grooming_data_bayes_recip_cens_edited %>%
  filter(event == 0) %>%
  slice_max(censored_time, n = 10) %>%
  select(file_id, dyad, censored_time, event) %>%
  mutate(censored_min = round(censored_time / 60, 2))

cat("Original censored:", sum(EMER_grooming_data_bayes$event == 0, na.rm = TRUE), "\n")
cat("Trimmed censored:", sum(EMER_grooming_data_bayes_recip_cens_edited$event == 0, na.rm = TRUE), "\n")
cat("Removed:", sum(EMER_grooming_data_bayes$event == 0, na.rm = TRUE) - sum(EMER_grooming_data_bayes_recip_cens_edited$event == 0, na.rm = TRUE), "\n")

surv_objEMER <- Surv(time = EMER_grooming_data_bayes_recip_cens_edited$time,
                     event = EMER_grooming_data_bayes_recip_cens_edited$event)
plot(surv_objEMER)

km_fitEMER <- survfit(surv_objEMER ~ 1)
summary(km_fitEMER)

ggsurvplot(km_fitEMER, data = EMER_grooming_data_bayes_recip_cens_edited,
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,
           title = "Kaplan-Meier Survival Curve",
           xlab = "Reciprocation duration - secs",
           ylab = "Grooming Probability",
           break.time.by = 2000)

km_fitEMER_sex <- survfit(surv_objEMER ~ sex,
                          data = EMER_grooming_data_bayes_recip_cens_edited)

ggsurvplot(km_fitEMER_sex,
           data          = EMER_grooming_data_bayes_recip_cens_edited,
           conf.int      = TRUE,
           pval          = TRUE,
           risk.table    = TRUE,
           title         = "EMER Kaplan-Meier Survival Curve by Sex",
           xlab          = "Time after end of received grooming (sec)",
           ylab          = "Probability of no reciprocation",
           break.time.by = 1000,
           legend.labs   = c("Female", "Male"),
           palette       = c("red", "blue"))

png("EMER_KM_Sex.png", width = 3000, height = 2400, res = 300)
print(ggsurvplot(km_fitEMER_sex,
                 data          = EMER_grooming_data_bayes_recip_cens_edited,
                 conf.int      = TRUE,
                 pval          = TRUE,
                 risk.table    = TRUE,
                 title         = "EMER Kaplan-Meier Survival Curve by Sex",
                 xlab          = "Time after end of received grooming (sec)",
                 ylab          = "Probability of no reciprocation",
                 break.time.by = 1000,
                 legend.labs   = c("Female", "Male"),
                 palette       = c("red", "blue")))
dev.off()

smooth_hazard_EMER <- muhaz(EMER_grooming_data_bayes_recip_cens_edited$time / 60,
                            EMER_grooming_data_bayes_recip_cens_edited$event,
                            bw.method = "global")

hazard_times_EMER  <- smooth_hazard_EMER$est.grid
hazard_values_EMER <- smooth_hazard_EMER$haz.est
n_obs_EMER         <- smooth_hazard_EMER$pin$nobs
hazard_se_EMER     <- sqrt(hazard_values_EMER / (n_obs_EMER * smooth_hazard_EMER$bw.glob))
ci_upper_EMER      <- hazard_values_EMER + 1.96 * hazard_se_EMER
ci_lower_EMER      <- pmax(hazard_values_EMER - 1.96 * hazard_se_EMER, 0)

crossing_index <- which(hazard_values_EMER < 0.054)[1]

crossing_time_min_EMER <- hazard_times_EMER[crossing_index - 1] +
  (0.054 - hazard_values_EMER[crossing_index - 1]) /
  (hazard_values_EMER[crossing_index] - hazard_values_EMER[crossing_index - 1]) *
  (hazard_times_EMER[crossing_index] - hazard_times_EMER[crossing_index - 1])

crossing_minutes_EMER <- floor(crossing_time_min_EMER)
crossing_seconds_EMER <- round((crossing_time_min_EMER - crossing_minutes_EMER) * 60)
cat("EMER Crossing point:", crossing_minutes_EMER, "min", crossing_seconds_EMER, "sec\n")

y_max_EMER <- max(ci_upper_EMER) * 1.1

plot(smooth_hazard_EMER,
     main     = "EMER Smooth Hazard Rate of Grooming Reciprocation",
     xlab     = "Time after end of received grooming (min)",
     ylab     = "Rate of reciprocation (eps/min)",
     col      = "red",
     lwd      = 2,
     ylim     = c(0, y_max_EMER),
     xlim     = c(0, max(hazard_times_EMER)),
     xaxt     = "n",
     cex.main = 0.9,
     cex.lab  = 0.85,
     cex.axis = 0.85)

x_breaks_EMER <- seq(0, max(hazard_times_EMER), by = 10)
axis(1, at = x_breaks_EMER, cex.axis = 0.85)

lines(hazard_times_EMER, ci_upper_EMER, lty = 2, col = "black", lwd = 1.5)
lines(hazard_times_EMER, ci_lower_EMER, lty = 2, col = "black", lwd = 1.5)

abline(h = 0.054, lty = 3, lwd = 1.5, col = "black")

legend("topright",
       legend = c("Smooth hazard function", "95% Confidence Interval", "Baseline rate"),
       col    = c("red", "black", "black"),
       lty    = c(1, 2, 3),
       lwd    = c(2, 1.5, 1.5),
       bty    = "n",
       cex    = 0.8)

data.frame(
  time_min = round(hazard_times_EMER[1:3], 3),
  hazard   = round(hazard_values_EMER[1:3], 4),
  baseline = 0.054
)

short_term_threshold_EMER <- 34

short_term_EMER <- sum(EMER_grooming_data_bayes_recip_cens_edited$event == 1 &
                       EMER_grooming_data_bayes_recip_cens_edited$time_to_event <= short_term_threshold_EMER, na.rm = TRUE)

long_term_EMER  <- sum(EMER_grooming_data_bayes_recip_cens_edited$event == 1 &
                       EMER_grooming_data_bayes_recip_cens_edited$time_to_event > short_term_threshold_EMER, na.rm = TRUE)

total_events_EMER <- short_term_EMER + long_term_EMER

cat("Total reciprocation events:", total_events_EMER, "\n")
cat("Short term (<=34 sec):", short_term_EMER, "(", round(short_term_EMER / total_events_EMER * 100, 1), "%)\n")
cat("Long term (>34 sec):", long_term_EMER, "(", round(long_term_EMER / total_events_EMER * 100, 1), "%)\n")

png("EMER_Smooth_Hazard_Plot.png", width = 3000, height = 2000, res = 300)

plot(smooth_hazard_EMER,
     main     = "EMER Smooth Hazard Rate of Grooming Reciprocation",
     xlab     = "Time after end of received grooming (min)",
     ylab     = "Rate of reciprocation (eps/min)",
     col      = "red",
     lwd      = 2,
     ylim     = c(0, y_max_EMER),
     xlim     = c(0, max(hazard_times_EMER)),
     xaxt     = "n",
     cex.main = 0.9,
     cex.lab  = 0.85,
     cex.axis = 0.85)

x_breaks_EMER <- seq(0, max(hazard_times_EMER), by = 10)
axis(1, at = x_breaks_EMER, cex.axis = 0.85)
lines(hazard_times_EMER, ci_upper_EMER, lty = 2, col = "black", lwd = 1.5)
lines(hazard_times_EMER, ci_lower_EMER, lty = 2, col = "black", lwd = 1.5)
abline(h = 0.054, lty = 3, lwd = 1.5, col = "black")
legend("topright",
       legend = c("Smooth hazard function", "95% Confidence Interval", "Baseline rate"),
       col    = c("red", "black", "black"),
       lty    = c(1, 2, 3),
       lwd    = c(2, 1.5, 1.5),
       bty    = "n",
       cex    = 0.8)

dev.off()

CERZ_grooming_data_dyads <- CERZ_grooming_data_dyadsFINALcsv1_combined

CERZ_grooming_data_dyads %>%
  summarise(
    group = "CERZ",
    max_censored_time = max(censored_time, na.rm = TRUE),
    max_censored_mins = round(max(censored_time, na.rm = TRUE) / 60, 2),
    n_censored = sum(censored == 1, na.rm = TRUE)
  )

EMER_grooming_data_bayes_FINALMODEL <- EMER_grooming_data_bayes_recip_cens_edited

EMER_grooming_data_bayes_FINALMODEL %>%
  summarise(
    group = "EMER",
    max_censored_time = max(censored_time, na.rm = TRUE),
    max_censored_mins = round(max(censored_time, na.rm = TRUE) / 60, 2),
    n_censored = sum(censored == 1, na.rm = TRUE)
  )

CERZ_grooming_data_dyads <- CERZ_grooming_data_dyads %>%
  mutate(
    group_ID = "CERZ",
    focal_id = str_split_fixed(dyad, "_", 2)[, 1]
  )

EMER_grooming_data_bayes_FINALMODEL <- EMER_grooming_data_bayes_FINALMODEL %>%
  mutate(group_ID = "EMER")

CERZ_EMER_grooming_data_combined <- bind_rows(
  CERZ_grooming_data_dyads,
  EMER_grooming_data_bayes_FINALMODEL
)

CERZ_EMER_grooming_data_combined <- CERZ_EMER_grooming_data_combined %>%
  mutate(
    ind1 = str_split_fixed(dyad, "_", 2)[, 1],
    ind2 = str_split_fixed(dyad, "_", 2)[, 2],
    partner_id = if_else(focal_id == ind1, ind2, ind1)
  ) %>%
  select(-ind1, -ind2)

CERZ_EMER_grooming_data_combined <- CERZ_EMER_grooming_data_combined %>%
  mutate(dyad_index = as.integer(factor(dyad)))

CERZ_EMER_grooming_data_combined %>%
  summarise(
    n_unique_dyad_index = n_distinct(dyad_index),
    max_dyad_index = max(dyad_index)
  )
CERZ_EMER_grooming_data_combined %>%
  group_by(group_ID) %>%
  summarise(n_dyads = n_distinct(dyad))

str(CERZ_EMER_grooming_data_combined)

CERZ_EMER_grooming_data_combined %>%
  summarise(
    n_total = n(),
    n_dyads = n_distinct(dyad),
    n_events = sum(event == 1, na.rm = TRUE),
    n_censored = sum(censored == 1, na.rm = TRUE),
    time_min = min(time_to_event, na.rm = TRUE),
    time_max = max(time_to_event, na.rm = TRUE),
    time_mean = mean(time_to_event, na.rm = TRUE),
    time_median = median(time_to_event, na.rm = TRUE),
    time_sd = sd(time_to_event, na.rm = TRUE),
    time_q25 = quantile(time_to_event, 0.25, na.rm = TRUE),
    time_q75 = quantile(time_to_event, 0.75, na.rm = TRUE),
    time_q95 = quantile(time_to_event, 0.95, na.rm = TRUE),
    time_q99 = quantile(time_to_event, 0.99, na.rm = TRUE),
    n_NA = sum(is.na(time_to_event))
  )

CERZ_EMER_grooming_data_combined %>%
  filter(censored == 1) %>%
  select(group_ID, dyad, censored_time) %>%
  mutate(time_minutes = censored_time / 60) %>%
  arrange(desc(time_minutes)) %>%
  head(10)

CERZ_EMER_grooming_data_combined %>%
  filter(event == 1) %>%
  select(group_ID, dyad, time_to_event) %>%
  mutate(time_minutes = time_to_event / 60) %>%
  arrange(desc(time_minutes)) %>%
  head(10)

CERZ_EMER_grooming_data_combined %>%
  filter(event == 1) %>%
  mutate(time_minutes = time_to_event / 60) %>%
  ggplot(aes(x = time_minutes)) +
  geom_histogram(binwidth = 5, fill = "#2c7bb6", colour = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count) * 5), colour = "#d7191c", linewidth = 0.8) +
  facet_wrap(~ group_ID, ncol = 1) +
  labs(
    title = "Distribution of Reciprocation Latency",
    x = "Time to Reciprocation (minutes)",
    y = "Count"
  ) +
  theme_classic()

CERZ_EMER_grooming_data_combined %>%
  filter(event == 1) %>%
  mutate(log_time = log(time_to_event)) %>%
  ggplot(aes(x = log_time)) +
  geom_histogram(binwidth = 0.3, fill = "#2c7bb6", colour = "white", alpha = 0.8) +
  geom_density(aes(y = after_stat(count) * 0.3), colour = "#d7191c", linewidth = 0.8) +
  facet_wrap(~ group_ID, ncol = 1) +
  labs(
    title = "Log-Transformed Reciprocation Latency",
    x = "log(Time to Reciprocation) (seconds)",
    y = "Count"
  ) +
  theme_classic()

CERZ_EMER_grooming_data_combined %>%
  filter(event == 1) %>%
  select(group_ID, dyad, time_to_event) %>%
  mutate(time_minutes = round(time_to_event / 60, 3),
         time_seconds = round(time_to_event, 2)) %>%
  arrange(time_to_event) %>%
  head(20)

my_priors_lognormal <- c(
  prior(normal(log(195), 0.5), class = "Intercept"),
  prior(normal(1, 0.5), class = "sigma")
)

my_inits_mm_lognormal <- function() {
  list(
    Intercept = log(195),
    sigma = 1.0
  )
}

CERZ_EMER_brm_hurdle_lognormal <- brm(
  time | cens(censored) ~ z_CSI +
    (1 | mm(focal_id, partner_id)) +
    (1 | dyad_index) +
    (1 | group_ID),
  data = CERZ_EMER_grooming_data_combined,
  family = hurdle_lognormal(link = "identity"),
  prior = my_priors_lognormal,
  init = my_inits_mm_lognormal,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4,
  seed = 123
)

summary(CERZ_EMER_brm_hurdle_lognormal)

CERZ_EMER_brm_null <- brm(
  time | cens(censored) ~ 1 +
    (1 | mm(focal_id, partner_id)) +
    (1 | dyad_index) +
    (1 | group_ID),
  data = CERZ_EMER_grooming_data_combined,
  family = hurdle_lognormal(link = "identity"),
  prior = my_priors_lognormal,
  init = my_inits_mm_lognormal,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4,
  seed = 123
)

loo_full <- loo(CERZ_EMER_brm_hurdle_lognormal)
loo_null <- loo(CERZ_EMER_brm_null)
loo_compare(loo_full, loo_null)

pp_check(CERZ_EMER_brm_hurdle_lognormal, type = "dens_overlay", ndraws = 50) +
  coord_cartesian(xlim = c(0, 12000)) +
  labs(title = "PPC: Observed vs Replicated Latencies Hurdle Lognormal",
       x = "Time (seconds)", y = "Density") +
  theme_classic()

ggsave(
  filename = "PPC_dens_overlay_hurdle_lognormal.png",
  plot = pp_check(CERZ_EMER_brm_hurdle_lognormal, type = "dens_overlay", ndraws = 50) +
    coord_cartesian(xlim = c(0, 12000)) +
    labs(title = "PPC: Observed vs Replicated Latencies Hurdle Lognormal",
         x = "Time (seconds)", y = "Density") +
    theme_classic(),
  width = 8, height = 6, dpi = 300, units = "in"
)

pp_check(CERZ_EMER_brm_hurdle_lognormal, type = "stat", stat = "mean", ndraws = 500) +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(title = "PPC: Mean Latency",
       x = "Mean Latency (seconds)", y = "Frequency") +
  theme_classic()

plot(CERZ_EMER_brm_hurdle_lognormal)

plot(loo_full, diagnostic = "k", label_points = TRUE)

my_inits_gamma <- function() {
  list(
    Intercept = log(195),
    shape = 0.5
  )
}

CERZ_EMER_brm_gamma_v2 <- brm(
  time | cens(censored) ~ z_CSI +
    (1 | mm(focal_id, partner_id)) +
    (1 | dyad_index) +
    (1 | group_ID),
  data = CERZ_EMER_grooming_data_combined,
  family = Gamma(link = "log"),
  prior = c(
    prior(normal(log(195), 0.5), class = "Intercept"),
    prior(gamma(2, 0.1), class = "shape"),
    prior(student_t(3, 0, 1), class = "sd")
  ),
  init = my_inits_gamma,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores = 4,
  seed = 123
)

summary(CERZ_EMER_brm_gamma_v2)

pp_check(CERZ_EMER_brm_gamma_v2, type = "dens_overlay", ndraws = 50) +
  coord_cartesian(xlim = c(0, 12000)) +
  labs(title = "PPC: Observed vs Replicated (Gamma v2)",
       x = "Time (seconds)", y = "Density") +
  theme_gray()

ggsave(
  filename = "PPC_dens_overlay_gamma_v2.png",
  plot = pp_check(CERZ_EMER_brm_gamma_v2, type = "dens_overlay", ndraws = 50) +
    coord_cartesian(xlim = c(0, 12000)) +
    labs(title = "PPC: Observed vs Replicated (Gamma v2)",
         x = "Time (seconds)", y = "Density") +
    theme_gray(),
  width = 8, height = 6, dpi = 300, units = "in"
)

pp_check(CERZ_EMER_brm_gamma_v2, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (Gamma v2)",
       x = "Mean Latency (seconds)", y = "Frequency") +
  theme_gray()

pp_check(CERZ_EMER_brm_gamma_v2, type = "stat", stat = "median", ndraws = 500) +
  labs(title = "PPC: Median Latency (Gamma v2)",
       x = "Median Latency (seconds)", y = "Frequency") +
  theme_gray()

loo_gamma_v2 <- loo(CERZ_EMER_brm_gamma_v2)
loo_compare(loo_full, loo_null, loo_gamma_v2)

conditional_effects(CERZ_EMER_brm_gamma_v2, effects = "z_CSI") %>%
  plot(plot = FALSE) %>%
  .[[1]] +
  scale_y_continuous(
    limits = c(0, 3000),
    breaks = seq(0, 3000, by = 500),
    labels = seq(0, 3000, by = 500)
  ) +
  labs(
    x = "Social Bond Score",
    y = "Event and Censored Duration (seconds)"
  ) +
  theme_gray()

ggsave(
  filename = "conditional_effects_z_CSI_gamma_v2.png",
  plot = conditional_effects(CERZ_EMER_brm_gamma_v2, effects = "z_CSI") %>%
    plot(plot = FALSE) %>%
    .[[1]] +
    scale_y_continuous(
      limits = c(0, 3000),
      breaks = seq(0, 3000, by = 500),
      labels = seq(0, 3000, by = 500)
    ) +
    labs(
      x = "Social Bond Score",
      y = "Event and Censored Duration (seconds)"
    ) +
    theme_gray(),
  width = 8, height = 6, dpi = 300, units = "in"
)

unique_predictors <- unique(CERZ_grooming_data_bayes[, c("dyad_index", "CSI_total", "ELO", "age")])
skew_csi <- skewness(unique_predictors$CSI_total, na.rm = TRUE)
cat("Skewness of CSI_total:", skew_csi, "\n")
hist(unique_predictors$CSI_total, main = "Histogram of CSI_total", xlab = "CSI_total")
qqnorm(unique_predictors$CSI_total); qqline(unique_predictors$CSI_total)

survival_times <- ifelse(!is.na(CERZ_grooming_data_bayes$time_to_event),
                         CERZ_grooming_data_bayes$time_to_event,
                         CERZ_grooming_data_bayes$censored_time)
skew_survival <- skewness(survival_times, na.rm = TRUE)
cat("Skewness of survival times (combined uncensored/censored):", skew_survival, "\n")
hist(survival_times, main = "Histogram of Survival Times", xlab = "Time")
qqnorm(survival_times); qqline(survival_times)

uncensored_times <- CERZ_grooming_data_bayes %>%
  filter(event == 1 & !is.na(time_to_event)) %>%
  pull(time_to_event)
gamma_fit <- fitdist(uncensored_times, "gamma")
summary(gamma_fit)

summary(uncensored_times)
hist(uncensored_times, main = "Uncensored Times", xlab = "Time")

uncensored_times_adj <- uncensored_times + 1

gamma_fit <- fitdist(uncensored_times_adj, "gamma",
                     start = list(shape = 0.5, rate = 0.001),
                     optim.method = "Nelder-Mead")

gamma_fit_mme <- fitdist(uncensored_times_adj, "gamma", method = "mme")
summary(gamma_fit_mme)

weibull_fit <- fitdist(uncensored_times, "weibull")
summary(weibull_fit)

my_priors <- c(
  prior(gamma(0.5, 1), class = "shape"),
  prior(normal(log(195), 0.5), class = "Intercept")
)

simple_model <- brm(
  time | cens(censored) ~ CSI_total,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = my_priors,
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4,
  seed = 123
)

summary(simple_model)

model2 <- brm(
  time | cens(censored) ~ CSI_std,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = my_priors,
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4,
  seed = 123
)

summary(model2)

model3_relatedness <- brm(
  time | cens(censored) ~ CSI_std + relatedness,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = my_priors,
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4,
  seed = 123
)

summary(model3_relatedness)

model4_sex <- brm(
  time| cens(censored) ~ CSI_std + relatedness + sex,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = my_priors,
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4,
  seed = 123
)

summary(model4_sex)

model5_elo <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = my_priors,
  chains = 4, iter = 2000, warmup = 1000,
  cores = 4,
  seed = 123
)

summary(model5_elo)

stronger_priors <- c(
  prior(gamma(1, 1), class = "shape"),
  prior(normal(log(195), 0.5), class = "Intercept")
)

my_inits <- function() {
  list(
    Intercept = log(195),
    shape = 0.5
  )
}

model5_adj_inits_prior <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std,
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model5_adj_inits_prior)

my_inits_dyad_rf <- function() {
  list(
    Intercept = log(195),
    shape = 0.5,
    sd__Intercept = rep(1, 1)
  )
}

stronger_priors_dyad_rf <- c(
  prior(gamma(1, 1), class = "shape"),
  prior(normal(log(195), 0.5), class = "Intercept"),
  prior(student_t(3, 0, 1), class = "sd")
)

model6_dyad_rf <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + (1 | dyad_index),
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits_dyad_rf,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model6_dyad_rf)

my_inits_mm <- function() {
  list(
    Intercept = log(195),
    shape = 0.5,
    sd__Intercept = rep(1, 1)
  )
}

model8_mm_dyad <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1| dyad_index),
  data = CERZ_grooming_data_bayes,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model8_mm_dyad)

pp_check(model8_mm_dyad, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (CERZ Model8)", x = "Time (seconds)", y = "Density") +
  theme_minimal()

pp_check(model8_mm_dyad, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (CERZ Model8)", x = "Mean Latency (seconds)", y = "Frequency") +
  theme_minimal()

ggsave("ppc_mean_stat_cerz_model8.png",
       pp_check(model8_mm_dyad, type = "stat", stat = "mean", ndraws = 500) +
         labs(title = "PPC: Mean Latency (CERZ Model8)", x = "Mean Latency (seconds)", y = "Frequency") +
         theme_minimal(),
       width = 8, height = 6, dpi = 150)

mcmc_plot(model8_mm_dyad, type = "intervals") +
  theme_minimal() +
  labs(title = "Posterior Intervals for Fixed Effects", x = "Estimate", y = "Parameter")

cond_effects <- conditional_effects(model8_mm_dyad, effects = "CSI_std")
ggplot(cond_effects$`CSI_std`, aes(x = CSI_std, y = estimate__)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3) +
  theme_minimal() +
  labs(title = "Conditional Effect of CSI_std on Latency", x = "CSI_std (standardized)", y = "Predicted Mean Latency (seconds)")
ggsave(filename = "csi_std_conditional_effects.png", width = 8, height = 6, dpi = 300, units = "in")

gamma_flex_fitEMER <- flexsurvreg(Surv(time_to_event, event) ~ 1, data = EMER_grooming_data_bayes_FINALMODEL, dist = "gamma")
summary(gamma_flex_fitEMER)
plot(gamma_flex_fitEMER, type = "survival", main = "Fitted Gamma Survival Curve", xlab = "Time", ylab = "Survival Probability", ci=TRUE)
lines(km_fit, col = "blue")

coef(gamma_flex_fitEMER, se = TRUE, conf.int = TRUE)
exp(coef(gamma_flex_fitEMER))

plot(gamma_flex_fitEMER, type = "hazard", main = "Fitted Gamma Hazard Function", xlab = "Time", ylab = "Hazard Rate")

plot(resid(gamma_flex_fitEMER), main = "Residuals for Gamma Fit")

weibull_flex_fitEMER <- flexsurvreg(Surv(time_to_event, event) ~ 1, data = EMER_grooming_data_bayes_FINALMODEL, dist = "weibull")
plot(weibull_flex_fitEMER, type = "survival", main = "Fitted weibull Survival Curve", xlab = "Time", ylab = "Survival Probability", ci=TRUE)
lines(km_fit, col = "blue")

AIC(gamma_flex_fitEMER, weibull_flex_fitEMER)

gamma_flex_new <- flexsurvreg(Surv(time, censored) ~ 1,
                                 data = EMER_grooming_data_bayes_FINALMODEL,
                                 dist = "gamma")
summary(gamma_flex_new)

plot(gamma_flex_new, type = "hazard", main = "Gamma Hazard (Proper Censoring)")
plot(gamma_flex_new, type = "survival", main = "Gamma Survival (Proper Censoring)")

pdf("gamma_hazard_proper_censoring.pdf", width = 8, height = 6)
plot(gamma_flex_new, type = "hazard", main = "Gamma Hazard (Proper Censoring)")
dev.off()

pdf("gamma_survival_proper_censoring.pdf", width = 8, height = 6)
plot(gamma_flex_new, type = "survival", main = "Gamma Survival (Proper Censoring)")
dev.off()

my_inits <- function() {
  list(
    Intercept = log(195),
    shape = 0.5
  )
}

EMER_grooming_data_bayes_FINALMODEL <- EMER_grooming_data_bayes_FINALMODEL %>%
  mutate(sex = factor(sex, levels = c("F", "M")))

model1EMER <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq,
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model1EMER)

my_inits_mm <- function() {
  list(
    Intercept = log(195),
    shape = 0.5,
    sd__Intercept = rep(1, 1)
  )
}

model2_EMER_mm <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model2_EMER_mm)

model3_EMER_mm_dyad <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1| dyad_index),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = Gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model3_EMER_mm_dyad)

pp_check(model3_EMER_mm_dyad, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (EMER Model3)") +
  theme_minimal()

pp_check(model3_EMER_mm_dyad, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Model3)") +
  theme_minimal()

ggsave("ppc_mean_stat_emer_model3.png",
       pp_check(model3_EMER_mm_dyad, type = "stat", stat = "mean", ndraws = 500) +
         labs(title = "PPC: Mean Latency (EMER Model3)", x = "Mean Latency (seconds)", y = "Frequency") +
         theme_minimal(),
       width = 8, height = 6, dpi = 150)

my_priors_lognormal <- c(
  prior(normal(0, 1), class = "sigma"),
  prior(normal(log(195), 0.5), class = "Intercept")
)

model3_EMER_lognormal_test <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1 | dyad_index),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = lognormal(link = "identity"),
  prior = my_priors_lognormal,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  cores = 4,
  seed = 123
)

summary(model3_EMER_lognormal_test)

model3_EMER_lognormal_test_amend <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1 | dyad_index),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = lognormal(link = "identity"),
  prior = my_priors_lognormal,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4,
  seed = 123
)

model3_EMER_hurdle_gamma <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1 | dyad_index),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = hurdle_gamma(link = "log"),
  prior = stronger_priors,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4,
  seed = 123
)

model3_EMER_hurdle_lognormal <- brm(
  time | cens(censored) ~ CSI_std + relatedness + sex + ELO_std + age_std + age_sq + (1 | mm(focal_id, partner_id)) + (1 | dyad_index),
  data = EMER_grooming_data_bayes_FINALMODEL,
  family = hurdle_lognormal(link = "identity"),
  prior = my_priors_lognormal,
  inits = my_inits_mm,
  chains = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4,
  seed = 123
)

pp_check(model3_EMER_lognormal_test, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (EMER Model3 Lognormal)") +
  theme_minimal()

pp_check(model3_EMER_lognormal_test, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Model3 Lognormal)") +
  theme_minimal()

ggsave("ppc_dens_overlay_emer_lognormal.png",
       pp_check(model3_EMER_lognormal_test, type = "dens_overlay", ndraws = 50) +
         labs(title = "PPC: Observed vs Replicated Latencies (EMER Model3 Lognormal)") +
         theme_minimal(),
       width = 8, height = 5, dpi = 150)

ggsave("ppc_mean_stat_emer_lognormal.png",
       pp_check(model3_EMER_lognormal_test, type = "stat", stat = "mean", ndraws = 500) +
         labs(title = "PPC: Mean Latency (EMER Model3 Lognormal)") +
         theme_minimal(),
       width = 8, height = 5, dpi = 150)

pp_check(model3_EMER_lognormal_test_amend, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Model3 Lognormal Amended)",
       x = "Mean Latency (seconds)", y = "Frequency") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

ggsave("ppc_mean_stat_emer_lognormal_amend.png",
       pp_check(model3_EMER_lognormal_test_amend, type = "stat", stat = "mean", ndraws = 500) +
         labs(title = "PPC: Mean Latency (EMER Model3 Lognormal Amended)",
              x = "Mean Latency (seconds)", y = "Frequency") +
         scale_x_continuous(labels = scales::comma) +
         theme_minimal(),
       width = 8, height = 6, dpi = 150)

pp_check(model3_EMER_hurdle_gamma, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (EMER Hurdle-Gamma)") +
  theme_minimal()

pp_check(model3_EMER_hurdle_gamma, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Hurdle-Gamma)") +
  theme_minimal()

ggsave("ppc_mean_stat_emer_hurdle_gamma.png",
       pp_check(model3_EMER_hurdle_gamma, type = "stat", stat = "mean", ndraws = 500) +
         labs(title = "PPC: Mean Latency (EMER Hurdle-Gamma)") +
         theme_minimal(),
       width = 8, height = 5, dpi = 150)

pp_check(model3_EMER_hurdle_lognormal, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Hurdle-Gamma)") +
  theme_minimal()

pdf("ppc_mean_stat_emer_hurdle_lognormal.pdf", width = 8, height = 6)
pp_check(model3_EMER_hurdle_lognormal, type = "stat", stat = "mean", ndraws = 500) +
  labs(title = "PPC: Mean Latency (EMER Hurdle-Lognormal)", x = "Mean Latency (seconds)", y = "Frequency") +
  theme_minimal()
dev.off()

pp_check(model3_EMER_hurdle_lognormal, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (EMER Hurdle-Lognormal)") +
  theme_minimal()

pdf("ppc_dens_overlay_emer_hurdle_lognormal.pdf", width = 8, height = 6)
pp_check(model3_EMER_hurdle_lognormal, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated Latencies (EMER Hurdle-Lognormal)") +
  theme_minimal()
dev.off()

mcmc_plot(model3_EMER_hurdle_lognormal, type = "intervals") +
  theme_minimal() +
  labs(title = "Posterior Intervals for Fixed Effects (Hurdle-Lognormal Model)", x = "Estimate", y = "Parameter")

pdf("posterior_intervals_emer_hurdle_lognormal.pdf", width = 8, height = 6)
mcmc_plot(model3_EMER_hurdle_lognormal, type = "intervals") +
  theme_minimal() +
  labs(title = "Posterior Intervals for Fixed Effects (Hurdle-Lognormal Model)", x = "Estimate", y = "Parameter")
dev.off()

plot(resid(model3_EMER_hurdle_lognormal))

pdf("residuals_emer_hurdle_lognormal.pdf", width = 8, height = 6)
plot(resid(model3_EMER_hurdle_lognormal),
     main = "Residuals for Hurdle-Lognormal Model (EMER)",
     xlab = "Observation Index",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)
dev.off()

CERZ_centrality_scores <- read_xlsx("CERZ_centrality_scores.xlsx") |>
  clean_names()

EMER_centrality_scores <- read_xlsx("EMER_centrality_scores.xlsx") |>
  clean_names()

CERZ_clean <- CERZ_centrality_scores |>
  select(individual, eigenvector) |>
  mutate(group = "CERZ")

EMER_clean <- EMER_centrality_scores |>
  select(individual, eigenvector) |>
  mutate(group = "EMER")

CERZ_EMER_centrality <- bind_rows(CERZ_clean, EMER_clean)

CERZ_EMER_centrality <- CERZ_EMER_centrality |>
  mutate(z_eigenvector = as.vector(scale(eigenvector)))

CERZ_EMER_centrality_groom_combined <- CERZ_EMER_grooming_data_combined |>
  left_join(
    CERZ_EMER_centrality |> select(individual, eigenvector, z_eigenvector),
    by = c("focal_id" = "individual")
  )

CERZ_EMER_centrality |>
  distinct(individual) |>
  anti_join(
    CERZ_EMER_centrality_groom_combined,
    by = c("individual" = "focal_id")
  )

cat("\n=== Summary by Group ===\n")
CERZ_EMER_centrality |>
  group_by(group) |>
  summarise(
    n          = n(),
    mean       = mean(eigenvector, na.rm = TRUE),
    median     = median(eigenvector, na.rm = TRUE),
    sd         = sd(eigenvector, na.rm = TRUE),
    min        = min(eigenvector, na.rm = TRUE),
    max        = max(eigenvector, na.rm = TRUE),
    skewness   = moments::skewness(eigenvector, na.rm = TRUE),
    kurtosis   = moments::kurtosis(eigenvector, na.rm = TRUE)
  ) |>
  print()

my_inits_gamma_eigenvector <- function() {
  list(
    Intercept = log(195),
    shape = 0.5
  )
}

CERZ_EMER_brm_gamma_eigenvector <- brm(
  time | cens(censored) ~ z_CSI + z_eigenvector +
    (1 | mm(focal_id, partner_id)) +
    (1 | dyad_index) +
    (1 | group_ID),
  data   = CERZ_EMER_centrality_groom_combined,
  family = Gamma(link = "log"),
  prior  = c(
    prior(normal(log(195), 0.5), class = "Intercept"),
    prior(gamma(2, 0.1),         class = "shape"),
    prior(student_t(3, 0, 1),    class = "sd")
  ),
  init    = my_inits_gamma_eigenvector,
  chains  = 4, iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  cores   = 4,
  seed    = 123
)

summary(CERZ_EMER_brm_gamma_eigenvector)

pp_check(CERZ_EMER_brm_gamma_eigenvector,
         type   = "dens_overlay",
         ndraws = 50) +
  coord_cartesian(xlim = c(0, 12000)) +
  labs(title = "PPC: Observed vs Replicated (Gamma eigenvector)",
       x     = "Time (seconds)",
       y     = "Density") +
  theme_gray()

p_dens_overlay_eigenvector <- pp_check(CERZ_EMER_brm_gamma_eigenvector,
                           type   = "dens_overlay",
                           ndraws = 50) +
  coord_cartesian(xlim = c(0, 12000)) +
  labs(title = "PPC: Observed vs Replicated (Gamma eigenvector)",
       x     = "Time (seconds)",
       y     = "Density") +
  theme_gray()

ggsave("PPC_dens_overlay_gamma_eigenvector.png",
       plot   = p_dens_overlay_eigenvector,
       width  = 8,
       height = 5,
       dpi    = 300)

pp_check(CERZ_EMER_brm_gamma_eigenvector,
         type   = "stat",
         stat   = "mean") +
  labs(title = "PPC: Mean Posterior Check (Gamma eigenvector)",
       x     = "Mean Time (seconds)") +
  theme_gray()

CERZ_EMER_brm_gamma_v2 <- add_criterion(CERZ_EMER_brm_gamma_v2, "loo")
CERZ_EMER_brm_gamma_eigenvector <- add_criterion(CERZ_EMER_brm_gamma_eigenvector, "loo")

loo_compare(CERZ_EMER_brm_gamma_v2, CERZ_EMER_brm_gamma_eigenvector)

CERZ_EMER_brm_gamma_v2 <- add_criterion(
  CERZ_EMER_brm_gamma_v2,
  "loo",
  moment_match = TRUE
)

CERZ_EMER_brm_gamma_eigenvector <- add_criterion(
  CERZ_EMER_brm_gamma_eigenvector,
  "loo",
  moment_match = TRUE
)

loo_compare(CERZ_EMER_brm_gamma_v2, CERZ_EMER_brm_gamma_eigenvector)

plot(loo(CERZ_EMER_brm_gamma_eigenvector))
