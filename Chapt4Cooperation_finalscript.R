# Packages
library(tidyverse)
library(readxl)
library(janitor)
library(igraph)
library(tidygraph)
library(ggraph)
library(brms)
library(mgcv)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(DHARMa)

cooperationrawdata_CERZ <- read_excel(
  path      = "cooperationrawdataCERZ.xlsx",
  sheet     = 1,
  col_types = "text"
)

cooperationrawdata_CERZ <- cooperationrawdata_CERZ %>%
  mutate(session = as.integer(session))

cooperationrawdata_CERZ <- cooperationrawdata_CERZ %>%
  mutate(
    pull_rope_l_id = na_if(pull_rope_l_id, "NA"),
    pull_rope_r_id = na_if(pull_rope_r_id, "NA")
  )

cooperationrawdata_EMER <- read_excel(
  path = "cooperationrawdataEMER.xlsx",
  sheet = 1,
  col_types = "text"
)
cooperationrawdata_EMER <- cooperationrawdata_EMER %>%
  mutate(session = as.integer(session))

cooperationrawdata_EMER <- cooperationrawdata_EMER %>%
  mutate(
    pull_rope_l_id = na_if(pull_rope_l_id, "NA"),
    pull_rope_r_id = na_if(pull_rope_r_id, "NA")
  )

time_cols <- c(
  "session_start",
  "pull_rope_l_time",
  "pull_rope_r_time",
  "hold_rope_l_time",
  "hold_rope_r_time",
  "approach_id_l_time",
  "approach_id_r_time",
  "look_l_id_time",
  "look_r_id_time",
  "lipsmack_id_time",
  "vocalize_id_time",
  "success_time",
  "reset_start",
  "reset_end"
)

CERZ_monkeys <- c(
  "BROW", "BUNTA", "FIDGET", "KAOS", "MAKASSAR",
  "MALI", "MONK", "QUANNY", "SULA", "SUNDA", "TALIA"
)

cooperationrawdata_CERZ <- cooperationrawdata_CERZ |>
  mutate(across(all_of(time_cols), as.numeric)) |>
  mutate(across(all_of(time_cols), ~ floor(.x * 1000) / 1000))

ELOtesttableCERZ <- read_excel(
  path      = "ELOtesttableCERZ.xlsx",
  sheet     = 1,
  col_types = "guess"
)

CERZ_ages <- c(
  QUANNY   = 9,
  BUNTA    = 7,
  SULA     = 10,
  TALIA    = 15,
  MALI     = 18,
  KAOS     = 17,
  FIDGET   = 21,
  BROW     = 4,
  MAKASSAR = 5,
  MONK     = 4,
  SUNDA    = 4
)

CERZ_sex <- c(
  QUANNY   = "M",
  BUNTA    = "F",
  SULA     = "F",
  TALIA    = "F",
  MALI     = "F",
  KAOS     = "F",
  FIDGET   = "F",
  BROW     = "M",
  MAKASSAR = "M",
  MONK     = "M",
  SUNDA    = "F"
)

CERZ_sex <- factor(CERZ_sex, levels = c("F", "M"))

CERZ_relatedness <- read_excel(
  path      = "CERZ_relatedness.xlsx",
  sheet     = 1,
  col_names = TRUE,
  trim_ws   = TRUE,
  col_types = c("text", "numeric")
)

CERZ_CSI <- read_excel(
  path = "CERZ_final_CSI.xlsx",
  sheet = 1,
  skip = 0,
  col_types = c(
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
) %>%
  mutate(
    dyad = trimws(dyad)
  )

str(CERZ_CSI)
str(EMER_CSI)

CERZ_monkeys <- c(
  "BROW", "BUNTA", "FIDGET", "KAOS", "MAKASSAR",
  "MALI", "MONK", "QUANNY", "SULA", "SUNDA", "TALIA"
)

groom_dur_summary <- read_excel(
  "CERZ_groom_dur_summary_directed.xlsx",
  col_types = c("text", "text", "numeric")
)

CERZ_individual_obs <- read_excel(
  path = "CERZ_individual_observation_time.xlsx",
  sheet = 1,
  skip = 0,
  col_types = c("text", "numeric", "numeric")
) %>%
  mutate(
    focal_id = trimws(focal_id)
  )

groom_dur_undirected <- groom_dur_summary %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner)
  ) %>%
  filter(id1 != id2) %>%
  group_by(id1, id2) %>%
  summarise(
    total_duration_comb = sum(total_duration_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  select(dyad_id, total_duration_comb) %>%
  arrange(dyad_id)

groom_dur_undirected_with_min_obs <- groom_dur_undirected %>%
  separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  select(
    dyad_id,
    total_duration_comb,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

groom_dur_undirected_with_min_obs <- groom_dur_undirected_with_min_obs %>%
  mutate(
    grooming_rate_per_hour = total_duration_comb / dyad_obs_hours_min
  )

groom_dur_undirected_with_min_obs <- groom_dur_undirected_with_min_obs %>%
  mutate(
    grooming_rate_mean = mean(grooming_rate_per_hour, na.rm = TRUE),
    grooming_rate_sd   = sd(grooming_rate_per_hour, na.rm = TRUE),

    z_groom_rate = (grooming_rate_per_hour - grooming_rate_mean) / grooming_rate_sd
  )

CERZ_affil_dyad_summary_directed <- read_excel(
  path = "CERZ_affil_dyad_summary_directed.xlsx",
  sheet = 1,
  skip = 0,
  col_names = TRUE,
  trim_ws = TRUE,
  col_types = c(
    "text",
    "text",
    rep("numeric", 8)
  )
) %>%
  mutate(
    focal_id = trimws(focal_id),
    partner = trimws(partner)
  )

CERZ_affil_dyad_summary_directed <- CERZ_affil_dyad_summary_directed %>%
  mutate(
    dyad_id = map2_chr(
      focal_id, partner,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    ),

    affil_total = rowSums(
      across(
        c(given_L, given_T, received_L, received_T,
          received_H, given_H, given_E, received_E),
        ~ replace_na(.x, 0)
      ),
      na.rm = TRUE
    )
  )

CERZ_affil_dyad_summary_undirected <- CERZ_affil_dyad_summary_directed %>%
  group_by(dyad_id) %>%
  summarise(
    affil_total = mean(affil_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    str_detect(dyad_id, "_") &
    !str_detect(dyad_id, "^(\\w+)\\1$")
  ) %>%
  arrange(dyad_id)

CERZ_affil_dyad_summary_min_obs <- CERZ_affil_dyad_summary_undirected %>%
  separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  select(
    dyad_id,
    affil_total,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

CERZ_affil_dyad_summary_min_obs <- CERZ_affil_dyad_summary_min_obs %>%
  mutate(
    affil_rate = affil_total / dyad_obs_hours_min
  )

CERZ_affil_dyad_summary_min_obs <- CERZ_affil_dyad_summary_min_obs %>%
  mutate(
    affil_rate_mean = mean(affil_rate, na.rm = TRUE),
    affil_rate_sd   = sd(affil_rate, na.rm = TRUE),
    z_affil_rate    = (affil_rate - affil_rate_mean) / affil_rate_sd
  )

CERZ_prox_summary <- read_excel(
  path = "CERZ_prox_summary.xlsx",
  sheet = 1,
  skip = 0,
  col_names = TRUE,
  trim_ws = TRUE,
  col_types = c(
    "text",
    "numeric",
    "numeric",
    "numeric"
  )
) %>%
  mutate(
    dyad = trimws(dyad)
  )

CERZ_prox_summary_min_obs <- CERZ_prox_summary %>%

  rename(dyad_id = dyad) %>%

  separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  left_join(
    CERZ_individual_obs %>%
      rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  select(
    dyad_id,
    total_prox,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

CERZ_prox_summary_min_obs <- CERZ_prox_summary_min_obs %>%
  mutate(
    prox_rate = total_prox / dyad_obs_hours_min
  )

CERZ_prox_summary_min_obs <- CERZ_prox_summary_min_obs %>%
  mutate(
    prox_rate_mean = mean(prox_rate, na.rm = TRUE),
    prox_rate_sd   = sd(prox_rate, na.rm = TRUE),
    z_prox_rate    = (prox_rate - prox_rate_mean) / prox_rate_sd
  )

CERZ_zscore_final <- groom_dur_undirected_with_min_obs %>%
  select(dyad_id, z_groom_rate) %>%
  full_join(
    CERZ_affil_dyad_summary_min_obs %>%
      select(dyad_id, z_affil_rate),
    by = "dyad_id"
  ) %>%
  full_join(
    CERZ_prox_summary_min_obs %>%
      select(dyad_id, z_prox_rate),
    by = "dyad_id"
  ) %>%
  mutate(
    final_zscore = rowMeans(
      across(c(z_groom_rate, z_affil_rate, z_prox_rate)),
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(final_zscore)) %>%
  select(
    dyad_id,
    z_groom_rate,
    z_affil_rate,
    z_prox_rate,
    final_zscore
  ) %>%
  arrange(dyad_id)

write_xlsx(
  CERZ_zscore_final,
  path = "CERZ_zscore_final.xlsx"
)

min_z <- min(CERZ_zscore_final$final_zscore, na.rm = TRUE)

CERZ_zscore_final_adj <- CERZ_zscore_final %>%
  mutate(
    weight_nonneg = final_zscore - min_z + 0.001
  )

edge_list <- CERZ_zscore_final_adj %>%
  separate(
    dyad_id,
    into = c("from", "to"),
    sep = "_",
    remove = FALSE
  ) %>%
  rename(weight = weight_nonneg) %>%
  select(from, to, weight, dyad_id)

g <- graph_from_data_frame(
  d = edge_list,
  directed = FALSE,
  vertices = tibble(name = CERZ_monkeys)
)

E(g)$weight <- edge_list$weight

cat("CERZ network summary:\n")
cat("Nodes:", vcount(g), "\n")
cat("Edges:", ecount(g), "\n")
cat("Density:", round(edge_density(g), 3), "\n")
cat("Weighted:", is_weighted(g), "\n")

centrality <- tibble(
  individual = V(g)$name,

  degree = degree(g, normalized = FALSE),

  strength = strength(g),

  eigenvector = eigen_centrality(g, directed = FALSE)$vector
) %>%
  arrange(desc(strength))

print(centrality, n = Inf)

CERZ_centrality_scores <- tibble(
  individual = V(g)$name,
  degree = degree(g, normalized = FALSE),
  strength = strength(g),
  eigenvector = eigen_centrality(g, directed = FALSE)$vector
) %>%
  arrange(desc(strength))

str(CERZ_centrality_scores)
CERZ_centrality_scores %>%
  summarise(
    mean_eigenvector = mean(eigenvector),
    sd_eigenvector   = sd(eigenvector),
    min_eigenvector  = min(eigenvector),
    max_eigenvector  = max(eigenvector)
  )

cor_matrix_spearman <- CERZ_centrality_scores %>%
  select(degree, strength, eigenvector) %>%
  cor(method = "spearman")

cat("\n=== Spearman Correlation Matrix ===\n")
print(round(cor_matrix_spearman, 3))

write_xlsx(
  CERZ_centrality_scores,
  path = "CERZ_centrality_scores.xlsx"
)

tg <- as_tbl_graph(g, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(
    node_strength = strength(g),
    short_name    = substr(name, 1, 3),
    display_size  = if_else(name == "BROW", NA_real_, pmax(node_strength * 1.8 + 6, 8)),
    is_brow       = (name == "BROW")
  )

set.seed(42)

layout_coords <- create_layout(tg, layout = "stress")
layout_coords$y[layout_coords$name == "BROW"] <-
  layout_coords$y[layout_coords$name == "BROW"] - 0.035

layout_coords$x[layout_coords$name == "SUNDA"] <-
  layout_coords$x[layout_coords$name == "SUNDA"] - 0.1

layout_coords$x[layout_coords$name == "MAKASSAR"] <-
  layout_coords$x[layout_coords$name == "MAKASSAR"] + 0.01
layout_coords$y[layout_coords$name == "MAKASSAR"] <-
  layout_coords$y[layout_coords$name == "MAKASSAR"] - 0.08

p <- ggraph(layout_coords) +
  geom_edge_link(
    aes(width = weight),
    colour = "grey60",
    alpha = 0.35,
    show.legend = FALSE
  ) +
  geom_edge_arc(
    aes(width = weight, alpha = weight),
    colour   = "steelblue",
    strength = 0.3,
    lineend  = "round",
    linejoin = "round"
  ) +
  scale_edge_width(
    range = c(0.4, 5.5),
    name  = "Shifted composite\nbond strength"
  ) +
  scale_edge_alpha(range = c(0.45, 1.0), guide = "none") +
  geom_node_point(
    aes(size = display_size),
    shape  = 21,
    colour = "grey35",
    fill   = "#f0f8ff",
    stroke = 1.5
  ) +
  scale_size_continuous(
    range = c(12, 32),
    name  = "Strength centrality"
  ) +
  geom_node_point(
    data   = function(x) filter(x, name == "BROW"),
    size   = 8,
    shape  = 21,
    colour = "grey35",
    fill   = "#f0f8ff",
    stroke = 1.5
  ) +

  geom_node_text(
    data     = function(x) filter(x, !name %in% c("SULA", "KAOS")),
    aes(label = short_name),
    size     = 3.5,
    colour   = "grey10",
    fontface = "bold"
  ) +
  geom_node_text(
    data     = function(x) filter(x, name %in% c("SULA", "KAOS")),
    aes(label = short_name),
    nudge_y  = 0.02,
    size     = 3.5,
    colour   = "grey10",
    fontface = "bold"
  ) +

  theme_graph(background = "grey92") +
  theme_void() +
  theme(
    plot.background      = element_rect(fill = "grey92", colour = NA),
    panel.background     = element_rect(fill = "grey92", colour = NA),
    plot.margin          = margin(t = 15, r = 15, b = 120, l = 15, unit = "pt"),
    legend.position      = "bottom",
    legend.justification = "center",
    legend.box.just      = "center",
    legend.box           = "horizontal",
    legend.margin        = margin(t = 5, b = 5),
    legend.key.width     = unit(2.2, "cm"),
    legend.title         = element_text(size = 10),
    legend.text          = element_text(size = 9)
  ) +
  labs(
    title    = "CERZ Social Network",
    subtitle = "Edge weight = shifted composite z-score (grooming + affiliation + proximity)",
    caption  = "Node size = strength centrality | Edge thickness = shifted z-score"
  ) +
  annotate(
  "text",
  x = Inf, y = Inf,
  label = "Monkey names\n\n" %>%
          paste(paste(sort(unique(tg %>% pull(name))), collapse = "\n"), sep = ""),
  hjust = 1, vjust = 1,
  size = 3.1, colour = "grey30", lineheight = 1.05
) +
  coord_cartesian(clip = "off")

ggsave(
  "CERZ_network_final.png",
  p,
  width  = 16,
  height = 13,
  dpi    = 600,
  units  = "in",
  bg     = "grey92"
)

EMER_monkeys <- c(
  "BASUKI", "BUMI", "DOUGIE", "DRUSILLA", "EKAH",
  "INDAH", "KERANA", "MASAMBA", "SETANA"
)

cooperationrawdata_EMER <- cooperationrawdata_EMER |>
  mutate(across(all_of(time_cols), as.numeric)) |>
  mutate(across(all_of(time_cols), ~ floor(.x * 1000) / 1000))

ELOtesttableEMER <- read_excel(
  path      = "ELOtesttableEMER.xlsx",
  sheet     = 1,
  col_types = "guess"
)

EMER_ages <- c(
  DOUGIE    = 18,
  DRUSILLA  = 24,
  INDAH     = 9,
  EKAH      = 6,
  KERANA    = 3,
  SETANA    = 19,
  BASUKI    = 5,
  BUMI      = 7,
  MASAMBA   = 9
)

EMER_sex <- c(
  DOUGIE    = "M",
  DRUSILLA  = "F",
  INDAH     = "F",
  EKAH      = "F",
  KERANA    = "F",
  SETANA    = "F",
  BASUKI    = "M",
  BUMI      = "M",
  MASAMBA   = "M"
)

EMER_sex <- factor(EMER_sex, levels = c("F", "M"))

EMER_relatedness <- read_excel(
  path      = "EMER_relatedness.xlsx",
  sheet     = 1,
  col_names = TRUE,
  trim_ws   = TRUE,
  col_types = c("text", "numeric")
)

EMER_CSI <- read_excel(
  path = "EMER_final_CSI.xlsx",
  sheet = 1,
  skip = 0,
  col_types = c(
    "text",
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
) %>%
  mutate(
    dyad = trimws(dyad)
  )

EMER_monkeys <- c(
  "BASUKI", "BUMI", "DOUGIE", "DRUSILLA", "EKAH",
  "INDAH", "KERANA", "MASAMBA", "SETANA"
)

EMER_groom_dur_summary <- read_excel(
  path = "EMER_groom_dur_summary_directed.xlsx"
)

EMER_individual_obs <- read_excel(
  path = "EMER_individual_observation_time.xlsx"
)

groom_dur_undirected_EMER <- EMER_groom_dur_summary %>%
  mutate(
    id1 = pmin(focal_id, partner),
    id2 = pmax(focal_id, partner)
  ) %>%
  filter(id1 != id2) %>%
  group_by(id1, id2) %>%
  summarise(
    total_duration_comb = sum(total_duration_sec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dyad_id = paste(id1, id2, sep = "_")
  ) %>%
  select(dyad_id, total_duration_comb) %>%
  arrange(dyad_id)

groom_dur_undirected_with_min_obs_EMER <- groom_dur_undirected_EMER %>%
  tidyr::separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  dplyr::mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  dplyr::select(
    dyad_id,
    total_duration_comb,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

groom_dur_undirected_with_min_obs_EMER <- groom_dur_undirected_with_min_obs_EMER %>%
  mutate(
    grooming_rate_per_hour = total_duration_comb / dyad_obs_hours_min
  )

groom_dur_undirected_with_min_obs_EMER <- groom_dur_undirected_with_min_obs_EMER %>%
  mutate(
    grooming_rate_mean = mean(grooming_rate_per_hour, na.rm = TRUE),
    grooming_rate_sd   = sd(grooming_rate_per_hour, na.rm = TRUE),

    z_groom_rate = (grooming_rate_per_hour - grooming_rate_mean) / grooming_rate_sd
  )

EMER_affil_dyad_summary_directed <- read_excel(
  path      = "EMER_affil_dyad_summary_directed.xlsx",
  sheet     = 1,
  trim_ws   = TRUE,
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
) %>%
  mutate(
    focal_id = trimws(focal_id),
    partner  = trimws(partner)
  )

EMER_affil_dyad_summary_undirected <- EMER_affil_dyad_summary_directed %>%
  mutate(
    dyad_id = map2_chr(
      focal_id, partner,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    ),

    affil_total = rowSums(
      across(
        c(given_L, given_T, received_L, received_T,
          given_H, received_H, given_E, received_E),
        ~ replace_na(.x, 0)
      ),
      na.rm = TRUE
    )
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    affil_total = mean(affil_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    str_detect(dyad_id, "_") &
      !str_detect(dyad_id, "^(\\w+)\\1$")
  ) %>%
  arrange(dyad_id)

EMER_affil_dyad_summary_min_obs <- EMER_affil_dyad_summary_undirected %>%
  tidyr::separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  dplyr::mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  dplyr::select(
    dyad_id,
    affil_total,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

EMER_affil_dyad_summary_min_obs <- EMER_affil_dyad_summary_min_obs %>%
  mutate(
    affil_rate_per_hour = affil_total / dyad_obs_hours_min
  )

EMER_affil_dyad_summary_min_obs <- EMER_affil_dyad_summary_min_obs %>%
  mutate(
    affil_rate_mean = mean(affil_rate_per_hour, na.rm = TRUE),
    affil_rate_sd   = sd(affil_rate_per_hour, na.rm = TRUE),

    z_affil_rate = (affil_rate_per_hour - affil_rate_mean) / affil_rate_sd
  )

EMER_prox_length_summary_directed <- read_excel(
  path = "EMER_prox_length_summary_directed.xlsx"
)

EMER_prox_dyad_summary_min_obs <- EMER_prox_length_summary_directed %>%
  filter(focal_id != partner) %>%
  mutate(
    dyad_id = map2_chr(
      focal_id, partner,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    ),
    total_prox = count_bc + count_1
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    total_prox = max(total_prox, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    str_detect(dyad_id, "_") &
    !str_detect(dyad_id, "^(\\w+)\\1$")
  ) %>%
  arrange(dyad_id)

EMER_prox_dyad_summary_min_obs <- EMER_prox_dyad_summary_min_obs %>%
  tidyr::separate(
    dyad_id,
    into = c("indiv1", "indiv2"),
    sep = "_",
    remove = FALSE
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv1 = obs_time_hours),
    by = c("indiv1" = "focal_id")
  ) %>%
  dplyr::left_join(
    EMER_individual_obs %>%
      dplyr::rename(obs_hours_indiv2 = obs_time_hours),
    by = c("indiv2" = "focal_id")
  ) %>%
  dplyr::mutate(
    dyad_obs_hours_min = pmin(obs_hours_indiv1, obs_hours_indiv2, na.rm = TRUE)
  ) %>%
  dplyr::select(
    dyad_id,
    total_prox,
    dyad_obs_hours_min,
    obs_hours_indiv1,
    obs_hours_indiv2
  )

EMER_prox_dyad_summary_min_obs <- EMER_prox_dyad_summary_min_obs %>%
  mutate(
    prox_rate_per_hour = total_prox / dyad_obs_hours_min
  )

EMER_prox_dyad_summary_min_obs <- EMER_prox_dyad_summary_min_obs %>%
  mutate(
    prox_rate_mean = mean(prox_rate_per_hour, na.rm = TRUE),
    prox_rate_sd   = sd(prox_rate_per_hour, na.rm = TRUE),

    z_prox_rate = (prox_rate_per_hour - prox_rate_mean) / prox_rate_sd
  )

EMER_zscore_final <- groom_dur_undirected_with_min_obs_EMER %>%
  select(dyad_id, z_groom_rate) %>%
  full_join(
    EMER_affil_dyad_summary_min_obs %>%
      select(dyad_id, z_affil_rate),
    by = "dyad_id"
  ) %>%
  full_join(
    EMER_prox_dyad_summary_min_obs %>%
      select(dyad_id, z_prox_rate),
    by = "dyad_id"
  ) %>%
  mutate(
    final_zscore = rowMeans(
      across(c(z_groom_rate, z_affil_rate, z_prox_rate)),
      na.rm = TRUE
    )
  ) %>%
  filter(!is.na(final_zscore)) %>%
  select(
    dyad_id,
    z_groom_rate,
    z_affil_rate,
    z_prox_rate,
    final_zscore
  ) %>%
  arrange(dyad_id)

min_z_EMER <- min(EMER_zscore_final$final_zscore, na.rm = TRUE)

EMER_zscore_final_adj <- EMER_zscore_final %>%
  mutate(
    weight_nonneg = final_zscore - min_z_EMER + 0.001
  )

edge_list_EMER <- EMER_zscore_final_adj %>%
  separate(
    dyad_id,
    into = c("from", "to"),
    sep = "_",
    remove = FALSE
  ) %>%
  rename(weight = weight_nonneg) %>%
  select(from, to, weight, dyad_id)

g_EMER <- graph_from_data_frame(
  d = edge_list_EMER,
  directed = FALSE,
  vertices = tibble(name = EMER_monkeys)
)

E(g_EMER)$weight <- edge_list_EMER$weight

cat("EMER network summary:\n")
cat("Nodes:", vcount(g_EMER), "\n")
cat("Edges:", ecount(g_EMER), "\n")
cat("Density:", round(edge_density(g_EMER), 3), "\n")
cat("Weighted:", is_weighted(g_EMER), "\n")

centrality_EMER <- tibble(
  individual = V(g_EMER)$name,

  degree = degree(g_EMER, normalized = FALSE),

  strength = strength(g_EMER),

  eigenvector = eigen_centrality(g_EMER, directed = FALSE)$vector
) %>%
  arrange(desc(strength))

print(centrality_EMER, n = Inf)

EMER_centrality_scores <- tibble(
  individual = V(g_EMER)$name,
  degree      = degree(g_EMER, normalized = FALSE),
  strength    = strength(g_EMER),
  eigenvector = eigen_centrality(g_EMER, directed = FALSE)$vector
) %>%
  arrange(desc(strength))

write_xlsx(
  EMER_centrality_scores,
  path = "EMER_centrality_scores.xlsx"
)

EMER_centrality_scores %>%
  summarise(
    mean_eigenvector = mean(eigenvector),
    sd_eigenvector   = sd(eigenvector),
     min_eigenvector  = min(eigenvector),
    max_eigenvector  = max(eigenvector)
  )

tg_EMER <- as_tbl_graph(g_EMER, directed = FALSE) %>%
  activate("nodes") %>%
  mutate(
    node_strength = strength(g_EMER),
    short_name = substr(name, 1, 3)
  )

set.seed(42)
layout_coords_EMER <- create_layout(tg_EMER, layout = "stress")

layout_coords_EMER$y[layout_coords_EMER$name == "BASUKI"]   <- layout_coords_EMER$y[layout_coords_EMER$name == "BASUKI"] + 0.045
layout_coords_EMER$x[layout_coords_EMER$name == "DRUSILLA"] <- layout_coords_EMER$x[layout_coords_EMER$name == "DRUSILLA"] + 0.045

p_EMER <- ggraph(layout_coords_EMER) +
  geom_edge_link(
    aes(width = weight),
    colour = "grey60",
    alpha = 0.35,
    show.legend = FALSE
  ) +
  geom_edge_arc(
    aes(width = weight, alpha = weight),
    colour = "steelblue",
    strength = 0.3,
    lineend = "round",
    linejoin = "round"
  ) +
  scale_edge_width(
    range = c(0.4, 5.5),
    name = "Shifted composite\nbond strength"
  ) +
  scale_edge_alpha(range = c(0.45, 1.0), guide = "none") +
  geom_node_point(
    aes(size = node_strength),
    shape = 21,
    colour = "grey35",
    fill = "#f0f8ff",
    stroke = 1.5
  ) +
  scale_size_continuous(
    range = c(12, 32),
    name = "Strength centrality"
  ) +
  geom_node_text(
    aes(label = short_name),
    size = 3.5,
    colour = "grey10",
    fontface = "bold"
  ) +
  theme_graph(background = "grey92") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "grey92", colour = NA),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.margin      = margin(t = 15, r = 15, b = 120, l = 15, unit = "pt"),
    plot.title       = element_text(margin = margin(b = 20)),
    plot.subtitle    = element_text(margin = margin(b = 20)),
    legend.position      = "bottom",
    legend.justification = "center",
    legend.box.just      = "center",
    legend.box           = "horizontal",
    legend.margin        = margin(t = 5, b = 5),
    legend.key.width     = unit(2.2, "cm"),
    legend.title         = element_text(size = 10),
    legend.text          = element_text(size = 9)
  ) +
  labs(
    title    = "EMER Social Network",
    subtitle = "Edge weight = shifted composite z-score (grooming + affiliation + proximity)",
    caption  = "Node size = strength centrality | Edge thickness = shifted z-score"
  ) +
  annotate(
    "text",
    x = Inf, y = Inf,
    label = "Monkey names\n\n" %>%
            paste(paste(sort(unique(tg_EMER %>% pull(name))), collapse = "\n"), sep = ""),
    hjust = 1, vjust = 1,
    size = 3.1, colour = "grey30", lineheight = 1.05
  ) +
  coord_cartesian(clip = "off")

ggsave(
  "EMER_network_final.png",
  p_EMER,
  width  = 16,
  height = 13,
  dpi    = 600,
  units  = "in",
  bg     = "grey92"
)

print(sum(cooperationrawdata_CERZ$success == "YES", na.rm = TRUE))

cooperationrawdata_CERZ <- cooperationrawdata_CERZ %>%
  mutate(
    dyad_id_success = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  )

all_possible_cerzdyads <- CERZ_monkeys %>%
  combn(2, simplify = FALSE) %>%
  map_chr(~ paste(sort(.x), collapse = "_")) %>%
  tibble(dyad_id_success = .) %>%
  mutate(possible = TRUE)

cerz_success_pairs <- cooperationrawdata_CERZ %>%
  filter(
    !is.na(success_id1), !is.na(success_id2),
    success_id1 != "NA", success_id2 != "NA",
    success_id1 != "",   success_id2 != ""
  ) %>%
  mutate(
    dyad_id_success = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  count(dyad_id_success, name = "frequency") %>%
  right_join(
    all_possible_cerzdyads %>% select(dyad_id_success),
    by = "dyad_id_success"
  ) %>%
  mutate(frequency = replace_na(frequency, 0L)) %>%
  arrange(desc(frequency))

cerz_success_individ <- cerz_success_pairs %>%
  filter(frequency > 0) %>%
  separate(
    col    = dyad_id_success,
    into   = c("ind1", "ind2"),
    sep    = "_",
    remove = FALSE
  ) %>%
  pivot_longer(
    cols       = c(ind1, ind2),
    names_to   = "which_partner",
    values_to  = "individual"
  ) %>%
  group_by(individual) %>%
  summarise(
    success_participations = sum(frequency),
    n_unique_partners      = n_distinct(dyad_id_success),
    .groups = "drop"
  ) %>%
  arrange(desc(n_unique_partners), desc(success_participations)) %>%
  mutate(
    rank = row_number()
  )
View(cerz_success_individ)

write_xlsx(cerz_success_individ, "cerz_success_individ.xlsx")

cerz_success_mean_per_individ <- cerz_success_individ %>%
  summarise(
    mean_successes = mean(success_participations),
    sd_successes   = sd(success_participations),
    min_successes  = min(success_participations),
    max_successes  = max(success_participations),
    n_individuals  = n()
  )

print(cerz_success_mean_per_individ)

print(sum(cooperationrawdata_EMER$success == "YES", na.rm = TRUE))

cooperationrawdata_EMER <- cooperationrawdata_EMER %>%
  mutate(
    dyad_id_success = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  )

all_possible_emerdyads <- EMER_monkeys %>%
  combn(2, simplify = FALSE) %>%
  map_chr(~ paste(sort(.x), collapse = "_")) %>%
  tibble(dyad_id_success = .) %>%
  mutate(possible = TRUE)

emer_success_pairs <- cooperationrawdata_EMER %>%
  filter(
    !is.na(success_id1), !is.na(success_id2),
    success_id1 != "NA", success_id2 != "NA",
    success_id1 != "",   success_id2 != ""
  ) %>%
  mutate(
    dyad_id_success = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  count(dyad_id_success, name = "frequency") %>%
  right_join(
    all_possible_emerdyads %>% select(dyad_id_success),
    by = "dyad_id_success"
  ) %>%
  mutate(frequency = replace_na(frequency, 0L)) %>%
  arrange(desc(frequency))
View(emer_success_pairs)

emer_success_individ <- emer_success_pairs %>%
  filter(frequency > 0) %>%
  separate(
    col    = dyad_id_success,
    into   = c("ind1", "ind2"),
    sep    = "_",
    remove = FALSE
  ) %>%
  pivot_longer(
    cols       = c(ind1, ind2),
    names_to   = "which_partner",
    values_to  = "individual"
  ) %>%
  group_by(individual) %>%
  summarise(
    success_participations = sum(frequency),
    n_unique_partners      = n_distinct(dyad_id_success),
    .groups = "drop"
  ) %>%
  arrange(desc(n_unique_partners), desc(success_participations)) %>%
  mutate(
    rank = row_number()
  )
View(emer_success_individ)

write_xlsx(emer_success_individ, "emer_success_individ.xlsx")

emer_success_mean_per_individ <- emer_success_individ %>%
  summarise(
    mean_successes = mean(success_participations),
    sd_successes   = sd(success_participations),
    min_successes  = min(success_participations),
    max_successes  = max(success_participations),
    n_individuals  = n()
  )

print(emer_success_mean_per_individ)

CERZstrong_bond_success <- cooperationrawdata_CERZ %>%
  filter(success == "YES") %>%
  select(
    file_id, session, session_start, success,
    success_time, success_id1, success_id2, monopolize_id
  )

raw_indexed <- cooperationrawdata_CERZ %>%
  mutate(row_index = row_number())

pull_rows_only <- raw_indexed %>%
  filter(
    !is.na(pull_rope_l_id) |
    !is.na(pull_rope_r_id)
  ) %>%
  select(
    row_index, session, session_start,
    pull_rope_l_id, pull_rope_l_time,
    pull_rope_r_id, pull_rope_r_time
  )

success_rows <- raw_indexed %>%
  filter(success == "YES") %>%
  select(
    row_index, file_id, session, session_start,
    success, success_time, success_id1, success_id2, monopolize_id
  )

CERZ_success_intervaltime <- success_rows %>%
  rowwise() %>%
  mutate(
    .session       = session,
    .session_start = session_start,
    .success_time  = success_time,

    preceding_pulls = list({
      eligible <- pull_rows_only %>%
        filter(session == .session) %>%
        filter(coalesce(pull_rope_l_time, pull_rope_r_time) >= .session_start) %>%
        filter(coalesce(pull_rope_l_time, pull_rope_r_time) <= .success_time) %>%
        mutate(
          puller      = coalesce(pull_rope_l_id,   pull_rope_r_id),
          puller_time = coalesce(pull_rope_l_time, pull_rope_r_time)
        ) %>%
        arrange(puller_time)

      n_rows <- nrow(eligible)

      if (n_rows >= 2) {
        last_pull   <- eligible[n_rows, ]
        last_puller <- last_pull$puller

        partner_row <- NULL
        for (j in (n_rows - 1):1) {
          if (eligible$puller[j] != last_puller) {
            partner_row <- eligible[j, ]
            break
          }
        }

        if (!is.null(partner_row)) {
          bind_rows(partner_row, last_pull)
        } else {
          eligible[0, ]
        }
      } else {
        eligible[0, ]
      }
    }),

    puller1_id   = coalesce(
      preceding_pulls$pull_rope_l_id[1],
      preceding_pulls$pull_rope_r_id[1]
    ),
    puller1_time = coalesce(
      preceding_pulls$pull_rope_l_time[1],
      preceding_pulls$pull_rope_r_time[1]
    ),

    puller2_id   = coalesce(
      preceding_pulls$pull_rope_l_id[2],
      preceding_pulls$pull_rope_r_id[2]
    ),
    puller2_time = coalesce(
      preceding_pulls$pull_rope_l_time[2],
      preceding_pulls$pull_rope_r_time[2]
    )
  ) %>%
  ungroup() %>%
  mutate(
    pull_interval_sec = abs(puller2_time - puller1_time)
  ) %>%
  select(
    file_id, session, session_start, success, success_time,
    success_id1, success_id2, monopolize_id,
    puller1_id, puller1_time,
    puller2_id, puller2_time,
    pull_interval_sec
  )
summary(CERZ_success_intervaltime$pull_interval_sec)
hist(CERZ_success_intervaltime$pull_interval_sec,
     breaks = 40,
     main = "Pull interval in successful attempts - CERZ",
     xlab = "Seconds between pulls")

all_pulls <- cooperationrawdata_CERZ %>%
  mutate(row_index = row_number()) %>%
  filter(
    !is.na(pull_rope_l_id) |
    !is.na(pull_rope_r_id)
  ) %>%
  mutate(
    puller_id   = coalesce(pull_rope_l_id, pull_rope_r_id),
    puller_time = coalesce(pull_rope_l_time, pull_rope_r_time)
  ) %>%
  filter(puller_id != "NA") %>%
  filter(!is.na(puller_time)) %>%
  select(row_index, session, puller_id, puller_time)

all_pulls_classified <- all_pulls %>%
  rowwise() %>%
  mutate(
    .session     = session,
    .puller_id   = puller_id,
    .puller_time = puller_time,

    partner_pull = list(
      all_pulls %>%
        filter(session   == .session) %>%
        filter(puller_id != .puller_id) %>%
        filter(abs(puller_time - .puller_time) <= 10) %>%
        mutate(time_diff  = abs(puller_time - .puller_time)) %>%
        arrange(time_diff) %>%
        slice(1)
    ),

    partner_present = nrow(partner_pull) > 0,
    partner_id      = if (nrow(partner_pull) > 0) partner_pull$puller_id[1]   else NA_character_,
    partner_time    = if (nrow(partner_pull) > 0) partner_pull$puller_time[1] else NA_real_
  ) %>%
  ungroup()

coordinated_attempts <- all_pulls_classified %>%
  filter(partner_present == TRUE) %>%
  mutate(
    dyad_id = map2_chr(
      puller_id, partner_id,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  )

coordinated_attempts_deduped <- coordinated_attempts %>%
  mutate(
    attempt_anchor_time = pmin(puller_time, partner_time),
    attempt_id = paste(
      dyad_id,
      session,
      round(attempt_anchor_time, 1),
      sep = "_"
    )
  ) %>%
  group_by(attempt_id) %>%
  slice(1) %>%
  ungroup()

success_anchored_attempts <- CERZstrong_bond_success %>%
  mutate(
    dyad_id = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  rowwise() %>%
  mutate(
    id1_pull = all_pulls %>%
      filter(session == .data$session,
             puller_id == success_id1,
             puller_time <= success_time,
             puller_time >= success_time - 10) %>%
      nrow(),
    id2_pull = all_pulls %>%
      filter(session == .data$session,
             puller_id == success_id2,
             puller_time <= success_time,
             puller_time >= success_time - 10) %>%
      nrow()
  ) %>%
  ungroup() %>%
  filter(id1_pull > 0, id2_pull > 0) %>%
  select(dyad_id, session, success_time) %>%
  left_join(
    coordinated_attempts_deduped %>%
      group_by(dyad_id, session) %>%
      summarise(already_counted = n(), .groups = "drop"),
    by = c("dyad_id", "session")
  ) %>%
  mutate(already_counted = replace_na(already_counted, 0L)) %>%
  group_by(dyad_id, session) %>%
  mutate(success_n = n()) %>%
  filter(already_counted < success_n) %>%
  slice(seq_len(first(success_n) - first(already_counted))) %>%
  ungroup() %>%
  mutate(
    attempt_anchor_time = success_time,
    attempt_id = paste(dyad_id, session, round(success_time, 1), sep = "_")
  ) %>%
  select(dyad_id, session, attempt_anchor_time, attempt_id)

coordinated_attempts_deduped <- coordinated_attempts_deduped %>%
  bind_rows(success_anchored_attempts) %>%
  arrange(session, attempt_anchor_time)

success_counts <- CERZstrong_bond_success %>%
  mutate(
    dyad_id = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  group_by(dyad_id, session) %>%
  summarise(
    successes_in_session = n(),
    .groups = "drop"
  )

attempts_per_session <- coordinated_attempts_deduped %>%
  group_by(dyad_id, session) %>%
  summarise(
    attempts_in_session = n(),
    .groups = "drop"
  ) %>%
  left_join(success_counts, by = c("dyad_id", "session")) %>%
  mutate(
    successes_in_session = coalesce(successes_in_session, 0L)
  )

CERZ_dyad_attempts <- attempts_per_session %>%
  group_by(dyad_id) %>%
  summarise(
    total_attempts  = sum(attempts_in_session),
    successes       = sum(successes_in_session),
    failed_attempts = total_attempts - successes,
    .groups         = "drop"
  )

cat("Total coordinated attempts across all dyads:\n")
print(sum(CERZ_dyad_attempts$total_attempts))

cat("\nTotal successes — should equal 298:\n")
print(sum(CERZ_dyad_attempts$successes))

cat("\nTotal failed coordinated attempts:\n")
print(sum(CERZ_dyad_attempts$failed_attempts))

cat("\nPer dyad breakdown:\n")
print(CERZ_dyad_attempts)

CERZsuccess_partners <- CERZstrong_bond_success %>%
  mutate(
    dyad_id = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  group_by(dyad_id) %>%
  summarise(
    success_freq = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(success_freq))

CERZ_individual_success_partners <- CERZsuccess_partners %>%
  separate(
    dyad_id,
    into = c("ind1", "ind2"),
    sep = "_",
    remove = FALSE
  ) %>%
  pivot_longer(
    cols = c(ind1, ind2),
    names_to = "position",
    values_to = "individual"
  ) %>%
  group_by(individual) %>%
  summarise(
    unique_successful_partners = n_distinct(dyad_id),
    .groups = "drop"
  ) %>%
  arrange(desc(unique_successful_partners))

CERZ_success_strong_bond_model <- CERZ_dyad_attempts %>%
  select(dyad_id, total_attempts, successes)

CERZ_success_strong_bond_model <- CERZ_success_strong_bond_model %>%
  left_join(
    CERZ_CSI %>%
      rename(dyad_id = dyad) %>%
      select(dyad_id, z_CSI),
    by = "dyad_id"
  )

str(CERZ_success_strong_bond_model)

CERZ_success_strong_bond_model %>%
  summarise(
    total_attempts  = sum(total_attempts),
    total_successes = sum(successes),
    cooperation_rate = sum(successes) / sum(total_attempts)
  )

EMERstrong_bond_success <- cooperationrawdata_EMER %>%
  filter(success == "YES") %>%
  select(
    file_id, session, session_start, success,
    success_time, success_id1, success_id2, monopolize_id
  )

raw_indexed_EMER <- cooperationrawdata_EMER %>%
  mutate(row_index = row_number())

pull_rows_only_EMER <- raw_indexed_EMER %>%
  filter(
    !is.na(pull_rope_l_id) |
    !is.na(pull_rope_r_id)
  ) %>%
  select(
    row_index, session, session_start,
    pull_rope_l_id, pull_rope_l_time,
    pull_rope_r_id, pull_rope_r_time
  )

success_rows_EMER <- raw_indexed_EMER %>%
  filter(success == "YES") %>%
  select(
    row_index, file_id, session, session_start,
    success, success_time, success_id1, success_id2, monopolize_id
  )

EMER_success_intervaltime <- success_rows_EMER %>%
  rowwise() %>%
  mutate(
    .session       = session,
    .session_start = session_start,
    .success_time  = success_time,

    preceding_pulls = list({
      eligible <- pull_rows_only_EMER %>%
        filter(session == .session) %>%
        filter(coalesce(pull_rope_l_time, pull_rope_r_time) >= .session_start) %>%
        filter(coalesce(pull_rope_l_time, pull_rope_r_time) <= .success_time) %>%
        mutate(
          puller      = coalesce(pull_rope_l_id,   pull_rope_r_id),
          puller_time = coalesce(pull_rope_l_time, pull_rope_r_time)
        ) %>%
        arrange(puller_time)

      n_rows <- nrow(eligible)

      if (n_rows >= 2) {
        last_pull   <- eligible[n_rows, ]
        last_puller <- last_pull$puller

        partner_row <- NULL
        for (j in (n_rows - 1):1) {
          if (eligible$puller[j] != last_puller) {
            partner_row <- eligible[j, ]
            break
          }
        }

        if (!is.null(partner_row)) {
          bind_rows(partner_row, last_pull)
        } else {
          eligible[0, ]
        }
      } else {
        eligible[0, ]
      }
    }),

    puller1_id   = coalesce(
      preceding_pulls$pull_rope_l_id[1],
      preceding_pulls$pull_rope_r_id[1]
    ),
    puller1_time = coalesce(
      preceding_pulls$pull_rope_l_time[1],
      preceding_pulls$pull_rope_r_time[1]
    ),

    puller2_id   = coalesce(
      preceding_pulls$pull_rope_l_id[2],
      preceding_pulls$pull_rope_r_id[2]
    ),
    puller2_time = coalesce(
      preceding_pulls$pull_rope_l_time[2],
      preceding_pulls$pull_rope_r_time[2]
    )
  ) %>%
  ungroup() %>%
  mutate(
    pull_interval_sec = abs(puller2_time - puller1_time)
  ) %>%
  select(
    file_id, session, session_start, success, success_time,
    success_id1, success_id2, monopolize_id,
    puller1_id, puller1_time,
    puller2_id, puller2_time,
    pull_interval_sec
  )

summary(EMER_success_intervaltime$pull_interval_sec)
hist(EMER_success_intervaltime$pull_interval_sec,
     breaks = 40,
     main = "Pull interval in successful attempts - EMER",
     xlab = "Seconds between pulls")

all_pulls_EMER <- cooperationrawdata_EMER %>%
  mutate(row_index = row_number()) %>%
  filter(
    !is.na(pull_rope_l_id) |
    !is.na(pull_rope_r_id)
  ) %>%
  mutate(
    puller_id   = coalesce(pull_rope_l_id, pull_rope_r_id),
    puller_time = coalesce(pull_rope_l_time, pull_rope_r_time)
  ) %>%
  filter(puller_id != "NA") %>%
  filter(!is.na(puller_time)) %>%
  select(row_index, session, puller_id, puller_time)

all_pulls_classified_EMER <- all_pulls_EMER %>%
  rowwise() %>%
  mutate(
    .session     = session,
    .puller_id   = puller_id,
    .puller_time = puller_time,

    partner_pull = list(
      all_pulls_EMER %>%
        filter(session   == .session) %>%
        filter(puller_id != .puller_id) %>%
        filter(abs(puller_time - .puller_time) <= 10) %>%
        mutate(time_diff  = abs(puller_time - .puller_time)) %>%
        arrange(time_diff) %>%
        slice(1)
    ),

    partner_present = nrow(partner_pull) > 0,
    partner_id      = if (nrow(partner_pull) > 0) partner_pull$puller_id[1]   else NA_character_,
    partner_time    = if (nrow(partner_pull) > 0) partner_pull$puller_time[1] else NA_real_
  ) %>%
  ungroup()

coordinated_attempts_EMER <- all_pulls_classified_EMER %>%
  filter(partner_present == TRUE) %>%
  mutate(
    dyad_id = map2_chr(
      puller_id, partner_id,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  )

coordinated_attempts_deduped_EMER <- coordinated_attempts_EMER %>%
  mutate(
    attempt_anchor_time = pmin(puller_time, partner_time),
    attempt_id = paste(
      dyad_id,
      session,
      round(attempt_anchor_time, 1),
      sep = "_"
    )
  ) %>%
  group_by(attempt_id) %>%
  slice(1) %>%
  ungroup()

success_anchored_attempts_EMER <- EMERstrong_bond_success %>%
  mutate(
    dyad_id = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  rowwise() %>%
  mutate(
    id1_pull = all_pulls_EMER %>%
      filter(session == .data$session,
             puller_id == success_id1,
             puller_time <= success_time,
             puller_time >= success_time - 10) %>%
      nrow(),
    id2_pull = all_pulls_EMER %>%
      filter(session == .data$session,
             puller_id == success_id2,
             puller_time <= success_time,
             puller_time >= success_time - 10) %>%
      nrow()
  ) %>%
  ungroup() %>%
  filter(id1_pull > 0, id2_pull > 0) %>%
  select(dyad_id, session, success_time) %>%
  left_join(
    coordinated_attempts_deduped_EMER %>%
      group_by(dyad_id, session) %>%
      summarise(already_counted = n(), .groups = "drop"),
    by = c("dyad_id", "session")
  ) %>%
  mutate(already_counted = replace_na(already_counted, 0L)) %>%
  group_by(dyad_id, session) %>%
  mutate(success_n = n()) %>%
  filter(already_counted < success_n) %>%
  slice(seq_len(first(success_n) - first(already_counted))) %>%
  ungroup() %>%
  mutate(
    attempt_anchor_time = success_time,
    attempt_id = paste(dyad_id, session, round(success_time, 1), sep = "_")
  ) %>%
  select(dyad_id, session, attempt_anchor_time, attempt_id)

coordinated_attempts_deduped_EMER <- coordinated_attempts_deduped_EMER %>%
  bind_rows(success_anchored_attempts_EMER) %>%
  arrange(session, attempt_anchor_time)

success_counts_EMER <- EMERstrong_bond_success %>%
  mutate(
    dyad_id = map2_chr(
      success_id1, success_id2,
      ~ paste(sort(c(.x, .y)), collapse = "_")
    )
  ) %>%
  group_by(dyad_id, session) %>%
  summarise(
    successes_in_session = n(),
    .groups = "drop"
  )

attempts_per_session_EMER <- coordinated_attempts_deduped_EMER %>%
  group_by(dyad_id, session) %>%
  summarise(
    attempts_in_session = n(),
    .groups = "drop"
  ) %>%
  left_join(success_counts_EMER, by = c("dyad_id", "session")) %>%
  mutate(
    successes_in_session = coalesce(successes_in_session, 0L)
  )

EMER_dyad_attempts <- attempts_per_session_EMER %>%
  group_by(dyad_id) %>%
  summarise(
    total_attempts  = sum(attempts_in_session),
    successes       = sum(successes_in_session),
    failed_attempts = total_attempts - successes,
    .groups         = "drop"
  )

print(sum(cooperationrawdata_EMER$success == "YES", na.rm = TRUE))

cat("Total coordinated attempts across all dyads — EMER:\n")
print(sum(EMER_dyad_attempts$total_attempts))

cat("\nTotal successes — EMER:\n")
print(sum(EMER_dyad_attempts$successes))

cat("\nTotal failed coordinated attempts — EMER:\n")
print(sum(EMER_dyad_attempts$failed_attempts))

cat("\nPer dyad breakdown — EMER:\n")
print(EMER_dyad_attempts)

missing_successes_EMER <- success_counts_EMER %>%
  anti_join(
    coordinated_attempts_deduped_EMER %>%
      select(dyad_id, session) %>%
      distinct(),
    by = c("dyad_id", "session")
  )

print(missing_successes_EMER)

cat("Unmatched successes:\n")
print(sum(missing_successes_EMER$successes_in_session))

if (nrow(missing_successes_EMER) > 0) {

  missing_check <- EMERstrong_bond_success %>%
    mutate(
      dyad_id = map2_chr(
        success_id1, success_id2,
        ~ paste(sort(c(.x, .y)), collapse = "_")
      )
    ) %>%
    semi_join(missing_successes_EMER, by = c("dyad_id", "session")) %>%
    select(session, dyad_id, success_id1, success_id2, success_time)

  for (i in 1:nrow(missing_check)) {

    cat("\n=====================\n")
    cat("Session:", missing_check$session[i],
        "| Dyad:", missing_check$dyad_id[i],
        "| Success time:", missing_check$success_time[i], "\n")

    relevant_pulls <- all_pulls_EMER %>%
      filter(session == missing_check$session[i]) %>%
      filter(puller_id %in% c(
        missing_check$success_id1[i],
        missing_check$success_id2[i]
      )) %>%
      filter(abs(puller_time - missing_check$success_time[i]) <= 30) %>%
      mutate(
        time_diff_from_success = abs(puller_time - missing_check$success_time[i])
      ) %>%
      arrange(puller_time)

    print(relevant_pulls)
  }

  missing_dyad_entries <- missing_successes_EMER %>%
    mutate(
      total_attempts  = successes_in_session,
      successes       = successes_in_session,
      failed_attempts = 0L
    ) %>%
    select(dyad_id, total_attempts, successes, failed_attempts)

  EMER_dyad_attempts <- EMER_dyad_attempts %>%
    bind_rows(missing_dyad_entries) %>%
    group_by(dyad_id) %>%
    summarise(
      total_attempts  = sum(total_attempts),
      successes       = sum(successes),
      failed_attempts = sum(failed_attempts),
      .groups         = "drop"
    )

  cat("\nMissing entries added to EMER_dyad_attempts\n")

} else {
  cat("No missing cases detected — EMER_dyad_attempts is complete\n")
}

EMER_success_strong_bond_model <- EMER_dyad_attempts %>%
  select(dyad_id, total_attempts, successes)

EMER_success_strong_bond_model <- EMER_success_strong_bond_model %>%
  left_join(
    EMER_CSI %>%
      rename(dyad_id = dyad) %>%
      select(dyad_id, z_CSI),
    by = "dyad_id"
  )

str(EMER_success_strong_bond_model)

EMER_success_strong_bond_model %>%
  summarise(
    total_attempts   = sum(total_attempts),
    total_successes  = sum(successes),
    cooperation_rate = sum(successes) / sum(total_attempts)
  )

CERZ_success_strong_bond_model <- CERZ_success_strong_bond_model %>%
  mutate(group = "CERZ")

EMER_success_strong_bond_model <- EMER_success_strong_bond_model %>%
  mutate(group = "EMER")

combined_success_strong_bond_model <- bind_rows(
  CERZ_success_strong_bond_model,
  EMER_success_strong_bond_model
) %>%
  mutate(
    group = factor(group, levels = c("CERZ", "EMER"))
  )

combined_success_strong_bond_model <- combined_success_strong_bond_model %>%
  separate(
    dyad_id,
    into   = c("ind1", "ind2"),
    sep    = "_",
    remove = FALSE
  )

str(combined_success_strong_bond_model)

combined_success_strong_bond_model %>%
  mutate(cooperation_rate = successes / total_attempts) %>%
  ggplot(aes(x = cooperation_rate)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "white") +
  facet_wrap(~ group) +
  labs(title = "Cooperation rate per dyad",
       x = "Successes / Total attempts",
       y = "Count")

combined_success_strong_bond_model %>%
  group_by(group) %>%
  summarise(
    total_dyads      = n(),
    zero_success     = sum(successes == 0),
    prop_zero        = mean(successes == 0)
  )

combined_success_strong_bond_model %>%
  mutate(
    cooperation_rate     = successes / total_attempts,
    expected_variance    = (successes / total_attempts) *
                           (1 - successes / total_attempts) / total_attempts
  ) %>%
  summarise(
    observed_variance  = var(cooperation_rate, na.rm = TRUE),
    mean_expected_var  = mean(expected_variance, na.rm = TRUE)
  )

summary(combined_success_strong_bond_model$total_attempts)
hist(combined_success_strong_bond_model$total_attempts,
     breaks = 20,
     main = "Distribution of total attempts per dyad",
     xlab = "Total attempts")

combined_success_strong_bond_model %>%
  summarise(
    overall_rate = sum(successes) / sum(total_attempts)
  )

priors_cooperation <- c(
  prior(normal(0, 1), class = b),

  prior(normal(-1.4, 0.5), class = Intercept),

  prior(exponential(1), class = sd),

  prior(beta(1, 1), class = zi)
)

model_cooperation <- brm(
  successes | trials(total_attempts) ~
    z_CSI +
    group +
    (1 | mm(ind1, ind2)) +
    (1 | dyad_id),
  data      = combined_success_strong_bond_model,
  family    = zero_inflated_binomial(),
  prior     = priors_cooperation,
  chains    = 4,
  iter      = 4000,
  warmup    = 1000,
  cores     = 4,
  seed      = 42,
  save_pars = save_pars(all = TRUE),
  control   = list(
    adapt_delta   = 0.99,
    max_treedepth = 12
  )
)

summary(model_cooperation)

model_cooperation_null <- brm(
  successes | trials(total_attempts) ~
    group +
    (1 | mm(ind1, ind2)) +
    (1 | dyad_id),
  data      = combined_success_strong_bond_model,
  family    = zero_inflated_binomial(),
  prior     = c(
    prior(normal(-1.4, 0.5), class = Intercept),
    prior(exponential(1),    class = sd),
    prior(beta(1, 1),        class = zi)
  ),
  chains    = 4,
  iter      = 4000,
  warmup    = 1000,
  cores     = 4,
  seed      = 42,
  save_pars = save_pars(all = TRUE),
  control   = list(
    adapt_delta   = 0.99,
    max_treedepth = 12
  )
)

model_cooperation      <- add_criterion(model_cooperation,      "loo")
model_cooperation_null <- add_criterion(model_cooperation_null, "loo")

loo_compare(model_cooperation, model_cooperation_null)

pp_check(model_cooperation, ndraws = 100)

pp_check(model_cooperation, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated — Cooperation Model",
       x = "Cooperation Success", y = "Density") +
  theme_gray()

ppc_density_cooperation <- pp_check(model_cooperation, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated — Cooperation Model",
       x = "Cooperation Success", y = "Density") +
  theme_gray()

ggsave(
  plot     = ppc_density_cooperation,
  filename = "PPC_density_cooperation.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

pp_check(model_cooperation, type = "stat", stat = "mean", ndraws = 1000) +
  labs(title = "PPC: Mean — Cooperation Model") +
  theme_gray()

ppc_mean_cooperation <- pp_check(model_cooperation, type = "stat", stat = "mean", ndraws = 1000) +
  labs(title = "PPC: Mean — Cooperation Model") +
  theme_gray()

ggsave(
  plot     = ppc_mean_cooperation,
  filename = "PPC_mean_cooperation.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

pp_check(model_cooperation, type = "stat", stat = "sd", ndraws = 1000) +
  labs(title = "PPC: SD — Cooperation Model") +
  theme_gray()

pp_check(model_cooperation, type = "dens_overlay_grouped",
         group = "group", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated by Group",
       x = "Cooperation Success", y = "Density") +
  theme_gray()

csi_seq <- data.frame(
  z_CSI = seq(
    min(combined_success_strong_bond_model$z_CSI, na.rm = TRUE),
    max(combined_success_strong_bond_model$z_CSI, na.rm = TRUE),
    length.out = 100
  ),
  total_attempts = 1,
  group          = factor("CERZ", levels = c("CERZ", "EMER"))
)

csi_fitted <- fitted(
  model_cooperation,
  newdata    = csi_seq,
  re_formula = NA,
  scale      = "response"
)

csi_plot_data <- cbind(csi_seq, csi_fitted)

ggplot(csi_plot_data, aes(x = z_CSI, y = Estimate)) +
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5),
    fill  = "lightblue",
    alpha = 0.4
  ) +
  geom_line(colour = "darkblue", linewidth = 1) +
  geom_rug(
    data        = combined_success_strong_bond_model,
    aes(x       = z_CSI),
    inherit.aes = FALSE,
    sides       = "b",
    alpha       = 0.5
  ) +
  labs(
    title = "Posterior Predicted Cooperation Rate across CSI Bond Strength",
    x     = "CSI Bond Strength (z-scored)",
    y     = "Predicted Cooperation Rate"
  ) +
  theme_gray()

ggsave(
  filename = "CSI_cooperation_posterior_plot.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

success_sessions <- bind_rows(
  attempts_per_session %>% mutate(group = "CERZ"),
  attempts_per_session_EMER %>% mutate(group = "EMER")
) %>%
  mutate(
    group   = as.factor(group),
    dyad_id = as.factor(dyad_id),
    session = as.integer(session)
  ) %>%
  arrange(group, dyad_id, session)

success_sessions <- success_sessions %>%
  mutate(
    attempts_in_session = case_when(
      dyad_id == "DOUGIE_EKAH" & session == 11 & group == "EMER" ~ successes_in_session,
      TRUE ~ attempts_in_session
    )
  )

success_sessions %>%
  filter(successes_in_session > attempts_in_session)

session_learning_model <- glmer(
  cbind(successes_in_session, attempts_in_session - successes_in_session) ~
    session +
    group +
    (1 | dyad_id),
  data   = success_sessions,
  family = binomial(link = "logit")
)

summary(session_learning_model)

session_learning_null <- glmer(
  cbind(successes_in_session, attempts_in_session - successes_in_session) ~
    1 + (1 | dyad_id),
  data   = success_sessions,
  family = binomial(link = "logit")
)

session_learning_zi <- glmmTMB(
  cbind(successes_in_session, attempts_in_session - successes_in_session) ~
    session +
    group +
    (1 | dyad_id),
  ziformula = ~1,
  data   = success_sessions,
  family = binomial(link = "logit")
)

summary(session_learning_zi)

session_learning_zi_null <- glmmTMB(
  cbind(successes_in_session, attempts_in_session - successes_in_session) ~
    1 + (1 | dyad_id),
  ziformula = ~1,
  data   = success_sessions,
  family = binomial(link = "logit")
)

anova(session_learning_null, session_learning_model)

session_learning_simulatedresiduals <- simulateResiduals(
  fittedModel = session_learning_model,
  n = 1000
)

plot(session_learning_simulatedresiduals)

testDispersion(session_learning_simulatedresiduals)

testZeroInflation(session_learning_simulatedresiduals)

anova(session_learning_zi_null, session_learning_zi)

session_learning_zi_residuals <- simulateResiduals(
  fittedModel = session_learning_zi,
  n = 1000
)
plot(session_learning_zi_residuals)
png("DHARMa_ZI_QQplot.png", width = 3000, height = 1500, res = 300)
plot(session_learning_zi_residuals)
dev.off()

testDispersion(session_learning_zi_residuals)
testZeroInflation(session_learning_zi_residuals)

success_sessions$predicted <- predict(session_learning_zi,
                                       type = "response")

success_sessions_plot <- success_sessions %>%
  group_by(session, group) %>%
  summarise(
    success_rate = sum(successes_in_session) / sum(attempts_in_session),
    .groups = "drop"
  )

new_data <- expand.grid(
  session = seq(min(success_sessions$session),
                max(success_sessions$session),
                length.out = 100),
  group   = c("CERZ", "EMER"),
  dyad_id = NA
)

new_data$predicted <- predict(session_learning_zi,
                               newdata  = new_data,
                               type     = "response",
                               allow.new.levels = TRUE)

ggplot(success_sessions_plot, aes(x = session, y = success_rate, colour = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(data = new_data, aes(x = session, y = predicted, colour = group),
              method    = "loess",
              se        = TRUE,
              linewidth = 0.8) +
  scale_colour_manual(values = c("CERZ" = "orange", "EMER" = "#0072B2")) +
  scale_fill_manual(values   = c("CERZ" = "orange", "EMER" = "#0072B2")) +
  labs(
    x      = "Session",
    y      = "Success rate (successes / attempts)",
    colour = "Group"
  ) +
  theme_classic()

ggsave(
  filename = "session_learning_plot.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

CERZ_centrality_scores %>%
  select(degree, strength, eigenvector) %>%
  cor() %>%
  round(2)

EMER_centrality_scores %>%
  select(degree, strength, eigenvector) %>%
  cor() %>%
  round(2)

cerz_success_individ <- cerz_success_individ %>% mutate(group = "CERZ")
emer_success_individ <- emer_success_individ %>% mutate(group = "EMER")

combined_individ_success <- bind_rows(cerz_success_individ, emer_success_individ)

hist(combined_individ_success$n_unique_partners,
     breaks = 10,
     main = "Distribution of unique cooperative partners per individual",
     xlab = "Number of unique cooperative partners")

partner_distribution_check <- combined_individ_success %>%
  ggplot(aes(x = n_unique_partners)) +
  geom_histogram(bins = 10, fill = "steelblue", colour = "white") +
  facet_wrap(~ group) +
  labs(title = "Unique cooperative partners per individual",
       x = "Number of unique partners",
       y = "Count") +
  theme_minimal(base_size = 14)

print(partner_distribution_check)

ggsave(filename = "unique_cooperative_partners_per_individual.png",
       plot = partner_distribution_check,
       width = 8,
       height = 6,
       dpi = 300,
       bg = "white")

combined_individ_success %>%
  group_by(group) %>%
  summarise(
    n_individuals  = n(),
    n_zeros        = sum(n_unique_partners == 0),
    prop_zeros     = mean(n_unique_partners == 0),
    mean_partners  = mean(n_unique_partners),
    max_partners   = max(n_unique_partners)
  )

combined_individ_success %>%
  summarise(
    mean_partners     = mean(n_unique_partners),
    variance_partners = var(n_unique_partners),
    dispersion_ratio  = var(n_unique_partners) / mean(n_unique_partners)
  )

combined_individ_success %>%
  group_by(group) %>%
  summarise(
    total_individuals    = n(),
    successful_individ   = sum(n_unique_partners > 0),
    unsuccessful_individ = sum(n_unique_partners == 0)
  )

cerz_success_individ <- cerz_success_individ %>%
  mutate(
    group                 = "CERZ",
    max_possible_partners = 10
  )

emer_success_individ <- emer_success_individ %>%
  mutate(
    group                 = "EMER",
    max_possible_partners = 8
  )

combined_individ_success <- bind_rows(
  cerz_success_individ,
  emer_success_individ
) %>%
  mutate(group = factor(group, levels = c("CERZ", "EMER")))

centrality_combined <- bind_rows(
  CERZ_centrality_scores %>%
    select(individual, eigenvector) %>%
    mutate(group = "CERZ"),
  EMER_centrality_scores %>%
    select(individual, eigenvector) %>%
    mutate(group = "EMER")
)

centrality_model <- combined_individ_success %>%
  left_join(
    centrality_combined,
    by = c("individual", "group")
  )

centrality_model <- centrality_model %>%
  mutate(
    z_eigenvector = as.numeric(scale(eigenvector))
  )

str(centrality_model)

priors_centrality <- c(
  prior(normal(0, 1), class = b),

  prior(normal(-0.53, 0.5), class = Intercept),

  prior(exponential(1), class = sd)
)

model_centrality_eigenvector <- brm(
  n_unique_partners | trials(max_possible_partners) ~
    z_eigenvector +
    group +
    (1 | individual),
  data      = centrality_model,
  family    = binomial(link = "logit"),
  prior     = priors_centrality,
  chains    = 4,
  iter      = 4000,
  warmup    = 1000,
  cores     = 4,
  seed      = 42,
  save_pars = save_pars(all = TRUE),
  control   = list(
    adapt_delta   = 0.95,
    max_treedepth = 12
  )
)

summary(model_centrality_eigenvector)

model_centrality_null <- brm(
  n_unique_partners | trials(max_possible_partners) ~
    group +
    (1 | individual),
  data      = centrality_model,
  family    = binomial(link = "logit"),
  prior     = c(
    prior(normal(-0.53, 0.5), class = Intercept),
    prior(exponential(1),     class = sd)
  ),
  chains    = 4,
  iter      = 4000,
  warmup    = 1000,
  cores     = 4,
  seed      = 42,
  save_pars = save_pars(all = TRUE),
  control   = list(
    adapt_delta   = 0.95,
    max_treedepth = 12
  )
)

model_centrality_eigenvector <- add_criterion(model_centrality_eigenvector, "loo")
model_centrality_null        <- add_criterion(model_centrality_null,        "loo")

loo_compare(model_centrality_eigenvector, model_centrality_null)

pp_check(model_centrality_eigenvector, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated — Eigenvector Centrality Model",
       x = "Number of Unique Cooperative Partners", y = "Density") +
  theme_gray()

ppc_density_eigenvector <- pp_check(model_centrality_eigenvector, type = "dens_overlay", ndraws = 50) +
  labs(title = "PPC: Observed vs Replicated — Eigenvector Centrality Model",
       x = "Number of Unique Cooperative Partners", y = "Density") +
  theme_gray()

ggsave(
  plot     = ppc_density_eigenvector,
  filename = "PPC_density_eigenvector.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

pp_check(model_centrality_eigenvector, type = "stat", stat = "mean", ndraws = 1000) +
  labs(title = "PPC: Mean — Eigenvector Centrality Model") +
  theme_gray()

ppc_mean_eigenvector <- pp_check(model_centrality_eigenvector, type = "stat", stat = "mean", ndraws = 1000) +
  labs(title = "PPC: Mean — Eigenvector Centrality Model") +
  theme_gray()

ggsave(
  plot     = ppc_mean_eigenvector,
  filename = "PPC_mean_eigenvector.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)

eigenvector_seq <- data.frame(
  z_eigenvector          = seq(
    min(centrality_model$z_eigenvector, na.rm = TRUE),
    max(centrality_model$z_eigenvector, na.rm = TRUE),
    length.out = 100
  ),
  max_possible_partners = 10,
  group                 = factor("CERZ", levels = c("CERZ", "EMER"))
)

eigenvector_fitted <- fitted(
  model_centrality_eigenvector,
  newdata    = eigenvector_seq,
  re_formula = NA,
  scale      = "response"
)

eigenvector_plot_data <- cbind(eigenvector_seq, eigenvector_fitted)

eigenvector_plot <- ggplot(eigenvector_plot_data, aes(x = z_eigenvector, y = Estimate)) +
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5),
    fill  = "lightblue",
    alpha = 0.4
  ) +
  geom_line(colour = "darkblue", linewidth = 1) +
  geom_rug(
    data        = centrality_model,
    aes(x       = z_eigenvector),
    inherit.aes = FALSE,
    sides       = "b",
    alpha       = 0.5
  ) +
  labs(
    title = "Posterior Predicted Cooperative Partner Breadth across Eigenvector Centrality",
    x     = "Eigenvector Centrality (z-scored)",
    y     = "Predicted Proportion of Cooperative Partners"
  ) +
  theme_gray()

eigenvector_plot

ggsave(
  plot     = eigenvector_plot,
  filename = "eigenvector_cooperation_posterior_plot.png",
  width    = 8,
  height   = 5,
  dpi      = 300
)
