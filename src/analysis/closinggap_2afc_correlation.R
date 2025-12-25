library(dplyr)
library(ggplot2)
library(ggh4x)
library(svglite)
library(tidyverse)


# ========read data=============
setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")
data <- readr::read_csv("D:/OneDrive/projects/numerosity_closing_gap/data/closinggap_2afc/data_closinggap_2afc.csv")
to_remove <- c(911343, 309153)

data <- data %>% 
  filter(!participant %in% to_remove)

data$chose_probe_binary <- as.numeric(data$choice_display_more == "chose_probe")


data <- data %>%
  mutate(
    # 1 =  chose radial as more numerous, 0 =  chose tangential
    chose_radial = case_when(
      choice_display_more == "chose_reference" & reference_type == "radial" ~ 1,
      choice_display_more == "chose_reference" & reference_type == "tangential" ~ 0,
      choice_display_more == "chose_probe" & reference_type == "radial" ~ 0,
      choice_display_more == "chose_probe" & reference_type == "tangential" ~ 1,
      TRUE ~ NA_real_ 
    )
  )

data <- data %>%
  mutate(
    radial_num = if_else(reference_type == "radial", reference_num, probe_num),
    tangential_num = if_else(reference_type == "tangential", reference_num, probe_num)
  )

data <- data %>%
  mutate(
    diff = radial_num - tangential_num
  )

# ========== correlation ==============
# large numerosity index abs(radial_pse) + abs(tangential_pse)
# RM range indix, sum of -(propotion chosing radial) for diff =-1,0 and 1 

# data RM range
data_rm_range <- data %>%
  filter(sector_angle == 0)

data_by_subject_rm <- data_rm_range %>% 
  group_by(diff, participant) %>% 
  summarise(
    n = n(),
    prop_choose_radial = mean(chose_radial),
    .groups = 'drop'
  )

aniso_small <- data_by_subject_rm %>% 
  dplyr::select(participant, diff, prop_choose_radial) %>% 
  dplyr::mutate(
    diff = dplyr::case_when(
      diff == -1 ~ "m1",
      diff ==  0 ~ "0",
      diff ==  1 ~ "p1",
      TRUE ~ as.character(diff)
    )
  ) %>% 
  tidyr::pivot_wider(
    names_from  = diff,
    values_from = prop_choose_radial,
    names_prefix = "diff_"
  ) %>% 
  dplyr::mutate(
    S = diff_m1 + diff_0,
    index_rm = -S
  ) %>% 
  dplyr::select(participant, index_rm)


# data numerosity range
pses_subj <- readr::read_csv("pses_subj.csv")

pses_subj <- pses_subj %>% 
  filter(!participant %in% to_remove)

aniso_large_by_ref <- pses_subj %>%
  group_by(participant, reference_num, probe_type) %>%
  summarise(
    mean_PSE_bias = mean(PSE_bias, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = probe_type,
    values_from = mean_PSE_bias
  ) %>%
  mutate(
    aniso_largeN = abs(radial) + abs(tangential))

aniso_large_wide <- aniso_large_by_ref %>%
  select(
    participant,
    reference_num,
    aniso_largeN
  ) %>%
  pivot_wider(
    names_from  = reference_num,
    values_from = c(
      aniso_largeN
    ),
    names_glue = "{.value}_ref_{reference_num}"
  )


# aniso_large_wide_oneidx <- aniso_large_wide %>%
#   mutate(
#     index_large = rowMeans(
#       select(
#         ., starts_with("aniso_")
#       ),
#       na.rm = TRUE
#     )
#   )
#   
# aniso_large_wide_oneidx <- inner_join(aniso_large_wide_oneidx, aniso_small, by = "participant")
# 
# cor.test(aniso_large_wide_oneidx$index_large, aniso_large_wide_oneidx$index_rm, method = "pearson")


aniso_large_wide <- aniso_large_wide[complete.cases(aniso_large_wide), ]

aniso_large_wide <- inner_join(aniso_large_wide, aniso_small, by = "participant")


# get df to plot, get p values
dat <- aniso_large_wide %>% select(-participant)
vars <- names(dat)

r_mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
p_mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))

for (i in seq_along(vars)) {
  for (j in seq_along(vars)) {
    x <- dat[[i]]
    y <- dat[[j]]
    ok <- complete.cases(x, y)
    if (sum(ok) >= 3) {
      ct <- cor.test(x[ok], y[ok], method = "pearson")
      r_mat[i, j] <- unname(ct$estimate)
      p_mat[i, j] <- ct$p.value
    }
  }
}

corr_df <- as.data.frame(r_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
  left_join(
    as.data.frame(p_mat) %>%
      rownames_to_column("var1") %>%
      pivot_longer(-var1, names_to = "var2", values_to = "p"),
    by = c("var1", "var2")
  )

corr_df <- corr_df %>%
  mutate(
    sig_star = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    )
  )

corr_df <- corr_df %>%
  mutate(sig_star = if_else(var1 == var2, "", sig_star))


# sort var in order
var_order <- c(
  "index_rm",
  "aniso_largeN_ref_9",
  "aniso_largeN_ref_13",
  "aniso_largeN_ref_18",
  "aniso_largeN_ref_24",
  "aniso_largeN_ref_30"
)

corr_df <- corr_df %>%
  mutate(
    var1 = factor(var1, levels = var_order),
    var2 = factor(var2, levels = var_order)
  )

# full matrix
ggplot(corr_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "r"
  ) +
  
  geom_text(
    aes(label = sprintf("%.2f", r)),
    size = 3,
    color = "black"
  ) +
  
  geom_text(
    aes(label = sig_star),
    size = 5,
    vjust = -0.2,
    color = "black"
  ) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# half matrix
ggplot(corr_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(
    data = subset(corr_df, as.integer(var1) >= as.integer(var2)),
    color = "white", linewidth = 0.4
  ) +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  geom_text(
    data = subset(corr_df, as.integer(var1) >= as.integer(var2)),
    aes(label = sprintf("%.2f", r)),
    size = 3, 
    color = "black"
  ) +
  geom_text(
    data = subset(corr_df, as.integer(var1) >= as.integer(var2)),
    aes(label = sig_star),
    size = 3, 
    vjust = -0.2,
    color = "black"
  ) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# ========== correlation 2==============
# separate index for 2 condition per reference_num

aniso_large_by_ref <- pses_subj %>%
  group_by(participant, reference_num, probe_type) %>%
  summarise(
    mean_PSE_bias = mean(PSE_bias, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from  = probe_type,
    values_from = mean_PSE_bias
  ) %>%
  mutate(
    aniso_largeN_radial = abs(radial),
    aniso_largeN_tangential = abs(tangential))

aniso_large_wide <- aniso_large_by_ref %>%
  select(
    participant,
    reference_num,
    aniso_largeN_radial,
    aniso_largeN_tangential
  ) %>%
  pivot_wider(
    names_from  = reference_num,
    values_from = c(
      aniso_largeN_radial,
      aniso_largeN_tangential
    ),
    names_glue = "{.value}_ref_{reference_num}"
  )


aniso_large_wide <- aniso_large_wide[complete.cases(aniso_large_wide), ]

aniso_large_wide <- inner_join(aniso_large_wide, aniso_small, by = "participant")


corr_mat <- cor(
  aniso_large_wide %>% select(-participant),
  use = "pairwise.complete.obs",
  method = "pearson"
)

corr_mat

corr_df <- as.data.frame(corr_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "r"
  )

var_order <- c(
  "index_rm",
  "aniso_largeN_radial_ref_9",
  "aniso_largeN_tangential_ref_9",
  "aniso_largeN_radial_ref_13",
  "aniso_largeN_tangential_ref_13",
  "aniso_largeN_radial_ref_18",
  "aniso_largeN_tangential_ref_18",
  "aniso_largeN_radial_ref_24",
  "aniso_largeN_tangential_ref_24",
  "aniso_largeN_radial_ref_30",
  "aniso_largeN_tangential_ref_30"
)

corr_df <- corr_df %>%
  mutate(
    var1 = factor(var1, levels = var_order),
    var2 = factor(var2, levels = var_order)
  )

ggplot(corr_df, aes(x = var1, y = var2, fill = r)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "r"
  ) +
  
  geom_text(
    aes(label = sprintf("%.2f", r)),
    size = 3,
    color = "black"
  ) +
  
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

