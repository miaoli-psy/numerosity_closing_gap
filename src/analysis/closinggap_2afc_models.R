library(dplyr)
library(ggplot2)
library(ggh4x)
library(svglite)
library(tidyverse)
library(quickpsy)
library(lme4)

# ==========read data=========
setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")


# ========Numerosity ranges,  losgistic fitting data======
fit_psychometric_subj <- readRDS("fit_psychometric_subj.rds")

# get pse, jnd, weber fraction (quickpsy -losgistic returns P1, P2 as alpha and beta, respectively)
pses_subj <- fit_psychometric_subj$par %>%
  select(participant, probe_type, reference_num, parn, par) %>%
  pivot_wider(names_from = parn, values_from = par) %>%
  mutate(
    PSE      = p1,
    beta     = p2,
    abs_beta = abs(beta),
    PSE_bias = PSE - reference_num,
    JND      = log(3) / abs_beta,
    WF       = JND / PSE # use jnd/pse instead of jnd/ref_num
  )

pses_subj <- pses_subj %>%
  mutate(
    PSE_bias_corrected = ifelse(probe_type == "tangential", -PSE_bias, PSE_bias)
  )

# pses_subj <- pses_subj %>%
#   mutate(slope_at_pse = beta / 4)
# 
# beta_cutoff <- quantile(pses_subj$abs_beta, probs = 0.05, na.rm = TRUE)
# 
# pses_subj <- pses_subj %>%
#   filter(abs_beta > beta_cutoff)

# remove participants that unable to do the task - based on fitting
to_remove <- c(309153, 71282, 719357, 967470, 911343)

pses_subj <- pses_subj %>% 
  filter(!participant %in% to_remove)

# pses_subj <- pses_subj %>% 
#   filter(PSE_bias_corrected > -1)
# 

# # combine 2 ref-probe configuration
# anisotropy_df <- pses_subj %>%
#   select(participant, probe_type, reference_num, PSE_bias, WF) %>%
#   pivot_wider(
#     names_from  = probe_type,
#     values_from = c(PSE_bias, WF),
#   ) %>%
#   filter(!is.na(PSE_bias_radial) & !is.na(PSE_bias_tangential)) %>%
#   mutate(
#     Anisotropy = (PSE_bias_radial - PSE_bias_tangential)/2
#   )


# remove extreme values: for each ref_num, a pse value was considered an extreme value
# and excluded if it deviated from the median by more than 2.5 MAD

# MAD: defened as the median of the absolute deviation from that median (better than 3std, 
# more stable when distribution are non-gaussian)
# 43 trials out of 330 trials are removed (13.0%)

pses_subj <- pses_subj %>%
  group_by(reference_num) %>%
  mutate(
    pse_med = median(PSE_bias_corrected, na.rm = TRUE),
    pse_mad = mad(PSE_bias_corrected, na.rm = TRUE),
    keep    = (PSE_bias_corrected >= pse_med - 2.5 * pse_mad) &
      (PSE_bias_corrected <= pse_med + 2.5 * pse_mad)
  ) %>%
  filter(keep) %>%
  ungroup() %>%
  select(-pse_med, -pse_mad, -keep)


model_anisotropy <- lmer(
  PSE_bias_corrected ~ factor(reference_num) + (1 | participant),
  data = pses_subj, REML = TRUE)

summary(model_anisotropy)

sjPlot::tab_model(
  model_anisotropy,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

emm <- emmeans::emmeans(model_anisotropy, ~ factor(reference_num))
emmeans::test(emm, adjust = "holm")  # each estimated marginal mean vs. 0


# Overall test: is anisotropy non-zero across all numerosities?
# (intercept-only model)
model_intercept <- lmer(
  PSE_bias_corrected ~ 1 + (1 | participant),
  data = pses_subj, REML = TRUE
)
summary(model_intercept)  # intercept t-test = overall anisotropy vs. 0

sjPlot::tab_model(
  model_intercept,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

average_pses_df <- pses_subj %>% 
  group_by(reference_num) %>% 
  summarise(
    PSE_bias_mean = mean(PSE_bias_corrected),
    PSE_bias_std = sd(PSE_bias_corrected),
    WF_mean = mean(WF),
    WF_std = sd(WF),
    n = n()
  ) %>% 
  mutate(
    PSE_sem = PSE_bias_std/sqrt(n),
    PSE_CI = PSE_sem * qt((1 - 0.05) / 2 + .5, n - 1),
    WF_sem = WF_std/sqrt(n),
    WF_CI = WF_sem * qt((1 - 0.05) / 2 + .5, n - 1)
  )



my_plot_pse <- ggplot()+
  geom_point(
    data = pses_subj,
    aes(
      x = factor(reference_num),
      y = PSE_bias_corrected,
      size = 3
    ),
    stat = "identity",
    color  = "#707070",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.05
  ) +
  
  geom_point(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      size = 3
    ),
    stat = "identity",
    color  = "#707070",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.8
  ) +
  
  geom_errorbar(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      ymin = PSE_bias_mean - PSE_CI,
      ymax = PSE_bias_mean + PSE_CI
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    color = "black",
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "PSE Anisotropy (items)", x = "Reference Numerosity") +
  
  # scale_color_manual(
  #   labels = c("radial", "tangential"),
  #   values = c("#800074", "#707070"),
  #   name   = "Probe Arrangement"
  # ) +
  # 
  
  scale_y_continuous(limits = c(-2, 11)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size  = 14,
      face  = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size  = 14,
      face  = "bold"
    ),
    panel.border       = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    axis.line          = element_line(colour = "grey"),
    axis.text.x        = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
    axis.text.y        = element_text(size = 12, face = "bold"),
    legend.title       = element_text(size = 12, face = "bold"),
    legend.text        = element_text(size = 10),
    strip.text.x       = element_text(size = 12, face = "bold"),
    panel.spacing      = unit(1.0, "lines")
  )

my_plot_pse

##Positive values indicate that radial arrangements were perceived as 
##less numerous than tangential arrangements. 
## PSE bias for tangential-probe trials was sign-reversed so that 
## both configurations index the same perceptual anisotropy

# ggsave(file = "my_plot_pse.svg", plot = my_plot_pse, width = 5, height = 5, units = "in")


# # plot wf
# 
# my_plot_wf <- ggplot()+
#   
#   geom_point(
#     data = pses_subj,
#     aes(
#       x = factor(reference_num),
#       y = WF,
#       size = 3
#     ),
#     stat = "identity",
#     position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
#     alpha = 0.1
#   ) +
#   geom_point(
#     data = average_pses_df,
#     aes(
#       x = factor(reference_num),
#       y = WF_mean,
#       size = 3
#     ),
#     stat = "identity",
#     position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
#     alpha = 1
#   ) +
#   
#   geom_errorbar(
#     data = average_pses_df,
#     aes(
#       x = factor(reference_num),
#       y = WF_mean,
#       ymin = WF_mean - WF_CI,
#       ymax = WF_mean + WF_CI
#     ),
#     size  = 0.8,
#     width = .00,
#     alpha = 1,
#     color = "black",
#     position = position_dodge(width = 0.2)
#   ) +
#   
#   labs(y = "Weber fraction", x = "Reference Numerosity") +
#   
#   
#   scale_y_continuous(limits = c(0, 0.6)) +
#   
#   theme(
#     axis.title.x = element_text(
#       color = "black",
#       size  = 14,
#       face  = "bold"
#     ),
#     axis.title.y = element_text(
#       color = "black",
#       size  = 14,
#       face  = "bold"
#     ),
#     panel.border       = element_blank(),
#     panel.grid.major   = element_blank(),
#     panel.grid.minor   = element_blank(),
#     panel.background   = element_blank(),
#     axis.line          = element_line(colour = "grey"),
#     axis.text.x        = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
#     axis.text.y        = element_text(size = 12, face = "bold"),
#     legend.title       = element_text(size = 12, face = "bold"),
#     legend.text        = element_text(size = 10),
#     strip.text.x       = element_text(size = 12, face = "bold"),
#     panel.spacing      = unit(1.0, "lines")
#   )
# 
# my_plot_wf

# ggsave(file = "my_plot_wf.svg", plot = my_plot_wf, width = 7, height = 5, units = "in")

# ======================RM ranges==================

# read data RM range
data <- readr::read_csv("D:/OneDrive/projects/numerosity_closing_gap/data/closinggap_2afc/data_closinggap_2afc.csv")

data_rm_range <- data %>%
  filter(sector_angle == 0)

# to_remove <- c(911343, 309153)
to_remove <- c(309153, 71282, 719357, 967470, 911343)


data_rm_range <- data_rm_range %>% 
  filter(!participant %in% to_remove)

# get numerosity difference
data_rm_range <- data_rm_range %>% 
  mutate(
    delta = probe_num - reference_num
  )

data_rm_range$chose_probe_binary <- as.numeric(data_rm_range$choice_display_more == "chose_probe")


data_rm_range <- data_rm_range %>%
  mutate(
    delta_corrected = ifelse(probe_type == "tangential", -delta, delta),
    # flip the response for tangential trials:
    # "chose probe" when tangential is probe and delta is negative 
    # is equivalent to "chose reference" in the flipped frame
    chose_corrected = ifelse(probe_type == "tangential", 
                             1 - chose_probe_binary, 
                             chose_probe_binary)
  )

glmm_rm_combined <- glmer(
  chose_corrected ~ delta_corrected + (1 + delta_corrected | participant),
  family = binomial,
  data = data_rm_range
)

summary(glmm_rm_combined)

sjPlot::tab_model(
  glmm_rm_combined,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

# ----group-level PSE ----
b_fix <- fixef(glmm_rm_combined)
# PSE = -intercept / slope (the delta at which P = 0.5)
group_PSE <- -b_fix["(Intercept)"] / b_fix["delta_corrected"]
group_JND <- log(3) / abs(b_fix["delta_corrected"])

cat("Group PSE (anisotropy):", round(group_PSE, 3), "\n")
cat("Group JND:", round(group_JND, 3), "\n")



# ----  participant-level PSEs ----
re <- ranef(glmm_rm_combined)$participant

subj_params_rm <- re %>%
  as.data.frame() %>%
  tibble::rownames_to_column("participant") %>%
  transmute(
    participant,
    intercept = b_fix["(Intercept)"] + `(Intercept)`,
    slope     = b_fix["delta_corrected"] + delta_corrected,
    PSE       = -intercept / slope,
    JND       = log(3) / abs(slope)
  )


# Check for problematic participants
subj_params_rm %>%
  arrange(slope) %>%
  head(10)

subj_params_rm <- subj_params_rm %>%
  filter(abs(slope) > 0.5)  # adjust threshold as needed

# ---- summary stats and plot ----
params_rm_combined <- subj_params_rm %>%
  summarise(
    mean_PSE = mean(PSE),
    sd_PSE   = sd(PSE),
    n        = n(),
    sem_PSE  = sd_PSE / sqrt(n),
    CI_PSE   = sem_PSE * qt(0.975, n - 1)
  )

my_plot_rm_combined <- ggplot() +
  geom_point(
    data = subj_params_rm,
    aes(x = "",
        y = PSE),
    color = "#707070",
    size = 3,
    alpha = 0.15,
    position = position_jitter(width = 0.05)
  ) +
  geom_point(
    data = params_rm_combined,
    aes(x = "", 
        y = mean_PSE),
    color = "#707070", size = 4
  ) +
  geom_errorbar(
    data = params_rm_combined,
    aes(x = "", 
        y = mean_PSE,
        ymin = mean_PSE - CI_PSE,
        ymax = mean_PSE + CI_PSE),
    width = 0, size = 0.8, color = "black"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  scale_y_continuous(limits = c(-2, 11)) +
  
  labs(y = "PSE Anisotropy (items)", x = "RM Range") +
  theme(
    axis.title.y       = element_text(color = "black", size = 14, face = "bold"),
    axis.title.x       = element_text(color = "black", size = 14, face = "bold"),
    panel.border       = element_blank(),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.background   = element_blank(),
    axis.line          = element_line(colour = "grey"),
    axis.text.y        = element_text(size = 12, face = "bold"),
    axis.text.x        = element_text(size = 12, face = "bold")
  )

my_plot_rm_combined

# ggsave(file = "my_plot_rm_combined.svg", plot = my_plot_rm_combined, width = 2, height = 5, units = "in")




# # plot RM
# data_by_subject_rm2 <- data_rm_range %>% 
#   group_by(probe_type, delta, participant) %>% 
#   summarise(
#     n = n(),
#     prop_choose_probe = mean(choice_display_more == "chose_probe"),
#     .groups = 'drop'
#   )
# 
# 
# data_across_subject_rm2 <- data_by_subject_rm2 %>% 
#   group_by(probe_type, delta) %>% 
#   summarise(
#     mean_prop_probe = mean(prop_choose_probe),
#     sd_prop_probe = sd(prop_choose_probe),
#     n = n(),
#     .groups = 'drop'
#   ) %>% 
#   mutate(
#     sem = sd_prop_probe / sqrt(n),
#     ci = sem * qt(0.975, df = n - 1)
#   )
# 
# plot_rm2 <- ggplot() +
#   
#   geom_point(
#     data = data_by_subject_rm2,
#     aes(
#       x = delta,
#       y = prop_choose_probe,
#       color = probe_type,
#       group = probe_type
#     ),
#     size = 4,
#     alpha = 0.1,
#     stat = "identity",
#     position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
#   ) +
#   
#   geom_point(
#     data = data_across_subject_rm2,
#     aes(
#       x = delta,
#       y = mean_prop_probe,
#       color = probe_type,
#       group = probe_type
#     ),
#     size = 4,
#     alpha = 0.8,
#     stat = "identity",
#     position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
#   ) +
#   
#   
#   geom_errorbar(data = data_across_subject_rm2, 
#                 aes(x = delta,
#                     y = mean_prop_probe,
#                     ymin = mean_prop_probe - ci,
#                     ymax = mean_prop_probe + ci,
#                     group = probe_type
#                     ),
#                 size  = 0.8,
#                 width = .00,
#                 alpha = 1,
#                 color = "black",
#                 position = position_dodge(width = 0.2)
#   ) +
#   
#   scale_y_continuous(limits = c(0, 1.1),
#                      breaks = seq(0, 1, by = 0.5)) +
#   
#   scale_x_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
#   
#   geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
#   
#   scale_color_manual(labels = c("radial", "tangential"),
#                      values = c("#BB5566", "#004488"),
#                      name = "Probe Arrangement") +
#   
#   
#   labs(y = "Proportion Choosing Probe", x = "Numerosity Difference (Probe - Reference)") + 
#   
#   theme(
#     axis.title.x = element_text(
#       color = "black",
#       size = 14,
#       face = "bold"
#     ),
#     axis.title.y = element_text(
#       color = "black",
#       size = 14,
#       face = "bold"
#     ),
#     panel.border = element_blank(),
#     # remove panel grid lines
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     # remove panel background
#     panel.background = element_blank(),
#     # add axis line
#     axis.line = element_line(colour = "grey"),
#     # x,y axis tick labels
#     axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
#     axis.text.y = element_text(size = 12, face = "bold"),
#     # legend size
#     legend.title = element_text(size = 12, face = "bold"),
#     legend.text = element_text(size = 10),
#     # facet wrap title
#     strip.text.x = element_text(size = 12, face = "bold"),
#     panel.spacing = unit(1.0, "lines")
#   ) 
# 
# plot_rm2

# ggsave(file = "plot_rm2.svg", plot = plot_rm2, width = 7, height = 5, units = "in")


# GLMM -Choose radial binary

data_rm_range2 <- data %>%
  filter(sector_angle == 0)

# to_remove <- c(911343, 309153)
to_remove <- c(309153, 71282, 719357, 967470, 911343)

data_rm_range2 <- data_rm_range2 %>% 
  filter(!participant %in% to_remove)


# arrange data
data_rm_range2 <- data_rm_range2 %>%
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

data_rm_range2 <- data_rm_range2 %>%
  mutate(
    radial_num = if_else(reference_type == "radial", reference_num, probe_num),
    tangential_num = if_else(reference_type == "tangential", reference_num, probe_num)
  )

data_rm_range2 <- data_rm_range2 %>%
  mutate(
    diff = radial_num - tangential_num
  )

data_rm_range2$reference_num <- as.factor(data_rm_range2$reference_num)

# data_rm_range2$reference_num <- as.numeric(as.character(data_rm_range2$reference_num))
# data_rm_range$reference_num <- scale(data_rm_range$reference_num, center = TRUE, scale = FALSE)


model_rm <- glmer(
  chose_radial ~ diff + reference_num + (1 | participant),
  data = data_rm_range2,
  family = binomial
)

summary(model_rm)


ems <- emmeans::emmeans(
  model_rm,
  ~ reference_num,
  at = list(diff = 0),
  type = "response"
)
ems

# prob: when radial and tangential contain the same number of item, participants
# choose the radial display only 16.7%, 11.9% and 9.0% of the time for set size 3, 4, 5, respectively
# -> when numerosity is physically equal, participants choose radial as less numerous


# arrange data plot
data_by_subject_rm2<- data_rm_range2 %>% 
  group_by(reference_num, diff, participant) %>% 
  summarise(
    n = n(),
    prop_choose_radial = mean(chose_radial),
    .groups = 'drop'
  )


data_across_subject_rm2<- data_by_subject_rm2 %>% 
  group_by(reference_num, diff) %>% 
  summarise(
    mean_prop_radial = mean(prop_choose_radial),
    sd_prop_radial = sd(prop_choose_radial),
    n = n(),
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_prop_radial / sqrt(n),
    ci = sem * qt(0.975, df = n - 1)
  )


# propotion choose radial
plot_rm2 <- ggplot() +
  geom_point(
    data = data_across_subject_rm2,
    aes(
      x = diff,
      y = mean_prop_radial,
      color = reference_num,
      group = reference_num
    ),
    size = 4,
    alpha = 0.8,
    position = position_dodge(0.2)
  ) +
  
  geom_errorbar(data = data_across_subject_rm2, 
                aes(x = diff,
                    y = mean_prop_radial,
                    ymin = mean_prop_radial - ci,
                    ymax = mean_prop_radial + ci,
                    group = reference_num),
                
                size  = 1,
                width = .00,
                color = "black",
                position = position_dodge(0.2)) +
  
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, by = 0.5)) +
  
  scale_x_continuous(limits = c(-1.2, 1.2),
                     breaks = seq(-1, 1, by = 1)) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  

  scale_color_manual(labels = c("3", "4", "5"),
                     values = c("#0072B2", "#D55E00", "#5e4c5f"),
                     name = "Reference Number") +
  
  
  labs(y = "Proportion Choosing Radial", x = "Numerosity Difference(radial - tangential)") + 
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    # remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # remove panel background
    panel.background = element_blank(),
    # add axis line
    axis.line = element_line(colour = "grey"),
    # x,y axis tick labels
    axis.text.x = element_text(angle = 0, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

plot_rm2

# ggsave(file = "plot_rm2.svg", plot = plot_rm2, width = 6, height = 5, units = "in")



# =======Correlation: PSE anisotropy across numerosity ranges =========

# df large ranges
aniso_large <- pses_subj %>%
  group_by(participant, reference_num) %>%
  summarise(
    PSE_aniso = mean(PSE_bias_corrected, na.rm = TRUE),
    .groups = "drop"
  )

aniso_large_wide <- aniso_large %>%
  pivot_wider(
    names_from  = reference_num,
    values_from = PSE_aniso,
    names_prefix = "PSE_ref_"
  )

# df RM ranges
aniso_rm <- subj_params_rm %>%
  mutate(participant = as.numeric(participant))  # match type if needed

# df to correlate
aniso_wide <- inner_join(aniso_large_wide, 
                         aniso_rm %>% select(participant, PSE_rm = PSE), 
                         by = "participant")

aniso_wide <- aniso_wide[complete.cases(aniso_wide), ]


# correlation matrix
dat <- aniso_wide %>% select(-participant)
vars <- names(dat)

r_mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
p_mat <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))


for (i in seq_along(vars)) {
  for (j in seq_along(vars)) {
    ok <- complete.cases(dat[[i]], dat[[j]])
    if (sum(ok) >= 3) {
      ct <- cor.test(dat[[i]][ok], dat[[j]][ok], method = "pearson")
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
  ) %>%
  mutate(
    sig_star = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    sig_star = if_else(var1 == var2, "", sig_star)
  )

# order of var names
var_order <- c(
  "PSE_rm",
  "PSE_ref_9",
  "PSE_ref_13",
  "PSE_ref_18",
  "PSE_ref_24",
  "PSE_ref_30"
)

corr_df <- corr_df %>%
  mutate(
    var1 = factor(var1, levels = var_order),
    var2 = factor(var2, levels = var_order)
  )

# plot
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
    size = 3, color = "black"
  ) +
  geom_text(
    data = subset(corr_df, as.integer(var1) >= as.integer(var2)),
    aes(label = sig_star),
    size = 5, vjust = -0.2, color = "black"
  ) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

