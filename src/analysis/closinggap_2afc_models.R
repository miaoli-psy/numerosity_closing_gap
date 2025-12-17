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
    PSE_bias = PSE - reference_num,
    JND      = log(3) / abs(beta),
    WF       = JND / PSE # use jnd/pse instead of jnd/ref_num
  )

pses_subj <- pses_subj %>%
  mutate(slope_at_pse = beta / 4)

# remove participants that unable to do the task
to_remove <- c(911343, 309153)

pses_subj <- pses_subj %>% 
  filter(!participant %in% to_remove)


# remove extreme values: for each ref_num, a pse value was considered an extreme value
# and excluded if it deviated from the median by more than 3 MAD
# MAD: defened as the median of the absolute deviation from that median (better than 3std, 
# more stable when distribution are non-gaussian)

pses_subj <- pses_subj %>%
  group_by(reference_num) %>%
  mutate(
    pse_med = median(PSE, na.rm = TRUE),
    pse_mad = mad(PSE, na.rm = TRUE),
    keep    = abs(PSE - pse_med) <= 3 * pse_mad
  ) %>%
  filter(keep) %>%
  ungroup() %>%
  select(-pse_med, -pse_mad, -keep)

# pses_subj <- pses_subj %>%
#   group_by(reference_num) %>%
#   mutate(
#     pse_mean = mean(PSE, na.rm = TRUE),
#     pse_sd   = sd(PSE, na.rm = TRUE),
#     keep     = abs(PSE - pse_mean) <= 3 * pse_sd
#   ) %>%
#   filter(keep) %>%
#   ungroup() %>%
#   select(-pse_mean, -pse_sd, -keep)

average_pses_df <- pses_subj %>% 
  group_by(probe_type, reference_num) %>% 
  summarise(
    PSE_bias_mean = mean(PSE_bias),
    PSE_bias_std = sd(PSE_bias),
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
      y = PSE_bias,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.1
  ) +
  
  geom_point(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.8
  ) +
  
  geom_errorbar(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      ymin = PSE_bias_mean - PSE_CI,
      ymax = PSE_bias_mean + PSE_CI,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "PSE bias", x = "Reference Numerosity") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  scale_y_continuous(limits = c(-10, 10)) +
  
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

# plot wf

my_plot_wf <- ggplot()+
  
  geom_point(
    data = pses_subj,
    aes(
      x = factor(reference_num),
      y = WF,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.1
  ) +
  geom_point(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = WF_mean,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 1
  ) +
  
  geom_errorbar(
    data = average_pses_df,
    aes(
      x = factor(reference_num),
      y = WF_mean,
      ymin = WF_mean - WF_CI,
      ymax = WF_mean + WF_CI,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "Weber fraction", x = "Reference Numerosity") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  scale_y_continuous(limits = c(0, 2)) +
  
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

my_plot_wf


# LMM on PSEs

# referece num back to numerical
pses_subj <- pses_subj %>% 
  mutate(
    reference_num = scale(reference_num, scale = FALSE)
  )

# probe_type to factor
pses_subj$probe_type <- factor(
  pses_subj$probe_type,
  levels = c( "radial", "tangential")
)

# contrast codind radial -0.5; tangential 0.5
contrasts(pses_subj$probe_type) <- matrix(
  c(-0.5, 0.5),
  ncol = 1)

model_psebias <- lmer(
  PSE_bias ~ probe_type * reference_num + (1 | participant),
  data = pses_subj
)

summary(model_psebias)

sjPlot::tab_model(
  model_psebias,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

emm <- emmeans::emmeans(
  model_psebias,
  ~ probe_type | reference_num,
  at = list(reference_num = sort(unique(pses_subj$reference_num)))
)

contrast_ref <- emmeans::contrast(
  emm,
  method = "pairwise",
  adjust = "holm"  
)

contrast_ref

# LMM on WFs

model_wf <- lmer(
  WF ~ probe_type * reference_num + (1 | participant),
  data = pses_subj
)

summary(model_wf)

sjPlot::tab_model(
  model_wf,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

emm <- emmeans::emmeans(
  model_wf,
  ~ probe_type | reference_num,
  at = list(reference_num = sort(unique(pses_subj$reference_num)))
)

contrast_ref <- emmeans::contrast(
  emm,
  method = "pairwise",
  adjust = "holm"  
)

contrast_ref



# ========RM ranges======

# read data RM range
data <- readr::read_csv("D:/OneDrive/projects/numerosity_closing_gap/data/closinggap_2afc/data_closinggap_2afc.csv")

data_rm_range <- data %>%
  filter(sector_angle == 0)

to_remove <- c(911343, 309153)

data_rm_range <- data_rm_range %>% 
  filter(!participant %in% to_remove)

# get numerosity difference
data_rm_range <- data_rm_range %>% 
  mutate(
    delta = probe_num - reference_num
  )

# plot RM
data_by_subject_rm2 <- data_rm_range %>% 
  group_by(probe_type, delta, participant) %>% 
  summarise(
    n = n(),
    prop_choose_probe = mean(choice_display_more == "chose_probe"),
    .groups = 'drop'
  )


data_across_subject_rm2 <- data_by_subject_rm2 %>% 
  group_by(probe_type, delta) %>% 
  summarise(
    mean_prop_probe = mean(prop_choose_probe),
    sd_prop_probe = sd(prop_choose_probe),
    n = n(),
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_prop_probe / sqrt(n),
    ci = sem * qt(0.975, df = n - 1)
  )

plot_rm2 <- ggplot() +
  
  geom_point(
    data = data_by_subject_rm2,
    aes(
      x = delta,
      y = prop_choose_probe,
      color = probe_type,
      group = probe_type
    ),
    size = 4,
    alpha = 0.1,
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
  ) +
  
  geom_point(
    data = data_across_subject_rm2,
    aes(
      x = delta,
      y = mean_prop_probe,
      color = probe_type,
      group = probe_type
    ),
    size = 4,
    alpha = 1,
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
  ) +
  
  
  geom_errorbar(data = data_across_subject_rm2, 
                aes(x = delta,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - ci,
                    ymax = mean_prop_probe + ci,
                    group = probe_type,
                    color = probe_type),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0.2)) +
  
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, by = 0.5)) +
  
  scale_x_continuous(limits = c(-1.1, 1.1), breaks = c(-1, 0, 1)) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  
  labs(y = "Proportion Choosing Probe", x = "Numerosity Difference (Probe - Reference)") + 
  
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
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    # legend size
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    # facet wrap title
    strip.text.x = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  ) 

plot_rm2

# GLMM
data_rm_range$chose_probe_binary <- as.numeric(data_rm_range$choice_display_more == "chose_probe")

data_rm_range$probe_type <- factor(data_rm_range$probe_type, levels = c("radial", "tangential"))
contrasts(data_rm_range$probe_type) <- matrix(c(-0.5, 0.5), ncol = 1)

glmm_rm <- glmer(
  chose_probe_binary ~ probe_type * delta + (1 + delta| participant),
  family = binomial,
  data = data_rm_range
)

summary(glmm_rm)

sjPlot::tab_model(
  glmm_rm,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

emm_delta <- emmeans::emmeans(
  glmm_rm,
  ~ probe_type | delta,
  at = list(delta = c(-1, 0, 1)),
  type = "response"   # returns probabilities, not log-odds
)

emm_delta

pairwise_contrasts <- emmeans::contrast(
  emm_delta,
  method = "pairwise",
  adjust = "holm"   # or "none" / "fdr"
)

pairwise_contrasts

#  extract fixed + random effects 
b_fix <- fixef(glmm_rm)
re    <- ranef(glmm_rm)$participant  #random intercept and random slope for delta


re_df <- re %>%
  as.data.frame() %>%
  tibble::rownames_to_column("participant")

# compute participant-specific params for a given condition
compute_params <- function(c_value, cond_label) {
  
  beta0  <- b_fix["(Intercept)"]
  betap  <- b_fix["probe_type1"]
  betad  <- b_fix["delta"]
  betapd <- b_fix["probe_type1:delta"]
  
  re_df %>%
    transmute(
      participant,
      probe_type = cond_label,
      c = c_value,
      
      # participant-specific intercept and delta slope (fixed + random)
      intercept = (beta0 + `(Intercept)`) + betap * c,
      slope = (betad + delta)         + betapd * c,
      
      # PSE-like delta50%
      delta50 = -intercept / slope,
      
      # JND-like (75–25 definition for logistic)
      JND = log(3) / abs(slope)
    )
}

#  radial = -0.5, tangential = +0.5 
subj_rad <- compute_params(-0.5, "radial")
subj_tan <- compute_params( 0.5, "tangential")

subj_params_rm <- bind_rows(subj_rad, subj_tan)

params_rm <- subj_params_rm %>% 
  group_by(probe_type) %>% 
  summarise(
    mean_delta50 = mean(delta50),
    sd_delta50 = sd(delta50),
    n = n(),
    mean_jnd = mean(JND),
    sd_jnd = sd(JND)
  ) %>% 
  mutate(
    delta50_sem = sd_delta50/sqrt(n),
    delta50_CI = delta50_sem * qt((1 - 0.05) / 2 + .5, n - 1),
    jnd_sem = sd_jnd/sqrt(n),
    jnd_CI = jnd_sem * qt((1 - 0.05) / 2 + .5, n - 1)
  )

my_plot_rm_pse <- ggplot()+
  geom_point(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_delta50,
      group = probe_type,
      color = probe_type,
      size = 4
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 1
  ) +
  
  geom_point(
    data = subj_params_rm,
    aes(
      x = probe_type,
      y = delta50,
      group = probe_type,
      color = probe_type,
      size = 4
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.1
  ) +
  
  geom_errorbar(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_delta50,
      ymin = mean_delta50 - delta50_CI,
      ymax = mean_delta50 + delta50_CI,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "PSE", x = "Probe Arrangement") +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  scale_y_continuous(limits = c(-10, 10)) +
  
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


my_plot_rm_pse

my_plot_rm_jnd <- ggplot()+
  geom_point(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_jnd,
      group = probe_type,
      color = probe_type,
      size = 4
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 1
  ) +
  
  geom_point(
    data = subj_params_rm,
    aes(
      x = probe_type,
      y = JND,
      group = probe_type,
      color = probe_type,
      size = 4
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.1
  ) +
  
  geom_errorbar(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_jnd,
      ymin = mean_jnd - jnd_CI,
      ymax = mean_jnd + jnd_CI,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha =1,
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "JND", x = "Probe Arrangement") +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  scale_y_continuous(limits = c(-5, 5)) +
  
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

my_plot_rm_jnd

