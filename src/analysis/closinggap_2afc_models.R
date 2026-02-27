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
    color  = "#800074",
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
    color  = "#800074",
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
  
  scale_y_continuous(limits = c(-10, 11)) +
  
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

# ggsave(file = "my_plot_pse.svg", plot = my_plot_pse, width = 7, height = 5, units = "in")


# plot wf

my_plot_wf <- ggplot()+
  
  geom_point(
    data = pses_subj,
    aes(
      x = factor(reference_num),
      y = WF,
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
      ymax = WF_mean + WF_CI
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    color = "black",
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "Weber fraction", x = "Reference Numerosity") +
  
  
  scale_y_continuous(limits = c(0, 0.6)) +
  
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

# ggsave(file = "my_plot_wf.svg", plot = my_plot_wf, width = 7, height = 5, units = "in")

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
    alpha = 0.8,
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
  ) +
  
  
  geom_errorbar(data = data_across_subject_rm2, 
                aes(x = delta,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - ci,
                    ymax = mean_prop_probe + ci,
                    group = probe_type
                    ),
                size  = 0.8,
                width = .00,
                alpha = 1,
                color = "black",
                position = position_dodge(width = 0.2)
  ) +
  
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

# ggsave(file = "plot_rm2.svg", plot = plot_rm2, width = 7, height = 5, units = "in")


# GLMM - chose_probe_binary
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
    alpha = 0.8
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
      group = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 1,
    color = "black",
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "PSE bias", x = "Probe Arrangement") +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  
  scale_color_manual(
    labels = c("radial", "tangential"),
    values = c("#800074", "#707070"),
    name   = "Probe Arrangement"
  ) +
  
  
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

# ggsave(file = "my_plot_rm_pse.svg", plot = my_plot_rm_pse, width = 5, height = 5, units = "in")


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
      group = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha =1,
    color = "black",
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "JND - precision", x = "Probe Arrangement") +
  
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

# ggsave(file = "my_plot_rm_jnd.svg", plot = my_plot_rm_jnd, width = 5, height = 5, units = "in")



# GLMM -Choose radial binary

data_rm_range2 <- data %>%
  filter(sector_angle == 0)

to_remove <- c(911343, 309153)

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
    alpha = 0.7,
    position = position_dodge(0.2)
  ) +
  
  geom_errorbar(data = data_across_subject_rm2, 
                aes(x = diff,
                    y = mean_prop_radial,
                    ymin = mean_prop_radial - ci,
                    ymax = mean_prop_radial + ci,
                    group = reference_num),
                
                size  = 0.5,
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

