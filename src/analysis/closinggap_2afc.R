library(dplyr)
library(ggplot2)
library(ggh4x)
library(svglite)
library(tidyverse)
library(quickpsy)
library(lme4)

# ==========read data ==============
setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")
data <- readr::read_csv("D:/OneDrive/projects/numerosity_closing_gap/data/closinggap_2afc/data_closinggap_2afc.csv")

to_remove <- c(911343, 309153)

data <- subset(data, participant != to_remove)

# numerosity range and RM range
data$chose_probe_binary <- as.numeric(data$choice_display_more == "chose_probe")

data_numerosity_range <- data %>%
  filter(sector_angle != 0)

data_rm_range <- data %>%
  filter(sector_angle == 0)

# arrange data
data_by_subject <- data_numerosity_range %>% 
  group_by(sector_angle, probe_type, probe_num, reference_num, participant) %>% 
  summarise(
    n = n(),
    prop_choose_probe = mean(choice_display_more == "chose_probe"),
    .groups = 'drop'
  )

data_across_subject <- data_by_subject %>% 
  group_by(sector_angle, probe_type, probe_num, reference_num) %>% 
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

# =======numerosity range ===================

# 01 numerosity range group level fit

# # fit psychometric
# fit_psychometric <- quickpsy(
#   data_numerosity_range,
#   x = probe_num,
#   k = chose_probe_binary,
#   grouping = .(probe_type, reference_num),
#   fun = logistic_fun, # cum_normal_fun
#   B = 1000
# )
# 
# # save fitting data
# saveRDS(fit_psychometric, file = "fit_psychometric.rds")

# overview fit
# plot(fit_psychometric)

# read fitting data
fit_psychometric <- readRDS("fit_psychometric.rds")

# get fitted data to plot
fitted_data <-fit_psychometric$curves

# get PSEs and slopes

# p1 -> PSE the x at which response = 0.5
pses <- fit_psychometric$par %>% 
  filter(parn == "p1") %>%
  rename(PSE = par)

pses <- pses %>%
  mutate(bias = PSE - reference_num)

# i get the slope at the PSEs (where x = p1)
slopes <- fit_psychometric$par %>%
  filter(parn == "p2") %>%
  mutate(slope_at_pse = 1 / (4 * par))

# print(pses)
# print(slopes)


# get unique probe numerosity for each subplot
x_axis_breaks <- data_across_subject %>%
  group_by(sector_angle) %>%
  summarise(breaks = list(sort(unique(probe_num))))

x_scales <- lapply(
  setNames(x_axis_breaks$breaks, x_axis_breaks$sector_angle),
  function(b) scale_x_continuous(breaks = b)
)

refnum_labeller <- labeller(
  reference_num = function(x) paste0("Reference Numerosity: ", x)
)

plot_psychometric <- ggplot() +
  geom_point(
    data = data_across_subject,
    aes(
      x = probe_num,
      y = mean_prop_probe,
      color = probe_type,
      group = probe_type
    ),
    size = 4,
    alpha = 0.5
  ) +
  
  geom_line(
    data = fitted_data,
    aes(
      x = x,
      y = y,
      color = probe_type,
      group = probe_type,
    ),
  ) +
  
  geom_errorbar(data = data_across_subject, 
                aes(x = probe_num,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - ci,
                    ymax = mean_prop_probe + ci,
                    group = probe_type,
                    color = probe_type),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0)) +
  
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1)) +

  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
    
    labs(y = "Proportion Choosing Probe", x = "Probe Numerosity") + 
    
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
    )  + 
  
  ggh4x::facet_wrap2(
    ~ reference_num, 
    scales = "free_x", 
    axes = "all", # draw all axes,
    labeller = refnum_labeller
  ) +
  
  # Add the custom scales
  ggh4x::facetted_pos_scales(
    x = x_scales
  )

plot_psychometric


#  02 numerosity range individual level fit

# fit individual logistic --> takes about 24h to ran
# fit_psychometric_subj <- quickpsy(
#   data_numerosity_range,
#   x = probe_num,
#   k = chose_probe_binary,
#   grouping = .(participant, probe_type, reference_num),
#   fun = logistic_fun,
#   B = 200
# )
# 
# # save fitting data
# saveRDS(fit_psychometric_subj, file = "fit_psychometric_subj.rds")

# read fitting data
fit_psychometric_subj <- readRDS("fit_psychometric_subj.rds")

# get fitted data to plot
fitted_data_subj <-fit_psychometric_subj$curves

# get PSEs and slopes
# pses_subj <- fit_psychometric_subj$par %>% 
#   filter(parn == "p1") %>%
#   rename(PSE = par,
#          parinf_pse = parinf,
#          parsup_pse = parsup) 
# 
# pses_subj <- pses_subj %>%
#   mutate(PSE_bias = PSE - reference_num)
# 
# slopes_subj <- fit_psychometric_subj$par %>%
#   filter(parn == "p2") %>%
#   mutate(slope_at_pse = 1 / (4 * par)) 

# get pse, jnd, wf
pses_subj <- fit_psychometric_subj$par %>%
  select(participant, probe_type, reference_num, parn, par) %>%
  pivot_wider(names_from = parn, values_from = par) %>%
  mutate(
    PSE      = p1,
    beta     = p2,
    PSE_bias = PSE - reference_num,
    JND      = log(3) / abs(beta),
    WF       = JND / reference_num
  )
pses_subj <- pses_subj %>%
  mutate(slope_at_pse = beta / 4)


# plot individual fit
participants <- unique(data$participant)

x_axis_breaks <- data_across_subject %>%
  group_by(sector_angle) %>%
  summarise(breaks = list(sort(unique(probe_num))))

x_scales <- lapply(
  setNames(x_axis_breaks$breaks, x_axis_breaks$sector_angle),
  function(b) scale_x_continuous(breaks = b)
)

refnum_labeller <- labeller(
  reference_num = function(x) paste0("Reference Numerosity: ", x)
)

for (p_id in participants) {
  
  # Filter data for this participant
  data_subj   <- data_by_subject %>% 
    filter(participant == p_id)
  fitted_subj <- fitted_data_subj %>%
    filter(participant == p_id)
  pses_subj_plot <- pses_subj %>%
    filter(participant == p_id)
  
  # x at the middle
  x_pos_df <- data_subj %>%
    group_by(reference_num) %>%
    summarise(
      x_pos = mean(range(probe_num), na.rm = TRUE),
      .groups = "drop"
    )
  
  # PSE label info
  pse_labels <- pses_subj_plot %>%
    left_join(x_pos_df, by = "reference_num") %>%
    mutate(
      label = paste0("PSE = ", round(PSE, 2)),
      
      y_pos = case_when(
        probe_type == "tangential" ~ 0.92,
        probe_type == "radial"      ~ 0.10,
        TRUE                        ~ 0.50
      )
    )
  
  plot_psychometric_subj <- ggplot() +
    geom_point(
      data = data_subj,
      aes(
        x     = probe_num,
        y     = prop_choose_probe,
        color = probe_type,
        group = probe_type
      ),
      size  = 4,
      alpha = 0.5
    ) +
    
    geom_line(
      data = fitted_subj,
      aes(
        x     = x,
        y     = y,
        color = probe_type,
        group = probe_type
      )
    ) +
  
    geom_text(
      data        = pse_labels,
      mapping     = aes(
        x     = x_pos,
        y     = y_pos,
        label = label,
        color = probe_type
      ),
      size        = 3,
      show.legend = FALSE,
      inherit.aes = FALSE
    ) +
    
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1)
    ) +
    
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
    
    scale_color_manual(
      labels = c("radial", "tangential"),
      values = c("#BB5566", "#004488"),
      name   = "Probe Arrangement"
    ) +
    
    labs(
      y = "Proportion Choosing Probe",
      x = "Probe Numerosity",
      title = paste("Participant:", p_id)
    ) +
    
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
      axis.text.x        = element_text(angle = 0, hjust = 1, size = 12, face = "bold"),
      axis.text.y        = element_text(size = 12, face = "bold"),
      legend.title       = element_text(size = 12, face = "bold"),
      legend.text        = element_text(size = 10),
      strip.text.x       = element_text(size = 12, face = "bold"),
      panel.spacing      = unit(1.0, "lines")
    ) +
    
    # 5 subplots: one per reference_num, for this participant only
    ggh4x::facet_wrap2(
      ~ reference_num,
      scales  = "free_x",
      axes    = "all",
      labeller = refnum_labeller
    ) +
    
    # custom x scales per reference
    ggh4x::facetted_pos_scales(
      x = x_scales
    )
  
  print(plot_psychometric_subj)
  
  # # save the figure
  # file_name <- paste0("psychometric_participant_", p_id, ".png")
  # ggsave(
  #   filename = file_name,
  #   plot = plot_psychometric_subj,
  #   width = 10,
  #   height = 6,
  #   dpi = 300
  # )

  # print(paste("Saved:", file_name))
}


# delete extreme values
pses_subj <- subset(pses_subj, participant != to_remove)

mean_bias <- mean(pses_subj$PSE_bias)
sd_bias <- sd(pses_subj$PSE_bias)

pses_subj_filtered <- pses_subj[pses_subj$PSE_bias >= (mean_bias - 3 * sd_bias) &
                                  pses_subj$PSE_bias <= (mean_bias + 3 * sd_bias), ]

# plot pse

average_df <- pses_subj_filtered %>% 
  group_by(probe_type, 
           reference_num) %>% 
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
    data = average_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.6
  ) +
  
  geom_errorbar(
    data = average_df,
    aes(
      x = factor(reference_num),
      y = PSE_bias_mean,
      ymin = PSE_bias_mean - PSE_sem,
      ymax = PSE_bias_mean + PSE_sem,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  
  # geom_point(
  #   data = pses_subj_filtered,
  #   aes(
  #     x = factor(reference_num),
  #     y = PSE_bias,
  #     group = probe_type,
  #     color = probe_type,
  #     size = 1
  #   ),
  #   
  #   position = position_dodge(3.5),
  #   stat = "identity",
  #   alpha = 0.2
  # ) +
  # 
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
    data = average_df,
    aes(
      x = factor(reference_num),
      y = WF_mean,
      group = probe_type,
      color = probe_type,
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.6
  ) +
  
  geom_errorbar(
    data = average_df,
    aes(
      x = factor(reference_num),
      y = WF_mean,
      ymin = WF_mean - WF_sem,
      ymax = WF_mean + WF_sem,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
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

# lmm

# reference_num to numerical again

pses_subj_filtered <- pses_subj_filtered %>%
  mutate(
    reference_num_c = scale(reference_num, scale = FALSE)
  )

pses_subj_filtered$probe_type <- factor(
  pses_subj_filtered$probe_type,
  levels = c( "radial", "tangential")
)

# contrast codind tan, radial to 0.5 and -0.5
contrasts(pses_subj_filtered$probe_type) <- matrix(
  c(-0.5, 0.5),
  ncol = 1)


model_psebias <- lmer(
  PSE_bias ~ probe_type * reference_num + (1 | participant),
  data = pses_subj_filtered
)
summary(model_psebias)

sjPlot::tab_model(
  model_psebias,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 


emm <- emmeans::emmeans(
  model_psebias,
  ~ probe_type | reference_num,
  at = list(reference_num = sort(unique(pses_subj_filtered$reference_num)))
)

contrast_ref <- emmeans::contrast(
  emm,
  method = "pairwise",
  adjust = "holm"  
)

contrast_ref

model_wf<- lmer(
  WF ~ probe_type * reference_num + (1 | participant),
  data = pses_subj_filtered
)

summary(model_wf)

sjPlot::tab_model(
  model_wf,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) 

# =======RM range blocks===================
# arrange data

data_rm_range <- subset(data_rm_range, participant != to_remove)

data_by_subject_rm <- data_rm_range %>% 
  group_by(sector_angle, probe_type, probe_num, reference_num, spacing_in_deg, participant) %>% 
  summarise(
    n = n(),
    prop_choose_probe = mean(choice_display_more == "chose_probe"),
    .groups = 'drop'
  )


data_across_subject_rm <- data_by_subject_rm %>% 
  group_by(sector_angle, probe_type, probe_num, reference_num, spacing_in_deg) %>% 
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


# get unique probe numerosity for each subplot
x_axis_breaks_rm <- data_across_subject_rm %>%
  group_by(sector_angle) %>%
  summarise(breaks = list(sort(unique(probe_num))))

x_scales_rm <- lapply(
  setNames(x_axis_breaks_rm$breaks, x_axis_breaks_rm$sector_angle),
  function(b) scale_x_continuous(breaks = b)
)

labeller <- labeller(
  reference_num = function(x) paste0("Reference Numerosity: ", x),
  spacing_in_deg = function(x) paste0("Spacing: ", x)
)


plot_rm <- ggplot() +
  geom_point(
    data = data_across_subject_rm,
    aes(
      x = probe_num,
      y = mean_prop_probe,
      color = probe_type,
      group = probe_type
    ),
    size = 4,
    alpha = 0.5
  ) +
  
  # geom_line(
  #   data = fitted_data_rm,
  #   aes(
  #     x = x,
  #     y = y,
  #     color = probe_type,
  #     group = probe_type,
  #   ),
  # ) +
  
  geom_errorbar(data = data_across_subject_rm, 
                aes(x = probe_num,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - ci,
                    ymax = mean_prop_probe + ci,
                    group = probe_type,
                    color = probe_type),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0)) +
  
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, by = 0.5)) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("radial", "tangential"),
                     values = c("#BB5566", "#004488"),
                     name = "Probe Arrangement") +
  
  
  labs(y = "Proportion Choosing Probe", x = "Probe Numerosity") + 
  
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
  )  + 
  
  ggh4x::facet_wrap2(
    ~ reference_num + spacing_in_deg, ncol = 2,
    scales = "free_x", 
    axes = "all", # draw all axes,
    labeller = labeller,
  ) +
  
  # Add the custom scales you created in Step 2
  ggh4x::facetted_pos_scales(
    x = x_scales_rm
  )

plot_rm

# plot rm each participant

for (p_id in participants) {
  
  # filter  data 
  data_subj_rm <- data_by_subject_rm %>%
    filter(participant == p_id)
  
  # x-axis breaks 
  x_axis_breaks_rm_subj <- data_subj_rm %>%
    group_by(sector_angle) %>%
    summarise(breaks = list(sort(unique(probe_num))), .groups = "drop")
  
  x_scales_rm_subj <- lapply(
    setNames(x_axis_breaks_rm_subj$breaks,
             x_axis_breaks_rm_subj$sector_angle),
    function(b) scale_x_continuous(breaks = b)
  )
  
  # plot
  plot_rm_subj <- ggplot() +
    
    geom_point(
      data = data_subj_rm,
      aes(
        x = probe_num,
        y = prop_choose_probe,
        color = probe_type,
        group = probe_type
      ),
      size  = 4,
      alpha = 0.5
    ) +
    
    scale_y_continuous(
      limits = c(0, 1.1),
      breaks = seq(0, 1, by = 0.5)
    ) +
    
    geom_hline(
      yintercept = 0.5,
      linetype   = "dashed",
      color      = "black"
    ) +
    
    scale_color_manual(
      labels = c("radial", "tangential"),
      values = c("#BB5566", "#004488"),
      name   = "Probe Arrangement"
    ) +
    
    labs(
      y = "Proportion Choosing Probe",
      x = "Probe Numerosity",
      title = paste("Participant:", p_id)
    ) +
    
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x  = element_text(size = 12, face = "bold"),
      axis.text.y  = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 10),
      strip.text.x = element_text(size = 12, face = "bold"),
      panel.grid   = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "grey"),
      panel.spacing = unit(1.0, "lines")
    ) +
    
    ggh4x::facet_wrap2(
      ~ reference_num + spacing_in_deg,
      ncol   = 2,
      scales = "free_x",
      axes   = "all",
      labeller = labeller
    ) +
    
    ggh4x::facetted_pos_scales(
      x = x_scales_rm_subj
    )
  
  print(plot_rm_subj)
  
  # ---- Optional: save ----
  # ggsave(
  #   filename = paste0("RM_participant_", p_id, ".png"),
  #   plot     = plot_rm_subj,
  #   width    = 8,
  #   height   = 6,
  #   dpi      = 300
  # )
}


# RM range GLMM 
data_rm_range <- data_rm_range %>% 
  mutate(
    delta = probe_num - reference_num
  )

data_rm_range$probe_type <- factor(data_rm_range$probe_type, levels = c("radial", "tangential"))
contrasts(data_rm_range$probe_type) <- matrix(c(-0.5, 0.5), ncol = 1)

glmm_rm <- glmer(
  chose_probe_binary ~ probe_type * delta + (1 + delta| participant),
  family = binomial,
  data = data_rm_range
)

summary(glmm_rm)

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

# plot 
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
    data = data_across_subject_rm2,
    aes(
      x = delta,
      y = mean_prop_probe,
      color = probe_type,
      group = probe_type
    ),
    size = 4,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  
  geom_errorbar(data = data_across_subject_rm2, 
                aes(x = delta,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - sem,
                    ymax = mean_prop_probe + sem,
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
  
  
  labs(y = "Proportion Choosing Probe", x = "Delat (Probe - Reference)") + 
  
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
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.6
  ) +
  
  geom_point(
    data = subj_params_rm,
    aes(
      x = probe_type,
      y = delta50,
      group = probe_type,
      color = probe_type,
      size = 2
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.3
  ) +
  
  geom_errorbar(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_delta50,
      ymin = mean_delta50 - delta50_sem,
      ymax = mean_delta50 + delta50_sem,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
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
      size = 3
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.6
  ) +
  
  geom_point(
    data = subj_params_rm,
    aes(
      x = probe_type,
      y = JND,
      group = probe_type,
      color = probe_type,
      size = 2
    ),
    stat = "identity",
    position = position_dodge2(width = 0.2, preserve = "single", padding = 0.1),
    alpha = 0.3
  ) +
  
  geom_errorbar(
    data = params_rm,
    aes(
      x = probe_type,
      y = mean_jnd,
      ymin = mean_jnd - jnd_sem,
      ymax = mean_jnd + jnd_sem,
      group = probe_type,
      color = probe_type
    ),
    size  = 0.8,
    width = .00,
    alpha = 0.8,
    position = position_dodge(width = 0.2)
  ) +
  
  labs(y = "JND", x = "Probe Arrangement") +
  
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

my_plot_rm_jnd

# 03 RM range mixed-effect logistic model

# arrange data
data_rm_range <- data_rm_range %>%
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

data_rm_range <- data_rm_range %>%
  mutate(
    radial_num = if_else(reference_type == "radial", reference_num, probe_num),
    tangential_num = if_else(reference_type == "tangential", reference_num, probe_num)
  )

data_rm_range <- data_rm_range %>%
  mutate(
    diff = radial_num - tangential_num
    )

data_rm_range$reference_num <- as.factor(data_rm_range$reference_num) 


model_rm <- glmer(
  chose_radial ~ diff * reference_num + (1 | participant),
  data = data_rm_range,
  family = binomial
)

summary(model_rm)

# log-odds to probability:
probability = 1 / (1 + exp(1.77443)) # probability = 0.145

# prob: when radial and tangential contain the same number of item, participants
# choose the radial display only 14.5% of the time --> when numerosity is physically 
# equal, participants choose radial as less numerous


# arrange data plot
data_by_subject_rm <- data_rm_range %>% 
  group_by(reference_num, diff, participant) %>% 
  summarise(
    n = n(),
    prop_choose_radial = mean(chose_radial),
    .groups = 'drop'
  )


data_across_subject_rm <- data_by_subject_rm %>% 
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
    data = data_across_subject_rm,
    aes(
      x = diff,
      y = mean_prop_radial,
      color = reference_num,
      group = reference_num
    ),
    size = 4,
    alpha = 0.5,
    position = position_dodge(0.2)
  ) +
  
  
  geom_errorbar(data = data_across_subject_rm, 
                aes(x = diff,
                    y = mean_prop_radial,
                    ymin = mean_prop_radial - ci,
                    ymax = mean_prop_radial + ci,
                    group = reference_num,
                    color = reference_num),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0.2)) +
  
  scale_y_continuous(limits = c(0, 1.1),
                     breaks = seq(0, 1, by = 0.5)) +
  
  scale_x_continuous(limits = c(-1.2, 1.2),
                     breaks = seq(-1, 1, by = 1)) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  
  scale_color_manual(labels = c("3", "4", "5"),
                     values = c("#5e4c5f", "#999999", "#ffbb6f"),
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
