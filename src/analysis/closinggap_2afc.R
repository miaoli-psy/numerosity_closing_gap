library(dplyr)
library(ggplot2)
library(ggh4x)
library(svglite)
library(tidyverse)
library(quickpsy)
library(lme4)


setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")


data <- readr::read_csv("D:/OneDrive/projects/numerosity_closing_gap/data/closinggap_2afc/data_closinggap_2afc.csv")

data$chose_probe_binary <- as.numeric(data$choice_display_more == "chose_probe")

data_numerosity_range <- data %>%
  filter(sector_angle != 0)

data_rm_range <- data %>%
  filter(sector_angle == 0)


# =======numerosity range blocks===================
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
pses <- fit_psychometric$par %>% 
  filter(parn == "p1") %>%
  rename(PSE = par)

pses <- pses %>%
  mutate(PSE_bias = PSE - reference_num)


slopes <- fit_psychometric$par %>%
  filter(parn == "p2") %>%
  mutate(slope_at_pse = 1 / (4 * par))

print(pses)
print(slopes)


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


# =======RM range blocks===================
# arrange data

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


# # fit psychometric # not ideal, 2-3 probe numerosity level
# fit_psychometric_rm <- quickpsy(
#   data_rm_range,
#   x = probe_num,
#   k = chose_probe_binary,
#   grouping = .(probe_type, reference_num, spacing_in_deg),
#   fun = logistic_fun,
#   B = 1000
# )

# save fitting data
# saveRDS(fit_psychometric_rm, file = "fit_psychometric_rm.rds")

# overview fit
# plot(fit_psychometric)

# read fitting data
fit_psychometric_rm <- readRDS("fit_psychometric_rm.rds")


# get fitted data to plot
fitted_data_rm <-fit_psychometric_rm$curves

# get PSEs and slopes
pses <- fit_psychometric_rm$par %>% 
  filter(parn == "p1") %>%
  rename(PSE = par)

slopes <- fit_psychometric_rm$par %>%
  filter(parn == "p2") %>%
  mutate(slope_at_pse = 1 / (4 * par))

print(pses)
print(slopes)

# get unique probe numerosity for each subplot
x_axis_breaks <- data_across_subject_rm %>%
  group_by(sector_angle) %>%
  summarise(breaks = list(sort(unique(probe_num))))

x_scales <- lapply(
  setNames(x_axis_breaks$breaks, x_axis_breaks$sector_angle),
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
                     name = "Reference Arrangement") +
  
  
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
    x = x_scales
  )

plot_rm


# mixed-effect logistic model

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
