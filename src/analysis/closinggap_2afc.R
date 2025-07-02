library(dplyr)
library(ggplot2)
library(ggh4x)
library(svglite)
library(tidyverse)
library(quickpsy)

setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")

data <- readr::read_csv(file.choose()) #D:\OneDrive\projects\numerosity_closing_gap\data\closinggap_2afc\data_clossinggap_2afc.csv
data$chose_probe_binary <- as.numeric(data$choice_display_more == "chose_probe")

data_numerosity_range <- data %>%
  filter(sector_angle != 0)

data_rm_range <- data %>%
  filter(sector_angle == 0)


# =======numerosity range blocks===================
# arrange data

data_by_subject <- data_numerosity_range %>% 
  group_by(sector_angle, reference_type, probe_num, reference_num, participant) %>% 
  summarise(
    n = n(),
    prop_choose_probe = mean(choice_display_more == "chose_probe"),
    .groups = 'drop'
  )


data_across_subject <- data_by_subject %>% 
  group_by(sector_angle, reference_type, probe_num, reference_num) %>% 
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


# fit psychometric
fit_psychometric <- quickpsy(
  data_numerosity_range,
  x = probe_num,
  k = chose_probe_binary,
  grouping = .(reference_type, reference_num),
  fun = logistic_fun
)

# overview fit
plot(fit_psychometric)

# get fitted data to plot
fitted_data <-fit_psychometric$curves

# get PSEs and slopes
pses <- fit_psychometric$par %>% 
  filter(parn == "p1") %>%
  rename(PSE = par)

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
      color = reference_type,
      group = reference_type
    ),
    size = 4,
    alpha = 0.5
  ) +
  
  geom_line(
    data = fitted_data,
    aes(
      x = x,
      y = y,
      color = reference_type,
      group = reference_type,
    ),
  ) +
  
  geom_errorbar(data = data_across_subject, 
                aes(x = probe_num,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - sem,
                    ymax = mean_prop_probe + sem,
                    group = reference_type,
                    color = reference_type),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0)) +
  
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1)) +

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
    ~ reference_num, 
    scales = "free_x", 
    axes = "all", # draw all axes,
    labeller = refnum_labeller
  ) +
  
  # Add the custom scales you created in Step 2
  ggh4x::facetted_pos_scales(
    x = x_scales
  )

plot_psychometric



# =======RM range blocks===================
# arrange data

data_by_subject <- data_rm_range %>% 
  group_by(sector_angle, reference_type, probe_num, reference_num, spacing_in_deg, participant) %>% 
  summarise(
    n = n(),
    prop_choose_probe = mean(choice_display_more == "chose_probe"),
    .groups = 'drop'
  )


data_across_subject <- data_by_subject %>% 
  group_by(sector_angle, reference_type, probe_num, reference_num, spacing_in_deg) %>% 
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


# fit psychometric
fit_psychometric <- quickpsy(
  data_rm_range,
  x = probe_num,
  k = chose_probe_binary,
  grouping = .(reference_type, reference_num, spacing_in_deg),
  fun = logistic_fun
)

# overview fit
plot(fit_psychometric)

# get fitted data to plot
fitted_data <-fit_psychometric$curves

# get PSEs and slopes
pses <- fit_psychometric$par %>% 
  filter(parn == "p1") %>%
  rename(PSE = par)

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

labeller <- labeller(
  reference_num = function(x) paste0("Reference Numerosity: ", x),
  spacing_in_deg = function(x) paste0("Spacing: ", x)
)


plot_psychometric <- ggplot() +
  geom_point(
    data = data_across_subject,
    aes(
      x = probe_num,
      y = mean_prop_probe,
      color = reference_type,
      group = reference_type
    ),
    size = 4,
    alpha = 0.5
  ) +
  
  geom_line(
    data = fitted_data,
    aes(
      x = x,
      y = y,
      color = reference_type,
      group = reference_type,
    ),
  ) +
  
  geom_errorbar(data = data_across_subject, 
                aes(x = probe_num,
                    y = mean_prop_probe,
                    ymin = mean_prop_probe - sem,
                    ymax = mean_prop_probe + sem,
                    group = reference_type,
                    color = reference_type),
                
                size  = 0.5,
                width = .00,
                position = position_dodge(0)) +
  
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.1)) +
  
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

plot_psychometric
