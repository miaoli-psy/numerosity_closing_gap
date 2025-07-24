library(dplyr)
library(ggplot2)
library(svglite)

setwd("D:/OneDrive/projects/numerosity_closing_gap/src/analysis/")
data <- readr::read_csv(file.choose()) #D:\OneDrive\projects\numerosity_closing_gap\data\enumeration\data_RMenumeration.csv


data_by_subject <- data %>% 
  group_by(numerosity, type, participant, protectzonetype) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    .groups = 'drop'
  )


data_across_subject <- data_by_subject %>% 
  group_by(numerosity, type, protectzonetype) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_deviaiton/sqrt(n),
    ci = sem * qt(0.975, df = n - 1),
    sem_rt = sd_rt/sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1)
  )


my_plot <- ggplot() +
  
  geom_point(data = data_across_subject, 
             aes(x = numerosity,
                 y = mean_deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject, 
             aes(x = numerosity,
                 y = deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_deviation - sem,
                    ymax = mean_deviation + sem,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"))


my_plot

# ggsave(file = "plot_ds.svg", plot = my_plot,  width = 11, height = 5, units = "in")


my_plot_rt <- ggplot() +
  
  geom_point(data = data_across_subject, 
             aes(x = numerosity,
                 y = mean_rt,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject, 
             aes(x = numerosity,
                 y = rt,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_rt - sem_rt,
                    ymax = mean_rt + sem_rt,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "RT", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"))


my_plot_rt


# ---- separate for different visual field------------

data_by_subject2 <- data %>% 
  group_by(numerosity, type, participant, protectzonetype, rotation_index) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    .groups = 'drop'
  )


data_across_subject2 <- data_by_subject2 %>% 
  group_by(numerosity, type, protectzonetype, rotation_index) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_deviaiton/sqrt(n),
    ci = sem * qt(0.975, df = n - 1),
    sem_rt = sd_rt/sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1)
  )


my_plot2 <- ggplot() +
  
  geom_point(data = data_across_subject2, 
             aes(x = numerosity,
                 y = mean_deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject2, 
             aes(x = numerosity,
                 y = deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject2, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_deviation - sem,
                    ymax = mean_deviation + sem,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap(~rotation_index)


my_plot2

# ggsave(file = "plot_ds_diff_vf.svg", plot = my_plot2, width = 18, height = 14, units = "in")


# -----upper vs. lower visual field---------------

data <- data %>%
  mutate(up_low = case_when(
    rotation_index > 1 & rotation_index < 13      ~ "upper visual field (2-12)",
    rotation_index> 13 & rotation_index < 25     ~ "lower visual field (14-24)",
    rotation_index == 1 | rotation_index == 13    ~ "horizontal meridian (1 & 13)",
    TRUE~ NA_character_ # Optional: for values outside all conditions
  ))



data_by_subject3 <- data %>% 
  group_by(numerosity, type, participant, protectzonetype, up_low) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    .groups = 'drop'
  )


data_across_subject3 <- data_by_subject3 %>% 
  group_by(numerosity, type, protectzonetype, up_low) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_deviaiton/sqrt(n),
    ci = sem * qt(0.975, df = n - 1),
    sem_rt = sd_rt/sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1)
  )


my_plot3 <- ggplot() +
  
  geom_point(data = data_across_subject3, 
             aes(x = numerosity,
                 y = mean_deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject3, 
             aes(x = numerosity,
                 y = deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject3, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_deviation - sem,
                    ymax = mean_deviation + sem,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap(~up_low)


my_plot3

# ggsave(file = "plot_ds_uplow.svg", plot = my_plot3, width = 12, height = 5, units = "in")


# -----left vs. right visual field---------------

data <- data %>%
  mutate(left_right = case_when(
    rotation_index > 7 & rotation_index < 19      ~ "left visual field (8-18)",
    rotation_index> 0 & rotation_index < 7     ~ "right visual field (1-6,20-24)",
    rotation_index> 19 & rotation_index < 25     ~ "right visual field (1-6,20-24)",
    rotation_index == 7 | rotation_index == 19    ~ "vertical meridian (7 & 19)",
    TRUE~ NA_character_ # Optional: for values outside all conditions
  ))



data_by_subject4 <- data %>% 
  group_by(numerosity, type, participant, protectzonetype, left_right) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    .groups = 'drop'
  )


data_across_subject4 <- data_by_subject4 %>% 
  group_by(numerosity, type, protectzonetype, left_right) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_deviaiton/sqrt(n),
    ci = sem * qt(0.975, df = n - 1),
    sem_rt = sd_rt/sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1)
  )


my_plot4 <- ggplot() +
  
  geom_point(data = data_across_subject4, 
             aes(x = numerosity,
                 y = mean_deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject4, 
             aes(x = numerosity,
                 y = deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject4, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_deviation - sem,
                    ymax = mean_deviation + sem,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap(~left_right)


my_plot4

# ggsave(file = "plot_ds_leftright.svg", plot = my_plot4, width = 12, height = 5, units = "in")


# -----8 locations---------------

data <- data %>%
  mutate(visual_field = case_when(
    rotation_index %in% c(1, 2, 24)      ~ "right",
    rotation_index %in% c(3, 4, 5)       ~ "upper_right",
    rotation_index %in% c(6, 7, 8)       ~ "upper",
    rotation_index %in% c(9, 10, 11)     ~ "upper_left",
    rotation_index %in% c(12, 13, 14)    ~ "left",
    rotation_index %in% c(15, 16, 17)    ~ "lower_left",
    rotation_index %in% c(18, 19, 20)    ~ "lower",
    rotation_index %in% c(21, 22, 23)    ~ "lower_right",
    TRUE                                 ~ NA_character_
  ))


data_by_subject5 <- data %>% 
  group_by(numerosity, type, participant, protectzonetype, visual_field) %>% 
  summarise(
    n = n(),
    deviation = mean(deviation, na.rm = TRUE),
    rt = mean(key_resp.rt),
    .groups = 'drop'
  )


data_across_subject5 <- data_by_subject5 %>% 
  group_by(numerosity, type, protectzonetype, visual_field) %>% 
  summarise(
    n = n(),
    mean_deviation = mean(deviation, na.rm = TRUE),
    sd_deviaiton = sd(deviation, na.rm = TRUE),
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>% 
  mutate(
    sem = sd_deviaiton/sqrt(n),
    ci = sem * qt(0.975, df = n - 1),
    sem_rt = sd_rt/sqrt(n),
    ci_rt = sem_rt * qt(0.975, df = n - 1)
  )


my_plot5 <- ggplot() +
  
  geom_point(data = data_across_subject5, 
             aes(x = numerosity,
                 y = mean_deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.8,
             size = 5) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_point(data = data_by_subject5, 
             aes(x = numerosity,
                 y = deviation,
                 color = type,
                 shape = protectzonetype),
             position = position_dodge(0.5), 
             stat = "identity", 
             alpha = 0.2,
             size = 3) +
  
  geom_errorbar(data = data_across_subject5, 
                aes(x = numerosity,
                    y = mean_deviation,
                    ymin = mean_deviation - sem,
                    ymax = mean_deviation + sem,
                    group = interaction(protectzonetype, type)),  # Fixed column name
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5),
                alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap(~visual_field)


my_plot5

ggsave(file = "plot_ds_visual_field.svg", plot = my_plot5, width = 14, height =10, units = "in")







