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

ggsave(file = "plot_ds.svg", plot = my_plot,  width = 11, height = 5, units = "in")


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
