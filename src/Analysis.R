library(tidyverse)
library(ggthemes)
library(ggpubr)
library(zoo)
library(patchwork)
library(ggdark)
library(scales)



# Define Functions
get_min <- function(Time) { # create a function with the name my_function
  hour <- format(strptime(Time, format="%H:%M:%S"), "%H") 
  min <- format(strptime(Time, format="%H:%M:%S"), "%M")
  sec <- format(strptime(Time, format="%H:%M:%S"), "%S") 
  min <- as.numeric(min) + as.numeric(sec) * (1/60) + as.numeric(hour) * 60
  return(min)
}

# Import Data
data <- read_csv(here::here("data/data.csv")) |>
  mutate(Participant = factor(Participant)) |>
  mutate("Min" = get_min(Time),
         Min = round(Min, 2)) |>
  select(-Time) |>
  arrange(Group, Participant, Min) |>
  filter(!is.na(Swimming)) |>
  pivot_longer(cols = c(Swimming, Flooding, Biodiversity, `Water Quality`),
               names_to = "ES") |>
  filter(value != 0)



# Easy Plot

i <- 0
groups <- unique(data$Group)
for(i in i:3){
  i <- i + 1
  group_num <- groups[i]
  data |>
    filter(Group == group_num, value != 0) |>
    mutate(Value = value - 1) |>
    ggplot(aes(x = Min, 
               y = Value)) +
    geom_step(size = 1,
              position = "identity",
              aes(color = Participant)) +
    scale_color_colorblind() +
    labs(x = "Minutes", y = "Importance") + 
    scale_x_continuous(breaks = seq(0, 100, 10),
                       minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(~ES, nrow = 4) +
    dark_theme_minimal() 
  
  ggsave(paste0(here::here("out/Group_"), group_num, "_og.png"), width = 400, height = 200, units = "mm", bg = "black")
  ggsave(paste0(here::here("out/Group_"), group_num, "_og.pdf"), width = 400, height = 200, units = "mm", bg = "black")
}


# Trend Plot

i <- 0
groups <- unique(data$Group)
for(i in i:3){
  i <- i + 1
  group_num <- groups[i]
  group <- data |>
    filter(Group == group_num, value != 0) 
  
  interpolated <- group |>
    complete(Min = seq(min(Min), max(Min), by = 0.01), 
             Group = group_num,
             Participant = factor(1:6),
             ES = ES) |>
    left_join(group, by = c("ES", "Min", "Group", "Participant")) |>
    select(ES, Min, Group, Participant, value.x) |>
    rename("Value" = value.x) |>
    arrange(Group, Participant, ES, Min) |>
    group_by(Group, Participant, ES) |>
    fill(Value, .direction = "down") |>
    mutate(Value = round(Value)) |>
    ungroup()
  
  joinedT <- interpolated |>
    group_by(Group, ES, Min) |>
    mutate("Value" = Value - 1) |>
    summarise("ES" = ES,
              "Group" = Group,
              "Min" = Min,
              "Max" = max(Value),
              "Minimum" = min(Value),
              "Mean" = mean(Value),
              "sd" = sd(Value)
    ) |>
    left_join(interpolated, by = c("Group", "ES", "Min"))  
  
  ggplot(joinedT, aes(x = Min, 
                      y = Value)) +
    geom_ribbon(aes(ymin = Minimum,
                    ymax = Max), alpha = 1) +
    scale_color_colorblind() +
    labs(x = "Minutes", y = "Importance") + 
    scale_x_continuous(breaks = seq(0, 100, 10),
                       minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(~ES, nrow = 4) +
    dark_theme_minimal() 
  
  ggsave(paste0(here::here("out/Group_"), group_num, "_boxes.png"), width = 400, height = 200, units = "mm", bg = "black")
  ggsave(paste0(here::here("out/Group_"), group_num, "_boxes.pdf"), width = 400, height = 200, units = "mm", bg = "black")



## Rolling Data Prep

time_interval <- 0.5
window <- 10
k <- round(window/time_interval)

#####################  ##########################################  #####################
bio <- data |>
  filter(Group == group_num, value != 0, ES == "Biodiversity") |>
  mutate(Min = round(Min * 2) / 2)  |>
  mutate(value = round(value)) 
max <- max(bio$Min)
minutes <- seq(0, max, 0.5)
mins <- crossing(Min = minutes, 
                 Participant = seq(1, 6, 1),
                 Group = "A",
                 ES = "Biodiversity")
bio <- bio |>
  mutate(Participant = as.numeric(Participant)) |>
  full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
  arrange(ES, Participant, Min) |>
  fill(value, .direction = "down") |>
  group_by(Group, ES, Min) |>
  reframe(Mean = mean(value),
          se = sd(value)/sqrt(6),
          min = min(value),
          max = max(value),
          range = max - min) |>
  mutate(rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
         rollSE = rollmean(se, k = k, fill = NA, align = "center"),
         rollRange = rollmean(range, k = k, fill = NA, align = "center"),
         rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))) 

#####################  ##########################################  #####################
flood <- data |>
  filter(Group == group_num, value != 0, ES == "Flooding") |>
  mutate(Min = round(Min * 2) / 2)  |>
  mutate(value = round(value)) 
max <- max(flood$Min)
minutes <- seq(0, max, 0.5)
mins <- crossing(Min = minutes, 
                 Participant = seq(1, 6, 1),
                 Group = "A",
                 ES = "Flooding")
flood <- flood |>
  mutate(Participant = as.numeric(Participant)) |>
  full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
  arrange(ES, Participant, Min) |>
  fill(value, .direction = "down") |>
  group_by(Group, ES, Min) |>
  reframe(Mean = mean(value),
          se = sd(value)/sqrt(6),
          min = min(value),
          max = max(value),
          range = max - min) |>
  mutate(rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
         rollSE = rollmean(se, k = k, fill = NA, align = "center"),
         rollRange = rollmean(range, k = k, fill = NA, align = "center"),
         rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))) 

#####################  ##########################################  #####################
qual <- data |>
  filter(Group == group_num, value != 0, ES == "Water Quality") |>
  mutate(Min = round(Min * 2) / 2)  |>
  mutate(value = round(value)) 
max <- max(qual$Min)
minutes <- seq(0, max, 0.5)
mins <- crossing(Min = minutes, 
                 Participant = seq(1, 6, 1),
                 Group = "A",
                 ES = "Water Quality")
qual <- qual |>
  mutate(Participant = as.numeric(Participant)) |>
  full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
  arrange(ES, Participant, Min) |>
  fill(value, .direction = "down") |>
  group_by(Group, ES, Min) |>
  reframe(Mean = mean(value),
          se = sd(value)/sqrt(6),
          min = min(value),
          max = max(value),
          range = max - min) |>
  mutate(rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
         rollSE = rollmean(se, k = k, fill = NA, align = "center"),
         rollRange = rollmean(range, k = k, fill = NA, align = "center"),
         rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))) 

#####################  ##########################################  #####################
swim <- data |>
  filter(Group == group_num, value != 0, ES == "Swimming") |>
  mutate(Min = round(Min * 2) / 2)  |>
  mutate(value = round(value)) 
max <- max(swim$Min)
minutes <- seq(0, max, 0.5)
mins <- crossing(Min = minutes, 
                 Participant = seq(1, 6, 1),
                 Group = "A",
                 ES = "Swimming")
swim <- swim |>
  mutate(Participant = as.numeric(Participant)) |>
  full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
  arrange(ES, Participant, Min) |>
  fill(value, .direction = "down") |>
  group_by(Group, ES, Min) |>
  reframe(Mean = mean(value),
          se = sd(value)/sqrt(6),
          min = min(value),
          max = max(value),
          range = max - min) |>
  mutate(rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
         rollSE = rollmean(se, k = k, fill = NA, align = "center"),
         rollRange = rollmean(range, k = k, fill = NA, align = "center"),
         rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))) 

allA <- rbind(bio, flood, qual, swim)


## Convergence
allA |>
  ggplot(aes(x = Min)) +
  geom_line(aes(y = rollRange),
            color = "orange",
            size = 0.8) +
  geom_ribbon(aes(ymin = min,
                  ymax = max),
              fill = "orange",
              alpha = 0.1) +
  dark_theme_minimal() +
  facet_wrap(~ES, nrow = 4)  +
  labs(title = "Level of Convergence",
       subtitle = "plotted over the time of the deliberation",
       x = "Duration of Deliberation (min)",
       y = "Level of Convergence") 
ggsave(paste0(here::here("out/Group_"), group_num, "_converge.png"), width = 400, height = 200, units = "mm", bg = "black")
ggsave(paste0(here::here("out/Group_"), group_num, "_converge.pdf"), width = 400, height = 200, units = "mm", bg = "black")


## Convergence Rate
allA |>
  ggplot(aes(x = Min)) +
  geom_line(aes(y = rescale(rate_of_change, to = c(-3, 3), from = c(3, -3))),
            color = "orange",
            size = 0.8) +
  geom_ribbon(aes(ymin = rescale(min - 1, to = c(-0.2,  0.2)),
                  ymax = rescale(max - 1, to = c(-0.2,  0.2))),
              fill = "orange",
              alpha = 0.1) +
  dark_theme_minimal() +
  facet_wrap(~ES, nrow = 4)  +
  labs(title = "Rate of Convergence",
       subtitle = "plotted over the time of the deliberation",
       x = "Duration of Deliberation (min)",
       y = "Rate of Convergence")
ggsave(paste0(here::here("out/Group_"), group_num, "_converge_rate.png"), width = 400, height = 200, units = "mm", bg = "black")
ggsave(paste0(here::here("out/Group_"), group_num, "_converge_rate.pdf"), width = 400, height = 200, units = "mm", bg = "black")


## Mountains
allA |>
  mutate(Rate = rescale(rate_of_change, to = c(-3, 3), from = c(3, -3))) |>
  mutate(Rate_p = case_when(
    Rate <= 0 ~ 0,
    Rate > 0 ~ Rate
  ),
  Rate_n = case_when(
    Rate >= 0 ~ 0,
    Rate < 0 ~ Rate
  )) |>
  ggplot(aes(x = Min)) +
  geom_line(aes(y = Rate), color = "orange", size = 0.8) +
  geom_ribbon(aes(ymin = 0,
                  ymax = Rate_p,
                  fill = "orange",
                  alpha = 0.8)) +
  geom_ribbon(aes(ymin = 0,
                  ymax = Rate_n,
                  fill = "red",
                  alpha = 0.8)) +
  geom_hline(yintercept = 0, alpha = 0.2) +
  dark_theme_minimal() +
  facet_wrap(~ES, nrow = 4)  +
  labs(title = "Rate of Convergence",
       x = "Duration of Deliberation (min)",
       y = "Rate of Convergence") +
  theme(legend.position="none")
ggsave(paste0(here::here("out/Group_"), group_num, "_mtns.png"), width = 400, height = 200, units = "mm", bg = "black")
ggsave(paste0(here::here("out/Group_"), group_num, "_mtns.pdf"), width = 400, height = 200, units = "mm", bg = "black")
}