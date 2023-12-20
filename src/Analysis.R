library(tidyverse)
library(ggthemes)
library(ggpubr)
library(zoo)
library(patchwork)
library(ggdark)
library(scales)


#### ADD
#--- Titles with Workshop
#--- The word Values
#--- ggarrange the plots as a grid?

theme_set(dark_theme_minimal())

main <- "#FF9A5B"
secondary <- "#A199FF"
accent <- "#92FF82"
earth <- "#AA8A76"
grey <- "#6F6E80"

participant_cols <- c(
  "#FFA984",
  "#C6C5FF",
  "#FF5F4F",
  "#4297FF",
  "#FFCA69",
  "#C9FFA7"
)


# Define Functions
get_min <- function(Time) { # create a function with the name my_function
  hour <- format(strptime(Time, format = "%H:%M:%S"), "%H")
  min <- format(strptime(Time, format = "%H:%M:%S"), "%M")
  sec <- format(strptime(Time, format = "%H:%M:%S"), "%S")
  min <- as.numeric(min) + as.numeric(sec) * (1 / 60) + as.numeric(hour) * 60
  return(min)
}

# Import Data
data <- read_csv(here::here("data/data.csv")) |>
  mutate(Participant = factor(Participant)) |>
  mutate(
    "Min" = get_min(Time),
    Min = round(Min, 2)
  ) |>
  select(-Time) |>
  arrange(Group, Participant, Min) |>
  filter(!is.na(Swimming)) |>
  pivot_longer(
    cols = c(Swimming, Flooding, Biodiversity, `Water Quality`),
    names_to = "ES"
  ) |>
  filter(value != 0)



# Easy Plot

i <- 0
groups <- unique(data$Group)
origin <- NULL
for (i in i:3) {
  i <- i + 1
  group_num <- groups[i]
  origin[[i]] <- data |>
    filter(Group == group_num, value != 0) |>
    mutate(Value = value - 1) |>
    ggplot(aes(
      x = Min,
      y = Value
    )) +
    geom_step(
      size = 1,
      position = "identity",
      aes(color = Participant)
    ) +
    #  scale_color_colorblind() +
    labs(
      title = paste0("Workshop ", group_num),
      x = "Duration of Deliberation (min)",
      y = "Importance"
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, 10),
      minor_breaks = NULL
    ) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(~ES, nrow = 4) +
    scale_color_manual(values = participant_cols)
  origin[[i]]

  ggsave(paste0(here::here("out/Group_"), group_num, "_og.png"), width = 400, height = 200, units = "mm", bg = "white")
}


# Boxes Plot

i <- 0
groups <- unique(data$Group)
boxes <- NULL
converge <- NULL
rate <- NULL
mountains <- NULL

for (i in i:3) {
  i <- i + 1
  group_num <- groups[i]
  group <- data |>
    filter(Group == group_num, value != 0)

  interpolated <- group |>
    complete(
      Min = seq(min(Min), max(Min), by = 0.01),
      Group = group_num,
      Participant = factor(1:6),
      ES = ES
    ) |>
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
    summarise(
      "ES" = ES,
      "Group" = Group,
      "Min" = Min,
      "Max" = max(Value),
      "Minimum" = min(Value),
      "Mean" = mean(Value),
      "sd" = sd(Value)
    ) |>
    left_join(interpolated, by = c("Group", "ES", "Min"))

  boxes[[i]] <- ggplot(joinedT, aes(
    x = Min,
    y = Value
  )) +
    geom_ribbon(
      aes(
        ymin = Minimum,
        ymax = Max
      ),
      alpha = 0.4,
      fill = main
    ) +
    labs(
      title = paste0("Workshop ", group_num),
      x = "Duration of Deliberation (min)",
      y = "Importance"
    ) +
    scale_x_continuous(
      breaks = seq(0, 100, 10),
      minor_breaks = NULL
    ) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_wrap(~ES, nrow = 4)
  boxes[[i]]

  ggsave(paste0(here::here("out/Group_"), group_num, "_boxes.png"), width = 400, height = 200, units = "mm", bg = "white")



  ## Rolling Data Prep

  time_interval <- 0.5
  window <- 10
  k <- round(window / time_interval)

  #####################  ##########################################  #####################
  bio <- data |>
    filter(Group == group_num, value != 0, ES == "Biodiversity") |>
    mutate(Min = round(Min * 2) / 2) |>
    mutate(value = round(value))
  max <- max(bio$Min)
  minutes <- seq(0, max, 0.5)
  mins <- crossing(
    Min = minutes,
    Participant = seq(1, 6, 1),
    Group = group_num,
    ES = "Biodiversity"
  )
  bio <- bio |>
    mutate(Participant = as.numeric(Participant)) |>
    full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
    arrange(ES, Participant, Min) |>
    fill(value, .direction = "down") |>
    group_by(Group, ES, Min) |>
    reframe(
      Mean = mean(value),
      se = sd(value) / sqrt(6),
      min = min(value),
      max = max(value),
      range = max - min
    ) |>
    mutate(
      rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
      rollSE = rollmean(se, k = k, fill = NA, align = "center"),
      rollRange = rollmean(range, k = k, fill = NA, align = "center"),
      rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))
    )

  #####################  ##########################################  #####################
  flood <- data |>
    filter(Group == group_num, value != 0, ES == "Flooding") |>
    mutate(Min = round(Min * 2) / 2) |>
    mutate(value = round(value))
  max <- max(flood$Min)
  minutes <- seq(0, max, 0.5)
  mins <- crossing(
    Min = minutes,
    Participant = seq(1, 6, 1),
    Group = group_num,
    ES = "Flooding"
  )
  flood <- flood |>
    mutate(Participant = as.numeric(Participant)) |>
    full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
    arrange(ES, Participant, Min) |>
    fill(value, .direction = "down") |>
    group_by(Group, ES, Min) |>
    reframe(
      Mean = mean(value),
      se = sd(value) / sqrt(6),
      min = min(value),
      max = max(value),
      range = max - min
    ) |>
    mutate(
      rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
      rollSE = rollmean(se, k = k, fill = NA, align = "center"),
      rollRange = rollmean(range, k = k, fill = NA, align = "center"),
      rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))
    )

  #####################  ##########################################  #####################
  qual <- data |>
    filter(Group == group_num, value != 0, ES == "Water Quality") |>
    mutate(Min = round(Min * 2) / 2) |>
    mutate(value = round(value))
  max <- max(qual$Min)
  minutes <- seq(0, max, 0.5)
  mins <- crossing(
    Min = minutes,
    Participant = seq(1, 6, 1),
    Group = group_num,
    ES = "Water Quality"
  )
  qual <- qual |>
    mutate(Participant = as.numeric(Participant)) |>
    full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
    arrange(ES, Participant, Min) |>
    fill(value, .direction = "down") |>
    group_by(Group, ES, Min) |>
    reframe(
      Mean = mean(value),
      se = sd(value) / sqrt(6),
      min = min(value),
      max = max(value),
      range = max - min
    ) |>
    mutate(
      rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
      rollSE = rollmean(se, k = k, fill = NA, align = "center"),
      rollRange = rollmean(range, k = k, fill = NA, align = "center"),
      rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))
    )

  #####################  ##########################################  #####################
  swim <- data |>
    filter(Group == group_num, value != 0, ES == "Swimming") |>
    mutate(Min = round(Min * 2) / 2) |>
    mutate(value = round(value))
  max <- max(swim$Min)
  minutes <- seq(0, max, 0.5)
  mins <- crossing(
    Min = minutes,
    Participant = seq(1, 6, 1),
    Group = group_num,
    ES = "Swimming"
  )
  swim <- swim |>
    mutate(Participant = as.numeric(Participant)) |>
    full_join(mins, by = c("Participant", "Min", "ES", "Group")) |>
    arrange(ES, Participant, Min) |>
    fill(value, .direction = "down") |>
    group_by(Group, ES, Min) |>
    reframe(
      Mean = mean(value),
      se = sd(value) / sqrt(6),
      min = min(value),
      max = max(value),
      range = max - min
    ) |>
    mutate(
      rollMean = rollmean(Mean, k = k, fill = NA, align = "center"),
      rollSE = rollmean(se, k = k, fill = NA, align = "center"),
      rollRange = rollmean(range, k = k, fill = NA, align = "center"),
      rate_of_change = (rollRange - lag(rollRange, n = 10)) / (Min - lag(Min, n = 10))
    )

  allA <- rbind(bio, flood, qual, swim) |>
    mutate(
      Convergence = rescale(rate_of_change, to = c(-3, 3), from = c(3, -3)),
      Inverted_Range = (rollRange * -1) + 3
    )


  ## Convergence
  converge[[i]] <- allA |>
    ggplot(aes(x = Min)) +
    geom_line(aes(y = Inverted_Range),
      color = main,
      size = 0.8
    ) +
    geom_ribbon(
      aes(
        ymin = rescale(min - 1, to = c(0, 3)),
        ymax = rescale(max - 1, to = c(0, 3))
      ),
      fill = main,
      alpha = 0.1
    ) +
    facet_wrap(~ES, nrow = 4) +
    labs(
      title = paste0("Workshop ", group_num),
      x = "Duration of Deliberation (min)",
      y = "Level of Convergence"
    )
  converge[[i]]

  ggsave(paste0(here::here("out/Group_"), group_num, "_converge.png"), width = 400, height = 200, units = "mm", bg = "white")


  ## Convergence Rate
  rate[[i]] <- allA |>
    ggplot(aes(x = Min)) +
    geom_line(aes(y = Convergence),
      color = main,
      size = 0.8
    ) +
    geom_ribbon(
      aes(
        ymin = rescale(min - 1, to = c(-0.2, 0.2)),
        ymax = rescale(max - 1, to = c(-0.2, 0.2))
      ),
      fill = main,
      alpha = 0.1
    ) +
    facet_wrap(~ES, nrow = 4) +
    labs(
      title = paste0("Workshop ", group_num),
      x = "Duration of Deliberation (min)",
      y = "Rate of Convergence"
    )
  rate[[i]]


  ggsave(paste0(here::here("out/Group_"), group_num, "_converge_rate.png"), width = 400, height = 200, units = "mm", bg = "white")


  ## Mountains
  mountains[[i]] <- allA |>
    mutate(Rate = rescale(rate_of_change, to = c(-3, 3), from = c(3, -3))) |>
    mutate(
      Rate_p = case_when(
        Rate <= 0 ~ 0,
        Rate > 0 ~ Rate
      ),
      Rate_n = case_when(
        Rate >= 0 ~ 0,
        Rate < 0 ~ Rate
      )
    ) |>
    ggplot(aes(x = Min)) +
    geom_line(aes(y = Rate), color = accent, size = 0.8) +
    geom_ribbon(
      aes(
        ymin = 0,
        ymax = Rate_p
      ),
      alpha = 0.4,
      fill = main
    ) +
    geom_ribbon(
      aes(
        ymin = 0,
        ymax = Rate_n
      ),
      fill = secondary,
      alpha = 0.4
    ) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    facet_wrap(~ES, nrow = 4) +
    labs(
      title = paste0("Workshop ", group_num),
      x = "Duration of Deliberation (min)",
      y = "Rate of Convergence"
    ) +
    theme(legend.position = "none")
  mountains[[i]]

  ggsave(paste0(here::here("out/Group_"), group_num, "_mtns.png"), width = 400, height = 200, units = "mm", bg = "white")
}

ggarrange(origin[[1]], origin[[2]], origin[[3]], origin[[4]])
ggsave(here::here("out/All_origin.png"), width = 300, height = 300, units = "mm")

ggarrange(boxes[[1]], boxes[[2]], boxes[[3]], boxes[[4]])
ggsave(here::here("out/All_boxes.png"), width = 300, height = 300, units = "mm")

ggarrange(converge[[1]], converge[[2]], converge[[3]], converge[[4]])
ggsave(here::here("out/All_converge.png"), width = 300, height = 300, units = "mm")

ggarrange(rate[[1]], rate[[2]], rate[[3]], rate[[4]])
ggsave(here::here("out/All_rate.png"), width = 300, height = 300, units = "mm")

ggarrange(mountains[[1]], mountains[[2]], mountains[[3]], mountains[[4]])
ggsave(here::here("out/All_Mtns.png"), width = 300, height = 300, units = "mm")
