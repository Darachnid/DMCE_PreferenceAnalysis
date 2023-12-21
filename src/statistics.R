library(tidyverse)
library(ggthemes)
library(ggdark)
library(scales)
library(performance)
library(emmeans)
library(broom)


theme_set(theme_minimal())

time_interval <- 0.5

GA <- read_rds("out/data/data_processed_GA.RDS")
GB <- read_rds("out/data/data_processed_GB.RDS")
GC <- read_rds("out/data/data_processed_GC.RDS")
GD <- read_rds("out/data/data_processed_GD.RDS")
data <- rbind(GA, GB, GC, GD) |>
  select(Group, ES, Min, Convergence) |>
  rename(Rate = Convergence) |>
  mutate(
    Convergence = case_when(
      Rate <= 0 ~ 0,
      Rate > 0 ~ Rate
    ),
    Divergence = case_when(
      Rate > 0 ~ 0,
      Rate <= 0 ~ (Rate * -1)
    )
  )

DATA <- NULL
i <- 0
groups <- unique(data$Group)

for (i in 0:3) {
  i <- i + 1
  group_num <- groups[i]


  bounds <- data |>
    filter(
      Group == group_num,
      Convergence > 0 | Divergence > 0
    ) |>
    summarise(
      "start" = min(Min),
      "end" = max(Min)
    )

  sect1_end <- bounds$end / 3
  sect2_end <- 2 * (bounds$end / 3)

  DATA[[i]] <- data |>
    select(-Rate) |>
    filter(
      Group == group_num,
      Min >= bounds$start & Min <= bounds$end
    ) |>
    mutate(
      Section = case_when(
        Min <= sect1_end ~ "Beginning",
        Min > sect1_end & Min <= sect2_end ~ "Middle",
        Min > sect2_end ~ "End"
      ),
      Section = factor(Section, levels = c(
        "Beginning",
        "Middle",
        "End"
      ))
    )
}

joined <- rbind(DATA[[1]], DATA[[2]], DATA[[3]], DATA[[4]])

table <- joined |>
  group_by(Group, ES, Section) |>
  summarise(
    Convergence = sum(Convergence) * time_interval,
    Divergence = sum(Divergence) * time_interval
  )



######### Boxplots

table |>
  ggplot(aes(x = Section, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Covergence per Section of Deliberation")

ggsave("out/summaryStats/Convergence_X_section.png", bg = "white")

table |>
  ggplot(aes(x = Section, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Section of Deliberation")

ggsave("out/summaryStats/Divergence_X_section.png", bg = "white")

table |>
  ggplot(aes(x = Group, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Covergence per Group")

ggsave("out/summaryStats/Convergence_X_group.png", bg = "white")

table |>
  ggplot(aes(x = Group, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Group")

ggsave("out/summaryStats/Divergence_X_group.png", bg = "white")

table |>
  ggplot(aes(x = ES, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Covergence per Ecosystem Service")

ggsave("out/summaryStats/Convergence_X_ES.png", bg = "white")

table |>
  ggplot(aes(x = ES, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Ecosystem Service")

ggsave("out/summaryStats/Divergence_X_ES.png", bg = "white")


############### Linear Model

lm1 <- lm(Convergence ~ Section + Group, data = table)
lm2 <- lm(Convergence ~ Section + ES, data = table)
lm3 <- lm(Convergence ~ Section, data = table)

check_model(lm1)
check_model(lm2)
check_model(lm3)
## Influential Observations, Normality of Residuals do not hold

glm1 <- glm(Convergence ~ Section + Group, data = table)
glm2 <- lm(Convergence ~ Section + ES, data = table)
glm3 <- lm(Convergence ~ Section, data = table)

check_model(glm1)
check_model(glm2)
check_model(glm3)


## They aren't perfect but glm2 seems the best

con_em <- emmeans(glm2, ~Section)

con_em |>
  tidy() |>
  mutate(
    Section = factor(Section, levels = c(
      "Beginning",
      "Middle",
      "End"
    )),
    upper = estimate + std.error,
    lower = estimate - std.error
  ) |>
  ungroup() |>
  ggplot(aes(x = Section)) +
  geom_point(aes(y = estimate),
    color = "black"
  ) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
    color = "black",
    width = 0.5,
    size = 1
  )

con_em_predict <- augment(glm2, interval = "confidence") |>
  mutate(
    Section = factor(Section, levels = c(
      "Beginning",
      "Middle",
      "End"
    )),
    Section = as.numeric(Section)
  )


ggplot(
  data = con_em_predict,
  aes(x = Section)
) +
  geom_jitter(
    mapping = aes(
      y = Convergence,
      color = ES
    ),
    width = 0.15,
    height = 0
  ) +
  geom_line(
    mapping = aes(
      y = .fitted,
      color = ES
    ),
    size = 1
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Beginning", "Middle", "End")
  ) +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) +
  theme_minimal() +
  scale_color_colorblind() +
  labs(
    title = "Mean Effect of Section on Convergence",
    subtitle = "1 = Beginning, 2 = Middle, 3 = End",
    y = "Mean Effect on Convergence"
  )

ggsave("out/Stats/MeanEffect.png", bg = "white")

ggplot(
  data = con_em_predict,
  aes(x = Section)
) +
  geom_ribbon(
    aes(
      ymin = .lower,
      ymax = .upper,
      fill = ES
    ),
    alpha = 0.4,
    color = NA
  ) +
  geom_jitter(
    mapping = aes(
      y = Convergence,
      color = ES
    ),
    width = 0,
    height = 0
  ) +
  geom_line(
    mapping = aes(
      y = .fitted,
      color = ES
    ),
    size = 1
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Beginning", "Middle", "End")
  ) +
  scale_y_continuous(breaks = c(0, 1, 2, 3)) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_wrap(~ES,
    nrow = 4
  ) +
  labs(
    title = "Mean Effect of Section on Convergence",
    subtitle = "1 = Beginning, 2 = Middle, 3 = End",
    y = "Mean Effect +- standard error"
  )

ggsave("out/Stats/MeanEffect_faceted.png", bg = "white")
