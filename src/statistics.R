library(tidyverse)
library(ggthemes)
library(scales)
library(performance)
library(emmeans)
library(broom)
library(car)
library(dotwhisker)


theme_set(theme_minimal())
main <- "#FF9A5B"
secondary <- "#A199FF"

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
  ) |>
  mutate(Convergent = as.integer(Convergence > 0))



######### Boxplots

consec <- table |>
  ggplot(aes(x = Section, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Section of Deliberation")
consec +
  labs(title = "Covergence per Section of Deliberation")
ggsave("out/summaryStats/Convergence_X_section.png", bg = "white")

table |>
  ggplot(aes(x = Section, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Section of Deliberation")

ggsave("out/summaryStats/Divergence_X_section.png", bg = "white")

congroup <- table |>
  ggplot(aes(x = Group, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Deliberation", x = "Deliberation")
congroup +
  labs(title = "Covergence per Deliberation")
ggsave("out/summaryStats/Convergence_X_group.png", bg = "white")

table |>
  ggplot(aes(x = Group, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Deliberation")

ggsave("out/summaryStats/Divergence_X_group.png", bg = "white")

cones <- table |>
  ggplot(aes(x = ES, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Ecosystem Service")
cones +
  labs(title = "Covergence per Ecosystem Service")
ggsave("out/summaryStats/Convergence_X_ES.png", bg = "white")

table |>
  ggplot(aes(x = ES, y = Divergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Divergence per Ecosystem Service")

ggsave("out/summaryStats/Divergence_X_ES.png", bg = "white")

ggarrange(consec, congroup, cones, nrow = 1) |>
  annotate_figure(top = text_grob("Convergence across", size = 16))

ggsave("out/summaryStats/ConvergenceXall.png", bg = "white", scale = 0.8) 

############### Linear Model

lm1 <- lm(Convergence ~ Section + Group, data = table)
lm2 <- lm(Convergence ~ Section + ES, data = table)
lm3 <- lm(Convergence ~ Section, data = table)

check_model(lm1)
check_model(lm2)
check_model(lm3)

glm1 <- glm(Convergence ~ Section + Group, data = table)
glm2 <- glm(Convergence ~ Section + ES, data = table)
glm3 <- glm(Convergence ~ Section, data = table)
glm4 <- glm(Convergent ~ Section + Group, data = table, family = binomial(link = "logit"))
glm5 <- glm(Convergent ~ Section + ES, data = table, family = binomial(link = "logit"))
glm6 <- glm(Convergent ~ Section, data = table, family = binomial(link = "logit"))

tabl <- table |>
  filter(Convergent == 1) |>
  rename("Workshop" = Group)

glm8 <- glm(Convergence ~ Section + Workshop, data = tabl)
glm9 <- glm(Convergence ~ Section + ES, data = tabl)
glm10 <- glm(Convergence ~ Section, data = tabl)


## Homo of Var
plot(fitted(glm1), residuals(glm1))
abline(h = 0, col = "red")
plot(fitted(glm2), residuals(glm2))
abline(h = 0, col = "red")
plot(fitted(glm3), residuals(glm3))
abline(h = 0, col = "red")
plot(fitted(glm4), residuals(glm4))
abline(h = 0, col = "red")
plot(fitted(glm5), residuals(glm5))
abline(h = 0, col = "red")
plot(fitted(glm6), residuals(glm6))
abline(h = 0, col = "red")

plot(fitted(glm8), residuals(glm8))
abline(h = 0, col = "red")
plot(fitted(glm9), residuals(glm9))
abline(h = 0, col = "red")
plot(fitted(glm10), residuals(glm10))
abline(h = 0, col = "red")


###### Rules out
# 3, 5, 6, 10
# VIF 
vif(glm1)
vif(glm2)
vif(glm8)
vif(glm9)

### All Passed

aic <- compare_performance(glm1, glm2, glm8, glm9, rank = TRUE)
aic

ggtexttable(aic, rows = NULL,
                   theme = ttheme("light")) 

ggsave("out/Tables/AIC.png", bg = "white", width = 9, height = 1.5)

tidy(glm8) |>
ggplot(aes(y = estimate,
           x = factor(term, 
                      levels = rev(c("(Intercept)", "WorkshopB", "WorkshopC", 
                                 "WorkshopD", "SectionMiddle", 
                                 "SectionEnd"))))) +
  geom_hline(yintercept = 0,
             color = "red",
             size = 1,
             alpha = 0.4) +
  geom_point(color = "black") +
  geom_errorbar(aes(ymin = estimate - std.error * 1.96,
                    ymax = estimate + std.error * 1.96)) +
  ylim(-2, 2) +
  coord_flip() +
  labs(title = "GLM Coefficients",
       y = "Coefficient Estimate",
       x = "Predictor Variable") 

ggsave("out/Stats/GLMcoeff.png", bg = "white")

tidy(glm8) |>
  ggtexttable(rows = NULL,
              theme = ttheme("light")) 
ggsave("out/Tables/glm.png", bg = "white", width = 5.2, height = 2)

############# Estimated Marginal Means


con_em <- emmeans(glm8, ~Section)

pa <- pairs(con_em, adjust = "tukey") |> tidy()
ggtexttable(pa, rows = NULL,
            theme = ttheme("light")) 
ggsave("out/Tables/tukey.png", bg = "white", width = 7.5, height = 1.5)


ggplot(pa, aes(x = estimate, y = reorder(contrast, estimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = estimate - std.error * 1.96, xmax = estimate + std.error * 1.96), width = 0.2) +
  geom_vline(xintercept = 0, color = "red",
             size = 1, alpha = 0.4) + 
  labs(title = "Mean Contrasts for the Effect of Section on Convergence",
       x = "Estimated Difference",
       y = "Section Contrast") +
  xlim(-2, 2)
ggsave("out/Stats/GLMcontrasts.png", bg = "white")


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

con_em_predict <- augment(glm8, interval = "confidence") |>
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
      color = Workshop
    ),
    width = 0.15,
    height = 0
  ) +
  geom_line(
    mapping = aes(
      y = .fitted,
      color = Workshop
    ),
    size = 1
  ) +
  scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = c("Beginning", "Middle", "End")
  ) +
  theme_minimal() +
  scale_color_colorblind() +
  labs(
    title = "Mean Effect of Section on Convergence",
    subtitle = "glm(Convergence ~ Section + Deliberation)",
    y = "Mean Effect on Convergence"
  )

ggsave("out/Stats/MeanEffect.png", bg = "white")


## Convergence after filtering
consec <- tabl |>
  ggplot(aes(x = Section, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Section of Deliberation")
consec +
  labs(title = "Covergence per Section of Deliberation")
ggsave("out/summaryStats/Convergence_X_section_filt.png", bg = "white")

congroup <- tabl |>
  ggplot(aes(x = Group, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Deliberation", x = "Deliberation")
congroup +
  labs(title = "Covergence per Deliberation")
ggsave("out/summaryStats/Convergence_X_group_filt.png", bg = "white")

cones <- tabl |>
  ggplot(aes(x = ES, y = Convergence)) +
  geom_boxplot(fill = "white", color = "black") +
  labs(title = "Ecosystem Service")
cones +
  labs(title = "Covergence per Ecosystem Service")
ggsave("out/summaryStats/Convergence_X_ES_filt.png", bg = "white")

ggarrange(consec, congroup, cones, nrow = 1) |>
  annotate_figure(top = text_grob("Convergence across", size = 16))

ggsave("out/summaryStats/ConvergenceXall_filt.png", bg = "white", scale = 0.8) 

