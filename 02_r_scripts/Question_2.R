library(dplyr)
library(ggplot2)

game_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')
game_films <- game_films |>
  mutate(release_date = as.Date(release_date))

# Audience score: CinemaScore letters -> points
grades <- c("A+" = 100, "A" = 97, "A-" = 93,
            "B+" = 90,  "B" = 87, "B-" = 84,
            "C+" = 80,  "C" = 77, "C-" = 74,
            "D+" = 70,  "D" = 67, "D-" = 64,
            "F+" = 60,  "F" = 57, "F-" = 54)

game_films <- game_films |>
  mutate(audience = unname(grades[cinema_score]))   # "N/A" and NA become NA

# Budget group from the Q1 results (run the Q1 script first so Q1.csv exists)
game_films_Q1 <- readr::read_csv("01_tidy_data/Q1.csv", show_col_types = FALSE) |>
  mutate(release_date = as.Date(release_date),
         budget_group = gsub("\r", "", budget_group),
         budget_group = factor(budget_group,
                               levels = c("Low\n$3-46M", "Medium\n$46-92M", "High\n$92-259M"))) |>
  select(title, director, release_date, budget_group)

game_films <- left_join(game_films, game_films_Q1,
                        by = c("title", "director", "release_date"))

# Critic score, gap, year
game_films <- game_films |>
  mutate(
    critic = (rotten_tomatoes + metacritic) / 2,
    gap    = audience - critic,
    year   = as.numeric(format(release_date, "%Y"))
  )

# Keep only films with all three ratings and a release date
q2 <- game_films |> filter(!is.na(gap), !is.na(release_date))

# Eras and average scores per era
q2 <- q2 |>
  mutate(era = cut(year, breaks = c(-Inf, 2009, 2019, Inf),
                   labels = c("Before 2010", "2010s", "2020s")))

era_summary <- q2 |>
  group_by(era) |>
  summarise(n = n(), mean_gap = mean(gap),
            mean_critic = mean(critic), mean_audience = mean(audience)) |>
  mutate(era_label = factor(paste0(era, "\n(n = ", n, ")"),
                            levels = paste0(era, "\n(n = ", n, ")")))

# Numbers for the write-up
nrow(q2)                                              # films in the sample
era_summary                                           # averages and gap by era
trend <- lm(gap ~ year, data = q2)
summary(trend)$coefficients["year", ]                 # slope per year and p-value
cor.test(q2$year, q2$gap, method = "spearman", exact = FALSE)$estimate

# Figure 1: gap over time, points colored by budget group
ggplot(q2, aes(x = release_date, y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_smooth(method = "lm", color = "grey30", fill = "grey60", alpha = 0.3) +
  geom_point(aes(color = budget_group), size = 2.8) +
  scale_color_brewer(palette = "Set2", na.value = "grey60",
                     labels = function(x) gsub("\n", " ", x)) +
  scale_y_continuous(limits = c(0, 80)) +
  labs(
    title = "Audiences rate game films higher than critics, but the gap is shrinking",
    subtitle = "Above 0 means the audience score is higher than the critic score",
    x = "Release date",
    y = "Audience minus critics (points)",
    color = "Budget group"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Figure 2: average critic and audience score by era (the line is the gap)
cols <- c("Critics" = "#D55E00", "Audience" = "#0072B2")

ggplot(era_summary, aes(y = era_label)) +
  geom_segment(aes(x = mean_critic, xend = mean_audience, yend = era_label),
               color = "grey60", linewidth = 1.2) +
  geom_point(aes(x = mean_critic,   color = "Critics"),  size = 4.5) +
  geom_point(aes(x = mean_audience, color = "Audience"), size = 4.5) +
  geom_text(aes(x = (mean_critic + mean_audience) / 2,
                label = sprintf("gap %.0f", mean_gap)), vjust = -1.2, size = 3.5) +
  scale_color_manual(values = cols) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_discrete(limits = rev) +
  labs(
    title = "Critics are catching up with audiences, but a large gap remains",
    x = "Average score (0-100)", y = NULL, color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")