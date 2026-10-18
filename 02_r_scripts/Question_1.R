library(dplyr)
library(ggplot2)

game_films_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')
CPI_raw <- readr::read_csv('00_raw_data/OECD_Table.csv')

CPI <- CPI_raw |>
  filter(UNIT_MEASURE == "IX") |>
  select(REF_AREA,TIME_PERIOD,OBS_VALUE,OBS_VALUE_TODAY)

currency_country <- tibble(
  budget_currency = c("$", "¥", "£"),
  worldwide_box_office_currency = c("$", "¥", "£"),
  country_code = c("USA", "JPN", "GBR"),
  exchange_rate = c(1,0.0063,1.32)
)

#First create df with budget in todays value in usd 
game_films_budget <- game_films_raw |>
  filter(!is.na(budget_currency)) |>
  mutate(
    budget_estimate = (budget_low+budget_high)/2,
    release_year = as.double(format(release_date,"%Y"))
  )

game_films_budget <- left_join(
  game_films_budget,
  currency_country,
  by="budget_currency")

game_films_budget <- left_join(
  game_films_budget,
  CPI,
  by=c(
    "country_code"="REF_AREA",
    "release_year"="TIME_PERIOD"
    )
)

game_films_budget <- game_films_budget |>
  mutate(
    budget_estimate_today = budget_estimate*(OBS_VALUE_TODAY/OBS_VALUE),
    budget_estimate_today_usd = budget_estimate_today*exchange_rate
    )

#Next create df with box office in todays value in usd
game_films_box_office <- game_films_raw |>
  filter(!is.na(worldwide_box_office_currency)) |>
  mutate(release_year = as.double(format(release_date,"%Y")))

game_films_box_office <- left_join(
  game_films_box_office,
  currency_country,
  by="worldwide_box_office_currency")

game_films_box_office <- left_join(
  game_films_box_office,
  CPI,
  by=c(
    "country_code"="REF_AREA",
    "release_year"="TIME_PERIOD"
  )
)

game_films_box_office <- game_films_box_office |>
  mutate(
    worldwide_box_office_today = worldwide_box_office*(OBS_VALUE_TODAY/OBS_VALUE),
    worldwide_box_office_today_usd = worldwide_box_office_today*exchange_rate
  )

#Join together budget and box office
game_films <- inner_join(
  game_films_budget,
  game_films_box_office,
  by=c("title","director","release_date")
)

game_films <- game_films |>
  select(category.x:release_date,original_game_publisher.x,budget_estimate_today_usd,worldwide_box_office_today_usd) |>
  mutate(ROI = (worldwide_box_office_today_usd-budget_estimate_today_usd)/budget_estimate_today_usd) 

budget_cutoff <- quantile(
  game_films$budget_estimate_today_usd,
  probs = c(0,1/3,2/3,1))

game_films$budget_group <- cut(game_films$budget_estimate_today_usd,breaks=budget_cutoff,labels=c("Low\n$3-46M","Medium\n$46-92M","High\n$92-259M"),include.lowest=TRUE)

#Median ROI per budget group
medians <- game_films |>
  dplyr::group_by(budget_group) |>
  dplyr::summarise(median_ROI = median(ROI))

write.csv(game_films, "01_tidy_data/Q1.csv")
# Figure 1
game_films %>%
  ggplot(aes(x = budget_group, y = ROI, fill = budget_group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.3, alpha = 0.8) +
  geom_text(data = dplyr::filter(game_films, ROI > 10), aes(label = title),
            nudge_x = 0.22, hjust = 0, size = 3) +
  geom_label(data = medians,
             aes(x = budget_group, y = 0.3 * max(game_films$ROI),
                 label = sprintf("Median ROI: %.1fx", median_ROI)),
             inherit.aes = FALSE, fill = "white", size = 3.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(expand = expansion(add = c(0.6, 1.4))) +
  scale_y_continuous(n.breaks = 10, labels = scales::label_number(suffix = "x")) +
  labs(
    title = "Low-budget films have the biggest hits, but not a higher typical ROI",
    subtitle = "ROI = (worldwide box office - budget) / budget",
    x = "Budget group (inflation-adjusted, today's USD)",
    y = "Return on investment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Figure 2: budget vs worldwide box office (log scales)
ggplot(game_films, aes(x = budget_estimate_today_usd / 1e6,
                       y = worldwide_box_office_today_usd / 1e6,
                       color = budget_group)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 2.6, alpha = 0.85) +
  scale_x_log10(labels = scales::label_dollar(suffix = "M", accuracy = 1)) +
  scale_y_log10(labels = scales::label_dollar(suffix = "M", accuracy = 1)) +
  scale_color_brewer(palette = "Set2", labels = function(x) gsub("\n", " ", x)) +
  labs(
    title = "Bigger budgets earn bigger grosses",
    subtitle = "Dashed line: worldwide box office equals budget",
    x = "Production budget (log scale)",
    y = "Worldwide box office (log scale)",
    color = "Budget tier"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")