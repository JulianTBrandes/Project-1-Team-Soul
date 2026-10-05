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
  mutate(ROI = worldwide_box_office_today_usd/budget_estimate_today_usd)

budget_cutoff <- quantile(
  game_films$budget_estimate_today_usd,
  probs = c(0,1/3,2/3,1))

game_films$budget_group <- cut(game_films$budget_estimate_today_usd,breaks=budget_cutoff,labels=c("Low","Medium","High"),include.lowest=TRUE)

game_films %>%
  ggplot(aes(x=budget_group, y=ROI, fill=budget_group)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal()
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ROI by Budget Group") +
  xlab("")

