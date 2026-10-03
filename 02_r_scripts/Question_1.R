library(dplyr)
library(ggplot2)

game_films_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')
CPI_raw <- readr::read_csv('00_raw_data/OECD_Table.csv')

CPI <- CPI_raw |>
  filter(UNIT_MEASURE == "IX") |>
  select(REF_AREA,TIME_PERIOD,OBS_VALUE)

currency_country <- tibble(
  budget_currency = c("$", "¥", "£"),
  country_code = c("USA", "JPN", "GBR")
)

#First add today's budget column
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



#mutate(budget_estimate_today = budget_estimate*(CPI_Today/CPI_original))



head(game_films)
