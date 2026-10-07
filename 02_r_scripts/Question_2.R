library(dplyr)
library(ggplot2)

game_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv')

game_films <- game_films |>
  mutate(
    numerical_cinema_score = recode(cinema_score,
      "A+" = 100,
      "A"  = 97,
      "A-" = 93,
      "B+" = 90,
      "B"  = 87,
      "B-" = 84,
      "C+" = 80,
      "C"  = 77,
      "C-" = 74,
      "D+" = 70,
      "D"  = 67,
      "D-" = 64,
      "F+" = 60,
      "F"  = 57,
      "F-" = 54,
      .default = 0
    )
  )

game_films$gap <- game_films$metacritic - game_films$rotten_tomatoes

ggplot(game_films, aes(x=release_date,y=gap)) +   geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size= 2.5, color="#08519c") +
  labs(
    title = "Audiences rate game films higher than critics do",
    subtitle = "Above 0 means the audience score is higher than the critic score",
    x = "Release date",
    y = "Audience minus critics (points)"
  )
  theme_minimal()


