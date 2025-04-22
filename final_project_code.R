library(tidyverse)
library(janitor)
library(ggplot2)

# Load Sonic games dataset
sonic_games_df <- read.csv("sonic_games.csv") |> clean_names()

# Convert sales, revenue, and streams to millions
sonic_games_df <- sonic_games_df |>
  mutate(
    units_sold = units_sold / 1e6,
    gross_revenue = gross_revenue / 1e6,
    soundtrack_streams = soundtrack_streams / 1e6
    )

# Group comparisons
sonic_games_df |>
  group_by(crush_40_involved) |>
  summarise(
    avg_critic = mean(metacritic),
    avg_user = mean(user_score),
    avg_sales = mean(units_sold, na.rm = TRUE)
  )

# Plots
# Violin plot of game success compared to Crush 40 involvement
sonic_games_df |>
  ggplot(aes(x = crush_40_involved, y = units_sold)) + 
  geom_violin(trim = FALSE, fill = "skyblue") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.3)

# Scatter plot of fan reception vs critic score
sonic_games_df |>
  ggplot(aes(x = metacritic, y = user_score)) +
  geom_point(aes(color = crush_40_involved), size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40") +
  labs(title = "Fan Ratings vs Critic Scores",
       x = "Critic Score (Metacritic)",
       y = "Fan Score (Metacritic)",
       color = "Is Crush 40 Involved") +
  facet_wrap(~ is_3d) +
  theme_bw()

# Relationship between fan rating, units sold, and number of streams of music
sonic_games_df |>
  ggplot(aes(x = soundtrack_streams, y = user_score, size = units_sold)) + 
  geom_point(alpha = 0.7, color = "steelblue") +
  labs(
    title = "Soundtrack Streams vs Fan Ratings (Bubble Size = Sales)",
    x = "Spotify and Youtube Streams (Millions)",
    y = "User Score",
    size = "Units Sold (Millions)"
  ) +
  theme_bw() +
  facet_wrap(~ crush_40_involved)

# Success of each game
sonic_games_success <- sonic_games_df |>
  pivot_longer(cols = c(units_sold, metacritic, user_score, gross_revenue),
               names_to = "metric", values_to = "value")

sonic_games_success |>
  ggplot(aes(x = metric, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~ title, scales = "free_y", nrow()) +
  labs(
    title = "Comparison of Metrics by Game",
    x = "Metric",
    y = "Value"
  ) +
  theme_bw()

sonic_games_df <- sonic_games_df |>
  mutate(score_diff = user_score - metacritic)

sonic_games_df |>
  group_by(crush_40_involved) |>
  summarise(avg_score_diff = mean(score_diff, na.rm = TRUE))
