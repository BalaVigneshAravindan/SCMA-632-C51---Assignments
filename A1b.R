.libPaths()
# Load necessary libraries
library(dplyr)
library(readr)
# Read the CSV file
data <- read_csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\Cricket_data.csv")
spec(data)
data <- read_csv("C:\\Users\\Bala Vignesh.A\\Desktop\\SCMA 632\\Cricket_data.csv", col_types = cols(
  home_key_batsman = col_character(),
  home_key_bowler = col_character(),
  home_playx1 = col_character(),
  away_playx1 = col_character(),
  away_key_batsman = col_character(),
  away_key_bowler = col_character()
))
columns_to_check <- c('home_key_batsman', 'home_key_bowler', 'home_playx1', 'away_playx1', 'away_key_batsman', 'away_key_bowler')
library(stringr)
# Create a filter mask
filter_mask <- data %>%
  select(all_of(columns_to_check)) %>%
  mutate(across(everything(), ~str_detect(., 'Rahul Tewatia'))) %>%
  rowwise() %>%
  mutate(filter_mask = any(c_across(everything()))) %>%
  pull(filter_mask)
# Filter the data
rahul_tewatia_data <- data[filter_mask, ]
# View the filtered data
print(rahul_tewatia_data)
# Count the number of rows where 'Rahul Tewatia' appears alone
num_alone <- nrow(rahul_tewatia_data)
print(rahul_tewatia_data)
# Calculate the number of rows where 'Rahul Tewatia' appears alone
num_alone <- nrow(rahul_tewatia_data)
cat("Number of rows where 'Rahul Tewatia' appears alone: ", num_alone, "\n")

# Arrange data round-wise and batsman, ball, runs, and wickets per player per match
data_arranged <- data %>%
  mutate(round = ifelse(match_days <= 15, "Round 1",
                        ifelse(match_days > 15 & match_days <= 30, "Round 2", "Round 3")),
         Balls = home_overs * 6) %>%
  select(round, season, id, name, short_name, description, home_team, away_team, toss_won, decision, `1st_inning_score`, `2nd_inning_score`, winner, result, start_date, end_date, venue_id, venue_name, home_captain, away_captain, pom, points, super_over, home_overs, home_runs, home_wickets, home_boundaries, Player = home_key_batsman, match_days, umpire1, umpire2, tv_umpire, referee, reserve_umpire, Runs = home_runs, Wickets = home_wickets) %>%
  filter(!is.na(Player)) %>%
  arrange(round, season, match_days, Player)

# Indicate the top three run-getters and top three wicket-takers in each round
# Indicate the top three run-getters and top three wicket-takers in each round
data_top_players <- data_arranged %>%
  group_by(round) %>%
  top_n(n = 3, wt = Runs) %>%
  arrange(round, desc(Runs)) %>%
  select(round, Player, Runs) %>%
  rename(Batsman = Player) %>%
  bind_rows(
    data_arranged %>%
      group_by(round) %>%
      top_n(n = 3, wt = Wickets) %>%
      arrange(round, desc(Wickets)) %>%
      select(round, Player, Wickets) %>%
      rename(Bowler = Player)
  ) %>%
  arrange(round, desc(Runs), desc(Wickets))

# Print the top three run-getters and top three wicket-takers in each round
print(data_top_players)

install.packages("ggplot2")

# Load necessary libraries
library(ggplot2)

# Create a dataframe with the player's details
player_data <- data.frame(
  Player = "Rahul Tewatia",
  Salary = 900,
  Runs = 1000  # assume 1000 runs for Rahul Tewatia
)

# Plot the relationship between a player's performance and the salary he gets
ggplot(player_data, aes(x = Runs, y = Salary, label = Player)) +
  geom_point() +
  geom_text(nudge_y = 0.1, size = 3) +
  labs(x = "Runs", y = "Salary", title = "Relationship between a player's performance and salary")

