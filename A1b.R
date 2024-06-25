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

install.packages("fitdistrplus")
library(fitdistrplus)
library(MASS)
library(dplyr)  

top_players_summary <- data_arranged %>%
  group_by(Player) %>%
  summarise(Total_Runs = sum(Runs), Total_Wickets = sum(Wickets)) %>%
  top_n(10, Total_Runs)

runs_fit <- fitdist(top_players_summary$Total_Runs, "gamma") 

library(stats)
start_lambda <- mean(top_players_summary$Total_Wickets)
wickets_fit <- suppressWarnings(fitdist(top_players_summary$Total_Wickets, "poisson", 
                                        start = list(lambda = start_lambda)))
dpoisson <- function(x, lambda) dpois(x, lambda)
ppoisson <- function(x, lambda) ppois(x, lambda = lambda)
 
salary_data <- data.frame(
  Player = c(
    "Abhishek Porel", "Anrich Nortje", "Axar Patel", "David Warner", "Ishant Sharma",
    "Kuldeep Yadav", "Lalit Yadav", "Lungi Ngidi", "Mitchell Marsh", "Mukesh Kumar",
    "Pravin Dubey", "Prithvi Shaw", "Rishabh Pant", "Khaleel Ahmed", "Vicky Ostwal",
    "Yash Dhull", "Ajay Mandal", "Ajinkya Rahane", "Deepak Chahar", "Devon Conway",
    "Maheesh Theekshana", "Matheesha Pathirana", "Mitchell Santner", "Moeen Ali",
    "MS Dhoni", "Mukesh Choudhary", "Nishant Sindhu", "Prashant Solanki",
    "Rajvardhan Hangargekar", "Ravindra Jadeja", "Ruturaj Gaikwad", "Shaik Rasheed",
    "Shivam Dube", "Simarjeet Singh", "Tushar Deshpande", "Abhinav Sadarangani",
    "B. Sai Sudharsan", "Darshan Nalkande", "David Miller", "Jayant Yadav",
    "Joshua Little", "Kane Williamson", "Matthew Wade", "Mohammad Shami", "Mohit Sharma",
    "Noor Ahmad", "R. Sai Kishore", "Rahul Tewatia", "Rashid Khan", "Shubman Gill",
    "Vijay Shankar", "Shreyas Iyer", "Nitish Rana", "Venkatesh Iyer", "Andre Russell",
    "Sunil Narine", "Harshit Rana", "Varun Chakravarthy", "Anukul Roy", "Rinku Singh",
    "Rahmanullah Gurbaz", "Amit Mishra", "Ayush Badoni", "Deepak Hooda", "Devdutt Padikkal (T)",
    "K. Gowtham", "KL Rahul", "Krunal Pandya", "Kyle Mayers", "Marcus Stoinis",
    "Mark Wood", "Mayank Yadav", "Mohsin Khan", "Naveen Ul Haq", "Nicholas Pooran",
    "Prerak Mankad", "Quinton De Kock", "Ravi Bishnoi", "Yash Thakur", "Akash Madhwal",
    "Arjun Tendulkar", "Dewald Brevis", "Ishan Kishan", "Hardik Pandya (T)", "Jason Behrendorff",
    "Jasprit Bumrah", "Kumar Kartikeya Singh", "Tilak Varma", "Nehal Wadhera",
    "Piyush Chawla", "Rohit Sharma", "Romario Shepherd (T)", "Shams Mulani", "Surya Kumar Yadav",
    "Tim David", "Vishnu Vinod", "Arshdeep Singh", "Atharva Taide", "Harpreet  Brar",
    "Harpreet Bhatia", "Jitesh Sharma", "Jonny Bairstow", "Kagiso Rabada", "Liam Livingstone",
    "Nathan Ellis", "Prabhsimran Singh", "Rahul Chahar", "Rishi Dhawan", "Sam Curran",
    "Shikhar Dhawan", "Shivam  Singh", "Sikandar Raza", "Vidwath Kaverappa", "Adam Zampa",
    "Avesh Khan (T)", "Dhruv Jurel", "Donovan Ferreira", "Jos Buttler", "Kuldeep  Sen",
    "Kunal Rathore", "Navdeep Saini", "Prasidh Krishna", "R. Ashwin", "Riyan Parag",
    "Sandeep Sharma", "Sanju Samson", "Shimron Hetmyer", "Trent Boult", "Yashaswi Jaiswal",
    "Yuzvendra Chahal", "Akash Deep", "Anuj Rawat", "Dinesh  Karthik", "Faf Du Plessis",
    "Glenn Maxwell", "Himanshu Sharma", "Karn Sharma", "Mahipal Lomror", "Manoj Bhandage",
    "Mayank Dagar (T)", "Mohammed Siraj", "Rajan Kumar", "Rajat Patidar", "Virat Kohli",
    "Vyshak Vijay Kumar", "Will Jacks", "Cameron Green (T)", "Abdul Samad", "Abhishek Sharma",
    "Aiden Markram", "Anmolpreet Singh", "Bhuvneshwar Kumar", "Fazalhaq Farooqi",
    "Glenn Phillips", "Heinrich Klaasen", "Marco Jansen", "Mayank Agarwal", "Mayank Markande",
    "Nitish Kumar Reddy", "Rahul Tripathi", "Sanvir  Singh", "Shahbaz Ahamad (T)", "T. Natarajan",
    "Umran Malik", "Upendra Singh Yadav", "Washington Sundar"
  ),
  Salary = c(
    "20 lakh", "6.5 crore", "9 crore", "6.25 crore", "50 lakh", "2 crore", "65 lakh", "5 crore", "6.5 crore", "5.5 crore",
    "50 lakh", "7.5 crore", "16 crore", "5.25 crore", "20 lakh", "50 lakh", "20 lakh", "50 lakh", "14 crore", "1 crore",
    "70 lakh", "20 lakh", "1.9 crore", "8 crore", "12 crore", "20 lakh", "60 lakh", "1.2 crore", "1.5 crore", "16 crore",
    "6 crore", "20 lakh", "4 crore", "20 lakh", "20 lakh", "2.6 crore", "20 lakh", "20 lakh", "3 crore", "1.7 crore",
    "4.4 crore", "2 crore", "2.4 crore", "6.25 crore", "50 lakhs", "30 lakh", "3 crore", "9 crore", "15 crore", "7 crore",
    "1.4 crore", "12.25 crore", "8 crore", "8 crore", "12 crore", "6 crore", "20 lakh", "8 crore", "20 lakh", "55 lakh",
    "50 lakh", "50 lakh", "20 lakh", "5.75 crore", "7.75 crore", "90 lakh", "17 crore", "8.25 crore", "50 lakh", "9.2 crore",
    "7.5 crore", "20 lakh", "20 lakh", "50 lakh", "16 crore", "20 lakh", "6.75 Crore", "4 crore", "45 lakh", "20 lakh",
    "30 lakh", "3 crore", "15.25 crore", "15 crore", "50 lakh", "12 crore", "20 lakh", "1.70 crore", "20 lakh", "50 lakh",
    "16 Crore", "50 lakh", "20 lakh", "8 crore", "8.25 Crore", "20 lakh", "4 crore", "20 lakh", "3.8 crore", "40 lakh",
    "20 lakh", "6.75 crore", "9.25 crore", "11.5 crore", "75 lakh", "60 lakh", "5.25 crore", "55 lakh", "18.50 crore",
    "8.25 crore", "20 lakh", "50 lakh", "20 lakh", "1.5 crore", "10 crore", "20 lakh", "50 lakh", "10 crore", "20 lakh",
    "20 lakh", "2.6 crore", "10 crore", "5 crore", "3.8 crore", "50 lakh", "14 crore", "8.5 crore", "8 crore", "4 crore",
    "6.5 crore", "20 lakh", "3.4 crore", "5.5 crore", "7 crore", "11 crore", "20 lakh", "50 lakh", "95 lakh", "20 lakh",
    "1.8 crore", "7 crore", "70 lakh", "20 lakh", "15 crore", "20 lakh", "3.2 crore", "17.5 crore", "4 crore", "6.5 crore",
    "2.6 crore", "20 lakh", "4.2 crore", "50 lakh", "1.5 crore", "5.25 crore", "4.2 crore", "8.25 crore", "50 lakh",
    "20 lakh", "8.5 crore", "20 lakh", "2.4 crore", "3.2 crore", "4 crore", "25 lakh", "8.75 crore")
  
   )
)

data_top_players <- merge(data_top_players, salary_data, by = "Player")
# Check the column names of data_top_players
names(data_top_players)

# Check the column names of salary_data
names(salary_data)

# Check the data type of the "Player" column in data_top_players
class(data_top_players$Player)

# Check the data type of the "Player" column in salary_data
class(salary_data$Player)
t_test_result <- t.test(Salary ~ Player_Type, data = data_top_players)
