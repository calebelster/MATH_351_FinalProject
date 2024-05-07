# Step 1: Load necessary libraries
library(dplyr)   # For data manipulation

# Step 2: Read the CSV data
nba_data_salary <- read.csv("./FinalProject/nba_salary_data_2023_24.csv")
nba_data_talent <- read.csv("./FinalProject/DARKO_player_talent_2024-05-01.csv")

# Step 3: Merge datasets based on player names
nba_data_salary$Salary <- as.numeric(gsub("\\$", "", nba_data_salary$Salary))
merged_data <- merge(nba_data_salary, nba_data_talent, by.x = "Player", by.y = "Player", all = TRUE)

# Step 4: Filter out players with missing salary, O-DPM, or D-DPM stats
filtered_data <- merged_data %>%
  filter(!is.na(Salary) & !is.na(`O.DPM`) & !is.na(`D.DPM`))

# Step 5: Define thresholds for good offensive and defensive stats
offensive_threshold <- 0  # O-DPM above 0 for good offensive player
defensive_threshold <- 0  # D-DPM above 0 for good defensive player

# Step 6: Select players with good offensive and defensive stats
good_offensive_players <- filtered_data %>% filter(`O.DPM` > offensive_threshold)
good_defensive_players <- filtered_data %>% filter(`D.DPM` > defensive_threshold)

# Step 7: Calculate population and sample statistics for both categories
offensive_mean <- mean(good_offensive_players$Salary)
defensive_mean <- mean(good_defensive_players$Salary)

offensive_pop_std <- sd(good_offensive_players$Salary)
defensive_pop_std <- sd(good_defensive_players$Salary)

offensive_sample_std <- sd(good_offensive_players$Salary) * sqrt((length(good_offensive_players$Salary) - 1) / length(good_offensive_players$Salary))
defensive_sample_std <- sd(good_defensive_players$Salary) * sqrt((length(good_defensive_players$Salary) - 1) / length(good_defensive_players$Salary))

offensive_count <- nrow(good_offensive_players)
defensive_count <- nrow(good_defensive_players)

# Step 6: Create dotplots for offensive and defensive player salaries
par(mfrow=c(1,2), mar=c(5, 5, 2, 2)) # Set up a multi-pane plot with 1 row and 2 columns, adjust margins

# Dotplot for offensive player salaries
plot(good_offensive_players$Salary/1e6, good_offensive_players$`O.DPM`,
     main = "Salaries vs O-DPM for Offensive Players",
     xlab = "Salary (Millions)",
     ylab = "O-DPM",
     col = "skyblue")

# Dotplot for defensive player salaries
plot(good_defensive_players$Salary/1e6, good_defensive_players$`D.DPM`,
     main = "Salaries vs D-DPM for Defensive Players",
     xlab = "Salary (Millions)",
     ylab = "D-DPM",
     col = "lightgreen")

# Step 6: Create boxplots for offensive and defensive player salaries
par(mfrow=c(1,2), mar=c(5, 5, 2, 2)) # Set up a multi-pane plot with 1 row and 2 columns, adjust margins

# Boxplot for offensive player salaries
boxplot(good_offensive_players$Salary/1e6, main = "Salaries for Offensive Players",
        col = "skyblue", border = "black", horizontal = TRUE, xlab = "Salary (Millions)", ylab = "")

# Boxplot for defensive player salaries
boxplot(good_defensive_players$Salary/1e6, main = "Salaries for Defensive Players",
        col = "lightgreen", border = "black", horizontal = TRUE, xlab = "Salary (Millions)", ylab = "")

# Step 8: Choose a Statistical Test
# Assuming data is normally distributed (you can check this assumption)
# Perform a t-test for independent samples

# Step 9: Perform the Hypothesis Test
t_test_result <- t.test(good_offensive_players$Salary, good_defensive_players$Salary)

# Step 10: Interpret the Results
t_test_result

# Step 11: Print calculated statistics
cat("Offensive Players Mean Salary:", offensive_mean, "\n")
cat("Defensive Players Mean Salary:", defensive_mean, "\n")
cat("Offensive Players Population Standard Deviation:", offensive_pop_std, "\n")
cat("Defensive Players Population Standard Deviation:", defensive_pop_std, "\n")
cat("Offensive Players Sample Standard Deviation:", offensive_sample_std, "\n")
cat("Defensive Players Sample Standard Deviation:", defensive_sample_std, "\n")
cat("Number of Offensive Players:", offensive_count, "\n")
cat("Number of Defensive Players:", defensive_count, "\n")
