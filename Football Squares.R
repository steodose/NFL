## Super Bowl Squares

#Load necessary packages
library(tidyverse)

#Load data
Football_Squares <- read_csv("Football Squares.csv")
View(Football_Squares)

#Plot Histogram of most common winning numbers
ggplot(data = Football_Squares, mapping = aes(x = `Winning No.`)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_y_continuous(breaks = seq(0,9, by = 1)) +
  labs( x = "Winning Team's Final Digit",
        caption = "Data from espn.com")

#Histogram for losing numbers
ggplot(data = Football_Squares, mapping = aes(x = `Losing No.`)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_y_continuous(breaks = seq(0,18, by = 1)) +
  labs( x = "Losing Team's Final Digit",
        caption = "Data from espn.com")

#Load 2015-2017 NFL scores data
NFL <- read_csv("2015-2017 NFL Scores.csv")
View(NFL)

#Plot Histogram of most common winning numbers for 2015 - 2017 data
NFL_winners <- NFL %>%
  filter(result1 == 1)
ggplot(data = NFL_winners, mapping = aes(x = score1_digit)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  labs( x = "Winning Team's Final Digit",
        caption = "Data from fivethirtyeight.com")

#Plot Histogram of most common losing numbers for 2015 - 2017 data
NFL_losers <- NFL %>%
  filter(result1 == 0)
ggplot(data = NFL_losers, mapping = aes(x = score2_digit)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_y_continuous(breaks = seq(0,100, by = 10)) +
  labs( x = "Losing Team's Final Digit",
        caption = "Data from fivethirtyeight.com")

#Counts
count(NFL_winners, score1_digit == 0)
count(NFL_winners, score1_digit == 1)
count(NFL_winners, score1_digit == 2)
count(NFL_winners, score1_digit == 3)
count(NFL_winners, score1_digit == 4)
count(NFL_winners, score1_digit == 5)
count(NFL_winners, score1_digit == 6)
count(NFL_winners, score1_digit == 7)
count(NFL_winners, score1_digit == 8)
count(NFL_winners, score1_digit == 9)

count(NFL_losers, score2_digit == 0)
count(NFL_losers, score2_digit == 1)
count(NFL_losers, score2_digit == 2)
count(NFL_losers, score2_digit == 3)
count(NFL_losers, score2_digit == 4)
count(NFL_losers, score2_digit == 5)
count(NFL_losers, score2_digit == 6)
count(NFL_losers, score2_digit == 7)
count(NFL_losers, score2_digit == 8)
count(NFL_losers, score2_digit == 9)