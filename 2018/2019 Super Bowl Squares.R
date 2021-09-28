######## Super Bowl Squares 2018 ##########
######## February 2, 2019 #################
######## Stephan Teodosescu ###############

# Load 2015-2018 NFL regular season and postseason results
games <- read_csv("2015-2018_results.csv") #on local disk
 
# Gather into one data frame of scores, and calculate the digit
scores <- games %>%
  mutate(digitW = PtsW %% 10, digitL = PtsL %% 10)

scores %>%
  count(digitW, digitL) %>%
  mutate(percent = n / sum(n))

# Calculate frequency of certain scores
scores %>%
  count(PtsW, sort = TRUE)

scores %>%
  count(PtsL, sort = TRUE)

scores %>%
  count(PtsW, PtsL, sort = TRUE)

#Plot most common digits

#Plot Histogram of most common winning numbers
ggplot(data = scores, mapping = aes(x = digitW)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  labs( x = "Winning Team's Final Digit",
        caption = "Data from Pro-Football-Reference.com")

#Histogram for losing numbers
ggplot(data = scores, mapping = aes(x = digitL)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  labs( x = "Losing Team's Final Digit",
        caption = "Data from Pro-Football-Reference.com")

# Make heat map
scores %>%
  count(digitW, digitL) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(digitW, digitL, fill = percent)) +
  geom_tile() +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = 0:9) +
  geom_text(aes(label = round(percent*100, 3))) +
  scale_fill_gradient2(high = "red", low = "white",
                       labels = scales::percent_format()) +
  theme_minimal() +
  labs(x = "Last digit of winning team score",
       y = "Last digit of losing team score",
       title = "Common pairs of NFL scores 2015-2018",
       fill = "% of games in '15-18")


