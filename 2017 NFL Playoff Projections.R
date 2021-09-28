## 2017 NFL Playoff Predictions

#Load necessary packages
library(tidyverse)
library(reshape2)
library(modelr)

NFL_2017 <- read_csv("2017 NFL Playoffs.csv") #Load 2017 data (testing set)
View(NFL_2017)

NFL_SRS <- read_csv("NFL SRS Ratings.csv") #Load 3 PY data (training set)
View(NFL_SRS)

#Build logistic regression model for Super Bowl winner
LogModel_SuperBowl <- glm(`Super Bowl` ~ SRS, data = NFL_SRS,family = "binomial")
summary(LogModel_SuperBowl)

#Add predictions for 2017 season
SB_Predictions <- predict(LogModel_SuperBowl, type = "response", 
                          newdata = NFL_2017)

Probs <- tibble(SB_Predictions) %>% 
  mutate(Probabilities = `SB_Predictions`*100) %>%
  left_join(NFL_2017, by = "Team")

Probs$Team <- NFL_2017$Team #Add the "Team" column to dataset
Probs$SRS <- NFL_2017$SRS #Add SRS of each team to dataset

#Data Viz

top_teams <- Probs %>% #For annotations
  filter(SRS > 9.3)

ggplot(Probs, aes(SRS, Probabilities)) +
  geom_point(aes(color = Team)) +
  geom_smooth() +
  geom_text(aes(label = Team), data = top_teams, nudge_x = -4) + #Annotate
  theme(legend.position = "none") +
  labs(
    title = "Higher SRS means higher probability of winning the Super Bowl",
    caption = "Data from pro-football-reference.com"
    )
  
#Write to CSV file to manually input which teams made playoffs in 2017
write_csv(Probs, "Probs_v2.csv")

Probs2 <- read_csv("Probs_v2.csv")
View(Probs2)

Probs2 <- Probs2 %>%
  mutate(
    Adjusted_Probability = Probabilities * 1.137 * Playoffs #Recalibrating probabilities for playoff teams only
  ) 

#Re-do Data Viz
top_teams <- Probs2 %>% #For annotations
  filter(SRS > 9.3)

Probs2 %>%
  filter(Playoffs == 1) %>%
  ggplot(aes(SRS, Adjusted_Probability)) +
  geom_point(aes(color = Team)) +
  geom_smooth() +
  geom_text(aes(label = Team), data = top_teams, nudge_x = -2.5) + #Annotate
  theme(legend.position = "none") +
  labs(
    y = "Probability (%)",
    title = "Higher SRS, Higher Probability of Super Bowl Victory",
    caption = "Data from pro-football-reference.com"
  )

