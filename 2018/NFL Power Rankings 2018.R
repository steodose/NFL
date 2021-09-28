#NFL Power Rankings 2018 & Weekly Win Probabilites

#Load packages
library(tidyverse)
library(lubridate)
library(stringr)
library(mosaic)
library(teamcolors)

#Set working directory

#Load 2018 Vegas spread data (Adjust for H/A and constant)
Lines <- read_csv("2018_Lines.csv") %>%
  mutate(V_Adj = `Visitor Rating` + 2.5, H_Adj = `Home Rating` - 2.5) %>%
  mutate(Adjustment_Factor = 1/((11 - Lines$Week) + 0.4)) %>%
  mutate(`Visitor Rating2` = `V_Adj`*`Adjustment_Factor`,
         `Home Rating2` = `H_Adj`*`Adjustment_Factor`)

#Spreads for visiting teams
Lines2 <- Lines %>%
  group_by(Visitor) %>%
  summarise(Visitor_Spread = mean(`Visitor Rating2`)) 

#Spreads for home teams
Lines3 <- Lines %>%
  group_by(Home) %>%
  summarise(Home_Spread = mean(`Home Rating2`))

#Merge home and away spreads for each team into a new data frame
Power_Rankings <- full_join(Lines2, Lines3, by = c("Visitor" = "Home")) %>%
  mutate(Points_Favored = (Visitor_Spread + Home_Spread)/2) %>%
  rename("Team" = "Visitor")

#Order by best to worst teams
Power_Rankings <- Power_Rankings[order(-Power_Rankings$Points_Favored),]

#Current Week probabilities (don't forget to change Week numbers in here)
Week11 <- read_csv("2018_Lines.csv") %>%
  filter(Week == 11) %>%
  inner_join(Power_Rankings, Week10, by = c("Visitor" = "Team")) %>%
  inner_join(Power_Rankings, Week10, by = c("Home" = "Team")) %>%
  select(Visitor, Home, Points_Favored.x, Points_Favored.y) %>%
  rename("Visitor_Spread" = "Points_Favored.x", "Home_Spread" = "Points_Favored.y") %>%
  mutate(HFA_Spread = Home_Spread + 2) %>% 
  mutate(Difference = HFA_Spread - Visitor_Spread) %>%
  mutate(H_Win_Prob = 1 - (pnorm(0.5, mean = Week11$Difference, sd = 13.45))) %>%
  mutate(Tie_Prob = pnorm(0.5, mean = Week11$Difference, sd = 13.45) - pnorm(-0.5, mean = Week11$Difference, sd = 13.45)) %>%
  mutate(Final_W_Prob = H_Win_Prob + 0.5*Tie_Prob) #Final win probability for home team


##final margin of victory for an NFL team in a given game can be approximated as a 
#normal random variable with a mean of the "spread" and a  
#standard deviation between 13-14 (using 13.45 based on the overall 
#NFL average from 1978-2012).

#See https://www.pro-football-reference.com/about/win_prob.htm

#Load team colors 
teamcolors1 <- teamcolors %>% 
  filter(league == "nfl") %>% 
  rename(Team = name) %>% 
  mutate(rand.color = ifelse(primary == "#010101", secondary, primary))

#Power Rankings Data visualization
ggplot(data = Power_Rankings, mapping = aes(x = reorder(Team, Points_Favored), 
                                            y = Points_Favored, fill = Team)) + 
  geom_bar(color ="black", stat = "identity") +
  labs(y = "Points Favored", x = "Team", caption = "Data from ESPN.com", 
       title = "NFL Power Rankings Through Week 11", subtitle = 
         "Points favored vs. average opponent at neutral site") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_fill_manual(name = NULL, values = teamcolors1$primary)
