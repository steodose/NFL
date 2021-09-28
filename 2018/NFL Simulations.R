##### NFL Simulations 2018 #######
##### January 31, 2019 ###########
##### Stephan Teodosescu #########

library(tidyverse)
library(RCurl)
library(mosaic)
library(teamcolors)
library(ggrepel)
library(ggimage)
library(patchwork)

library(moderndive)
library(infer)

#### Question: Using simulation techniques, how did NFL's final 
#### regular season standings compare to what we originally predicted?

###### Load play by play data ######

# From Michael Lopez's blog post: https://statsbylopez.netlify.com/post/nfl-team-logos-using-ggimage/

# Load data from nflscrapR 
url.18 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
pbp <- read.csv(text = url.18)

# Load 2018 NFL schedule (from PFR)
schedule <- read_csv("2018_schedule.csv")

####### Power Ratings ########

# Determine Expected Points Added (will use this as a power rating 
# for each team)
EPA_off <- pbp %>%
  group_by(posteam) %>% 
  summarise(ave.epa = sum(epa, na.rm = TRUE)/16)

EPA_def <- pbp %>%
  group_by(defteam) %>% 
  summarise(ave.epa = -1*sum(epa, na.rm = TRUE)/16)

EPA <- EPA_off %>%
  left_join(EPA_def, by = c("posteam" = "defteam")) %>%
  rename("ave.epa.off" = "ave.epa.x", "ave.epa.def" = "ave.epa.y") %>% 
  mutate(Total_EPA = ave.epa.off + ave.epa.def) 

EPA <- EPA[order(-EPA$Total_EPA),]

#Filter for schedule
NFL_Schedule <- pbp %>%
  filter(game_seconds_remaining == 0) %>%
  select(game_id, game_date, home_team, away_team, total_home_score,
         total_away_score)

####### Determine win probabilities #######

#Final margin of victory for an NFL team in a given game can be approximated as a 
#normal random variable with a mean of the "spread" and a  
#standard deviation between 13-14 (using 13.45 based on the overall 
#NFL average from 1978-2012).

#See https://www.pro-football-reference.com/about/win_prob.htm

#### Using SRS #####
schedule <- read_csv("2018_Schedule.csv")

schedule <- schedule %>%
  mutate(H_Win_Prob = 1 - (pnorm(0.5, mean = schedule$Spread, sd = 13.45))) %>%
  mutate(Tie_Prob = pnorm(0.5, mean = schedule$Spread, sd = 13.45) - pnorm(-0.5, mean = schedule$Spread, sd = 13.45)) %>%
  mutate(Final_W_Prob = H_Win_Prob + 0.5*Tie_Prob) #Final win probability for home team

# New England's schedule
NE <- schedule %>%
  filter(Home == "New England Patriots" | Visitor == "New England Patriots") %>%
  mutate(Prob = ifelse(Visitor=="New England Patriots", 1-`Final_W_Prob`,
                       `Final_W_Prob`*1))

sim_NE <- rbinom(1000, 16, NE$Final_W_Prob) #Wins in a season

ggplot() + 
  geom_histogram(aes(sim_NE), binwidth = 1, color = "white") +
  labs(x = "Wins") +
  geom_vline(aes(xintercept=mean(sim_NE)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

# Write function to simulate NFL season 1,000x
set.seed(1234)

Simulate_Season <- function(x) {
  x <- data_frame() #creates empty data frame
  for (i in seq_along(schedule)) {
    x <- rbinom(1000, 8, schedule$Final_W_Prob)
    return(x)
  } 
}



home <- schedule %>%
  distinct(Home, Final_W_Prob)
home <- home[order(home$Home),] %>%
  group_by(Home) %>%
  summarise(Avg = mean(Final_W_Prob))

visitor <- schedule %>%
  distinct(Visitor, Final_W_Prob) %>%
  mutate(V_Final_W_Prob = 1-Final_W_Prob) %>%
  group_by(Visitor) %>%
  summarise(Avg = mean(V_Final_W_Prob))

visitor$Final_W_Prob <- NULL
visitor <- visitor[order(visitor$Visitor),]


rbinom(1000, 8, home$Final_W_Prob)
rbinom(1000, 8, visitor$V_Final_W_Prob)

#Loop through every game

