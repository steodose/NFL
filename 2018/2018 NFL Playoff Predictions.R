###### 2018 NFL Playoff Predictions ######

library(tidyverse)
library(RCurl)
library(mosaic)
library(teamcolors)
library(ggrepel)
library(ggimage)
library(patchwork)

##### Load all necessary data ---------------------------------------

# From Michael Lopez's blog post: https://statsbylopez.netlify.com/post/nfl-team-logos-using-ggimage/
url.18 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
pbp <- read.csv(text = url.18)

# Load data (from Pro Football Reference)
NFL_2018 <- read_csv("SRS_2018.csv")
View(NFL_2018)

# Load 2018 playoffs schedule
schedule <- read_csv("2018_schedule.csv")
View(schedule)

####### Determine win probabilities #######

#Final margin of victory for an NFL team in a given game can be approximated as a 
#normal random variable with a mean of the "spread" and a  
#standard deviation between 13-14 (using 13.45 based on the overall 
#NFL average from 1978-2012).

#See https://www.pro-football-reference.com/about/win_prob.htm

schedule <- schedule %>%
  mutate(Difference = `Home SRS` - `Visitor SRS`, 
         Adj_Diff = Difference + 2.5) %>%
  mutate(H_Win_Prob = 1 - (pnorm(0.5, mean = schedule$Adj_Diff, sd = 13.45))) %>%
  mutate(Tie_Prob = pnorm(0.5, mean = schedule$Adj_Diff, sd = 13.45) - pnorm(-0.5, mean = schedule$Adj_Diff, sd = 13.45)) %>%
  mutate(Final_W_Prob = H_Win_Prob + 0.5*Tie_Prob) #Final win probability for home team

######## Data using nflscrapR ########

#Filter for plays from scrimmage (no special teams or penalties)
scrimmage.plays <- pbp %>% 
  filter(play_type == "pass"|play_type == "run")

#Create summary
scrimmage.plays.summary <- scrimmage.plays %>% 
  group_by(posteam, play_type) %>% 
  summarise(ave.epa = mean(epa, na.rm = TRUE)) %>%
  spread(play_type, ave.epa)

#Create summaries for offensive and defensive teams (scrimmage plays)
scrimmage.plays.summary.off <- pbp %>% 
  filter(play_type == "pass"|play_type == "run")
group_by(posteam, game_id) %>% 
  summarise(off.epa = sum(epa, na.rm = TRUE))

scrimmage.plays.summary.def <- pbp %>% 
  filter(play_type == "pass"|play_type == "run") %>%
  group_by(defteam, game_id) %>% 
  summarise(def.epa = sum(epa, na.rm = TRUE))

scrimmage.plays2 <- full_join(scrimmage.plays.summary.off, scrimmage.plays.summary.def,
                              by = c("posteam" = "defteam")) %>%
  rename("Team" = "posteam")
scrimmage.plays2$game_id.x <- NULL
scrimmage.plays2$game_id.y <- NULL

bound.label <- 0.38
df.text <- data.frame(lab.text = c("Run +, Pass -", "Run +, Pass +", 
                                   "Run -, Pass +", "Run -, Pass -"), 
                      x = c(bound.label, bound.label, -1*bound.label, 
                            -1*bound.label), 
                      y = c(-1*bound.label, bound.label, bound.label, -1*bound.label))

########## Make visualizations ############

# Add in NFL team logos
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)
scrimmage.plays.summary <- scrimmage.plays.summary %>% 
  left_join(df.logos, by = c("posteam" = "team_code"))

# Make ggplot image with team logos (Offensive EPA run vs. pass)
ggplot(scrimmage.plays.summary, aes(run, pass, label = posteam)) + 
  geom_image(aes(image = url), size = 0.06) +
  xlab("Run: Average Offensive EPA") + 
  ylab("Pass: Average Offensive EPA") + 
  labs(title = "Expected Points Added by Runs and Pass Plays", subtitle = "2018 NFL Regular Season", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  xlim(c(-0.45, 0.45)) + ylim(c(-0.45, 0.45)) + 
  geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red")

# Load team colors 
teamcolors1 <- teamcolors %>% 
  filter(league == "nfl") %>% 
  rename(Team = name) %>% 
  mutate(rand.color = ifelse(primary == "#010101", secondary, primary))

# SRS Power Rankings Data visualization
ggplot(data = NFL_2018, mapping = aes(x = reorder(Tm, SRS), 
                                            y = SRS, fill = Tm)) + 
  geom_bar(color ="black", stat = "identity") +
  labs(y = "Simple Rating System (SRS)", x = "Team", caption = "Data from Pro-Football-Reference.com", 
       title = "NFL 2018 Regular Season", subtitle = 
         "Points favored vs. league average \n opponent at neutral site") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_fill_manual(name = NULL, values = teamcolors1$primary)

# Expected Points Added
EPA.off <- pbp %>%
  group_by(posteam) %>% 
  summarise(ave.epa = sum(epa, na.rm = TRUE)/17)

EPA.def <- pbp %>%
  group_by(defteam) %>% 
  summarise(ave.epa = sum(epa, na.rm = TRUE)/17)

EPA <- EPA.off %>%
  left_join(EPA.def, by = c("posteam" = "defteam")) %>%
  rename("ave.epa.off" = "ave.epa.x", "ave.epa.def" = "ave.epa.y")

ggplot(EPA, aes(ave.epa.off, ave.epa.def, label = posteam)) + 
  geom_point(alpha = 1/2) +
  geom_text_repel() + 
  xlab("Average Offensive EPA") + 
  ylab("Average Defensive EPA") + 
  labs(title = "Expected Points Added", subtitle = "2018 NFL Regular Season", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) +
  annotate("text", x = 14, y = -6, color = "red", label = "Good") +
  annotate("text", x = -10, y = 8, color = "red", label = "Bad") +
  annotate("text", x = 14, y = 8, color = "red", label = "Fun") +
  annotate("text", x = -10, y = -6, color = "red", label = "Dull")
  
#Remake ggplot image with team logos
EPA <- EPA %>% 
  left_join(df.logos, by = c("posteam" = "team_code"))

ggplot(EPA, aes(ave.epa.off, ave.epa.def, label = posteam)) + 
  geom_image(aes(image = url), size = 0.07) +
  xlab("Average Offensive EPA per game") + 
  ylab("Average Defensive EPA per game") + 
  labs(title = "Expected Points Added", subtitle = "2018 NFL Regular Season", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) +
  annotate("text", x = 14, y = -6, color = "red", label = "Good") +
  annotate("text", x = -10, y = 8, color = "red", label = "Bad") +
  annotate("text", x = 14, y = 8, color = "red", label = "Fun") +
  annotate("text", x = -10, y = -6, color = "red", label = "Dull")

###### Play type analysis ######
scrimmage.plays.2 <- scrimmage.plays %>% 
  group_by(posteam, play_type) %>% 
  count(play_type) %>%
  spread(play_type, n) %>%
  mutate(pass_share = pass/(pass + run)) %>%
  mutate(run_share = run/(pass + run)) %>%
  left_join(df.logos, by = c("posteam" = "team_code"))

#Visualize run vs. pass
ggplot(data = scrimmage.plays.2, mapping = aes(x = reorder(team, pass_share), 
                                      y = pass_share, fill = team)) + 
  geom_bar(color ="black", stat = "identity") +
  labs(y = "Share of Plays (Pass)", x = "Team", caption = "Data Source: nflscrapR", 
       title = "NFL 2018 Regular Season", subtitle = 
         "Pass Plays as a Share of Total Plays") +
  guides(fill = FALSE) +
  scale_fill_manual(name = NULL, values = teamcolors1$primary) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_hline(aes(yintercept = 0.5), lty = 2, col = "red", alpha = 0.5)

###### Play call sequencing #######
scrimmage.plays.downs <- scrimmage.plays %>% 
  group_by(down, play_type) %>% 
  count(play_type) %>%
  spread(play_type, n) %>%
  mutate(pass_share = pass/(pass + run)) %>%
  mutate(run_share = run/(pass + run)) 

######### Playoff Simulations ##########

# New England Patriots
NE <- schedule %>%
  filter(Home == "New England Patriots" | Visitor == "New England Patriots") %>%
  mutate(Prob = ifelse(Visitor=="New England Patriots", 1-`Final_W_Prob`,
                       `Final_W_Prob`*1))

sim_NE <- rbinom(1000, 3, NE$Prob)
mean(sim_NE == 3) * 100 #Percent chance of winning Super Bowl given schedule

# Los Angeles Rams
LAR <- schedule %>%
  filter(Home == "Los Angeles Rams" | Visitor == "Los Angeles Rams") %>%
  mutate(Prob = ifelse(Visitor=="Los Angeles Rams", 1-`Final_W_Prob`,
                       `Final_W_Prob`*1))

sim_LAR <- rbinom(1000, 3, LAR$Prob)
mean(sim_LAR == 3) * 100 #Percent chance of winning Super Bowl given schedule


