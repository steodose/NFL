##NFL Expected Points Added

#Load packages
library(tidyverse)
library(RCurl)
library(mosaic)
library(teamcolors)
library(ggrepel)
library(ggimage)

#From Michael Lopez's blog post: https://statsbylopez.netlify.com/post/nfl-team-logos-using-ggimage/
url.18 <- getURL("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/regular_season/reg_pbp_2018.csv")
pbp <- read.csv(text = url.18)

head(pbp)

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

########## Make visualizations: 
ggplot(scrimmage.plays.summary, aes(run, pass, label = posteam)) + 
  geom_point() + 
  geom_text_repel() + 
  xlab("Run: Average Offensive EPA") + 
  ylab("Pass: Average Offensive EPA") + 
  labs(title = "Expected Points Added by Runs and Pass Plays", subtitle = "Through Week 7 of 2018 NFL Season", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  xlim(c(-0.45, 0.45)) + ylim(c(-0.45, 0.45)) + 
  geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red")

#Add in NFL team logos
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)
scrimmage.plays.summary <- scrimmage.plays.summary %>% 
  left_join(df.logos, by = c("posteam" = "team_code"))

#Remake ggplot image with team logos
ggplot(scrimmage.plays.summary, aes(run, pass, label = posteam)) + 
  geom_image(aes(image = url), size = 0.06) +
  xlab("Run: Average Offensive EPA") + 
  ylab("Pass: Average Offensive EPA") + 
  labs(title = "Expected Points Added by Runs and Pass Plays", subtitle = "Through Week 7 of 2018 NFL Season", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  xlim(c(-0.45, 0.45)) + ylim(c(-0.45, 0.45)) + 
  geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red")

######Plot of Expected Points Added per game (Not working yet)
ggplot(scrimmage.plays.summary2, aes(posteam, defteam)) + 
  geom_point() + 
  geom_text_repel() + 
  xlab("Run: Offensive Expected Points Added") + 
  ylab("Pass: Defensive Points Added") + 
  labs(title = "Assessing NFL Team Performance 2018", subtitle = "Through Week 7; Estimates a play’s value based on
the change in situation.", 
       caption = "Data Source: nflscrapR") + 
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5)

###### Win Probability chart: Week 6 Chicago vs. Miami

# Pull out the Bears and Dolphins colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
chi_color <- nfl_teamcolors %>%
  filter(name == "Chicago Bears") %>%
  pull(primary)
mia_color <- nfl_teamcolors %>%
  filter(name == "Miami Dolphins") %>%
  pull(primary)

pbp %>%
  filter(game_id == 2018101404) %>%
  filter(!is.na(home_wp), !is.na(away_wp)) %>%
  select(game_seconds_remaining, home_wp, away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("CHI", "MIA"),
                     values = c(chi_color, mia_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .90, label = "MIA", color = mia_color, size = 8) + 
  annotate("text", x = 3000, y = .12, label = "CHI", color = chi_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(x = "Time Remaining (seconds)", y = "Win Probability",
    title = "Week 6 Win Probability Chart",
    subtitle = "Chicago Bears vs. Miami Dolphins",
    caption = "Data from nflscrapR")

######