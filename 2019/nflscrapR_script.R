###### 2019 NFLscrapR - Ben Baldwin Tutorial ######
##### Aug 21, 2019 ###########
##### Stephan Teodosescu #########

library(tidyverse)
library(RCurl)
library(teamcolors)
library(ggrepel)
library(na.tools)
library(ggimage)
library(ggthemes)
library(extrafont)

# Source (Ben Baldwin - The Athletic): https://gist.github.com/guga31bb/5634562c5a2a7b1e9961ac9b6c568701
# nflscrapR: https://github.com/maksimhorowitz/nflscrapR

## Load data
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))
head(pbp)

## Exploratory Data Analysis 
pbp %>% 
    select(posteam, defteam, desc, play_type) %>% 
    head

# Filter for pass, run, or 'no play' plays only
pbp_rp <- pbp %>% 
    filter(!is_na(epa), play_type == "no_play" | play_type == "pass" | play_type == "run")

pbp_rp %>%
    select(posteam, defteam, desc, play_type) %>% 
    head

# Look at no plays
pbp_rp %>% 
    filter(play_type=="no_play") %>% 
    select(desc, rush_attempt, pass_attempt) %>% 
    head

# Create variables for pass, rush, and successful plays for those designated as no play (i.e. penalties, sacks, etc.)
pbp_rp <- pbp_rp %>%
    mutate(
        pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
        rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
        success = ifelse(epa > 0, 1 , 0)
    )    

#Keep only pass or rush plays
pbp_rp <- pbp_rp %>% 
    filter(pass == 1 | rush == 1)

## Chart data - dropback success rate vs EPA
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

chart_data <- pbp_rp %>%
  filter(pass==1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

chart <- chart_data %>% 
  left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "Dropback success rate & EPA/play",
       subtitle = "2018") +
  theme_bw()

## Rushing and passing EPA/play at the team level on 1st and 2nd down
chart_data <- pbp_rp %>%
  group_by(posteam) %>%
  filter(down<=2) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  )

chart <- chart_data %>% 
  left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
  ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass EPA/play",
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) #line with slope of 1

## Bring in abbreviated team names to join teamcolors dataset with nflscrapR data
nfl_colors <- teamcolors %>%
  filter(league == "nfl") %>%
  mutate(
    team_abb = case_when(
      name == "Arizona Cardinals" ~ "ARI",
      name == "Atlanta Falcons" ~ "ATL",
      name == "Baltimore Ravens" ~ "BAL",
      name == "Buffalo Bills" ~ "BUF",
      name == "Carolina Panthers" ~ "CAR",
      name == "Chicago Bears" ~ "CHI",
      name == "Cincinnati Bengals" ~ "CIN",
      name == "Cleveland Browns" ~ "CLE",
      name == "Dallas Cowboys" ~ "DAL",
      name == "Denver Broncos" ~ "DEN",
      name == "Detroit Lions" ~ "DET",
      name == "Green Bay Packers" ~ "GB",
      name == "Houston Texans" ~ "HOU",
      name == "Indianapolis Colts" ~ "IND",
      name == "Jacksonville Jaguars" ~ "JAX",
      name == "Kansas City Chiefs" ~ "KC",
      name == "Los Angeles Rams" ~ "LA",
      name == "Los Angeles Chargers" ~ "LAC",
      name == "Miami Dolphins" ~ "MIA",
      name == "Minnesota Vikings" ~ "MIN",
      name == "New England Patriots" ~ "NE",
      name == "New Orleans Saints" ~ "NO",
      name == "New York Giants" ~ "NYG",
      name == "New York Jets" ~ "NYJ",
      name == "Oakland Raiders" ~ "OAK",
      name == "Philadelphia Eagles" ~ "PHI",
      name == "Pittsburgh Steelers" ~ "PIT",
      name == "Seattle Seahawks" ~ "SEA",
      name == "San Francisco 49ers" ~ "SF",
      name == "Tampa Bay Buccaneers" ~ "TB",
      name == "Tennessee Titans" ~ "TEN",
      name == "Washington Redskins" ~ "WAS",
      TRUE ~ NA_character_
    ),
    posteam = team_abb
  )

## More color work per Thomas Mock's tutorial
pbp_colors <- left_join(pbp, nfl_colors, by = c("posteam"))

pbp_colors %>%
  # Excludes non-plays, eg end of quarter
  filter(!is.na(posteam)) %>%
  select(posteam, team_abb, name, primary, secondary) %>%
  # Distinct grabs only the distinct/unique cases of column
  distinct(posteam, .keep_all = TRUE)

##Investigate EPA more
epa_play <- pbp_rp %>% #similar to chart data data frame
  filter(pass == 1) %>% 
  group_by(posteam) %>% 
  summarize(
    n = n(),
    epa_per_db = sum(epa, na.rm = TRUE) / n,
    success_rate = sum(epa) / n
  )


#Labeled bar chart
epa_play %>%
  ggplot(aes(x = reorder(posteam, epa_per_db), y = epa_per_db)) +
  geom_col(aes(fill = if_else(epa_per_db >= 0, "#2c7bb6", "#d7181c"))) +
  geom_text(aes(
    label = posteam,
    color = if_else(epa_per_db >= 0, "#2c7bb6", "#d7181c"),
    hjust = if_else(epa_per_db > 0, -0.1, 1.1)
  )) +
  coord_flip() +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
 theme(panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "",
    y = "EPA per Dropback",
    title = "Chiefs and Saints led the way in EPA/dropback",
    subtitle = "2018 NFL regular season",
    caption = "Data: @nflscrapR"
  )

#Number of rush vs. pass plays on every down (by team). 
chart_data2 <- pbp_rp %>%
  filter(down<=4) %>%
  group_by(posteam, down) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    percent_pass = sum(n_dropbacks)/(n_dropbacks + n_rush),
    percent_run = sum(n_rush)/(n_dropbacks + n_rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush) %>%
  mutate(delta = percent_pass - percent_run)

# Make chart 
