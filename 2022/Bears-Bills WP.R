##### NFL Win Probability Charts #####
##### By: Stephan Teodosescu #####
##### December 2022 #####

library(tidyverse)
library(glue)
library(nflfastR)
library(nflseedR)
library(teamcolors)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggimage)
library(animation)
library(DBI)
library(RSQLite)
library(glue)
library(ggtext)
library(patchwork)


###### Create themes to use throughout #####
# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

##### Recreate plots with NFL logo #####

# Function for logo generation

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}



# First load play by play data from nflfastR for the 2021 season
pbp_data <- load_pbp(2022)

pbp_wp <- pbp_data %>% 
  filter(!is.na(home_wp), !is.na(away_wp)) %>%
  group_by(game_id) %>% 
  mutate(win_prob_change = wpa - lag(wpa)) %>% #redundant so won't use win_prob_change going forward
  relocate(win_prob_change) %>% 
  drop_na(win_prob_change) #omit rows that have NA WP

## Normalize for length of games

# filter for OT games and calculate how long each one went
ot_games <- pbp_wp %>% 
  filter(qtr == 5) %>% 
  group_by(game_id) %>% 
  summarise(ot_length = max(game_seconds_remaining) - min(game_seconds_remaining))

#calculate raw GEI
games_gei <- pbp_wp %>%
  group_by(game_id) %>% 
  summarise(gei = round(sum(abs(wpa)),2)) %>% #this is how I'm calculating GEI
  relocate(gei)

# now join in OT game lengths to calculate GEIs normalized for game length
games_gei <- games_gei %>% 
  left_join(ot_games) %>% 
  mutate(game_length = 3600 + ot_length, 
         game_length = replace_na(game_length, 3600),
         normalization = 3600/game_length,
         gei = round(gei*normalization, digits = 2))

#Load Lee Sharpe's games data 
games <- load_sharpe_games() %>% 
  filter(season == 2022)

games <- games %>% 
  select(game_id, game_type, gameday, away_team:home_score, spread_line, 
         away_spread_odds, home_spread_odds,away_qb_name, home_qb_name, stadium)

# join datasets together
games <- left_join(games, games_gei, by = "game_id")

# team logos
team_logos <- nflfastR::teams_colors_logos


##### Win Probability Data and Plots #####

## Bears vs Bills Dec 24

game_data <- pbp_data %>%
  filter(game_id == "2022_16_BUF_CHI")

# Build in logic to deal with OT timing of Bills-Chiefs for x-axis
min_sec <- game_data %>%
  filter(qtr == 5 & !is.na(game_seconds_remaining)) %>%
  pull(game_seconds_remaining) %>%
  min()

max_sec <- game_data %>%
  filter(qtr == 5 & !is.na(game_seconds_remaining)) %>%
  pull(game_seconds_remaining) %>%
  max()

game_data <- game_data %>% 
  mutate(game_seconds_remaining = 
           if_else(qtr == 5, -1*(max_sec - game_seconds_remaining), 
                   game_seconds_remaining))





# Get home_team and away_team and annotate on chart
home_team_abbr <- game_data[1, ]$home_team
away_team_abbr <- game_data[1, ]$away_team

# Build a data frame with coordinates of team logo to place on chart
logo_placement_data <- data.frame(
  x = c(3600, 3600),
  y = c(0.875, 0.125),
  team_abbr = c(home_team_abbr, away_team_abbr),
  stringsAsFactors = FALSE) %>% 
  inner_join(team_logos, by = "team_abbr")


single_game_id <- game_data[1, ]$game_id

game_title_pieces <- strsplit(single_game_id, "_")[[1]]
game_year <- game_title_pieces[1]
game_week <- game_title_pieces[2]

# Compute Game Excitement Index
wp_gei <- games %>%
  filter(game_id == "2022_16_BUF_CHI") %>% 
  pull(gei)



#create WP plot
wp_plot <- game_data %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa)) +
  geom_line(size = 1.4) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  geom_image(
    data = logo_placement_data,
    aes(x = x, y = y, image = team_logo_espn),
    size = 0.08,
    asp = 16 / 9
  ) +
  # scale_color_manual(labels = c("SF", "GB"),
  #                    values = c(la_color, tb_color),
  #                    guide = FALSE) +
  #scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "",
    y = "Win Probability",
    title = str_interp(
      "${game_year} Week ${game_week}: ${away_team_abbr} at ${home_team_abbr}"
    ),
    subtitle = glue("Game Excitment Index (GEI): {wp_gei}")
  ) + 
  theme_custom() +
  theme(plot.title = element_text(face="bold")) +
  scale_x_continuous(
    trans = "reverse",
    breaks = c(2700, 1800, 900, 0), 
    limits = c(3700, 0),
    labels = c("END\nQ1", "HALF\nTIME", "END\nQ3", "END\nQ4")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  theme(plot.title = element_markdown()) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_markdown())

wp_plot

ggsave("Bears-Bills.png")

# Add logo to plot
WP_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL-win-probability/Bears-Bills.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(WP_plot_with_logo, "Bears-Bills with Logo.png")