##### NFL 2024 Season Preview #####
##### August 2024 #####
##### By: Stephan Teodosescu #####

library(nflverse)
library(tidyverse)
library(gt)
library(gtExtras)
library(googlesheets4)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggrepel) # better labels
library(ggimage)
library(glue)
library(ggtext)
library(reactable)
library(reactablefmtr)
library(ggalt) #for dumbbell plot
library(ggforce)
library(ggsci)
library(prismatic)
library(rvest)
library(ggchicklet)
library(webshot2)
library(scales)
library(zoo)
library(grid)  # Ensure the grid package is loaded for the unit function



# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Outfit") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}


# create aspect ration to use throughout
asp_ratio <- 1.618

# Function for plot with logo generation
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




## ------------- 1. Strength of Schedule Smooth Facet --------------------

# load team logos and colors
team_df <- nflreadr::load_teams() %>% 
  select(team_logo_espn, team_abbr, team_name, team_conf, team_division, team_color)


#Load games data from Lee Sharpe/nflseedR
games <- load_sharpe_games() %>% 
  filter(season == 2024)

ratings <- read_csv('https://raw.githubusercontent.com/steodose/NFL/master/preseason_elo_2024.csv')

games2 <- games %>%
  left_join(ratings, by = c('home_team' = 'team')) %>%
  rename('home_elo' = 'elo') %>%
  left_join(ratings, by = c('away_team' = 'team')) %>%
  rename('away_elo' = 'elo') %>%
  select(home_team, home_elo, away_team, away_elo)

# schedule <- games2 %>%
#   mutate(teama = home_team,
#          teamb = away_team) %>% 
#   pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") %>% 
#   mutate(opp = ifelse(team == teama, teamb, teama)) %>% 
#   #rename(team = value) %>% 
#   select(-teama, -teamb, -home_away)

# pivot dataframe to get ech team-game in a single row
schedule <- games2 %>%
  mutate(teama = home_team,
         teamb = away_team,
         elo_teama = home_elo,
         elo_teamb = away_elo) %>% 
  pivot_longer(cols = c(home_team, away_team), names_to = "home_away", values_to = "team") %>%
  mutate(elo = ifelse(home_away == "home_team", home_elo, away_elo)) %>%
  mutate(opp = ifelse(team == teama, teamb, teama),
         opp_elo = ifelse(team == teama, elo_teamb, elo_teama)) %>%
  select(-teama, -teamb, -elo_teama, -elo_teamb, -home_away, -home_elo, -away_elo)

# calculate 5-game rolling average of opponent win total 
schedule <- schedule %>% 
  group_by(team) %>% 
  mutate(opp_elo_ra = rollmean(opp_elo, k = 5, na.pad = TRUE, align = 'right'), 
         gameno = row_number()) %>% 
  ungroup() 

# add team colors in 
schedule <- left_join(schedule, team_df, by = c("team" = "team_abbr"))

# set up duplicate team column for charting purposes 
schedule$teamDuplicate <- schedule$team 

# calculate the average opponent ELO for each team
team_avg_opp_elo <- schedule %>%
  group_by(team) %>%
  summarize(avg_opp_elo = mean(opp_elo, na.rm = TRUE))

# merge the average opponent ELO back into the schedule dataframe
schedule <- schedule %>%
  left_join(team_avg_opp_elo, by = "team")

# Make chart (smoothed version)
p1 <- schedule %>% 
  ggplot(aes(x = gameno, y = opp_elo)) + 
  #geom_smooth(data = mutate(schedule, team_abbr = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
  # geom_smooth(aes(group = team, color = team_color), method = "lm",  
  #             formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, 
  #             show.legend = FALSE, linetype = "dashed") +
  geom_line(data = mutate(schedule, team = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
  geom_line(aes(group = team, color = team_color), size = .5, alpha = 1, show.legend = FALSE) +
  # scale_y_continuous(breaks = seq(30, 50, 10)) +
  scale_x_continuous(breaks = seq(1, 18, 5), limits = c(1, 18, 5)) +
  scale_color_identity() +
  facet_wrap(~fct_reorder(team, -avg_opp_elo)) + # order be highest avg opp elo descending
  theme_custom() + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold',
                                  size = 16,
                                  hjust = 0.5),
        plot.subtitle = element_text(
          size = 8,
          hjust = 0.5),
        #plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6),  # Reduce x-axis tick label size
        axis.text.y = element_text(size = 6),
        panel.spacing = unit(0.5, 'lines')) +
  labs(x = "Week", 
       y = "Opponent Elo rating", 
       title = "NFL Schedule Difficulty, 2024", 
       subtitle = "Sorted by toughest schedule based on Elo rating of opponents.",
       caption = "Data: nflfastR | Plot: @steodosescu")


# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld <- ggplot_gtable(ggplot_build(p1))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
  p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
  id <- facet_id[i]
  url <-
    schedule %>% filter(team == !!id) %>% pull(team_logo_espn)
  lab <-
    grid::textGrob(
      id,
      x = unit(0, 'npc'),
      gp = grid::gpar(
        col = 'black',
        fontfamily = 'Outfit',
        fontface = 'bold',
        fontsize = 8
      ),
      hjust = 0
    )
  img <-
    grid::rasterGrob(
      image = magick::image_read(url),
      # just = 'right',
      hjust = 1,
      x = unit(1, 'npc'),
      ## 1 and 0.75 is also fine
      vp = grid::viewport(height = 1, width = 0.75)
    )
  tot_tree <- grid::grobTree(lab, img)
  p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}

p1 <- cowplot::ggdraw(p_bld)

ggsave("Opponent Elos Facet Smoothed.png", p1, w = 6, h = 6, dpi = 300)

# Add logo to plot
schedule_difficulty_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Opponent Elos Facet Smoothed.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(schedule_difficulty_with_logo, "Opponent Elos Facet Smoothed with Logo.png")





## ------------- 2. Vegas win totals bar chart --------------------

# URL of the Google Sheet where I have SB odds from various sportsbooks via the Odds API
sheet_url <- "https://docs.google.com/spreadsheets/d/1e06WaaQNhFs33uzQ6i3EY4AKtJmB8iizXd61iQqHxnk/edit?gid=62773379#gid=62773379"

# Name of the tab
tab_name <- "NFL Combined Odds"

# Read the specific tab into a dataframe
vegas_odds <- read_sheet(sheet_url, sheet = tab_name)


# scrape NFL win totals from vegas insider
viURL <- "https://www.vegasinsider.com/nfl/odds/win-totals/"

vi_raw <- viURL %>% 
  rvest:: read_html() %>% 
  rvest::html_nodes("#K2UOclcdAl6YCeHN , .page-footer .hydrated , .wp-block-list~ p+ .wp-block-list li") %>%
  rvest::html_text()

# collect into a tibble df for further processing
vi_clean <- vi_raw %>% 
  as_tibble()

# clean dataset further...extract into team and win total columns
vi_clean <- vi_clean %>% 
  extract(value, 
          into = c("team", "vegas_win_total"),
          # regex matching for any amount of consecutive non-digits at the start
          regex = "(^\\D+)(.*)", 
          convert = TRUE
  )

# trim white space in the team and win total columns
vi_clean$team <- str_trim(vi_clean$team)
vi_clean$vegas_win_total <- str_trim(vi_clean$vegas_win_total)

# fix Niners record
vi_clean <- vi_clean %>% 
  mutate(vegas_win_total = case_when(
    team == "San Francisco" ~ "10.5", #Niners expected to win 10.5 games as of Aug 4,2024 data pull
    TRUE ~ vegas_win_total
  ))

# convert type and calculate implied win %
vi_clean <- vi_clean %>% 
  type_convert() %>%
  mutate(implied_win_perc = vegas_win_total/17)

# fix Niners again
vi_clean <- vi_clean |> 
  mutate(team = case_when(
    team == "San Francisco" ~ "San Francisco 49ers",
    TRUE ~ team
  ))

# join in team logos
vi_clean <- vi_clean %>%
  left_join(team_df, by = c("team" = "team_name"))

vegas_combined <- vi_clean %>%
  left_join(vegas_odds) %>%
  filter(team_abbr != "LAR") # the Rams row has a duplicate bc of the join from team_df


# make bar plot
vegas_combined %>% 
  ggplot(aes(x = fct_rev(fct_reorder(team, vegas_win_total)), y = vegas_win_total)) +
  geom_col(aes(fill = team_color, 
               color = after_scale(clr_darken(fill, 0.3))
  ),
  width = 0.4, 
  alpha = .75,
  ) + 
  scale_color_identity(aesthetics = c("fill")) +
  geom_image(
    aes(
      image = team_logo_espn                                  
    ), 
    size = 0.035, 
    by = "width", 
    asp = asp_ratio
  ) +
  geom_text(aes(label = scales::percent(odds, accuracy = 1)), 
            vjust = -1.5, 
            size = 3, 
            family = "Outfit") +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 13)) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Vegas Win Total", 
       caption = "Data: nflverse/Bet365 | Plot: @steodosescu",
       title = glue("What does Vegas think?"),
       subtitle = glue("Vegas win totals and implied Super Bowl odds for 2024 season. Data as of August 4, 2024.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  )

ggsave("2024 Vegas Win Totals.png")

# Add logo to plot
win_totals_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2024 Vegas Win Totals.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(win_totals_with_logo, "2024 Vegas Win Totals with Logo.png")

