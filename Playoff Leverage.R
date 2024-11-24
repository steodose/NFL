##### NFL Playoff Leverage Plots #####
##### Novemner 2024 #####
##### By: Stephan Teodosescu #####

library(nflverse)
library(tidyverse)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggimage)
library(glue)
library(ggtext)
library(webshot2)
library(scales)


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




## ------------- 1. Playoff Leverage Plots --------------------

# load team logos and colors
team_df <- nflreadr::load_teams() %>% 
    select(team_logo_espn, team_abbr, team_name, team_conf, team_division, team_color)


# load file from Neil Paine's playoff leverage article: https://neilpaine.substack.com/p/football-bytes-the-detroit-lions
playoff_odds <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/W12_playoff_odds_paine.csv') %>%
    janitor::clean_names() %>%
    mutate(now = str_remove(now, "%") %>% 
               as.numeric() / 100,
           w_w = str_remove(w_w, "%") %>% 
               as.numeric() / 100,
           w_l = str_remove(w_l, "%") %>% 
               as.numeric() / 100,
           wtd_chg = str_remove(wtd_chg, "%") %>% 
               as.numeric() / 100,
           )

# calculate raw difference in playoff probability outcomes
playoff_odds <- playoff_odds %>%
    mutate(leverage_win = now + w_w,
           leverage_loss = now + w_l)

# join in colors and logos
playoff_odds_joined <- playoff_odds %>%
    left_join(team_df, by = c("team" = "team_abbr")) %>%
    mutate(logo_label = glue::glue("<img src='{team_logo_espn}' width='12'/>")) # create html tags for geom_richtext to read


# Make plot (NFC)
playoff_odds_nfc <- playoff_odds_joined %>% 
    filter(team_conf == "NFC")

playoff_odds_nfc %>%
    arrange(now) %>%
    mutate(team = fct_reorder(team, now)) %>%
    ggplot() +
    # Connecting segment
    geom_segment(aes(x = leverage_loss, xend = leverage_win, y = team, yend = team, color = team_color),
                 size = 2.5, alpha = 0.6) +
    # Left point (loss)
    geom_point(aes(x = leverage_loss, y = team, color = team_color), size = 2) +
    # Right point (win)
    geom_point(aes(x = leverage_win, y = team, color = team_color), size = 2) +
    # Team logos
    geom_image(aes(x = now, y = team, image = team_logo_espn), size = 0.04, asp = 1.8) +
    # Percentage labels
    geom_text(data = playoff_odds_nfc, 
              aes(x = leverage_win, y = team, label = sprintf("%.1f%%", leverage_win * 100)),
              color = "#2E86C1", size = 2.75, vjust = 2.5, family = "Titillium Web", fontface = "bold") +
    geom_text(data = playoff_odds_nfc, 
              aes(x = leverage_loss, y = team, label = sprintf("%.1f%%", leverage_loss * 100)),
              color = "#CB4335", size = 2.75, vjust = 2.5, family = "Titillium Web", fontface = "bold") +
    # Labels and scales
    labs(title = "**NFC Playoff Picture**", 
         subtitle = glue("Difference between <span style = 'color:#CB4335'>**playoff % if loss**</span> and <span style = 'color:#2E86C1'>**playoff % if win**</span> | Entering **Week 12**"),
         caption = "Data: Neil Paine\nGraphic: @steodosescu", 
         x = "Playoff Probability",
         y = "") +
    theme_custom() +
    scale_fill_identity() + 
    scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    scale_color_identity() +  # Suppress legend for team colors
    theme(plot.title.position = 'plot', 
          plot.title = element_markdown(face = 'bold', size = 20, hjust = 0.5),
          plot.subtitle = element_markdown(hjust = 0.5),
          panel.grid.major.x = element_line(size = 0.05),
          panel.grid.major.y = element_blank())

ggsave("Playoff Leverage.png")

# Add logo to plot
leverage_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Playoff Leverage.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(leverage_with_logo, "Playoff Leverage with Logo.png")




