##### NFL 2023 Playoffs #####
##### December 2023 #####
##### By: Stephan Teodosescu #####

library(nflverse)
library(tidyverse)
library(gt)
library(gtExtras)
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




## ------------- 1. Playoff Probabilities --------------------

# load team logos and colors
team_df <- nflreadr::load_teams() %>% 
  select(team_logo_espn, team_abbr, team_name, team_conf, team_division, team_color)

# build Vegas odds dataset
# vegas_odds <- tibble(team_abbr= c("SF", "PHI", "KC", "BAL", "MIA", "DAL", "DET", "JAC", "BUF", "HOU"),
#                      conf_odds = c(-145, +120, -120, -110, +150, +170),
#                      sb_odds = c(+310, +550, +650, +800, +650, +850, +1400, +2000, +5000, +3500))

vegas_odds <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/champ_odds_2023.csv')

# create implied odds calculation
vegas_odds <- vegas_odds %>%
  mutate(conf_implied_odds = if_else(conf_odds < 0, (conf_odds)/(conf_odds-100), 1-conf_odds/(conf_odds+100)),
         sb_implied_odds = if_else(sb_odds < 0, -1*sb_odds/100+(sb_odds*1), 1-sb_odds/(sb_odds+100)))

vegas_odds$conf_implied_odds_label <- percent(vegas_odds$conf_implied_odds, accuracy = 1)
vegas_odds$sb_implied_odds_label <- percent(vegas_odds$sb_implied_odds, accuracy = 1)



# join teams 
vegas_odds <- vegas_odds %>%
  left_join(team_df,by = "team_abbr")

vegas_odds %>% 
  ggplot(aes(y = reorder(team_abbr, -conf_implied_odds), x = conf_implied_odds, fill =  team_color)) +
  geom_col(aes(fill = team_color, color = after_scale(clr_darken(fill, 0.3))),
           width = 0.7,
           alpha = 0.5) + 
  geom_col(aes(x = sb_implied_odds), width = 0.7) +
  geom_text(data= vegas_odds,  aes(label = conf_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.7)) +
  geom_text(data= vegas_odds,  aes(label = sb_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.3)) +
  scale_color_identity(aesthetics =  c("fill"))  +
  geom_image(
    aes(
      image = team_logo_espn                                  
    ), 
    size = 0.045, 
    by = "width", 
    asp = asp_ratio
  ) +
  theme_custom() +
  coord_flip() +
  scale_x_continuous(limits = c(0,1), labels = scales::percent_format()) +
  labs(x = "Super Bowl Odds | Conference Champ Odds", y = "",
       y = "",
       caption = "Data: Pinnacle.com\nGraphic: @steodosescu",
       title = glue("The Betting Favorites"),
       subtitle =  glue("Implied win prob % based on Pinnacle odds. Data as of Jan 8th, 2023.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("2023 Championship Odds.png")

# Add logo to plot
champs_odds_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2023 Championship Odds.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(champs_odds_with_logo, "2023 Championship Odds with Logo.png")


## ------------- 2. Composite Odds --------------------

composite_odds <- read_csv("/Users/Stephan/Desktop/R Projects/NFL/composite_odds.csv")

# change vegas odds to implied percentages and calculate means
composite_odds <- composite_odds %>%
  mutate(pinnacle_conf_odds = if_else(pinnacle_conf_odds < 0, (pinnacle_conf_odds)/(pinnacle_conf_odds-100), 1-pinnacle_conf_odds/(pinnacle_conf_odds+100)),
       pinnacle_sb_odds = if_else(pinnacle_sb_odds < 0, -1*pinnacle_sb_odds/100+(pinnacle_sb_odds*1), 1-pinnacle_sb_odds/(pinnacle_sb_odds+100))
       ) %>%
  mutate(
    conf_odds = rowMeans(select(., ends_with('conf_odds')), na.rm = TRUE),
    sb_odds = rowMeans(select(., ends_with('sb_odds')), na.rm = TRUE)
  )


composite_odds <- composite_odds %>%
  left_join(team_df, by = "team_abbr")


# Make gt table
composite_odds %>%
  arrange(desc(sb_odds)) %>%
  mutate(rank = row_number()) %>%
  select(rank, team_abbr, team_logo_espn, team_conf, pinnacle_conf_odds:sb_odds) %>%
  gt() %>%
  gt_img_rows(team_logo_espn, height = 30) %>%
  # Relabel columns
  cols_label(
    rank = "Rank",
    team_abbr = "Team",
    team_logo_espn = "",
    team_conf = "Conf",
    pinnacle_conf_odds = "Conf Odds",
    pinnacle_sb_odds = "SB Odds",
    np_conf_odds = "Conf Odds",
    np_sb_odds = "SB Odds",
    opta_conf_odds = "Conf Odds",
    opta_sb_odds = "SB Odds",
    pff_conf_odds = "Conf Odds",
    pff_sb_odds = "SB Odds",
    sumer_conf_odds = "Conf Odds",
    sumer_sb_odds = "SB Odds",
    fpi_conf_odds = "Conf Odds",
    fpi_sb_odds = "SB Odds",
    conf_odds = "Conf Odds",
    sb_odds = "SB Odds"
  ) %>%
  cols_width(
      rank ~ px(50),
      team_abbr ~ px(50),
      team_conf ~ px(50),
      team_logo_espn ~ px(50),
      everything() ~ px(100)
  ) %>%
  data_color(
    columns = c("pinnacle_conf_odds":"sb_odds"), 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material",
        direction = 1
      ) %>% as.character(),
      domain = NULL
    )) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(conf_odds, sb_odds)
    )
  ) %>%
  fmt_percent(columns = ends_with('odds'),
             decimals = 1) %>%
  tab_spanner(label = "Pinnacle", 
              columns = pinnacle_conf_odds:pinnacle_sb_odds) %>%
  tab_spanner(label = "Neil Paine", 
              columns = np_conf_odds:np_sb_odds) %>%
  tab_spanner(label = "Opta", 
              columns = opta_conf_odds:opta_sb_odds) %>%
  tab_spanner(label = "PFF", 
              columns = pff_conf_odds:pff_sb_odds) %>%
  tab_spanner(label = "Sumer Sports", 
              columns = sumer_conf_odds:sumer_sb_odds) %>%
  tab_spanner(label = "ESPN FPI", 
              columns = fpi_conf_odds:fpi_sb_odds) %>%
  tab_spanner(label = "Composite", 
              columns = conf_odds:sb_odds) %>%
  tab_header(title = md("**2023 NFL Composite Playoff Odds**"),
             subtitle = glue("Using publicly available models. Data as of January 8, 2023.")) %>%
  opt_align_table_header(align = "center") %>%
  tab_source_note(
    source_note = md("Data: Pinnacle.com, Neil Paine (The Messenger), Opta Analyst, PFF.com, Sumer Sports, ESPN<br>Table: @steodosescu")) %>% 
  tab_footnote(
    footnote = "Implied probabilities converted using monelyine odds.",
    locations = cells_column_labels(vars(pinnacle_conf_odds))
  ) %>%
  opt_table_font(
    font = "Chivo"
    ) %>%
  tab_options(
    column_labels.background.color = "white",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    heading.title.font.size = 28,
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "center"
  ) %>%
  gtsave("2023 NFL Playoffs Composite Rankings.png", vwidth = 1650, vheight = 1020)


# Add logo to plot
composite_odds_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2023 NFL Playoffs Composite Rankings.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(composite_odds_with_logo, "2023 NFL Playoffs Composite Rankings with Logo.png")
