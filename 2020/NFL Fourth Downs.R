##### NFL Fourth Down Go for it Rates #####
## By: Stephan Teodosescu
## Updated January 4, 2020

# Inspiration from: https://github.com/kcuilla/fourth_down_go_rates/blob/main/fourth_down_go_rates.Rmd

##### Load packages #####
library(nflfastR)
library(tidyverse)
library(htmltools)
library(gt) #for 538-themed tables
library(palateer) #for color palettes 
library(webshot) #saving images of gt tables
library(reactable)
library(reactablefmtr)


##### Load 2010-2020 data  #####
seasons <- 2010:2021
fourth_down_plays <- purrr::map_df(seasons, function(x) {
   readRDS(
     url(
       glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
     )
   )
 }) %>%
   filter(
     down == 4,
     qb_kneel == 0,
     !is.na(posteam),
     !is.na(yardline_100),
     !is.na(score_differential)
   )

# Focus on fourth down plays only
go_for_it <- fourth_down_plays %>%
  mutate(
    yards_to_go = case_when(
      ydstogo <= 2 ~ "2 or less",
      ydstogo >= 3 & ydstogo <= 5 ~ "3 to 5",
      ydstogo >= 6 ~ "6 or more",
      TRUE ~ "NA"
    )
  ) %>%
  mutate(
    play_type = case_when(
      play_type == "field_goal" | play_type == "punt" ~ "Punt/FG",
      play_type == "run" | play_type == "pass" ~ "Run/Pass",
      play_type == "no_play" ~ "Penalty",
      TRUE ~ "NA"
    )
  ) %>%
  filter(yards_to_go == "2 or less" &
           play_type != "Penalty" & 
           wp > .20 & 
           wp < .80) %>%
  dplyr::group_by(season, posteam, play_type) %>%
  summarize(n = n()) %>% 
  mutate(`2010-2021` = round(100 * (n / sum(n)), 1)) %>%
  select(-c(n)) %>% 
  pivot_wider(names_from = "season", values_from = "2010-2021") %>%
  filter(play_type == "Run/Pass") %>%
  ungroup() %>%
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  pivot_longer(cols = starts_with("20"),
               names_to = "season",
               values_to = "2010-2021") %>% 
  arrange(posteam, season)

# Analyze trends
trend <- go_for_it %>%
  ungroup() %>%
  select(team = posteam, `2010-2021`) %>%
  group_by(team) %>%
  mutate(`2010-2021` = list(`2010-2021`)) %>%
  distinct(team, `2010-2021`) %>%
  ungroup()


go_for_it_by_year <- go_for_it %>%
  select(season, team = posteam, `2010-2021`) %>%
  pivot_wider(names_from = "season", values_from = "2010-2021") %>%
  mutate_if(is.numeric, list(~replace_na(., 0))) %>% 
  ungroup() %>%
  inner_join(trend, by = c("team" = "team")) %>% 
  left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>% 
  select(-c(team_name,team_id,team_nick,team_color2,team_color3,team_color4,team_logo_wikipedia, team_wordmark))

# Make color palette
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x)
    rgb(get_color(x), maxColorValue = 255)
}

# Orange pallette
orange_pal <-
  make_color_pal(c(
    "#fef4eb",
    "#facba6",
    "#f8b58b",
    "#f59e72",
    "#f2855d",
    "#ef6a4c"
  ),
  bias = 0.7)

# Percentage color pallette
pct_col <- colDef(
  maxWidth = 60,
  class = "number",
  footer = function(value)
    paste0(sprintf("%.1f", mean(value)),"%"),
  cell = function(value)
    paste0(format(
      value, digits = 1, nsmall = 1
    ), "%"),
  style = function(y) {
    normalized <-
      ((y - 0) / (100 - 0))
    color <- orange_pal(normalized)
    list(background = color)
  }
)


##### Create 538 table theme from Thomas Mock's blog #####
# Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_nfl_theme_538 <- function(data,...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        vars(team_logo_espn)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    # Relabel columns
    cols_label(
      team_logo_espn = ""
    ) %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
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
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

##### GT Table #####
go_for_it_by_year %>%
  select(team_logo_espn, team, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`) %>%
  arrange(desc(`2021`)) %>%
  gt() %>%
  data_color(columns = 14,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_nfl_theme_538() %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**Teams are going for it on 4th Down more often nowadays**"),
             subtitle ="Thru 2021 Week 2. Percentages shown are how often a team went for it when it on 4th & short, defined as less than 3 yards to go, and when win probability was between 20% and 80% (game-neutral situations).") %>%
  tab_source_note(
    source_note = md("DATA: nflfastR<br>TABLE: @steodosescu")) %>%
  gtsave("Fourth Down Table.png")


##### Fourth Downs by Play Type (YUSAG) #####
df <- tibble(
  Play_Type = c("QB sneaks", "Non-QB sneaks", "Passes"),
  EPA = c(1.61, 0.83, 0.86)
)

df %>%
  rename(`Play Type` = Play_Type)


df %>%
  gt() %>%
  tab_header(
    title = md("**Expected Points Added (EPA) on 4th-and-1 to go**"),
    subtitle = md("Quarterback sneaks are the most efificent play type in fourth and short situations")
  ) %>%
  tab_source_note(
    source_note = md("SOURCE: Yale Undergraduate Sports Analytics Group<br>TABLE: @steodosescu")
  ) %>%
  gtsave("EPA Table.png")
  gt_theme_538(table.width = px(550))
