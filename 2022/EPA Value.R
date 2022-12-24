##### 49ers EPA Analysis #####
##### By: Stephan Teodosescu #####
##### April 2022 #####

library(tidyverse)
library(nflfastR)
library(tidyverse)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggrepel) # better labels
library(ggimage)
library(glue)
library(ggtext)
library(reactable)
library(reactablefmtr)
library(ggsci)
library(ggchicklet)
library(gt)
library(gtExtras)


##### Setup #####

# Optional but makes R prefer not to display numbers in scientific notation
options(scipen = 9999)

#Set aspect ratio for logo based plots
asp_ratio <- 1.618

# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
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


#Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

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

##### Load data #####

data <- load_pbp(2022) #Load PBP data from nflfastR

# Filter for Run/Pass plays for the 2022 season only
pbp_rp <- data %>%
    filter(rush == 1 | pass == 1, !is.na(epa)) # Exclude plays with missing EPA.

# Identify current week
current_week <- max(pbp_rp$week)



##### Data Preparation #####

#create offense/defense/special teams data frames

offense <- pbp_rp %>%
    group_by(posteam)%>%
    summarize(
        n_pass=sum(pass),
        n_rush=sum(rush),
        epa_per_pass=sum(epa*pass)/n_pass,
        epa_per_rush=sum(epa*rush)/n_rush,
        success_per_pass=sum(pass*epa>0)/n_pass,
        success_per_rush=sum(rush*epa>0)/n_rush,
        off_epa=mean(epa))

defense <- pbp_rp %>%
    group_by(defteam)%>%
    summarize(
        def_n_pass=sum(pass),
        def_n_rush=sum(rush),
        def_epa_per_pass=sum(epa*pass)/def_n_pass,
        def_epa_per_rush=sum(epa*rush)/def_n_rush,
        def_success_per_pass=sum(pass*epa>0)/def_n_pass,
        def_success_per_rush=sum(rush*epa>0)/def_n_rush,
        def_epa=mean(epa)
    )



# join offense and defense data frames
off_def_epa <- offense %>% 
    inner_join(defense, by=c("posteam" = "defteam")) %>%
    left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))

# 

##### Data visualization #####

## 1. EPA Plot

off_def_epa %>% 
    ggplot(aes(x = off_epa, y = def_epa)) + 
    geom_image(aes(image = team_logo_espn), size = 0.065, by = "width", asp = asp_ratio) +
    geom_hline(yintercept = mean(off_def_epa$off_epa), color = "red", linetype = "dashed") +
    geom_vline(xintercept =  mean(off_def_epa$def_epa), color = "red", linetype = "dashed") +
    theme_custom() +
    geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3, -.4), alpha = .2) +
    labs(x = "Offense EPA/play",
         y = "Defense EPA/play",
         caption = "Data: @nflfastR | Plot: @steodosescu",
         title = glue("Expected Points Added (EPA) Tiers"),
         subtitle = glue("Rush and pass plays only. Thru **Week {current_week}** TNF game.")) +
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
    scale_y_reverse()

ggsave("EPA Plot.png")

# Add EPL logo to plot
epa_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022/EPA Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NFL/NFL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(epa_plot_with_logo, "EPA Plot with Logo.png")


## 2. QB Summary Table

player_stats <- load_player_stats()

nfl_qb <- player_stats |>
    filter(position=="QB")

nfl_team <- teams_colors_logos

#reshape data
qb_summary_data <- nfl_qb |>
    filter(passing_yards>100)|> 
    select(headshot_url, player_name, player_display_name, recent_team, completions, attempts, passing_yards, passing_tds, interceptions, passing_epa, sacks, rushing_yards, rushing_tds)|>
    group_by(headshot_url, player_name, player_display_name, recent_team,) |> 
    summarise(total_completions = sum(completions),
              total_attempts = sum(attempts),
              total_passing_yards = sum(passing_yards),
              total_passing_tds = sum(passing_tds),
              total_interceptions = sum(interceptions),
              total_passing_epa = sum(passing_epa),
              total_sacks = sum(sacks),
              total_rushing_yards = sum(rushing_yards),
              total_rushing_tds = sum(rushing_tds)) |> 
    inner_join(teams_colors_logos|>
                   select(team_abbr, team_nick, team_division, team_logo_espn), by=c("recent_team"="team_abbr"))|>
    separate(team_division, into=c("conference","division"), sep=" ")|>
    mutate(comp_perc = total_completions/total_attempts)|>
    arrange(conference, division, -total_passing_epa, -total_passing_tds, -comp_perc, -total_passing_yards)|>
    group_by(conference, division)|>
    mutate(rank = row_number(), 
           pos = -rank)


# Create gt table
qb_summary_table <- qb_summary_data |> 
    select(headshot_url, player_display_name, team_logo_espn, recent_team:total_interceptions, conference:comp_perc, total_passing_epa) |> 
    gt() |> 
    tab_header(
        title = md("**Quarterback Passing Summary**"), 
        subtitle = glue("Minimum 100 pass yards, and ordered by passing TDs thrown. Thru Week {current_week} TNF game.")
    )  %>%
    cols_label(headshot_url = "",
               player_display_name = "Player",
               recent_team = "Team",
               team_logo_espn = "",
               total_completions = "Comps",
               total_attempts = "Att",
               total_passing_yards = "Pass Yds",
               total_passing_tds = "Pass TDs",
               total_interceptions = "INTs",
               comp_perc = "Comp %",
               total_passing_epa = "Pass EPA") %>% 
    gt_img_rows(headshot_url) %>%
    gt_img_rows(team_logo_espn) %>%
    data_color(
        columns = vars(total_passing_epa),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "RColorBrewer::PRGn",
                direction  = 1
            ) %>% as.character(),
            domain = NULL, 
            na.color = "#00441BFF"
        )
    ) %>%
    fmt_percent(
        columns = comp_perc,
        decimals = 1
    ) |> 
    fmt_number(
        columns = total_passing_epa,
        decimals = 1
    ) |> 
    tab_options(
        table.background.color = "white",
        heading.title.font.size  = 28,
        heading.title.font.weight = 'bold',
        heading.subtitle.font.size = 14,
        table.font.names = "Chivo", 
        table.font.color = 'black',
        table.border.top.color = "transparent",
        footnotes.font.size = 12,
        source_notes.font.size = 12,
        data_row.padding = px(2), 
        footnotes.padding = px(1), 
    ) %>%
    tab_source_note(
        source_note = md("Table: @steodosescu | Data: nflfastR")
    )


gtsave(qb_summary_table, "QB Summary Table.png")
