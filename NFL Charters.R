##### NFL Charter Flights #####
##### By: Stephan Teodosescu #####
##### November 2024 #####

library(tidyverse)
library(glue)
library(rvest)
library(googlesheets4)
library(nflverse)


## 2. ---------- Scrape single webpage ------------ ##

# Function to generate the URL for a specific week (weeks 1 and 2 of 2024 which have different url than the rest of the weeks)
generate_url <- function(year, week) {
    paste0("https://blog.jettip.net/nfl-", year, "-week-", week, "-team-charter-flights")
}

# Function to scrape data from a single page
scrape_nfl_data <- function(url) {
    # Read the webpage content
    page <- read_html(url)
    
    # Extract relevant data (example assumes a list structure)
    flights <- page %>%
        html_nodes("h2 , li") %>%  # Adjust the selector based on the actual HTML
        html_text()
    
    # Process and tidy up the data
    data <- tibble(Flight_Info = flights) %>%
        separate(Flight_Info, into = c("Team", "Flight_Details"), sep = " - ", extra = "merge") %>% # Adjust split logic if needed
        mutate(Week = week) %>%
        filter(!is.na(Flight_Details))
    
    return(data)
}

# Scrape data for week 1-2 of 2024
year <- 2024
week <- 'two'
url <- generate_url(year, week)

# Scrape and view the data
charters <- scrape_nfl_data(url)
print(charters)

# process data 
charters_clean <- charters %>%
    mutate(
        Week = case_when(
            Week == 'one' ~ 1,
            Week == 'two' ~ 2,
            Week == 'three' ~ 3,
            Week == 'four' ~ 4,
            Week == 'five' ~ 5,
            Week == 'six' ~ 6,
            Week == 'seven' ~ 7,
            Week == 'eight' ~ 8,
            Week == 'nine' ~ 9
        )) %>%
    mutate(
        Away_Team = str_extract(Team, "^[^@]+") %>% str_trim(), # Extract everything before '@'
        Home_Team = str_extract(Team, "(?<=@ ).*?(?=[A-Z]{3})") %>% str_trim(), # Capture text after @ and before the airport code
        Departure_Airport = str_extract(Team, "[A-Z]{3}(?= \\()"),
        
        # Extract details from Flight_Details
        Arrival_Airport = str_extract(Flight_Details, "^[A-Z]{3}"),  # The 3-letter code at the start
        Flight_Number = str_extract(Flight_Details, "(?<=, )\\w+"),    # After the first comma
        Aircraft_Type = str_extract(Flight_Details, "(?<=, )\\w+$")    # The last word/number after the second comma
    ) %>%
    select(Week, Away_Team, Home_Team, Departure_Airport, Arrival_Airport, Flight_Number, Aircraft_Type) %>%
    mutate(
        Away_Team = str_remove(Away_Team, "\\s*\\(.*\\)"), # Remove parentheses and text within them
        Home_Team = str_remove(Home_Team, "\\s*\\(.*\\)")  # Do the same for Home_Team
    ) %>%
    mutate(Away_Team = case_when(
        Away_Team == 'Kansas City' ~ 'Kansas City Chiefs',
        TRUE ~ as.character(Away_Team)
    ),
    Home_Team = case_when(
        Home_Team == 'Kansas City' ~ 'Kansas City Chiefs',
        TRUE ~ as.character(Home_Team)
    )) %>%
    mutate(ts = Sys.Date()) # add in last refreshed timestamp

    

# run the scraping function for a second time for W2 and combine

# Save the data to a CSV file
write_csv(charters_clean, file = paste0("nfl_week_", week, "_", year, ".csv"))





## 2. ---------- Scrape multiple webpages at once ------------ ##

# Function to map numbers to their spelled-out counterparts
week_to_url_part <- function(week) {
    if (week <= 9) {
        # For single-digit weeks, return the spelled-out word
        week_map <- c(
            "1" = "one", "2" = "two", "3" = "three", "4" = "four", "5" = "five",
            "6" = "six", "7" = "seven", "8" = "eight", "9" = "nine"
        )
        return(week_map[as.character(week)])
    } else {
        # For double-digit weeks, return the numeric value as a string
        return(as.character(week))
    }
}

# Function to generate the URL for a specific week
generate_url <- function(year, week) {
    week_part <- week_to_url_part(week)
    paste0("https://blog.jettip.net/nfl-", year, "-week-", week_part, "-team-charter-flights")
}


# Function to scrape data from a single page
scrape_nfl_data <- function(url) {
    # Read the webpage content
    page <- read_html(url)
    
    # Extract relevant data (adjust the CSS selector as needed)
    flights <- page %>%
        html_nodes("h2 , li") %>%  # Adjust based on the actual HTML structure
        html_text()
    
    # Process and tidy up the data
    data <- tibble(Flight_Info = flights) %>%
        separate(Flight_Info, into = c("Team", "Flight_Details"), sep = " - ", extra = "merge") %>%
        mutate(Week = str_extract(url, "week-[a-z0-9]+") %>% str_replace("week-", ""),
               Week = case_when(
                   Week == 'one' ~ 1,
                   Week == 'two' ~ 2,
                   Week == 'three' ~ 3,
                   Week == 'four' ~ 4,
                   Week == 'five' ~ 5,
                   Week == 'six' ~ 6,
                   Week == 'seven' ~ 7,
                   Week == 'eight' ~ 8,
                   Week == 'nine' ~ 9,
                   TRUE ~ as.numeric(Week)
               ))
    
    return(data)
}

# Scrape data for weeks 1 to current
year <- 2025
weeks <- 1:18

all_weeks_data <- bind_rows(
    lapply(weeks, function(w) {
        url <- generate_url(year, w)
        message("Scraping: ", url) # Optional: log the progress
        scrape_nfl_data(url)
    })
)


# Remove rows with NAs
all_weeks_data_clean <- all_weeks_data %>%
    filter(!is.na(Flight_Details))




## 3. -------- Process resulting dataframe ---------

charters_data <- all_weeks_data_clean %>%
    mutate(
        Away_Team = str_extract(Team, "^[^@]+") %>% str_trim(), # Extract everything before '@'
        Home_Team = str_extract(Team, "(?<=@ ).*?(?=[A-Z]{3})") %>% str_trim(), # Capture text after @ and before the airport code
        Departure_Airport = str_extract(Team, "[A-Z]{3}(?= \\()"),
        
        # Extract details from Flight_Details
        Arrival_Airport = str_extract(Flight_Details, "^[A-Z]{3}"),  # The 3-letter code at the start
        Flight_Number = str_extract(Flight_Details, "(?<=, )\\w+"),    # After the first comma
        Aircraft_Type = str_extract(Flight_Details, "(?<=, )\\w+$")    # The last word/number after the second comma
    ) %>%
    select(Week, Away_Team, Home_Team, Departure_Airport, Arrival_Airport, Flight_Number, Aircraft_Type) %>%
    mutate(
        Away_Team = str_remove(Away_Team, "\\s*\\(.*\\)"), # Remove parentheses and text within them
        Home_Team = str_remove(Home_Team, "\\s*\\(.*\\)")  # Do the same for Home_Team
    ) %>%
    mutate(Away_Team = case_when(
               Away_Team == 'Kansas City' ~ 'Kansas City Chiefs',
               TRUE ~ as.character(Away_Team)
           ),
           Home_Team = case_when(
               Home_Team == 'Kansas City' ~ 'Kansas City Chiefs',
               TRUE ~ as.character(Home_Team)
           )) %>%
    mutate(ts = Sys.Date()) # add in last refreshed timestamp

# save to wd
#write_csv(charters_data, "charters.csv")


# Read W1 and W2 datasets back in and union with multipage df
w1 <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/nfl_week_one_2024.csv')
w2 <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/nfl_week_two_2024.csv')

charters_data <- charters_data %>%
    bind_rows(w1) %>%
    bind_rows(w2) %>%
    arrange(Week)



## 4. ------- Scrape Playoff Rounds webpages ---------

# Function to generate the URL for a specific week (weeks 1 and 2 of 2024 which have different url than the rest of the weeks)
generate_url <- function(year, week) {
    paste0("https://blog.jettip.net/nfl-", year, "-", week, "-team-charter-flights")
}

# Function to scrape data from a single page
scrape_nfl_data <- function(url) {
    # Read the webpage content
    page <- read_html(url)
    
    # Extract relevant data (example assumes a list structure)
    flights <- page %>%
        html_nodes("h2 , li") %>%  # Adjust the selector based on the actual HTML
        html_text()
    
    # Process and tidy up the data
    data <- tibble(Flight_Info = flights) %>%
        separate(Flight_Info, into = c("Team", "Flight_Details"), sep = " - ", extra = "merge") %>% # Adjust split logic if needed
        mutate(Week = week) %>%
        filter(!is.na(Flight_Details))
    
    return(data)
}

# Scrape data for playoff weekends
year <- 2024
week <- 'wild-card'
url <- generate_url(year, week)

# Scrape and view the data
charters <- scrape_nfl_data(url)
print(charters)

# process data 
charters_clean <- charters %>%
    mutate(
        Week = case_when(
            Week == 'one' ~ 1,
            Week == 'two' ~ 2,
            Week == 'three' ~ 3,
            Week == 'four' ~ 4,
            Week == 'five' ~ 5,
            Week == 'six' ~ 6,
            Week == 'seven' ~ 7,
            Week == 'eight' ~ 8,
            Week == 'nine' ~ 9,
            Week == 'wild-card' ~ 19
        )) %>%
    mutate(
        Away_Team = str_extract(Team, "^[^@]+") %>% str_trim(), # Extract everything before '@'
        Home_Team = str_extract(Team, "(?<=@ ).*?(?=[A-Z]{3})") %>% str_trim(), # Capture text after @ and before the airport code
        Departure_Airport = str_extract(Team, "[A-Z]{3}(?= \\()"),
        
        # Extract details from Flight_Details
        Arrival_Airport = str_extract(Flight_Details, "^[A-Z]{3}"),  # The 3-letter code at the start
        Flight_Number = str_extract(Flight_Details, "(?<=, )\\w+"),    # After the first comma
        Aircraft_Type = str_extract(Flight_Details, "(?<=, )\\w+$")    # The last word/number after the second comma
    ) %>%
    select(Week, Away_Team, Home_Team, Departure_Airport, Arrival_Airport, Flight_Number, Aircraft_Type) %>%
    mutate(
        Away_Team = str_remove(Away_Team, "\\s*\\(.*\\)"), # Remove parentheses and text within them
        Home_Team = str_remove(Home_Team, "\\s*\\(.*\\)")  # Do the same for Home_Team
    ) %>%
    mutate(Away_Team = case_when(
        Away_Team == 'Kansas City' ~ 'Kansas City Chiefs',
        TRUE ~ as.character(Away_Team)
    ),
    Home_Team = case_when(
        Home_Team == 'Kansas City' ~ 'Kansas City Chiefs',
        TRUE ~ as.character(Home_Team)
    )) %>%
    mutate(ts = Sys.Date()) # add in last refreshed timestamp



## 5. -------- Save to googlesheets ------------

# Authenticate with Google Sheets (you only need to do this once per session)
gs4_auth()

# URL or ID of the Google Sheets document
sheet_url <- "https://docs.google.com/spreadsheets/d/17fVR5KukHeCyoTf3wQcKwKrR4XqPXwNuPEj6sxBkzvE/edit?gid=759422868#gid=759422868"

# Save data to the "Charters" tab
sheet_write(data = charters_data, ss = sheet_url, sheet = "Charters")



## 6. Schedules table
team_schedules <- load_sharpe_games() %>% 
    filter(season == 2025)

phi_schedule <- team_schedules %>%
    filter(str_detect(game_id,"PHI"))



