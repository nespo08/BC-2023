# Load by year - Bill Petti function --------------------------------------

annual_statcast_query <- function(season) {
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  safe_savant <- safely(statcast_search)
  
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
}

temp_2022 <- annual_statcast_query("2022")
write_rds(temp_2022, "data/SSW-data/pitching_data_2022.rds", compress = "gz") # Write as RDS for better storage

# Same process (csv to rds) for active spin avgs - https://baseballsavant.mlb.com/leaderboard/spin-direction-pitches?year=2022&min=q&sort=9&sortDir=asc&pitch_type=ALL&throws=&playerName=&team=&pov=Bat
temp_activespin <- read_csv("data/SSW-data/MLB_2022_spin_direction_pitches.csv")
write_rds(temp_activespin, "data/SSW-data/MLB_2022_spin_direction_pitches.rds", compress = "gz") # Write as RDS for better storage

###### Run code from here down. Only run code above once to download data locally

# Load the libraries -----------------------------------------------------------

library(baseballr)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(glue)

# Read as RDS ------------------------------------

pitching_data <- data.frame(read_rds("data/SSW-data/pitching_data_2022.rds"))
spin_data <- data.frame(read_rds("data/SSW-data/MLB_2022_spin_direction_pitches.rds"))

# Cleaning and calculations ----------------------------------------------------------------

# Pitching Data
pitching_data <- pitching_data %>%
  filter(!is.na(release_spin_rate), !is.na(release_speed), !is.na(release_extension), game_type == 'R') %>%
  filter(!is.na(pitch_type), pitch_type != "") %>%
  mutate(pitch_type = recode(pitch_type,
                             "FF" = "Fastball",
                             "SL" = "Slider",
                             "FC" = "Cutter",
                             "CU" = "Curveball",
                             "CH" = "Changeup",
                             "FS" = "Splitter",
                             "SI" = "Sinker",
                             "ST" = "Sweeper",
                             "KC" = "Knuckle-curve",
                             "CS" = "Other",
                             "SC" = "Screwball",
                             "SV" = "Slurve",
                             "FA" = "Other",
                             "PO" = "Pitch-out",
                             "EP" = "Eephus",
                             "KN" = "Knuckleball")) %>%
  mutate(pitcher_id = pitcher)

unique_pitches <- unique(pitching_data$pitch_type)

# Find the average of each metric, by pitch type
metric_averages <- pitching_data %>%
  select(pitch_type, release_spin_rate, release_speed, release_extension, p_throws) %>%
  group_by(pitch_type, p_throws) %>%
  mutate(avg_spin = mean(release_spin_rate),
         avg_velocity = mean(release_speed),
         avg_extension = mean(release_extension)) %>%
  distinct(avg_spin, avg_velocity, avg_extension)

Count_DF <- pitching_data %>%
  group_by(pitcher_id, pitch_type) %>%
  count(pitch_type) %>%
  ungroup() %>%
  filter(., n > 10)

pitching_data_filtered <- pitching_data %>%
  filter(., pitch_type %in% c(Count_DF$pitch_type) & pitcher_id %in% c(Count_DF$pitcher_id))

# Spin Data
clean_spin_data <- spin_data %>%
  mutate(pitch_type = recode(api_pitch_type,
                             "FF" = "Fastball",
                             "SL" = "Slider",
                             "FC" = "Cutter",
                             "CU" = "Curveball",
                             "CH" = "Changeup",
                             "FS" = "Splitter",
                             "SI" = "Sinker",
                             "ST" = "Sweeper",
                             "KC" = "Knuckle-curve",
                             "CS" = "Other",
                             "SC" = "Screwball",
                             "SV" = "Slurve",
                             "FA" = "Other",
                             "PO" = "Pitch-out",
                             "EP" = "Eephus",
                             "KN" = "Knuckleball")) %>%
  mutate(pitcher_id = player_id)

unique_spin_pitches <- unique(clean_spin_data$pitch_type)

# Filter fastballs and sinkers and append to original dataframe
fb_sink_data <- clean_spin_data %>% 
  filter(pitch_type %in% c('Fastball', 'Sinker')) %>%
  filter(active_spin > 0.8)

clean_spin_data_no_fb_sink <- clean_spin_data %>% 
  filter(!pitch_type %in% c('Fastball', 'Sinker'))

clean_spin_data_merged <- rbind(clean_spin_data_no_fb_sink, fb_sink_data)

# Dataframes for GitHub

# Metric data (velo, spin rate, extension)
github_metric_data <- pitching_data_filtered %>%
  select(pitch_type, release_spin_rate, release_speed, release_extension, p_throws) %>%
  group_by(pitch_type, p_throws)

write_rds(github_metric_data, "data/SSW-data/GitHub_Metric_Data_2022.rds")

# Spin data (efficiency)
github_eff_data <- clean_spin_data_merged %>%
  select(pitch_hand, pitch_type, active_spin) %>%
  group_by(pitch_type, pitch_hand)

write_rds(github_eff_data, "data/SSW-data/GitHub_Eff_Data_2022.rds")
