library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(stringr)

# Load data
pressure_stats <- read.csv("edge_rusher_pressures_modified.csv")

# Replace values for pressure with indicator variable
pressure_stats <- pressure_stats%>%
  arrange(team, week, PlayId)%>%
  mutate(PFFPressure = if_else(PFFPressure == "", 0, 1))

# Obtain total player snap numbers
pressure_stats <- pressure_stats%>%
  group_by(Player)%>%
  mutate(snap = row_number(Player))%>% ## Total snap number (EX: player's fifth snap of the season)
  ungroup()%>%
  group_by(Player, week)%>%
  mutate(game_snap = row_number(week))%>% ## Game snap number (EX: player's third snap of the game)
  ungroup()%>%
  group_by(Player, week, DriveNumber)%>%
  mutate(drive_snap = row_number(DriveNumber))%>% ## Drive snap number (EX: player's second snap of the drive)
  ungroup()

# Obtain total player snap numbers -- only on pass rushing plays
psrsh_pressure_stats <- pressure_stats%>%
  filter(Drpbk == 1)%>% ## Filter data to only contain plays where player rushes the passer
  group_by(Player)%>%
  mutate(pass_rush_snap = row_number(Player))%>% ## Total pass rush snap number
  ungroup()%>%
  group_by(Player, week)%>%
  mutate(game_pass_rush_snap = row_number(week))%>% ## Game pass rush snap number
  ungroup()%>%
  group_by(Player, week, DriveNumber)%>%
  mutate(drive_pass_rush_snap = row_number(DriveNumber))%>% ## Drive pass rush snap number
  ungroup()

# Restore original data (pass rush snaps and non-pass rush snaps)
non_pressure_stats <- pressure_stats%>%
  filter(Drpbk == 0)

# Join two datasets and filter for pass rush snaps only
pressure_stats <- full_join(psrsh_pressure_stats, non_pressure_stats)%>%
  filter(Drpbk == 1)

# Function to get rolling/cumulative pressure rate
get_rolling_rate <- function(ind, snap) {
  rate = c() ## Create vector
  
  rolling_sum = 0 ## Start with sum at zero
  
  for (i in 1:length(snap)) { ## At each snap number...
    rolling_sum = rolling_sum + ind[i] ## Get cumulative sum of values at snap i
    
    rolling_rate = rolling_sum / snap[i] ## Get rate at snap i
    
    rate = c(rate, rolling_rate) ## Add rate at snap i to vector
  }
  
  return(rate) ## Return vector of rolling pressure rates
}

# Function to get rolling/cumulative total pressures
get_rolling_total <- function(ind) {
  tot = c() ## Create vector
  
  rolling_sum = 0 ## Start with sum at zero
  
  for (i in 1:length(ind)) {
    rolling_sum = rolling_sum + ind[i] ## Get cumulative sum of values at snap i
    
    tot = c(tot, rolling_sum) ## Append sum into vector
  }
  
  return(tot) ## Return cumulative sum
}

# Function to convert game clock seconds from MM:SS format to raw number of seconds remaining at time
get_seconds <- function(time, qtr) {
  
  mins = 15*(4-qtr) + as.numeric(strsplit(time, ":")[[1]][1]) ## Convert MM value to numeric value
  secs = as.numeric(strsplit(time, ":")[[1]][2]) ## Convert SS value to numeric value
  
  return(mins*60 + secs) ## Return seconds remaining
}

# Add column containing the number of seconds remaining based on game clock time and quarter
pressure_stats$raw_secs <- unlist(mapply(get_seconds, pressure_stats$GameClock, qtr = pressure_stats$qtr))

# Data frame containing player's rolling pressure rate over the course of the season
test <- pressure_stats%>%
  group_by(Player, team, pass_rush_snap)%>%
  summarize(Player, team, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, 
            snap, game_snap, drive_snap, pass_rush_snap, game_pass_rush_snap, drive_pass_rush_snap)%>%
  mutate(rolling_pressure_rate = get_rolling_rate(PFFPressure, pass_rush_snap),
         rolling_pressure_total = get_rolling_total(PFFPressure))%>%
  ungroup()%>%
  arrange(Player, snap)

# Data frame containing player's rolling pressure rate over the course of each game 
test2 <- test%>%
  group_by(Player, team, week, game_pass_rush_snap)%>%
  summarize(Player, team, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, 
            game_pass_rush_snap, drive_pass_rush_snap)%>%
  mutate(game_rolling_pressure_rate = get_rolling_rate(PFFPressure, game_pass_rush_snap),
         game_rolling_pressure_total = get_rolling_total(PFFPressure))%>%
  ungroup()

# Data frame containing player's rolling pressure rate over the course of each drive
test3 <- test%>%
  group_by(Player, team, week, game_pass_rush_snap, drive_pass_rush_snap)%>%
  summarize(Player, team, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, drive_pass_rush_snap)%>%
  mutate(drive_rolling_pressure_rate = get_rolling_rate(PFFPressure, drive_pass_rush_snap),
         drive_rolling_pressure_total = get_rolling_total(PFFPressure))

# Join data frames together
pressure_data <- inner_join(test2, test3, by = join_by("Player", "team", "week", "qtr", "GameClock", "raw_secs", "Drpbk","PFFPressure", 
                                                       "game_pass_rush_snap", "drive_pass_rush_snap"))
pressure_data <- inner_join(test, pressure_data, by = join_by("Player", "team", "week", "qtr", "GameClock", 
                                                              "raw_secs", "Drpbk", "PFFPressure", 
                                                              "game_pass_rush_snap", "drive_pass_rush_snap"))
# Filter data to only contain pass rush snaps
pressure_data <- pressure_data%>%
  filter(Drpbk == 1)

# Join pressure data with team colors data
final_pressure_data <- pressure_data%>%
  left_join(load_teams(), by = c('team' = 'team_abbr'))

# Plot for Cumulative Pressure Rate at each snap
drive_rolling_pressure_rate_plot <- final_pressure_data%>%
  ggplot(aes(x = drive_pass_rush_snap, y = drive_rolling_pressure_rate, group = Player)) + ## Set axes and group by player
  geom_smooth(se = FALSE) + ## Hide confidence interval bands
  labs( ## Labels for axes, title, and caption
    x = "Pass Rush Snap Number on Drive",
    y = "Cumulative Pressure Rate",
    title = "Pass Rusher Pressure Rates Over the Course of a Drive (2023 Season Only)",
    caption = "Data via TruMedia, Player Names and Team Names Modified"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold") ## Adjust font size and alignment of title
  ) +
  scale_x_continuous(n.breaks = 11) + ## Set number of breaks to show each pass rush snap number
  theme(legend.position = "right") + ## Move legend to the right
  geom_smooth(aes(color = Player), size = 1.5, se = FALSE) + ## Assign legend points to each group
  scale_color_manual("",
                     values=c("#4F2683","#69be28", "#5A1414", "#007BC7")) ## Change colors and legend values

drive_rolling_pressure_rate_plot ## Display plot

# Obtain the average cumulative pressure rate at each snap
pressure_rate_drive_snap <- pressure_data%>%
  group_by(drive_pass_rush_snap)%>%
  summarize(n_plays = n(), rolling_pressure_rate = mean(drive_rolling_pressure_rate))

# ------------------------------------------------------------------------------------

# Load QB time to throw data
qb_data <- read.csv("qb_ttt_modified.csv")

# Get time to throw seconds 
get_secs <- function(ttt) {
  secs = as.numeric(strsplit(ttt, "s")[[1]][1])
}

qb_data$TTT = unlist(lapply(qb_data$TTT, get_secs)) ## Convert seconds to numeric form

qb_time_to_throw_downs <- qb_data%>%
  filter(!is.na(TTT))%>% ## Filter to make sure no NA's
  group_by(down)%>% ## Group by down
  summarize(avg_ttt = mean(TTT)) ## Get mean time to throw on each down
