# setwd("pass_rusher_production_project") # Set working directory

#### Load packages ####

library(magrittr)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(stringr)

#### Key helper functions ####

#' Get a cumulative percentage.
#' 
#' This function iterates through a pair of vectors. At each iteration, the current value in the first input vector is added to the sum of
#' the previous values in that vector, then divided by the value of the second vector at that iteration.
#' 
#' @param numerator_vec A vector containing values, where, at each iteration, the current value will be added to the previous values.
#' @param denominator_vec A vector containing the denominator value that the sum of the values in the numerator vector will be divided by.
#' 
#' @return A vector containing the percentages created by dividing the cumulative sum of the numerator vector by the current value in the
#' denominator vector.
get_rolling_rate <- function(numerator_vec, denominator_vec) {
  
  # Create a command to stop the function if both input vectors are not equal in length
  stopifnot(
    "STOP: Both input vectors must be of equal length!" = length(numerator_vec) == length(denominator_vec)
  )%>%
    try()
  
  rate = c() # Create vector to store the cumulative rate at each iteration
  
  rolling_sum = 0 # Create a variable to store the cumulative sum of the numerator vector, starting at zero.
  
  for (i in 1:length(numerator_vec)) { ## Iterate through the columns...
    
    ## Get cumulative sum of values at index i
    rolling_sum = rolling_sum + numerator_vec[i]
    
    ## Get rate at index i
    rolling_rate = rolling_sum / denominator_vec[i]
    
    ## Add rate at index i to vector
    rate = c(rate, rolling_rate)
  }
  
  return(rate) # Return vector of cumulative rates
}

#' Get a cumulative total.
#' 
#' This function iterates through a vector of values, adding the value at the current iteration to the sum of the values at previous
#' iterations.
#' 
#' @param index_vec A vector containing values where, at each iteration, the current value will be added to the previous values.
#' @return A vector containing the cumulative sum at each iteration of the input vector.
get_rolling_total <- function(index_vec) {
  
  tot = c() # Create vector to store the cumulative sum at each iteration
  
  rolling_sum = 0 # Create a variable to store the cumulative sum of the numerator vector, starting at zero.
  
  for (i in 1:length(index_vec)) {
    
    ## Get cumulative sum of values at index i
    rolling_sum = rolling_sum + index_vec[i]
    
    ## Add sum at index i to vector
    tot = c(tot, rolling_sum)
  }
  
  return(tot) # Return vector of cumulative sums
}

#' Convert game clock seconds from MM:SS to total seconds.
#' 
#' Obtain the total seconds left in the game based on the game clock and quarter of the game. Since 14:20 in the first quarter isn't the
#' same as 14:20 in the fourth quarter, lubridate functions won't work here.
#' 
#' @param time A character containing the game clock in MM:SS format.
#' @param qtr The quarter of the game.
#' 
#' @return The total seconds remaining in the game based on game clock and quarter.
get_seconds <- function(time, qtr) {
  
  # Convert the MM value on the game clock to numeric value
  mins = 15*(4-qtr) + as.numeric(stringr::str_split(time, ":")[[1]][1])
  
  # Convert the SS value on the game clock to numeric value
  secs = as.numeric(stringr::str_split(time, ":")[[1]][2])
  
  return(mins*60 + secs) # Return seconds remaining
}

#### Load play by play data ####

# Data initially from TruMedia; name and team of edge rushers de-identified.
pressure_stats <- readr::read_csv("edge_rusher_pressures_modified.csv")

#### Clean data ####

# Instead of "Yes" or NA, modify "PFFPressure" variable to be 0 or 1
pressure_stats <- pressure_stats%>%
  dplyr::arrange(team, week, PlayId)%>%
  dplyr::mutate(
    PFFPressure = dplyr::case_when(
      is.na(PFFPressure) ~ 0,
      TRUE ~ 1
    )
  )

# Add column containing the number of seconds remaining based on game clock time and quarter
pressure_stats$raw_secs <- unlist(mapply(get_seconds, pressure_stats$GameClock, qtr = pressure_stats$qtr))

##### Create variables for player's snap numbers #####

# Obtain a player's snap number, regardless of whether the play is a pass rush play or not
pressure_stats <- pressure_stats%>%
  ## Total snap number (EX: player's fifth snap of the season)
  dplyr::group_by(Player)%>%
  dplyr::mutate(
    snap = dplyr::row_number(Player)
  )%>%
  dplyr::ungroup()%>%
  ## Game snap number (EX: player's third snap of the game)
  dplyr::group_by(Player, week)%>%
  dplyr::mutate(
    game_snap = dplyr::row_number(week)
  )%>%
  dplyr::ungroup()%>%
  ## Drive snap number (EX: player's second snap of the drive)
  dplyr::group_by(Player, week, DriveNumber)%>%
  dplyr::mutate(
    drive_snap = dplyr::row_number(DriveNumber)
  )%>%
  dplyr::ungroup()

# Create a separate dataset to only contain plays where the QB dropped back to pass (classified as a pass rush snap)
# and obtain player snap numbers
psrsh_pressure_stats <- pressure_stats%>%
  ## Filter data to only contain plays where player rushes the passer
  dplyr::filter(Drpbk == 1)%>%
  ## Total pass rush snap number
  dplyr::group_by(Player)%>%
  dplyr::mutate(
    pass_rush_snap = dplyr::row_number(Player)
  )%>%
  dplyr::ungroup()%>%
  ## Game pass rush snap number
  dplyr::group_by(Player, week)%>%
  dplyr::mutate(
    game_pass_rush_snap = dplyr::row_number(week)
  )%>%
  dplyr::ungroup()%>%
  dplyr::group_by(Player, week, DriveNumber)%>%
  ## Drive pass rush snap number
  dplyr::mutate(
    drive_pass_rush_snap = dplyr::row_number(DriveNumber)
  )%>%
  dplyr::ungroup()

##### Create final dataset #####

pressure_stats <- pressure_stats%>%
  ## Use left join with just key joining variables and snap numbers from dataset of just pass rush snaps
  dplyr::left_join(
    psrsh_pressure_stats%>%
      dplyr::select(team, Player, snap, pass_rush_snap:drive_pass_rush_snap),
    by = dplyr::join_by(team, Player, snap)
  )

# Get rolling pressure rate and rolling pressure total for each player over the entire span of the dataset
rolling_pressure_stats_total <- pressure_stats%>%
  ## Filter to only be pass rush snaps
  dplyr::filter(Drpbk == 1)%>%
  ## Group by player, team, and snap number
  dplyr::group_by(Player, team, pass_rush_snap)%>%
  ## Add Pressures yes/no indicator column
  dplyr::summarize(PFFPressure)%>%
  ## Calculate rolling pressure rate and rolling pressure total
  dplyr::mutate(
    rolling_pressure_rate = get_rolling_rate(PFFPressure, pass_rush_snap),
    rolling_pressure_total = get_rolling_total(PFFPressure)
  )%>%
  ## Ungroup the grouping variables
  dplyr::ungroup()

# Get rolling pressure rate and rolling pressure total for each player per game
rolling_pressure_stats_game <- pressure_stats%>%
  ## Filter to only be pass rush snaps
  dplyr::filter(Drpbk == 1)%>%
  ## Group by player/team, week, and pass rush snap within the game
  dplyr::group_by(
    Player, team, week, game_pass_rush_snap
  )%>%
  ## Add Pressures yes/no indicator column
  dplyr::summarize(PFFPressure)%>%
  ## Calculate rolling pressure rate and rolling pressure total
  dplyr::mutate(
    game_rolling_pressure_rate = get_rolling_rate(PFFPressure, game_pass_rush_snap),
    game_rolling_pressure_total = get_rolling_total(PFFPressure)
  )%>%
  ## Ungroup the grouping variables
  ungroup()

# Get rolling pressure rate and rolling pressure total for each player per drive
rolling_pressure_stats_drive <- pressure_stats%>%
  dplyr::filter(Drpbk == 1)%>%
  ## Filter to only be pass rush snaps
  dplyr::filter(Drpbk == 1)%>%
  ## Group by player/team, week, drive number, and pass rush snap within the drive
  dplyr::group_by(Player, team, week, DriveNumber, drive_pass_rush_snap)%>%
  ## Add Pressures yes/no indicator column
  dplyr::summarize(PFFPressure)%>%
  ## Calculate rolling pressure rate and rolling pressure total
  dplyr::mutate(
    drive_rolling_pressure_rate = get_rolling_rate(PFFPressure, drive_pass_rush_snap),
    drive_rolling_pressure_total = get_rolling_total(PFFPressure)
  )%>%
  ## Ungroup the grouping variables
  ungroup()

# Create final pressure stats dataset by joining the rolling pressure datasets
final_pressure_stats <- pressure_stats%>%
  ## Rolling pressure rate/total overall
  dplyr::left_join(
    rolling_pressure_stats_total,
    by = dplyr::join_by(Player, team, pass_rush_snap, PFFPressure)
  )%>%
  ## Rolling pressure rate/total by game
  dplyr::left_join(
    rolling_pressure_stats_game,
    by = dplyr::join_by(Player, team, week, game_pass_rush_snap, PFFPressure)
  )%>%
  ## Rolling pressure rate/total by drive
  dplyr::left_join(
    rolling_pressure_stats_drive,
    by = dplyr::join_by(Player, team, week, DriveNumber, drive_pass_rush_snap, PFFPressure)
  )%>%
  ## Filter to only include pass rush snaps
  dplyr::filter(Drpbk == 1)

# Join pressure data with team colors data
final_pressure_stats <- final_pressure_stats%>%
  left_join(load_teams(), by = c('team' = 'team_abbr'))

#### Create plot for Cumulative Pressure Rate at each snap in a drive ####

drive_rolling_pressure_rate_plot <- final_pressure_stats%>%
  ## Set axes and group by player
  ggplot(
    aes(
      x = drive_pass_rush_snap,
      y = drive_rolling_pressure_rate,
      group = Player
    )
  ) +
  geom_smooth(se = FALSE) + ## Hide confidence interval bands
  ## Labels for axes, title, and caption
  labs(
    x = "Pass Rush Snap Number on Drive",
    y = "Cumulative Pressure Rate",
    title = "Pass Rusher Pressure Rates Over the Course of a Drive (2023 Season Only)",
    caption = "Data via TruMedia, Player Names and Team Names De-Identified"
  ) +
  ## Black and white background
  theme_bw() +
  ## Edit thematic elements
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold") ## Adjust font size and alignment of title
  ) +
  scale_x_continuous(n.breaks = 11) + ## Set number of breaks to show each pass rush snap number
  theme(legend.position = "right") + ## Move legend to the right
  geom_smooth(aes(color = Player), linewidth = 1.5, se = FALSE) + ## Assign legend points to each group
  scale_color_manual("",
                     values=c("#4F2683","#69be28", "#5A1414", "#007BC7")) ## Change colors and legend values

drive_rolling_pressure_rate_plot ## Display plot

# Obtain the average cumulative pressure rate at each snap
pressure_rate_drive_snap <- final_pressure_stats%>%
  group_by(drive_pass_rush_snap)%>%
  summarize(n_plays = n(), rolling_pressure_rate = mean(drive_rolling_pressure_rate))

# Obtain the average cumulative pressure rate at each snap for each player
pressure_rate_drive_snap_player <- final_pressure_stats%>%
  group_by(Player, drive_pass_rush_snap)%>%
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
