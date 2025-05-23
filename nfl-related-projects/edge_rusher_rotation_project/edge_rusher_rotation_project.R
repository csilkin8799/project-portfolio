# Load packages
library(pdftools)
library(stringr)
library(nflreadr)
library(nflplotR)
library(nflfastR)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(gt)
library(gtExtras)
library(rlang)

off_personnel_short <- function(group) {
  
  if (is.na(group) == TRUE | group == "") {
    return(NA)
  }
  
  if(as.numeric(stringr::str_sub(group, end = 1)) >= 6) {
    group <- stringr::str_sub(group, 7,22)
  }
  
  group <- stringr::str_sub(group, end = 16)
  
  num_rb <- strsplit(group, ", ")[[1]][1]
  num_te <- strsplit(group, ", ")[[1]][2]
  num_wr <- strsplit(group, ", ")[[1]][3]
  
  return(paste(substr(num_rb,1,1), substr(num_te,1,1), substr(num_wr,1,1), sep = "-"))
}

def_personnel_short <- function(group) {
  
  if (is.na(group) == TRUE | group == "") {
    return(NA)
  }
  
  group <- stringr::str_sub(group, end = 16)
  
  num_dl <- strsplit(group, ", ")[[1]][1]
  num_lbs <- strsplit(group, ", ")[[1]][2]
  num_dbs <- strsplit(group, ", ")[[1]][3]
  
  return(paste(substr(num_dl,1,1), substr(num_lbs,1,1), substr(num_dbs,1,1), sep = "-"))
}

def_coverage <- function(group) {
  
  if (is.na(group) == TRUE | group == "") {
    return(NA)
  }
  
  group <- stringr::str_sub(group, end = 16)
  
  num_dbs <- strsplit(group, ", ")[[1]][3]
  num_dbs <- as.numeric(substr(num_dbs,1,1))
  
  if(num_dbs < 4 | num_dbs > 7) {
    return("OTHER")
  }
  
  if (num_dbs == 4) {
    return("BASE")
  }
  
  if(num_dbs == 5) {
    return("NICKEL")
  }
  
  if(num_dbs == 6) {
    return("DIME")
  }
  
  if(num_dbs == 7) {
    return("QUARTER")
  }
  
}

ret_player_names <- function(id_list){
  
  player_ids <- nflreadr::load_players()
  
  main_list <- c()
  
  for (id in id_list) {
    if (is.na(id) == TRUE | startsWith(id, "00-") == FALSE) {
      main_list <- c(main_list, NA) ## FIX --> Return vector w/ 2 empty strings instead of NA
    }
    
    player_name = na.omit(player_ids$display_name[player_ids$gsis_id == id])[1]
    
    main_list <- c(main_list, player_name)
  }
  
  return(main_list)
}

ret_player_position_list <- function(id_list){
  
  player_ids <- nflreadr::load_players()
  
  main_list <- c()
  
  for (id in id_list) {
    if (is.na(id) == TRUE | startsWith(id, "00-") == FALSE) {
      main_list <- c(main_list, NA) ## FIX --> Return vector w/ 2 empty strings instead of NA
    }
    
    player_name = na.omit(player_ids$position[player_ids$gsis_id == id])[1]
    
    main_list <- c(main_list, player_name)
  }
  
  return(main_list)
}

player_ids <- nflreadr::load_players()

ret_player_position <- function(id) {
  
  if (is.na(id) == TRUE | startsWith(id, "00-") == FALSE) {
    return(NA)
  }
  
  player_name = na.omit(player_ids$position[player_ids$gsis_id == id])[1]
  
  return(player_name)
}

modified_pass_length <- function(air_yds) {
  
  if(is.na(air_yds)) {
    return(NA)
  }
  
  else {
    if(air_yds < 0){
      return("Behind LOS")
    }
    
    if(air_yds >= 0 && air_yds < 10) {
      return("Short")
    }
    
    if(air_yds >= 10 && air_yds < 20) {
      return("Medium")
    }
    
    if(air_yds >= 20) {
      return("Deep")
    }
  }
}

ftn_charting_data <- nflreadr::load_ftn_charting(seasons = TRUE)

pbp_data <- nflreadr::load_participation(seasons = TRUE, include_pbp = TRUE)

pbp_data$off_personnel_abbr <- unlist(lapply(pbp_data$offense_personnel, off_personnel_short))
pbp_data$def_personnel_abbr <- unlist(lapply(pbp_data$defense_personnel, def_personnel_short))
pbp_data$coverage_scheme <- unlist(lapply(pbp_data$defense_personnel, def_coverage))
colnames(pbp_data)
pbp_data <- pbp_data[,c(1:5,390,6:7,391:392,8:389)]

pbp_data <- full_join(pbp_data, ftn_charting_data, 
                      by = join_by("nflverse_game_id" == "nflverse_game_id",
                                   "play_id" == "nflverse_play_id", "season", "week"))

pbp_data$explosive_play = 0
pbp_data$explosive_play[pbp_data$rushing_yards >= 10] = 1
pbp_data$explosive_play[pbp_data$passing_yards >= 20] = 1

pbp_data$blitz = 0
pbp_data$blitz[pbp_data$n_blitzers > 0] = 1

pbp_data$pass_length_mod <- unlist(lapply(pbp_data$air_yards, modified_pass_length))

reg_szn_pbp_data <- pbp_data%>%
  filter(season_type == "REG")

data_23 <- reg_szn_pbp_data%>%
  filter(season == 2023)

varnames = nflfastR::field_descriptions # Variable names for nflfastR play-by-play repository

personnel_dict <- nflreadr::dictionary_participation # Variable names for NFL personnel grouping data

rosters <- load_rosters(2023)

rosters <- rosters%>% # Adjust positions in roster data
  mutate(main_position = case_when(
    depth_chart_position == "ILB" | depth_chart_position == "MLB" ~ "LB",
    depth_chart_position == "OLB" | depth_chart_position == "DE" ~ "EDGE",
    depth_chart_position == "DB" ~ "CB",
    depth_chart_position == "DT" | depth_chart_position == "NT" ~ "iDL",
    depth_chart_position == "FS" | depth_chart_position == "SS" ~ "S",
    .default = depth_chart_position
  ))

data_23 <- data_23%>%
  filter(week <= 15) ## nflverse stopped producing participation data for all games, just primetime after Week 15

real_play_data <- data_23%>%
  filter(!is.na(def_personnel_abbr), !is.na(epa), !(pass == 0 & rush == 0 & penalty == 1))%>%
  select(old_game_id, play_id, posteam, defteam, week, 
         offense_players, off_personnel_abbr, defense_players, def_personnel_abbr, down, ydstogo, 
         qtr, game_half, drive, quarter_seconds_remaining, half_seconds_remaining, game_seconds_remaining, desc)%>%
  filter(qtr != 5, half_seconds_remaining > 120)%>%
  mutate(player_in = 0)

edge_rushers_23 <- rosters%>%
  filter(main_position == "EDGE")%>%
  select(season, full_name, team, main_position, depth_chart_position,gsis_id, pff_id)

for (i in 1:nrow(edge_rushers_23)) {
  
  snap_info <- real_play_data%>%
    filter(defteam == edge_rushers_23$team[i])
  
  snap_info <- snap_info%>%
    mutate(player_in = ifelse(is.na(str_extract(defense_players, edge_rushers_23$gsis_id[i])), 0, 1))%>%
    group_by(week, drive)%>%
    arrange(week, drive)%>%
    mutate(status_change = player_in - lag(player_in))%>%
    mutate(rotation = ifelse(is.na(status_change) | status_change == 0, 0, 1))
  
  edge_rushers_23$snaps[i] = sum(snap_info$player_in)
  edge_rushers_23$snap_pct[i] = mean(snap_info$player_in)
  edge_rushers_23$rotations[i] = sum(snap_info$rotation)
  edge_rushers_23$rotation_rate[i] = edge_rushers_23$rotations[i] / edge_rushers_23$snaps[i]
  
}

# NOTE: Difference in % of snaps rotated (rotations/snaps) and rotation rate (% of team's snaps where player rotated = mean(rotations))
mean(edge_rushers_23$snap_pct)

final_edges <- edge_rushers_23%>%
  filter(snap_pct >= 0.25)%>%
  left_join(load_teams(),
            by = join_by(team == team_abbr))

rotation_snaps_plot <- final_edges%>%
  ggplot(aes(x = rotation_rate, y = snaps)) +
  geom_hline(yintercept = mean(final_edges$snaps), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(final_edges$rotation_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.02, alpha = 0.7) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=full_name)) +
  labs( ## Labels for axes, title, and caption
    x = "Rotation Rate",
    y = "Number of Snaps",
    title = "How Often Did NFL EDGE Rushers Rotate in 2023?",
    subtitle = "Weeks 1-15 Only, Min. 25% of Team's Snaps, Excluding Under 2 Minute Warning and OT",
    caption = "Charles Silkin | Data: nflreadr"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    plot.subtitle = element_text(size = 10, hjust = 0.5) ## Adjust font size and alignment of subtitle
  ) +
  scale_x_reverse(labels = scales::percent_format(accuracy = 1))

rotation_snaps_plot_2 <- final_edges%>%
  ggplot(aes(x = rotation_rate, y = snaps)) +
  geom_hline(yintercept = mean(final_edges$snaps), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(final_edges$rotation_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(fill = final_edges$team_color, color = final_edges$team_color2, pch = 21, size = 5) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=full_name), box.padding = 1.125) +
  annotate(geom = "label", x = 0.045, y = 700, label = "Primary Players", fontface = "bold") +
  annotate(geom = "label", x = 0.25, y = 575, label = "Quality Role Players", fontface = "bold") +
  annotate(geom = "label", x = 0.26, y = 225, label = "Rotational Players", fontface = "bold") +
  labs( ## Labels for axes, title, and caption
    x = "Rotation Rate (% of Plays When Player Came In or Out of Game)",
    y = "Number of Snaps",
    title = "Rotation Rates of NFL EDGE Rushers",
    subtitle = "2023 Weeks 1-15 Only, Min. 25% of Team's Snaps, Excluding Under 2 Minute Warning and OT",
    caption = "Charles Silkin | Data: nflreadr"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    plot.subtitle = element_text(size = 10, hjust = 0.5) ## Adjust font size and alignment of subtitle
  ) +
  scale_x_reverse(labels = scales::percent_format(accuracy = 1))

rotation_snaps_plot_2

eagles_jets_comp <- edge_rushers_23%>%
  left_join(load_teams(),
            by = join_by(team == team_abbr))%>%
  filter(team == "PHI" | team == "NYJ", snaps >= 100)%>%
  ggplot(aes(x = rotation_rate, y = snaps)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.04, alpha = 0.7) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=full_name), force = 8, box.padding = 1.5) +
  labs( ## Labels for axes, title, and caption
    x = "Rotation Rate (% of Player's Snaps When Player Came In or Out of Game)",
    y = "Number of Snaps",
    title = "Rotation Rates of Eagles and Jets EDGE Rushers",
    subtitle = '2023 Weeks 1-15 Only, Min. 100 Snaps, Excluding Under Two Minute Warning and OT | EDGE Rusher = Player Designated as "DE" or "OLB" by Pro-Football-Reference',
    caption = "Charles Silkin | Data: nflreadr"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    plot.subtitle = element_text(size = 10, hjust = 0.5) ## Adjust font size and alignment of subtitle
  ) +
  scale_x_reverse(labels = scales::percent_format(accuracy = 1))

eagles_jets_comp

vikings_edges <- final_edges%>%
  filter(team == "MIN" | full_name %in% c("Jonathan Greenard", "Andrew Van Ginkel", "Jihad Ward", "Jonah Williams"))

rotation_snaps_plot_copy <- vikings_edges%>%
  ggplot(aes(x = rotation_rate, y = snaps)) +
  geom_hline(yintercept = mean(final_edges$snaps), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(final_edges$rotation_rate), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(fill = vikings_edges$team_color, color = vikings_edges$team_color2, pch = 21, size = 5) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=full_name), force = 10, box.padding = 1.75) +
  labs( ## Labels for axes, title, and caption
    x = "Rotation Rate (% of Plays When Player Came In or Out of Game)",
    y = "Number of Snaps",
    title = "Comparing Rotation Rates of 2023 and 2024 Minnesota Vikings EDGE Rushers",
    subtitle = "2023 Weeks 1-15 Only, Min. 25% of Team's Snaps, Excluding Under 2 Minute Warning and OT",
    caption = "Charles Silkin | Data: nflreadr"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    plot.subtitle = element_text(size = 10, hjust = 0.5) ## Adjust font size and alignment of subtitle
  ) +
  scale_x_reverse(labels = scales::percent_format(accuracy = 1))

rotation_snaps_plot_copy

avg_rotation_rates <- edge_rushers_23%>%
  group_by(team)%>%
  summarize(avg_rotation_rates = mean(rotations))%>%
  arrange(desc(avg_rotation_rates))

edge_rushers_23%>%
  filter(team == "DET")

median(edge_rushers_23$rotation_rate)
mean(edge_rushers_23$rotation_rate)
#' Obtain Playing Time Patterns for NFL Defensive Players
#'
#' Extracts information about playing time stats from already-loaded participation data using NGS IDs from roster data.
#' @param first An object of class "string". Next Gen Stats ID associated with the player.
#' @param second The player's team
#' @param third An object of class "string". One of "snaps", "snap_pct", "rotations", "rotation_rate"
#' @return Returns number associated with the third parameter

get_defensive_playtime_pattern <- function(id, team, interest_var, data = real_play_data) {
  snaps_info <- data%>%
    filter({{"defteam"}} == team)%>%
    mutate({{"player_in"}} := ifelse(is.na(str_extract({{"defense_players"}}, id)), 0, 1))%>%
    group_by({{"week"}}, {{"drive"}})%>%
    arrange({{"week"}}, {{"drive"}})%>%
    mutate({{"status_change"}} := {{"player_in"}} - lag({{"player_in"}}))%>%
    mutate({{"rotation"}} := ifelse(is.na({{"status_change"}}) | {{"status_change"}} == 0, 0, 1))
  
  if (interest_var == "snaps") {
    return(sum(snaps_info$player_in))
  }
  
  if (interest_var == "snap_pct") {
    return(mean(snaps_info$player_in))
  }
  
  if (interest_var == "rotations") {
    return(sum(snaps_info$rotation))
  }
  
  if (interest_var == "rotation_rate") {
    return(mean(snaps_info$rotation))
  }
}

edge_rushers_23 <- unlist(sapply(edge_rushers_23$gsis_id, get_defensive_playtime_pattern, team = edge_rushers_23$team, interest_var = "snaps"))

test3 <- mcdonald_in%>%
  filter(week == 15, mcdonald_in == 1)

for (i in 1:15) {
  print(nrow(mcdonald_in%>%
               filter(week == i, mcdonald_in == 1)))
}

rosters%>%
  filter(full_name == "Jermaine Johnson")%>%
  pull(gsis_id)

test_data <- real_play_data%>%
  filter(defteam == "NYJ")%>%
  arrange(week, game_half, play_id)%>%
  mutate(jermaine_in = ifelse(is.na(str_extract(defense_players, "00-0037663")), 0, 1))

test_rotation <- test_data%>%
  group_by(week, drive)%>%
  arrange(week, drive)%>%
  mutate(status_change = jermaine_in - lag(jermaine_in))%>%
  mutate(rotation = ifelse(is.na(status_change) | status_change == 0, 0, 1))

rotation_rate = mean(test_rotation$rotation)
sum(test_rotation$rotation)