library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(stringr)

pressure_stats <- read.csv("edge_rusher_pressures.csv")

pressure_stats <- pressure_stats%>%
  arrange(team, week, PlayId)%>%
  mutate(PFFPressure = if_else(PFFPressure == "", 0, 1), 
         PFFHit = if_else(PFFHit == "", 0, 1),
         Hurried = if_else(Hurried == "true", 1, 0))

pressure_stats <- pressure_stats%>%
  group_by(Player)%>%
  mutate(snap = row_number(Player))%>%
  ungroup()%>%
  group_by(Player, week)%>%
  mutate(game_snap = row_number(week))%>%
  ungroup()%>%
  group_by(Player, week, DriveNumber)%>%
  mutate(drive_snap = row_number(DriveNumber))%>%
  ungroup()

psrsh_pressure_stats <- pressure_stats%>%
  filter(Drpbk == 1)%>%
  group_by(Player)%>%
  mutate(pass_rush_snap = row_number(Player))%>%
  ungroup()%>%
  group_by(Player, week)%>%
  mutate(game_pass_rush_snap = row_number(week))%>%
  ungroup()%>%
  group_by(Player, week, DriveNumber)%>%
  mutate(drive_pass_rush_snap = row_number(DriveNumber))%>%
  ungroup()

non_pressure_stats <- pressure_stats%>%
  filter(Drpbk == 0)

pressure_stats <- full_join(psrsh_pressure_stats, non_pressure_stats)%>%
  filter(Drpbk == 1)

get_rolling_rate <- function(ind, snap) {
  rate = c()
  
  rolling_sum = 0
  
  for (i in 1:length(snap)) {
    rolling_sum = rolling_sum + ind[i]
    
    rolling_rate = rolling_sum / snap[i]
    
    rate = c(rate, rolling_rate)
  }
  
  return(rate)
}

get_rolling_total <- function(ind) {
  tot = c()
  
  rolling_sum = 0
  
  for (i in 1:length(ind)) {
    rolling_sum = rolling_sum + ind[i]
    
    tot = c(tot, rolling_sum)
  }
  
  return(tot)
}

get_seconds <- function(time, qtr) {
  
  mins = 15*(4-qtr) + as.numeric(strsplit(time, ":")[[1]][1])
  secs = as.numeric(strsplit(time, ":")[[1]][2])
  
  return(mins*60 + secs)
}

pressure_stats$raw_secs <- unlist(mapply(get_seconds, pressure_stats$GameClock, qtr = pressure_stats$qtr))

test <- pressure_stats%>%
  group_by(Player, team, pass_rush_snap)%>%
  summarize(Player, team, opponent, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, 
            snap, game_snap, drive_snap, pass_rush_snap, game_pass_rush_snap, drive_pass_rush_snap)%>%
  mutate(rolling_pressure_rate = get_rolling_rate(PFFPressure, pass_rush_snap),
         rolling_pressure_total = get_rolling_total(PFFPressure))%>%
  ungroup()%>%
  arrange(Player, snap)

test2 <- test%>%
  group_by(Player, team, week, game_pass_rush_snap)%>%
  summarize(Player, team, opponent, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, 
            game_pass_rush_snap, drive_pass_rush_snap)%>%
  mutate(game_rolling_pressure_rate = get_rolling_rate(PFFPressure, game_pass_rush_snap),
         game_rolling_pressure_total = get_rolling_total(PFFPressure))%>%
  ungroup()

test3 <- test%>%
  group_by(Player, team, week, game_pass_rush_snap, drive_pass_rush_snap)%>%
  summarize(Player, team, opponent, week, qtr, GameClock, raw_secs, Drpbk, PFFPressure, drive_pass_rush_snap)%>%
  mutate(drive_rolling_pressure_rate = get_rolling_rate(PFFPressure, drive_pass_rush_snap),
         drive_rolling_pressure_total = get_rolling_total(PFFPressure))

pressure_data <- inner_join(test2, test3, by = join_by("Player", "team", "week", "opponent", 
                                                       "qtr", "GameClock", "raw_secs", "Drpbk","PFFPressure", 
                                                       "game_pass_rush_snap", "drive_pass_rush_snap"))
pressure_data <- inner_join(test, pressure_data, by = join_by("Player", "team", "week", "opponent", 
                                                      "qtr", "GameClock", "raw_secs", "Drpbk", "PFFPressure", 
                                                      "game_pass_rush_snap", "drive_pass_rush_snap"))

pressure_data <- pressure_data%>%
  filter(Drpbk == 1)

final_pressure_data <- pressure_data%>%
  left_join(load_teams(), by = c('team' = 'team_abbr'))

rolling_pressure_rate_plot <- final_pressure_data%>%
  ggplot(aes(x = pass_rush_snap, y = rolling_pressure_rate, group = Player)) +
  geom_line() +
  labs(
    x = "Pass Rush Snaps",
    y = "Pressure Rate",
    title = "Rolling Pressure Rate of Top Edge Rushers",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  theme(legend.position = "right") +
  geom_line(aes(color = Player), size = 1) +
  scale_color_manual("",
                     values=c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))

rolling_pressure_rate_plot

rolling_pressures_plot <- final_pressure_data%>%
  ggplot(aes(x = pass_rush_snap, y = rolling_pressure_total, group = Player)) +
  geom_line() +
  labs(
    x = "Pass Rush Snaps",
    y = "Pressures",
    title = "Rolling Pressures of Top Edge Rushers",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  theme(legend.position = "right") +
  geom_line(aes(color = Player), size = 1.5) +
  scale_color_manual("",
                     values=c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))
rolling_pressures_plot

pressure_data_wrt_opponent <- pressure_data%>%
  left_join(load_teams(), by = c('opponent' = 'team_abbr'))

micah_game_rolling_pressure_rate <- pressure_data_wrt_opponent%>%
  filter(Player == "Micah Parsons")%>%
  ggplot(aes(x = raw_secs, y = game_rolling_pressure_rate, group = team_nick)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Game Seconds Remaining",
    y = "Pressure Rate",
    title = "Micah Parsons Rolling Pressure Rate in Games",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_x_reverse() +
  theme(legend.position = "right") +
  geom_smooth(aes(color = team_nick), size = 1, se = FALSE) +
  scale_color_manual("Opponent", values = c("#AA0000", "#97233F", "#007BC7","#0B2265", "#003F2D", "#002244"))
micah_game_rolling_pressure_rate

unique(pressure_data_wrt_opponent[pressure_data_wrt_opponent$Player == "Micah Parsons",]$team_color)

avg_game_rolling_pass_rate_snap_num <- pressure_data%>%
  group_by(Player, team, game_pass_rush_snap)%>%
  mutate(avg_rolling_pressure_rate = mean(game_rolling_pressure_rate))%>%
  ungroup()

avg_qtr_pressure_rate <- pressure_data%>%
  group_by(Player, team, qtr)%>%
  summarize(avg_pressure_rate = mean(Pressure))


game_time_rolling_pass_rate <- avg_game_rolling_pass_rate_snap_num%>%
  ggplot(aes(x = raw_secs, y = avg_rolling_pressure_rate, group = Player)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Game Seconds Remaining",
    y = "Average Pressure Rate",
    title = "Average Rolling Pressure Rate in Games",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_x_reverse() +
  theme(legend.position = "right") +
  geom_smooth(aes(color = Player), size = 1.5, se = FALSE) +
  scale_color_manual("", values = c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))

game_time_rolling_pass_rate

time_rolling_pressures_plot <- final_pressure_data%>%
  ggplot(aes(x = raw_secs, y = game_rolling_pressure_total, group = Player)) +
  geom_line() +
  labs(
    x = "Game Seconds Remaining",
    y = "Pressures",
    title = "Rolling Pressures of Top Edge Rushers",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  theme(legend.position = "right") +
  scale_x_reverse() +
  geom_line(aes(color = Player), size = 1.5) +
  scale_color_manual("",
                     values=c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))
time_rolling_pressures_plot

time_rolling_pressure_rate_plot <- final_pressure_data%>%
  ggplot(aes(x = raw_secs, y = game_rolling_pressure_rate, group = Player)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Seconds Left in Game",
    y = "Pressure Rate",
    title = "Pass Rusher Pressure Rates Over the Course of the Game",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  theme(legend.position = "right") +
  scale_x_reverse() +
  geom_smooth(aes(color = Player), size = 1.5, se = FALSE) +
  scale_color_manual("",
                     values=c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))
time_rolling_pressure_rate_plot

drive_rolling_pressure_rate_plot <- final_pressure_data%>%
  ggplot(aes(x = drive_pass_rush_snap, y = drive_rolling_pressure_rate, group = Player)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Pass Rush Snap Number on Drive",
    y = "Cumulative Pressure Rate",
    title = "Pass Rusher Pressure Rates Over the Course of a Drive (2023 Season Only)",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_x_continuous(n.breaks = 11) +
  theme(legend.position = "right") +
  geom_smooth(aes(color = Player), size = 1.5, se = FALSE) +
  scale_color_manual("",
                     values=c("#A5ACAF","#002244", "#FF3C00", "#FFB612"))
  
drive_rolling_pressure_rate_plot

micah_game_rolling_pressures <- pressure_data_wrt_opponent%>%
  filter(Player == "Micah Parsons")%>%
  ggplot(aes(x = raw_secs, y = game_rolling_pressure_total, group = team_nick)) +
  geom_line() +
  labs(
    x = "Game Seconds Remaining",
    y = "Pressures",
    title = "Micah Parsons Rolling Pressure Rate in Games",
    caption = "Data via TruMedia"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  scale_x_reverse() +
  theme(legend.position = "right") +
  geom_line(aes(color = team_nick), size = 1.5) +
  scale_color_manual("Opponent", values = c("#AA0000", "#97233F", "#007BC7","#0B2265", "#003F2D", "#002244"))
micah_game_rolling_pressures

pass_rush_drive_snap_nums <- pressure_data%>%
  group_by(drive_snap, drive_pass_rush_snap)%>%
  summarize(n_snaps = n())

qb_data <- read.csv("qb_ttt.csv")

get_secs <- function(ttt) {
  secs = as.numeric(strsplit(ttt, "s")[[1]][1])
}

qb_data$TTT = unlist(lapply(qb_data$TTT, get_secs))
qb_data$raw_secs <- unlist(mapply(get_seconds, qb_data$GameClock, qtr = qb_data$qtr))

unique(qb_data$qtr)

qb_data <- qb_data%>%
  mutate(inside_two_mins = ifelse((raw_secs <= 1800 + 120 & raw_secs > 1800) | (raw_secs <= 120 & raw_secs > 0) |
                                    raw_secs <= -900+120, 
                                   "Inside 2 mins", "Regular Time"))

qb_time_to_throw_comps <- qb_data%>%
  filter(!is.na(TTT))%>%
  group_by(inside_two_mins)%>%
  summarize(avg_ttt = mean(TTT))

pressure_data <- pressure_data%>%
  mutate(inside_two_mins = ifelse((raw_secs <= 1800 + 120 & raw_secs > 1800) | (raw_secs <= 120 & raw_secs > 0) |
                                    raw_secs <= -900+120, 
                                  "Inside 2 mins", "Regular Time"))

pressure_rate_drive_snap <- pressure_data%>%
  group_by(drive_snap)%>%
  summarize(n_plays = n(), rolling_pressure_rate = mean(drive_rolling_pressure_rate))

pressure_rate_drive_snap_2 <- pressure_data%>%
  group_by(drive_pass_rush_snap)%>%
  summarize(pass_rushes = n(), 
            pressure_rate = mean(PFFPressure))

pressure_rate_drive_snap_3 <- pressure_data%>%
  group_by(Player, drive_pass_rush_snap)%>%
  summarize(n_plays = n(), rolling_pressure_rate = mean(drive_rolling_pressure_rate))
