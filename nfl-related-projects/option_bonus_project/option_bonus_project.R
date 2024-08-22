# Load packages
library(pdftools)
library(stringr)
library(nflreadr)
library(tm)
library(SnowballC)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(gtExtras)
library(rlang)

contracts <- nflreadr::load_contracts()%>%
  filter(is_active == TRUE)%>%
  mutate(player2 = stringr::word(toupper(removePunctuation(player)), 1, 2))%>%
  arrange(desc(apy_cap_pct))

rosters_23 <- nflreadr::load_rosters(seasons = 2023)

rosters_24 <- rosters_23%>%
  mutate(season = 2024)%>%
  rbind(rosters_23)

rosters2 <- nflreadr::load_rosters(seasons = min(contracts$year_signed):2022)%>%
  rbind(rosters_24)%>%
  mutate(full_name = stringr::word(toupper(removePunctuation(paste(football_name, last_name))), 1, 2))

contracts <- contracts%>%
  left_join(rosters2, by = join_by(player2 == full_name, year_signed == season, draft_year == entry_year))

contracts$age_at_signing = contracts$year_signed - as.numeric(str_split_fixed(contracts$birth_date, "-", 3)[,1])

contracts <- contracts%>%
  mutate(Rookie_Contract = ifelse(draft_year == year_signed, 1, 0),
         pct_guaranteed = guaranteed/value)

colnames(contracts)  
contracts <- contracts[,c(1:5,60,59,6:9,61,10:24,33,37:45,47:48,53:55)]%>%
  rename(position = position.x, team = team.x, height = height.x, weight = weight.x, college = college.x)%>%
  mutate(main_position = position)%>%
  mutate(main_position = case_when(position == "LT" | position == "RT" ~ "OT",
                              position == "LG" | position == "RG" ~ "OG",
                              position == "ED" ~ "EDGE",
                              .default = position))%>%
  mutate(position = main_position)%>%
  select(-main_position)

extract_contract_data_total <- function(contract_data, info) {
  contract_data <- data.frame(contract_data)
  
  return(contract_data[nrow(contract_data), info])
  
}

extract_active_contract_total <- function(contract_data, year_signed, info) {
  
  contract_data <- data.frame(contract_data)
  
  if (info %in% colnames(contract_data)) {
    info_col = contract_data[nrow(contract_data),info]
    
    if (is.na(sum(info_col))) {
      return(NA)
    }
    
    
    else {
      return(sum(info_col))
    }
  }
  
  else {
    return(NA)
  }
 }

contracts$option_bonus_sum = NA

for (i in 1:nrow(contracts)) {
  contracts$option_bonus_sum[i] = extract_active_contract_total(contract_data = contracts$cols[i],
                                                                year_signed = contracts$year_signed[i],
                                                                info = "option_bonus")
}

option_bonus_teams <- contracts%>%
  filter(!is.na(option_bonus_sum))%>%
  group_by(team)%>%
  summarize(count = n())%>%
  arrange(desc(count))

browns_option_bonuses <- contracts%>%
  filter(!is.na(option_bonus_sum), team == "Browns")

option_bonuses <- contracts%>%
  filter(!is.na(option_bonus_sum), player != "Russell Wilson")

option_bonuses%>%
  group_by(year_signed)%>%
  summarize(count = n())%>%
  arrange(desc(year_signed))

create_concat_string <- function(player_list) {
  
  player_list <- unlist(player_list)
  
  player_string <- ""
  for (i in player_list) {
    player_string <- paste(player_string, ", ", i, sep = "")
  }
  
  return(substr(player_string, 2, nchar(player_string)))
}

option_bonus_contracts <- contracts%>%
  filter(!is.na(option_bonus_sum), player != "Russell Wilson")%>%
  group_by(team)%>%
  summarize(team, player, count = n())%>%
  ungroup()%>%
  group_by(team, count)%>%
  summarize(team, count, player = list(player))%>%
  distinct(player)%>%
  mutate(team = case_when(team == "NYJ/PHI" ~ "Jets",
                            .default = team),
         player = create_concat_string(player))%>%
  arrange(desc(count))

nfl_option_bonus_table <- option_bonus_contracts%>%
  left_join(load_teams(), by = join_by(team == team_nick))%>%
  ungroup()%>%
  select(team_wordmark, count, player)%>%
  arrange(desc(count))%>%
  gt()%>%
  gt_img_rows(team_wordmark)%>%
  opt_align_table_header("center")%>%
  cols_align("center")%>%
  tab_source_note("Table: Charles Silkin | data: nflverse, OvertheCap")%>%
  cols_label(team_wordmark = "Team",
             count = "Count",
             player = "Players")%>%
  opt_row_striping()%>%
  tab_header(title = "Players with Option Bonuses in Their Contracts",
             subtitle = "as of March 27, 2024")%>%
  tab_style(style = cell_borders(
    sides = c("left", "right", "top", "bottom"),
    color = "black",
    weight = px(1),
    style = "solid"
  ),
  locations = cells_body()
  )%>%
  gt_theme_538()

nfl_option_bonus_table

sum(option_bonus_contracts$count)

idl_contracts <- contracts%>%
  filter(position == "IDL")
