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

file_path <- "C:/Users/Student/Desktop/Personal Stuff/Portfolio/Charlie 2023-2024 Job Search/summer_nfl_content/sneed_cb_project/pff_cb_data"

pff_cb_data_20 <- read.csv(paste(file_path, "pff_cb_data_20.csv", sep = "/"))%>%
  filter(position == "CB")

rosters_20 <- nflreadr::load_rosters(seasons = 2020)%>%
  mutate(pff_id = as.numeric(pff_id))

test <- inner_join(pff_cb_data_20, rosters_20, by = join_by("player_id" == "pff_id"))

final_cb_df <- data.frame()

for (i in 2014:2023) {
  year_short = substr(as.character(i),3,4)
  pff_data_file = paste(year_short, "csv", sep = ".")
  pff_data_file = paste("pff_cb_data", pff_data_file, sep = "_")
  
  pff_data <- read.csv(paste(file_path, pff_data_file, sep = "/"))%>%
    filter(position == "CB")
  rosters_data <- nflreadr::load_rosters(seasons = i)%>%
    mutate(pff_id = as.numeric(pff_id))
  
  joined_df <- inner_join(pff_data, rosters_data, by = join_by("player_id" == "pff_id", "position" == "depth_chart_position"))
  
  final_cb_df <- rbind(final_cb_df, joined_df)
}

final_cb_df$season_age = final_cb_df$season - as.numeric(str_split_fixed(final_cb_df$birth_date, "-", 3)[,1])
colnames(final_cb_df)

final_cb_df%>%
  filter(season_age <= 32, snap_counts_coverage >= 250)%>%
  group_by(season_age)%>%
  summarize(count = n(), avg_snaps = mean(snap_counts_coverage), 
            cov_grade = round(mean(grades_coverage_defense),1), qb_rating_against = mean(qb_rating_against))%>%
  arrange(season_age)%>%
  select(season_age, count, cov_grade)%>%
  ungroup()%>%
  gt()%>%
  data_color(
    columns = cov_grade,
    method = "numeric",
    palette = c("#FA8072", "#5CED73")
  )%>%
  opt_align_table_header("center")%>%
  cols_align("center")%>%
  tab_source_note("Table: Charles Silkin | data: PFF")%>%
  cols_label(season_age = "Age",
             count = "Count",
             cov_grade = "Mean Coverage Grade")%>%
  opt_row_striping()%>%
  tab_header(title = "PFF Coverage Grades for Cornerbacks by Age",
             subtitle = "2014-2023 Seasons, Min. 250 Coverage Snaps")%>%
  tab_style(style = cell_borders(
    sides = c("left", "right", "top", "bottom"),
    color = "black",
    weight = px(1),
    style = "solid"
  ),
  locations = cells_body()
  )%>%
  gt_theme_538()

final_cb_df <- final_cb_df%>%
  filter(season_age <= 32, snap_counts_coverage >= 250)%>%
  select(player, player_id, position, team_name, season, season_age, player_game_count, snap_counts_coverage,
         grades_defense, grades_coverage_defense, grades_run_defense, grades_tackle, missed_tackle_rate, qb_rating_against)%>%
  group_by(player_id)%>%
  arrange(player_id, season_age)%>%
  mutate(age_delta = season_age - lag(season_age))%>%
  ungroup()


final_cb_df <- final_cb_df%>%
  filter(age_delta <= 1 | is.na(age_delta))

test2 <- final_cb_df[duplicated(final_cb_df$player_id),]$player_id

final_cb_df <- final_cb_df[final_cb_df$player_id %in% test2,]

final_cb_df_test <- final_cb_df%>%
  group_by(player_id)%>%
  arrange(player_id, season_age)%>%
  mutate(coverage_grade_delta = grades_coverage_defense - lag(grades_coverage_defense),
         weighted_cvg_grade_delta = ((grades_coverage_defense * snap_counts_coverage) - 
           (lag(grades_coverage_defense) * lag(snap_counts_coverage))) / (snap_counts_coverage + lag(snap_counts_coverage)),
         pff_grade_delta = grades_defense - lag(grades_defense))%>%
  ungroup()%>%
  filter(!is.na(coverage_grade_delta), !is.na(weighted_cvg_grade_delta), !is.na(pff_grade_delta))

cb_deltas <- final_cb_df_test%>%
  group_by(season_age)%>%
  summarize(n_cbs = n(), avg_snaps = mean(snap_counts_coverage), mean_cvg_delta = mean(coverage_grade_delta), 
            mean_weighted_cvg_delta = mean(weighted_cvg_grade_delta),
            mean_cvg_grade = mean(grades_coverage_defense),
            mean_grade_delta = mean(pff_grade_delta), mean_grade = mean(grades_defense))



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

cb_deltas <- cb_deltas%>%
  mutate(rolling_delta = get_rolling_total(mean_cvg_delta),
         rolling_weighted_delta = get_rolling_total(mean_weighted_cvg_delta))

cb_aging_curve_graph_unweighted <- cb_deltas%>%
  ggplot(aes(x = season_age, y = rolling_delta)) + 
  geom_line(color = "#4F2683") + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  labs( ## Labels for axes, title, and caption
    x = "Age",
    y = "Coverage Grade Delta",
    title = "Cornerback Aging Curve, 2014-2023 Seasons",
    caption = "Charles Silkin | Data: PFF"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(n.breaks = 10)

cb_aging_curve_graph_weighted <- cb_deltas%>%
  ggplot(aes(x = season_age, y = rolling_weighted_delta)) + 
  geom_line(color = "#007BC7") + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  labs( ## Labels for axes, title, and caption
    x = "Age",
    y = "Coverage Grade Delta (Weighted by Snap Count)",
    title = "Weighted Cornerback Aging Curve, 2014-2023 Seasons",
    caption = "Charles Silkin | Data: PFF"
  ) +
  theme_bw() + ## Black and white background
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), ## Adjust font size and alignment of title
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(n.breaks = 10)
cb_aging_curve_graph_unweighted
