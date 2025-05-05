#### Set working directory ####

setwd("C:/Users/Student/Desktop/project-portfolio/nfl-related-projects/nfl_draft_predictive_big_board_project")

#### Install/load packages ####

# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("nflreadr")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("gt")
# install.packages("gtExtras")
# install.packages("pacman")
# pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
library(pdftools)
library(stringr)
library(nflreadr)
library(tm)
library(SnowballC)
library(ggplot2)
library(tidyr)
library(gt)
library(gtExtras)
library(cfbfastR)

Sys.setenv(CFBD_API_KEY = "e1YWURnM4kNo+ywslr0qJxr55RYGhgRG2isA8SDcKzSPeIXOCmL8WCNXRLoK3XUr")

# Function to convert height from "FIEE" format to numeric inches
fiie_height_to_inches <- function(FIEE) {
  feet = as.numeric(stringr::str_sub(FIEE, end = 1))*12
  inches = as.numeric(stringr::str_sub(FIEE, start = 2, end = 3))
  eighths_inches = as.numeric(stringr::str_sub(FIEE, start = 4))/8
  
  return(feet + inches + eighths_inches)
}

#### Load "The Beast" 2019 ####

# Load PDF of "The Beast"
beast_2019 <- pdftools::pdf_text(file.path(getwd(), "the_beast_info", "beast_2019.pdf"), opw = "draftguide2019")

# Create empty string and reformat text as one large string instead of being separated by pages
fin_beast_2019 <- ""

for (i in 1:length(beast_2019)) {
  fin_beast_2019 = paste(fin_beast_2019, beast_2019[i]) ## Add contents from each page onto main string
}

name_pattern = "[A-Z]+.*[A-Z]+[.]*\\s{1}[|]{1}\\s{1}" ## Name Pattern --> [at least one capital letter][any character][at least one capital letter][. (for names ending with "Jr.",etc.)][one space][|][one space]
player_info <- data.frame(Name = stringr::str_sub(unlist(stringr::str_extract_all(fin_beast_2019, name_pattern)), end = -4)) ## Extract all player names, create data frame, extract " | " from end of string
player_info$Initial_Info <- unlist(stringr::str_split(fin_beast_2019, name_pattern))[-1] ## Split main string by names and remove first string (just has pdf info and qb measurements)
player_info$position = c(
  rep("QB", 20),
  rep("RB", 37),
  rep("FB", 4),
  rep("WR", 52),
  rep("TE", 24),
  rep("OT", 30),
  rep("OG", 30),
  rep("OC", 10),
  rep("EDGE", 45),
  rep("iDL", 36),
  rep("LB", 43),
  rep("CB", 50),
  rep("S", 34)
)

player_info <- player_info%>%
  tidyr::separate(Initial_Info, into = c("coll_ht", "weight", "other_info"), sep = "[|]")%>%
  dplyr::mutate(
    coll_ht = stringr::str_trim(coll_ht, side = "right"),
    weight = as.numeric(stringr::str_sub(weight, end = 4))%>%stringr::str_trim(),
    height = stringr::str_sub(coll_ht, start = -4)%>%fiie_height_to_inches(),
    college = stringr::str_sub(coll_ht, end = -5)%>%str_trim(),
    coll_class = stringr::str_extract(other_info, "[^.]+")%>%str_trim(),
    other_info = stringr::str_replace(other_info, paste0(coll_class, "."), "")%>%stringr::str_trim(side = "left")
  )%>%
  dplyr::mutate(
    hs_loc = stringr::str_extract(other_info, "[^(]+")%>%stringr::str_trim(),
    hs_name = stringr::str_extract(other_info, "[^)]+")%>%stringr::str_replace(hs_loc, replacement = "")%>%stringr::str_sub(start = 3)%>%str_trim(),
    other_info = stringr::str_replace(other_info, hs_loc, "")%>%stringr::str_replace(hs_name, "")%>%stringr::str_sub(start = 4)%>%stringr::str_trim(side = "left")
  )%>%
  dplyr::mutate(
    bd_age = stringr::str_extract(other_info, "[^#]+")%>%stringr::str_trim(),
    bd_age = dplyr::case_when(
      nchar(bd_age) > 22 ~ NA_character_,
      TRUE ~ bd_age
    ),
    birthday = stringr::str_extract(bd_age, "[^(]+")%>%stringr::str_trim(),
    age = stringr::str_extract(bd_age, "[\\d]+[.][\\d]+"),
    other_info = dplyr::case_when(
      !is.na(bd_age) ~ stringr::str_sub(other_info, start = nchar(bd_age)+1)%>%stringr::str_trim(side = "left"),
      TRUE ~ other_info
    )
  )%>%
  dplyr::mutate(
    jersey_number = stringr::str_extract(other_info, "[#]\\d+")%>%stringr::str_sub(start = 2)%>%as.numeric(),
    other_info = stringr::str_sub(other_info, start = nchar(as.character(jersey_number)) + 2)%>%stringr::str_replace_all("\n", " ")%>%stringr::str_trim(side = "left")
  )%>%
  dplyr::mutate(
    background_info = stringr::str_extract(other_info, "(?<=BACKGROUND:)(.*)(?=YEAR )")%>%stringr::str_replace("\\[\\d+\\]", "")%>%stringr::str_trim(),
    other_info = dplyr::case_when(
      !is.na(background_info) ~ paste0("YEAR ", stringr::str_extract(other_info, "(?<=YEAR ).*"))%>%stringr::str_trim(side = "left"),
      TRUE ~ other_info%>%stringr::str_trim(side = "left")
    )
  )%>%
  dplyr::mutate(
    stats = dplyr::case_when(
      stringr::str_detect(other_info, "STRENGTHS:") ~ stringr::str_extract(other_info, ".*(?=STRENGTHS:)"),
      TRUE ~ stringr::str_extract(other_info, ".*(?=SUMMARY:)")
    ),
    other_info = dplyr::case_when(
      stringr::str_detect(other_info, "STRENGTHS:") ~ paste0("STRENGTHS:", stringr::str_extract(other_info, "(?<=STRENGTHS:).*"))%>%stringr::str_trim(side = "left"),
      TRUE ~ paste0("SUMMARY:", stringr::str_extract(other_info, "(?<=SUMMARY:).*"))%>%stringr::str_trim(side = "left")
    )
  )%>%
  dplyr::mutate(
    strengths = stringr::str_extract(other_info, "(?<=STRENGTHS:)(.*)(?=WEAKNESSES:)")%>%stringr::str_trim(),
    weaknesses = stringr::str_extract(other_info, "(?<=WEAKNESSES:)(.*)(?=SUMMARY:)")%>%stringr::str_trim(),
    summary = stringr::str_extract(other_info, "(?<=SUMMARY:)(.*)(?=GRADE:)")%>%stringr::str_trim(),
    other_info = stringr::str_extract(other_info, "(?<=GRADE:)(.*)")%>%stringr::str_trim(side = "left")
  )%>%
  dplyr::mutate(
    grade = dplyr::case_when(
      stringr::str_detect(other_info, "Priority Free Agent") ~ "PFA",
      TRUE ~ stringr::str_extract(other_info, ".*(?=Round)")%>%stringr::str_trim()
    ),
    rank = stringr::str_extract(other_info, "[#]\\d+")%>%stringr::str_sub(start = 2)%>%as.numeric()
  )%>%
  dplyr::select(
    Name, position,
    college, jersey_number, birthday, age, coll_class,
    height, weight, 
    hs_loc, hs_name,
    background_info, stats, strengths, weaknesses, summary, grade, rank
  )

player_info <- player_info%>%
  dplyr::mutate(
    player_stats = paste0("YEAR ", stringr::str_extract(stats, "(?<=YEAR)(.*)(?=HT)")),
    drill_info = paste0("COMBINE ", stringr::str_extract(stats, "(?<=COMBINE).*"))
  )%>%
  dplyr::mutate(
    stat_cols = stats%>%
      stringr::str_extract(pattern = ".*(?=NOTES )")%>%
      stringr::str_trim()%>%
      stringr::str_split(pattern = "\\s+")%>%
      lengths() + 1
  )

player_info_2 <- data.frame()

for (pos in unique(player_info$position)) {
  
  player_info_subset = player_info%>%
    dplyr::filter(position == pos)%>%
    dplyr::mutate(
      stats_18 = stringr::str_extract(player_stats, "(?<=2018:)(.*)(?=Total:)")%>%
        stringr::str_extract("[^a-zA-Z]+")%>%
        stringr::str_trim()%>%
        stringr::str_replace_all("\\s{2,}", " ")%>%
        stringr::str_replace_all("[,()]", "")
    )
  
  if (pos == "QB") {
    stat_col_names = c("gp_gs", "cp_att", "comp_pct", "pass_yds", "pass_td", "qb_int", "rush_att", "rush_yds", "rush_ypc", "rush_TD")
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      tidyr::separate(cp_att, into = c("pass_comp", "pass_att"), sep = "-")%>%
      dplyr::mutate_all(.funs = as.numeric)%>%
      dplyr::mutate(
        rush_ypc = rush_yds / rush_att
      )
    
    player_info_subset = player_info_subset%>%
      dplyr::bind_cols(player_stats18)
  }
  
  if (pos %in% c("RB", "FB", "WR", "TE")) {
    player_info_subset = player_info_subset%>%
      dplyr::mutate(rushing_stats = stringr::str_detect(player_stats, "CAR"))
    
    player_info_subset_rushing <- player_info_subset%>%
      dplyr::filter(rushing_stats == TRUE)
    
    stat_col_names = c("gp_gs", "rush_att", "rush_yds", "rush_ypc", "rush_TD", "rec", "rec_yds", "rec_ypc", "rec_TD")
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset_rushing$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      dplyr::mutate_all(.funs = as.numeric)%>%
      dplyr::mutate(
        rush_ypc = rush_yds / rush_att,
        rec_ypc = rec_yds / rec
      )
    
    player_info_subset_rushing = player_info_subset_rushing%>%
      dplyr::bind_cols(player_stats18)
    
    player_info_subset_receiving <- player_info_subset%>%
      dplyr::filter(rushing_stats == FALSE)
    
    stat_col_names = c("gp_gs", "rec", "rec_yds", "rec_ypc", "rec_TD")
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset_receiving$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      dplyr::mutate_all(.funs = as.numeric)%>%
      dplyr::mutate(
        rec_ypc = rec_yds / rec
      )
    
    player_info_subset_receiving = player_info_subset_receiving%>%
      dplyr::bind_cols(player_stats18)
    
    player_info_subset <- dplyr::bind_rows(player_info_subset_rushing, player_info_subset_receiving)%>%dplyr::select(-rushing_stats)
    
    rm(player_info_subset_rushing, player_info_subset_receiving)
  }
  
  if (pos %in% c("OT", "OG", "OC")) {
    stat_col_names = c("gp_gs", "etc")
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      dplyr::select(-etc)%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      dplyr::mutate_all(.funs = as.numeric)
    
    player_info_subset = player_info_subset%>%
      dplyr::bind_cols(player_stats18)
  }
  
  if (pos %in% c("EDGE", "iDL", "LB", "CB", "S")) {
    stat_col_names = c("gp_gs", "tackles", "tfl", "sacks", "forced_fumbles", "passes_defended", "def_int")
    
    player_info_subset <- player_info_subset%>%
      dplyr::mutate(
        stats_18 = dplyr::case_when(
          stringr::str_count(stats_18, " ") == length(stat_col_names) ~ stringr::str_sub(stats_18, end = -3),
          TRUE ~ stats_18
        )
      )
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      dplyr::mutate_all(.funs = as.numeric)
    
    player_info_subset = player_info_subset%>%
      dplyr::bind_cols(player_stats18)
  }
  
  player_info_2 <- player_info_2%>%
    dplyr::bind_rows(player_info_subset%>%dplyr::select(-stats_18))
  
  rm(player_info_subset, player_stats18, stat_col_names)
}

player_info <- player_info_2
rm(player_info_2)

drill_names <- c(
  "meas_ht", "meas_wt", "arm_length", "hand_size", "wingspan",
  "forty_dash", "twenty_shuttle", "ten_split",
  "vert_jump", "broad_jump",
  "short_shuttle", "three_cone",
  "bench"
)

player_info <- player_info%>%
  dplyr::mutate(
    combine_drill_info = stringr::str_extract(drill_info, "(?<=COMBINE).*(?=PRO DAY)")%>%stringr::str_trim()
  )
combine_drill_info_test <- stringr::str_extract(player_info$drill_info[2], "(?<=COMBINE).*(?=PRO DAY)")%>%stringr::str_trim()%>%stringr::str_split(pattern = "\\s+")%>%unlist()

full_player_combine_info <- data.frame()
for (i in 1:nrow(player_info)) {
  player_combine_info = player_info$combine_drill_info[i]%>%stringr::str_split(pattern = "\\s+")%>%unlist()
  
  fractional_measure_points = c()
  
  for (j in 1:length(player_combine_info)) {
    
    if (stringr::str_detect(player_combine_info[j], "[a-zA-Z\\-]")) {
      player_combine_info[j] = NA
    }
    
    else if (stringr::str_detect(player_combine_info[j], "/")) {
      player_combine_info[j] = as.character(
        (
          as.numeric(stringr::str_sub(player_combine_info[j], end = 1)) /
            as.numeric(stringr::str_sub(player_combine_info[j], start = -1))
        )
        + as.numeric(player_combine_info[j-1])
      )
      
      fractional_measure_points = c(fractional_measure_points, j-1)
    }
  }
  
  if (!is.null(fractional_measure_points)) {
    player_combine_info = player_combine_info[-fractional_measure_points]
  }
  
  player_combine_info = player_combine_info[1:min(length(player_combine_info), 13)]
  player_combine_info = c(player_combine_info, rep(NA, 13 - length(player_combine_info)))
  
  drill_info_df <- data.frame(combine = player_combine_info)%>%t()%>%
    as.data.frame()%>%
    tibble::rownames_to_column()%>%
    dplyr::select(-rowname)
  colnames(drill_info_df) = drill_names
  
  full_player_combine_info <- full_player_combine_info%>%
    dplyr::bind_rows(drill_info_df)
  
  rm(player_combine_info, fractional_measure_points, drill_info_df)
}

full_player_combine_info <- full_player_combine_info%>%
  dplyr::mutate(
    broad_jump = dplyr::case_when(
      !is.na(broad_jump) ~ as.numeric(stringr::str_sub(broad_jump, end = 2)) * 12 +
        as.numeric(stringr::str_sub(broad_jump, start = 4, end = -2)),
      TRUE ~ NA_real_
    )
  )%>%
  dplyr::mutate_all(.funs = as.numeric)

player_info <- player_info%>%
  dplyr::bind_cols(full_player_combine_info)
rm(full_player_combine_info)

player_info <- player_info%>%
  dplyr::select(Name:background_info, strengths:rank, GP:def_int, arm_length:bench)

readr::write_excel_csv(player_info, "beast_2019_scraped_data.csv", na = ".")

# Next steps (4/10/25):
# 1) Clean up names (remove apostrophes, periods (NOT DASHES))
# 2) Make sure all new column creations that don't involve extra for-loops are in the one big mutate operation
# 3) HS recruitment (if background, extract from there, else extract from summary)
# 4a) Join in draft info
# 4b) Make a join key variable... create joining key in both Beast and nflreadr data with pasting of combine measureables, and if no combine data, use name
# 5) Comments
# 6) Get started on 2020-2025