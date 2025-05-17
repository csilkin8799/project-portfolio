#### Set working directory ####

# setwd("nfl_draft_predictive_big_board_project")

#### Install/load packages ####

# install.packages("pdftools")
# install.packages("tidyverse")
# install.packages("nflreadr")
library(magrittr)

#### Function to convert height from "FIIE" format to numeric inches ####

#' In "The Beast", Dane Brugler denotes height in "FIIE" format.
#' 
#' This function identifies the FIIE parameters in the string, converts them to inches, and calculates height in inches.
#' 
#' @param FIEE Height in "FIIE" format.
#' @return Height in inches
fiie_height_to_inches <- function(FIEE) {
  
  # Extract the first character of the string (feet), convert to numeric, and multiply by 12.
  feet = as.numeric(stringr::str_sub(FIEE, end = 1))*12
  
  # Extract the second and third characters of the string (inches) and convert to numeric
  inches = as.numeric(stringr::str_sub(FIEE, start = 2, end = 3))
  
  # Extract the last character of the string (eighths of an inch), convert to numeric, and divide by 8.
  eighths_inches = as.numeric(stringr::str_sub(FIEE, start = 4))/8
  
  # Return the total height
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

#### Create data frame with all the player info ####

player_info <- data.frame(
  ## Extract all player names, extract " | " from end of string, convert to uppercase, and remove periods and apostrophes
  Name = stringr::str_sub(unlist(stringr::str_extract_all(fin_beast_2019, name_pattern)), end = -4)%>%toupper()%>%stringr::str_remove_all("[.'’]"),
  
  ## Split main string by names and remove first string (just has pdf info and qb measurements)
  Initial_Info = unlist(stringr::str_split(fin_beast_2019, name_pattern))[-1],
  
  ## Positions (had to go manually through Beast to get numbers)
  position = c(
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
)

##### Create separate columns with specific info from all info collected in "Initial Info" variable #####

player_info <- player_info%>%
  # Separate initial info into three columns: 1) college and height, 2) weight, 3) other info
  tidyr::separate(
    Initial_Info, into = c("coll_ht", "weight", "other_info"), sep = "[|]"
  )%>%
  # Demographic info
  dplyr::mutate(
    ## Get just the weight number, convert to numeric
    weight = as.numeric(stringr::str_sub(weight, end = 4)),
    ## Remove whitespace to the right of combined college and height string
    coll_ht = stringr::str_trim(coll_ht, side = "right"),
    ## Get height from combined college and height string (last 4 characters), and convert to inches
    height = stringr::str_sub(coll_ht, start = -4)%>%fiie_height_to_inches(),
    ## Get height from combined college and height string (everything before the last 5 characters) and remove whitespace
    college = stringr::str_sub(coll_ht, end = -5)%>%stringr::str_trim(),
    ## Get college class (junior, senior, etc.) by extracting everything in the remaining information before the first instance of a period
    coll_class = stringr::str_extract(other_info, "[^.]+")%>%stringr::str_trim(),
    ## For the rest of the remaining info, remove the information in the "coll_class" column and get rid of the whitespace to the left
    other_info = stringr::str_remove(other_info, paste0(coll_class, "."))%>%stringr::str_trim(side = "left")
  )%>%
  # High school info
  dplyr::mutate(
    ## High school location - everything before the first instance of an opening parentheses
    hs_loc = stringr::str_extract(other_info, "[^(]+")%>%stringr::str_trim(),
    ## High school name - everything before the first instance of a closing parentheses, remove info in "hs_loc" column,
    # start from the 3rd character in the string, then remove whitespace
    hs_name = stringr::str_extract(other_info, "[^)]+")%>%stringr::str_remove(hs_loc)%>%stringr::str_sub(start = 3)%>%stringr::str_trim(),
    ## Remove high school name and location from remaining info and remove whitespace
    other_info = stringr::str_replace(other_info, hs_loc, "")%>%stringr::str_remove(hs_name)%>%stringr::str_sub(start = 4)%>%stringr::str_trim(side = "left")
  )%>%
  # Age info
  dplyr::mutate(
    ## Extract birthday and age from remaining info
    bd_age = stringr::str_extract(other_info, "[^#]+")%>%stringr::str_trim(),
    ### If no age available, then the function just copies the entire string... 
    ### when this happens (check is more than 22 characters), convert to NA
    bd_age = dplyr::case_when(
      nchar(bd_age) > 22 ~ NA_character_,
      TRUE ~ bd_age
    ),
    ## Extract birthday - everything before first opening parentheses
    birthday = stringr::str_extract(bd_age, "[^(]+")%>%stringr::str_trim(),
    ## Extract age - digits on both side of a decimal point
    age = stringr::str_extract(bd_age, "[\\d]+[.][\\d]+"),
    ## Remove birthday and age info from the rest of the other info when age isn't missing
    other_info = dplyr::case_when(
      !is.na(bd_age) ~ stringr::str_sub(other_info, start = nchar(bd_age)+1)%>%stringr::str_trim(side = "left"),
      TRUE ~ other_info
    )
  )%>%
  # Get jersey number
  dplyr::mutate(
    jersey_number = stringr::str_extract(other_info, "[#]\\d+")%>%stringr::str_sub(start = 2)%>%as.numeric(),
    ## Replace all "\n" characters with spaces
    other_info = stringr::str_sub(other_info, start = nchar(as.character(jersey_number)) + 2)%>%stringr::str_replace_all("\n", " ")%>%stringr::str_trim(side = "left")
  )%>%
  # Get background info
  dplyr::mutate(
    ## Extract everything between the first instance of the string "BACKGROUND:" and the string "YEAR " 
    background_info = stringr::str_extract(other_info, "(?<=BACKGROUND:)(.*)(?=YEAR )")%>%stringr::str_remove("\\[\\d+\\]")%>%stringr::str_trim(),
    ## Remove background info from remaining info if not missing
    other_info = dplyr::case_when(
      !is.na(background_info) ~ paste0("YEAR ", stringr::str_extract(other_info, "(?<=YEAR ).*"))%>%stringr::str_trim(side = "left"),
      TRUE ~ other_info%>%stringr::str_trim(side = "left")
    )
  )%>%
  # Get player stats
  dplyr::mutate(
    ## If there is a "strengths" section, extract everything before the first instance of "STRENGTHS:",
    ## Otherwise, extract everything before the first instance of "SUMMARY:"
    stats = dplyr::case_when(
      stringr::str_detect(other_info, "STRENGTHS:") ~ stringr::str_extract(other_info, ".*(?=STRENGTHS:)"),
      TRUE ~ stringr::str_extract(other_info, ".*(?=SUMMARY:)")
    ),
    ## Update remaining info string
    other_info = dplyr::case_when(
      stringr::str_detect(other_info, "STRENGTHS:") ~ paste0("STRENGTHS:", stringr::str_extract(other_info, "(?<=STRENGTHS:).*"))%>%stringr::str_trim(side = "left"),
      TRUE ~ paste0("SUMMARY:", stringr::str_extract(other_info, "(?<=SUMMARY:).*"))%>%stringr::str_trim(side = "left")
    )
  )%>%
  # Extract strengths, weaknesses, and summary sections
  dplyr::mutate(
    strengths = stringr::str_extract(other_info, "(?<=STRENGTHS:)(.*)(?=WEAKNESSES:)")%>%stringr::str_trim(),
    weaknesses = stringr::str_extract(other_info, "(?<=WEAKNESSES:)(.*)(?=SUMMARY:)")%>%stringr::str_trim(),
    summary = stringr::str_extract(other_info, "(?<=SUMMARY:)(.*)(?=GRADE:)")%>%stringr::str_trim(),
    other_info = stringr::str_extract(other_info, "(?<=GRADE:)(.*)")%>%stringr::str_trim(side = "left")
  )%>%
  # Extract Brugler's grade and rank (if available)
  dplyr::mutate(
    grade = dplyr::case_when(
      stringr::str_detect(other_info, "Priority Free Agent") ~ "PFA",
      TRUE ~ stringr::str_extract(other_info, ".*(?=Round)")%>%stringr::str_trim()
    ),
    rank = stringr::str_extract(other_info, "[#]\\d+")%>%stringr::str_sub(start = 2)%>%as.numeric()
  )%>%
  # Extract high school recruitment info (5-star, 4-star, etc.)
  dplyr::mutate(
    HS_recruitment = dplyr::case_when(
      stringr::str_detect(background_info, "-star") ~ stringr::str_extract(background_info, "(?<=\\s)(.*)(?=-star)"),
      TRUE ~ stringr::str_extract(summary, "(?<=\\s)(.*)(?=-star)")
    ),
    HS_recruitment = stringr::str_sub(HS_recruitment, end = 5)%>%stringr::str_remove("-"),
    HS_recruitment = dplyr::case_when(
      HS_recruitment == "five" ~ 5,
      HS_recruitment == "four" ~ 4,
      HS_recruitment == "three" ~ 3,
      HS_recruitment == "two" ~ 2,
      HS_recruitment == "one" ~ 1,
      HS_recruitment == "no" ~ 0,
      TRUE ~ NA_real_
    )
  )%>%
  # Separate stats column into player statistics and combine drill info
  dplyr::mutate(
    player_stats = paste0("YEAR ", stringr::str_extract(stats, "(?<=YEAR)(.*)(?=HT)")),
    combine_drill_info = stringr::str_extract(stats, "(?<=COMBINE).*(?=PRO DAY)")%>%stringr::str_trim()
  )%>%
  # Select only necessary columns
  dplyr::select(
    Name, position,
    college, jersey_number, birthday, age, coll_class,
    height, weight, 
    hs_loc, hs_name, HS_recruitment,
    background_info, stats, strengths, weaknesses, summary, grade, rank,
    player_stats, combine_drill_info
  )

##### Extract player statistics from 2018 season #####

# Need to do a for loop since different positions have different stats

# Create a separate data frame
player_info_2 <- data.frame()

for (pos in unique(player_info$position)) { # For each position
  
  ## Subset player info data frame to only contain players from specific position
  player_info_subset = player_info%>%
    dplyr::filter(position == pos)%>%
    ### Extract 2018 stats - extract everything between "2018:" and "Total:", extract any characters that are NOT letters,
    ### remove whitespace, replace instances of 2+ spaces with one single whitespace, and remove commas and parentheses
    dplyr::mutate(
      stats_18 = stringr::str_extract(player_stats, "(?<=2018:)(.*)(?=Total:)")%>%
        stringr::str_extract("[^a-zA-Z]+")%>%
        stringr::str_trim()%>%
        stringr::str_replace_all("\\s{2,}", " ")%>%
        stringr::str_remove_all("[,()]")
    )
  
  ## For QB's...
  if (pos == "QB") {
    
    ### Vector of QB stats
    stat_col_names = c("gp_gs", "cp_att", "comp_pct", "pass_yds", "pass_td", "qb_int", "rush_att", "rush_yds", "rush_ypc", "rush_TD")
    
    ### Create a data frame with each 2018 stat in a separate column by splitting on space, then change the column names
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    ### Modifications to stats
    player_stats18 = player_stats18%>%
      #### Separate "GP/GS" by forward slash
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      #### "Separate "completions-attempts" by dash
      tidyr::separate(cp_att, into = c("pass_comp", "pass_att"), sep = "-")%>%
      #### Convert all statistics to numeric
      dplyr::mutate_all(.funs = as.numeric)%>%
      #### Get actual rushing yards per carry (not string of rounded to nearest tenth)
      dplyr::mutate(
        rush_ypc = rush_yds / rush_att
      )
    
    ### Add statistics columns to subsetted player info data frame
    player_info_subset = player_info_subset%>%
      dplyr::bind_cols(player_stats18)
  }
  
  ## For RB's and skill positions...
  if (pos %in% c("RB", "FB", "WR", "TE")) {
    
    ### Add a column to indicate whether a player has rushing stats based on whether the string "CAR" is present
    player_info_subset = player_info_subset%>%
      dplyr::mutate(rushing_stats = stringr::str_detect(player_stats, "CAR"))
    
    ### Obtain stats from players with rushing stats (same procedure as with getting QB stats)
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
    
    #### Add stats columns to subsetted player info data frame for only skill players with rushing stats
    player_info_subset_rushing = player_info_subset_rushing%>%
      dplyr::bind_cols(player_stats18)
    
    ### Obtain receiving statistics from players with no rushing statistics
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
    
    #### Add stats columns to subsetted player info data frame for only skill players with NO rushing stats
    player_info_subset_receiving = player_info_subset_receiving%>%
      dplyr::bind_cols(player_stats18)
    
    ### Combine data frames of players with rushing stats and players without rushing stats
    player_info_subset <- dplyr::bind_rows(player_info_subset_rushing, player_info_subset_receiving)%>%dplyr::select(-rushing_stats)
    
    ### Remove smaller data frames that only have players with rushing stats or only have players without rushing stats
    rm(player_info_subset_rushing, player_info_subset_receiving)
  }
  
  ## For OL...
  if (pos %in% c("OT", "OG", "OC")) {
    
    ### Only get GP and GS
    
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
  
  ## For defensive players...
  if (pos %in% c("EDGE", "iDL", "LB", "CB", "S")) {
    stat_col_names = c("gp_gs", "tackles", "tfl", "sacks", "forced_fumbles", "passes_defended", "def_int")
    
    ### In some cases, there were notes about # of games started which created an extra number.
    ### When this happens, remove the last three characters of the string.
    player_info_subset <- player_info_subset%>%
      dplyr::mutate(
        stats_18 = dplyr::case_when(
          stringr::str_count(stats_18, " ") == length(stat_col_names) ~ stringr::str_sub(stats_18, end = -3),
          TRUE ~ stats_18
        )
      )
    
    ### Obtain player statistics
    
    player_stats18 = data.frame(stringr::str_split_fixed(player_info_subset$stats_18, pattern = "\\s", n = length(stat_col_names)))
    colnames(player_stats18) = stat_col_names
    
    player_stats18 = player_stats18%>%
      tidyr::separate(gp_gs, into = c("GP", "GS"), sep = "/")%>%
      dplyr::mutate_all(.funs = as.numeric)
    
    #### Add player statistics to subsetted player info data frame
    player_info_subset = player_info_subset%>%
      dplyr::bind_cols(player_stats18)
  }
  
  ## Add specific position stat info to "player_info_2" data frame
  player_info_2 <- player_info_2%>%
    dplyr::bind_rows(player_info_subset%>%dplyr::select(-stats_18))
  
  ## Remove helper data frames and vectors created in the for loops
  rm(player_info_subset, player_stats18, stat_col_names)
}

# "player_info_2" was just a temporary place holder for the main player info data frame, so overwrite "player_info"
# and remove "player_info_2" data frame
player_info <- player_info_2
rm(player_info_2)

##### Extract combine info #####

# Create vector with names of drills/measureables
drill_names <- c(
  "meas_ht", "meas_wt", "arm_length", "hand_size", "wingspan",
  "forty_dash", "twenty_shuttle", "ten_split",
  "vert_jump", "broad_jump",
  "short_shuttle", "three_cone",
  "bench"
)

# Create data frame designed to contain just the player's combine info
full_player_combine_info <- data.frame()

# For each player individually...
for (i in 1:nrow(player_info)) {
  
  ## Split the string containing the player's combine info by spaces and unlist. This is a vector
  player_combine_info = player_info$combine_drill_info[i]%>%stringr::str_split(pattern = "\\s+")%>%unlist()
  
  ## Create an empty vector to indicate which player's measurements have fractional values
  fractional_measure_points = c()
  
  # The way the combine info string is structured, a value of 9 and 3/4 is represented as "9 3/4" in the string
  # This operation will take the "3/4" string, add 9 to make it 9.75, then remove the previous "9" string
  
  ## For each measurement in the combine info vector...
  for (j in 1:length(player_combine_info)) {
    
    ### If the value has a letter or a hyphen, replace with NA
    if (stringr::str_detect(player_combine_info[j], "[a-zA-Z\\-]")) {
      player_combine_info[j] = NA
    }
    
    ### If the value has a forward slash indicating a fraction...
    else if (stringr::str_detect(player_combine_info[j], "/")) {
      
      #### Get the numbers on each side of the back slash, divide the first by the second, then add to the previous string
      player_combine_info[j] = as.character(
        (
          as.numeric(stringr::str_sub(player_combine_info[j], end = 1)) /
            as.numeric(stringr::str_sub(player_combine_info[j], start = -1))
        )
        + as.numeric(player_combine_info[j-1])
      )
      
      #### Add the index of the previous string before the fraction to the vector indicating which measurements are fractional values
      fractional_measure_points = c(fractional_measure_points, j-1)
    }
  }
  
  ## If a player has fractional combine measurements, then remove the inaccurate strings
  if (!is.null(fractional_measure_points)) {
    player_combine_info = player_combine_info[-fractional_measure_points]
  }
  
  ## Reduce the length of the player combine info vector to be the minimum of the available combine measurements and 13,
  ## then replace missing combine measurements with NA
  player_combine_info = player_combine_info[1:min(length(player_combine_info), 13)]
  player_combine_info = c(player_combine_info, rep(NA, 13 - length(player_combine_info)))
  
  ## Convert combine measurements to data frame, transpose to make it 1x13 instead of 13x1, and make column names as the drill names
  drill_info_df <- data.frame(combine = player_combine_info)%>%t()%>%
    as.data.frame()%>%
    tibble::rownames_to_column()%>%
    dplyr::select(-rowname)
  colnames(drill_info_df) = drill_names
  
  ## Add individual player's combine info containing combine info for every player
  full_player_combine_info <- full_player_combine_info%>%
    dplyr::bind_rows(drill_info_df)
  
  ## Remove helper data frames and vectors
  rm(player_combine_info, fractional_measure_points, drill_info_df)
}

# Additional combine measurement modifications
full_player_combine_info <- full_player_combine_info%>%
  ## Convert broad jump from FF'IN" string to total inches
  dplyr::mutate(
    broad_jump = dplyr::case_when(
      !is.na(broad_jump) ~ as.numeric(stringr::str_sub(broad_jump, end = 2)) * 12 +
        as.numeric(stringr::str_sub(broad_jump, start = 4, end = -2)),
      TRUE ~ NA_real_
    )
  )%>%
  ## Convert all numbers to numeric
  dplyr::mutate_all(.funs = as.numeric)

# Add combine info columns to main player info data frame
player_info <- player_info%>%
  dplyr::bind_cols(full_player_combine_info)

# Remove data frame that only has combine info
rm(full_player_combine_info)

# Rearrange order of columns in main player info data frame
player_info <- player_info%>%
  dplyr::select(Name:background_info, strengths:HS_recruitment, GP:def_int, arm_length:bench)

#### Merge draft information within nflreadr's combine data #####

# Load nflreadr's combine data from 2019
nflreadr_combine_data <- nflreadr::load_combine(seasons = 2019)

# Within nflreadr's combine data...
nflreadr_combine_data <- nflreadr_combine_data%>%
  ## Convert player names to uppercase string, remove periods and apostrophes
  dplyr::mutate(
    player_name = toupper(player_name)%>%stringr::str_remove_all("[.'’]")
  )%>%
  ## Create joining variable to join draft info with Beast data
  ## Joining variable: a concatenated string of the last 4 characters of player's name, weight, forty-yard dash time, and bench press reps
  dplyr::mutate(join_var = paste(stringr::str_sub(player_name, start = -4), wt, forty, bench, sep = ","))

# Within Beast data
player_info <- player_info%>%
  ## Create placeholder name variable with lineage indicators ("III", "II", and "JR") removed
  dplyr::mutate(name2 = stringr::str_remove_all(Name, "\\sIII|\\sII|\\JR"))%>%
  ## Create joining variable to join draft info with nflreadr combine data
  ## Joining variable: a concatenated string of the last 4 characters of player's name, weight, forty-yard dash time, and bench press reps
  dplyr::mutate(join_var = paste(stringr::str_sub(name2, start = -4), weight, forty_dash, bench, sep = ","))%>%
  ## Join with nflreadr combine data
  dplyr::left_join(nflreadr_combine_data%>%dplyr::select(season:pfr_id, join_var), by = dplyr::join_by(join_var))%>%
  ## Remove temporary name and joining variables
  dplyr::select(-name2, -join_var)

#### Create csv with combined Beast and draft info data ####

readr::write_excel_csv(player_info, "beast_2019_scraped_data.csv", na = ".")

# Next steps (4/10/25):
# 5) Comments
# 6) Get started on 2020-2025