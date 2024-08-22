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

# Load PDF of "The Beast"
beast_2019 <- pdftools::pdf_text("C:/Users/Student/Desktop/Personal Stuff/NFL Draft Project/The Beast/beast_2019.pdf", opw = "...")

# Create empty string and reformat text as one large string instead of being separated by pages
fin_beast_2019 <- ""

for (i in 1:length(beast_2019)) {
  fin_beast_2019 = paste(fin_beast_2019, beast_2019[i]) ## Add contents from each page onto main string
}

# Function to convert height from "FIEE" format to numeric inches
fiie_height_to_inches <- function(FIEE) {
  feet = as.numeric(str_sub(FIEE,1,1))*12
  inches = as.numeric(str_sub(FIEE,2,3))
  eighths_inches = as.numeric(str_sub(FIEE,4,4))/8
  
  return(feet + inches + eighths_inches)
}



name_pattern = "[A-Z]+.*[A-Z]+[.]*\\s{1}[|]{1}\\s{1}" ## Name Pattern --> [at least one capital letter][any character][at least one capital letter][. (for names ending with "Jr.",etc.)][one space][|][one space]
player_names <- data.frame(Name = str_sub(unlist(str_extract_all(fin_beast_2019, name_pattern)), end = -4)) ## Extract all player names, create data frame, extract " | " from end of string
player_names$Full_Name = player_names$Name ## Create duplicate column of name (for later)
player_names$Initial_Info <- unlist(str_split(fin_beast_2019, name_pattern))[-1] ## Split main string by names and remove first string (just has pdf info and qb measurements)

test <- str_split_fixed(player_names$Initial_Info, "\\s{1}[|]{1}\\s{1}",3) ## Split main info into three separate columns separated by " | "

player_names$College <- sub("\\s+\\d.*", "", test[,1]) ## Get column for college by removing everything to the right of the college name
player_names$Height <- unlist(lapply(unlist(str_extract_all(test[,1], "\\d{4}")), fiie_height_to_inches)) ## Get height by extracting the four digits and apply FIIE function
player_names$Weight <- as.numeric(str_sub(test[,2], 1, 3)) ## Get weight
player_names$Class <- str_split_fixed(test[,3],"[.]",2)[,1] ## Get class 

player_names$Initial_Info <- str_split_fixed(test[,3],"[.]\\s+",2)[,2] ## Replace "Initial Info" column with information not extracted yet

# Function that can apply str_extract() to data frame column with missing info
str_pattern_match <- function(string, pattern) {
  if (length(unlist(str_extract_all(string, pattern))) == 0) {
    return(NA)
  }
  
  else {
    unlist(str_extract_all(string,pattern))
  }
}

## Get players' age
player_names$Age <- sapply(player_names$Initial_Info, str_pattern_match, pattern = "[(][a-zA-Z]{3}\\s{1}\\d+[.]{1}\\d+[)]")
player_names$Age <- as.numeric(str_sub(player_names$Age, start = -6, end = -2)) ### Extract numeric information only and convert

player_names$Initial_Info <- sub(".*\\s+[#]{1}\\d+\\s+", "", player_names$Initial_Info) ## Replace "Initial Info" column with information not extracted yet

extract_text_between_patterns <- function(text, start_pattern, end_pattern, include_start = FALSE, include_end = FALSE) {
  # Construct the pattern using regular expressions
  pattern <- paste('(?s)', start_pattern, '(.*?)', end_pattern, sep = "")
  
  # Extract text between start_pattern and end_pattern
  matches <- regexpr(pattern, text, perl = TRUE)
  
  # Check if a match is found
  if (matches == -1) {
    return(NA)
  } 
  
  else {
    result <- regmatches(text, matches)
    if (include_start == FALSE) { ## Option to omit start_pattern from final string
      result <- sub(start_pattern, "", result)
    }
    if (include_end == FALSE) { ## Option to omit end_pattern from final string
      result <- sub(end_pattern, "", result)
    }
    return(result)
  }
}

## Extract background info
player_names$Background <- sapply(player_names$Initial_Info, extract_text_between_patterns, start_pattern = "BACKGROUND:\\s{0,}", end_pattern = "YEAR")

## Get statistics and combine numbers
player_names$Stats <- sub("SUMMARY.*", "", player_names$Initial_Info) ### Exclude non-stats from players where background is NA
player_names$Stats <- sub("STRENGTHS.*", "", player_names$Stats) ### Exclude non-stats from everyone else
player_names$Stats <- paste("YEAR (", sub(".*YEAR\\s[(]", "", player_names$Stats), sep = "") ### Extract stats only by removing background info (if applicable)

## Extract strengths and weaknesses (if applicable)
player_names$Strengths <- sapply(player_names$Initial_Info, extract_text_between_patterns, start_pattern = "STRENGTHS:\\s{1}", end_pattern = "WEAKNESSES")
player_names$Weaknesses <- sapply(player_names$Initial_Info, extract_text_between_patterns, start_pattern = "WEAKNESSES:\\s{1}", end_pattern = "SUMMARY")

### Replace rogue new lines with spaces
player_names$Strengths <- gsub("\\n", " ", player_names$Strengths)
player_names$Weaknesses <- gsub("\\n", " ", player_names$Weaknesses)

## Get summary
player_names$Summary <- sapply(player_names$Initial_Info, extract_text_between_patterns, start_pattern = "SUMMARY:\\s{1}", end_pattern = "GRADE")
player_names$Summary <- gsub("\\n", " ", player_names$Summary)


player_names$Grade <- sapply(player_names$Initial_Info, extract_text_between_patterns, start_pattern = "GRADE:\\s{1}", end_pattern = "\\n{2}")
player_names$Overall_Rank <- as.numeric(gsub("[#]", "", sapply(player_names$Grade, str_pattern_match, pattern = "[#]\\d+")))
player_names$Grade <- sub("\\s{1}[(].*", "", player_names$Grade)

test_str <- unlist(str_split(player_names$Stats[1], "\\n{2}"))[2]
test_str <- gsub("\\s+", " ", test_str)

position_list <- c("QUARTERBACKS", "RUNNING BACKS", "FULLBACKS/H-BACKS", "WIDE RECEIVERS", "TIGHT ENDS",
                   "OFFENSIVE TACKLES", "OFFENSIVE GUARDS", "OFFENSIVE CENTERS",
                   "EDGE RUSHERS", "DEFENSIVE LINEMEN", "LINEBACKERS", "CORNERBACKS", "SAFETIES")

player_names <- player_names%>%
  mutate(Name = toupper(Name),
         Name = gsub("\\’", "", Name),
         Name = gsub("\\'", "", Name),
         Name = gsub("\\.", "", Name))%>%
  separate(Name, c("First_Name", "Last_Name"), " ")

test_init_info <- data.frame(Initial_Info = unlist(str_split(fin_beast_2019, name_pattern)))

prospect_basic_measurements <- c()

for (i in 1:nrow(test_init_info)) {
  for (j in 1:length(position_list)) {
    if (grepl(paste("\n ", position_list[j], "\n", sep = ""), test_init_info$Initial_Info[i], fixed = TRUE) == TRUE) {
      prospect_basic_measurements <- c(prospect_basic_measurements, test_init_info$Initial_Info[i])
    }
  }
}

prospect_basic_measurements <- data.frame(Basic_Measurements = prospect_basic_measurements, Position = position_list)%>%
  mutate(Position = paste("\n ", Position, "\n", sep = ""))

prospect_basic_measurements$Basic_Measurements <- str_split_fixed(prospect_basic_measurements$Basic_Measurements, prospect_basic_measurements$Position, 2)[,2]

prospect_basic_measurements <- data.frame(Measurements = unlist(str_split(prospect_basic_measurements$Basic_Measurements, "\\n+\\d+[.]\\s+")))

prospect_basic_measurements <- data.frame(Measurements = prospect_basic_measurements[!(prospect_basic_measurements$Measurements == "" | 
                                                              endsWith(prospect_basic_measurements$Measurements, "AGE") == TRUE),])

test3 <- data.frame(str_split_fixed(prospect_basic_measurements$Measurements, "\\s{2,}\\d{4}\\s{2,}", 2))

prospect_basic_measurements$Name <- sub("^(\\S+\\s+\\S+).*", "\\1", test3$X1) ## Get player names (work with later)
prospect_basic_measurements$Measurements <- test3$X2

prospect_basic_measurements <- prospect_basic_measurements%>%
  separate(Measurements, c("Weight", "Forty_Time", "Arm_Length", "Hand_Size", "Wingspan", "Age"), "\\s{2,}")%>%
  separate(Forty_Time, c("Forty_Time", "Ten_Split"), "\\(")%>%
  mutate(Ten_Split = gsub(")","",Ten_Split),
         Weight = as.numeric(Weight),
         Forty_Time = as.numeric(Forty_Time),
         Ten_Split = as.numeric(Ten_Split),
         Arm_Length = unlist(lapply(Arm_Length, string_fraction_converter)),
         Hand_Size = unlist(lapply(Hand_Size, string_fraction_converter)),
         Wingspan = unlist(lapply(Wingspan, string_fraction_converter)),
         Age = as.numeric(Age),
         Name = toupper(Name),
         Name = gsub("\\’", "", Name),
         Name = gsub("\\'", "", Name),
         Name = gsub("\\.", "", Name))%>%
  separate(Name, c("First_Name", "Last_Name"), "\\s")

player_names <- player_names%>%
  full_join(prospect_basic_measurements, by = c("First_Name", "Last_Name", "Weight"))

string_fraction_converter <- function(fraction) {
  whole_num <- as.numeric(unlist(str_split(fraction, "\\s"))[1])
  frac <- unlist(str_split(fraction, "\\s"))[2]
  
  if (is.na(frac)) {
    return(whole_num)
  }
  
  else {
    numerator <- as.numeric(unlist(str_split(frac, "/"))[1])
    denominator <- as.numeric(unlist(str_split(frac, "/"))[2])
    
    final_num <- whole_num + numerator/denominator
    
    return(final_num)
  }
}

stats_test <- data.frame(First_Name = player_names$First_Name, Last_Name = player_names$Last_Name, Stats = player_names$Stats)

stats_test$Combine_Stats <- str_split_fixed(stats_test$Stats, "\n\n", 2)[,2]
# ---------- #

test3$Arm_Length_Num <- unlist(lapply(test3$Arm_Length, string_fraction_converter))

test3$Ten_Split <- gsub(")","",test3$Ten_Split)
pattern <- paste("(?<=BACKGROUND:\\s).*?YEAR)", sep = "")

regexpr("BACKGROUND: .*?=\n\n", test_str, perl = TRUE)

str_extract_all(test_str, "(?s)BACKGROUND:(.*?)YEAR")

qdapRegex::ex_between(test_str, "BACKGROUND", "YEAR")[[1]]

str_match(test_str, "(?s)BACKGROUND:\\s{0,}(.*?)YEAR")

test_str <- player_names$Initial_Info[1]
extract_text_between_patterns(test_str, "BACKGROUND[:]\\s", "YEAR")

player_names$Background[1]
# ---------------------------------- #

test <- unlist(strsplit(test_str, "\n\n"))[2:8]
test

base_stats <- unlist(strsplit(test[1], "BACKGROUND"))[1]
base_stats

background <- unlist(strsplit(test[1], "BACKGROUND"))[2]

strengths <- ""
weaknesses <- ""

for (i in 1:length(test)) {
  if (startsWith(test[i], "STRENGTHS") == TRUE) {
    strengths <- test[i]
    strengths <- gsub("\n", " ", strengths)
  }
  
  if (startsWith(test[i], "WEAKNESSES") == TRUE) {
    weaknesses <- test[i]
  }
}

strengths
weaknesses

head(test2)
test2[19]

test <- nflreadr::load_combine()
test2 <- nflreadr::load_draft_picks(seasons = 2019)

library(nflreadr)



combine_data_19 <- nflreadr::load_combine(seasons = 2019)%>%
  mutate(school = ifelse(school == "Mississippi", "Ole Miss", school),
         school = ifelse(school == "North Carolina State", "NC State", school),
         school = ifelse(school == "East. Michigan", "Eastern Michigan", school),
         school = ifelse(school == "Central Florida", "UCF", school))

combine_data_19 <- combine_data_19%>%
  mutate(Name = player_name,
         School = school,
         Name = toupper(Name),
         Name = gsub("\\’", "", Name),
         Name = gsub("\\'", "", Name),
         Name = gsub("\\.", "", Name))

combine_data_19 <- combine_data_19%>%
  separate(School, c("School_1", "School_2"), " ")

player_names <- player_names%>%
  separate(School, c("School_1", "School_2"), " ")

combine_data_19 <- combine_data_19%>%
  separate(Name, c("First_Name", "Last_Name"), " ")

test2 <- player_names%>%
  full_join(combine_data_19, by = c("First_Name","Last_Name", "School_1"))