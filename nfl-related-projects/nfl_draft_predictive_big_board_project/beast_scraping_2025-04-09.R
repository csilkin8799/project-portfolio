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
library(pdftools)
library(stringr)
library(nflreadr)
library(tm)
library(SnowballC)
library(ggplot2)
library(tidyr)
library(gt)
library(gtExtras)

#### Load "The Beast" ####
# Load PDF of "The Beast"
beast_2019 <- pdftools::pdf_text(file.path(getwd(), "the_beast_info", "beast_2019.pdf"), opw = "draftguide2019")

# Create empty string and reformat text as one large string instead of being separated by pages
fin_beast_2019 <- ""

for (i in 1:length(beast_2019)) {
  fin_beast_2019 = paste(fin_beast_2019, beast_2019[i]) ## Add contents from each page onto main string
}

# Function to convert height from "FIEE" format to numeric inches
fiie_height_to_inches <- function(FIEE) {
  feet = as.numeric(stringr::str_sub(FIEE, end = 1))*12
  inches = as.numeric(stringr::str_sub(FIEE, start = 2, end = 3))
  eighths_inches = as.numeric(stringr::str_sub(FIEE, start = 4))/8
  
  return(feet + inches + eighths_inches)
}

name_pattern = "[A-Z]+.*[A-Z]+[.]*\\s{1}[|]{1}\\s{1}" ## Name Pattern --> [at least one capital letter][any character][at least one capital letter][. (for names ending with "Jr.",etc.)][one space][|][one space]
player_info <- data.frame(Name = stringr::str_sub(unlist(stringr::str_extract_all(fin_beast_2019, name_pattern)), end = -4)) ## Extract all player names, create data frame, extract " | " from end of string
player_info$Full_Name = player_info$Name ## Create duplicate column of name (for later)
player_info$Initial_Info <- unlist(stringr::str_split(fin_beast_2019, name_pattern))[-1] ## Split main string by names and remove first string (just has pdf info and qb measurements)

player_info <- player_info%>%
  tidyr::separate(Initial_Info, into = c("coll_ht", "weight", "other_info"), sep = "[|]", remove = FALSE)%>%
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
  dplyr::select(Name, Full_Name, college, height, weight, coll_class, hs_loc, hs_name, birthday, age, jersey_number, other_info)%>%
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
  )
