#load libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(MLmetrics)

office_data <- read.csv("data/office_sentiment .csv", stringsAsFactors = FALSE)

#structure and summary
str(office_data)
summary(office_data)

#remove duplicates
office_data <- office_data %>%
  distinct()

#removing missing imdb and vote values
office_data <- office_data %>%
  filter(!is.na(imdb_rating) & !is.na(total_votes))

##change character and season columns to factors
office_data$character <- as.factor(office_data$character)
office_data$season <- as.factor(office_data$season)

##remove NA values, 0 values, and duplicated rows
office_data <- office_data[!is.na(data$sentimentAnalysis_score) & data$sentimentAnalysis_score != 0 & !duplicated(office_data), ]


#split writer column into individual - some episodes have multiple writers and characters
office_data_clean <- office_data %>%
  separate_rows(character, sep = ",\\s*") %>%
  separate_rows(character, sep = "and\\s*") %>%
  separate_rows(character, sep = "&\\s*") %>%
  separate_rows(writer, sep = ";\\s*") %>%  
  separate_rows(writer, sep = ",\\s*") %>%
  separate_rows(writer, sep = "and\\s*") %>%
  separate_rows(writer, sep = "&\\s*") 

#list of writers who are also actors
actor_writers <- c(
  "Steve Carell", 
  "B.J. Novak", 
  "Mindy Kaling", 
  "Paul Lieberstein", 
  "Rainn Wilson", 
  "Michael Schur", 
  "Greg Daniels"
)

#new columns for writter-actors and writer-only
office_data_clean <- office_data_clean %>%
  mutate(writer_category = ifelse(writer %in% actor_writers, "Writer-Actor", "Writer-Only"))



#save the cleaned data to a new rds file
saveRDS(office_data_clean, "Final/clean_office_data.rds")

