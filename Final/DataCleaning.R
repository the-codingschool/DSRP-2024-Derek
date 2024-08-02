# Load necessary libraries
library(tidyverse)
library(dplyr)
library(stringr)

# Load the dataset
office_data <- read.csv("data/office_sentiment .csv", stringsAsFactors = FALSE)

# Examine the structure and summary of the dataset
str(office_data)
summary(office_data)

# Check for duplicates and remove them
office_data <- office_data %>%
  distinct()

# Handling missing values by filling NA with appropriate measures
# For this dataset, we'll assume that IMDb ratings and votes should not have NAs
office_data <- office_data %>%
  filter(!is.na(imdb_rating) & !is.na(total_votes))

# Correcting any misformatted data in the columns
office_data <- office_data %>%
  mutate(
    episode_name = str_trim(episode_name),  # Trim whitespace from episode names
    writer = str_trim(writer),              # Trim whitespace from writer names
    director = str_trim(director)           # Trim whitespace from director names
  )

# Splitting the 'writer' column into separate rows based on multiple writers per episode
office_data_clean <- office_data %>%
  separate_rows(writer, sep = ";\\s*and\\s*") %>%
  separate_rows(writer, sep = "and\\s*") %>%
  mutate(writer = str_trim(writer))  # Ensure no leading/trailing spaces

# List of writers who are also actors
actor_writers <- c(
  "Steve Carell", 
  "B.J. Novak", 
  "Mindy Kaling", 
  "Paul Lieberstein", 
  "Rainn Wilson", 
  "Michael Schur", 
  "Greg Daniels"
)

# Adding a new column to categorize writers
office_data_clean <- office_data_clean %>%
  mutate(writer_category = ifelse(writer %in% actor_writers, "Writer-Actor", "Writer-Only"))

# Convert air_date to Date format if it exists
if ("air_date" %in% colnames(office_data_clean)) {
  office_data_clean$air_date <- as.Date(office_data_clean$air_date, format = "%Y-%m-%d")
}

# Validate the clean data structure
str(office_data_clean)
summary(office_data_clean)

# Check for any anomalies or inconsistencies
# Unique writer names
unique_writers <- unique(office_data_clean$writer)
print(unique_writers)

# Save the cleaned data to a new CSV file
write.csv(office_data_clean, "data/clean_office_data.csv", row.names = FALSE)
