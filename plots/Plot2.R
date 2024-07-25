library(dplyr)
library(tidyr)
library(ggplot2)

office_data <- read.csv("data/office_sentiment .csv")

#research question: significant difference in sentiment scores between characters across seasons

summary(office_data)

##change character and season columns to factors
office_data$character <- as.factor(office_data$character)
office_data$season <- as.factor(office_data$season)

##remove NA, 0 values, and duplicated rows
office_data <- office_data[!is.na(data$sentimentAnalysis_score) & data$sentimentAnalysis_score != 0 & !duplicated(office_data), ]


##separate rows for multiple characters
office_data_split <- office_data %>%
  separate_rows(character, sep = ",\\s*") %>%
  separate_rows(character, sep = "and\\s*") %>%
  separate_rows(character, sep = "&\\s*") 
  
##average sentiment score for each character per season
result <- office_data_split %>%
  group_by(character, season) %>%
  summarize(mean_score = mean(sentimentAnalysis_score, na.rm = TRUE), .groups = 'drop') %>%
  filter(mean_score != 0)

##bar graph
ggplot(result, aes(x = season, y = mean_score, color = character, group = character)) +
  geom_line() +
  geom_point(size = 0.2) +
  facet_wrap(~ character, scales = "y") +  # Create a separate plot for each character
  labs(
    x = "Season",
    y = "Average Sentiment Score",
    color = "Character"
  )