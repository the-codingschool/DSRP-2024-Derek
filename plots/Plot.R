library(dplyr)
library(tidyr)
library(ggplot2)

office_data <- read.csv("data/office_sentiment .csv")

#research question: significant difference in sentiment scores between characters across seasons

summary(office_data)

##change character and season columns to factors
office_data$character <- as.factor(office_data$character)
office_data$season <- as.factor(office_data$season)

##remove NA values, 0 values, and duplicated rows
office_data <- office_data[!is.na(data$sentimentAnalysis_score) & data$sentimentAnalysis_score != 0 & !duplicated(office_data), ]

##check to see values are removed
sentiment_scores <- office_data_split$sentimentAnalysis_score
print(sentiment_scores)

##separate rows for multiple characters
office_data_split <- office_data %>%
  separate_rows(character, sep = ",\\s*") %>%
  separate_rows(character, sep = "and\\s*") %>%
  separate_rows(character, sep = "&\\s*") 

##find average for each characters sentiment score per season
result <- office_data_split %>%
  group_by(character, season) %>%
  summarize(mean_score = mean(sentimentAnalysis_score, na.rm = TRUE)) %>%
  filter(mean_score != 0)

##create a bar graph
ggplot(result, aes(x = character, y = mean_score, fill = season)) +
  geom_bar(stat = "identity") +
  labs(title = "Avg. Sentiment Score by Character Across Seasons",
       x = "Character",
       y = "Average Sentiment Score") 


