library(dplyr)
library(ggplot2)

data <- read.csv("data/office_sentiment .csv")
filtered_df <- data %>%
  filter(season == 1) %>%
  group_by(character) %>%
  filter(n() > 5) %>%
  ungroup()
result <- filtered_df %>%
  group_by(character) %>%
  summarize(mean_score = mean(sentimentAnalysis_score, na.rm = TRUE)) %>%
  filter(mean_score != 0)
ggplot(result , aes(x = character, y = mean_score)) + geom_bar(stat = "Identity")
