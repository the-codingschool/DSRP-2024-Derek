library(dplyr)
library(ggplot2)
library(caTools)
library(readr)
library(tidyverse)


# read in dataset
data <- read.csv("data/office_sentiment .csv")
data <- data |>
  rename('character_name' = character)
head(data, n = 5)


summary_data <- data %>%
  group_by(character_name) %>%
  summarise(total_appearances = n()) %>%
  arrange(desc(total_appearances))
summary_data

##Main Characters Michael, Dwight, Jim, Pam, Andy

ggplot(summary_data, aes(x = season, y = mean_score , fill = character_name)) + 
  geom_bar(stat = "identity", color = "skyblue", width = 0.7) +
  labs(title = "Mean SentimentR Score by Character/Season", 
       x = "Character",
       y = "Mean Season Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##Research Question Idea -> Relationship between character emotions and imdb rating, perhpas, average emotion throughout episode, or relationship between one character's emotions and rating




data$jimSentiment <- ifelse(data$character == "Jim", data$sentimentAnalysis_score, NA)
data$jimSentiment2 <- ifelse(data$character == "Jim", data$sentimentr_score, NA)
data$jimSentiment3 <- ifelse(data$character == "Jim", data$syuzhet_score, NA)

set.seed(42)
datas <- data[, c("imdb_rating", "jimSentiment", "jimSentiment2", "jimSentiment3", "dwightSentiment", "dwightSentiment2", "dwightSentiment3", "season")]
split <- sample.split(datas$imdb_rating, SplitRatio = 0.8)
train_data <- subset(datas, split == TRUE)
test_data <- subset(datas, split == FALSE)
lr_model <- lm(imdb_rating ~ jimSentiment + jimSentiment2 + jimSentiment3 + dwightSentiment + dwightSentiment2 + dwightSentiment3, data = train_data)
pred <- predict(lr_model, newdata = test_data)
ggplot(test_data, aes(x = pred, y = imdb_rating, color = factor(season))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Real v.s. Predicted Values",
       x = "Predicted IMDB Rating",
       y = "True IMDB Rating",
       color = "Season")
r_squared <- summary(lr_model)$r.squared
r_squared




summary(data)

sdata <- data %>%
  group_by(character_name, season) %>%
  filter(character_name == "Dwight") %>%
  filter(sentimentr_score != 0) 

ggplot(sdata, aes(y = sentimentr_score, color = factor(season))) +
  geom_boxplot() +
  labs(title = "Box Plot of IMDB Rating")


ggplot(sdata, aes(y = sentimentr_score, color = factor(episode))) +
  geom_boxplot() +
  labs(title = "Box Plot of IMDB Rating")

ggplot(sdata, aes(y = mean_score, x = factor(season), group = 1)) +
  geom_point(color = "blue") +
  geom_line(color = "red", linetype = "dashed") +
  labs(title = "Mean SentimentR Score of Jim Over Time",
       x = "Season",
       y = "Mean SentimentR Score")

ndata <- data%>%
  group_by(season, ) %>%
  summarise(mean_scores = mean(imdb_rating, na.rm = TRUE))

ggplot(ndata, aes(y = mean_scores, x= factor(season), group = 1)) +
  geom_point(color = "blue") +
  geom_line(color = "red", linetype = "dashed") +
  labs(title = "Mean IMDB Rating Over Time",
       x = "Season",
       y = "Mean IMDB Rating")


