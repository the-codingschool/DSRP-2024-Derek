library(dplyr)
library(ggplot2)
library(caTools)
library(readr)

# read in dataset
data <- read.csv("data/office_sentiment .csv")
data <- data |>
  rename('character_name' = character)
head(data, n = 20)


summary_data <- data %>%
  group_by(season, character_name) %>%
  summarise(total_appearances = n(), 
            sum_score = sum(sentimentr_score)) %>%
  filter(total_appearances > 50)


ggplot(summary_data, aes(x = season, y = sum_score , fill = character_name)) + 
  geom_bar(stat = "identity", color = "skyblue", width = 0.7) +
  labs(title = "Season 1 Sum SentimentR Score by Character/Season", 
       x = "Character",
       y = "Sum Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

##Research Question Idea -> Relationship between character emotions and imdb rating, perhpas, average emotion throughout episode, or relationship between one character's emotions and rating

data$dwightSentiment <- ifelse(data$character == "Dwight", data$sentimentAnalysis_score, NA)
data$dwightSentiment2 <- ifelse(data$character == "Dwight", data$sentimentr_score, NA)
data$dwightSentiment3 <- ifelse(data$character == "Dwight", data$syuzhet_score, NA)

set.seed(42)
datas <- data[, c("imdb_rating", "dwightSentiment", "dwightSentiment2", "dwightSentiment3")]
split <- sample.split(datas$imdb_rating, SplitRatio = 0.8)
train_data <- subset(datas, split == TRUE)
test_data <- subset(datas, split == FALSE)
lr_model <- lm(imdb_rating ~ dwightSentiment + dwightSentiment2 + dwightSentiment3, data = train_data)
pred <- predict(lr_model, newdata = test_data)
ggplot(test_data, aes(x = imdb_rating, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Real v.s. Predicted Values",
       x = "True IMDB Rating",
       y = "Predicted IMDB Rating")
r_squared <- summary(lr_model)$r.squared
r_squared

summary(data)

ggplot(data, aes(y = imdb_rating, color = factor(season))) +
  geom_boxplot() +
  labs(title = "Box Plot of IMDB Rating")
