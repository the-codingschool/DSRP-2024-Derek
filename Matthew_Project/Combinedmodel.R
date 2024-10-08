library(dplyr)
library(ggplot2)
library(caTools)
library(readr)
library(tidyverse)

data <- read.csv("data/office_sentiment .csv")
data <- data |>
  rename('character_name' = character)
data$seasonEp <- paste(data$season, data$episode, sep = ".")
head(data)



result <- data %>%
  group_by(character_name, season, episode) %>% 
  filter(sentimentAnalysis_score != 0) %>%
  summarise(mean_value = mean(sentimentAnalysis_score)) 

resultr <- data %>%
  mutate(character_name_plus_2 = paste(character_name, "2", sep = ''))
resultr <- resultr %>%
  group_by(character_name_plus_2, season, episode) %>% 
  filter(sentimentr_score != 0) %>%
  summarise(mean_value = mean(sentimentr_score)) 

resultz <- data %>%
  mutate(character_name_plus_3 = paste(character_name, "3", sep = ''))
resultz <- resultz %>%
  group_by(character_name_plus_3, season, episode) %>% 
  filter(syuzhet_score != 0) %>%
  summarise(mean_value = mean(syuzhet_score)) 

df_wide <- result %>%
  pivot_wider(
    names_from = character_name,     
    values_from = mean_value,         
    names_sort = TRUE) %>%                 
  arrange(episode, season)

df_wider <- resultr %>%
  pivot_wider(
    names_from = character_name_plus_2,     
    values_from = mean_value,         
    names_sort = TRUE) %>%                 
  arrange(episode, season)

df_widez <- resultz %>%
  pivot_wider(
    names_from = character_name_plus_3,     
    values_from = mean_value,         
    names_sort = TRUE) %>%                 
  arrange(episode, season)


merged_df <- df_wide %>%
  full_join(df_wider) %>%
  full_join(df_widez)
print(merged_df)

results <- data %>%
  group_by(season, episode, imdb_rating) %>%
  distinct(season, episode)


merged_df$imdb_rating <- paste(results$imdb_rating)
merged_df$imdb_rating <- as.numeric(merged_df$imdb_rating)
summary(merged_df$Michael)
summary(merged_df$Michael2)
summary(merged_df$Michael3)

merged_df[10,"Michael"]

##Cringiest Episodes: 7.1, 8.4, 6.13, 1.2, 3.6, 5.13, 3.1, 3.16, 4.13, 

set.seed(42)
datas <- merged_df[, c("imdb_rating", "Jim", "Jim2", "Jim3", "Dwight", "Dwight2", "Dwight3", "Michael", "Michael2", "Michael3", "Pam", "Pam2", "Pam3", "Andy", "Andy2", "Andy3", "season")]
split <- sample.split(datas$imdb_rating, SplitRatio = 0.8)
train_data <- subset(datas, split == TRUE)
test_data <- subset(datas, split == FALSE)
lr_model <- lm(imdb_rating ~ Andy + Andy2 + Andy3 , data = train_data)
pred <- predict(lr_model, newdata = test_data)
ggplot(test_data, aes(x = imdb_rating, y = pred, color = factor(season))) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Real v.s. Predicted Values (All Seasons)",
       x = "True IMDB Rating",
       y = "Predicted IMDB Rating",
       color = "Season",
  ) +
  scale_x_continuous(
    limits = c(6,10)
  ) +
  scale_y_continuous(
    limits = c(6,10)
  )
r_squared <- summary(lr_model)$r.squared
r_squared

summary(lr_model)

##Side characters present in every season: Stanley, Kevin, Meredith, Angela, Oscar, Phyllis, Kelly, Toby, Creed, Darryl
