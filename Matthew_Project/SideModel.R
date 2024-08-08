library(dplyr)
library(ggplot2)
library(caTools)
library(readr)
library(tidyverse)

install.packages("car")
library(car)


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

##Side characters present in every season: Stanley, Kevin, Meredith, Angela, Oscar, Phyllis, Kelly, Toby, Creed, Darryl
set.seed(42)
datas <- merged_df[, c("imdb_rating", "Ryan", "Ryan2", "Ryan3", "Stanley", "Stanley2", "Stanley3", "Kevin", "Kevin2", "Kevin3", "Meredith", "Meredith2", "Meredith3", "Angela", "Angela2", "Angela3", "Oscar", "Oscar2", "Oscar3", "Phyllis", "Phyllis2", "Phyllis3", "Kelly", "Kelly2", "Kelly3", "Toby", "Toby2", "Toby3", "Creed", "Creed2", "Creed3", "Darryl", "Darryl2", "Darryl3", "season")]
split <- sample.split(datas$imdb_rating, SplitRatio = 0.8)
train_data <- subset(datas, split == TRUE)
test_data <- subset(datas, split == FALSE)
lr_model <- lm(imdb_rating ~ Ryan + Ryan2 + Ryan3 + Stanley + Stanley2 + Stanley3 + Kevin + Kevin2 + Kevin3 + Meredith + Meredith2 + Meredith3 + Angela + Angela2 , data = train_data)
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

