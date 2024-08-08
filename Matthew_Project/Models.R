library(dplyr)
library(ggplot2)
library(caTools)
library(readr)

data <- read.csv("data/office_sentiment .csv")
data <- data |>
  rename('character_name' = character)
data$seasonEp <- paste(data$season, data$episode, sep = ".")
head(data)



result <- data %>%
  group_by(character_name, season, episode) %>% 
  filter(sentimentAnalysis_score != 0) %>%
  summarise(mean_value = mean(sentimentAnalysis_score)) %>%
  filter(season == 9) #Edit this and below to change season

resultr <- data %>%
  mutate(character_name_plus_2 = paste(character_name, "2", sep = ''))
resultr <- resultr %>%
  group_by(character_name_plus_2, season, episode) %>% 
  filter(sentimentr_score != 0) %>%
  summarise(mean_value = mean(sentimentr_score)) %>%
  filter(season == 9) #Edit this too

resultz <- data %>%
  mutate(character_name_plus_3 = paste(character_name, "3", sep = ''))
resultz <- resultz %>%
  group_by(character_name_plus_3, season, episode) %>% 
  filter(syuzhet_score != 0) %>%
  summarise(mean_value = mean(syuzhet_score)) %>%
  filter(season == 9) #Edit this too
  
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

colnames(df_wide)

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
  group_by(season) %>%
  filter(season == 9) ##Change this

new_data <- results %>%
  group_by(episode, imdb_rating) %>%
  distinct(episode)

merged_df$imdb_rating <- paste(new_data$imdb_rating)
merged_df$imdb_rating <- as.numeric(merged_df$imdb_rating)

print(merged_df, n= 26)
col_v <- merged_df[,44]
print(col_v, n =25)
print(merged_df, n = 25)

set.seed(42)
datas <- merged_df[, c("imdb_rating", "Andy", "Andy2", "Andy3")]
split <- sample.split(datas$imdb_rating, SplitRatio = 0.8)
train_data <- subset(datas, split == TRUE)
test_data <- subset(datas, split == FALSE)
lr_model <- lm(imdb_rating ~ Andy + Andy2 + Andy3 , data = train_data)
pred <- predict(lr_model, newdata = test_data)
ggplot(test_data, aes(x = imdb_rating, y = pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Real v.s. Predicted Values Season 1",
       x = "True IMDB Rating",
       y = "Predicted IMDB Rating",
       ) +
  scale_x_continuous(
    limits = c(6,10)
  ) +
  scale_y_continuous(
    limits = c(6,10)
  )
summary(lr_model)


