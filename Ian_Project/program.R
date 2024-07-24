library(dplyr)
library(ggplot2)

original_data <- read.csv("data/office_sentiment .csv") 
data <- na.omit(original_data)


data <- data |> mutate(average_score = (sentimentAnalysis_score+ sentimentr_score+ syuzhet_score)/3)
# Mean Character Sentiment Score
filtered_data <- data |>
  group_by(character) |>
  filter(n() > 40) |>
  ungroup()
result <- filtered_data |>
  group_by(character) |>
  summarize(mean_score = mean(average_score)) |>
  filter(mean_score != 0)
ggplot(result , aes(x = character, y = mean_score)) + geom_bar(stat = "Identity")


#Episode score vs Episode rating
episode_data <- data |> 
  distinct(season, episode) |>
  mutate(episode_index = row_number())
new_data <- merge(data, episode_data, by = c("season", "episode")) 
average_episode <- new_data |>
  group_by(episode_index) |>
  summarise(mean_score = mean(average_score)) |>
  left_join(new_data |> select(episode_index, imdb_rating) |> distinct(), by = "episode_index")
ggplot(average_episode, aes(x = mean_score, y = imdb_rating)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(title = paste("Correlation:", cor(average_episode$mean_score, average_episode$imdb_rating), 2))
