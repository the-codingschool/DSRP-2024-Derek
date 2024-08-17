library(dplyr)
library(ggplot2)

original_data <- read.csv("data/office_sentiment .csv") 
data <- na.omit(original_data)

data <- data |> mutate(average_score = sentimentr_score)
data <- data |>
  #filter(season == 5) |>
  group_by(character) |>
  filter(n() > 55) |>
  ungroup()
#Episode score vs Episode rating
episode_data <- data |> 
  filter(character=="Andy") |>
  distinct(season, episode) |>
  mutate(episode_index = row_number())
new_data <- merge(data, episode_data, by = c("season", "episode")) 
average_episode <- new_data |>
  group_by(episode_index) |>
  summarise(mean_score = mean(average_score, na.rm = TRUE)) |>
  left_join(new_data |> select(episode_index, imdb_rating) |> distinct(), by = "episode_index")
ggplot(average_episode, aes(x = mean_score, y = imdb_rating)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
lm(average_episode$imdb_rating ~ average_episode$mean_score, data = average_episode)
print(cor.test(average_episode$mean_score, average_episode$imdb_rating, method = "spearman"))