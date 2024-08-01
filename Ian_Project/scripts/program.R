library(dplyr)
library(ggplot2)


selected_character <- "Kevin"

original_data <- read.csv("data/office_sentiment .csv") 
data <- na.omit(original_data)

data <- data |> mutate(average_score = (sentimentAnalysis_score*0.5)+ (sentimentr_score*0.3)+ (syuzhet_score*0.2))
#Average character sentiment by season
average_season <- data |>
  filter(character == selected_character) |>
  group_by(season) |>
  summarise(mean_score = mean(average_score, na.rm = TRUE))
#Average character sentiment by episode
episode_data <- data |> 
  distinct(season, episode) |>
  mutate(episode_index = row_number())
new_data <- merge(data, episode_data, by = c("season", "episode")) |> filter(character == selected_character)

average_episode <- new_data |>
  group_by(episode_index) |>
  summarise(mean_score = mean(average_score, na.rm = TRUE)) 

ggplot(average_season, aes(x = season, y = mean_score)) + geom_line()


