library(dplyr)
library(ggplot2)

original_data <- read.csv("data/office_sentiment .csv") 
data <- na.omit(original_data)

data <- data |> mutate(average_score = (sentimentAnalysis_score*0.5)+ (sentimentr_score*0.3)+ (syuzhet_score*0.2))
# Mean Character Sentiment Score

result <- data |>
  filter(character == "Pam")
  group_by(season) |>
  summarize(mean_score = mean(average_score, na.rm = TRUE)) |>
  filter(mean_score != 0)
ggplot(result , aes(x = character, y = mean_score)) + geom_line()
