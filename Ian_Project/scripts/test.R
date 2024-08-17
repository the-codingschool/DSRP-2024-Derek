library(dplyr)
library(ggplot2)

original_data <- read.csv("data/office_sentiment .csv") 
data <- na.omit(original_data)

data <- data |> mutate(average_score = (sentimentAnalysis_score*0.5)+ (sentimentr_score*0.3)+ (syuzhet_score*0.2))
# Mean Character Sentiment Score
filtered_data <- data |>
  group_by(character) |>
  filter(n() > 100) |>
  summarize(ncount=n())
ggplot(filtered_data , aes(x = character, y = ncount)) + geom_bar(stat = "Identity")
