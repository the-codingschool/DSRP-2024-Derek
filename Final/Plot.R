# 1. Extend the dataset with a pretend season 10
season_10 <- data.frame(
  season = 10,
  episode = 1:27,  # 27 episodes in season 10
  writer_category = rep(c("Writer-Actor", "Writer-Only"), length.out = 27),
  episode_name = paste("Episode", 1:27),  # Placeholder names for episodes
  total_votes = sample(1000:5000, 27, replace = TRUE)  # Random vote counts
)

# Fit linear regression models
model_writer_actor <- lm(mean_imdb_rating ~ episode, data = imdb_writer_summary %>% filter(writer_category == "Writer-Actor"))
model_writer_only <- lm(mean_imdb_rating ~ episode, data = imdb_writer_summary %>% filter(writer_category == "Writer-Only"))

# Predict IMDb ratings for each episode in season 10
season_10 <- season_10 %>%
  mutate(
    mean_imdb_rating = ifelse(writer_category == "Writer-Actor", 
                              predict(model_writer_actor, newdata = season_10),
                              predict(model_writer_only, newdata = season_10))
  )

# Combine the datasets
imdb_writer_summary_extended <- bind_rows(imdb_writer_summary, season_10)

# Define color palette
color_palette <- c(
  "Writer-Actor" = "darkblue",
  "Writer-Only" = "lightblue"
)

# Create the plot
imdbgraph <- plot_ly(
  data = imdb_writer_summary_extended,
  x = ~episode,
  y = ~mean_imdb_rating,
  frame = ~season,
  size = ~total_votes,
  color = ~writer_category,
  colors = color_palette,
  type = 'scatter',
  mode = 'markers',  # Keep the type as markers
  text = ~paste(
    "Episode:", episode_name, 
    "<br>Writer Category:", writer_category, 
    "<br>IMDb Rating:", round(mean_imdb_rating, 2),
    "<br>Total Votes:", total_votes
  ),
  hoverinfo = 'text',
  marker = list(sizemode = 'diameter', opacity = 0.7)
) %>%
  layout(
    title = list(
      text = "IMDb Ratings of Episodes by Writer Category Across Seasons",
      font = list(
        family = "Arial",
        size = 20,
        color = "#4B3F2D",
        face = "bold"
      )
    ),
    xaxis = list(
      title = "Episode Number",
      titlefont = list(
        family = "Arial",
        size = 14,
        color = "#4B3F2D",
        face = "bold"
      ),
      tickfont = list(
        family = "Arial",
        size = 12,
        color = "#4B3F2D"
      )
    ),
    yaxis = list(
      title = "IMDb Rating",
      titlefont = list(
        family = "Arial",
        size = 14,
        color = "#4B3F2D",
        face = "bold"
      ),
      tickfont = list(
        family = "Arial",
        size = 12,
        color = "#4B3F2D"
      ),
      range = c(0, 10)
    ),
    paper_bgcolor = "#f2e9dd",
    plot_bgcolor = "#FFFFFE",
    showlegend = TRUE
  )

# Show the plot
imdbgraph

