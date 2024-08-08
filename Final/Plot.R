#create the animated plot
imdbgraph <- plot_ly(
  data = imdb_writer_summary,
  x = ~episode,
  y = ~mean_imdb_rating,
  frame = ~season,
  size = ~total_votes,
  color = ~writer_category,
  type = 'scatter',
  mode = 'markers',
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
    title = "IMDb Ratings of Episodes by Writer Category Across Seasons",
    xaxis = list(title = "Episode Number"),
    yaxis = list(title = "IMDb Rating", range = c(0, 10)),
    showlegend = TRUE
  )

#show the plot
imdbgraph