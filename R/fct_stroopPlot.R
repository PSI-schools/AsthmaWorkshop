#' stroopPlot 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 

StroopPlot <- function(group, ...) {
  words <- c("RED", "BLUE", "GREEN", "PURPLE", "ORANGE")
  colors <-
    c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
  my_dgrey <- "#f0f0f0"
  
  my_theme <- theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 25),
    panel.background = element_rect(fill = my_dgrey),
    panel.border = element_rect(fill = NA),
    panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )
  
  df <- data.frame(
    word = sample(words, 16, replace = TRUE),
    color = sample(colors, 16, replace = TRUE),
    x = rep(1:4, each = 4),
    y = rep(1:4, times = 4)
  )
  
  df1 <- data.frame(
    (i = sample(1:5, 16, replace = TRUE)),
    word = words[i],
    color = colors[i],
    x = rep(1:4, each = 4),
    y = rep(1:4, times = 4)
  )
  
  
  if (group != "Stroop") {
    df <- df1
  }
  
  
  # Plot using ggplot2
  ggplot(df, aes(
    x = factor(x),
    y = y,
    label = word,
    color = color
  )) +
    geom_text(size = 12) +
    scale_x_discrete(limits = factor(1:4)) +
    scale_color_identity() +
    theme_void() +
    theme(legend.position = "none") +
    coord_fixed() +
    my_theme
  
}
