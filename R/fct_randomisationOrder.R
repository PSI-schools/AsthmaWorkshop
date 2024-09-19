#' randomisationOrder
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import ggplot2
#' @import emojifont


randomisationOrder <-
  function(data,
           values_per_row = 20,
           size = 14,
           filter = 20) {
    data$x <-
      1:nrow(data) - (floor(1:nrow(data) / (values_per_row + 1)) * values_per_row)
    data$y <-
      1 - (floor(1:nrow(data) / values_per_row) * (size  / 150))
    rows <-  ceiling(nrow(data) / values_per_row)
    
    data$colour <- ifelse(data$order == "Non-Stroop", colours$control, colours$stroop)
    
    data <-
      data[1:min(nrow(data), filter), 1:ncol(data), drop = FALSE]
    
    plot <-  ggplot(data = data, aes(x = x, y = y)) +
      geom_fontawesome(
        x = data$x,
        y = data$y,
        alias = "fa-child",
        size = size,
        color = data$colour
      ) +
      labs(
        caption = sprintf("The randomisation outcome for the previous %s people", 
                          min(nrow(data), filter))
        ) +
          theme_minimal() +
          theme(
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            plot.caption = element_text(hjust = 0, size = 14)
          )
        
        
        if (rows == 1) {
          coords <- coord_cartesian(ylim = c(1 - size / 400, 1 + size / 400))
        } else {
          coords <-
            coord_cartesian(ylim = c(1 - rows * (size / 250), 1 + size / 400))
        }
        plot + coords
        
  }
