#' Histogram function
#'
#' This function creates a Histogram with ggplot2.
#'
#' @param data The data frame containing the variables.
#' @param x The variable used for the x-axis.
#' @param y The variable used for the y-axis.
#' @param fill The variable used for fill color.
#'
#' @return A ggplot object displaying a Histogram.
#'
#' @import ggplot2
#'
#' @examples
#' data <- data.frame(
#'   Peak_Expiratory_Flow = c(250, 300, 280),
#'   Height = c(160, 170, 165),
#'   Treatment = c('Placebo', 'Drug', 'Placebo')
#' )
#' Histogram(data = data, x = Peak_Expiratory_Flow, y = Height, fill = Treatment)


Histogram <- function(data, x, y = NULL, group) {
  plot <- ggplot(data = data,
                 aes(
                   x = !!sym(x),
                   y = after_stat(density),
                   fill = !!sym(group)
                 )) +
    geom_histogram(binwidth = 20,
                   color = MyPallette$MyPallette$black,
                   alpha = 0.7) +
    labs(x = "Peak Expiratory Flow (L/min)",
         y = "Frequency Density") +
    scale_fill_manual(
      "Treatment",
      labels = c("Placebo", "Drug"),
      values = c(MyPallette$col_pla, MyPallette$col_drug)
    ) +
    scale_color_manual(
      "Treatment",
      labels = c("Placebo", "Drug"),
      values = c(MyPallette$col_pla, MyPallette$col_drug)
    )
    # ) +
    # facet_grid(rows = vars(!!group))
  
  return(plot)
  
  
}
