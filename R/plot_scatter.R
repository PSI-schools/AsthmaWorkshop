#' Scatter plot function
#'
#' This function creates a scatter plot with ggplot2.
#'
#' @param data The data frame containing the variables.
#' @param x The variable used for the x-axis.
#' @param y The variable used for the y-axis.
#' @param fill The variable used for fill color.
#' @param label The variable used for labels. Optional.
#'
#' @return A ggplot object displaying a scatter plot.
#'
#' @import ggplot2
#'
#' @examples
#' data <- data.frame(
#'   Peak_Expiratory_Flow = c(250, 300, 280),
#'   Height = c(160, 170, 165),
#'   Treatment = c('Placebo', 'Drug', 'Placebo')
#' )
#' ScatterPlot(data = data, x = Peak_Expiratory_Flow, y = Height, fill = Treatment)
#'

ScatterPlot <- function(data, x, y, fill, label = NULL) {
  plot <-
    ggplot(data = data, aes(x = {{x}}, {{y}}, fill = {{fill}})) +
    geom_point(
      shape = 21,
      color = MyPallette$black,
      size = 10,
      alpha = 0.7
    ) +
    labs(x = "Peak Expiratory Flow",
         y = "Height (cm)") +
    scale_fill_manual(
      "Treatment",
      labels = c("Placebo", "Drug"),
      values = c(MyPallette$col_pla, MyPallette$col_drug)
    )
  if (notnull(label)) {
    plot <-
      plot + geom_text(aes(labels = {{label}}), size = 6, nudge_y = 0.1) 
  }
  
  return(plot)
  
  
}
