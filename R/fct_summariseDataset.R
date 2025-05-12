#' summariseDataset
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

summariseDataset <- function(dataset) {
  stopifnot(is.data.frame(dataset),
            c("Test", "Value") %in% names(dataset))
  
  data <- dataset |>
    select(c("ID", "Initials", "Group", "Test", "Value")) |>
    pivot_wider(names_from = Test,
                values_from = Value)
  
  data$diff <- data$Stroop -  data$Control
  
  return(
    list(
      Leader = data$Initials[which.min(data$diff)],
      ControlMean = mean(data$Control),
      StroopMean = mean(data$Stroop),
      MeanDifference = mean(data$diff)
    )
  )
  
}
