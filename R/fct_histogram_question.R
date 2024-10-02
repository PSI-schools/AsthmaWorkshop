#' histogram_question
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

HistogramQuestion <- function(values, ...) {
  # Create intervals
  intervals <-
    cut(values,
        breaks = seq(floor(min(values)), ceiling(max(values)), by = 1),
        right = FALSE)
  
  # Count the number of values in each interval
  interval_counts <- table(intervals)
  
  Answer <- interval_counts[which.max(interval_counts)]
  
  potentialAnswers <- interval_counts[interval_counts <interval_counts[which.max(interval_counts)]]
  
  Answers <-
    c(c(Answer = names(interval_counts[which.max(interval_counts)])),
      names(potentialAnswers[sample(1:length(potentialAnswers), size = 3, replace = FALSE)]))[sample(1:4, size = 4, replace = FALSE)]
  
  return(
    list(
      Question = "Which bin has the most values in it",
      ChoiceA = Answers[[1]],
      ChoiceB = Answers[[2]],
      ChoiceC = Answers[[3]],
      ChoiceD = Answers[[4]],
      Answer = Answers[names(Answers) == "Answer"]
    )
  )
}
