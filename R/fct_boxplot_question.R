#' boxplot_question
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

BoxPlotQuestion <- function(values, group = "Stroop", ...) {
  summary <- fivenum(values)
  
  Answers <-
    c(c(Answer = summary[3]), summary[sample(c(1, 2, 4, 5), size = 3, replace = FALSE)])[sample(1:4, size = 4, replace = FALSE)]
  
  return(
    list(
      Question = sprintf("What is the median time taken for the %s experiment", 
                         group),
      ChoiceA = round(Answers[[1]], 2),
      ChoiceB = round(Answers[[2]], 2),
      ChoiceC = round(Answers[[3]], 2),
      ChoiceD = round(Answers[[4]], 2),
      Answer = Answers[names(Answers) == "Answer"]
    )
  )
}