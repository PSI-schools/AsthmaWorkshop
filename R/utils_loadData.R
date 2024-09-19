#' loadData 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' 
#' @importFrom googlesheets4 read_sheet

loadData <- function(id = NULL, ...) {
  
  if (is.null(id)) {
    return(data.frame(
      ID = character(0L),
      Initials = character(0L),
      Group = character(0L),
      Height = numeric(0L),
      Sex = factor(levels = c("Male", "Female")),
      Value = numeric(0L)
    ))
  }
  
  # Read the data
  read_sheet(ss = id)
}
