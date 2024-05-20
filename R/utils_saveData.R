#' saveData 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' @importFrom googlesheets4 sheet_append

saveData <- function(id, data, ...) {
  
  stopifnot(is.character(id), is.data.frame(data))
  
  # Check if drive exists and can be accessed.
  # if (id %in% drive_find(type = "spreadsheet")) {
  #   
  # } else {
  #   
  # }
  # Append new rows
  sheet_append(id, data = data)
} 