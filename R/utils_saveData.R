#' saveData 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' @importFrom googlesheets4 sheet_append

saveData <- function(id, data, with_progress = TRUE, ...) {
  
  stopifnot(is.character(id), is.data.frame(data))
  
  withProgress(message = "Preparing data for upload", expr = {
    
    # Slowing down to reduce API calls (limit of 60 per second)
    Sys.sleep(0.5)
    
    setProgress(value = 0.5, message = "Uploading Data")
    
    # Check if drive exists and can be accessed.
    # if (id %in% drive_find(type = "spreadsheet")) {
    #   
    # } else {
    #   
    # }
    # Append new rows
    sheet_append(id, data = data)
    
    setProgress(value = 1.0, message = "Uploading Complete")
    Sys.sleep(0.2)
  })
  
  
} 