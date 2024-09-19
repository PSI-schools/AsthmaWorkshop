#' checkFunction 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

#' @importFrom googledrive drive_find
checkFunction <- function(id = NULL) {
  if (is.null(id)) {
    return(invisible(NULL))
  } else {
    availableSheets <- drive_find(type = "spreadsheet")
    return(availableSheets[availableSheets$id == id][["drive_resource"]][[1L]][["modifiedTime"]]) 
  }
}