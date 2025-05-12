#' saveData
#'
#' @description A function to append a users data to class data.
#' @return None. Used for side-effects of saving data to GoogleSheets.
#' @importFrom googlesheets4 sheet_append
#' @importFrom shiny withProgress
#' @noRd

saveData <- function(id, data, ...) {

  stopifnot(is.character(id), is.data.frame(data))

  withProgress(message = "Preparing data for upload", expr = {

    # Slowing down to reduce API calls
    # due to rate-limit of 60 calls per second
    Sys.sleep(0.5)

    setProgress(value = 0.5, message = "Uploading Data")

    sheet_append(id, data = data)

    setProgress(value = 1.0, message = "Uploading Complete")
    Sys.sleep(0.2)
  })


}
