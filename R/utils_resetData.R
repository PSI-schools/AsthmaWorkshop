#' resetData
#'
#' @author Gareth Burns
#' @description A function to reset the data from Googlesheets if
#' the metadata is a specified date.
#'
#' This function runs on loading of the application to check if there is any
#' data persisting from previous session and removes it.
#'
#' @param id A Character vector. The UUID of the GoogleSheets document used for
#'   persistent storage
#' @param lag A Double. The period of time in days before day that data should
#'   be retained from. See details.
#' @details
#' If a lag of one is supplied all from today and yesterday will be retained. All
#' older data will be deleted.
#' Date Age is attributed to the modified data meta data of the GoogleSheet.
#' @return Silent. Function for side effects
#' @importFrom lubridate ymd_hms today days
#' @importFrom googledrive drive_find
#' @importFrom googlesheets4 read_sheet sheet_write
#' @noRd
#' @export

reset_data <- function(id, lag = 0L) {

  if (is.null(id)) {
    return(invisible(NULL))
  } else {
    availableSheets <- drive_find(type = "spreadsheet")

    if (id %notin% availableSheets$id) {
      warning(
        sprintf(
          "GoogleSheets id:%s does not exist or you do not have permission to access it",
          id
        )
      )
      return(invisible(NULL))
    }

    classDataTime <- ymd_hms(availableSheets[availableSheets$id == id, ][["drive_resource"]][[1L]][["modifiedTime"]], tz = "UTC")

    if (isTRUE(classDataTime < today(tz = "UTC") - days(lag))) {
      # Read just the headers
      headers <- read_sheet(id, sheet = "Sheet1", n_max = 0)

      # Re-write just the headers to clear everything else
      sheet_write(data = headers[0, ],
                  ss = id,
                  sheet = "Sheet1")

      message(sprintf("Old Data Records from %s Cleared", classDataTime))

    }
  }
}
