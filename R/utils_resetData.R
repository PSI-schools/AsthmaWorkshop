#' resetData
#'
#' @author Gareth Burns
#' @description A function to reset the data from Googlesheets if
#' the metadata is a specified date.
#'
#' This function is ran on loading of the application and
#'
#' @param date
#' @param id description
#' @return Silent. Function for side effects
#' @noRd
#' @export

reset_data <- function(date = Sys.Date(), id) {

  # Read the Google Sheet into a data frame
  sheet_data <- read_sheet(sheet_url)

  # Check if the metadata date column is equal to today's date
  if (any(sheet_data$date != todays_date)) {
    # Filter out rows where the date is not today (assuming 'date' column exists)
    updated_data <- sheet_data %>%
      filter(date == todays_date)

    # If no data for today, remove all data
    if (nrow(updated_data) == 0) {
      # Delete all rows (clear the sheet)
      range_delete(sheet_url, range = "A2:Z")
      message("Data deleted as no valid records are for today.")
    } else {
      # Otherwise, write the valid data back
      write_sheet(updated_data, sheet_url)
      message("Data updated with today's valid records.")
    }
  } else {
    message("All data is already from today.")
  }
}
