#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  applicationState <-
    reactiveValues(GoogleSheets = "1KEpGStSMctMBiATDwQlqdP5uu5qUFf_Sumjrk-As5HU")
  
  userChoices <-
    reactiveValues(
      Group = c("Group A", "Group B", "Group C"),
      Treatment = c("Control", "Placebo")
    )
  
  classData <-
    reactivePoll(
      20000,
      session,
      checkFunc = function(id = applicationState$GoogleSheets) {
        if (is.null(id)) {
          return(invisible(NULL))
        } else {
          availableSheets <- drive_find(type = "spreadsheet")
          return(availableSheets[availableSheets$id == id][["drive_resource"]][[1L]][["modifiedTime"]])
        }
      },
      valueFunc = function(id = applicationState$GoogleSheets) {
        if (is.null(id)) {
          return(data.frame(
            ID = character(0L),
            Person = character(0L),
            Group = character(0L),
            Treatment = character(0L),
            Value = numeric(0L)
          ))
        }
        
        # Read the data
        read_sheet(ss = id)
      }
       
    )
  
  data <-
    reactiveValues(
      GroupData = data.frame(
        ID = character(0L),
        Person = character(0L),
        Group = character(0L),
        Treatment = character(0L),
        Value = numeric(0L)
      ))
  
  mod_data_entry_server(
    "data_entry",
    data = data,
    class_data = classData,
    application_state = applicationState,
    user_choices = userChoices
  )
  mod_admin_server("admin",
                   application_state = applicationState,
                   user_choices = userChoices)
  
}
