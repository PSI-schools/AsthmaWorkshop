#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom googlesheets4 gs4_auth gs4_deauth
#' @importFrom googledrive drive_deauth drive_auth
#' @noRd
app_server <- function(input, output, session) {
  
  library(ggplot2)
  library(extrafont)
  gs4_deauth()
  drive_deauth()
  drive_auth(cache = ".secrets", email = "psischoolsinitiative@gmail.com")
  gs4_auth(cache = ".secrets", email = "psischoolsinitiative@gmail.com")
  
  applicationState <-
    reactiveValues(GoogleSheets = "1KEpGStSMctMBiATDwQlqdP5uu5qUFf_Sumjrk-As5HU")
  
  userChoices <-
    reactiveValues(
      ValueLabel = "Time (s)",
      Group = c("Group A", "Group B"),
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
          
          if (id %notin% availableSheets$id) {
            cat(sprintf("GoogleSheets id:%s does not exist or you do not have permission to access it", id))
                return(invisible(NULL))
          }
          
          return(availableSheets[availableSheets$id == id,][["drive_resource"]][[1L]][["modifiedTime"]])
        }
      },
      valueFunc = function(id = applicationState$GoogleSheets) {
        if (is.null(id)) {
          return(data.frame(
            ID = character(0L),
            Initials = character(0L),
            Group = character(0L),
            Order = numeric(0L),
            Test = character(0L),
            Value = numeric(0L)
          ))
        }
        # Read the data
        read_sheet(ss = id)
      }
  )
  
  mod_stroop_test_server(
    "stroop_test",
    class_data = classData,
    application_state = applicationState
  )
  mod_results_table_server("results_table", 
                           class_data = classData, 
                           user_choices = userChoices)
  mod_view_data_server("view_data", 
                       class_data = classData, 
                       user_choices = userChoices)
  mod_admin_server("admin",
                   application_state = applicationState,
                   user_choices = userChoices)
  
}
