#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom googlesheets4 gs4_auth gs4_deauth
#' @importFrom googledrive drive_deauth drive_auth
#' @noRd

# See Readme section Sensitive Information for updating these.
email <- Sys.getenv("Email")
GoogleSheetsID <- Sys.getenv("GoogleSheetsID")

# A lag of 0 will delete all data older than today
# reset_data(lag = 0, id = GoogleSheetsID)

app_server <- function(input, output, session) {


  dir.create('~/.fonts')
  file.copy("www/FontAwesome.ttf", "~/.fonts")
  system('fc-cache -f ~/.fonts')

  library(ggplot2)
  library(extrafont)

  # This lines are for checking Google auth locally
  # gs4_deauth()
  # drive_deauth()

  # See readme Authentication for details on maintaining this.
  drive_auth(cache = ".secrets", email = email)
  gs4_auth(cache = ".secrets", email = email)

  # Application State is the Global State of the application passed to all
  # Modules. This is a reactiveValues for future design whereby an admin
  # user may want to update the sheets
  applicationState <-
    reactiveValues(GoogleSheets = GoogleSheetsID)

  userChoices <-
    reactiveValues(
      ValueLabel = "Time (s)",
      Group = c("Group A", "Group B"),
      Treatment = c("Control", "Placebo")
    )

  # The class data contains the data.frame that updates every 10,000ms (10 seconds)
  # This ensures the data for the results is updated.
  # Note the Google API is free and has rate limiting. This may need to be updated
  # if this rate limiting is effecting performance.
  classData <-
    reactivePoll(
      10000,
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

  # Trigger is a reactiveVal that is used to trigger reactivity outside the module.
  # When the trigger is activated it navigates to the results tab.
  trigger <- reactiveVal()

  observeEvent(trigger(), {
    updateNavbarPage(session, "navbar", selected = "results")
  }, ignoreInit = TRUE)


  mod_stroop_test_server(
    "stroop_test",
    class_data = classData,
    application_state = applicationState,
    trigger = trigger
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
