#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  applicationState <- reactiveValues(googlesheets = NULL)
  
  userChoices <- reactiveValues(Group = c("Group A", "Group B", "Group C"),
                               Treatment = c("Control", "Placebo"))
  
  data <-
    reactiveValues(
      GroupData = data.frame(
        ID = character(0L),
        Person = character(0L),
        Group = character(0L),
        Treatment = character(0L),
        Value = numeric(0L)
      ), 
      ClassData = data.frame(
        ID = character(0L),
        Person = character(0L),
        Group = character(0L),
        Treatment = character(0L),
        Value = numeric(0L)
      )
    )
  
  mod_data_entry_server("data_entry", data = data, application_state = applicationState, user_choices = userChoices)
  mod_admin_server("admin", application_state = applicationState, user_choices = userChoices)
  
}
