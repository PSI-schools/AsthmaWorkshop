#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_fluid card card_header card_body
#' @importFrom shinyWidgets actionBttn checkboxGroupButtons

mod_admin_ui <- function(id) {
  ns <- NS(id)
  page_fluid(card(
    card_header(h2("Admin"), class = "bg-primary"),
    card_body(
      textInput(ns("googlesheets"), label = "Google Sheets url"),
      checkboxGroupButtons(
        inputId = ns("treatment"),
        label = "Treatment",
        choices = c(
          "Placebo",
          "Control",
          "Treatment",
          "Treatment A",
          "Treatment B"
        ),
        selected = c("Control", "Treatment"),
        status = "secondary",
        individual = TRUE,
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon")
        )
      ),
      checkboxGroupButtons(
        inputId = ns("group"),
        label = "Class Group",
        choices = c("Group A",
                    "Group B",
                    "Group C",
                    "Group D",
                    "Group E"),
        selected = c("Group A", "Group B", "Group C"),
        status = "secondary",
        individual = TRUE,
        checkIcon = list(
          yes = icon("ok",
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon")
        )
      ),
      actionBttn(inputId = ns("update"), block = TRUE)
    )
  ))
}

#' admin Server Functions
#'
#' @noRd
mod_admin_server <- function(id, application_state, user_choices) {
  moduleServer(id, function(input,
                            output,
                            session) {
    ns <- session$ns
    
    observeEvent(input$update,
                 {
                   if (!identical(application_state$google_sheets, input$googlesheets)) {
                     application_state$google_sheets <- input$googlesheets
                   }
                   
                   user_choices$Treatment <- input$treatment
                   user_choices$Group <- input$group
                   
                 },
                 ignoreNULL = TRUE,
                 ignoreInit = TRUE)
    
    
  })
}
