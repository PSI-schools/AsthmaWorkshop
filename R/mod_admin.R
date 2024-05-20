#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_fluid card card_header card_body
#' @importFrom shinyWidgets actionBttn

mod_admin_ui <- function(id) {
  ns <- NS(id)
  page_fluid(card(card_header(h2("Admin"), class = "bg-primary"),
                  card_body(
                    textInput(ns("googlesheets"), label = "Google Sheets url"),
                    actionBttn(ns("update"))
                  )))
}

#' admin Server Functions
#'
#' @noRd
mod_admin_server <- function(id, application_state) {
  moduleServer(id, function(input,
                            output,
                            session) {
    ns <- session$ns
    
    observeEvent(input$update, {
      application_state$google_sheets <- input$googlesheets
    })
    
    
  })
}
