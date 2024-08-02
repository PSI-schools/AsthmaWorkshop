#' scoreboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom 'shinyWidgets' 'actionBttn'
#' @importFrom 'reactable' 'reactableOutput' 'colDef' 'colFormat'


mod_results_table_ui <- function(id) {
  ns <- NS(id)
  tagList(card(card_header(
    h2("Compare your scores with your class"),
    class = "bg-primary"
  ),
  card_body(
    card(reactableOutput(ns("results_table")),
         full_screen = TRUE)
  )))
}

#' results_table Server Functions
#'
#' @noRd

mod_results_table_server <-
  function(id, class_data, user_choices) {
    stopifnot(is.reactive(class_data))
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      output$results_table <- renderReactable({
        validate(need(nrow(class_data()) > 0, "Please enter some values"))
        reactable(data = class_data(),
                  columns = list(
                    ID = colDef(show = FALSE),
                    Value = colDef(name = user_choices$ValueLabel, 
                                   format = colFormat(digits = 2))
                  ))
      })
    })
  }