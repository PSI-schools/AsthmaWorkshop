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
#' @importFrom 'reactable' 'reactableOutput' 'colDef' 'colFormat' 'reactableTheme'


mod_results_table_ui <- function(id) {
  ns <- NS(id)
  tagList(card(card_header(
    h2("Compare your scores with your class"),
    class = "bg-primary"
  ),
  card_body(
    card(
      mod_score_cards_ui(ns("score_card")),
      reactableOutput(ns("results_table")),
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
        
        # Datawrangliing of data for table 
        data <- class_data() |>
          select(c("ID", "Initials", "Group", "Test", "Value")) |>
          pivot_wider(names_from = Test, 
                      values_from = Value)
        
        data$diff <- data$Stroop - data$Control
        
        reactable(data = data,
                  columns = list(
                    ID = colDef(show = FALSE),
                    Control = colDef(name = "Control Time (s)", 
                                    format = colFormat(digits = 2)),
                    Stroop = colDef(name = "Stroop Time (s)", 
                                   format = colFormat(digits = 2)), 
                    diff = colDef(name = "Time Difference (s)", 
                                  format = colFormat(digits = 2))
                  ),
                  # Global table options
                  pagination = TRUE,
                  highlight = TRUE,
                  bordered = TRUE,
                  striped = TRUE,
                  defaultPageSize = 20,
                  defaultSorted = list(diff = "asc"),
                  searchable = FALSE,
                  theme = reactableTheme(
                    borderColor = "#D3D3D3",
                    stripedColor = "#F9F9F9",
                    highlightColor = "#D8EBF9",
                    cellPadding = "8px",
                    style = list(fontFamily = "Arial", fontSize = "14px"),
                    searchInputStyle = list(width = "100%")
                  ))
      })
      
      mod_score_cards_server("score_card", dataset = class_data)
      
    })
  }