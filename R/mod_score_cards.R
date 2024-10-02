#' score_cards UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_score_cards_ui <- function(id) {
  ns <- NS(id)
  tagList(layout_columns(
    value_box(
      title = "Leader",
      value = textOutput(ns("leader")),
      showcase = bs_icon("person-arms-up")
    ),
    value_box(
      title = "control",
      value = textOutput(ns("control")),
      showcase = bs_icon("segmented-nav")
    ),
    value_box(
      title = "stroop",
      value = textOutput(ns("stroop")),
      showcase = bs_icon("trophy-fill")
    ),
    value_box(
      title = "Difference",
      value = textOutput(ns("difference")),
      showcase = bs_icon("layout-split")
    )
  ))
}

#' score_cards Server Functions
#'
#' @noRd
mod_score_cards_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Values to display on value cards
    info <- reactiveValues(
      leader = character(0L),
      control = character(0L),
      stroop = character(0L),
      difference = character(0L)
    )
    
    
    observe({
      req(dataset())
     
      outcome <- summariseDataset(dataset())
      
      info$leader <-  outcome$Leader 
      info$control <- outcome$ControlMean 
      info$stroop <- outcome$StroopMean 
      info$difference <- outcome$MeanDifference 
    })
    
    
    output$leader <- renderText({
      info$leader
    })
    
    output$control <- renderText({
      round(info$control, 2)
    })
    
    output$stroop <- renderText({
      round(info$stroop, 2)
    })
    
    output$difference <- renderText({
      round(info$difference, 2)
    })
  })
}
