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
      showcase = bs_icon("person-arms-up"), 
      class = "bg-secondary",
      min_height = "120px"
    ),
    value_box(
      title = "Mean Control Experiment",
      value = textOutput(ns("control")),
      p("seconds"), 
      showcase = bs_icon("segmented-nav"),
      class = "bg-secondary",
      min_height = "120px"
    ),
    value_box(
      title = "Mean Stroop Experiment",
      value = textOutput(ns("stroop")),
      p("seconds"), 
      showcase = bs_icon("trophy-fill"), 
      class = "bg-secondary",
      min_height = "120px"
    ),
    value_box(
      title = "Mean Difference",
      value = textOutput(ns("difference")),
      p("seconds"), 
      showcase = bs_icon("layout-split"),
      class = "bg-secondary", 
      min_height = "120px"
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
      req(dataset(), nrow(dataset()) >1)
     
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
