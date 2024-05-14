#' data_entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom bslib page_sidebar sidebar
#' @importFrom reactable reactableOutput reactable renderReactable
#' @importFrom shinyWidgets radioGroupButtons

mod_data_entry_ui <- function(id){
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = "30%",
      "Entry Patient Details",
      textInput(ns("name"), label = "Name"),
      radioGroupButtons(
        inputId = ns("group"),
        label = "Class Group",
        choices = c("Group A", "Group B", "Group C"),
        status = "primary"
      ),
      radioGroupButtons(
        inputId = ns("treatment"),
        label = "Treament",
        choices = c("Placebo", "Condition"),
        status = "primary"
      ),
      numericInput(
        inputId = ns("value"),
        label = "Value",
        value = NULL,
        min = 0L
      ),
      actionButton(inputId = ns("add"), label = "Add")
    ),
    reactableOutput(ns("table"))
  )
}
    
#' data_entry Server Functions
#'
#' @noRd 
mod_data_entry_server <- function(id, data, application_state){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$add, {
      data$dataset[nrow(data$dataset) + 1, ] <-
        c(input$name, input$group, input$treatment, input$value)
    }, ignoreInit = TRUE)
    
    output$table <- renderReactable({
      if (nrow(data$dataset) <= 0) {
        validate("Please enter some values")
      }
      reactable(data = data$dataset)
    })
 
  })
}
