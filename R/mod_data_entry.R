#' data_entry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom bslib page_sidebar sidebar navset_card_tab nav_panel
#' @importFrom reactable reactableOutput reactable renderReactable
#' @importFrom shinyWidgets radioGroupButtons sendSweetAlert

mod_data_entry_ui <- function(id){
  ns <- NS(id)
  page_sidebar(
    sidebar = sidebar(
      width = "30%",
      "Enter Patient Details",
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
    navset_card_tab(
      nav_panel("Group Data", reactableOutput(ns("group_table")), 
                actionButton(inputId = ns("upload"), label = "Upload")),
      nav_panel("Class Data", reactableOutput(ns("class_table"))),
    )
  )
}
    
#' data_entry Server Functions
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @noRd 

 
mod_data_entry_server <- function(id, data, application_state, user_choices){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(user_choices$Treatment, {
      # browser()
      updateRadioGroupButtons(session = session,
                              inputId = ns("treatment"), 
                              choices = user_choices$Treatment)
    }, label = "UpdateTreatmentOptions", ignoreNULL = TRUE, ignoreInit = TRUE)
    
    observeEvent(input$add, {
      data$GroupData[nrow(data$GroupData) + 1, ] <-
        c(UUIDgenerate(), input$name, input$group, input$treatment, input$value)
    }, ignoreInit = TRUE)
    
    output$group_table <- renderReactable({
      if (nrow(data$GroupData) <= 0) {
        validate("Please enter some values")
      }
      reactable(data = data$GroupData)
    })
    
    output$class_table <- renderReactable({
      if (nrow(data$ClassData) <= 0) {
        validate("Please enter some values")
      }
      reactable(data = data$ClassData)
    })
    
    
    observeEvent(input$upload, {
      
      if (is.null(application_state$GoogleSheets)) {
        sendSweetAlert(
          session = session,
          title = "Googlesheets",
          text = "If you wish to use multiple computers, these data need stored on Googlesheets.
          See the user guide for help setting up Googlesheets",
          type = "info"
        )
      } else if (isTRUE((!isTruthy(data$ClassData) && nrow(data$ClassData)> 0))){
        sendSweetAlert(
          session = session,
          title = "No Data to Upload",
          text = "Before uploading data to Googlesheets, please make sure you've entered
          all the data from yout group",
          type = "warning"
        )
      } else {
        saveData(url = application_state$GoogleSheets, data = data$ClassData)
        sendSweetAlert(
          session = session,
          title = "Success: Data Upload",
          text = "The Group Data was successfully uploaded. Once each group has uploaded 
        their data be sure to compare results!",
          type = "success"
        )
      }
      

    })
  })
}
