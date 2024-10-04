#' stroop_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_sidebar sidebar navset_card_tab nav_panel layout_columns
#' @importFrom reactable reactableOutput reactable renderReactable
#' @importFrom shinyWidgets radioGroupButtons sendSweetAlert

mod_stroop_test_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(
    tags$style(
      HTML(
        "
    .stopwatch-timer {
      font-size: 24px;           /* Large font for timer */
      font-family: 'Courier New', Courier, monospace;  /* Monospace font */
      border: 2px solid black;   /* Border to give a stopwatch-like look */
      border-radius: 10px;       /* Rounded corners */
      padding: 5px;             /* Padding for spacing */
      width: 150px;              /* Fixed width for consistency */
      text-align: center;        /* Center the text inside */
      background-color: #f0f0f0; /* Light background color */
      margin: 10px auto;         /* Center horizontally on the page */
    }
  "
      )
    ),
    sidebar = sidebar(
      width = "30%",
      open = "always",
      card(
        card_header(
          class = "bg-primary d-flex justify-content-between align-items-center",
          strong("Subject Details"),
          mod_randomisation_ui(ns("randomisation"))
        ),
        card_body(
          layout_columns(
            textInput(ns("initials"), label = "Your Initials", value = ""),
            layout_columns(div(
              class = "mb-3",
              p("Current Experiment"),
              textOutput(ns("current_experiment"))
            ))
          )
          ,
          radioGroupButtons(
            inputId = ns("group"),
            label = "Class Group",
            choices = c("A",
                        "B", "C", "D"),
            status = "primary"
          )
        )
      ),
      card(
        card_header(
          class = "bg-primary d-flex justify-content-between align-items-center",
          strong("StopWatch"),
          div(
            actionButton(
              inputId = ns("start"),
              label = "Start",
              icon = icon("play"),
              style = "background-color: green; color: white;",
              disabled = TRUE
            ),
            actionButton(
              inputId = ns("stop"),
              label = "Stop",
              icon = icon("stop"),
              style = "background-color: red; color: white;",
              disabled = TRUE
            )
          )
        ),
        card_body(div(
          id = "stopwatch-container",
          div(textOutput(ns("timer")), class = "stopwatch-timer")  # Wrap with div for styling
        ))
      ),
      card(
        card_header(
          class = "bg-primary d-flex justify-content-between align-items-center",
          strong("Your times"),
          actionButton(
            inputId = ns("submit"),
            label = "Submit",
            icon = icon("cloud-arrow-up"),
            disabled = TRUE,
            style = "background-color: green; color: white;",
          )
        ),
        card_body(layout_columns(
          div(
            class = "mb-1",
            strong("Result 1"),
            div(class = "stopwatch-timer",
                textOutput(ns("time1")))
          ),
          div(
            class = "mb-1",
            strong("Result 2"),
            div(class = "stopwatch-timer",
                textOutput(ns("time2")))
          )
        ))
      )
    ),
    card(
      card_header(h2("Experiment"), class = "bg-primary"),
      card_body(plotOutput(ns("stroop_plot"), width = "100%"))
    )
  )
}

#' stroop_test Server Functions
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @importFrom uuid UUIDgenerate
#' @importFrom reactable reactable colDef
#' @importFrom bslib value_box
#' @importFrom bsicons bs_icon
#' @noRd


mod_stroop_test_server <-
  function(id,
           class_data,
           application_state, 
           trigger) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      active <- reactiveVal(FALSE)
      timer <- reactiveVal(0)
      group <- reactiveVal(NULL)
      experiment <- reactiveVal(NULL)
      
      StopWatch <- StopWatch$new()
      
      firstTest <-
        mod_randomisation_server("randomisation", dataset = class_data)
      
      output$current_experiment <- renderText({
        experiment()
      })
      
      
      observe({
        req(isTruthy(firstTest()))
        updateActionButton(inputId = "start", disabled = FALSE)
        experiment("1")
      })
      
      # For a effect of stopwatch timer counting up
      observe({
        invalidateLater(1000, session)
        isolate({
          if (active())
          {
            timer(timer() + 1)
          }
        })
      })
      
      output$timer <-
        renderText(sprintf("%02d:%02d", timer() %/% 60, timer() %% 60))
      
      stroopPlot <- eventReactive(input$start, {
        updateActionButton(inputId = "start", disabled = TRUE)
        updateActionButton(inputId = "stop", disabled = FALSE)
        
        if (all(c(
          !length(StopWatch$ControlDuration),
          !length(StopWatch$StroopDuration)
        ))) {
          group(firstTest())
        }
        
        if (is.null(firstTest())) {
          sendSweetAlert(title = "Action Required",
                         type = "info",
                         text = "Click the randomisation button first")
        } else {
          plot <- StroopPlot(group = group())
          
        }
        
        StopWatch$Start(treatment = group())
        active(TRUE)
        
        return(plot)
        
      })
      
      output$stroop_plot <- renderPlot({
        stroopPlot()
      })
      
      observeEvent(input$stop, {
        updateActionButton(inputId = "start", disabled = FALSE)
        updateActionButton(inputId = "stop", disabled = TRUE)
        StopWatch$Stop(treatment = group())
        currentDuration <-
          as.numeric(StopWatch[[paste0(group(), "Duration")]], format = "seconds")
        
        # Reset State
        timer(0)
        active(FALSE)
        
        showModal(modalDialog(
          tagList(
            value_box(
              title = h2("Time"),
              value = round(StopWatch[[paste0(group(), "Duration")]], 2),
              showcase = bs_icon("stopwatch"),
              theme = "primary",
              p("seconds"),
            ),
            if (experiment() == 1) {
              p("Be sure to carry out the second experiment to see how your results compare.")
            } else {
              p("Be sure to compare your result to the rest of the class.")
            },
            p("Click \"Confirm\" to enter your result")
          ),
          footer = tagList(
            actionButton(ns("cancel"), "Cancel", style = "background-color: red; color: white;"),
            actionButton(ns("confirm"), "Confirm", style = "background-color: green; color: white;")
          )
        ))
      })
      
      observeEvent(input$confirm, {
        if (all(c(
          length(StopWatch$ControlDuration),
          length(StopWatch$StroopDuration)
        ))) {
          output$time2 <- renderText({
            as.character(round(as.numeric(StopWatch$Time2, format = "seconds"),
                               2))
          })
        } else {
          output$time1 <- renderText({
            as.character(round(as.numeric(StopWatch$Time1, format = "seconds"),
                               2))
          })
          experiment("2")
          # Change to second group
          group(choices[!(choices %in% group())])
        }
        
        # Enable the submit button if both the requiset times are available
        if (all(length(StopWatch$StroopDuration) &&
                length(StopWatch$ControlDuration))) {
          updateActionButton(inputId = "submit", disabled = FALSE)
        }
        
        removeModal()
      })
      
      observeEvent(input$cancel, {
        removeModal()
      })
      
      observeEvent(input$submit, {
        StopWatch$SetInitials(value = input$initials)
        StopWatch$SetGroup(value = input$group)
        
        if (is.null(application_state$GoogleSheets)) {
          sendSweetAlert(
            session = session,
            title = "Googlesheets",
            text = "If you wish to use multiple computers, these data need stored on Googlesheets.
          See the user guide for help setting up Googlesheets",
            type = "info"
          )
        } else if (isTRUE((!isTruthy(class_data()) &&
                           nrow(StopWatch$GetData()) < 2))) {
          sendSweetAlert(
            session = session,
            title = "No Data to Upload",
            text = "Something went wrong! These thing happen, try refreshing the page and
            trying again.",
            type = "warning"
          )
        } else {
          timediff <- StopWatch$StroopDuration - StopWatch$ControlDuration
          
          text <- ifelse(timediff < 0, "quicker", "slower")
          
          showModal(modalDialog(
            value_box(
              title = h2("Outcome"),
              value = abs(round(as.numeric(timediff), 2)),
              showcase = bs_icon("speedometer"),
              theme = "primary",
              p(sprintf("seconds %s on the control",
                        text))
            ),
            p(
              "Your results are being uploaded to the rest of the class data."
            ),
            footer = tagList(
              actionButton(inputId = ns("new_student"), label = "New Student"),
              actionButton(
                inputId = ns("view_results"),
                label = "View Results"
              )
            )
          ))
          
          dataset <- class_data()
          
          updatedData <- saveData(
            id = application_state$GoogleSheets,
            data = StopWatch$GetData(),
            with_progress = TRUE, 
            online = FALSE,
            dataset = dataset)
          
          class_data(updatedData)
          
        }
        
        
        # Resetting the State
        updateTextInput(inputId = "initials", value = "")
        updateActionButton(inputId = "start", disabled = TRUE)
        updateActionButton(inputId = "stop", disabled = TRUE)
        updateActionButton(inputId = "submit", disabled = TRUE)
        experiment("1")
        output$time1 <- renderText({
          "-"  # Default message before any updates
        })
        output$time2 <- renderText({
          "-"  # Default message before any updates
        })
        StopWatch$Reset()
      })
      
      observeEvent(input$new_student,{
        removeModal()
      })
      
      observeEvent(input$view_results,{
        trigger(UUIDgenerate())
        removeModal()
      })
    })
  }
