#' stroop_test UI Function
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

mod_stroop_test_ui <- function(id) {
  ns <- NS(id)
  page_sidebar(sidebar = sidebar(
    width = "30%",
    open = "always",
    wellPanel(
      h2("Subject Details"),
      textInput(ns("initials"), label = "Your Initials"),
      # numericInput(
      #   inputId = ns("height"),
      #   label = "Height (cm)",
      #   value = NULL,
      #   min = 0L,
      #   max = 250
      # ),
      radioGroupButtons(
        inputId = ns("sex"),
        label = "Sex",
        choices = c("Male", "Female"),
        status = "primary"
      ),
      wellPanel(
      h2("Stop Watch"),
      actionButton(
        inputId = ns("start"),
        label = "Start",
        style = "background-color: green; color: white;"
      ),
      actionButton(
        inputId = ns("stop"),
        label = "Stop",
        style = "background-color: red; color: white;"
      ),
      h2("Time"),
      textOutput(ns("duration"))
    ))
  ),
  card(
    card_header(h2("Stroop Test"), class = "bg-primary"),
    card_body(plotOutput(ns("stroop_plot"), width = "auto"))
  ))
}

#' stroop_test Server Functions
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @importFrom uuid UUIDgenerate
#' @importFrom reactable reactable colDef
#' @noRd


mod_stroop_test_server <-
  function(id,
           class_data,
           application_state) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      
      active <- reactiveVal(FALSE)
      startTime <- reactiveVal(value = NULL)
      timer <- reactiveVal(0)
      duration <- reactiveVal(0)
      group <- reactiveVal(NULL)
      
      
      observe({
        invalidateLater(1000, session)
        isolate({
          if (active())
          {
            timer(timer() + 1)
          }
        })
      })
      
      output$duration <- renderText(timer())
      
      stroopPlot <- eventReactive(input$start, {
        Sys.sleep(1.5)
        
        startTime(Sys.time())
        active(TRUE)
        
        # Randomisation
        
        words <- c("RED", "BLUE", "GREEN", "PURPLE", "ORANGE")
        colors <-
          c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
        my_dgrey <- "#f0f0f0"
        
        my_theme <- theme(
          text = element_text(size = 30),
          plot.title = element_text(size = 25),
          panel.background = element_rect(fill = my_dgrey),
          panel.border = element_rect(fill = NA),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
        )
        
        df <- data.frame(
          word = sample(words, 16, replace = TRUE),
          color = sample(colors, 16, replace = TRUE),
          x = rep(1:4, each = 4),
          y = rep(1:4, times = 4)
        )
        
        df1 <- data.frame(
          (i = sample(1:5, 16, replace = TRUE)),
          word = words[i],
          color = colors[i],
          x = rep(1:4, each = 4),
          y = rep(1:4, times = 4)
        )
        
        rand <- runif(1)
        group("A")
        if (rand < 0.5) {
          group("B")
          df = df1
        }
        
        # Plot using ggplot2
        ggplot(df, aes(
          x = factor(x),
          y = y,
          label = word,
          color = color
        )) +
          geom_text(size = 15) +
          scale_x_discrete(limits = factor(1:4)) +
          scale_color_identity() +
          theme_void() +
          theme(legend.position = "none") +
          coord_fixed() +
          my_theme +
          ggtitle(paste("Group:", group()))
      })
      
      output$stroop_plot <- renderPlot({
        stroopPlot()
      })
      
      observeEvent(input$stop, {
        duration(Sys.time() - startTime())
        
        # Reset State
        timer(0)
        active(FALSE)
        
        showModal(modalDialog(
          title = "Congratulations",
          tagList(
            p(sprintf("You had a time of %.2f seconds", duration())),
            p(
              "Click \"Submit\" to enter your time and compare how you did against the rest of your class"
            )
          ),
          footer = tagList(
            actionButton(ns("cancel"), "Cancel", style = "background-color: red; color: white;"),
            actionButton(ns("submit"), "Submit", style = "background-color: green; color: white;")
          )
        ))
      })
      
      observeEvent(input$submit, {
        removeModal()
        
        if (is.null(application_state$GoogleSheets)) {
          sendSweetAlert(
            session = session,
            title = "Googlesheets",
            text = "If you wish to use multiple computers, these data need stored on Googlesheets.
          See the user guide for help setting up Googlesheets",
            type = "info"
          )
        } else if (isTRUE((!isTruthy(class_data()) &&
                           nrow(class_data()) > 0))) {
          sendSweetAlert(
            session = session,
            title = "No Data to Upload",
            text = "Something went wrong! These thing happen, try refreshing the page and
            trying again.",
            type = "warning"
          )
        } else {
          saveData(id = application_state$GoogleSheets,
                   data = data.frame(
                     ID = UUIDgenerate(),
                     Initials = input$initials,
                     Group = group(),
                     Height = "",
                       # input$height,
                     Sex = input$sex,
                     Value = as.numeric(duration(), units = "secs")
                   ))
          sendSweetAlert(
            session = session,
            title = "Data Successfully Uploaded!",
            text = "Your time was successfully uploaded. Be sure to check how your compare to the rest of your class",
            type = "success"
          )
        }
      })
      observeEvent(input$cancel, {
        removeModal()
      })
    })
  }
