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
      radioGroupButtons(
        inputId = ns("sex"),
        label = "Sex",
        choices = c("Male", "Female"),
        status = "primary"
      ),
      mod_randomisation_ui(ns("randomisation")),
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
        textOutput(ns("timer"))
      ),
      card(
        card_header(
          class = "bg-primary d-flex justify-content-between align-items-center",
          h5("Your times"),
          actionBttn(inputId = ns("submit"), label = "Submit")
        ),
        card_body(
          layout_column_wrap(
            width = 1 / 2,
            h3("Control Time"),
            textOutput(ns("ControlTime")),
            h3("Stroop Time"),
            textOutput(ns("StroopTime"))
          )
        )
      )
    ),
    card(
      card_header(h2("Stroop Test"), class = "bg-primary"),
      card_body(plotOutput(ns("stroop_plot"), width = "auto"))
    )
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
      StopWatch <- StopWatch$new()
      active <- reactiveVal(FALSE)
      timer <- reactiveVal(0)
      group <- reactiveVal(NULL)
      
      firstTest <- mod_randomisation_server("randomisation")
      
      # For a effect of stopwatch
      observe({
        invalidateLater(1000, session)
        isolate({
          if (active())
          {
            timer(timer() + 1)
          }
        })
      })
      
      output$timer <- renderText(timer())
      
      stroopPlot <- eventReactive(input$start, {
        Sys.sleep(1.5)
        
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
        
        if (any(c(
          is.null(duration$StroopStatus),
          is.null(duration$ControlStatus)
        ))) {
          group(firstTest())
          
        } else {
          choices <- c("Control", "Stroop")
          
          group(choices[!(firstTest() %in% choices)])
          df = df1
        }
        
        StopWatch$Start(treatment = group())
        active(TRUE)
        
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
        StopWatch$Stop(Treatment = group())
        
        currentDuration <-
          StopWatch[paste0(group, "Duration")]
        
        # Reset State
        timer(0)
        active(FALSE)
        
        showModal(modalDialog(
          title = "Congratulations",
          tagList(
            h2(Timer[paste0(group, "Duration")]),
            p(sprintf("seconds. For the %s group", group())),
            p(
              "Click \"Submit\" to enter your time and compare how you did against the rest of your class"
            )
          ),
          footer = tagList(
            actionButton(ns("cancel"), "Cancel", style = "background-color: red; color: white;"),
            actionButton(ns("confirm"), "Confirm", style = "background-color: green; color: white;")
          )
        ))
      })
      
      observeEvent(input$confirm, {
        removeModal()
      })
      
      observeEvent(input$cancel, {
        # Reset time for that group
        StopWatch$Reset(treatment = group)
        removeModal()
      })
      
      
      observeEvent(input$submit, {
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
          saveData(
            id = application_state$GoogleSheets,
            data = data.frame(
              ID = UUIDgenerate(),
              Initials = input$initials,
              Group = group(),
              Height = "",
              Sex = input$sex,
              Value = as.numeric(duration(), units = "secs")
            )
          )
          sendSweetAlert(
            session = session,
            title = "Data Successfully Uploaded!",
            text = "Your time was successfully uploaded. Be sure to check how your compare to the rest of your class",
            type = "success"
          )
        }
      })
    })
  }
