# Add the custom JS to control the animation
jsCode <- "
Shiny.addCustomMessageHandler('spinWheel', function(message) {
  var roulette = document.getElementById('roulette');
  roulette.classList.add('spin');

  // Remove the spin class after 2 seconds (same as animation duration)

});
"

# setTimeout(function() {
#   roulette.classList.remove('spin');
# }, 2000);


#' randomisation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_randomisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
    .modal-dialog {
      margin-top: 50px !important;  /* Adjust the margin-top to move the modal up */
    }
  ")),
    tags$head(tags$script(HTML(jsCode))),
    actionBttn(
      inputId = ns("randomisation"),
      label = "Randomisation",
      size = "s",
      style = "simple",
      color = "primary",
      icon = icon(name = "circle-info")
    )
  )
}

#' randomisation Server Functions
#' @importFrom waffle waffle
#' @noRd

mod_randomisation_server <- function(id, dataset = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    first <- reactiveVal(value = NULL,
                         label = "First Test in order")
    
    observeEvent(input$randomisation, {
      showModal(
        modalDialog(
          title = tags$div(icon("shuffle"),
                           "Randomisation"),
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          tagList(
            tags$div(
              style = "display: flex; justify-content: center; align-items: center; flex-direction: row;",
              tags$div(
                style = "display: flex; justify-content: center; align-items: center; flex-direction: column;",
                # Arrow pointing to the wheel
                tags$div(id = "arrow", style = "width: 0; height: 0; border-left: 20px solid transparent; border-right: 20px solid transparent; border-top: 30px solid black; margin-bottom: 10px;"),
                tags$div(
                  id = "roulette",
                  tags$div(id = "left-half", "A"),
                  tags$div(id = "right-half", "B"),
                  style = "width: 200px; height: 200px; border-radius: 50%; border: 2px solid black; position: relative;"
                ),
                # Add the keyframes animation for spinning
                tags$style(
                  "
          @keyframes spin {
            from {transform: rotate(0deg);}
            to {transform: rotate(1480deg);}
          }
          #roulette.spin {
            animation: spin 3.5s ease-in-out;
          }
          #left-half {
            width: 50%;
            height: 100%;
            background-color: #1f77b4;
            position: absolute;
            top: 0;
            left: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            border-top-left-radius: 100px;
            border-bottom-left-radius: 100px;
            font-size: 20px;
            color: white;
            font-weight: bold;
          }
          #right-half {
            width: 50%;
            height: 100%;
            background-color: #ff7f0e;
            position: absolute;
            top: 0;
            right: 0;
            display: flex;
            justify-content: center;
            align-items: center;
            border-top-right-radius: 100px;
            border-bottom-right-radius: 100px;
            font-size: 20px;
            color: white;
            font-weight: bold;
          }
          "
                )
              ),
              div(
                style = "
      background-color: #007bff;
      color: white;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 2px 2px 12px rgba(0, 0, 0, 0.2);
      text-align: center;
      max-width: 500px;
      margin: 20px auto;
    ",
                # Add the "i" icon using Font Awesome
                tags$i(class = "fa fa-info-circle", style = "font-size: 24px;"),
                p(
                  "Randomisation is the process of randomly assigning patients to a
              treatment or in this case the order which tests are carried out."
                ),
                p(
                  "The outcome is also 'blinded' this means you're not told which test
              comes first. Can you think of the reasons why to blind an experiment?"
                )
              )
            ),
            br(),
            h3("Your Outcome"),
            textOutput(ns("order")),
            div(style = "text-align: center;",
                plotOutput(
                  ns("randomisationOrder"),
                  height = "150px",
                  width = "700px"
                ))
          )
        )
      )
      
      
      # Trigger JavaScript to start the spinning animation
      session$sendCustomMessage(type = "spinWheel", message = list())
      
      # Run the spinning animation and determine the result
      # invalidateLater(2000, session)  # Wait 2 seconds for the spin to finish
      
      isolate({
        # Randomly choose between A and B
        options <- c(A = "Stroop", B =  "Non-Stroop")
        
        first(sample(options, 1L))
        
        output$order <- renderText({
          sprintf(
            "First you will be treatment %s then treatment %s",
            names(first()),
            names(options[options != first()])
          )
        })
        
        output$randomisationOrder <- renderPlot({
          # invalidateLater(4000, session)
          # Data for the waffle chart
          data <- data.frame(values = 1:20,
                             order = sample(options, 20L, replace = TRUE))
          
          randomisationOrder(data = data)
          
        })
      })
    })
    
    return(first)
  })
}
