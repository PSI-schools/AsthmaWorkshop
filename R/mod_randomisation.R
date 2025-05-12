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
      margin-top: 25px !important;  /* Adjust the margin-top to move the modal up */
    }
  ")),
    tags$head(tags$script(HTML(jsCode))),
    actionButton(
      inputId = ns("randomisation"),
      label = "Randomisation",
      icon = icon(name = "shuffle"),
      style = "background-color: green; color: white;"
    )
  )
}

#' randomisation Server Functions
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
                  style = "width: 180px; height: 180px; border-radius: 50%; border: 2px solid black; position: relative;"
                ),
                # Add the keyframes animation for spinning
                tags$style(
                  "
          @keyframes spin {
            from {transform: rotate(0deg);}
            to {transform: rotate(1480deg);}
          }
          #roulette.spin {
            animation: spin 3.5s ease-in;
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
            border-top-left-radius: 90px;
            border-bottom-left-radius: 90px;
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
            border-top-right-radius: 90px;
            border-bottom-right-radius: 90px;
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
      margin: 10px auto;
    ",
                # Add the "i" icon using Font Awesome
                tags$i(class = "fa fa-info-circle", style = "font-size: 18px;"),
                p(
                  "Randomisation is the process of randomly assigning patients to a
              treatment or control. In this example the randomisation assignes the order 
                  in which you will carry out the control then treatment."
                ),
                p(
                  "The outcome is 'blinded' which means you're not told which test
              comes first." 
                ), 
                strong("Can you think of the reasons why to blind an experiment?")
              )
            ),
            h3("Class Randomisation"),
            # textOutput(ns("order")),
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
        first(sample(choices, 1L))
        
        output$order <- renderText({
          sprintf(
            "First you will be treatment %s then treatment %s",
            names(first()),
            names(choices[choices != first()])
          )
        })
        
        output$randomisationOrder <- renderPlot({
          validate(
            need(nrow(dataset()) >2, "To display previous class data please add more data")
          )
          randomisationOrder(data = dataset())
          
        })
      })
    })
    
    return(first)
  })
}
