#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib card_footer
mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  
  page_fillable(card(
    card_header(h2("Stroop Test"), class = "bg-primary"),
    card_body(layout_column_wrap(
      width = 1 / 2,
      card(title = "Overview",
           card_body(
             p(
               "The Stroop Test is a psychological experiment that demonstrates the interference in reaction time when different cognitive tasks are in conflict. It involves showing participants a list of words, each representing a color, printed in ink that either matches or contradicts the color name."
             ),
             p(
               "For example, the word ",
               span("RED", style = "color:blue;"),
               "might be printed in blue ink. The task is to name the color of the ink, not the word itself. This test illustrates how our brains process conflicting information and can be slower when the color and word do not match, a phenomenon known as the Stroop effect."
             )
           )),
      card(title = "Instructions",
           card_body(div(
             tags$ol(
               tags$li("Enter your details in the 'Subject Details' tab."),
               tags$li("Click Randomisation to be assigned a random order."),
               tags$li(
                 "Click ",
                 span("Start", style = "font-style: bold;"),
                 "when you've said each word"
               ),
               tags$ol(
                 tags$li(
                   "Stroop Effect (",
                   span("BLUE", style = "color:red;"),
                   ", ",
                   span("RED", style = "color:green;"),
                   ", ",
                   span("GREEN", style = "color:blue;"),
                   ")"
                 ),
                 tags$li(
                   "Control (",
                   span("BLUE", style = "color:blue;"),
                   ", ",
                   span("RED", style = "color:red;"),
                   ", ",
                   span("GREEN", style = "color:green;"),
                   ")"
                 )
               ),
               tags$li("After 2 seconds, 16 words will appear."),
               tags$li("Focus only on the colour of the ink and not the written word."),
               tags$li("Say the colour of the ink for each word"),
               tags$li(
                 "Click",
                 span("Stop", style = "font-style:bold;"),
                 "when you've said each word"
               )
             )
           )))
    ))
  ))
}

#' landing_page Server Functions
#'
#' @noRd
mod_landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # No server logic at present
    
    
  })
}
