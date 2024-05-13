#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landing_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Introduction"),
    fluidRow(
      column(6,
             h3("Make a New Asthma Medicine"),
             p("Have you ever wondered how statistics could be useful in the real world?"),
             p("In this app we're going to imagine you're a statistician working for a pharmaceutical company developing a new asthma medicine. 
               Before a medicine is made available to the public, the company needs to conduct a clinical trial to test whether the drug works (and investigate any side effects).
               The results will be submitted to a health authority, who may grant the medicine a marketing licence if the results are postive."),
             p("You might think that statistics is all about facts and figures, but statisticians in the pharmaceutical industry are involved in making
               critical decisions, as we shall see."),
             p("Are you ready to be a pharmaceutical statistician? Click on 'Lets Go' to continue..."),
      ),
      column(3,
             img(src = "img/asthma.jpg", contentType = "image/jpg",
                 height = "auto",
                 width = "auto",
                 alt = "Patient recieving treatment for asthma"))
    )
  )
}
    
#' landing_page Server Functions
#'
#' @noRd 
mod_landing_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # No server logic at present
    
 
  })
}
