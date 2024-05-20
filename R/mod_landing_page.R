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
  
  page_fillable(layout_column_wrap(
    width = NULL,
    style = css(grid_template_columns = "2fr 1fr"),
    heights_equal = "row",
    card(card_header(h2(
      "Make a New Asthma Medicine"
    ), class = "bg-primary"),
    card_body(
      p(
        "Have you ever wondered how statistics could be useful in the real world?"
      ),
      p(
        "In this app we're going to imagine you're a statistician working for a pharmaceutical company developing a new asthma medicine.
               Before a medicine is made available to the public, the company needs to conduct a clinical trial to test whether the drug works (and investigate any side effects).
               The results will be submitted to a health authority, who may grant the medicine a marketing licence if the results are postive."
      ),
      p(
        "You might think that statistics is all about facts and figures, but statisticians in the pharmaceutical industry are involved in making
               critical decisions, as we shall see."
      ),
      p(
        "Are you ready to be a pharmaceutical statistician? Click on 'Lets Go' to continue..."
      ),
    )),
    
    card(
      card_image(file = NULL,
                 src = "img/asthma.jpg",
                 href = "https://psiweb.org/"),
      card_footer(div(
        style = "text-align: center;", # Center-align the logos
        fluidRow(column(
          4,
          tags$a(
            href = "https://psiweb.org/",
            target =
              "_blank",
            HTML(
              '<i class="fas fa-info-circle fa-3x" style="color: #00638e;"></i>'
            )
          )
        ),
        column(
          4,
          tags$a(
            href = "https://twitter.com/PSIUpdate",
            target =
              "_blank",
            HTML(
              '<i class="fab fa-twitter fa-3x" style="color: #00638e;"></i>'
            )
          )
        ),
        column(
          4,
          tags$a(
            href = "https://www.linkedin.com/company/psi-statisticians-in-the-pharmaceutical-industry/",
            target =
              "_blank",
            HTML(
              '<i class="fab fa-linkedin fa-3x" style="color: #00638e;"></i>'
            )
          )
        ))
      ))
    )
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
