#' further_details UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_fillable layout_column_wrap card_image
#' @importFrom htmltools  css

mod_further_details_ui <- function(id) {
  ns <- NS(id)
  page_fillable(layout_column_wrap(
    width = NULL,
    style = css(grid_template_columns = "2fr 1fr"),
    heights_equal = "row",
    card(
      card_header("Tell me more", class = "bg-primary"),
      card_body(
        h3("How to become a Statistician or Statistical Programmer"),
        p(
          "There is not just one route to becoming a medical statistician or statistical programmer in the pharmaceutical industry. There are many valuable skills and experiences
                you can bring to a company. However, all statisticians and programmers need to firstly be good mathematicians, so it is important to choose A levels carefully.
                Both medical statisticians and programmers will need to go to university to get their first degree (bachelor’s degree [BSc]). Medical statisticians usually also need a second
                degree (master’s degree [Msc or MMath], or sometimes even a doctorate [PhD]) in statistics or medical statistics."
        ),
        p(
          "Explore the profiles below to see examples of how you can develop mathematical, statistical and scientific skills that will be invaluable for building your career."
        )
      )
    ),
    card(
    card_image(
      file = NULL,
      src = "img/profiles.jpg",
      href = "https://psiweb.org/"
    ))
  ))
  
}

#' further_details Server Functions
#'
#' @noRd
mod_further_details_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}
