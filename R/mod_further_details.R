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
    div(
      card(card_header(
        h2("How to become a Statistician or Statistical Programmer"),
        class = "bg-primary"
      ),
      card_body(
        p(
          "There is not just one route to becoming a medical statistician or statistical programmer in the pharmaceutical industry. There are many valuable skills and experiences
                you can bring to a company. However, all statisticians and programmers need to firstly be good mathematicians, so it is important to choose A levels carefully.
                Both medical statisticians and programmers will need to go to university to get their first degree (bachelor’s degree [BSc]). Medical statisticians usually also need a second
                degree (master’s degree [Msc or MMath], or sometimes even a doctorate [PhD]) in statistics or medical statistics."
        )
      )),
      card(
        card_header(h2("Application Team"),
                    class = "bg-primary"),
        card_body(
          p(
            "Explore the profiles below to see examples of how the team that developed this application have used their mathematical, statistical and scientific skills and how this may relate your career."
          ),
          layout_column_wrap(
            userCard(
              name = "Gareth Burns",
              job_title = "Data Scientist",
              company = "Exploristics",
              image = "img/gareth.jpg"
            ),
            userCard(
              name = "Steve Mallett",
              job_title = "Senior Manager",
              company = "Veramed",
              image = "img/steve.jpg"
            ),
            userCard(
              name = "Alex Spiers",
              job_title = "Principal Statistician",
              company = "GSK",
              image = "img/alex.jpg"
            ),
            userCard(
              name = "Katie Gwinnell",
              job_title = "Placement Student",
              company = "Bayer",
              image = "img/katie.jpg"
            ),
            userCard(
              name = "Emma Crawford",
              job_title = "Statistician",
              company = "PSI",
              image = "img/emma.jpg"
            ),
            userCard(
              name = "My Luong Vuong",
              job_title = "Statistician",
              company = "Kuleuven",
              image = "img/myluong.jpg"
            )
          )
        )
      )
    ),
    card(
      card_image(file = NULL,
                 src = "img/profiles.jpg",
                 href = "https://psiweb.org/")
    )
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
