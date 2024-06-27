#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(title = "Asthma Workshop", 
                theme = psi_theme,
                bg = "#00638e",
                nav_panel(icon("home"), mod_landing_page_ui("home")), 
                nav_panel("Data Entry", mod_data_entry_ui("data_entry")), 
                nav_panel("View Data", mod_view_data_ui("view_data")), 
                nav_spacer(), 
                nav_panel("Find Out More", mod_further_details_ui("further_details")),
                nav_panel(icon("gear"), mod_admin_ui("admin")))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("img",
                    system.file("app/img", package = "AsthmaWorkshop"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AsthmaWorkshop"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
