#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom googlesheets4 gs4_auth gs4_deauth
#' @importFrom googledrive drive_deauth drive_auth
#' @noRd
app_server <- function(input, output, session) {
  
  library(ggplot2)
  
  theme_set(
    theme(
      panel.background = element_rect(fill = MyPallette$grey),
      panel.grid = element_line(),
      axis.line = element_line(
        colour = MyPallette$black,
        linewidth = 0.25,
        linetype = 1
      ),
      axis.text = element_text(colour = MyPallette$black,
                               size = 20),
      axis.title = element_text(colour = MyPallette$black,
                                size = 20),
      legend.text = element_text(colour = MyPallette$black,
                                 size = 14),
      legend.title = element_text(colour = MyPallette$black,
                                  size = 14)
    )
  )
  
  
  
  gs4_deauth()
  drive_deauth()
  drive_auth(cache = ".secrets", email = "psischoolsinitiative@gmail.com")
  gs4_auth(cache = ".secrets", email = "psischoolsinitiative@gmail.com")
  
  applicationState <-
    reactiveValues(GoogleSheets = "1KEpGStSMctMBiATDwQlqdP5uu5qUFf_Sumjrk-As5HU")
  
  userChoices <-
    reactiveValues(
      Group = c("Group A", "Group B", "Group C"),
      Treatment = c("Control", "Placebo")
    )
  
  classData <-
    reactivePoll(
      20000,
      session,
      checkFunc = function(id = applicationState$GoogleSheets) {
        if (is.null(id)) {
          return(invisible(NULL))
        } else {
          availableSheets <- drive_find(type = "spreadsheet")
          
          if (id %notin% availableSheets$id) {
            cat(sprintf("GoogleSheets id:%s does not exist or you do not have permission to access it", id))
                return(invisible(NULL))
          }
          
          return(availableSheets[availableSheets$id == id,][["drive_resource"]][[1L]][["modifiedTime"]])
        }
      },
      valueFunc = function(id = applicationState$GoogleSheets) {
        if (is.null(id)) {
          return(data.frame(
            ID = character(0L),
            Person = character(0L),
            Group = character(0L),
            Treatment = character(0L),
            Height = numeric(0L),
            Sex = factor(levels = c("Male", "Female")),
            Value = numeric(0L)
          ))
        }

        # Read the data
        read_sheet(ss = id)
      }
  )
  
  data <-
    reactiveValues(
      GroupData = data.frame(
        ID = character(0L),
        Person = character(0L),
        Group = character(0L),
        Treatment = character(0L),
        Height = numeric(0L),
        Sex = factor(levels = c("Male", "Female")),
        Value = numeric(0L)
      ))
  
  mod_data_entry_server(
    "data_entry",
    data = data,
    class_data = classData,
    application_state = applicationState,
    user_choices = userChoices
  )
  mod_view_data_server("view_data", 
                       data = data,
                       class_data = classData)
  mod_admin_server("admin",
                   application_state = applicationState,
                   user_choices = userChoices)
  
}
