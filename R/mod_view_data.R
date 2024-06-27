#' view_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_fluid card layout_sidebar
#' @importFrom shinyWidgets awesomeCheckbox

mod_view_data_ui <- function(id) {
  ns <- NS(id)
  page_fluid(card(layout_sidebar(
    sidebar = sidebar(
      title = "Customise",
      position = "left",
      open = TRUE,
      radioGroupButtons(
        inputId = ns("data_source"),
        label = "What do you want displayed?",
        choices = c("Group Data",
                    "Class Data"),
        status = "primary"
      ),
      radioGroupButtons(
        inputId = ns("plot_type"),
        label = with_red_star("Plot"),
        choices = c(
          "Histogram" = "Histogram",
          "Boxplot" = "BoxPlot",
          "CDF Plot" = "CDFPlot",
          "Scatter Plot" = "ScatterPlot",
          "CI Plot" = "CIPlot"
        ),
        selected = "Histogram",
        direction = "vertical"
      ),
      wellPanel(
        h2("Customise Plot"),
        awesomeCheckbox(
          inputId = ns("labels"),
          label = "Show Labels",
          value = FALSE,
          status = "primary"
        ),
        awesomeCheckbox(
          inputId = ns("density"),
          label = "Display Density Curve",
          value = FALSE,
          status = "primary"
        )
      )
    ),
    card_body(plotOutput(ns("plot")))
  ),
  full_screen = TRUE))
}

#' view_data Server Functions
#'
#' @noRd
mod_view_data_server <- function(id, class_data, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    PlotData <- reactive({
      if (input$data_source == "Class Data") {
        data <- class_data()
      } else {
        data <- data$GroupData
      }
      return(data)
    })
    
    
    output$plot <- renderPlot({
      validate(
        need(
          nrow(PlotData()) > 4 && length(unique(PlotData()$Treatment)) > 1,
          "To visualise these data, there must be multiple unique datapoints!"
        )
      )
      
      
      switch(
        input$plot_type,
        "Histogram" = {
          plot <- ggplot(data = PlotData(),
                         aes(
                           x = Value,
                           y = after_stat(density),
                           fill = Treatment
                         )) +
            geom_histogram(color = MyPallette$black,
                           alpha = 0.7) +
            labs(x = "Peak Expiratory Flow (L/min)",
                 y = "Frequency Density") +
            scale_fill_manual(
              "Treatment",
              labels = c("Placebo", "Drug"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) +
            scale_color_manual(
              "Treatment",
              labels = c("Placebo", "Drug"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) +
            facet_grid(rows = vars(Treatment))
          
          if (isTRUE(input$labels)) {
            mean <- mean(PlotData()$Value)
            median <- median(PlotData()$Value)
            
            plot <- plot +
              geom_vline(
                xintercept = mean,
                color = MyPallette$red,
                linetype = "dashed",
                size = 1
              ) +
              geom_vline(
                xintercept = median,
                colour = MyPallette$green,
                linetype = "dotted",
                size = 1.5
              )
          }
          
          if (isTRUE(input$density)) {
            plot <-
              plot + stat_density(geom = "line",
                                  aes(color = factor(Treatment)),
                                  size =
                                    1)
          }
          
          print(plot)
          
        },
        "BoxPlot" = {
          plot <-
            ggplot(data = PlotData(), aes(x = Value, fill = factor(Treatment))) +
            geom_boxplot(width = 1,
                         color = MyPallette$black,
                         alpha = 0.7) +
            labs(x = "Peak Expiratory Flow (L/min)") +
            scale_fill_manual(
              "Treatment",
              labels = c("Placebo", "Drug"),
              values = c(col_pla, col_drug)
            ) +
            facet_grid(rows = vars(Treatment))
          print(plot)
        },
        "CDFPlot" = {
          plot <-
            ggplot(data = PlotData(), aes(x = Value, fill = factor(Treatment))) +
            stat_ecdf(aes(color = Treatment), size = 1.5) +
            geom_segment(
              y = 0.5,
              yend = 0.5,
              x = min(PlotData()$Value * 0.9),
              xend = max(PlotData()$Value * 1.1),
              color = MyPallette$black,
              linetype = "dashed",
              size = 0.50
            ) +
            # geom_segment(x=median(data_p$pef), xend=median(data_p$pef), y=-0.5, yend=1, color=col_pla, linetype="dashed", size=0.75) +
            # geom_segment(x=median(data_d$pef), xend=median(data_d$pef), y=-0.5, yend=1, color=col_drug, linetype="dashed", size=0.75) +
            scale_color_manual("Treatment",
                               values = c(col_pla, col_drug)) +
            labs(x = "Peak Expiratory Flow (L/min)")
          scale_y_continuous("Proportion",
                             limits = c(0, 1),
                             breaks = seq(0, 1, by = 0.1))
          print(plot)
        },
        "ScatterPlot" = {
          plot <-
            ggplot(data = PlotData(), aes(x = Value, Height, fill = Treatment)) +
            geom_point(
              shape = 21,
              color = MyPallette$black,
              size = 10,
              alpha = 0.7
            ) +
            labs(x = "Peak Expiratory Flow",
                 y = "Height (cm)") +
            scale_fill_manual(
              "Treatment",
              labels = c("Placebo", "Drug"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            )
          
          if (isTRUE(input$labels)) {
            plot <-
              plot + geom_text(aes(labels = Name),
                               size = 6,
                               nudge_y = 0.1)
          }
          
          print(plot)
        },
        "CIPlot" = {
          # Data Wrangling
          #' @import dplyr
          #' @import tidyr
          summary_stats <- PlotData() %>%
            group_by(Group, Treatment) %>%
            summarise(
              mean_value = mean(Value),
              sd_value = sd(Value),
              n = n()
            ) %>%
            pivot_wider(names_from = Treatment,
                        values_from = c(mean_value, sd_value, n)) %>%
            mutate(
              diff_mean = sd_value_Condition - mean_value_Placebo,
              se_diff = sqrt((sd_value_Condition ^ 2 / n_Condition) + (sd_value_Placebo ^ 2 / sd_value_Condition)
              ),
              ci_lower = diff_mean - qt(0.975, df = n_Condition + sd_value_Condition - 2) * se_diff,
              ci_upper = diff_mean + qt(0.975, df = n_Condition + sd_value_Condition - 2) * se_diff,
              c = if_else(ci_lower > 0 |
                            ci_upper < 0, "1", "0")
            )
          
          
          # Plot
          plot <-  ggplot(data = summary_stats) +
            geom_segment(
              show.legend = FALSE,
              colour = "darkred",
              linewidth = 6,
              aes(
                x = ci_lower,
                xend = ci_upper,
                y = Group,
                yend = Group,
              )
            ) +
            geom_vline(aes(xintercept = 0),
                       color = MyPallette$black,
                       size = 1) +
            labs(x = "Effect of Treatment (Drug - Control)",
                 y = "Group") +
            # scale_color_manual(values = c("0" = MyPallette$red, "1" = MyPallette$green)) +
            # scale_fill_manual(values = c("0" = MyPallette$red, "1" = MyPallette$green)) +
            annotate(
              "text",
              x = -40,
              y = 0.8,
              color = MyPallette$blue,
              label = "Placebo Better",
              hjust = 0,
              size = 7
            ) +
            annotate(
              "text",
              x = 5,
              y = 0.8,
              color = MyPallette$blue,
              label = "Drug Better",
              hjust = 0,
              size = 7
            )
          
          print(plot)
          
          
        }
      )
    })
    
    
  })
}
