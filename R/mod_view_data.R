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
        inputId = ns("plot_type"),
        label = with_red_star("Plot"),
        choices = c(
          "Histogram" = "Histogram",
          "Boxplot" = "BoxPlot",
          "CDF Plot" = "CDFPlot",
          "Scatter Plot" = "ScatterPlot"
          # ,
          # "CI Plot" = "CIPlot"
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
mod_view_data_server <- function(id, class_data, user_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    output$plot <- renderPlot({
      validate(
        need(
          nrow(class_data()) > 4 && length(unique(class_data()$Group)) > 1,
          "To visualise these data, there must be multiple unique datapoints!"
        )
      )
      
      
      switch(
        input$plot_type,
        "Histogram" = {
          plot <- ggplot(data = class_data(),
                         aes(
                           x = Value,
                           y = after_stat(density),
                           fill = Group
                         )) +
            geom_histogram(color = MyPallette$black,
                           alpha = 0.7) +
            labs(x = user_choices$ValueLabel,
                 y = "Frequency Density") +
            scale_fill_manual(
              "Group",
              labels = c("Group A", "Group B"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) +
            scale_color_manual(
              "Group",
              labels = c("Group A", "Group B"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) +
            facet_grid(rows = vars(Group)) +
            theme(
              panel.background = element_rect(fill = MyPallette$grey),
              panel.grid = element_line(),
              axis.line.x = element_line(
                colour = MyPallette$black,
                linewidth = 0.25,
                linetype = 1
              ),
              axis.text.x = element_text(colour = MyPallette$black,
                                         size = 20),
              axis.title.x = element_text(colour = MyPallette$black,
                                          size = 20),
              axis.line.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              strip.text = element_text(
                angle = 0,
                size = 16,
                color = MyPallette$black
              ),
              strip.background = element_rect(
                color = MyPallette$black,
                fill = "white",
                size = 1,
                linetype = "solid"
              ),
              panel.spacing.y = unit(2, "lines"),
              legend.position = "none"
            )
          
          if (isTRUE(input$labels)) {
            mean <- mean(class_data()$Value)
            median <- median(class_data()$Value)
            
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
                                  aes(color = factor(Group)),
                                  size =
                                    1)
          }
          
          print(plot)
          
        },
        "BoxPlot" = {
          plot <-
            ggplot(data = class_data(), aes(x = Value, fill = factor(Group))) +
            geom_boxplot(width = 1,
                         color = MyPallette$black,
                         alpha = 0.7) +
            labs(x = user_choices$ValueLabel) +
            scale_fill_manual(
              "Group",
              labels = c("Group A", "Group B"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) +
            facet_grid(rows = vars(Group)) + 
            theme(
              panel.background = element_rect(fill = MyPallette$grey),
              panel.grid = element_line(),
              axis.line.x = element_line(
                colour = MyPallette$black,
                linewidth = 0.25,
                linetype = 1
              ),
              axis.text.x = element_text(colour = MyPallette$black,
                                         size = 20),
              axis.title.x = element_text(colour = MyPallette$black,
                                          size = 20),
              axis.line.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              strip.text = element_text(
                angle = 0,
                size = 16,
                color = MyPallette$black
              ),
              strip.background = element_rect(
                color = MyPallette$black,
                fill = "white",
                size = 1,
                linetype = "solid"
              ),
              panel.spacing.y = unit(2, "lines"),
              legend.position = "none"
            ) 
          print(plot)
        },
        "CDFPlot" = {
          plot <-
            ggplot(data = class_data(), aes(x = Value, fill = factor(Group))) +
            stat_ecdf(aes(color = Group), size = 1.5) +
            geom_segment(
              y = 0.5,
              yend = 0.5,
              x = min(class_data()$Value * 0.9),
              xend = max(class_data()$Value * 1.1),
              color = MyPallette$black,
              linetype = "dashed",
              size = 0.50
            ) +
            # geom_segment(x=median(data_p$pef), xend=median(data_p$pef), y=-0.5, yend=1, color=col_pla, linetype="dashed", size=0.75) +
            # geom_segment(x=median(data_d$pef), xend=median(data_d$pef), y=-0.5, yend=1, color=col_drug, linetype="dashed", size=0.75) +
            scale_color_manual("Group",
                               values = c(MyPallette$col_pla, MyPallette$col_drug)) +
            labs(x = user_choices$ValueLabel)
          scale_y_continuous("Proportion",
                             limits = c(0, 1),
                             breaks = seq(0, 1, by = 0.1))
          print(plot)
        },
        "ScatterPlot" = {
          plot <-
            ggplot(data = class_data(), aes(x = Value, Height, fill = Group)) +
            geom_point(
              shape = 21,
              color = MyPallette$black,
              size = 10,
              alpha = 0.7
            ) +
            labs(x = user_choices$ValueLabel,
                 y = "Height (cm)") +
            scale_fill_manual(
              "Group",
              labels = c("Group A", "Group B"),
              values = c(MyPallette$col_pla, MyPallette$col_drug)
            ) + 
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
          summary_stats <- class_data() %>%
            group_by(Group, Group) %>%
            summarise(
              mean_value = mean(Value),
              sd_value = sd(Value),
              n = n()
            ) %>%
            pivot_wider(names_from = Group,
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
            labs(x = "Effect of Group (Drug - Control)",
                 y = "Group") +
            scale_color_manual(values = c("0" = MyPallette$red, "1" = MyPallette$green)) +
            scale_fill_manual(values = c("0" = MyPallette$red, "1" = MyPallette$green)) +
            annotate(
              "text",
              x = -1,
              y = 0.8,
              color = MyPallette$blue,
              label = "Placebo Better",
              hjust = 1,
              size = 7
            ) +
            annotate(
              "text",
              x = 1,
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
