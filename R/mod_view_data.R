#' view_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bslib page_fillable card layout_sidebar
#' @importFrom shinyWidgets awesomeCheckbox prettyRadioButtons

mod_view_data_ui <- function(id) {
  ns <- NS(id)
  page_fillable(card(layout_sidebar(
    sidebar = sidebar(
      position = "left",
      open = "always",
      width = 350,
      radioGroupButtons(
        inputId = ns("plot_type"),
        label = with_red_star("Plot"),
        choices = c("Histogram" = "Histogram", "Boxplot" = "BoxPlot"),
        # "CDF Plot" = "CDFPlot"
        # ,
        # "CI Plot" = "CIPlot"),
        selected = "Histogram",
        direction = "horizontal"
      ),
      strong("Test Your Understanding"),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.plot_type == 'Histogram'",
        ns = ns,
        prettyRadioButtons(
          inputId = ns("histogram_question"),
          label = "Which time bin contained the most students?",
          choices = c("1", "2", "3"),
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "tada"
        ),
        actionButton(inputId = ns("hist_submit"), label = "Submit"),
        textOutput(ns("histogram_feedback"))
      ),
      # Only show this panel if Custom is selected
      conditionalPanel(
        condition = "input.plot_type == 'BoxPlot'",
        ns = ns,
        prettyRadioButtons(
          inputId = ns("boxplot_question"),
          label = "What is the median value?",
          choices = c("1", "2", "3"),
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "tada"
        ),
        actionButton(inputId = ns("boxplot_submit"), label = "Submit"),
        textOutput(ns("boxplot_feedback"))
      )


      # ,
      # wellPanel(
      #   h2("Customise Plot"),
      #   awesomeCheckbox(
      #     inputId = ns("labels"),
      #     label = "Show Labels",
      #     value = FALSE,
      #     status = "primary"
      #   ),
      #   awesomeCheckbox(
      #     inputId = ns("density"),
      #     label = "Display Density Curve",
      #     value = FALSE,
      #     status = "primary"
      #   )
      # )
    ),
    card_body(plotOutput(ns("plot")))
  ), full_screen = TRUE))
}

#' view_data Server Functions
#'
#' @noRd
mod_view_data_server <- function(id, class_data, user_choices) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    boxplotQuestion <-
      reactiveValues(
        question = "A box and whisker plot displays the marks of students in a maths exam.
        The plot shows that the minimum mark is 30, the lower quartile is 45, the median
        is 60, the upper quartile is 75, and the maximum mark is 90",
        ChoiceA = "25%",
        ChoiceB = "50%",
        ChoiceC = "75%",
        ChoiceD = "100%",
        Answer = "Choice B"
      )

    histogramQuestion <-
      reactiveValues(
        question = "A histogram displays the number of hours students spent studying
        for their exams. The bars are tallest in the range of 5-10 hours, with
        fewer students studying for more or fewer hours.",
        ChoiceA = "Most students studied for 5-10 hours.",
        ChoiceB = "The data is evenly spread across all study hours.",
        ChoiceC = "No student studied for more than 10 hours.",
        ChoiceD = "The histogram shows a normal distribution.",
        Answer = "Choice A"
      )

    output$plot <- renderPlot({
      # Validation to ensure multiple unique data points for plots
      # Otherwise display message.

      validate(
        need(
          nrow(class_data()) > 4 && length(unique(class_data()$Test)) > 1,
          "To visualise these data, there must be multiple unique datapoints!"
        )
      )


      switch(
        input$plot_type,
        "Histogram" = {
          plot <- ggplot(data = class_data(), aes(
            x = Value,
            y = after_stat(count),
            fill = Test
          )) +
            geom_histogram(
              color = MyPallette$black,
              alpha = 0.7,
              binwidth = 1L,
              center = 0.5
            ) +
            labs(x = "Time taken (seconds)",
                 y = "Count",
                 title = "Histogram Showing the Time Taken (Seconds) to Complete the Stroop Experiment and Control") +
            scale_x_continuous(breaks = seq(floor(min(
              class_data()$Value
            )), ceiling(max(
              class_data()$Value
            )), by = 1)) +
            scale_fill_manual(
              "Test",
              labels = c("Control", "Stroop"),
              values = c(colours$control, colours$stroop)
            ) +
            scale_color_manual(
              "Experiment",
              labels = c("Control", "Stroop"),
              values = c(colours$control, colours$stroop)
            ) +
             facet_grid(rows = vars(Test)) +
            theme(
              panel.background = element_rect(fill = MyPallette$grey),
              panel.grid = element_line(),
              axis.line.x = element_line(
                colour = MyPallette$black,
                linewidth = 0.25,
                linetype = 1
              ),
              axis.text = element_text(colour = MyPallette$black, size = 20),
              axis.title = element_text(colour = MyPallette$black, size = 20),
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
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16),
              legend.key.size = unit(1.5, "cm") ,
              plot.title = element_text(size = 16)
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
            ggplot(data = class_data(), aes(x = Value, fill = factor(Test))) +
            geom_boxplot(width = 1,
                         color = MyPallette$black,
                         alpha = 0.7) +
            labs(x = "Time Taken (Seconds)", title = "Box and Whisker Plot Showing the Time Taken (Seconds) to Complete the Stroop Experiment and Control") +
            scale_fill_manual(
              "Experiment",
              labels = c("Control", "Stroop"),
              values = c(colours$control, colours$stroop)
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
              axis.text.x = element_text(colour = MyPallette$black, size = 20),
              axis.title.x = element_text(colour = MyPallette$black, size = 20),
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
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 16),
              legend.key.size = unit(2.5, "cm"),
              plot.title = element_text(size = 16)
            ) +
            scale_x_continuous(
              breaks = seq(floor(min(
                class_data()$Value
              )), ceiling(max(
                class_data()$Value
              )), by = 2),
              label = seq(floor(min(
                class_data()$Value
              )), ceiling(max(
                class_data()$Value
              )), by = 2)
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
            labs(x = user_choices$ValueLabel, y = "Height (cm)") +
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
              axis.text = element_text(colour = MyPallette$black, size = 20),
              axis.title = element_text(colour = MyPallette$black, size = 20),
              legend.text = element_text(colour = MyPallette$black, size = 14),
              legend.title = element_text(colour = MyPallette$black, size = 14)
            )

          if (isTRUE(input$labels)) {
            plot <-
              plot + geom_text(aes(labels = Initials),
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
            labs(x = "Effect of Group (Drug - Control)", y = "Group") +
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

    # Update the questions
    observeEvent(class_data(), {
      req(nrow(class_data()) > 4)
      histChoices <- HistogramQuestion(values = class_data()$Value)

      boxChoices <-
        BoxPlotQuestion(values = class_data()$Value[class_data()$Test == "Stroop"], group = "Stroop")

      boxplotQuestion$Question <- boxChoices[["Question"]]
      boxplotQuestion$ChoiceA <- boxChoices[["ChoiceA"]]
      boxplotQuestion$ChoiceB <- boxChoices[["ChoiceB"]]
      boxplotQuestion$ChoiceC <- boxChoices[["ChoiceC"]]
      boxplotQuestion$ChoiceD <- boxChoices[["ChoiceD"]]
      boxplotQuestion$Answer <- boxChoices[["Answer"]]

      histogramQuestion$Question <- histChoices[["Question"]]
      histogramQuestion$ChoiceA <- histChoices[["ChoiceA"]]
      histogramQuestion$ChoiceB <- histChoices[["ChoiceB"]]
      histogramQuestion$ChoiceC <- histChoices[["ChoiceC"]]
      histogramQuestion$ChoiceD <- histChoices[["ChoiceD"]]
      histogramQuestion$Answer <- histChoices[["Answer"]]


      updateRadioButtons(
        inputId = "histogram_question",
        choices =           c(
          histogramQuestion$ChoiceA,
          histogramQuestion$ChoiceB,
          histogramQuestion$ChoiceC,
          histogramQuestion$ChoiceD
        )
      )

      updateRadioButtons(
        inputId = "boxplot_question",
        label = boxplotQuestion$Question,
        choices =  c(
          boxplotQuestion$ChoiceA,
          boxplotQuestion$ChoiceB,
          boxplotQuestion$ChoiceC,
          boxplotQuestion$ChoiceD
        )
      )
    })

    # Observe event when submit button is clicked
    observeEvent(input$hist_submit, {
      if (input$histogram_question == histogramQuestion$Answer) {
        output$histogram_feedback <-
          renderText("Correct!")
      } else {
        output$histogram_feedback <-
          renderText("Not quite, Please try again.")
      }
    })

    observeEvent(input$boxplot_submit, {
      if (input$boxplot_question == as.character(round(boxplotQuestion$Answer, 2))) {
        output$boxplot_feedback <-
          renderText("Correct!")
      } else {
        output$boxplot_feedback <-
          renderText("Not quite, Please try again.")
      }
    })


  })
}
