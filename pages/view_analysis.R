# Page: User Input -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
viewAnalysisPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Project Analysis:")),
          p(""),
          br(),
          actionButton(
            NS(id, "generate_analysis"),
            "Start Analysis",
            class = "btn btn-primary",
            style = "margin-bottom: 20px;"
          ),
        )
      ),
      shinyjs::hidden(
        tags$div(
          id = NS(id, "analysis_container"),

          # NON-SINGLETON RESULTS ----------------------------------------------
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                     h5(strong("Predicted Value Delivery")),
                     p("The value delivery is represented as an interval, as it 
                       incorporates the uncertainty present within the system 
                       and its input."),
                     br(),
                     DT::DTOutput(
                       NS(id, "ns_value_delivery_results")
                     )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                     plotOutput(NS(id, "iawa_plot"))
              ),
              column(1)
            ),
            br(),
            fluidRow(
              column(1),
              column(10,
                     p("The interval is calculated using an approach called the 'Interval Agreement 
                       Weighted Average'; more information about this 
                       approach will be added here, as publications are 
                       released."),
                    
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # SENSITIVITY ANALYSIS -----------------------------------------------
          
          fluidPage(
            column(1),
            column(10,
                   h5(strong("Sensitivity Analysis")),
                   p("A sensitivity analysis is used to investigate how the output 
                     may change if you adjust one of the input parameters."),
                   p("Select one of the input properties from the drop-down 
                     menu to see how it affects output value delivery."),
                   selectInput(
                     NS(id, "sensitivity_analysis_select"),
                     "Property",
                     choices = c()
                   ),
                   fluidRow(
                     column(12,
                            prettyCheckboxGroup(
                              NS(id, "sensitivity_analysis_value_choices"),
                              "Values",
                              choices = c(),
                              inline = TRUE,
                              width = "100%"
                            )
                     )
                   ),
                   br(),
                   fluidRow(
                     column(12,
                            plotOutput(
                              NS(id, "sensitivity_analysis_results")
                            )
                     )
                   )
            ),
            column(1)
          ),
          br()
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------
viewAnalysisPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Hide analysis container whenever inputs change; force reanalysis
    observeEvent(project_data$EP_amounts, {
      shinyjs::hide("analysis_container")
    })

    # Value Delivery -----------------------------------------------------------
    observeEvent(input$generate_analysis, {

      fis_input_names <- sapply(project_data$fis$input, "[[", 1)
      fis_output_names <- sapply(project_data$fis$output, "[[", 1)

      # Order EP Amounts data to match with FIS Inputs.
      input_stack <- c()
      for (EP in fis_input_names) {
        input_stack <- c(
          input_stack,
          as.numeric(project_data$EP_amounts[[EP, 'Central']])
        )
      }
      
      # NON-SINGLETON APPROACH -------------------------------------------------

      # Value Delivery (for given property amounts)
      ns_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))
      
      ns_value_delivery_table["Value"] <- fis_output_names
      # ns_value_delivery_table["Midpoint"] <- NULL
      ns_value_delivery_table["IAWA"] <- NULL
      
      nsfis <- convertfis(
        project_data$fis,
        option = 's2n',
        fuzzification.method = "interval",
        fuzzification.params = c(0,1)
      )
      
      # Correct fuzzy parameters
      for (i in 1:length(fis_input_names)) {
        params <- as.vector(as.numeric(
          project_data$EP_amounts[i, c("Min", "Max")]
        ))
        nsfis$input[[i]]$fuzzification.params <- params
      }
      
      # Generates rangex and OUT_RULE_AGG behind scenes
      round(
        t(evalfis(input_stack, nsfis, point_n = POINT_N)),
        2
      )
      
      # Handle Output Fuzzy Sets
      ns_output_sets <- data.frame(matrix(
        ncol = 3,
        nrow = length(fis_output_names) * POINT_N
      ))
      colnames(ns_output_sets) <- c("Value", "X", "Y")
      
      for (i in 1:length(fis_output_names)) {
        start_index <- ((i - 1) * POINT_N + 1)
        end_index <- (i * POINT_N)
        ns_output_sets[start_index:end_index, "Value"] <- fis_output_names[i]
        ns_output_sets[start_index:end_index, "X"] <- rangex[, i]
        ns_output_sets[start_index:end_index, "Y"] <- OUT_RULE_AGG[
          1, start_index:end_index
        ]
      }

      # Calculate Uncertainty
      iawa_plot_data <- data.frame(matrix(
        ncol = 3,
        nrow = 0
      ))
      colnames(iawa_plot_data) <- c("Value", "l", "r")
      
      for (i in 1:length(fis_output_names)) {
        
        output_set <- ns_output_sets[
          which(ns_output_sets["Value"] == fis_output_names[i]),
        ]
        
        # IAWA Uncertainty
        iawa_output <- iawa(output_set)
        
        # Submit data to table
        # ns_value_delivery_table[i, "Midpoint"] <- 
        #   min(iawa_output) + (max(iawa_output) / 2)
        ns_value_delivery_table[i, "IAWA"] <- toString(iawa_output)
        
        # Format data for plotting
        iawa_list <- as.list(iawa_output)
        start_index <- nrow(iawa_plot_data) + 1
        end_index <- start_index + length(iawa_output) - 1
        iawa_plot_data[start_index:end_index, "Value"] <- fis_output_names[i]
        iawa_plot_data[start_index:end_index, "l"] <- 
          sapply(iawa_list, function(interval) { 
            return (unlist(interval)[1])  
          })
        iawa_plot_data[start_index:end_index, "r"] <- 
          sapply(iawa_list, function(interval) { 
            return (unlist(interval)[2])  
          })
        # iawa_plot_data[start_index:end_index, "m"] <-
        #   iawa_plot_data[start_index:end_index, "l"] +
        #   (iawa_plot_data[start_index:end_index, "r"] / 2)
        
      }
      
      # Display results --------------------------------------------------------
      
      # Table
      output$ns_value_delivery_results <- DT::renderDT({
        return(
          setNames(
            ns_value_delivery_table,
            c("Value Name", "Interval Delivery")
          )
        )
      },
      options = list(
        scrollY = 100,
        dom="t",
        paging = FALSE,
        columnDefs = list(list(
          className = 'dt-left',
          width = '200px',
          targets = "_all"
        ))
      ),
      selection = "none",
      rownames = FALSE
      )
      
      
      
      # IAWA Interval (Uncertainty)
      output$iawa_plot <- renderPlot({
        ggplot(
          data = iawa_plot_data,
          aes(
            colour = Value, 
            fill = Value, 
            y = factor(
              Value, 
              levels = rev(levels(factor(Value))))
            )
        ) +
        geom_segment(
          aes(
            x = l,
            xend = r,
            yend = Value
          ),
          alpha = 0.7,
          linewidth = 100/length(fis_output_names)
        ) +
        # geom_point(
        #   aes(
        #     x = m
        #   ),
        #   alpha = 0.7,
        #   size = 30 / length(fis_output_names),
        #   shape = 18
        # ) +
        xlab("Value Delivery") +
        scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 1), n.breaks = 11) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, margin = margin(b = 12), size = 16),
          panel.border = element_blank(),
          legend.key.size = unit(0.6, 'cm'),
          legend.key.height = unit(0.6, 'cm'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.background = element_rect(color = "lightgrey", linetype = "solid"),
          legend.justification = "top",
          legend.position = "none",
          axis.title.x = element_text(
            size = 16, color="black", margin = margin(t = 12)
          ),
          axis.text.x = element_text(
            size = 13, color="black", margin = margin(t = 2)
          ),
          axis.title.y = element_blank(),
          axis.text.y = element_text(
            size = 16, color="black", margin = margin(r = 2)
          ),
          strip.text = element_text(
            size = 20, color="black", margin = margin(r = 8, l = 8)),
          strip.background = element_rect(fill = "aliceblue"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          aspect.ratio = 1/2
        )  
        
      })
      
      # Display results to user ------------------------------------------------

      output$value_delivery_results <- DT::renderDT({
        return(value_delivery_table)
      },
        options = list(
          scrollY = 200,
          dom="t",
          paging = FALSE,
          columnDefs = list(list(className = 'dt-left', targets = "_all"))
        ),
        selection = "none",
        rownames = FALSE
      )

      shinyjs::show("analysis_container")

    })

    # Sensitivity Analysis ----------------------------------------------------

    # Keep Select updated for Sensitivity Analysis
    observeEvent(project_data$EP_names, {
      if (!isTruthy(project_data$EP_names)) {
        EP_choice <- ""
        EP_selected <- ""
      } else {
        EP_choice <- list()
        for (EP in project_data$EP_names) {
          EP_choice[formatEPString(EP)] = EP
        }
        EP_selected <- project_data$EP_names[1]
      }
      updateSelectInput(
        session,
        "sensitivity_analysis_select",
        choices = EP_choice,
        selected = EP_selected
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$sensitivity_analysis_select, {
      if (isTruthy(input$sensitivity_analysis_select)) {

        # Find Values relevant to this EP
        connected_values <- project_data$val_conn_names[
          which(
            lapply(
              project_data$val_conn_names, "[[", 1
            ) == input$sensitivity_analysis_select
          )
        ]
        connected_values <- sapply(connected_values, "[[", 2)

        # Update Values Choices
        updatePrettyCheckboxGroup(
          session,
          "sensitivity_analysis_value_choices",
          choices = connected_values,
          selected = connected_values,
          inline = TRUE
        )

        output$sensitivity_analysis_results <- renderPlot({
          constructSensitivityAnalysisPlot(
            input$sensitivity_analysis_select,
            input$sensitivity_analysis_value_choices,
            project_data
          )

        })
      }
    }, ignoreNULL = FALSE)


  })
}
