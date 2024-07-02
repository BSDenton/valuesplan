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
          p("Placeholder explanation text..."),
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
                h5(strong("Non-Singleton: Value Delivery")),
                DT::DTOutput(
                  NS(id, "ns_value_delivery_results")
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                plotOutput(NS(id, "ns_plot"))
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # MIN-MAX RESULTS ----------------------------------------------------
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                h5(strong("Min-Max Approach: Value Delivery")),
                DT::DTOutput(
                  NS(id, "mm_value_delivery_results")
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                     plotOutput(NS(id, "mm_plot"))
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # UNIFORM SAMPLING RESULTS -------------------------------------------
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                     h5(strong("Uniform Sampling Approach: Value Delivery")),
                     DT::DTOutput(
                       NS(id, "us_value_delivery_results")
                     )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                     plotOutput(NS(id, "us_plot"))
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # MONTE CARLO RESULTS ------------------------------------------------
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                h5(strong("Monte Carlo Approach: Value Delivery")),
                DT::DTOutput(
                  NS(id, "mc_value_delivery_results")
                )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                plotOutput(NS(id, "mc_plot"))
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # QUASI-RANDOM SAMPLING RESULTS -------------------------------------------
          fluidPage(
            fluidRow(
              column(1),
              column(10,
                     h5(strong("Halton Sampling Approach: Value Delivery")),
                     DT::DTOutput(
                       NS(id, "h_value_delivery_results")
                     )
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                     plotOutput(NS(id, "h_plot"))
              ),
              column(1)
            )
          ),
          br(), hr(), br(),

          # OVERALL PLOT -------------------------------------------------------

          fluidPage(
            fluidRow(
              column(1),
              column(10,
                h5(strong("All Results")),
              ),
              column(1)
            ),
            fluidRow(
              column(1),
              column(10,
                plotOutput(NS(id, "overall_delivery_plot"), height = 500)
              ),
              column(1)
            ),
            br(), br(), br(),
            fluidRow(
              column(1),
              column(10,
                plotOutput(NS(id, "overall_uncertainty_plot"), height = 500)
              ),
              column(1)
            )
          ),
          br(), hr(), br(),


          # SENSITIVITY ANALYSIS -----------------------------------------------
          br(),
          fluidPage(
            column(1),
            column(10,
              h5(strong("Sensitivity Analysis")),
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

      # Value Delivery (for given property amounts)
      s_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      s_value_delivery_table["Value"] <- fis_output_names
      s_value_delivery_table["Delivery"] <- round(
        t(evalfis(input_stack, project_data$fis, point_n = POINT_N)),
        2
      )

      # NON-SINGLETON APPROACH -------------------------------------------------

      # Value Delivery (for given property amounts)
      ns_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      ns_value_delivery_table["Value"] <- fis_output_names

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

      ns_value_delivery_table["Delivery"] <- round(
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
      for (i in 1:length(fis_output_names)) {
        output_set <- ns_output_sets[
          which(ns_output_sets["Value"] == fis_output_names[i]),
        ]

        # IAWA Uncertainty
        iawa_output <- iawa(output_set)
        ns_value_delivery_table[i, "IAWA"] <- toString(iawa_output)
        ns_value_delivery_table[i, "Spread"] <-
          round(max(iawa_output) - min(iawa_output), 2)

        # Alpha-Cut Uncertainty
        alphacut_data <- data.frame(matrix(
          ncol = 0,
          nrow = nrow(output_set)
        ))
        alphacut_data["X"] <- output_set["X"]
        alphacut_data["Y"] <- output_set["Y"] > 0.5

        alphacut_intervals <- data.frame(matrix(
          ncol = 2,
          nrow = 0
        ))
        colnames(alphacut_intervals) <- c("Lower", "Upper")

        curr_interval <- c(NULL, NULL)
        for (j in 1:nrow(alphacut_data)) {
          if (is.null(curr_interval[1])) {
            if (alphacut_data[j, "Y"]) {
              curr_interval[1] <- alphacut_data[j, "X"]
            }
          } else {
            if (j == nrow(alphacut_data) || !alphacut_data[j+1, "Y"]) {
              curr_interval[2] <- alphacut_data[j, "X"]
              alphacut_intervals[nrow(alphacut_intervals)+1, ] <- curr_interval
              curr_interval <- c(NULL, NULL)
            }
          }
        }

        alphacut_intervals_sets <- with(
          alphacut_intervals,
          mapply("interval", l = Lower, r = Upper, SIMPLIFY = FALSE)
        )
        alphacut_result <- interval_union(alphacut_intervals_sets)
        ns_value_delivery_table[i, "Alphacut"] <- toString(alphacut_result)

      }


      # Display results
      output$ns_value_delivery_results <- DT::renderDT({
        return(ns_value_delivery_table)
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

      output$ns_plot <- renderPlot({
        ggplot(
          data = ns_output_sets,
          aes(x = X, y = Y, colour = Value)
        ) +
        geom_line(linewidth = 1.2) +
        ylab("Membership Values (μ)") +
        xlab("Delivery") +
        theme_classic() +
        theme(
          legend.key.size = unit(0.4, 'cm'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(
            size = 16, color="black", margin = margin(t = 6)
          ),
          axis.text.x = element_text(
            size = 14, color="black", margin = margin(t = 2)
          ),
          axis.title.y = element_text(
            size = 16, color="black", margin = margin(r = 12)
          ),
          axis.text.y = element_text(
            size = 14, color="black", margin = margin(r = 2)
          )
        ) +
        guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # MIN-MAX APPROACH -------------------------------------------------------

      # Value Delivery (for given property amounts)
      mm_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      mm_value_delivery_table["Value"] <- fis_output_names

      # Generate table of all Min - Max input permutations
      combn_table <- combn(
        rep(c(1, 2), length(fis_input_names)),
        length(fis_input_names)
      )
      combn_table <- as.data.frame(t(combn_table))
      combn_table <- data.matrix(distinct(combn_table))

      # Convert into input stacks
      for (i in 1:nrow(combn_table)) {
        row <- combn_table[i,]
        for (j in 1:length(row)) {
          if (row[j] == 1) {
            row[j] <- as.numeric(project_data$EP_amounts[[j, 'Min']])
          } else {
            row[j] <- as.numeric(project_data$EP_amounts[[j, 'Max']])
          }
        }
        combn_table[i, ] <- row
      }

      min_max_output <-  data.frame(matrix(
        ncol = 2,
        nrow = 0
      ))
      colnames(min_max_output) <- c("Value", "Delivery")

      for (i in 1:nrow(combn_table)) {
        start_row <- (((i - 1) * length(fis_output_names))) + 1
        end_row <- i * length(fis_output_names)

        curr_input_stack <- combn_table[i, ]
        min_max_output[start_row:end_row, "Value"] <- fis_output_names

        min_max_output[start_row:end_row, "Delivery"] <- round(
          t(evalfis(curr_input_stack, project_data$fis, point_n = POINT_N)),
          2
        )
      }

      mm_value_delivery_table[["Min Delivery"]] <- NA
      mm_value_delivery_table[["Max Delivery"]] <- NA
      mm_value_delivery_table[["Uncertainty"]] <- NA
      for (i in 1:length(fis_output_names)) {
        name <- fis_output_names[i]
        rows <- min_max_output[which(min_max_output$Value == name), ]
        min_delivery <- min(rows[, "Delivery"])
        max_delivery <- max(rows[, "Delivery"])
        min_max_idu <- round(abs(max_delivery - min_delivery), 2)
        mm_value_delivery_table[[i, "Min Delivery"]] <- min_delivery
        mm_value_delivery_table[[i, "Max Delivery"]] <- max_delivery
        mm_value_delivery_table[[i, "Uncertainty"]] <- min_max_idu
      }

      output$mm_value_delivery_results <- DT::renderDT({
        return(mm_value_delivery_table)
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

      output$mm_plot <- renderPlot({
        ggplot(
          data = min_max_output
        ) +
          geom_vline(aes(xintercept = Delivery, colour = Value)) +
          xlim(0, 1) +
          xlab("Delivery") +
          labs(colour = "Value") +
          theme_classic() +
          theme(
            legend.key.size = unit(0.4, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(
              size = 16, color="black", margin = margin(t = 6)
            ),
            axis.text.x = element_text(
              size = 14, color="black", margin = margin(t = 2)
            ),
            axis.title.y = element_text(
              size = 16, color="black", margin = margin(r = 12)
            ),
            axis.text.y = element_text(
              size = 14, color="black", margin = margin(r = 2)
            )
          ) +
          guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # UNIFORM SAMPLING APPROACH ----------------------------------------------

      # Value Delivery (for given property amounts)
      us_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      us_value_delivery_table["Value"] <- fis_output_names

      # Generate all input stacks / input permutations.
      us_input <- data.frame(matrix(
        ncol = US_SAMPLES,
        nrow = length(fis_input_names)
      ))

      rownames(us_input) <- fis_input_names
      for (i in 1:length(fis_input_names)) {
        name <- fis_input_names[i]
        min <- as.numeric(project_data$EP_amounts[[name, "Min"]])
        max <- as.numeric(project_data$EP_amounts[[name, "Max"]])
        us_input[i, ] <- seq(min, max, length.out = US_SAMPLES)
      }

      us_combn_table <-
        combn(
          rep(c(1:US_SAMPLES), length(fis_input_names)),
          length(fis_input_names)
        )
      us_combn_table <- as.data.frame(t(us_combn_table))
      us_combn_table <- data.matrix(distinct(us_combn_table))
      colnames(us_combn_table) <- fis_input_names

      for (i in 1:nrow(us_combn_table)) {
        for (j in 1:ncol(us_combn_table)) {  # Values
          sample_index <- us_combn_table[i, j]
          us_combn_table[[i, j]] <- us_input[j, sample_index]
        }
      }

      us_output <- list()
      us_output <- apply(us_combn_table, 1, function(row) {
        evalfis(row, project_data$fis, point_n = POINT_N)
      })

      us_output <- data.frame(
        matrix(us_output, ncol= length(fis_output_names), byrow=TRUE)
      )
      colnames(us_output) <- fis_output_names

      # Analysis results, grouped by output
      us_value_delivery_table[["Mean"]] <- NA
      us_value_delivery_table[["Median"]] <- NA
      us_value_delivery_table[["Min"]] <- NA
      us_value_delivery_table[["Max"]] <- NA
      us_value_delivery_table[["Midrange"]] <- NA
      us_value_delivery_table[["Skewness"]] <- NA
      us_value_delivery_table[["Kurtosis"]] <- NA
      us_value_delivery_table[["Range"]] <- NA
      us_value_delivery_table[["IQR"]] <- NA
      us_value_delivery_table[["Standard Deviation"]] <- NA

      for (i in 1:length(fis_output_names)) {
        column <- us_output[, i]
        output_values <- as.vector(column)
        us_value_delivery_table[i, "Mean"] <- round(
          mean(output_values), 2
        )
        us_value_delivery_table[i, "Median"] <- round(
          median(output_values), 2
        )
        us_value_delivery_table[i, "Min"] <- round(
          min(output_values), 2
        )
        us_value_delivery_table[i, "Max"] <- round(
          max(output_values), 2
        )
        us_value_delivery_table[i, "Midrange"] <- round(
          ((max(output_values) + min(output_values)) / 2), 2
        )
        us_value_delivery_table[i, "Skewness"] <- round(
          skewness(output_values), 2
        )
        us_value_delivery_table[i, "Kurtosis"] <- round(
          kurtosis(output_values), 2
        )
        us_value_delivery_table[i, "Range"] <- round(
          (max(output_values) - min(output_values)), 2
        )
        us_value_delivery_table[i, "IQR"] <- round(
          IQR(output_values), 2
        )
        us_value_delivery_table[i, "Standard Deviation"] <- round(
          sd(output_values), 2
        )
      }

      # Melt data frame for plotting
      melted_us_output <- reshape2::melt(us_output, id.vars=NULL)

      # Display results to user
      output$us_value_delivery_results <- DT::renderDT({
        return(us_value_delivery_table)
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

      output$us_plot <- renderPlot({
        ggplot(
          data = melted_us_output
        ) +
          geom_vline(aes(xintercept = value, colour = variable)) +
          xlim(0, 1) +
          xlab("Delivery") +
          labs(colour = "Value") +
          theme_classic() +
          theme(
            legend.key.size = unit(0.4, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(
              size = 16, color="black", margin = margin(t = 6)
            ),
            axis.text.x = element_text(
              size = 14, color="black", margin = margin(t = 2)
            ),
            axis.title.y = element_text(
              size = 16, color="black", margin = margin(r = 12)
            ),
            axis.text.y = element_text(
              size = 14, color="black", margin = margin(r = 2)
            )
          ) +
          guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # MONTE CARLO APPROACH ---------------------------------------------------

      # Value Delivery (for given property amounts)
      mc_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      mc_value_delivery_table["Value"] <- fis_output_names

      # Generate all input stacks / input permutations.
      monte_carlo_input <- data.frame(matrix(
        ncol = MC_SAMPLES,
        nrow = length(fis_input_names)
      ))

      rownames(monte_carlo_input) <- fis_input_names
      for (i in 1:length(fis_input_names)) {
        name <- fis_input_names[i]
        min <- as.numeric(project_data$EP_amounts[[name, "Min"]])
        max <- as.numeric(project_data$EP_amounts[[name, "Max"]])
        monte_carlo_input[i, ] <- runif(MC_SAMPLES, min, max)
      }

      mc_combn_table <-
        combn(
          rep(c(1:MC_SAMPLES), length(fis_input_names)),
          length(fis_input_names)
        )
      mc_combn_table <- as.data.frame(t(mc_combn_table))
      mc_combn_table <- data.matrix(distinct(mc_combn_table))
      colnames(mc_combn_table) <- fis_input_names

      for (i in 1:nrow(mc_combn_table)) {
        for (j in 1:ncol(mc_combn_table)) {  # Values
          sample_index <- mc_combn_table[i, j]
          mc_combn_table[[i, j]] <- monte_carlo_input[j, sample_index]
        }
      }

      monte_carlo_output <- list()

      monte_carlo_output <- apply(mc_combn_table, 1, function(row) {
        evalfis(row, project_data$fis, point_n = POINT_N)
      })

      monte_carlo_output <- data.frame(
        matrix(monte_carlo_output, ncol= length(fis_output_names), byrow=TRUE)
      )
      colnames(monte_carlo_output) <- fis_output_names

      # Analysis results, grouped by output
      mc_value_delivery_table[["Mean"]] <- NA
      mc_value_delivery_table[["Median"]] <- NA
      mc_value_delivery_table[["Min"]] <- NA
      mc_value_delivery_table[["Max"]] <- NA
      mc_value_delivery_table[["Midrange"]] <- NA
      mc_value_delivery_table[["Skewness"]] <- NA
      mc_value_delivery_table[["Kurtosis"]] <- NA
      mc_value_delivery_table[["Range"]] <- NA
      mc_value_delivery_table[["IQR"]] <- NA
      mc_value_delivery_table[["Standard Deviation"]] <- NA

      for (i in 1:length(fis_output_names)) {
        column <- monte_carlo_output[, i]
        output_values <- as.vector(column)
        mc_value_delivery_table[i, "Mean"] <- round(
          mean(output_values), 2
        )
        mc_value_delivery_table[i, "Median"] <- round(
          median(output_values), 2
        )
        mc_value_delivery_table[i, "Min"] <- round(
          min(output_values), 2
        )
        mc_value_delivery_table[i, "Max"] <- round(
          max(output_values), 2
        )
        mc_value_delivery_table[i, "Midrange"] <- round(
          ((max(output_values) + min(output_values)) / 2), 2
        )
        mc_value_delivery_table[i, "Skewness"] <- round(
          skewness(output_values), 2
        )
        mc_value_delivery_table[i, "Kurtosis"] <- round(
          kurtosis(output_values), 2
        )
        mc_value_delivery_table[i, "Range"] <- round(
          (max(output_values) - min(output_values)), 2
        )
        mc_value_delivery_table[i, "IQR"] <- round(
          IQR(output_values), 2
        )
        mc_value_delivery_table[i, "Standard Deviation"] <- round(
          sd(output_values), 2
        )
      }

      # Melt data frame for plotting
      melted_mc_output <- reshape2::melt(monte_carlo_output, id.vars=NULL)

      # Display results to user
      output$mc_value_delivery_results <- DT::renderDT({
        return(mc_value_delivery_table)
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

      output$mc_plot <- renderPlot({
        ggplot(
          data = melted_mc_output
        ) +
        geom_vline(aes(xintercept = value, colour = variable)) +
        xlim(0, 1) +
        xlab("Delivery") +
        labs(colour = "Value") +
        theme_classic() +
        theme(
          legend.key.size = unit(0.4, 'cm'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          axis.title.x = element_text(
            size = 16, color="black", margin = margin(t = 6)
          ),
          axis.text.x = element_text(
            size = 14, color="black", margin = margin(t = 2)
          ),
          axis.title.y = element_text(
              size = 16, color="black", margin = margin(r = 12)
          ),
          axis.text.y = element_text(
            size = 14, color="black", margin = margin(r = 2)
          )
        ) +
        guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # HALTON SAMPLING APPROACH ----------------------------------------------

      # Value Delivery (for given property amounts)
      h_value_delivery_table <- data.frame(matrix(
        ncol = 0,
        nrow = length(fis_output_names)
      ))

      h_value_delivery_table["Value"] <- fis_output_names

      # Generate all input stacks / input permutations.
      h_input <- data.frame(matrix(
        ncol = H_SAMPLES,
        nrow = length(fis_input_names)
      ))

      rownames(h_input) <- fis_input_names
      for (i in 1:length(fis_input_names)) {
        name <- fis_input_names[i]
        h_min <- as.numeric(project_data$EP_amounts[[name, "Min"]])
        h_max <- as.numeric(project_data$EP_amounts[[name, "Max"]])
        h_range <- abs(h_max - h_min)
        h_input[i, ] <-
          (ghalton(H_SAMPLES, 1, method = "halton") * h_range) + h_min
      }

      h_combn_table <-
        combn(
          rep(c(1:H_SAMPLES), length(fis_input_names)),
          length(fis_input_names)
        )
      h_combn_table <- as.data.frame(t(h_combn_table))
      h_combn_table <- data.matrix(distinct(h_combn_table))
      colnames(h_combn_table) <- fis_input_names

      for (i in 1:nrow(h_combn_table)) {
        for (j in 1:ncol(h_combn_table)) {  # Values
          sample_index <- h_combn_table[i, j]
          h_combn_table[[i, j]] <- h_input[j, sample_index]
        }
      }

      h_output <- list()

      h_output <- apply(h_combn_table, 1, function(row) {
        evalfis(row, project_data$fis, point_n = POINT_N)
      })

      h_output <- data.frame(
        matrix(h_output, ncol= length(fis_output_names), byrow=TRUE)
      )
      colnames(h_output) <- fis_output_names

      # Analysis results, grouped by output
      h_value_delivery_table[["Mean"]] <- NA
      h_value_delivery_table[["Median"]] <- NA
      h_value_delivery_table[["Min"]] <- NA
      h_value_delivery_table[["Max"]] <- NA
      h_value_delivery_table[["Midrange"]] <- NA
      h_value_delivery_table[["Skewness"]] <- NA
      h_value_delivery_table[["Kurtosis"]] <- NA
      h_value_delivery_table[["Range"]] <- NA
      h_value_delivery_table[["IQR"]] <- NA
      h_value_delivery_table[["Standard Deviation"]] <- NA

      for (i in 1:length(fis_output_names)) {
        column <- h_output[, i]
        output_values <- as.vector(column)
        h_value_delivery_table[i, "Mean"] <- round(
          mean(output_values), 2
        )
        h_value_delivery_table[i, "Median"] <- round(
          median(output_values), 2
        )
        h_value_delivery_table[i, "Min"] <- round(
          min(output_values), 2
        )
        h_value_delivery_table[i, "Max"] <- round(
          max(output_values), 2
        )
        h_value_delivery_table[i, "Midrange"] <- round(
          ((max(output_values) + min(output_values)) / 2), 2
        )
        h_value_delivery_table[i, "Skewness"] <- round(
          skewness(output_values), 2
        )
        h_value_delivery_table[i, "Kurtosis"] <- round(
          kurtosis(output_values), 2
        )
        h_value_delivery_table[i, "Range"] <- round(
          (max(output_values) - min(output_values)), 2
        )
        h_value_delivery_table[i, "IQR"] <- round(
          IQR(output_values), 2
        )
        h_value_delivery_table[i, "Standard Deviation"] <- round(
          sd(output_values), 2
        )
      }

      # Melt data frame for plotting
      melted_h_output <- reshape2::melt(h_output, id.vars=NULL)

      # Display results to user
      output$h_value_delivery_results <- DT::renderDT({
        return(h_value_delivery_table)
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

      output$h_plot <- renderPlot({
        ggplot(
          data = melted_h_output
        ) +
          geom_vline(aes(xintercept = value, colour = variable)) +
          xlim(0, 1) +
          xlab("Delivery") +
          labs(colour = "Value") +
          theme_classic() +
          theme(
            legend.key.size = unit(0.4, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(
              size = 16, color="black", margin = margin(t = 6)
            ),
            axis.text.x = element_text(
              size = 14, color="black", margin = margin(t = 2)
            ),
            axis.title.y = element_text(
              size = 16, color="black", margin = margin(r = 12)
            ),
            axis.text.y = element_text(
              size = 14, color="black", margin = margin(r = 2)
            )
          ) +
          guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # OVERALL RESULTS --------------------------------------------------------

      overall_delivery_table <- data.frame(matrix(
        ncol = 3,
        nrow = 0
      ))

      overall_uncertainty_table <- data.frame(matrix(
        ncol = 3,
        nrow = 0
      ))

      del_col_names <- c("Approach", "Value", "Delivery")
      unc_col_names <- c("Approach", "Value", "Uncertainty")
      colnames(overall_delivery_table) <- del_col_names
      colnames(overall_uncertainty_table) <- unc_col_names

      # NS
      data_to_add <- matrix(
        c(
          rep("Non-Singleton Approach", length(fis_output_names)),
          fis_output_names,
          ns_value_delivery_table[, "Delivery"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- del_col_names
      overall_delivery_table <- rbind(overall_delivery_table, data_to_add)

      data_to_add <- matrix(
        c(
          rep("Non-Singleton Approach", length(fis_output_names)),
          fis_output_names,
          ns_value_delivery_table[, "Spread"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- unc_col_names
      overall_uncertainty_table <- rbind(overall_uncertainty_table, data_to_add)

      # MM
      data_to_add <- matrix(
        c(
          rep("Min-Max Approach: Minimum", length(fis_output_names)),
          fis_output_names,
          mm_value_delivery_table[, "Min Delivery"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- del_col_names
      overall_delivery_table <- rbind(overall_delivery_table, data_to_add)

      data_to_add <- matrix(
        c(
          rep("Min-Max Approach: Maximum", length(fis_output_names)),
          fis_output_names,
          mm_value_delivery_table[, "Max Delivery"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- del_col_names
      overall_delivery_table <- rbind(overall_delivery_table, data_to_add)

      data_to_add <- matrix(
        c(
          rep("Min-Max Approach", length(fis_output_names)),
          fis_output_names,
          mm_value_delivery_table[, "Uncertainty"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- unc_col_names
      overall_uncertainty_table <- rbind(overall_uncertainty_table, data_to_add)

      # US
      data_to_add <- matrix(
        c(
          rep("Non-Random Sampling Approach: Median", length(fis_output_names)),
          fis_output_names,
          us_value_delivery_table[, "Median"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- del_col_names
      overall_delivery_table <- rbind(overall_delivery_table, data_to_add)

      data_to_add <- matrix(
        c(
          rep("Non-Random Sampling: IQR", length(fis_output_names)),
          fis_output_names,
          us_value_delivery_table[, "IQR"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- unc_col_names
      overall_uncertainty_table <- rbind(overall_uncertainty_table, data_to_add)

      # MC
      data_to_add <- matrix(
        c(
          rep("Monte Carlo Approach: Median", length(fis_output_names)),
          fis_output_names,
          mc_value_delivery_table[, "Median"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- del_col_names
      overall_delivery_table <- rbind(overall_delivery_table, data_to_add)

      data_to_add <- matrix(
        c(
          rep("Monte Carlo Approach: IQR", length(fis_output_names)),
          fis_output_names,
          mc_value_delivery_table[, "IQR"]
        ) , ncol = 3
      )
      colnames(data_to_add) <- unc_col_names
      overall_uncertainty_table <- rbind(overall_uncertainty_table, data_to_add)

      # Plot

      overall_delivery_table$Delivery <- as.numeric(
        overall_delivery_table$Delivery
      )

      overall_uncertainty_table$Uncertainty <- as.numeric(
        overall_uncertainty_table$Uncertainty
      )

      output$overall_delivery_plot <- renderPlot({
        ggplot(
          data = overall_delivery_table,
          aes(x = Delivery)
        ) +
          geom_vline(
            aes(xintercept = Delivery, colour = Approach),
            linewidth = 1,
            alpha = 0.8
          ) +
          facet_grid(rows = vars(Value)) +
          xlim(0, 1) +
          theme_bw() +
          theme(
            legend.key.size = unit(0.4, 'cm'),
            legend.key.height = unit(1, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(
              size = 16, color="black", margin = margin(t = 6)
            ),
            axis.text.x = element_text(
              size = 14, color="black", margin = margin(t = 2)
            ),
            strip.text = element_text(
              size = 16, color="black", margin = margin(r = 8, l = 8)),
            strip.background = element_rect(fill = "white")
          ) +
          guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      output$overall_uncertainty_plot <- renderPlot({
        ggplot(
          data = overall_uncertainty_table,
          aes(x = Uncertainty)
        ) +
          geom_vline(
            aes(xintercept = Uncertainty, colour = Approach),
            linewidth = 1,
            alpha = 0.8
          ) +
          facet_grid(rows = vars(Value)) +
          xlim(0, 1) +
          theme_bw() +
          theme(
            legend.key.size = unit(0.4, 'cm'),
            legend.key.height = unit(1, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(
              size = 16, color="black", margin = margin(t = 6)
            ),
            axis.text.x = element_text(
              size = 14, color="black", margin = margin(t = 2)
            ),
            strip.text = element_text(
              size = 16, color="black", margin = margin(r = 8, l = 8)),
            strip.background = element_rect(fill = "white")
          ) +
          guides(colour = guide_legend(override.aes = list(linewidth = 5)))
      })

      # ------------------------------------------------------------------------

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


