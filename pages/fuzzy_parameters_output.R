# Page 4: Values Membership Functions ------------------------------------------

valueMFsPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      wellPanel(
        tags$div(
          style = WELL_STYLE,

          # Page Description ---------------------------------------------------
          h4(strong("Output Membership Functions:")),
          p("This page allows you to view the output variable parameters of the 
            underlying fuzzy system."),
          p("The option to finetune output parameters will be added in a future 
            update."),
          br(), hr(),

          # Select Value --------------------------------------------
          fluidRow(
            column(1),
            column(5,
              selectInput(
                NS(id, "value_select"),
                "Select a Value",
                choices = ""
              )
            ),
            column(1)
          )
        )
      ),
      fluidRow(align = "center",
        column(12,
          fluidPage(style = "min-height: 400px; padding: 0px; margin: 0px;",
            plotOutput(NS(id, "valueMF"), width = "100%")
          )
        )
      )
    )
  )
}

valueMFsPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Update FIS Output Membership Functions -----------------------------------
    observeEvent(project_data$values, {
      # Extracts output names from FIS
      fis_output_names <- sapply(project_data$fis$output, "[[", 1)
      # Calculate differences between FIS and project data
      missing_output <- setdiff(project_data$values, fis_output_names)
      extra_output <-  setdiff(fis_output_names, project_data$values)

      # Add missing elements to FIS output, if any. [Default Parameters]
      if (length (missing_output) > 0) {
        for (i in 1:length(missing_output)) {
          project_data$fis <- addvar(
            project_data$fis,
            'output',
            missing_output[i],
            c(0, 1)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'output',
            length(project_data$fis$output),
            'Low',
            'gaussmf',
            c(0.2, 0)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'output',
            length(project_data$fis$output),
            'Medium',
            'gaussmf',
            c(0.2, 0.5)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'output',
            length(project_data$fis$output),
            'High',
            'gaussmf',
            c(0.2, 1)
          )
        }
      }

      # Remove extraneous elements from FIS Input
      if (length (extra_output) > 0) {
        for (i in 1:length(extra_output)) {
          extra_output_index <- which(fis_output_names == extra_output[i])
          project_data$fis$output <- project_data$fis$output[
            -(extra_output_index),
            drop = FALSE
          ]
        }
      }

    }, ignoreNULL = FALSE)

    # Select Membership Function -----------------------------------------------
    observeEvent(project_data$values, {
      if (!isTruthy(project_data$elements)) {
        value_choice <- ""
        value_selected <- ""
      } else {
        value_choice <- project_data$values
        value_selected <- project_data$values[1]
      }
      updateSelectInput(
        session,
        "value_select",
        choices = value_choice,
        selected = value_selected
      )
    }, ignoreNULL = FALSE)

    # Plot Selected MF ---------------------------------------------------------
    output$valueMF <- renderPlot({
      if (isTruthy(input$value_select)) {
        constructMFPlot(input$value_select, project_data$fis, "output")
      }
    }, height = 400)

  })
}
