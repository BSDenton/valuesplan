# Page 3: Element-Property Membership Functions --------------------------------

# UI ---------------------------------------------------------------------------

EPMFsPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    tags$head(
      tags$style(HTML(
        "
        .selectize-dropdown-content { background-color: white; }
        .panel-default { border-color: grey }
        "
      ))
    ),
    fluidPage(
      wellPanel(
        tags$div(
          style = WELL_STYLE,

          # Page Description ---------------------------------------------------
          h4(strong("Input Membership Functions:")),
          p("For users who are familiar with fuzzy logic systems, this page 
            allows you to finetune the input variable parameters of the 
            underlying fuzzy system."),
          strong("This page is optional; the system will 
            still perform well if the default parameters are used."),
          br(), hr(),

          # Select Element Property --------------------------------------------
          fluidRow(
            column(1),
            column(5,
                   selectInput(
                     NS(id, "element_select"),
                     "Select an Element",
                     choices = ""
                   )
            ),
            column(5,
                   selectizeInput(
                     NS(id, "property_select"),
                     "Select a Property",
                     choices = ""
                   )
            ),
            column(1)
          )
        )
      ),
      fluidRow(
        column(5,
          wellPanel(
            style = "height: 500px; overflow-y: scroll;",
            shinyjs::hidden(
              tags$div(
                id = NS(id, "EP_params_container"),
                htmlOutput(NS(id, "EP_params_name")),
                h5(strong("Range")),
                fluidRow(
                  column(6,
                    HTML("Minimum"),
                    numericInput(
                      NS(id,"EP_params_min_range"),
                      NULL,
                      0
                    )
                  ),
                  column(6,
                    HTML("Maximum"),
                    numericInput(
                      NS(id,"EP_params_max_range"),
                      NULL,
                      100
                    )
                  )
                ),
                br(),
                h5(strong("Linguistic Terms")),
                uiOutput(NS(id, "edit_params_container")),
                # fluidRow(
                #   column(12,
                #     h5(strong("Add New Linguistic Term"))
                #   ),
                #   column(7,
                #     textInput(
                #       NS(id, "new_mf_name_input"),
                #       NULL
                #     )
                #   ),
                #   column(5,
                #     actionButton(
                #       NS(id, "add_new_mf_button"),
                #       "Add",
                #       class = "btn btn-primary btn-sm",
                #       style = "width: 100%"
                #     )
                #   )
                # ),
                # fluidRow(
                #   column(12,
                #     h5(strong("Remove Linguistic Term"))
                #   ),
                #   column(7,
                #     selectInput(
                #       NS(id, "select_mf_to_delete"),
                #       NULL,
                #       choices = c()
                #     )
                #   ),
                #   column(5,
                #     actionButton(
                #       NS(id, "delete_mf_button"),
                #       "Delete",
                #       class = "btn btn-danger btn-sm",
                #       style = "width: 100%"
                #     )
                #   )
                # )
              )
            )
          )
        ),
        column(7,
          fluidPage(style = "min-height: 500px; padding: 0px; margin: 0px;
                             margin-top: 50px",
            plotOutput(NS(id, "EPMF_plot"), width = "100%")
          )
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------
EPMFsPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    EPMF_ui_list <- reactiveValues(
      mf_container_ids = c()
    )

    # Update FIS Input Membership Functions ------------------------------------
    observeEvent(project_data$EP_names, {
      # Extracts input names from FIS
      fis_input_names <- sapply(project_data$fis$input, "[[", 1)
      # Calculate differences between FIS and project data
      missing_input <- setdiff(project_data$EP_names, fis_input_names)
      extra_input <-  setdiff(fis_input_names, project_data$EP_names)

      # Add missing elements to FIS Input, if any. [Default Parameters]
      if (length (missing_input) > 0) {
        for (i in 1:length(missing_input)) {
          project_data$fis <- addvar(
            project_data$fis,
            'input',
            missing_input[i],
            c(0, 1)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'input',
            length(project_data$fis$input),
            'Low',
            'gaussmf',
            c(0.2, 0)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'input',
            length(project_data$fis$input),
            'Medium',
            'gaussmf',
            c(0.2, 0.5)
          )
          project_data$fis <- addmf(
            project_data$fis,
            'input',
            length(project_data$fis$input),
            'High',
            'gaussmf',
            c(0.2, 1)
          )
        }
      }

      # Remove extraneous elements from FIS Input
      if (length (extra_input) > 0) {
        for (i in 1:length(extra_input)) {
          extra_input_index <- which(fis_input_names == extra_input[i])
          project_data$fis$input <- project_data$fis$input[
            -(extra_input_index),
            drop = FALSE
          ]
        }
      }

    }, ignoreNULL = FALSE)

    # Select Membership Function -----------------------------------------------

    # Select Element Property
    observeEvent(project_data$EP_conn_matrix, {
      if (!is.null(project_data$EP_conn_matrix)) {
        elements_with_properties <- rownames(
          project_data$EP_conn_matrix[
            rowSums(project_data$EP_conn_matrix) > 0,
            ,
            drop = FALSE
          ]
        )
        updateSelectInput(
          session,
          "element_select",
          choices = elements_with_properties,
          selected = elements_with_properties[1]
        )
      }
    }, ignoreNULL = FALSE)

    # Update Property select options
    observeEvent(c(input$element_select, project_data$EP_conn_matrix), {
      # Find previously selected properties, and match to column names.
      matches <- which(project_data$EP_conn_matrix[input$element_select,] > 0)
      matches <- colnames(project_data$EP_conn_matrix)[matches]
      if(length(matches) == 0) {
        matches = c("")
      }
      updateSelectInput(
        session,
        "property_select",
        choices = matches
      )
    }, ignoreNULL = FALSE)

    # Change Parameters and Membership Functions -------------------------------
    # Set-up Container and UI elements
    observeEvent(c(input$element_select, input$property_select), {
      if (isTruthy(input$element_select) && isTruthy(input$property_select)) {

        element <- input$element_select
        property <- input$property_select
        EP <- formatEPName(element, property)

        # Check that FIS contains this input
        fis_input_names <- sapply(project_data$fis$input, "[[", 1)
        if (EP %in% fis_input_names) {
          curr_params <- project_data$fis$input[[which(fis_input_names == EP)]]
          shinyjs::show("EP_params_container")

          # Change Title
          output$EP_params_name <- renderUI({
            HTML(paste0("<h4>", formatEPString(EP), "</h4>" ))
          })

          # Update Range values
          updateNumericInput(
            session,
            "EP_params_min_range",
            value = curr_params$range[1],
            max = input$EP_params_max_range
          )
          updateNumericInput(
            session,
            "EP_params_max_range",
            value = curr_params$range[2],
            min = input$EP_params_min_range
          )

          # Update 'Deletion' options
          input_index <- which(fis_input_names == EP)
          mf_names <- sapply(project_data$fis$input[[input_index]]$mf, "[[", 1)
          updateSelectInput(
            session,
            "select_mf_to_delete",
            choices = mf_names
          )

          # Adjust UI list
          EPMF_ui_list$mf_container_ids <- lapply(
            seq_along(curr_params$mf),
            function(i) {
              paste0("mf_", i)
            }
          )

          setupMFContainer(
            paste0(parent_id, "-", id),
            EPMF_ui_list,
            curr_params,
            input,
            output,
            project_data
          )
        }
      } else {
        shinyjs::hide("EP_params_container")
      }
    }, ignoreNULL = FALSE)

    # Accept changes to Range input
    observeEvent(c(input$EP_params_min_range, input$EP_params_max_range), {
      if (isTruthy(input$element_select) && isTruthy(input$property_select)) {
        # Identify Input position in FIS
        EP <- formatEPName(input$element_select, input$property_select)
        fis_input_names <- sapply(project_data$fis$input, "[[", 1)
        curr_EP_index <- which(fis_input_names == EP)

        # Update FIS values
        project_data$fis$input[[curr_EP_index]]$range[1] =
          input$EP_params_min_range
        project_data$fis$input[[curr_EP_index]]$range[2] =
          input$EP_params_max_range

        # Ensure minimum is never higher than maximum
        updateNumericInput(
          session,
          "EP_params_min_range",
          max = input$EP_params_max_range,
        )
        updateNumericInput(
          session,
          "EP_params_max_range",
          min = input$EP_params_min_range,
        )
      }
    })

    # Observe Add New Membership Function Button
    # observeEvent(input$add_new_mf_button , {
    #   EP <- formatEPName(input$element_select, input$property_select)
    #   fis_input_names <- sapply(project_data$fis$input, "[[", 1)
    #   input_index <- which(fis_input_names == EP)
    #   mf_names <- sapply(project_data$fis$input[[input_index]]$mf, "[[", 1)
    #   count <- 1
    #   if (!(isTruthy(input$new_mf_name_input))) {
    #     # Add unique MF term
    #     new_name <- "Unnamed"
    #     while (new_name %in% mf_names) {
    #       new_name <- paste0("Unnamed-", toString(count))
    #       count = count + 1
    #     }
    #   } else {
    #     new_name <- input$new_mf_name_input
    #     while (new_name %in% mf_names) {
    #       new_name <- paste0(input$new_mf_name_input, "-", toString(count))
    #       count = count + 1
    #     }
    #   }
    # 
    #   project_data$fis <- addmf(
    #     project_data$fis,
    #     'input',
    #     input_index,
    #     new_name,
    #     'trimf',
    #     c(0.15, 0)
    #   )
    # 
    #   curr_params <- project_data$fis$input[[which(fis_input_names == EP)]]
    # 
    #   # Update list of UI elements
    #   EPMF_ui_list$mf_container_ids <- lapply(
    #     seq_along(curr_params$mf),
    #     function(i) {
    #       paste0("mf_", i)
    #     }
    #   )
    # 
    #   # Set-up UI
    #   setupMFContainer(
    #     paste0(parent_id, "-", id),
    #     EPMF_ui_list,
    #     curr_params,
    #     input,
    #     output,
    #     project_data
    #   )
    # 
    #   # Reset EPV relationships based on this Term
    #   matches <- sapply(
    #     names(project_data$EPV_relationships),
    #     startsWith,
    #     EP
    #   )
    # 
    #   if (isTruthy(matches)) {
    #     matches <- which(matches)
    #     project_data$EPV_relationships <- project_data$EPV_relationships[
    #       -(matches), drop = FALSE
    #     ]
    #   }
    # })

    # Update Deletion Options as needed.
    # observeEvent(project_data$fis$input, {
    #   if (isTruthy(input$element_select) && isTruthy(input$property_select)) {
    #     # Get list of options from FIS
    #     EP <- formatEPName(input$element_select, input$property_select)
    #     if (EP %in% project_data$EP_names) {
    #       fis_input_names <- sapply(project_data$fis$input, "[[", 1)
    #       input_index <- which(fis_input_names == EP)
    # 
    #       mf_names <- sapply(project_data$fis$input[[input_index]]$mf, "[[", 1)
    # 
    #       if (length(mf_names) > 0) {
    #         mf_selected <- mf_names[1]
    #       } else {
    #         mf_names <- ""
    #         mf_selected <- ""
    #       }
    # 
    #       # Update Select element
    #       updateSelectInput(
    #         session,
    #         "select_mf_to_delete",
    #         choices = mf_names,
    #         selected = mf_selected
    #       )
    #     }
    #   }
    # })

    # Observe Remove Term button
    # observeEvent(input$delete_mf_button, {
    # 
    #   mf_to_delete <- input$select_mf_to_delete
    # 
    #   if (isTruthy(mf_to_delete)) {
    #     # Extract FIS information
    #     EP <- formatEPName(input$element_select, input$property_select)
    #     fis_input_names <- sapply(project_data$fis$input, "[[", 1)
    #     input_index <- which(fis_input_names == EP)
    #     mf_names <- sapply(project_data$fis$input[[input_index]]$mf, "[[", 1)
    #     mf_index <- which(mf_names == mf_to_delete)
    # 
    #     if (isTruthy(mf_index)) {
    #       # Delete term
    #       project_data$fis$input[[input_index]]$mf <-
    #         project_data$fis$input[[input_index]]$mf[-(mf_index), drop = FALSE]
    # 
    #       curr_params <- project_data$fis$input[[which(fis_input_names == EP)]]
    # 
    #       # Update list of UI elements
    #       EPMF_ui_list$mf_container_ids <- lapply(
    #         seq_along(curr_params$mf),
    #         function(i) {
    #           paste0("mf_", i)
    #         }
    #       )
    # 
    #       # Reset EPV relationships based on this Term
    #       matches <- sapply(
    #         names(project_data$EPV_relationships),
    #         startsWith,
    #         EP
    #       )
    # 
    #       if (isTruthy(matches)) {
    #         matches <- which(matches)
    #         project_data$EPV_relationships <- project_data$EPV_relationships[
    #           -(matches), drop = FALSE
    #         ]
    #       }
    # 
    #       # Set-up UI
    #       setupMFContainer(
    #         paste0(parent_id, "-", id),
    #         EPMF_ui_list,
    #         curr_params,
    #         input,
    #         output,
    #         project_data
    #       )
    #     }
    #   }
    # })

    # Plot Selected MF ---------------------------------------------------------
    output$EPMF_plot <- renderPlot({
      current_name <- formatEPName(input$element_select,input$property_select)
      if (current_name %in% project_data$EP_names) {
        constructMFPlot(
          current_name,
          project_data$fis,
          "input"
        )
      }
    }, height = 400)

  })
}
