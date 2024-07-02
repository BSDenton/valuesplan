# Home Page --------------------------------------------------------------------

# Homepage Helpers
generateSummaryTable <- function(file_contents) {

  summary_df <- data.frame(matrix(nrow = 0, ncol = 2))

  # Title
  df_title <-       if (isTruthy(file_contents$title))
                    file_contents$title
                    else "-"
  summary_df[nrow(summary_df) + 1, ] <- c("Title", df_title)

  # Elements
  df_elements <-    if (isTruthy(file_contents$elements))
                    paste0(file_contents$elements, collapse = ", ")
                    else "-"
  summary_df[nrow(summary_df) + 1, ] <- c("Elements", df_elements)

  # Properties
  df_properties <-  if (isTruthy(file_contents$properties))
                    paste0(file_contents$properties, collapse = ", ")
                    else "-"
  summary_df[nrow(summary_df) + 1, ] <- c("Properties", df_properties)

  # Values
  df_values <-      if (isTruthy(file_contents$values))
                    paste0(file_contents$values, collapse = ", ")
                    else "-"
  summary_df[nrow(summary_df) + 1, ] <- c("Values", df_values)

  return(summary_df)

}

# UI ---------------------------------------------------------------------------

homePageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      tags$head(
        tags$style(HTML(
          "
          /* File Input */
          .input-group-btn:first-child>.btn {
            height: 40px !important;
            padding-top: 8px !important;
            border-radius: 0px !important;
          }

          .input-group .form-control {
            height: 40px !important;
            border-radius: 0px !important;
            background-color: white;
          }

          /* File Input Progress Bar */
          .progress {
            height: 16px;
            border-radius: 0px;
          }
          .progress .progress-bar {
            line-height: 16px;
          }

          /* Summary Table */
          table.dataTable thead th {
            padding: 0px !important;
          }

          "
        ))
      ),

      # Title ------------------------------------------------------------------
      fluidPage(
        useShinyjs(),
        h2("ValuesPlan"),
        br(),
        # Learn More: Valuesplan -----------------------------------------------
        p("Welcome to ValuesPlan!"),
        p("ValuesPlan is a decision-support software developed around the 
          values-driven management framework. More information about the 
          framework and its key definitions can be found in Wallace (2012)."),
        p("DOI: 10.1016/j.envsci.2011.12.001"),
        br()
      ),

      # Create Project ---------------------------------------------------------
      wellPanel(
        style = HP_SECTION_STYLE,
        h4(strong("Build a New Project:")),
        p("If you’re looking to define a new project, start here."),
        actionButton(
          NS(id, "go_to_pb"),
          "Start a New Project",
          class = "btn btn-primary"
        )
      ),

      # Edit Project -----------------------------------------------------------
      wellPanel(
        style = HP_SECTION_STYLE,
        fluidRow(
          column(12,
            h4(strong("Edit an Existing Project:")),
            p("If you want to continue working upon an existing project, 
              upload the project file here."
            ),
          )
        ),
        fluidRow(
          # File Upload and Accept
          column(5,
            fileInput(
              NS(id, "edit_upload_project"),
              NULL,
              accept = ".rds"
            ),
            shinyjs::hidden(
              actionButton(
                NS(id, "edit_import_project_button"),
                "Import Project",
                class = "btn btn-primary",
                style = "width: 100%; margin-top: 5px;"
              )
            )
          ),
          # Show Project Summary
          column(7,
            shinyjs::hidden(
              h5(
                strong("File Summary:"),
                style="margin-top: 0px;",
                id = NS(id, "edit_summary_table_title")
              )
            ),
            DT::DTOutput(
              NS(id, "edit_project_summary")
            )
          )
        )
      ),

      # Analyse Project --------------------------------------------------------
      wellPanel(
        style = HP_SECTION_STYLE,
        fluidRow(
          column(12,
            h4(strong("Analyse a Project:")),
            p("If you have finished building a project, or have an existing 
              project ready, you can analyse it here. Analysis allows you to 
              investigate how changes in the management system state will 
              affect the delivery of important human values."),
            prettyRadioButtons(
              inputId = NS(id, "choose_analysis_project"),
              label = "Do you want to analyse the current project,
                       or upload a new one?",
              choices = list(
                "Analyse the current project" = "current",
                "Analyse a different project" = "new"
              )
            ),
            br()
          )
        ),
        shinyjs::hidden(
          fluidRow(id = NS(id, "row_analysis_current"), align = 'center',
            column(12,
              actionButton(
                NS(id, "analysis_current_project_button"),
                "Analyse Current Project",
                class = "btn btn-primary",
                style = "width: 80%; margin-top: 5px;"
              ),
              shinyjs::hidden(
                htmlOutput(
                  NS(id, "current_project_validity"),
                  style = "padding-top: 20px;"
                )
              )
            ),
          )
        ),
        shinyjs::hidden(
          fluidRow(id = NS(id, "row_analysis_new"),
            column(12,
              fluidRow(
                # File Upload and Accept
                column(4,
                  fileInput(
                    NS(id, "analysis_upload_project"),
                    "Choose a project...",
                    accept = ".rds"
                  )
                ),
                # Show Project Summary
                column(7,
                  shinyjs::hidden(
                    h5(
                      strong("File Summary:"),
                      style="margin-top: 25px;",
                      id = NS(id, "analysis_summary_table_title")
                    )
                  ),
                  DT::DTOutput(
                    NS(id, "analysis_imported_project_summary")
                  )
                )
              ),
              fluidRow(align = 'center',
                shinyjs::hidden(
                  tags$div(
                    id = NS(id, "analysis_warning_for_import"),
                    style = "padding: 25px 0px;",
                    strong(class = "text-danger",
                      "Warning: Importing a project will override any current
                      project data.")
                  )
                ),
                shinyjs::hidden(
                  actionButton(
                    NS(id, "analysis_new_project_button"),
                    "Import and Analyse Project",
                    class = "btn btn-primary",
                    style = "width:80%; margin-top: 0px;"
                  ),
                  shinyjs::hidden(
                    htmlOutput(
                      NS(id, "new_project_validity"),
                      style = "padding-top: 20px;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

}

# Server -----------------------------------------------------------------------

homePageServer <- function(id, project_data, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Create Project ---------------------------------------------------------
    observeEvent(input$go_to_pb, {
      # Reset data to default values
      for (item in names(DEFAULT_PROJECT_DATA)) {
        project_data[[item]] <- DEFAULT_PROJECT_DATA[[item]]
      }
      # Switch to Project Builder Panel
      updateTabsetPanel(
        parent_session,
        "nav_wizard",
        selected = paste0("page_pb")
      )
    })

    # Edit Project -------------------------------------------------------------
    # File Summary -------------------------------------------------------------
    observeEvent(input$edit_upload_project, {
      output$edit_project_summary <- DT::renderDT({

        # Checks and upload ------------------------------------------------------
        # Hide Import Button by default, unless checks are passed
        shinyjs::hide("edit_import_project_button")

        if (is.null(input$edit_upload_project)) {
          return(NULL)
        }

        # Confirm if an .rds file has been uploaded.
        validate(
          need(
            endsWith(input$edit_upload_project$name, ".rds"),
            "File must be an .rds file."
          )
        )

        # Read in .rds file
        file_contents <- readRDS(input$edit_upload_project$datapath)

        # Confirm if file has valuesplan marker
        validate(
          need(
            file_contents[['type']] == "valuesplan-project",
            "File is not recognised as a Valuesplan Project."
          )
        )

        # If valid file, show summary and import button
        shinyjs::show("edit_summary_table_title")
        shinyjs::show("edit_import_project_button")

        # Render Summary Table ---------------------------------------------------
        return(generateSummaryTable(file_contents))

      },
      options = list(scrollY = 100, dom="t", paging = FALSE, ordering = FALSE),
      selection = "none",
      rownames = FALSE,
      colnames = rep("", 2)
      )
    })

    # Import file data & open Project Builder upon button press ------------------
    observeEvent(input$edit_import_project_button, {
      if (isTruthy(input$edit_upload_project)) {
        # Import file contents
        file_contents <- readRDS(input$edit_upload_project$datapath)
        for (item in names(file_contents)) {
          if (item %in% names(project_data)) {
            project_data[[item]] <- file_contents[[item]]
          }
        }

        # Move user to Project Builder
        updateTabsetPanel(
          parent_session,
          "nav_wizard",
          selected = paste0("page_pb")
        )

        # Clear 'Edit Project' tab & other elements
        shinyjs::reset("edit_upload_project")
        shinyjs::hide("edit_summary_table_title")
        output$edit_project_summary <- NULL
        shinyjs::hide("edit_import_project_button")
        shinyjs::hide("current_project_validity")
        shinyjs::hide("new_project_validity")
      }
    })

    # Analyse Project ----------------------------------------------------------

    # Radio Buttons Input
    observeEvent(input$choose_analysis_project, {
      if (input$choose_analysis_project == "current") {
        shinyjs::hide("row_analysis_new")
        shinyjs::show("row_analysis_current")
      }
      if (input$choose_analysis_project == "new") {
        shinyjs::hide("row_analysis_current")
        shinyjs::show("row_analysis_new")
      }
    })

    # Analyse Existing Project -------------------------------------------------
    observeEvent(input$analysis_current_project_button, {
      # Check validity of project: TBA
      if (isTruthy(project_data$title)) {
        shinyjs::hide("current_project_validity")
        # Move user to Project Analysis
        updateTabsetPanel(
          parent_session,
          "nav_wizard",
          selected = paste0("page_pa")
        )

      } else {
        shinyjs::show("current_project_validity")
        output$current_project_validity <- renderUI({
          HTML("Projects must be titled before analysis.")
        })
      }
    })

    # Analyse New Project ------------------------------------------------------
    # Select Project
    observeEvent(input$analysis_upload_project, {
      output$analysis_imported_project_summary <- DT::renderDT({

        # Checks and upload ----------------------------------------------------
        # Hide Import Button by default, unless checks are passed
        shinyjs::hide("analysis_warning_for_import")
        shinyjs::hide("analysis_new_project_button")

        if (is.null(input$analysis_upload_project)) {
          return(NULL)
        }

        # Confirm if an .rds file has been uploaded.
        validate(
          need(
            endsWith(input$analysis_upload_project$name, ".rds"),
            "File must be an .rds file."
          )
        )

        # Read in .rds file
        file_contents <- readRDS(input$analysis_upload_project$datapath)

        # Confirm if file has valuesplan marker
        validate(
          need(
            file_contents[['type']] == "valuesplan-project",
            "File is not recognised as a Valuesplan Project."
          )
        )

        # If valid file, show summary and import button
        shinyjs::show("analysis_summary_table_title")
        shinyjs::show("analysis_warning_for_import")
        shinyjs::show("analysis_new_project_button")

        # Render Summary Table -------------------------------------------------
        return(generateSummaryTable(file_contents))

      },
      options = list(scrollY = 100, dom="t", paging = FALSE, ordering = FALSE),
      selection = "none",
      rownames = FALSE,
      colnames = rep("", 2)
      )
    })

    # Check project and upload
    observeEvent(input$analysis_new_project_button, {
      # Import file contents
      if (isTruthy(input$analysis_upload_project)) {
        file_contents <- readRDS(input$analysis_upload_project$datapath)
        for (item in names(file_contents)) {
          if (item %in% names(project_data)) {
            project_data[[item]] <- file_contents[[item]]
          }
        }
      }
      
      # Check validity of project: TBA
      if (isTruthy(project_data$title) &&
          length(project_data$EPV_relationships) > 0 &&
          isTruthy(project_data$fis$rule)
          ) {
        shinyjs::hide("new_project_validity")
        # Move to Project Analysis
        updateTabsetPanel(
          parent_session,
          "nav_wizard",
          selected = paste0("page_pa")
        )
        # Reset UI elements
        shinyjs::reset("analysis_upload_project")
        shinyjs::hide("analysis_summary_table_title")
        output$analysis_imported_project_summary <- NULL
        shinyjs::hide("analysis_warning_for_import")
        shinyjs::hide("analysis_new_project_button")
        shinyjs::hide("current_project_validity")
        shinyjs::hide("new_project_validity")
      } else {
        shinyjs::show("new_project_validity")
        output$new_project_validity <- renderUI({
          HTML("Projects must be titled before analysis.
               <br>
               Projects must also contain at least one valid relationship between 
               its properties and values.")
        })
      }
    })
  })
}
