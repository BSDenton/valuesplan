# Page 6: Property-Value Relationships and Rulesbase ---------------------------

# UI ---------------------------------------------------------------------------

EPVRelationshipsPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      tags$head(
        tags$style(HTML(PAGE_6_STYLE))
      ),
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Value Property Relationships:")),
          p("You have successfully described which element properties relate to 
          particular human values. Now it is important to describe how they are 
          related."),
          p("This page allows you to define the shape of these relationships 
            below; please note, if the shape of a relationship is not saved, it 
            will not be included in analysis."),
          br(), hr(),
          selectInput(
            NS(id, "EP_value_select"),
            "Select a connected Property and Value...",
            choices = ""
          )
        )
      ),
      uiOutput(outputId = NS(id, 'relationship_sliders')),
      fluidRow(
        column(2),
        column(8,
          uiOutput(outputId = NS(id, 'relationship_status'))
        ),
        column(2)
      )
    )
  )
}

# Server -----------------------------------------------------------------------

EPVRelationshipsPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Page 6 Variables ---------------------------------------------------------
    ui_list <- reactiveValues(
      # Stores both the textOutput ids and the current input relationship data:
      # [textID, EP name, EP term, Value name, Value term].
      text = c(),
      # Stores the sliderInput ids.
      sliders = c()
    )

    # EPV Relationship Selection and Definition --------------------------------
    # Update Select when Element-Property / Value connections are changed.
    observeEvent(project_data$val_conn_names, {

      # Format Element-Property and Values connections for a grouped select
      EPV_choices <- list()

      for (EPV in project_data$val_conn_names) {
        val <- EPV[2]
        EP <- formatEPString(EPV[1])
        EPV_choices[[EP]][[val]] <- generateEPVName(EPV)
      }

      if (!(isTruthy(EPV_choices))) {
        EPV_choices <- ""
        EPV_selected <- ""
      } else {
        EPV_selected <- EPV_choices[1][1]
      }

      updateSelectInput(
        session,
        "EP_value_select",
        choices = EPV_choices,
        selected = EPV_selected
      )
    }, ignoreNULL = FALSE)

    # When the selection is changed, show relevant relationship input options.
    observeEvent(input$EP_value_select, {
      if (isTruthy(input$EP_value_select)) {
        # Split Element-Property from Value
        EPV <- str_split_1(input$EP_value_select, "@")

        # Find Input MF information from FIS.
        fis_input_names <- sapply(project_data$fis$input, "[[", 1)
        input_index <- which(fis_input_names == EPV[1])
        input_params <- project_data$fis$input[[input_index]]
        input_mf_names <- sapply(input_params$mf, "[[", 1)

        if (length(input_mf_names) > 0) {

          # Find Output MF information from FIS.
          fis_output_names <- sapply(project_data$fis$output, "[[", 1)
          output_index <- which(fis_output_names == EPV[2])
          output_params <- project_data$fis$output[[output_index]]
          output_mf_names <- sapply(output_params$mf, "[[", 1)

          # Save references to UI elements that are about to be added.
          ui_list$text <- lapply(seq_along(input_mf_names), function(i) {
            c(
              paste0("text", input_index, output_index, input_mf_names[i]),
              EPV[1],
              input_mf_names[i],
              EPV[2],
              output_mf_names[1]
            )
          })

          ui_list$sliders <- lapply(seq_along(input_mf_names), function(i) {
            paste0("slider", input_index, output_index, input_mf_names[i])
          })

          # Generate UI dynamically based on this information
          output$relationship_sliders <- renderUI({
            list(
              lapply(rev(seq_along(input_mf_names)), function(i) {
                fluidRow(
                  style = paste0(WELL_STYLE, " margin: 30px 0;"),
                  column(6,
                    textOutput(
                      NS(
                        paste0(parent_id, "-", id),
                        paste0(
                          "text",
                          input_index,
                          output_index,
                          input_mf_names[i]
                        )
                      )
                    )
                  ),
                  column(6, align="center",
                    sliderTextInput(
                      inputId = NS(
                        paste0(parent_id, "-", id),
                        paste0(
                          "slider",
                          input_index,
                          output_index,
                          input_mf_names[i]
                        )
                      ),
                      label = NULL,
                      grid = TRUE,
                      choices = output_mf_names
                    )
                  )
                )
              }),
              fluidRow(
                style = paste0(WELL_STYLE, " margin: 30px 0;"),
                column(12, align = "center",
                  actionButton(
                    NS(paste0(parent_id, "-", id), "save_relationship"),
                    "Save Relationship",
                    class = "btn btn-success"
                  )
                )
              )
            )
          })

          # Generate Observers for new inputs, which update output text.
          lapply(ui_list$sliders, function(item) {
            observeEvent(input[[item]], {
              # Update list with latest value
              ui_list$text[[which(ui_list$sliders == item)]][5] <- input[[item]]
              # Isolate information
              text_info <- ui_list$text[[which(ui_list$sliders == item)]]
              text_id <- text_info[1]
              # Update text
              output[[text_id]] <- renderText(
                paste0(
                  "When ", formatEPString(text_info[2]),
                  " is ", text_info[3], ", ", text_info[4], " is ", text_info[5]
                )
              )
            })
          })

          # Fill in previous selections, if present, based on stored data.
          if (input$EP_value_select %in%
              names(project_data$EPV_relationships)) {

            relationship_info <- project_data$EPV_relationships[[
              input$EP_value_select,
              drop = FALSE
            ]]

            for (i in 1:length(relationship_info)) {
              updateSliderTextInput(
                session,
                ui_list$sliders[[i]],
                selected = relationship_info[[i]][5]
              )
            }
          }
        } else {
          ui_list$sliders <- c()
          output$relationship_sliders <- renderUI({
            HTML(
              "<p style = 'text-align: center; margin: 30px 0px;'>
              This property does not have any linguistic terms associated with
              it.
              </p>"
            )
          })
        }
      }
    })

    # Remove extraneous EPV relationships when an EP or Value is deleted.
    observeEvent(project_data$val_conn_names, {
      all_EPVs <- sapply(project_data$val_conn_names, generateEPVName)
      matches <- which(names(project_data$EPV_relationships) %in% all_EPVs)
      project_data$EPV_relationships <- project_data$EPV_relationships[matches]
    })

    # Save EPV relationship on button click.
    observeEvent(input$save_relationship, {
      project_data$EPV_relationships[[input$EP_value_select]] <- ui_list$text
    })

    # Defined Relationships Status Table ---------------------------------------
    observeEvent(c(
      project_data$val_conn_names,
      names(project_data$EPV_relationships)), {
        undefined_relationships <- c()

        for (EPV in project_data$val_conn_names) {
          EPV_name <- generateEPVName(EPV)
          # Check whether status has been defined.
          if (!(EPV_name %in% names(project_data$EPV_relationships))) {
            undefined_relationships[[EPV_name]] <- EPV
          }
        }

        if (isTruthy(undefined_relationships)) {
          output$relationship_status <- renderUI({
            list(
              HTML(
                "<div id = 'relationship_status'>
                <h5>
                The following relationships have <b>not</b> been defined:
                </h5>"
              ),
              lapply(undefined_relationships, function(rel) {
                HTML(paste0(
                  "<li>",
                  formatEPString(rel[1]),
                  "&emsp;->&emsp;",
                  rel[2],
                  "</li>")
                )
              }),
              HTML("</div>")
            )
          })
        } else {
          output$relationship_status <- renderUI({
            NULL
          })
        }
      }, ignoreNULL = FALSE
    )
  })
}
