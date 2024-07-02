# Page 7: FIS Rulebase ---------------------------------------------------------

# UI ---------------------------------------------------------------------------

rulesbasePageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Rulesbase:")),
          p("When you have finished defining the relationships between element 
            properties and human values, you can click here to generate a fuzzy 
            system ‘rulebase’, an important component of fuzzy logic systems."),
          strong("Once you have completed this step, the project is ready for 
                 analysis."),
          p("Save your project, and use the Return Home button in the upper-left
            to return to the Homepage. From here, you can access the Project
            Analysis section."),
          br(), hr(),
          actionButton(
             NS(id, "generate_rulesbase"),
            "Auto-Generate a Simple Rulesbase",
            class = "btn btn-success"
          )
        )
      ),
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          DT::DTOutput(NS(id, "show_rulesbase")),
          # Allow deletion of selected rule.
          br(),
          shinyjs::hidden(
            tags$div(
              id = NS(id, "remove_rule_button"),
              actionButton(
                NS(id, "remove_rule"),
                "Remove Selected Rule",
                class = "btn btn-danger"
              )
            )
          )
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

rulesbasePageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Page 7 Variables ---------------------------------------------------------

    rulesbase <- reactiveValues(
      df = NULL
    )

    # Generate a rulesbase when user clicks button -----------------------------
    # Currently: Produces simple single-input rules based on relationships.
    observeEvent(input$generate_rulesbase, {
      # Reset FIS rulebase.
      project_data$fis$rule <- NULL
      # Extract information about FIS.
      fis_input_names <- sapply(project_data$fis$input, "[[", 1)
      fis_output_names <- sapply(project_data$fis$output, "[[", 1)
      input_count <- length(project_data$fis$input)
      output_count <- length(project_data$fis$output)

      for (EPV in project_data$EPV_relationships) {
        # Match relationships data to FIS.
        input_name <- EPV[[1]][2]
        output_name <- EPV[[1]][4]
        input_index <- which(fis_input_names == input_name)
        output_index <- which(fis_output_names == output_name)
        input_mf_names <-  sapply(
          project_data$fis$input[[input_index]]$mf,
          "[[",
          1
        )
        output_mf_names <-  sapply(
          project_data$fis$output[[output_index]]$mf,
          "[[",
          1
        )

        for (relationship in EPV) {
          # Generate rule in FuzzyR format, and add to FIS.
          # Currently: Sets rule's weight to 1. Sets AND as operator.
          new_rule <- c(rep(0, (input_count + output_count)), 1, 1)
          input_mf_index <- which(input_mf_names == relationship[3])
          output_mf_index <- which(output_mf_names == relationship[5])

          new_rule[input_index] <- input_mf_index
          new_rule[output_index + input_count] <- output_mf_index
          # Add rule to FIS
          project_data$fis <- addrule(project_data$fis, new_rule)
        }
      }

      if (isTruthy(project_data$fis$rule)) {
        # Add rulebase data to page's data frame
        fis_rulesbase <- project_data$fis$rule
        rulesbase$df <- data.frame(matrix(
          nrow = 0,
          ncol = 3
        ))

        # Iterate over rules, format into strings, and add them to rulesbase df.
        for (row_index in 1:nrow(fis_rulesbase)) {
          rule <- fis_rulesbase[row_index, , drop = FALSE]
          # First column: "When..."
          rule_str <- c("When")
          # Second column: antecedents
          rule_input_str <- ""
          rule_input <- rule[
            0:input_count,
            drop = FALSE
          ]
          for (i in 1:length(rule_input)) {
            # Check if antecedent is used
            if (rule_input[i] != 0) {
              # Add correct operator if not first antecedent: "...AND"
              if (rule_input_str != "") {
                if (rule[length(rule)] == 1) {
                  paste0(rule_input_str, " AND ")
                } else{
                  paste0(rule_input_str, " OR ")
                }
              }
              # Add antecedent info to rule: "...X is x"
              rule_input_str <- paste0(
                rule_input_str,
                formatEPString(fis_input_names[i]),  # Input name
                " is ",
                project_data$fis$input[[i]]$mf[[rule_input[i]]]$name  # Term name
              )
            }
          }
          rule_str <- c(rule_str, rule_input_str)
          # Third column: consequent
          rule_output_str <- ""
          rule_output <- rule[
            (input_count + 1):(input_count + output_count),
            drop = FALSE
          ]
          for (j in 1:length(rule_output)) {
            if (rule_output[j] != 0) {
              # Add consequent info to rule: "... Y is y"
              rule_output_str <- paste0(
                rule_output_str,
                # Output Name
                fis_output_names[j],
                " is ",
                # Term / MF name
                project_data$fis$output[[j]]$mf[[rule_output[j]]]$name
              )
            }
          }
          rule_str <- c(rule_str, rule_output_str)
          # Add to data frame
          rulesbase$df[nrow(rulesbase$df) + 1, ] <- rule_str
        }
      }
      if (isTruthy(rulesbase$df) && nrow(rulesbase$df) > 0) {
        shinyjs::show("remove_rule_button")
      }
      
    })

    # Show user the up-to-date rulesbase ---------------------------------------
    observeEvent(rulesbase$df, {
      output$show_rulesbase <- DT::renderDT(
        {
          if (nrow(rulesbase$df) > 0) {
            return(rulesbase$df)
          }
        },
        options = list(
          scrollY = 150,
          dom="t",
          paging = FALSE,
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = rep("", ncol(rulesbase$df)),
        selection = 'single'
      )
      
      if (isTruthy(rulesbase$df) && nrow(rulesbase$df) == 0) {
        shinyjs::hide("remove_rule_button")
      }

      
    })

    # User deletes selected rule, through button click -------------------------
    observeEvent(input$remove_rule, {
      selected_index <- input$show_rulesbase_rows_selected
      if(isTruthy(selected_index)) {
        project_data$fis$rule <- project_data$fis$rule[
          -(selected_index),
          ,
          drop = FALSE
        ]
        rulesbase$df <- rulesbase$df[
          -(selected_index),
          ,
          drop = FALSE
        ]
      }
    })
  })
}
