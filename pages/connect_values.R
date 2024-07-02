# Page 5: Property-Value Connections -------------------------------------------

constructValuesConnectionsPlot <- function(properties, values, connections) {
  # Constructs an image that shows the current connections between element
  # properties and human values, as users input them.

  if (!isTruthy(properties) || !isTruthy(values)) {
    return (NULL)
  }

  # Initialize labels data frame.
  label_df <- setNames(data.frame(
    matrix(
      ncol = 3,
      nrow = 0
    )),
    c("position", "group", "name")
  )

  # Add element properties to table, row by row.
  for (index in 1:length(properties)) {
    label_df[nrow(label_df) + 1,] <- list(
      index,
      "Element Properties",
      properties[index]
    )
  }

  # Add values to table, row by row.
  for (index in 1:length(values)) {
    label_df[nrow(label_df) + 1,] <- list(index, "Values", values[index])
  }

  # Initialize new connections data frame (formatted for geom_segment)
  conn_df <- setNames(data.frame(
    matrix(
      ncol = 5,
      nrow = 0
    )),
    c("property", "x1", "property_index", "x2", "value_index")
  )

  # Calculate data for connections data frame
  for (row_name in rownames(connections)) {
    for (col_name in colnames(connections)) {
      if (connections[row_name, col_name] > 0) {
        # Find y indices for properties/values, based on label_df
        property_index <- label_df$position[label_df$name == col_name]
        value_index <- label_df$position[label_df$name == row_name]
        # TBA: Account for differences in label size
        conn_df[nrow(conn_df) + 1,] <- list(
          row_name, 1, property_index, 2, value_index
        )
      }
    }
  }

  # Make Element-Property labels more user readable
  label_df$name[label_df$group == "Element Properties"] <- lapply(
    label_df$name[label_df$group == "Element Properties"],
    function(EP) {
      formatEPString(EP)
    }
  )

  # Plot Graph
  ggplot() +
    geom_segment(
      data = conn_df,
      mapping = aes(
        x = x1,
        y = property_index,
        xend = x2,
        yend = value_index
      ),
      color = "grey20"
    ) +
    geom_label(
      data = label_df,
      mapping = aes(x = group, y = position, label = name, fill = group),
      size = 5,
      label.size = 0.4,
      label.padding = unit(0.7, "lines")
    ) +
    theme(
      plot.margin = margin(0, 0, 20, 0),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 14, margin = margin(t = 0)),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    ) +
    scale_x_discrete() +
    expand_limits(y = c(0, max(length(properties), length(values)) + 1)) +
    scale_fill_manual(values = c(PROPERTY_COLOR, VALUE_COLOR))

}

# UI ---------------------------------------------------------------------------

connectValuesPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(

      # Input Connections ------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Value Property Connections:")),
          p("For each element property in the system, select all the human 
            values that it affects."),
          p("For example, if you want to investigate how the Species Richness 
            of a Vegetation Element affects the Aesthetic Enjoyment value of 
            the element, you would need to select the ‘Vegetation Element: 
            Species Richness’ property, and then select the ‘Aesthetic 
            Enjoyment’ value to connect them."),
          br(), hr(),
          fluidRow(
            column(1),
            column(5,
              selectInput(
                NS(id, "EP_select"),
                "Select an Element Property",
                choices = ""
              )
            ),
            column(5,
              selectInput(
                NS(id, "value_select"),
                "Select Values",
                choices = "",
                multiple = TRUE,
                selectize = TRUE
              )
            ),
            column(1)
          )
        )
      ),
      # Connections Plot -------------------------------------------------------
      fluidPage(
        tags$div(
          style = WELL_STYLE,
          fluidRow(align = "center",
            plotOutput(NS(id, "VPplot"), width = "80%", height = "100%")
          )
        )
      )
    )
  )
}

# Server -----------------------------------------------------------------------

connectValuesPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Update Connections data frame, when values/element-properties change -----
    observeEvent(c(project_data$values, project_data$EP_names), {

      VP_exists <- isTruthy(project_data$values) &&
                   isTruthy(project_data$EP_names)

      # Remove Connections data frame if values or properties don't exist.
      if (!(is.null(project_data$val_conn_matrix)) && !(VP_exists)) {
        project_data$val_conn_matrix <- NULL
      }

      # Set-up Connections data frame, when both elements and properties exist.
      else if (is.null(project_data$val_conn_matrix) && VP_exists) {
        project_data$val_conn_matrix <- data.frame(matrix(
          nrow = length(project_data$values),
          ncol = length(project_data$EP_names)
        ))
        rownames(project_data$val_conn_matrix) <- project_data$values
        colnames(project_data$val_conn_matrix) <- project_data$EP_names
        project_data$val_conn_matrix[,] <- 0
      }

      # Update Connections data frame, if all exist.
      else if (!(is.null(project_data$val_conn_matrix)) && VP_exists) {
        # Extract column and row names from connections data frame.
        conn_values <- rownames(project_data$val_conn_matrix)
        conn_properties <- colnames(project_data$val_conn_matrix)
        # Find mismatches.
        missing_values <- setdiff(project_data$values, conn_values)
        extra_values <-  setdiff(conn_values, project_data$values)
        missing_properties <- setdiff(project_data$EP_names, conn_properties)
        extra_properties <- setdiff(conn_properties, project_data$EP_names)

        for (value in missing_values) {
          # Add missing values to Connections data frame.
          if (!(value %in% conn_values)) {
            project_data$val_conn_matrix[value, ] <- 0
          }
        }

        for (value in extra_values) {
          # Remove extraneous values from Connections data frame.
          project_data$val_conn_matrix <- project_data$val_conn_matrix[
            !(row.names(project_data$val_conn_matrix) %in% value),
            ,
            drop = FALSE
          ]
        }

        for (property in missing_properties) {
          # Add missing properties to Connections data frame.
          if (!(property %in% conn_properties)) {
            project_data$val_conn_matrix[, property] <- 0
          }
        }

        for (property in extra_properties) {
          # Remove extraneous properties from Connections data frame.
          project_data$val_conn_matrix <- project_data$val_conn_matrix[,
            !(colnames(project_data$val_conn_matrix) %in% property),
            drop = FALSE
          ]
        }
      }
    }, ignoreNULL = FALSE)

    # Update Value-Property Names vector, whenever Connections changes ---------
    observeEvent(project_data$val_conn_matrix, {
      if (!isTruthy(project_data$val_conn_matrix)) {
        project_data$val_conn_names <- list()
      } else {
        curr_names <- list()
        for(i in 1:nrow(project_data$val_conn_matrix)) {
          row <- project_data$val_conn_matrix[i, , drop = FALSE]
          matches <- which(row > 0)
          matches <- colnames(project_data$val_conn_matrix)[matches]
          for (property in matches) {
            curr_names[[length(curr_names) + 1]] <- c(property, rownames(row))
          }
        }
        project_data$val_conn_names <- curr_names
      }
    }, ignoreNULL = FALSE)

    # Input Value-Property Connections ---------------------------------------
    # Update Element-Property select options
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
        "EP_select",
        choices = EP_choice,
        selected = EP_selected
      )
    }, ignoreNULL = FALSE)

    # Update Values select options
    observeEvent(c(input$EP_select, project_data$values), {
      if (isTruthy(input$EP_select)) {
        if (input$EP_select %in% colnames(project_data$val_conn_matrix)) {
          # Find previously selected values, and match to column names.
          matches <- which(project_data$val_conn_matrix[, input$EP_select] > 0)
          matches <- rownames(project_data$val_conn_matrix)[matches]
          updateSelectInput(
            session,
            "value_select",
            choices = project_data$values,
            selected = matches
          )
        }
      }
    }, ignoreNULL = FALSE)

    # Update Connections data frame on selection
    observeEvent(input$value_select, {
      if (isTruthy(input$EP_select)) {
        # Match selected items to columns in Connections data frame.
        selected_values <- input$value_select
        conn_rows <- rownames(project_data$val_conn_matrix)
        matches <- conn_rows %in% selected_values
        matches <- as.integer(matches)
        project_data$val_conn_matrix[, input$EP_select] <- matches
      }
    }, ignoreNULL = FALSE)

    # Connections Plot ---------------------------------------------------------
    observeEvent(project_data$val_conn_names, {
      if (isTruthy(project_data$values) &&
          isTruthy(project_data$EP_names)) {
        output$VPplot <- renderPlot({
          constructValuesConnectionsPlot(
            project_data$EP_names,
            project_data$values,
            project_data$val_conn_matrix
          )}, height = 500
        )
      }
    }, ignoreNULL = FALSE)

  })
}



























