# Page 2: Element-Property Connections -----------------------------------------

constructElementConnectionsPlot <- function(elements, properties, connections) {
# Constructs an image that shows the current connections between elements and
# properties, as users input them.

  if (!isTruthy(elements) || !isTruthy(properties)) {
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

  # Add elements to table, row by row.
  for (index in 1:length(elements)) {
    label_df[nrow(label_df) + 1,] <- list(index, "Elements", elements[index])
  }

  # Add properties to table, row by row.
  for (index in 1:length(properties)) {
    label_df[nrow(label_df) + 1,] <- list(index, "Properties", properties[index])
  }

  # Initialize new connections data frame (formatted for geom_segment)
  conn_df <- setNames(data.frame(
    matrix(
      ncol = 5,
      nrow = 0
    )),
    c("element", "x1", "element_index", "x2", "property_index")
  )

  for (row_name in rownames(connections)) {
    for (col_name in colnames(connections)) {
      if (connections[row_name, col_name] > 0) {
        # Find y indices for elements/properties, based on label_df
        element_index <- label_df$position[label_df$name == row_name]
        property_index <- label_df$position[label_df$name == col_name]
        # TBA: Account for differences in label size
        conn_df[nrow(conn_df) + 1,] <- list(
          row_name, 1, element_index, 2, property_index
        )
      }
    }
  }

  # Plot Graph
  ggplot() +
    geom_segment(
      data = conn_df,
      mapping = aes(
        x = x1,
        y = element_index,
        xend = x2,
        yend = property_index
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
    expand_limits(y = c(0, max(length(elements), length(properties)) + 1)) +
    scale_fill_manual(values = c(ELEMENT_COLOR, PROPERTY_COLOR))

}


# UI ---------------------------------------------------------------------------

connectElementsPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(

      # Input Connections ------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Connecting Elements and Properties:")),
          p("For each element in the system, select all its properties that you 
          are interested in investigating. "),
          p("For example, if you want to investigate how Species Richness 
            affects the Aesthetic Enjoyment Value of a Vegetation Element, you 
            would need to select the ‘Vegetation Element’ and then the ‘Species 
            Richness’ property to connect them."),
          br(), hr(),
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
              selectInput(
                NS(id, "property_select"),
                "Add Properties for this Element",
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
            plotOutput(NS(id, "EPplot"), width = "80%", height = "100%")
          )
        )
      )
    )
  )
}


# Server -----------------------------------------------------------------------
connectElementsPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Update Connections data frame, when elements/properties change -----------
    observeEvent(c(project_data$elements, project_data$properties), {

      EP_exists <- isTruthy(project_data$elements) &&
                   isTruthy(project_data$properties)

      # Remove Connections data frame if elements or properties don't exist.
      if (!(is.null(project_data$EP_conn_matrix)) && !(EP_exists)) {
        project_data$EP_conn_matrix <- NULL
      }

      # Set-up Connections data frame, when both elements and properties exist.
      else if (is.null(project_data$EP_conn_matrix) && EP_exists) {
        project_data$EP_conn_matrix <- data.frame(matrix(
          nrow = length(project_data$elements),
          ncol = length(project_data$properties)
        ))
        rownames(project_data$EP_conn_matrix) <- project_data$elements
        colnames(project_data$EP_conn_matrix) <- project_data$properties
        project_data$EP_conn_matrix[,] <- 0

      }

      # Update Connections data frame, if all exist.
      else if (!(is.null(project_data$EP_conn_matrix)) && EP_exists) {
        # Extract column and row names from connections data frame.
        conn_elements <- rownames(project_data$EP_conn_matrix)
        conn_properties <- colnames(project_data$EP_conn_matrix)
        # Find mismatches.
        missing_elements <- setdiff(project_data$elements, conn_elements)
        extra_elements <-  setdiff(conn_elements, project_data$elements)
        missing_properties <- setdiff(project_data$properties, conn_properties)
        extra_properties <- setdiff(conn_properties, project_data$properties)

        for (element in missing_elements) {
          # Add missing elements to Connections data frame.
          if (!(element %in% conn_elements)) {
            project_data$EP_conn_matrix[element,] <- 0
          }
        }

        for (element in extra_elements) {
          # Remove extraneous elements from Connections data frame.
          project_data$EP_conn_matrix <- project_data$EP_conn_matrix[
            !(row.names(project_data$EP_conn_matrix) %in% element),
            , drop = FALSE
          ]
        }

        for (property in missing_properties) {
          # Add missing properties to Connections data frame.
          if (!(property %in% conn_properties)) {
            project_data$EP_conn_matrix[,property] <- 0
          }
        }

        for (property in extra_properties) {
          # Remove extraneous properties from Connections data frame.
          project_data$EP_conn_matrix <- project_data$EP_conn_matrix[,
            !(colnames(project_data$EP_conn_matrix) %in% property),
            drop = FALSE
          ]
        }
      }
    })

    # Update Element-Property Names vector, whenever Connections changes -------
    observeEvent(project_data$EP_conn_matrix, {
      if (!isTruthy(project_data$EP_conn_matrix)) {
        project_data$EP_names <- c()
      } else {
        curr_names <- c()
        for(i in 1:nrow(project_data$EP_conn_matrix)) {
          row <- project_data$EP_conn_matrix[i, ,drop = FALSE]
          matches <- which(row > 0)
          matches <- colnames(project_data$EP_conn_matrix)[matches]
          for (property in matches) {
            curr_names <- append(
              curr_names,
              formatEPName(rownames(row), property)
            )
          }
        }
        project_data$EP_names <- curr_names
      }
    }, ignoreNULL = FALSE)

    # Input Element-Property Connections ---------------------------------------
    # Update Elements select options
    observeEvent(project_data$elements, {
      if (!isTruthy(project_data$elements)) {
        ele_choice <- ""
        ele_selected <- ""
      } else {
        ele_choice <- project_data$elements
        ele_selected <- project_data$elements[1]
      }
      updateSelectInput(
        session,
        "element_select",
        choices = ele_choice,
        selected = ele_selected
      )
    }, ignoreNULL = FALSE)

    # Update Properties select options
    observeEvent(c(input$element_select, project_data$properties), {
      # Find previously selected properties, and match to column names.
      matches <- which(project_data$EP_conn_matrix[input$element_select,] > 0)
      matches <- colnames(project_data$EP_conn_matrix)[matches]

      updateSelectInput(
        session,
        "property_select",
        choices = project_data$properties,
        selected = matches
      )
    }, ignoreNULL = FALSE)

    # Update Connections data frame on selection
    observeEvent(input$property_select, {
      if(input$element_select != "") {
        # Match selected items to columns in Connections data frame.
        selected_properties <- input$property_select
        conn_columns <- colnames(project_data$EP_conn_matrix)
        matches <- conn_columns %in% selected_properties
        matches <- as.integer(matches)
        project_data$EP_conn_matrix[input$element_select,] <- matches
      }
    }, ignoreNULL = FALSE)

    # Connections Plot ---------------------------------------------------------
    observeEvent(project_data$EP_conn_matrix, {
      if (length(project_data$elements) > 0 &&
          length(project_data$properties > 0)) {
        output$EPplot <- renderPlot({
          constructElementConnectionsPlot(
            project_data$elements,
            project_data$properties,
            project_data$EP_conn_matrix
          )}, height = 500
        )
      }
    }, ignoreNULL = FALSE)

  })
}
