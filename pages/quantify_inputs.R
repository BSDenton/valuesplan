# Page 8: Property Amounts -----------------------------------------------------

# UI ---------------------------------------------------------------------------

inputPropertyAmountsPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(
      tags$head(
        tags$style(HTML(PAGE_8_STYLE))
      ),
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h4(strong("Current Property Amounts:")),
          p("Enter the quantity for each element property that you wish to be
            modelled."),
          br(),
        )
      ),
      uiOutput(outputId = NS(id, 'property_amounts'))
    )
  )
}

# Server -----------------------------------------------------------------------

inputPropertyAmountsPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Page 8 Variables ---------------------------------------------------------
    ui_list <- reactiveValues(
      sliderinput = c()
    )

    # Add/remove rows in EP_amounts whenever the list of EP names changes ------
    observeEvent(project_data$EP_names, {
      if (!isTruthy(project_data$EP_amounts)) {
        project_data$EP_amounts <- data.frame(matrix(
          nrow = 0,
          ncol = 4
        ))
        colnames(project_data$EP_amounts) <- c(
          "Property", "Min", "Central", "Max"
        )
      }

      # Extract EP information from EP_amounts.
      EP_amounts_rows <- project_data$EP_amounts[[1]]
      # Find mismatches.
      missing_EPs <- setdiff(project_data$EP_names, EP_amounts_rows)
      extra_EPs <-  setdiff(EP_amounts_rows, project_data$EP_names)

      for (EP in missing_EPs) {
        # Add missing EP to EP_amounts data frame.
        if (!(EP %in% EP_amounts_rows)) {

          input_names <- sapply(project_data$fis$input, "[[", 1)
          input_index <- which(input_names == EP)
          input_range <- project_data$fis$input[[input_index]]$range

          project_data$EP_amounts[EP, ] <- c(
            EP,
            input_range[1],
            input_range[2] - (0.5* (input_range[2] - input_range[1])),
            input_range[2]
          )
        }
      }

      for (EP in extra_EPs) {
        # Remove extraneous values from Connections data frame.
        project_data$EP_amounts <- project_data$EP_amounts[
          !(row.names(project_data$EP_amounts) %in% EP),
          ,
          drop = FALSE
        ]
      }
    }, ignoreNULL = FALSE)

    # Generate UI elements whenever EP_amounts changes -------------------------
    observeEvent(project_data$EP_amounts, {
      # Store list of ids for UI elements about to be generated
      ui_list$sliderinput <- lapply(project_data$EP_names, function(EP) {
        element <- separateEP(EP)[1]
        property <- separateEP(EP)[2]
        chartr(" ","_",paste0(element, "___", property, "sliderinput"))
      })

      # Generate UI elements
      output$property_amounts <- renderUI({
        list(
          # Column Titles
          fluidRow(
            tags$div(
              class = "EP-amounts-display",
              column(2, HTML("<b>Element</b>")),
              column(2, HTML("<b>Property</b>")),
              column(4, HTML("<b>Quantity</b>"))
            )
          ),
          # Rows per EP
          lapply(project_data$EP_names, function(EP) {
            
            element <- separateEP(EP)[1]
            property <- separateEP(EP)[2]
            input_names <- sapply(project_data$fis$input, "[[", 1)
            input_index <- which(input_names == EP)
            
            if (!(identical(input_index, integer(0)))) {
              input_range <- project_data$fis$input[[input_index]]$range
              fluidRow(
                tags$div(
                  class = "EP-amounts-display",
                  column(2, HTML(element)),
                  column(2, HTML(property)),
                  column(4, noUiSliderInput(
                    NS(
                      paste0(parent_id, "-", id),
                      chartr(
                        " ",
                        "_",
                        paste0(element, "___", property, "sliderinput")
                      )
                    ),
                    label = NULL,
                    min = input_range[1],
                    max = input_range[2],
                    step = input_range[2] / POINT_N,
                    value = unlist(project_data$EP_amounts[EP, c("Min", "Max")])
                  )),
                )
              )
            }
          })
        )
      })

      # Generate Observers for newly-created inputs, which update EP_amounts df.
      lapply(ui_list$sliderinput, function(item) {
        observeEvent(input[[item]], {
          # Format id into standard EP format for matching.
          EP <- str_sub(item, end = -12)
          EP <- str_split_1(EP, "___")
          EP <- lapply(EP, function(x) {
            chartr("_", " ", x)
          })
          EP <- formatEPName(EP[1], EP[2])
          # Add value to EP_amounts data frame.
          project_data$EP_amounts[EP, c("Min", "Max")] <- input[[item]]
          project_data$EP_amounts[EP, "Central"] <-
            input[[item]][2] - (0.5* (input[[item]][2] - input[[item]][1]))
        })
      })
    })
  })
}
