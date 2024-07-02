# Page 1: User Input -----------------------------------------------------------

# UI ---------------------------------------------------------------------------
inputDataPageUI <- function(id) {
  tags$div(
    style = PAGE_STYLE,
    fluidPage(

      # Project title ----------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          h3(strong("ValuesPlan Project Creation:")),
          p("In this section, you can define the components of your management 
          system. Simply follow the instructions on each page to input information 
            about your system. You can save your progress at any point using the 
            button at the top of the page."),
          br(),
          h4(strong("Project Title:")),
          # Display current title to user.
          tags$div(
            style = "margin: 10px 0;",
            htmlOutput(NS(id, "title_display"))
          ),
          # Accept user input to change title.
          textInput(
            NS(id, "input_title"),
            NULL,
            placeholder = "Enter name..."
          )
        )
      ),

      # Elements ---------------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          # Panel Title and Description
          h4(strong("Add Project Elements:")),
          p("The elements of a system refer to its resources, both natural 
            and human-built."),
          p("Examples: 'Vegetation Element', 'Seahorse Species', 'Water Body'."),
          br(),
          # Accept new Element input
          fluidRow(
            column(5, textInput(
              NS(id, "input_element"),
              NULL,
              placeholder = "Add new element..."
            )),
            column(3, actionButton(
              NS(id, "add_element"),
              "Add Element",
              class = "btn btn-primary"
            ))
          ),
          shinyjs::hidden(
            tags$div(
              id = NS(id, "element_container"),
              # Show current elements
              DT::DTOutput(NS(id, "show_elements")),
              # Allow deletion of selected row.
              br(),
              p("You can select the elements above and click this button to 
                remove them."),
              actionButton(
                NS(id, "remove_element"),
                "Remove Selected Element",
                class = "btn btn-danger"
              )
            )
          )
        )
      ),

      # Properties -------------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          # Panel Title and Description
          h4(strong("Add Project Properties:")),
          p("The key properties of the system’s elements are used to quantify 
            the state of each element."),
          p("Examples: 'Population Size', 'Height', 'Rarity'"),
          p("Note: Add all relevant element properties here, even if the 
            properties are not shared by all the named elements."),
          br(),
          # Accept new Property input
          fluidRow(
            column(5, textInput(
              NS(id, "input_property"),
              NULL,
              placeholder = "Add new property..."
            )),
            column(3, actionButton(
              NS(id, "add_property"),
              "Add Property",
              class = "btn btn-primary"
            ))
          ),
          shinyjs::hidden(
            tags$div(
              id = NS(id, "property_container"),
              # Show current elements
              DT::DTOutput(NS(id, "show_properties")),
              # Allow deletion of selected row.
              br(),
              p("You can select the properties above and click this button to 
                remove them."),
              actionButton(
                NS(id, "remove_property"),
                "Remove Selected Property",
                class = "btn btn-danger"
              )
            )
          )
        )
      ),

      # Values -----------------------------------------------------------------
      wellPanel(
        tags$div(
          style = WELL_STYLE,
          # Panel Title and Description
          h4(strong("Add Project Values:")),
          p("The human values of a system refer to all the end-state values 
            which define human well-being and survival."),
          p("Examples: 'Recreational Enjoyment', 'Cultural/heritage 
            Fulfillment', 'Meaningful Occupation'"),
          br(),
          # Accept new Value input
          fluidRow(
            column(5, textInput(
              NS(id, "input_value"),
              NULL,
              placeholder = "Add new value..."
            )),
            column(3, actionButton(
              NS(id, "add_value"),
              "Add Value",
              class = "btn btn-primary"
            ))
          ),
          shinyjs::hidden(
            tags$div(
              id = NS(id, "value_container"),
              # Show current elements
              DT::DTOutput(NS(id, "show_values")),
              # Allow deletion of selected row.
              br(),
              p("You can select the values above and click this button to 
                remove them."),
              actionButton(
                NS(id, "remove_value"),
                "Remove Selected Value",
                class = "btn btn-danger"
              )
            )
          )
        )
      ),
    )
  )
}

# Server -----------------------------------------------------------------------
inputDataPageServer <- function(id, parent_id, project_data) {
  moduleServer(id, function(input, output, session) {

    # Project title ------------------------------------------------------------
    # Update project data when new title is input.
    observeEvent(input$input_title, {
      project_data$title <- input$input_title
      project_data$fis$name <- input$input_title
    })

    # Display current title to user.
    output$title_display <- renderPrint(
      if (!(isTruthy(project_data$title))) {
        HTML(
          "Please enter a project name below."
        )
      } else {
        HTML(
          paste0("This project is currently titled: ", project_data$title, ".")
        )
      }
    )

    # Elements -----------------------------------------------------------------

    # Update project data when new element is input.
    observeEvent(input$add_element, {
      element <- input$input_element
      if (element > 0 &&
          !(element %in% project_data$elements) &&
          !(element %in% project_data$properties)
          ) {
        project_data$elements <- c(
          isolate(project_data$elements),
          isolate(element)
        )
        updateTextInput(session, "input_element", NULL, "" )
        shinyjs::show("element_container")
      }
    })

    # Show users the currently stored elements.
    output$show_elements <- DT::renderDT(
      {
        if (length(project_data$elements > 0)) {
          df <- enframe(project_data$elements)['value']
          colnames(df) <- "Elements"
          return(df)
        }
      },
      options = list(dom="t", paging = FALSE),
      rownames = FALSE,
      selection = 'single'
    )

    # Remove element upon user button input.
    observeEvent(input$remove_element, {
      selected_element <- project_data$elements[
        input$show_elements_rows_selected,
        drop = FALSE
      ]
      project_data$elements <- project_data$elements[
        !(project_data$elements %in% selected_element),
        drop = FALSE
      ]
      if (length(project_data$elements) == 0) {
        shinyjs::hide("element_container")
      }
    })

    # Properties ---------------------------------------------------------------

    # Update project data when new property is input.
    observeEvent(input$add_property, {
      property <- input$input_property
      if (property > 0
          && !(property %in% project_data$properties)
          && !(property %in% project_data$elements)
          ) {
        project_data$properties <- c(
          isolate(project_data$properties),
          isolate(property)
        )
        updateTextInput(session, "input_property", NULL, "" )
        shinyjs::show("property_container")
      }
    })
    
    # Show users the currently stored elements.
    output$show_properties <- DT::renderDT(
      {
        if (length(project_data$properties > 0)) {
          df <- enframe(project_data$properties)['value']
          colnames(df) <- "Properties"
          return(df)
        }
      },
      options = list(dom="t", paging = FALSE),
      rownames = FALSE,
      selection = 'single'
    )

    # Remove property upon user button input.
    observeEvent(input$remove_property, {
      selected_property <- project_data$properties[
        input$show_properties_rows_selected,
        drop = FALSE
      ]
      project_data$properties <- project_data$properties[
        !(project_data$properties %in% selected_property),
        drop = FALSE
      ]
      if (length(project_data$properties) == 0) {
        shinyjs::hide("property_container")
      }
    })

    # Values -------------------------------------------------------------------

    # Update project data when new value is input.
    observeEvent(input$add_value, {
      value <- input$input_value
      if (value > 0 && !(value %in% project_data$values)) {
        project_data$values <- c(
          isolate(project_data$values),
          isolate(value)
        )
        updateTextInput(session, "input_value", NULL, "" )
        shinyjs::show("value_container")
      }
    })
    
    # Show users the currently stored values.
    output$show_values <- DT::renderDT(
      {
        if (length(project_data$values > 0)) {
          df <- enframe(project_data$values)['value']
          colnames(df) <- "Values"
          return(df)
        }
      },
      options = list(dom="t", paging = FALSE),
      rownames = FALSE,
      selection = 'single'
    )

    # Remove value upon user button input.
    observeEvent(input$remove_value, {
      selected_value <- project_data$values[
        input$show_values_rows_selected,
        drop = FALSE
      ]
      project_data$values <- project_data$values[
        !(project_data$values %in% selected_value),
        drop = FALSE
      ]
      if (length(project_data$values) == 0) {
        shinyjs::hide("value_container")
      }
    })
  })
}
