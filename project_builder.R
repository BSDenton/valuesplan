# Project Builder --------------------------------------------------------------

source("pages/input_project_data.R")
source("pages/connect_element_properties.R")
source("pages/fuzzy_parameters_input.R")
source("pages/fuzzy_parameters_output.R")
source("pages/connect_values.R")
source("pages/define_input_output_relationships.R")
source("pages/generate_rulebase.R")


# UI ---------------------------------------------------------------------------

projectBuilderUI <- function(id) {

  pageslist <- list(
    inputDataPageUI(NS(id, "page1")),
    connectElementsPageUI(NS(id, "page2")),
    EPMFsPageUI(NS(id, "page3")),
    valueMFsPageUI(NS(id, "page4")),
    connectValuesPageUI(NS(id, "page5")),
    EPVRelationshipsPageUI(NS(id, "page6")),
    rulesbasePageUI(NS(id, "page7"))
  )

  fluidPage(
    NULL,
    header = tags$head(
      tags$style(
        type =
          "text/css",
        "body { overflow-y: scroll; }
          .navbar-brand { display:none; }"
      )
    ),
    tabPanel(
      "Project Builder",
      wizardUI(
        id = NS(id, "pb_wizard"),
        pages = pageslist
      )
    )
  )
}

# Server -----------------------------------------------------------------------

projectBuilderServer <- function(id, project_data, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Set up wizard layout
    wizardServer("pb_wizard", 7, project_data)

    # Page content
    inputDataPageServer("page1", id, project_data)
    connectElementsPageServer("page2", id, project_data)
    EPMFsPageServer("page3", id, project_data)
    valueMFsPageServer("page4", id, project_data)
    connectValuesPageServer("page5", id, project_data)
    EPVRelationshipsPageServer("page6", id, project_data)
    rulesbasePageServer("page7", id, project_data)

  })
}

