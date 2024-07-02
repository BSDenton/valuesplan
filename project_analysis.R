# Project Analysis -------------------------------------------------------------

source("pages/quantify_inputs.R")

## Version with final uncertainty implementation.
source("pages/view_analysis.R")

## Version with all uncertainty methods implemented for testing purposes.
# source("pages/view_analysis_testing_approaches.R")

# UI ---------------------------------------------------------------------------

projectAnalysisUI <- function(id) {

  pageslist <- list(
    inputPropertyAmountsPageUI(NS(id, "page1")),
    viewAnalysisPageUI(NS(id, "page2"))

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
      "ValuesPlan",
      wizardUI(
        id = NS(id, "pa_wizard"),
        pages = pageslist
      )
    )
  )
}


# Server -----------------------------------------------------------------------

projectAnalysisServer <- function(id, project_data, parent_session) {
  moduleServer(id, function(input, output, session) {

    # Set up wizard layout
    wizardServer("pa_wizard", 2, project_data)

    # Page content
    inputPropertyAmountsPageServer("page1", id, project_data)
    viewAnalysisPageServer("page2", id, project_data)

  })
}

