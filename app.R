# install.packages(
#   c(
#     "shiny", "shinythemes", "shinyWidgets", "shinyBS", "shinyjs",
#     "ggplot2", "dplyr", "DT", "tibble", "reshape2", "stringr", "FuzzyR",
#     "sets"
#   ),
#   repos = "http://cran.us.r-project.org"
# )

library(shiny, quietly = TRUE, warn.conflicts = FALSE)
library(shinythemes, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)
library(shinyBS, warn.conflicts = FALSE)  # collapsible panels
library(shinyjs, quietly = TRUE, warn.conflicts = FALSE)  # hidden()
library(sets, warn.conflicts = FALSE)  # interval_union
library(ggplot2, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(tibble, warn.conflicts = FALSE)  # enframe()
library(reshape2, warn.conflicts = FALSE)  # melt()
library(FuzzyR, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)  # str_split(), str_sub(), str_replace_all()

# # Libraries used for experimental testing
# library(qrng, quietly = TRUE, warn.conflicts = FALSE) # halton sequence generator
# library(moments)  # skewness

source("globals.R")
source("helpers.R")
source("wizard.R")
source("homepage.R")
source("project_builder.R")
source("project_analysis.R")

valuesPlanApp <- function(...) {

  # Variables ------------------------------------------------------------------

  pageslist = list(
    list("hp", homePageUI("hp")),
    list("pb", projectBuilderUI("pb")),
    list("pa", projectAnalysisUI("pa"))
  )

  project_data <- reactiveValues(
    type = "valuesplan-project",
    title = NULL,
    elements = c(),
    properties = c(),
    values = c(),
    # EP_conn_matrix is a data-frame that holds Elements in its rows and
    # Properties in its column. A zero value indicates no connection; a non-zero
    # value indicates that the Element has that Property.
    EP_conn_matrix = c(),
    # EP names are stored as strings: "Element&Property"
    EP_names = c(),
    EP_amounts = c(),
    # val_conn_matrix is a data-frame that holds Values in its rows and
    # Element-Properties in its column. A zero value indicates no connection; a
    # non-zero value indicates that the Property affects that Value.
    val_conn_matrix = c(),
    # EPV names are stored as strings: "EP@Value"
    val_conn_names = list(),
    # EPV_relationships is a named list. Names represent the Element Property
    # and Value involved, while each list contains vectors describing the
    # relationship between them for each linguistic term.
    EPV_relationships = list(),
    # FIS created by FuzzyR package
    fis = newfis("")
  )

  # UI -------------------------------------------------------------------------
  ui <- navbarPage(
    NULL,
    theme = shinytheme("flatly"),

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
      useShinyjs(),
      # Additional Navbar Elements ---------------------------------------------
      # Return Home
      shinyjs::hidden(
        actionButton(
          "return_home_button",
          "Return Home",
          class = "btn btn-primary",
          style = "
          position: absolute;
            top: 0px;
            right: 15px;
            z-index: 10000;
            padding: 19px 30px;
            border: none;
            border-radius: 0px;
          "
        )
      ),
      # Save Project
      shinyjs::hidden(
        downloadLink(
          "save_button",
          "Save Project",
          style = "
          position: absolute;
          top: 0px;
          right: 180px;
          z-index: 10000;
          padding: 19px 30px;
          color: white;"
        )
      ),

      # Page Content -----------------------------------------------------------
      generateNavigationUI(pageslist)
    )

  )

  # Server ---------------------------------------------------------------------
  server <- function(input, output, session) {

    # Increase upload allowance
    options(shiny.maxRequestSize=30*1024^2)

    # Additional Navbar Elements -----------------------------------------------
    # Return Home Button
    observeEvent(input$return_home_button, {
      updateTabsetPanel(
        session,
        "nav_wizard",
        selected = paste0("page_hp")
      )
    })

    # Save Project Button
    output$save_button <- downloadHandler(
      filename = function() {
        if (!(isTruthy(project_data$title))) {
          title <- "project"
        } else {
          # replace special characters in title.
          title <- str_replace_all(project_data$title, "[^[:alnum:]]", "_")
        }
        paste(title, "-", Sys.Date(), ".rds", sep="")
      },
      content = function(file) {
        saveRDS(project_data, file)
      }
    )

    # Page content -------------------------------------------------------------
    homePageServer("hp", project_data, session)
    projectBuilderServer("pb", project_data, session)
    projectAnalysisServer("pa", project_data, session)

    observeEvent(input$nav_wizard, {
      if (input$nav_wizard == "page_hp") {
        shinyjs::hide("save_button")
        shinyjs::hide("return_home_button")
      } else {
        shinyjs::show("save_button")
        shinyjs::show("return_home_button")
      }
    })

  }

  shinyApp(ui, server, ...)


}

valuesPlanApp()
