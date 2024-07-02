# Code based on: mastering-shiny.org/scaling-modules.html

# Helpers ----------------------------------------------------------------------

nextPageButton <- function(id, i) {
  actionButton(
    NS(id, paste0("go_", i, "_", i + 1)),
    "Next",
    class = "btn btn-primary",
    style = "min-width: 100px; font-size: 14px"
  )
}

prevPageButton <- function(id, i) {
  actionButton(
    NS(id, paste0("go_", i, "_", i - 1)),
    "Back",
    class = "btn btn-primary",
    style = "min-width: 100px; font-size: 14px"
  )
}

wrapPage <- function(title, page, button_left = NULL, button_right = NULL) {
  tabPanel(
    title = title,
    style = PAGE_STYLE,
    fluidRow(
      column(12, page)
    ),
    tags$div(
      class = "breadcrumb",
      style = "width: 95%; margin: 20px auto; padding: 10px;",
      fluidRow(
        column(4, align ="center", button_left),
        column(4, align ="center"),
        column(4, align ="center", button_right)
      )
    )
  )
}

# UI ---------------------------------------------------------------------------

wizardUI <- function(id, pages, doneButton = NULL) {

  # Check pages is given as list.
  stopifnot(is.list(pages))

  # Wrap each page in wizard UI.
  n <- length(pages)
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    # First page only has Next; last page only Back + doneButton
    column_left <- if (i > 1) prevPageButton(id, i)
    column_right <- if (i < n) nextPageButton(id, i) else doneButton
    wrapped[[i]] <- wrapPage(
      paste0("page_", i),
      pages[[i]],
      column_left,
      column_right
    )
  }

  # Create tabsetPanel.
  wrapped$id <- NS(id, "wizard")
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)

}

# Server -----------------------------------------------------------------------

wizardServer <- function(id, n, project_data) {
  moduleServer(id, function(input, output, session) {

    # Helper function to generate an observeEvent for a navigation button.
    changePage <- function(from, to) {
      observeEvent(input[[paste0("go_", from, "_", to)]], {
        updateTabsetPanel(session, "wizard", selected = paste0("page_", to))
      })
    }

    # Generate observeEvents for wizard navigation buttons.
    ids <- seq_len(n)
    lapply(ids[-1], function(i) changePage(i, i - 1))
    lapply(ids[-n], function(i) changePage(i, i + 1))

  })
}

