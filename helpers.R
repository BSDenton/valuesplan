# UI Functions -----------------------------------------------------------------

wrapNavPage <- function(title, page) {
  tabPanel(
    title = title,
    style = PAGE_STYLE,
    fluidRow(
      column(12, page)
    )
  )
}

generateNavigationUI <- function(pages) {
  # Check pages is given as list.
  stopifnot(is.list(pages))

  # Wrap each page in wizard UI.
  n <- length(pages)
  wrapped <- vector("list", n)
  for (i in seq_along(pages)) {
    wrapped[[i]] <- wrapNavPage(
      paste0("page_", pages[[i]][1]),
      pages[[i]][2]
    )
  }

  # Create tabsetPanel.
  wrapped$id <- "nav_wizard"
  wrapped$type <- "hidden"
  do.call("tabsetPanel", wrapped)
}

# String Functions -------------------------------------------------------------

formatEPName <- function(element, property) {
  return(paste0(element, "&", property))
}

separateEP <- function(EP) {
  return(str_split_1(EP, "&"))
}

formatEPString <- function(EP) {
  return(paste0(separateEP(EP), collapse = ": "))
}

separateEPV <- function(EPV) {
  return(str_split_1(EPV, "@"))
}

generateEPVName <- function(EPV) {
  return(paste0(EPV[1], "@", EPV[2]))
}

# Membership Functions ---------------------------------------------------------

# Helper function for finding membership values over x for an Interval MF,
# accounting for singleton edge-cases.
intervalmf <- function (mf.params) {
  if (length(mf.params) != 2) {
    stop("improper parameters for interval membership function")
  }
  a <- mf.params[1]
  b <- mf.params[2]
  saved_index <- NULL
  saved_value <- a
  intervalmf <- function(x) {
    if (a == b) {  # If entered value was a singleton...
      if (length(x) == 1) {  # If only checking one value...
        if (x == saved_value) {
          return(1)
        }
        else {
          return(0)
        }
      } else {  # Calculate closest x value to input crisp value.
        saved_index <- which(abs(x-a)==min(abs(x-a)))
        saved_value <- x[saved_index]
        memberships <- rep(0, length(x))
        memberships[saved_index] <- 1
        return(memberships)
      }
    } else {
      return(as.integer(x >= a & x <= b))
    }

  }
}

interval.fuzzification <- function (x, mf.params) {
  if (length(mf.params) != 2) {
    stop("improper parameters for interval fuzzification")
  }
  genmf("intervalmf", mf.params)

}

# IAWA -------------------------------------------------------------------------

# Equality tolerance
epsilon <- 1e-10 # set this as a global setting
`%>=%` <- function(x, y) (x + epsilon > y)

iawa <- function(set, level_count = 9) {
  # Accepts dataframe containing X and Y values, representing a type-1 Fuzzy
  # set. Transforms set into representative intervals through alpha-cuts and
  # IWA.

  # Confirm only one set has been entered
  set_names <- (distinct(set["Value"]))
  if (nrow(set_names) > 1) {
    print("Too many sets entered to IAWA function.")
    return(NULL)
  }
  
  # Calculate alpha levels
  levels <- seq(0, 1, length.out = level_count)
  cuts <- matrix(
    ncol = length(set[["X"]]),
    nrow = length(levels)
  )
  colnames(cuts) <- set[["X"]]
  rownames(cuts) <- levels
  alpha_sum <- sum(levels)

  # Calculate alpha cuts
  for (i in 1:length(levels)) {
    cuts[i, ] <- as.numeric(set["Y"] %>=% levels[i])
  }

  # Calculate intervals
  intervals <- data.frame(matrix(
    ncol = 3,
    nrow = 0
  ))
  colnames(intervals) <- c("level", "lower", "upper")

  row_index <- 1
  for (level in rownames(cuts)) {
    # Isolate results for alpha cut
    curr_cut <- as.numeric(cuts[level, ])
    # Split into intervals of concurrent 0s and 1s.
    indices <- split(set[["X"]], cumsum(c(1, diff(curr_cut)!= 0)))
    # Find x indices for concurrent 1s, to isolate intervals.
    interval_indices <- names(
      Filter(
        \(row) sum(row) > 0,
        split(curr_cut, cumsum(c(1, diff(curr_cut)!= 0)))
      )
    )
    curr_intervals <- indices[interval_indices]
    # Add found intervals to dataframe.
    for (interval in curr_intervals) {
      intervals[row_index, ] <- c(
        as.numeric(level),
        as.numeric(interval[1]),
        as.numeric(tail(interval, n = 1))
      )
      row_index = row_index + 1
    }
  }

  if (nrow(intervals) == 0) {
    return(NULL)
  }

  # Find N - number of continuous intervals
  levels <- unique(intervals[["level"]])

  iawa_n <- data.frame(matrix(
    ncol = 2,
    nrow = length(levels)
  ))
  colnames(iawa_n) <- c("level", "interval_count")

  iawa_n["level"] <- levels
  iawa_n["interval_count"] <- as.vector(table(intervals["level"]))

  for (i in nrow(iawa_n):1) {
    if (iawa_n[i, "interval_count"] == 0) {
      levels <- levels[-i]
      iawa_n <- iawa_n[-i,]
    } else {
      break
    }
  }

  # Find all interval combinations
  total_combn <- prod(iawa_n["interval_count"])

  combn_table <- data.frame(matrix(
    ncol = length(levels),
    nrow = total_combn
  ))

  if (total_combn > 1) {
    assign("rep_tally", total_combn, envir = globalenv())
    combn_table <- apply(iawa_n, 1, function(x) {
      assign("rep_tally", rep_tally / x[[2]], envir = globalenv())
      rep(1:x[[2]], each = rep_tally, length.out = total_combn)
    })
    colnames(combn_table) <- levels

  } else {
    combn_table <- matrix(
      rep(1, length(levels)),
      ncol = length(levels)
    )
    colnames(combn_table) <- levels
  }

  # Calculate IAWAs
  iawa_intervals <- data.frame(matrix(
    ncol = 2,
    nrow = nrow(combn_table)
  ))
  colnames(iawa_intervals) <- c("lower", "upper")

  interval_indices <- data.frame(matrix(
    ncol = 2,
    nrow = length(levels)
  ))
  colnames(interval_indices) <- c("Level", "StartIndex")

  interval_indices["Level"] <- levels
  interval_indices["StartIndex"] <- unlist(lapply(levels, function(x) {
    which(intervals["level"] == x)[1]
  }))

  for (i in 1:nrow(combn_table)) {
    # Find intervals for current combination
    curr_intervals <- list()
    for (j in 1:length(levels)) {
      curr_start_index <- interval_indices[j, "StartIndex"] - 1
      curr_interval_index <- combn_table[[i, j]]
      curr_interval <- intervals[curr_start_index + curr_interval_index, ]
      curr_intervals <- append(curr_intervals, list(curr_interval))
    }
    curr_intervals <- bind_rows(curr_intervals)

    lower <- (curr_intervals[, 1] / alpha_sum) * curr_intervals[, 2]
    lower <- sum(lower)
    iawa_intervals[i, "lower"] <- round(lower, 2)

    upper <- (curr_intervals[, 1] / alpha_sum) * curr_intervals[, 3]
    upper <- sum(upper)
    iawa_intervals[i, "upper"] <- round(upper, 2)
  }
  
  iawa_interval_sets <-
    apply(iawa_intervals, 1, function(row) {
      row
    })

  iawa_result <- interval_union(iawa_interval_sets)
  
  return(iawa_result)

}

# MF Parameters UI Functions ---------------------------------------------------

setupMFContainer <- function(id, ui_list, curr_params, input, output,
                             project_data) {

  # Generate UI elements for overall Term
  output$edit_params_container <- generateMFContainer(
    id,
    ui_list,
    curr_params
  )

  # Generate UI elements for MF Type Parameters
  lapply(seq_along(ui_list$mf_container_ids), function(i) {
    container_id <- ui_list$mf_container_ids[[i]]
    element_id <- paste0(container_id, "-type_specific_params-", i)
    output[[element_id]] <- renderUI({
      # First instance is isolated from reactive environment; a later
      # observeEvent will manage changes.
      isolate(
        generateMFParams(
          id,
          ui_list,
          curr_params,
          input[[paste0(container_id, "-typeinput")]],
          i,
          type_changed = FALSE
        )
      )
    })
  })

  # Modify UI upon changes to Type input
  lapply(seq_along(ui_list$mf_container_ids), function(i) {
    input_id <- paste0(
      ui_list$mf_container_ids[[i]],
      "-typeinput"
    )
    output_id <- paste0(
      ui_list$mf_container_ids[[i]],
      "-type_specific_params-",
      i
    )
    observeEvent(input[[input_id]], {
      output[[output_id]] <- renderUI({
        # Extract up-to-date FIS information
        EP <- formatEPName(input$element_select, input$property_select)
        fis_input_names <- sapply(project_data$fis$input, "[[", 1)
        curr_params <- project_data$fis$input[[
          which(fis_input_names == EP)
        ]]
        is_diff_type <- !(curr_params$mf[[i]]$type == input[[input_id]])

        # Generate UI
        generateMFParams(
          id,
          ui_list,
          curr_params,
          input[[input_id]],
          i,
          is_diff_type
        )
      })
    })
  })

  # 'Apply Changes' observe events
  observeApplyChangesButton(
    id,
    ui_list,
    curr_params,
    input,
    output,
    project_data
  )
}

generateMFContainer <- function(id, ui_list, curr_params) {

  # Generate MF Parameters containers
   renderUI({
    lapply(seq_along(curr_params$mf), function (i) {
      bsCollapsePanel(
        title = curr_params$mf[[i]]$name,
        # Shared elements inside collapsible panel
        # Name Input
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Name:"
          ),
          column(7,
            textInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-nameinput")),
              NULL,
              curr_params$mf[[i]]$name
            )
          )
        ),
        # MF Type Input
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Function Type:"
          ),
          column(7,
            selectInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-typeinput")),
              NULL,
              choices = TYPE_CHOICES,
              selected = curr_params$mf[[i]]$type
            )
          )
        ),
        # Container for Parameters, based on MF Type selected
        uiOutput(NS(
          id,
          paste0(ui_list$mf_container_ids[[i]], "-type_specific_params-", i)
        )),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(12,
            actionButton(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-editbutton")),
              "Apply Changes",
              class = "btn btn-primary btn-sm",
              style = "width: 100%;"
            )
          )
        )
      )
    })
  })
}

observeApplyChangesButton <- function(id, ui_list, curr_params, input, output,
                               project_data) {
  # Save data and refresh UI, when an 'Apply Changes' button is pressed.
  lapply(ui_list$mf_container_ids, function(container) {
    observeEvent(input[[paste0(container, "-editbutton")]], {

      # Identify position of current EP in FIS
      EP <- formatEPName(input$element_select, input$property_select)
      fis_input_names <- sapply(project_data$fis$input, "[[", 1)
      curr_EP_index <- which(fis_input_names == EP)

      # Identify MF position in Input data
      mf_index <- str_split_1(container, "_")
      mf_index <- as.numeric(mf_index[length(mf_index)])

      # Update Name
      old_name <- project_data$fis$input[[curr_EP_index]]$mf[[mf_index]]$name
      new_name <- input[[paste0(container, "-nameinput")]]
      project_data$fis$input[[curr_EP_index]]$mf[[mf_index]]$name <-
         input[[paste0(container, "-nameinput")]]

      # Update Relationships if MF name has changed
      if (old_name != new_name) {
        EP_matches <- sapply(
          names(project_data$EPV_relationships),
          startsWith,
          EP
        )

        if (isTruthy(EP_matches)) {
          EP_matches <- which(EP_matches)
          for (EP_index in EP_matches) {

            mf_matches <- which(
              sapply(
                project_data$EPV_relationships[[EP_index]], "[[", 3
              ) == old_name
            )
            lapply(mf_matches, function (i) {
              project_data$EPV_relationships[[EP_index]][[i]][[3]] <- new_name
              project_data$EPV_relationships[[EP_index]][[i]][[1]] <-
                str_replace(
                  project_data$EPV_relationships[[EP_index]][[i]][[1]],
                  old_name,
                  new_name
              )
            })
          }
        }
      }

      # Update Type
      project_data$fis$input[[curr_EP_index]]$mf[[mf_index]]$type <-
        input[[paste0(container, "-typeinput")]]

      # Update Parameters
      params_count <- length(
        TYPE_DEFAULT_PARAMETERS[[input[[paste0(container, "-typeinput")]]]]
      )

      new_params <- sapply(1:params_count, function(i) {
        input[[paste0(container, "-params-", i)]]
      })

      project_data$fis$input[[curr_EP_index]]$mf[[mf_index]]$params <-
        new_params

      # Refresh UI
      output$edit_params_container <- generateMFContainer(
        id,
        ui_list,
        project_data$fis$input[[which(fis_input_names == EP)]]
      )
    })
  })
}

generateMFParams <- function(id, ui_list, curr_params, type, i,
                             type_changed = TRUE) {

  if (!(isTruthy(type))) {
    return(NULL)
  }

  # Gaussian MF
  if (type == "gaussmf") {
    if (type_changed) {
      params <- TYPE_DEFAULT_PARAMETERS[[type]]
    } else {
      params <- curr_params$mf[[i]]$params
    }
    return(
      list(
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Spread:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-1")),
              NULL,
              params[[1]],
              min = 0
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Peak:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-2")),
              NULL,
              params[[2]]
            )
          )
        )
      )
    )
  }

  # Triangular MF
  if (type == "trimf") {
    if (type_changed) {
      params <- TYPE_DEFAULT_PARAMETERS[[type]]
    } else {
      params <- curr_params$mf[[i]]$params
    }
    return(
      list(
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Left Foot:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-1")),
              NULL,
              params[[1]]
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Peak:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-2")),
              NULL,
              params[[2]]
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Right Foot:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-3")),
              NULL,
              params[[3]]
            )
          )
        )
      )
    )
  }

  # Trapezoidal MF
  if (type == "trapmf") {
    if (type_changed) {
      params <- TYPE_DEFAULT_PARAMETERS[[type]]
    } else {
      params <- curr_params$mf[[i]]$params
    }
    return(
      list(
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Left Foot:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-1")),
              NULL,
              params[[1]]
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Left Shoulder:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-2")),
              NULL,
              params[[2]]
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Right Shoulder:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-3")),
              NULL,
              params[[3]]
            )
          )
        ),
        fluidRow(
          style = "margin-bottom: 10px;",
          column(5,
            "Right Foot:"
          ),
          column(7,
            numericInput(
              NS(id, paste0(ui_list$mf_container_ids[[i]], "-params-4")),
              NULL,
              params[[4]]
            )
          )
        )
      )
    )
  }

}

# Plotting / Graphs ------------------------------------------------------------

# Construct a graph showing the membership functions for all terms in an input
# or output variable
constructMFPlot <- function(name, fis, type) {

  if (!(type %in% c("input", "output"))) {
    return(NULL)
  }


  # Isolate FIS data for selected input
  if (type == "input") {
    input_index <- which(sapply(fis$input, "[[", 1) == name)
    variable_data <- fis$input[[input_index, drop = FALSE]]
  } else {
    output_index <- which(sapply(fis$output, "[[", 1) == name)
    variable_data <- fis$output[[output_index, drop = FALSE]]
  }

  if (length(variable_data$mf) < 1) {
    return(NULL)
  }

  # Create empty data frame to store MF information.
  mf_data <- data.frame(matrix(
    nrow = POINT_N,
    ncol = length(variable_data$mf) + 1
  ))

  # Generate x values over variable range.
  x_values <- seq(
    variable_data$range[1],
    variable_data$range[2],
    length = POINT_N
  )
  colnames(mf_data)[1] <- "X"
  mf_data[, 1] <- x_values
  # Generate y (membership) values
  for (mf_index in 1:length(variable_data$mf)) {
    generated_mf <- genmf(
      variable_data$mf[[mf_index]]$type,
      variable_data$mf[[mf_index]]$params
    )
    y_values <- evalmf(x_values, generated_mf)
    colnames(mf_data)[mf_index + 1] <- variable_data$mf[[mf_index]]$name
    mf_data[, mf_index + 1] <- y_values
  }

  # Format data for multi-line plotting
  mf_data <- reshape2::melt(mf_data, id = "X")

  # Plot graph
  ggplot(mf_data) +
    labs(
      title = gsub("&", ": ", name),
      x = "Value",
      y = "Degree of Membership"

    ) +
    geom_line(
      aes(x = X, y = value, colour = variable)
    ) +
    theme(
      plot.margin = margin(10, 20, 10, 20),
      legend.position = "none",
      plot.title = element_text(
        margin = margin(b = 15),
        hjust = 0.5,
        size = 18
      ),
      axis.line = element_line(colour = "grey"),
      axis.title = element_text(size = 16),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    ) +
    # Force y axis to start at origin
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    # Force x axis to cover variable range
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(variable_data$range[1], variable_data$range[2])
    )
}

constructSensitivityAnalysisPlot <- function(property, values, project_data) {

  if (!(isTruthy(values))) {
    return(
      NULL
    )
  }

  fis_input_names <- sapply(project_data$fis$input, "[[", 1)
  fis_output_names <- sapply(project_data$fis$output, "[[", 1)
  
  # Order EP Amounts data to match with FIS Inputs.
  curr_input_stack <- c()
  min_input_stack <- c()
  max_input_stack <- c()
  for (EP in fis_input_names) {
    curr_input_stack <- c(
      curr_input_stack,
      as.numeric(project_data$EP_amounts[[EP, 'Central']])
    )
    min_input_stack <- c(
      min_input_stack,
      as.numeric(project_data$EP_amounts[[EP, 'Min']])
    )
    max_input_stack <- c(
      max_input_stack,
      as.numeric(project_data$EP_amounts[[EP, 'Max']])
    )
  }

  val_indices <- which(fis_output_names %in% values)
  val_ranges <- lapply(project_data$fis$output[val_indices], "[[", 2)
  val_min <- min(sapply(val_ranges, "[[", 1))
  val_max <- max(sapply(val_ranges, "[[", 2))

  analysis_df <- data.frame(matrix(
    nrow = POINT_N,
    ncol = length(values) + 1
  ))
  colnames(analysis_df) <- c("X", values)

  # Generate x values based on input range and resolution
  input_index <- which(sapply(project_data$fis$input, "[[", 1) == property)
  input_data <- project_data$fis$input[[input_index]]

  x_values <- seq(
    input_data$range[1],
    input_data$range[2],
    length = POINT_N
  )

  # Generate y values based on Value Delivery across multiple input values.
  for (i in 1: length(x_values)) {
    input_stack <- curr_input_stack
    input_stack[input_index] <- x_values[i]
    output_stack <- evalfis(input_stack, project_data$fis, point_n = POINT_N)
    output_stack <- output_stack[val_indices]
    analysis_df[i, ] <- c(x_values[i], output_stack)
  }

  # Format data for multi-line plotting
  analysis_df <- reshape2::melt(analysis_df, id = "X")

  # Plot Graph
  ggplot(analysis_df) +
    labs(
      title = NULL,
      colour = "Value",
      x = "Property Amount",
      y = "Value Delivery"
    ) +
    geom_rect(
      xmin = min_input_stack[input_index],
      xmax = max_input_stack[input_index],
      ymin = 0, 
      ymax = 1,
      linewidth = 0,
      fill = "gray98"
    ) +
    geom_vline(
      xintercept = min_input_stack[input_index],
      linetype = "dashed",
      color = "darkgray",
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = max_input_stack[input_index],
      linetype = "dashed",
      color = "darkgray",
      alpha = 0.7
    ) +
    geom_line(
      aes(x = X, y = value, colour = variable), linewidth = 1.2
    ) +
    theme(
      plot.margin = margin(t= 10),
      plot.title = element_text(
        margin = margin(b = 15),
        hjust = 0.5,
        size = 18
      ),
      legend.key = element_rect("white", "white"),
      legend.key.size = unit(2, 'cm'),
      legend.key.height = unit(1, 'cm'),
      legend.key.width = unit(1.5, 'cm'),
      legend.title = element_text(size=16),
      legend.text = element_text(size=14),
      axis.line = element_line(colour = "grey"),
      axis.title = element_text(size = 16),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    ) +
    # Force y axis to start at origin
    scale_y_continuous(expand = c(0, 0), limits = c(val_min, val_max)) +
    # Force x axis to cover variable range
    scale_x_continuous(
      expand = c(0, 0),
      limits = c(input_data$range[1], input_data$range[2])
    )

}
