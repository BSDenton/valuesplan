# Project Data Default Values --------------------------------------------------
DEFAULT_PROJECT_DATA <- list(
  type = "valuesplan-project",
  title = NULL,
  elements = c(),
  properties = c(),
  values = c(),
  EP_conn_matrix = c(),
  EP_names = c(),
  EP_amounts = c(),
  val_conn_matrix = c(),
  val_conn_names = list(),
  EPV_relationships = list(),
  fis = newfis("")
)

POINT_N <- 101
MC_SAMPLES <- 10
US_SAMPLES <- 10
H_SAMPLES <- 10

# Color Parameters -------------------------------------------------------------
ELEMENT_COLOR <-  "#e4f2fa"
PROPERTY_COLOR <- "#efe4fa"
VALUE_COLOR <-    "#ffcccc"

# Membership Function Parameters -----------------------------------------------
TYPE_CHOICES <- list(
  "Gaussian" = "gaussmf",
  "Triangular" = "trimf",
  "Trapezoidal" = "trapmf"
)
TYPE_DEFAULT_PARAMETERS <- list(
  "gaussmf" = c(0, 0),
  "trimf" = c(0, 0, 0),
  "trapmf" = c(0, 0, 0, 0)
)


# Style Parameters -------------------------------------------------------------
# All pages
PAGE_STYLE <-     "min-width: 800px; max-width: 1100px; margin: auto;"
WELL_STYLE <-     "padding: 10px;"

# Specific pages
HP_SECTION_STYLE <- "padding: 30px 40px;"
PAGE_6_STYLE <-   "

/* !important tags must be used to override shinythemes' Bootstrap theme */

/* SELECT */

/* Select's Group Names */
.selectize-dropdown .optgroup-header {
  font-size: 14px !important;
  font-weight: bold !important;
  color: black !important;
}

/* RELATIONSHIP TEXT */

/* SLIDERS */

/* General Settings */
.irs--shiny .irs-line::before {
 cursor: pointer !important;
}

/* Container */
.shiny-input-container:not(.shiny-input-container-inline) {
  width: 100%;
}

/* Handle showing selected element */
.irs--shiny .irs-handle {
  top: 0px !important;
  width: 25px !important;
  height: 15px !important;
  border-radius: 0px !important;
  background-color: #fff !important;
}

/* Background bar */
.irs--shiny .irs-line {
  top: 0px !important;
  width: 100% !important;
  height: 15px !important;
  border: none !important;
  border-radius: 0px !important;
}

/* Filled bar up to selection */
.irs--shiny .irs-bar {
  top: 0px !important;
  visibility: hidden !important;
}

/* Selected value label positioning */
.irs-from, .irs-to, .irs-single {
  top: 0px !important;
}

/* Selected value label */
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  color: black !important;
  background: none !important;
  border-radius: 0px !important;
  font-size: 13px !important;
}

/* Ticks */

.irs--shiny.irs-with-grid {
  height: 0px !important;
}

.irs--shiny .irs-grid {
  top: 2px !important;
}

.irs-grid-pol {
  top: 13px !important;
}

.irs--shiny .irs-grid-pol.small {
  visibility: hidden !important;
}

.irs--shiny .irs-single {
    visibility: hidden !important;
}

.irs--shiny .irs-min, .irs--shiny .irs-max {
  visibility: hidden !important;
}

.irs-grid-text {
  visibility: hidden !important;
}

/* CURRENT RELATIONSHIPS STATUS TEXT */

/* Container */

#relationship_status {
  padding: 10px 30px;
  border: 2px solid #ffac59 !important;
}

"

PAGE_8_STYLE <- "

/* Amount Input Box */

.form-control {
  height: 32px !important;
}

.form-group {
  margin: 0px !important;
}

.EP-amounts-display {
  display: inline-flex;
  min-width: 650px;
  width: 80%;
  margin: 0 10%;
  justify-content: center;
  padding-top: 10px;
  padding-bottom: 10px;
  border-bottom: lightgrey 1px dashed;
  align-items: flex-end;
}

.noUi-tooltip {
  padding: 2px 5px !important;
}

.noUi-horizontal .noUi-handle {
  width: 26px !important;
  height: 22px !important;
  top: -3px !important;
  cursor: ew-resize !important;
}

.noUi-handle:before, .noUi-handle:after {
  left: 9px !important;
  height: 11px !important;
  top: 5px !important;
}

.noUi-handle:after {
    left: 14px !important;
}

"

