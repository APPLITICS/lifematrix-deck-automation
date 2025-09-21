# ------ TILE MODULE UI --------------------------------------------------------
mod_tile_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLE -------------------------------------------------------------
    # Title of the slide
    textInput(
      inputId = ns("title"),
      label = "Slide Title",
      value = ""
    ),

    # ------ METRICS -----------------------------------------------------------
    # Select metrics and their corresponding hours
    pickerInput(
      inputId = ns("metrics"),
      label = "Activity metrics",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),
    pickerInput(
      inputId = ns("metric_hours"),
      label = "Activity hours metrics",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),

    # ------ UNIT --------------------------------------------------------------
    # Choose unit of measurement
    pickerInput(
      inputId = ns("unit"),
      label = "Unit",
      choices = c("None" = "", "hrs"),
      selected = "",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    # ------ PREFERRED VALUE & LIMIT -------------------------------------------
    # Preferred ranking (high/low) and number of activities
    div(
      class = "inline-inputs",
      pickerInput(
        inputId = ns("preferred_value"),
        label = "Preferred value",
        choices = c("high", "low"),
        selected = NULL
      ),
      numericInput(
        inputId = ns("n_activities"),
        label = "Number of activities",
        value = "5",
        min = 1,
        max = 20
      )
    ),

    # ------ FOCAL GROUP -------------------------------------------------------
    # Define focal group and optional subset values
    h6("Focal Group"),
    div(
      class = "inline-inputs",
      pickerInput(
        inputId = ns("focal_name"),
        label = "Group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        inputId = ns("focal_subset_title"),
        label = "Subset title (optional)",
        choices = c("None" = "", all_cols),
        selected = "",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        inputId = ns("focal_subset_value"),
        label = "Subset value (optional)",
        choices = character(0),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),

    # ------ COMPARISON GROUPS -------------------------------------------------
    # Define comparison groups and their subsets
    h6("Comparison Groups"),
    div(
      class = "inline-inputs",
      pickerInput(
        inputId = ns("new_comp_name"),
        label = "Group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        inputId = ns("new_subset_title"),
        label = "Subset title (optional)",
        choices = c("None" = "", all_cols),
        selected = "",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        inputId = ns("new_subset_value"),
        label = "Subset value (optional)",
        choices = character(0),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    actionButton(
      ns("append_comp"),
      "Append comparison group",
      class = "btn-secondary"
    ),

    # ------ DISPLAY COMPARISON GROUPS -----------------------------------------
    # Show appended comparison groups in a table
    conditionalPanel(
      condition = paste0("output['", ns("show_comp_table"), "']"),
      h6("Appended groups:"),
      DTOutput(ns("comp_table"))
    )
  )
}
