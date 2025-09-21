# ------ BAR METRIC MODULE UI --------------------------------------------------
mod_bar_metric_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Slide and axis titles
    textInput(
      ns("title"),
      "Slide Title",
      ""
    ),
    textInput(
      ns("x_title"),
      "X axis title",
      ""
    ),
    textInput(
      ns("y_title"),
      "Y axis title",
      ""
    ),

    # ------ UNIT ----------------------------------------------------------------
    # Select the unit of measurement (none or percentage)
    pickerInput(
      ns("unit"),
      "Unit",
      choices = c("None" = "", "%"),
      selected = "%",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    # ------ BAR VALUES --------------------------------------------------------
    # Select variables to display as bars
    pickerInput(
      ns("bar_value"),
      "Bar values",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),

    # ------ TARGETS -----------------------------------------------------------
    # Select target metrics for optional dashed lines
    pickerInput(
      ns("target"),
      "Target metrics",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),
    checkboxInput(
      ns("show_target"),
      "Show target lines",
      value = FALSE
    ),

    # ------ FOCAL GROUP -------------------------------------------------------
    # Define focal group and optional subset values
    h6("Focal Group"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("focal_name"),
        "Group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("focal_subset_title"),
        "Subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("focal_subset_value"),
        "Subset value",
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
    # Append optional comparison groups
    h6("Comparison Groups"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_comp_name"),
        "Group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_subset_title"),
        "Subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_subset_value"),
        "Subset value",
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
    conditionalPanel(
      condition = paste0("output['", ns("show_comp_table"), "']"),
      h6("Appended groups:"),
      DTOutput(ns("comp_table"))
    ),

    # ------ PLACEHOLDERS ------------------------------------------------------
    # Add placeholder groups for layout preservation
    h6("Placeholders"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_placeholder_name"),
        "Placeholder group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_placeholder_subset_title"),
        "Subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_placeholder_subset_value"),
        "Subset value",
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
      ns("append_placeholder"),
      "Append placeholder",
      class = "btn-secondary"
    ),
    conditionalPanel(
      condition = paste0("output['", ns("show_placeholder_table"), "']"),
      h6("Appended placeholders:"),
      DTOutput(ns("placeholder_table"))
    )
  )
}
