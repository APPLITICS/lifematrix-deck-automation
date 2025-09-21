# ------ DENSITY MODULE UI -----------------------------------------------------
mod_density_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Text fields for slide title and axis labels
    textInput(
      ns("title"),
      "Slide Title",
      "LIFE SATISFACTION"
    ),
    textInput(
      ns("x_title"),
      "X title",
      "Life Satisfaction"
    ),
    textInput(
      ns("y_title"),
      "Y title",
      "Density"
    ),

    # ------ METRIC ------------------------------------------------------------
    # Select the numeric column used for density
    pickerInput(
      ns("metric"),
      "Metric",
      choices = num_cols,
      selected = "life_satisfaction",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    # Select the unit of measurement (none or percentage)
    pickerInput(
      ns("unit"),
      "Unit",
      choices = c("None" = "", "%"),
      selected = "%",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    # ------ FOCAL GROUP -------------------------------------------------------
    # Define the primary group and optional subset
    h4("Focal Group"),
    div(
      class = "inline-inputs",
      # Base group name
      pickerInput(
        ns("focal_name"),
        "Group name",
        choices = unique_groups,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset column
      pickerInput(
        ns("focal_subset_title"),
        "Subset title (optional)",
        choices = c("None" = "", all_cols),
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset values (updated in server)
      pickerInput(
        ns("focal_subset_value"),
        "Subset value (optional)",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),

    # ------ COMPARISON GROUPS -------------------------------------------------
    # Define additional groups and subsets to compare with the focal group
    h4("Comparison Groups"),
    div(
      class = "inline-inputs",
      # Comparison group name
      pickerInput(
        ns("new_comp_name"),
        "Group name",
        choices = unique_groups,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset column for comparison group
      pickerInput(
        ns("new_subset_title"),
        "Subset title (optional)",
        choices = c("None" = "", all_cols),
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset values (updated in server)
      pickerInput(
        ns("new_subset_value"),
        "Subset value (optional)",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),

    # ------ APPEND COMPARISON GROUP -------------------------------------------
    # Button to add the selected comparison group to the table
    actionButton(
      ns("append_comp"),
      "Append comparison group",
      class = "btn-secondary"
    ),

    # ------ DISPLAY COMPARISON GROUPS -----------------------------------------
    # Show the table of appended groups (conditionally displayed)
    conditionalPanel(
      condition = paste0("output['", ns("show_comp_table"), "']"),
      h6("Appended groups:"),
      DTOutput(ns("comp_table"))
    )
  )
}
