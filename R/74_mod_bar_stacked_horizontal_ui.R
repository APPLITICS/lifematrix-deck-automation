# ------ STACKED HORIZONTAL BAR MODULE UI --------------------------------------
mod_stacked_horizontal_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Input fields for slide title and optional unit
    textInput(
      ns("title"),
      "Slide Title",
      "STACKED HORIZONTAL BAR"
    ),

    # ------ METRIC ------------------------------------------------------------
    # Optional numeric metric (e.g. percentages or values)
    pickerInput(
      ns("metric"),
      "Metric (optional)",
      choices = c("None" = "", num_cols),
      selected = "",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    textInput(
      ns("unit"),
      "Unit (displayed with metric)",
      ""
    ),
    # ------ CATEGORY Y --------------------------------------------------------
    # Manage multiple Y groups with optional subsets
    h6("Category Y"),
    div(
      class = "inline-inputs",
      # Base Y variable
      pickerInput(
        ns("new_y_name"),
        "Y variable",
        choices = all_cols,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional Y subset column
      pickerInput(
        ns("new_y_subset_title"),
        "Y subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional Y subset values (updated dynamically)
      pickerInput(
        ns("new_y_subset_value"),
        "Y subset value",
        choices = character(0),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    # Append new Y group to table
    actionButton(
      ns("append_y"),
      "Append Y group",
      class = "btn-secondary"
    ),
    # Conditionally display appended Y groups
    conditionalPanel(
      condition = paste0("output['", ns("show_y_table"), "']"),
      h6("Appended Y groups:"),
      DTOutput(ns("y_table"))
    ),

    # ------ CATEGORY X --------------------------------------------------------
    # Define stacked segments inside bars with optional order and filter
    h6("Category X"),
    div(
      class = "inline-inputs",
      # Base X variable
      pickerInput(
        ns("category_x_name"),
        "X variable",
        choices = all_cols,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional column defining order of levels
      pickerInput(
        ns("category_x_order"),
        "X order",
        choices = c("None" = "", all_cols),
        selected = "",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional filter for selected levels of X
      pickerInput(
        ns("category_x_value"),
        "X filter",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),

    # ------ FOCAL GROUP -------------------------------------------------------
    # Define primary group and optional subset
    h6("Focal Group"),
    div(
      class = "inline-inputs",
      # Focal group name
      pickerInput(
        ns("focal_name"),
        "Group name",
        choices = unique_groups,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset column
      pickerInput(
        ns("focal_subset_title"),
        "Subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      # Optional subset values (updated dynamically)
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
    )
  )
}
