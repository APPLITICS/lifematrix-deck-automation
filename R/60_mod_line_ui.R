# ------ LINE MODULE UI --------------------------------------------------------
mod_line_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Slide and y-axis titles
    textInput(
      ns("title"),
      "Slide Title",
      ""
    ),
    textInput(
      ns("y_title"),
      "Y axis title",
      ""
    ),

    # ------ METRICS -----------------------------------------------------------
    # Select one or more metrics to plot
    pickerInput(
      ns("metric"),
      "Metrics",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),

    # ------ CATEGORY ----------------------------------------------------------
    # Select category variable and optional ordering column
    h4("Category"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("category_name"),
        "Category name",
        choices = all_cols,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("category_order"),
        "Category order (levels column)",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      )
    ),

    # ------ UNIT --------------------------------------------------------------
    # Free text input for specifying unit
    textInput(
      ns("unit"),
      "Unit",
      ""
    ),

    # ------ FOCAL GROUP -------------------------------------------------------
    # Define focal group and optional subset values
    h4("Focal Group"),
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
    )
  )
}
