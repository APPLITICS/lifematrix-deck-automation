# ------ CIRCLE MODULE UI ------------------------------------------------------
mod_circle_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Text input fields for slide title
    textInput(
      ns("title"),
      "Slide Title",
      "CIRCLE CHART"
    ),
    
    # ------ METRIC -------------------------------------------------
    # Optional numeric metric to plot in the circle chart
    pickerInput(
      ns("metric"),
      "Metric",
      choices = c("None" = "", num_cols),
      selected = "",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),
    
    # ------ UNIT --------------------------------------------------------------
    # Optional unit selector (none or percentage)
    pickerInput(
      ns("unit"),
      "Unit",
      choices = c("None" = "", "%"),
      selected = "",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
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
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("focal_subset_title"),
        "Subset title",
        choices = c("None" = "", all_cols),
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("focal_subset_value"),
        "Subset value",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    
    # ------ CATEGORY (NAME + ORDER) -------------------------------------------
    # Choose category column and order for grouping
    h6("Category"),
    fluidRow(
      column(
        6,
        pickerInput(
          ns("category_name"),
          "Category name",
          choices = all_cols,
          selected = "reunion_class",
          multiple = FALSE,
          options = list(`live-search` = TRUE)
        )
      ),
      column(
        6,
        pickerInput(
          ns("category_order"),
          "Category order",
          choices = all_cols,
          selected = "reunion_class_levels",
          multiple = FALSE,
          options = list(`live-search` = TRUE)
        )
      )
    )
  )
}
