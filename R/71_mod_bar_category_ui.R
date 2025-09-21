# ------ BAR CATEGORY MODULE UI ------------------------------------------------
mod_bar_category_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)

  tagList(
    # ------ TITLES ------------------------------------------------------------
    # Inputs for slide title and axis labels
    textInput(
      ns("title"),
      "Slide Title",
      "CATEGORY BAR CHART"
    ),
    textInput(
      ns("x_title"),
      "X axis title",
      "Category"
    ),
    textInput(
      ns("y_title"),
      "Y axis title",
      "Value"
    ),

    # ------ METRIC ------------------------------------------------------------
    # Select a single metric to display in the bar chart
    pickerInput(
      ns("metric"),
      "Metric",
      choices = num_cols,
      selected = NULL,
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),

    # ------ CATEGORY ----------------------------------------------------------
    # Define category variable and optional order column
    h6("Category"),
    fluidRow(
      column(
        6,
        pickerInput(
          ns("category_name"),
          "Category name",
          choices = all_cols,
          selected = NULL,
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
          selected = NULL,
          multiple = FALSE,
          options = list(`live-search` = TRUE)
        )
      )
    ),

    # ------ UNIT -------------------------------------------------------------
    # Optional unit to display on the chart
    pickerInput(
      ns("unit"),
      "Unit",
      choices = c("None" = "", "%"),
      selected = "",
      multiple = FALSE
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

    # ------ TREND LINE --------------------------------------------------------
    # Option to overlay a trend line on the bar chart
    checkboxInput(
      ns("trend_line"),
      "Show trend line",
      value = FALSE
    )
  )
}
