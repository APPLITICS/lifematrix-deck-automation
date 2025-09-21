# ------ STACKED VERTICAL BAR MODULE UI ----------------------------------------
mod_stacked_vertical_ui <- function(
    id,
    unique_groups,
    all_cols,
    num_cols
) {
  ns <- NS(id)
  
  tagList(
    # ------ TITLES ------------------------------------------------------------
    textInput(ns("title"), "Slide Title", "STACKED VERTICAL BAR"),

    # ------ METRIC ------------------------------------------------------------
    pickerInput(
      ns("metric"),
      "Metric (optional)",
      choices = c("None" = "", num_cols),
      selected = "",
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),
    textInput(ns("unit"), "Unit (displayed with metric)", ""),
    # ------ CATEGORY X --------------------------------------------------------
    h6("Category X (defines groups shown along the x-axis)"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_x_name"),
        "X variable",
        choices = all_cols,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_x_order"),
        "X order",
        choices = c("None" = "", all_cols),
        selected = "",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      )
    ),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_x_subset_title"),
        "X subset title",
        choices = c("None" = "", all_cols),
        selected = "None",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_x_subset_value"),
        "X subset value",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    actionButton(ns("append_x"), "Append X group", class = "btn-secondary"),
    conditionalPanel(
      condition = paste0("output['", ns("show_x_table"), "']"),
      h6("Appended X groups:"),
      DTOutput(ns("x_table"))
    ),
    
    # ------ CATEGORY Y --------------------------------------------------------
    h6("Category Y (defines the stacked segments)"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_y_name"),
        "Y variable",
        choices = all_cols,
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_y_order"),
        "Y order",
        choices = c("None" = "", all_cols),
        selected = "",
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
      pickerInput(
        ns("new_y_value"),
        "Y filter values",
        choices = character(0),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    actionButton(ns("append_y"), "Append Y group", class = "btn-secondary"),
    conditionalPanel(
      condition = paste0("output['", ns("show_y_table"), "']"),
      h6("Appended Y groups:"),
      DTOutput(ns("y_table"))
    ),
    
    # ------ FOCAL GROUP -------------------------------------------------------
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
        selected = "None",
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
    )
  )
}
