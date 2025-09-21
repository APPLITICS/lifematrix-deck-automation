# ------ HORIZONTAL BAR UI -----------------------------------------------------
mod_horizontal_bar_ui <- function(
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
      "DISCRETIONARY TIME – ACTIVITIES"
    ),
    textInput(
      ns("x_title"),
      "X axis title",
      "Avg. Hours Per Week"
    ),
    textInput(
      ns("y_title"),
      "Y axis title",
      "Activity"
    ),
    
    # ------ METRICS -----------------------------------------------------------
    # Select main metrics (hours per week)
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
    
    # ------ SUBJECTIVE TITLE --------------------------------------------------
    # Optional subjective axis title
    h6("subjective metrics"),
    textInput(
      ns("x_title_subjective"),
      "X axis title (subjective, optional)",
      ""
    ),
    
    # ------ SUBJECTIVE VALUES -------------------------------------------------
    # Select subjective value metrics
    pickerInput(
      ns("subjective_value"),
      "Subjective value metrics",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),
    
    # ------ FOCAL GROUP -------------------------------------------------------
    # Select focal group and optional subset
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
        "Subset title (optional)",
        choices = c("None" = "", all_cols),
        multiple = FALSE,
        options = list(`live-search` = TRUE)
      ),
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
    )
  )
}
