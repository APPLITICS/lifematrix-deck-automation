# ------ SCATTER MODULE UI -----------------------------------------------------
mod_scatter_ui <- function(
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
      "X title",
      ""
    ),
    textInput(
      ns("y_title"),
      "Y title",
      ""
    ),
    
    # ------ METRICS -----------------------------------------------------------
    # Select metrics for x-axis (hours) and y-axis (subjective values)
    pickerInput(
      ns("metric"),
      "Hour metrics (x-axis)",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
    ),
    pickerInput(
      ns("subjective_value"),
      "Subjective value metrics (y-axis)",
      choices = num_cols,
      selected = NULL,
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE
      )
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
        selected = "",
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
