# ------ DONUT MODULE UI -------------------------------------------------------
mod_donut_ui <- function(
    id,
    unique_groups,
    all_cols
) {
  ns <- NS(id)
  
  tagList(
    # ------ TITLE -------------------------------------------------------------
    # Slide title
    textInput(
      ns("title"),
      "Slide Title",
      ""
    ),

    # ------ CATEGORY ----------------------------------------------------------
    # Select category variable for donut slices
    pickerInput(
      ns("category_name"),
      "Category name",
      choices = all_cols,
      selected = NULL,
      multiple = FALSE,
      options = list(`live-search` = TRUE)
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
    ),

    # ------ COMPARISON GROUPS -------------------------------------------------
    # Append optional comparison groups with subset values
    h4("Comparison Groups"),
    div(
      class = "inline-inputs",
      pickerInput(
        ns("new_comp_name"),
        "Group name",
        choices = c("Placeholder", unique_groups),
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
    )
  )
}
