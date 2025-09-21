# ------ CIRCLE MODULE SERVER --------------------------------------------------
mod_circle_server <- function(
    id,
    pipeline_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
    # Dynamically populate subset values for selected focal column
    observeEvent(input$focal_subset_title, {
      col <- input$focal_subset_title
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(pipeline_data)
      ) {
        vals <- sort(
          unique(
            as.character(pipeline_data[[col]])
          )
        )
        vals <- vals[vals != "" & !is.na(vals)]
        updatePickerInput(
          session,
          "focal_subset_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "focal_subset_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)
    
    # ------ COLLECT PARAMETERS ------------------------------------------------
    # Assemble parameters into a list for chart generation
    get_params <- reactive({
      focal_subset <- NULL
      if (!is.null(input$focal_subset_title) &&
          nzchar(input$focal_subset_title) &&
          length(input$focal_subset_value) > 0
      ) {
        focal_subset <- list(
          title = input$focal_subset_title,
          value = input$focal_subset_value
        )
      }
      
      list(
        function_name = "generate_circle_slide",
        metric = if (nzchar(input$metric)) input$metric else NULL,
        category = list(
          name = input$category_name,
          order = input$category_order
        ),
        unit = if (nzchar(input$unit)) input$unit else NULL,
        title = if (nzchar(input$title)) input$title else NULL,
        focal_group = list(
          name = input$focal_name,
          subset = focal_subset
        )
      )
    })
    
    list(get_params = get_params)
  })
}
