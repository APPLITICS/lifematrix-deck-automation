# ------ SCATTER MODULE SERVER -------------------------------------------------
mod_scatter_server <- function(
    id,
    pipeline_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
    # Dynamically populate focal subset values from data
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
    # Build the instruction list for slide generation
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
        function_name = "generate_scatter_slide",
        metric = input$metric,
        subjective_value = input$subjective_value,
        title = if (nzchar(input$title)) input$title else NULL,
        x_title = if (nzchar(input$x_title)) input$x_title else NULL,
        y_title = if (nzchar(input$y_title)) input$y_title else NULL,
        focal_group = list(
          name = input$focal_name,
          subset = focal_subset
        )
      )
    })

    list(get_params = get_params)
  })
}
