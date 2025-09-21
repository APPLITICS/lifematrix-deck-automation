# ------ HORIZONTAL BAR SERVER -------------------------------------------------
mod_horizontal_bar_server <- function(
    id,
    pipeline_data
) {
  moduleServer(id, function(input, output, session) {

    # ------ Update focal subset values ----------------------------------------
    observeEvent(input$focal_subset_title, {
      col <- input$focal_subset_title
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(pipeline_data)
      ) {
        vals <- sort(
          unique(as.character(pipeline_data[[col]]))
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

    # ------ Collect parameters ------------------------------------------------
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
      # Build combined x_title vector
      x_titles <- c()
      if (nzchar(input$x_title)) {
        x_titles <- c(x_titles, input$x_title)
      }
      if (nzchar(input$x_title_subjective)) {
        x_titles <- c(x_titles, input$x_title_subjective)
      }
      if (length(x_titles) == 0) {
        x_titles <- NULL
      }

      list(
        function_name = "generate_horizontal_bar_slide",
        metric = input$metric,
        subjective_value = if (
          length(input$subjective_value) > 0
        ) input$subjective_value else NULL,
        title = if (nzchar(input$title)) input$title else NULL,
        x_title = x_titles,
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
