# ------ TILE MODULE SERVER ----------------------------------------------------
mod_tile_server <- function(
    id,
    pipeline_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ INIT REACTIVE -----------------------------------------------------
    # Initialize reactive state for comparison groups
    comparison_groups_state <- reactiveVal(data.frame(
      name = character(),
      subset = character(),
      stringsAsFactors = FALSE
    ))

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
    # Dynamically populate focal subset values from data
    observeEvent(input$focal_subset_title, {
      col <- input$focal_subset_title
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(pipeline_data)) {
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

    # ------ UPDATE COMPARISON SUBSET VALUES -----------------------------------
    # Dynamically populate comparison subset values from data
    observeEvent(input$new_subset_title, {
      col <- input$new_subset_title
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
          "new_subset_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "new_subset_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ APPEND COMPARISON GROUP -------------------------------------------
    # Add new comparison group to the reactive state
    observeEvent(input$append_comp, {
      new_subset <- if (!is.null(input$new_subset_title) &&
                        nzchar(input$new_subset_title) &&
                        length(input$new_subset_value) > 0
      ) {
        paste0(
          input$new_subset_title,
          " = ",
          paste(input$new_subset_value, collapse = ", ")
        )
      } else {
        "No subset"
      }

      comparison_df <- comparison_groups_state()
      comparison_df <- rbind(
        comparison_df,
        data.frame(
          name = input$new_comp_name,
          subset = new_subset,
          stringsAsFactors = FALSE
        )
      )
      comparison_groups_state(comparison_df)
    })

    # ------ SHOW COMPARISON GROUPS TABLE --------------------------------------
    # Render appended groups as a DataTable with delete buttons
    output$comp_table <- renderDataTable({
      comparison_df <- comparison_groups_state()
      if (nrow(comparison_df) == 0) {
        return(
          datatable(data.frame())
        )
      }

      comparison_df$delete <- sprintf(
        '<button id="delete_%s" class="btn btn-danger btn-sm">🗑</button>',
        seq_len(nrow(comparison_df))
      )

      datatable(
        comparison_df,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE
        )
      )
    })

    # ------ CONTROL VISIBILITY OF TABLE ---------------------------------------
    # Only show table if at least one comparison group exists
    output$show_comp_table <- reactive({
      nrow(comparison_groups_state()) > 0
    })
    outputOptions(
      output,
      "show_comp_table",
      suspendWhenHidden = FALSE
    )

    # ------ CAPTURE DELETE BUTTON CLICKS --------------------------------------
    # Handle delete button clicks inside comparison group table
    observeEvent(input$comp_table_cell_clicked, {
      info <- input$comp_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()

      if (grepl("^<button id=\"delete_", info$value)) {
        row_id <- as.integer(
          gsub("\\D", "", info$value)
        )
        comparison_df <- comparison_groups_state()
        if (!is.na(row_id) &&
            row_id > 0 &&
            row_id <= nrow(comparison_df)
        ) {
          comparison_df <- comparison_df[-row_id, , drop = FALSE]
          comparison_groups_state(comparison_df)
        }
      }
    })

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

      comparison_list <- if (nrow(comparison_groups_state()) > 0) {
        lapply(
          seq_len(
            nrow(comparison_groups_state())
          ), function(i) {
            comparison_row <- comparison_groups_state()[i, ]
            list(
              name = comparison_row$name,
              subset = if (comparison_row$subset != "No subset") {
                parts <- strsplit(comparison_row$subset, " = ")[[1]]
                list(
                  title = parts[1],
                  value = unlist(strsplit(parts[2], ", "))
                )
              } else {
                NULL
              }
            )
          })
      } else {
        NULL
      }

      list(
        function_name = "generate_tile_slide",
        metrics = input$metrics,
        metric_hours = input$metric_hours,
        unit = if (nzchar(input$unit)) input$unit else NULL,
        title = if (nzchar(input$title)) input$title else NULL,
        focal_group = list(
          name = input$focal_name,
          subset = focal_subset
        ),
        comparison_groups = comparison_list,
        preferred_value = input$preferred_value,
        n_activities = input$n_activities
      )
    })

    list(get_params = get_params)
  })
}
