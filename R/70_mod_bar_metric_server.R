# ------ BAR METRIC MODULE SERVER ----------------------------------------------
mod_bar_metric_server <- function(id, pipeline_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE VALUES ---------------------------------------------------
    comparison_values <- reactiveVal(
      data.frame(
        name = character(),
        subset = character(),
        stringsAsFactors = FALSE
      )
    )

    placeholder_values <- reactiveVal(
      data.frame(
        name = character(),
        subset = character(),
        stringsAsFactors = FALSE
      )
    )

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
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

    # ------ UPDATE COMPARISON SUBSET VALUES -----------------------------------
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
    observeEvent(input$append_comp, {
      new_subset <- if (!is.null(input$new_subset_title) &&
                        nzchar(input$new_subset_title) &&
                        length(input$new_subset_value
                        ) > 0
      ) {
        paste0(
          input$new_subset_title,
          " = ",
          paste(input$new_subset_value, collapse = ", ")
        )
      } else {
        "No subset"
      }

      df <- comparison_values()
      df <- rbind(df, data.frame(
        name = input$new_comp_name,
        subset = new_subset,
        stringsAsFactors = FALSE
      ))
      comparison_values(df)
    })

    # ------ SHOW COMPARISON GROUPS TABLE --------------------------------------
    output$comp_table <- renderDataTable({
      df <- comparison_values()
      if (nrow(df) == 0) {
        return(datatable(data.frame()))
      }

      df$delete <- sprintf(
        '<button id="delete_%s" class="btn btn-danger btn-sm">🗑</button>',
        seq_len(nrow(df))
      )

      datatable(
        df,
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

    # ------ CONTROL VISIBILITY OF COMP TABLE ----------------------------------
    output$show_comp_table <- reactive({
      nrow(comparison_values()) > 0
    })
    outputOptions(output, "show_comp_table", suspendWhenHidden = FALSE)

    # ------ DELETE COMPARISON GROUP -------------------------------------------
    observeEvent(input$comp_table_cell_clicked, {
      info <- input$comp_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()

      if (grepl("^<button id=\"delete_", info$value)) {
        row_id <- as.integer(
          gsub("\\D", "", info$value)
        )
        df <- comparison_values()
        if (!is.na(row_id) && row_id > 0 && row_id <= nrow(df)) {
          df <- df[-row_id, , drop = FALSE]
          comparison_values(df)
        }
      }
    })

    # ------ UPDATE PLACEHOLDER SUBSET VALUES ----------------------------------
    observeEvent(input$new_placeholder_subset_title, {
      col <- input$new_placeholder_subset_title
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
          "new_placeholder_subset_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "new_placeholder_subset_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ APPEND PLACEHOLDER ------------------------------------------------
    observeEvent(input$append_placeholder, {
      new_subset <- if (!is.null(input$new_placeholder_subset_title) &&
                        nzchar(input$new_placeholder_subset_title) &&
                        length(input$new_placeholder_subset_value) > 0
      ) {
        paste0(
          input$new_placeholder_subset_title,
          " = ",
          paste(input$new_placeholder_subset_value, collapse = ", ")
        )
      } else {
        "No subset"
      }

      df <- placeholder_values()
      df <- rbind(df, data.frame(
        name = input$new_placeholder_name,
        subset = new_subset,
        stringsAsFactors = FALSE
      ))
      placeholder_values(df)
    })

    # ------ SHOW PLACEHOLDERS TABLE -------------------------------------------
    output$placeholder_table <- renderDataTable({
      df <- placeholder_values()
      if (nrow(df) == 0) {
        return(datatable(data.frame()))
      }

      df$delete <- sprintf(
        '<button id="delete_ph_%s" class="btn btn-danger btn-sm">🗑</button>',
        seq_len(nrow(df))
      )

      datatable(
        df,
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

    # ------ CONTROL VISIBILITY OF PLACEHOLDER TABLE ---------------------------
    output$show_placeholder_table <- reactive({
      nrow(placeholder_values()) > 0
    })
    outputOptions(output, "show_placeholder_table", suspendWhenHidden = FALSE)

    # ------ DELETE PLACEHOLDER ------------------------------------------------
    observeEvent(input$placeholder_table_cell_clicked, {
      info <- input$placeholder_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()

      if (grepl("^<button id=\"delete_ph_", info$value)) {
        row_id <- as.integer(
          gsub("\\D", "", info$value)
        )
        df <- placeholder_values()
        if (!is.na(row_id) && row_id > 0 && row_id <= nrow(df)) {
          df <- df[-row_id, , drop = FALSE]
          placeholder_values(df)
        }
      }
    })

    # ------ COLLECT PARAMETERS ------------------------------------------------
    get_params <- reactive({
      focal_subset <- NULL
      if (!is.null(input$focal_subset_title) &&
          nzchar(input$focal_subset_title) &&
          length(input$focal_subset_value) > 0) {
        focal_subset <- list(
          title = input$focal_subset_title,
          value = input$focal_subset_value
        )
      }

      comparison_list <- if (nrow(comparison_values()) > 0) {
        lapply(seq_len(
          nrow(comparison_values())
        ), function(i) {
          row <- comparison_values()[i, ]
          list(
            name = row$name,
            subset = if (row$subset != "No subset") {
              parts <- strsplit(row$subset, " = ")[[1]]
              list(
                title = parts[1],
                value = unlist(
                  strsplit(parts[2], ", ")
                )
              )
            } else {
              NULL
            }
          )
        })
      } else {
        NULL
      }

      placeholder_list <- if (nrow(placeholder_values()) > 0) {
        lapply(seq_len(
          nrow(placeholder_values())
        ), function(i) {
          row <- placeholder_values()[i, ]
          list(
            name = row$name,
            subset = if (row$subset != "No subset") {
              parts <- strsplit(row$subset, " = ")[[1]]
              list(
                title = parts[1],
                value = unlist(
                  strsplit(parts[2], ", ")
                )
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
        function_name = "generate_bar_metric_slide",
        unit = if (nzchar(input$unit)) input$unit else NULL,
        x_title = if (nzchar(input$x_title)) input$x_title else NULL,
        y_title = if (nzchar(input$y_title)) input$y_title else NULL,
        title = if (nzchar(input$title)) input$title else NULL,
        focal_group = list(
          name = input$focal_name,
          subset = focal_subset
        ),
        comparison_groups = comparison_list,
        placeholders = placeholder_list,
        bar_value = input$bar_value,
        target = input$target,
        show_target = input$show_target
      )
    })

    list(get_params = get_params)
  })
}
