# ------ STACKED HORIZONTAL BAR MODULE SERVER ----------------------------------
mod_stacked_horizontal_server <- function(
    id,
    data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE VALUES ---------------------------------------------------
    # Store appended Category Y groups in a reactive data frame
    y_groups <- reactiveVal(
      data.frame(
        name   = character(),
        subset = character(),
        stringsAsFactors = FALSE
      )
    )

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
    # Dynamically populate focal subset values based on selected column
    observeEvent(input$focal_subset_title, {
      col <- input$focal_subset_title
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(data)
      ) {
        vals <- sort(
          unique(
            as.character(data[[col]])
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

    # ------ UPDATE Y SUBSET VALUES --------------------------------------------
    # Dynamically populate Y subset values based on selected column
    observeEvent(input$new_y_subset_title, {
      col <- input$new_y_subset_title
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(data)
      ) {
        vals <- sort(
          unique(
            as.character(data[[col]])
          )
        )
        vals <- vals[vals != "" & !is.na(vals)]
        updatePickerInput(
          session,
          "new_y_subset_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "new_y_subset_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ APPEND CATEGORY Y GROUP -------------------------------------------
    # Add new Category Y group (with subset if applicable) to the store
    observeEvent(input$append_y, {
      new_subset <- if (!is.null(input$new_y_subset_title) &&
                        nzchar(input$new_y_subset_title) &&
                        length(input$new_y_subset_value) > 0
      ) {
        paste0(
          input$new_y_subset_title,
          " = ",
          paste(input$new_y_subset_value, collapse = ", ")
        )
      } else {
        "No subset"
      }
      
      df <- y_groups()
      df <- rbind(
        df,
        data.frame(
          name   = input$new_y_name,
          subset = new_subset,
          stringsAsFactors = FALSE
        )
      )
      y_groups(df)
    })

    # ------ SHOW CATEGORY Y GROUPS TABLE --------------------------------------
    # Render appended Y groups table with delete buttons
    output$y_table <- renderDataTable({
      df <- y_groups()
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

    # ------ CONTROL VISIBILITY OF Y TABLE -------------------------------------
    # Show Y groups table only when at least one row exists
    output$show_y_table <- reactive({
      nrow(y_groups()) > 0
    })
    outputOptions(output, "show_y_table", suspendWhenHidden = FALSE)

    # ------ DELETE CATEGORY Y GROUP -------------------------------------------
    # Capture delete button clicks and remove corresponding row
    observeEvent(input$y_table_cell_clicked, {
      info <- input$y_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()
      
      if (grepl("^<button id=\"delete_", info$value)) {
        row_id <- as.integer(
          gsub("\\D", "", info$value)
        )
        df <- y_groups()
        if (!is.na(row_id) && 
            row_id > 0 && 
            row_id <= nrow(df)
        ) {
          df <- df[-row_id, , drop = FALSE]
          y_groups(df)
        }
      }
    })

    # ------ UPDATE X FILTER VALUES --------------------------------------------
    # Populate X filter values dynamically based on selected X variable
    observeEvent(input$category_x_order, {
      col <- input$category_x_name
      if (!is.null(col) &&
          nzchar(col) &&
          col %in% names(data)
      ) {
        vals <- sort(
          unique(
            as.character(data[[col]])
          )
        )
        vals <- vals[vals != "" & !is.na(vals)]
        updatePickerInput(
          session,
          "category_x_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "category_x_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ COLLECT PARAMETERS ------------------------------------------------
    # Build instruction parameters list for slide generation
    get_params <- reactive({
      # Build category_y list
      y_list <- if (nrow(y_groups()) > 0) {
        lapply(seq_len(nrow(y_groups())), function(i) {
          row <- y_groups()[i, ]
          if (row$subset != "No subset") {
            parts <- strsplit(row$subset, " = ")[[1]]
            list(
              name   = row$name,
              subset = list(
                title = parts[1],
                value = unlist(strsplit(parts[2], ", "))
              )
            )
          } else {
            list(name = row$name, subset = NULL)
          }
        })
      } else {
        NULL
      }
      # Category X list
      x_list <- list(
        list(
          name  = input$category_x_name,
          order = if (!is.null(input$category_x_order) &&
                      nzchar(input$category_x_order)) {
            input$category_x_order
          } else NULL,
          value = if (!is.null(input$category_x_value) &&
                      length(input$category_x_value) > 0) {
            trimws(input$category_x_value)
          } else NULL
        )
      )
      # Build final params
      list(
        function_name = "generate_stacked_horizontal_slide",
        title = input$title,
        metric = if (nzchar(input$metric)) input$metric else NULL,
        unit = if (nzchar(input$unit)) input$unit else NULL,
        category_y = y_list,
        category_x = x_list,
        focal_group = list(
          name   = input$focal_name,
          subset = if (!is.null(input$focal_subset_title) &&
                       nzchar(input$focal_subset_title) &&
                       length(input$focal_subset_value) > 0
          ) {
            list(
              title = input$focal_subset_title,
              value = input$focal_subset_value
            )
          } else NULL
        )
      )
    })

    return(list(get_params = get_params))
  })
}
