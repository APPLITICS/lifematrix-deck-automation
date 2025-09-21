# ------ DONUT MODULE SERVER ---------------------------------------------------
mod_donut_server <- function(
    id,
    pipeline_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    comparison_values <- reactiveVal(data.frame(
      name = character(),
      subset = character(),
      stringsAsFactors = FALSE
    ))

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
    # Dynamically populate focal subset values
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
    # Dynamically populate comparison subset values
    observeEvent(input$new_subset_title, {
      col <- input$new_subset_title
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
    # Add new comparison group to the table
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

      df <- comparison_values()
      df <- rbind(
        df,
        data.frame(
          name = input$new_comp_name,
          subset = new_subset,
          stringsAsFactors = FALSE
        )
      )
      comparison_values(df)
    })

    # ------ SHOW COMPARISON GROUPS TABLE --------------------------------------
    # Display appended comparison groups with delete button
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
    # Show the comparison groups table only when groups exist
    output$show_comp_table <- reactive({
      nrow(comparison_values()) > 0
    })
    outputOptions(output, "show_comp_table", suspendWhenHidden = FALSE)

    # ------ DELETE COMPARISON GROUP -------------------------------------------
    # Capture clicks on delete button and remove group
    observeEvent(input$comp_table_cell_clicked, {
      info <- input$comp_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()
      
      if (grepl("^<button id=\"delete_", info$value)) {
        row_id <- as.integer(
          gsub("\\D", "", info$value)
        )
        df <- comparison_values()
        if (!is.na(row_id) &&
            row_id > 0 &&
            row_id <= nrow(df)) {
          df <- df[-row_id, , drop = FALSE]
          comparison_values(df)
        }
      }
    })

    # ------ COLLECT PARAMETERS ------------------------------------------------
    # Build instruction list for donut slide generation
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

      comparison_list <- if (nrow(comparison_values()) > 0) {
        lapply(
          seq_len(
            nrow(comparison_values())
          ), function(i) {
            row <- comparison_values()[i, ]
            list(
              name = ifelse(
                is.na(row$name) || row$name == "Placeholder",
                NA,
                row$name
              ),
              subset = if (row$subset != "No subset") {
                parts <- strsplit(row$subset, " = ")[[1]]
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
        function_name = "generate_donut_slide",
        category = list(
          name = input$category_name,
          order = input$category_order
        ),
        title = if (nzchar(input$title)) input$title else NULL,
        focal_group = list(
          name = input$focal_name,
          subset = focal_subset
        ),
        comparison_groups = comparison_list
      )
    })

    list(get_params = get_params)
  })
}
