# ------ STACKED VERTICAL BAR MODULE SERVER ------------------------------------
mod_stacked_vertical_server <- function(
    id,
    data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------ REACTIVE VALUES ---------------------------------------------------
    # Store appended Category X and Category Y groups
    x_groups <- reactiveVal(
      data.frame(
        name = character(),
        order = character(),
        subset = character(),
        stringsAsFactors = FALSE
      )
    )
    y_groups <- reactiveVal(
      data.frame(
        name = character(),
        order = character(),
        value = I(list()),
        stringsAsFactors = FALSE
      )
    )

    # ------ UPDATE FOCAL SUBSET VALUES ----------------------------------------
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

    # ------ UPDATE X SUBSET VALUES --------------------------------------------
    observeEvent(input$new_x_subset_title, {
      col <- input$new_x_subset_title
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
          "new_x_subset_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "new_x_subset_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ UPDATE Y FILTER VALUES --------------------------------------------
    observeEvent(input$new_y_name, {
      col <- input$new_y_name
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
          "new_y_value",
          choices = vals
        )
      } else {
        updatePickerInput(
          session,
          "new_y_value",
          choices = character(0)
        )
      }
    }, ignoreInit = TRUE)

    # ------ APPEND CATEGORY X GROUP -------------------------------------------
    observeEvent(input$append_x, {
      df <- x_groups()
      df <- rbind(
        df,
        data.frame(
          name = input$new_x_name,
          order = if (nzchar(input$new_x_order)) input$new_x_order else "",
          subset = if (!is.null(input$new_x_subset_title) &&
                       nzchar(input$new_x_subset_title) &&
                       length(input$new_x_subset_value) > 0
          ) {
            paste0(
              input$new_x_subset_title,
              " = ",
              paste(input$new_x_subset_value, collapse = ", ")
            )
          } else {
            "No subset"
          },
          stringsAsFactors = FALSE
        )
      )
      x_groups(df)
    })

    # ------ APPEND CATEGORY Y GROUP -------------------------------------------
    observeEvent(input$append_y, {
      df <- y_groups()
      df <- rbind(
        df,
        data.frame(
          name = input$new_y_name,
          order = if (nzchar(input$new_y_order)) input$new_y_order else "",
          value = I(list(input$new_y_value)),
          stringsAsFactors = FALSE
        )
      )
      y_groups(df)
    })

    # ------ SHOW CATEGORY X GROUPS TABLE --------------------------------------
    output$x_table <- renderDataTable({
      df <- x_groups()
      if (nrow(df) == 0) return(datatable(data.frame()))
      
      df$delete <- sprintf(
        '<button id="delete_x_%s" class="btn btn-danger btn-sm">🗑</button>',
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

    # ------ SHOW CATEGORY Y GROUPS TABLE --------------------------------------
    output$y_table <- renderDataTable({
      df <- y_groups()
      if (nrow(df) == 0) return(datatable(data.frame()))
      
      df$delete <- sprintf(
        '<button id="delete_y_%s" class="btn btn-danger btn-sm">🗑</button>',
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

    output$show_x_table <- reactive({ nrow(x_groups()) > 0 })
    output$show_y_table <- reactive({ nrow(y_groups()) > 0 })
    outputOptions(output, "show_x_table", suspendWhenHidden = FALSE)
    outputOptions(output, "show_y_table", suspendWhenHidden = FALSE)

    # ------ DELETE CATEGORY X GROUP -------------------------------------------
    observeEvent(input$x_table_cell_clicked, {
      info <- input$x_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()
      
      if (grepl("^<button id=\"delete_x_", info$value)) {
        row_id <- as.integer(gsub("\\D", "", info$value))
        df <- x_groups()
        if (!is.na(row_id) && row_id > 0 && row_id <= nrow(df)) {
          df <- df[-row_id, , drop = FALSE]
          x_groups(df)
        }
      }
    })

    # ------ DELETE CATEGORY Y GROUP -------------------------------------------
    observeEvent(input$y_table_cell_clicked, {
      info <- input$y_table_cell_clicked
      if (is.null(info$value) || info$value == "") return()
      
      if (grepl("^<button id=\"delete_y_", info$value)) {
        row_id <- as.integer(gsub("\\D", "", info$value))
        df <- y_groups()
        if (!is.na(row_id) && row_id > 0 && row_id <= nrow(df)) {
          df <- df[-row_id, , drop = FALSE]
          y_groups(df)
        }
      }
    })

    # ------ COLLECT PARAMETERS ------------------------------------------------
    get_params <- reactive({
      # Category X
      x_list <- if (nrow(x_groups()) > 0) {
        lapply(seq_len(nrow(x_groups())), function(i) {
          row <- x_groups()[i, ]
          if (row$subset != "No subset") {
            parts <- strsplit(row$subset, " = ")[[1]]
            list(
              name = row$name,
              order = if (nzchar(row$order)) row$order else NULL,
              subset = list(
                title = parts[1],
                value = unlist(strsplit(parts[2], ", "))
              )
            )
          } else {
            list(
              name = row$name,
              order = if (nzchar(row$order)) row$order else NULL,
              subset = NULL
            )
          }
        })
      } else NULL
      # Category Y
      y_list <- if (nrow(y_groups()) > 0) {
        lapply(seq_len(nrow(y_groups())), function(i) {
          row <- y_groups()[i, ]
          list(
            name = row$name,
            order = if (nzchar(row$order)) row$order else NULL,
            value = if (length(row$value[[1]]) > 0) row$value[[1]] else NULL
          )
        })
      } else NULL
      
      list(
        function_name = "generate_stacked_vertical_slide",
        title = input$title,
        metric = if (nzchar(input$metric)) input$metric else NULL,
        unit = if (nzchar(input$unit)) input$unit else NULL,
        category_x = x_list,
        category_y = y_list,
        focal_group = list(
          name = input$focal_name,
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
