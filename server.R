server <- function(input, output, session) {
  # ------ RESOLVE SESSION-SPECIFIC PPT OUTPUT PATH ----------------------------
  # Ensure each session writes to a unique PowerPoint file
  session_ppt_path <- ppt_output_path
  if (file.exists(session_ppt_path)) {
    base_dir <- dirname(ppt_output_path)
    base_name <- tools::file_path_sans_ext(basename(ppt_output_path))
    ext <- tools::file_ext(ppt_output_path)

    counter <- 1
    repeat {
      candidate <- file.path(
        base_dir,
        paste0(base_name, "_", counter, ".", ext)
      )
      if (!file.exists(candidate)) {
        session_ppt_path <- candidate
        break
      }
      counter <- counter + 1
    }
  }

  # ------ ACTIVE MODULE -------------------------------------------------------
  # Store the currently selected slide module
  active_module <- reactiveVal(NULL)

  # ------ DYNAMIC MODULE UI ---------------------------------------------------
  # Render the UI of the selected module
  output$module_ui <- renderUI({
    req(input$fn_choice)
    mod_def <- module_registry[[input$fn_choice]]
    mod_def$ui(mod_def$id)
  })

  # ------ DYNAMIC MODULE SERVER -----------------------------------------------
  # Launch server logic for the selected module
  observeEvent(input$fn_choice, {
    req(input$fn_choice %in% names(module_registry))
    mod_def <- module_registry[[input$fn_choice]]
    active_module(mod_def$server(mod_def$id))
  })

  # ------ SLIDE DECK STATE ----------------------------------------------------
  # Manage current pptx doc, preview, title, and instruction state
  ppt_doc <- reactiveVal(read_pptx(ppt_template_path))
  preview_plot <- reactiveVal(NULL)
  preview_title <- reactiveVal(NULL)
  built_instr <- reactiveVal(NULL)

  slide_counter <- reactiveVal(0)  # previews
  slide_count <- reactiveVal(0)    # appended

  # ------ HELPER: RUN SLIDE FUNCTION ------------------------------------------
  # Execute selected slide generator function safely
  run_slide_function <- function(instr, ppt) {
    fn_name <- instr$function_name %||% input$fn_choice
    req(fn_name, exists(fn_name, mode = "function"))
    fn <- match.fun(fn_name)

    tryCatch(
      fn(data = pipeline_data, instruction = instr, ppt_doc = ppt),
      error = function(e) {
        showNotification(
          paste("❌ Error in", fn_name, ":", conditionMessage(e)),
          type = "error",
          duration = 5
        )
        NULL
      }
    )
  }

  # ------ BUILD SLIDE (PLOT + SAVE INSTRUCTIONS) ------------------------------
  # Build preview slide and persist instructions to disk
  observeEvent(input$build_graph, {
    mod <- active_module()
    req(mod)

    instr <- mod$get_params()
    built_instr(instr)

    res <- run_slide_function(instr, ppt = NULL)
    if (!is.null(res)) {
      preview_plot(res$plot_obj)
      preview_title(instr$title %||% "Untitled")
      slide_counter(slide_counter() + 1)

      if (!dir.exists(instr_output_dir)) {
        dir.create(instr_output_dir, recursive = TRUE)
      }

      fn_name <- instr$function_name %||% input$fn_choice
      date_tag <- format(Sys.time(), "%Y%m%d")
      base_name <- paste0("instruction_", fn_name, "_", date_tag, ".rds")
      file_path <- file.path(instr_output_dir, base_name)

      counter <- 1
      while (file.exists(file_path)) {
        counter <- counter + 1
        file_path <- file.path(
          instr_output_dir,
          paste0(
            "instruction_", fn_name, "_",
            date_tag, "_", counter, ".rds"
          )
        )
      }

      saveRDS(instr, file_path)
      showNotification(
        paste0(
          "✅ Preview built & instruction saved: ",
          basename(file_path)
        ),
        type = "message",
        duration = 4
      )
    }
  })

  # ------ APPEND SLIDE (INSERT INTO PPT + SAVE TO DISK) -----------------------
  # Append slide to PowerPoint file and save to disk
  observeEvent(input$append_slide, {
    instr <- built_instr()
    req(instr)

    res <- run_slide_function(instr, ppt = ppt_doc())
    if (!is.null(res)) {
      ppt_doc(res$ppt_doc)

      if (!dir.exists(dirname(session_ppt_path))) {
        dir.create(dirname(session_ppt_path), recursive = TRUE)
      }

      save_ok <- tryCatch({
        print(res$ppt_doc, target = session_ppt_path)
        TRUE
      }, error = function(e) {
        showNotification(
          paste(
            "⚠️ Unable to save PowerPoint. Please close the file ",
            "if it is open in PowerPoint, then try again."
          ),
          type = "error",
          duration = 6
        )
        FALSE
      })

      if (save_ok) {
        slide_count(slide_count() + 1)
        showNotification(
          paste0(
            "✅ Slide appended (number ",
            slide_count(),
            ") and saved to ",
            basename(session_ppt_path)
          ),
          type = "message",
          duration = 5
        )
      }
    }
  })

  # ------ PREVIEW -------------------------------------------------------------
  # Render the live preview of the current slide
  output$slide_preview <- renderPlot(
    {
      plt <- preview_plot()
      ttl <- preview_title()

      if (!is.null(plt)) {
        plt <- add_slide_title(plt, ttl)
        plt <- scale_plot_theme(
          plt,
          base_size = 12,
          factor = 1.2
        )
        plt
      }
    },
    res = 96
  )
}
