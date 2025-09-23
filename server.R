#' LIFE Matrix PPT Builder – Server Logic
#'
#' Handles dynamic module UI, server logic, slide building, appending,
#' preview, and download of PowerPoint slides.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @return No return value, used for Shiny side effects
server <- function(input, output, session) {
  
  # ------ ACTIVE MODULE -------------------------------------------------------
  active_module <- reactiveVal(NULL)
  
  # ------ DYNAMIC MODULE UI ---------------------------------------------------
  output$module_ui <- renderUI({
    req(input$fn_choice)
    
    switch(
      input$fn_choice,
      "generate_density_slide" = mod_density_ui("dens", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_circle_slide" = mod_circle_ui("circle", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_tile_slide" = mod_tile_ui("tile", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_bar_metric_slide" = mod_bar_metric_ui("bar_metric", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_bar_category_slide" = mod_bar_category_ui("bar_category", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_donut_slide" = mod_donut_ui("donut", UNIQUE_GROUPS, ALL_COLS),
      "generate_line_slide" = mod_line_ui("line", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_stacked_vertical_slide" = mod_stacked_vertical_ui("stacked_v", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_stacked_horizontal_slide" = mod_stacked_horizontal_ui("stacked_h", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_horizontal_bar_slide" = mod_horizontal_bar_ui("hbar", UNIQUE_GROUPS, ALL_COLS, NUM_COLS),
      "generate_scatter_slide" = mod_scatter_ui("scatter", UNIQUE_GROUPS, ALL_COLS, NUM_COLS)
    )
  })
  
  # ------ DYNAMIC MODULE SERVER -----------------------------------------------
  observeEvent(input$fn_choice, {
    mod_server <- switch(
      input$fn_choice,
      "generate_density_slide" = function(id) mod_density_server(id, pipeline_data),
      "generate_circle_slide" = function(id) mod_circle_server(id, pipeline_data),
      "generate_tile_slide" = function(id) mod_tile_server(id, pipeline_data),
      "generate_scatter_slide" = function(id) mod_scatter_server(id, pipeline_data),
      "generate_donut_slide" = function(id) mod_donut_server(id, pipeline_data),
      "generate_line_slide" = function(id) mod_line_server(id, pipeline_data),
      "generate_bar_metric_slide" = function(id) mod_bar_metric_server(id, pipeline_data),
      "generate_horizontal_bar_slide" = function(id) mod_horizontal_bar_server(id, pipeline_data),
      "generate_bar_category_slide" = function(id) mod_bar_category_server(id, pipeline_data),
      "generate_stacked_vertical_slide" = function(id) mod_stacked_vertical_server(id, pipeline_data),
      "generate_stacked_horizontal_slide" = function(id) mod_stacked_horizontal_server(id, pipeline_data),
      NULL
    )
    
    if (!is.null(mod_server)) {
      mod_id <- switch(
        input$fn_choice,
        "generate_density_slide" = "dens",
        "generate_circle_slide" = "circle",
        "generate_tile_slide" = "tile",
        "generate_scatter_slide" = "scatter",
        "generate_donut_slide" = "donut",
        "generate_line_slide" = "line",
        "generate_bar_metric_slide" = "bar_metric",
        "generate_horizontal_bar_slide" = "hbar",
        "generate_bar_category_slide" = "bar_category",
        "generate_stacked_vertical_slide" = "stacked_v",
        "generate_stacked_horizontal_slide" = "stacked_h"
      )
      active_module(mod_server(mod_id))
    }
  })
  
  # ------ SLIDE DECK STATE ----------------------------------------------------
  ppt_doc <- reactiveVal(read_pptx(ppt_template_path))
  preview_plot <- reactiveVal(NULL)
  preview_title <- reactiveVal(NULL)
  built_slide <- reactiveVal(NULL)
  built_instr <- reactiveVal(NULL)
  
  # ------ BUILD SLIDE ---------------------------------------------------------
  slide_counter <- reactiveVal(0)
  
  observeEvent(input$build_slide, {
    mod <- active_module()
    req(mod)
    
    instr <- mod$get_params()
    built_instr(instr)
    
    fn_name <- instr$function_name %||% input$fn_choice
    if (is.null(fn_name) || !exists(fn_name, mode = "function")) {
      showNotification(
        paste("❌ Function not found:", fn_name),
        type     = "error",
        duration = 5
      )
      return()
    }
    
    fn <- match.fun(fn_name)
    res <- tryCatch(
      fn(
        data        = pipeline_data,
        instruction = instr,
        ppt_doc     = ppt_doc()
      ),
      error = function(e) {
        showNotification(
          paste("❌ Error in", fn_name, ":", conditionMessage(e)),
          type     = "error",
          duration = 5
        )
        NULL
      }
    )
    
    if (!is.null(res)) {
      built_slide(res)
      preview_plot(res$plot_obj)
      preview_title(instr$title %||% "Untitled")
      
      if (!dir.exists(instr_output_dir)) {
        dir.create(instr_output_dir, recursive = TRUE)
      }
      saveRDS(
        instr,
        file = file.path(
          instr_output_dir,
          paste0("instruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        )
      )
      
      slide_counter(slide_counter() + 1)
      
      showNotification(
        paste0(
          "✅ Built and saved slide Nº", slide_counter(),
          " with ", fn_name, ": ", instr$title
        ),
        type     = "message",
        duration = 4
      )
    }
  })
  
  # ------ APPEND SLIDE --------------------------------------------------------
  slide_count <- reactiveVal(0)
  
  observeEvent(input$append_slide, {
    res <- built_slide()
    if (is.null(res)) {
      showNotification(
        "⚠️ No slide built yet. Click 'Build Slide' first.",
        type = "warning",
        duration = 4
      )
      return()
    }
    
    new_count <- slide_count() + 1
    slide_count(new_count)
    ppt_doc(res$ppt_doc)
    
    if (!is.null(res$instruction)) {
      instr_file <- file.path(
        instr_output_dir,
        paste0("instruction_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      )
      saveRDS(res$instruction, instr_file)
    }
    
    showNotification(
      paste0("✅ Slide appended (number ", new_count, ") and instructions saved"),
      type = "message",
      duration = 4
    )
  })
  
  # ------ PREVIEW -------------------------------------------------------------
  output$slide_preview <- renderPlot({
    plt <- preview_plot()
    ttl <- preview_title()
    
    if (!is.null(plt)) {
      plt <- add_slide_title(plt, ttl)
      plt <- scale_plot_theme(plt, base_size = 12, factor = 1.2)
      plt
    }
  },
  res = 96) 
}