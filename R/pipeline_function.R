# ------ PIPELINE FUNCTIONS ----------------------------------------------------

#' Run chart generation pipeline and export slides
#'
#' Applies a list of slide instructions to generate a PowerPoint deck.
#' Slides are created using chart functions like `generate_*_slide()`.
#'
#' @param data Input data frame.
#' @param instructions List of slide instruction configurations.
#' @param ppt_template_path Path to the .pptx template.
#' @param ppt_output_path Output file path (auto-uniqued if file exists).
#'
#' @return NULL. Saves a PowerPoint file to disk.
run_pipeline <- function(
    data,
    instructions,
    ppt_template_path = "inputs/template.pptx",
    ppt_output_path = "outputs/generated_slides.pptx"
) {
  # ------ HELPERS -------------------------------------------------------------
  get_unique_path <- function(path) {
    if (!file.exists(path)) return(path)
    base <- tools::file_path_sans_ext(path)
    ext  <- tools::file_ext(path)
    i <- 2
    repeat {
      candidate <- paste0(base, "_", i, ".", ext)
      if (!file.exists(candidate)) return(candidate)
      i <- i + 1
    }
  }
  
  # Print warnings immediately with slide label, suppressing end-of-pipeline repeats
  run_with_inline_warnings <- function(label, expr) {
    old_opts <- options(warn = 1)
    on.exit(options(old_opts), add = TRUE)
    withCallingHandlers(
      expr,
      warning = function(w) {
        message("\n")
        message("⚠️  ", label, " Warning: ", conditionMessage(w))
        message("\n")
        
        invokeRestart("muffleWarning")
        
      }
    )
  }
  
  # ------ LOAD TEMPLATE -------------------------------------------------------
  ppt_doc <- read_pptx(ppt_template_path)
  
  # ------ GENERATE SLIDES -----------------------------------------------------
  for (i in seq_along(instructions)) {
    inst <- instructions[[i]]
    fn_name <- inst$function_name
    has_fn <- !is.null(fn_name)
    label <- paste0("[", i, "] ", if (has_fn) paste0(fn_name, "()") else "N/A")
    
    message("\n------------------------------------------------------------")
    message("➡️ Generating", label, "...")
    
    
    if (!has_fn) {
      message("⚠️  [", i, "] Skipping instruction: no function_name provided.")
      next
    }
    
    # Resolve function once
    fn <- match.fun(fn_name)
    
    tryCatch(
      {
        result <- run_with_inline_warnings(
          label = label,
          expr = fn(
            data = data,
            instruction = inst,
            ppt_doc = ppt_doc
          )
        )
        
        if (is.null(result)) {
          message("⚠️  [", i, "] Slide not generated (e.g., missing column/s).")
        } else {
          ppt_doc <- result
          message("✅  [", i, "] Slide successfully added.")
        }
      },
      error = function(e) {
        message("❌  [", i, "] Error in function ", fn_name, ": ", conditionMessage(e))

      }
    )
  }
  
  # ------ SAVE TO UNIQUE FILE -------------------------------------------------
  unique_path <- get_unique_path(ppt_output_path)
  print(ppt_doc, target = unique_path)
  message("\n----------------------------------------------------------\n")
  message("✅ Presentation saved to: ", unique_path)
}

