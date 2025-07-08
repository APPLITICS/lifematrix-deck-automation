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
    ppt_output_path   = "outputs/generated_slides.pptx"
) {
  # ------ HELPERS -------------------------------------------------------------
  # Generate a unique file path by appending _2, _3, etc. if needed
  get_unique_path <- function(path) {
    if (!file.exists(path)) return(path)

    base    <- tools::file_path_sans_ext(path)
    ext     <- tools::file_ext(path)
    counter <- 2

    repeat {
      new_path <- paste0(base, "_", counter, ".", ext)
      if (!file.exists(new_path)) return(new_path)
      counter <- counter + 1
    }
  }
  # ------ LOAD TEMPLATE -------------------------------------------------------
  # Load the PowerPoint template file
  ppt_doc <- officer::read_pptx(ppt_template_path)
  # ------ GENERATE SLIDES -----------------------------------------------------
  # Loop over instructions and generate slides using specified chart functions
  for (i in seq_along(instructions)) {
    inst <- instructions[[i]]
    if (!is.null(inst$function_name)) {
      func_name <- inst$function_name
      message(paste0("➡️ Generating slide using ", func_name, "()"))

      tryCatch({
        ppt_doc <- do.call(
          what = match.fun(func_name),
          args = list(
            data        = data,
            instruction = inst,
            ppt_doc     = ppt_doc
          )
        )
      }, error = function(e) {
        message(paste0(
          "❌ Error in function ", func_name, ": ", e$message
        ))
      })
    } else {
      message(paste0(
        "⚠️ Skipping instruction ", i,
        ": no function_name provided"
      ))
    }
  }

  # ------ SAVE TO UNIQUE FILE -------------------------------------------------
  # Save the final PowerPoint to a unique output path
  unique_path <- get_unique_path(ppt_output_path)

  print(ppt_doc, target = unique_path)

  message(paste0("✅ Presentation saved to: ", unique_path))
}
