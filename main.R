# ------ LIBRARIES ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(officer)
library(rvg)
library(grid)
library(gridExtra)
library(cowplot)
library(stringr)
library(scales)
# ------ LOAD DATA ------------------------------------------------------------

pipeline_data <- data.table::fread("data/simulated_pipeline_input.csv")

# ------ DEFINE GROUPS ---------------------------------------------------------

focal_group        <- "Xilio"
comparison_group_1 <- "HBS"
comparison_group_2 <- "IWF"

# ------ LOAD FUNCTIONS & INSTRUCTIONS -----------------------------------------

invisible(
  lapply(
    list.files(
      "R",
      pattern    = "\\.R$",
      full.names = TRUE
    ),
    source
  )
)

# ------ RUN PIPELINE ----------------------------------------------------------

run_pipeline(
  data         = pipeline_data,
  instructions = instructions,
  ppt_template_path = "inputs/template.pptx",
  ppt_output_path   = "outputs/generated_slides.pptx"
)
