# ------ LIBRARIES ------------------------------------------------------------
library(ggplot2)
library(ggtext)
library(ggrepel)
library(patchwork)
library(dplyr)
library(tidyr)
library(officer)
library(rvg)
library(grid)
library(gridExtra)
library(cowplot)
library(stringr)
library(scales)
library(tibble)
library(forcats)
library(data.table)


# ------ LOAD DATA ------------------------------------------------------------
#pipeline_data <- fread("data/simulated_pipeline_input.csv")
pipeline_data <- fread("data/Corrupted_Simulated_Data.csv")

# replace empty and inf values by NA. Add to the list if any other element is relevant
na_equivilent <- c("inf", "")
pipeline_data <- pipeline_data[, lapply(.SD, function(x) {
  if (is.numeric(x)) {
    x[is.infinite(x)] <- NA
  } else if (is.character(x)) {
    x <- trimws(x)
    x[tolower(x) %in% na_equivilent] <- NA
  }
  x
})]




# ------ LOAD MAPPING FILE -----------------------------------------------------
variable_map  <- fread("inputs/mapping_file.csv")

# ------ DEFINE GROUPS ---------------------------------------------------------
focal_group <- "Xilio"
comparison_group_1 <- "HBS"
comparison_group_2 <- "IWF"

# ------ LOAD FUNCTIONS & INSTRUCTIONS -----------------------------------------
invisible(
  lapply(
    list.files(
      "R",
      pattern = "\\.R$",
      full.names = TRUE
    ),
    source
  )
)

# ------ RUN PIPELINE ----------------------------------------------------------
run_pipeline(
  data = pipeline_data,
  instructions = instructions,
  ppt_template_path = "inputs/template.pptx",
  ppt_output_path = "outputs/generated_slides.pptx"
)
