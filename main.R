# ------ LIBRARIES -------------------------------------------------------------
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

# ------ PATHS -----------------------------------------------------------------
#data_path <- "data/simulated_pipeline_input.csv"
data_path <- "data/Corrupted_Simulated_Data.csv"
map_path  <- "inputs/mapping_file.csv"
ppt_template <- "inputs/template.pptx"
ppt_output <- "outputs/generated_slides.pptx"



# ------ LOAD DATA -------------------------------------------------------------
pipeline_data <- fread(data_path)

# ------ NORMALIZE NAs ---------------------------------------------------------
# Replace numeric Inf with NA; replace "inf" or "" (case-insensitive) in chars.
na_equivalent <- c("inf", "")
pipeline_data <- pipeline_data[, lapply(.SD, function(x) {
  if (is.numeric(x)) {
    x[is.infinite(x)] <- NA_real_
  } else if (is.character(x)) {
    x <- trimws(x)
    x[tolower(x) %in% na_equivalent] <- NA_character_
  }
  x
})]

# ------ LOAD MAPPING FILE -----------------------------------------------------
variable_map <- fread(map_path)

# ------ DEFINE GROUPS ---------------------------------------------------------
focal_group <- "Xilio"
comparison_group_1 <- "HBS"
comparison_group_2 <- "IWF"

# ------ LOAD FUNCTIONS & INSTRUCTIONS ----------------------------------------
invisible(lapply(
  list.files(
    path       = "R",
    pattern    = "\\.R$",
    full.names = TRUE
  ),
  source
))

# ------ RUN PIPELINE ----------------------------------------------------------
run_pipeline(
  data = pipeline_data,
  instructions = instructions,
  ppt_template_path = ppt_template,
  ppt_output_path = ppt_output
)
